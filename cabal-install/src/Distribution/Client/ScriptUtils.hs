{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Utilities to help commands with scripts
--
module Distribution.Client.ScriptUtils (
    getScriptCacheDirectory,
    withTempTempDirectory,
    getContextAndSelectorsWithScripts
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (toList)

import Distribution.Client.ProjectOrchestration
import Distribution.Client.NixStyleOptions
         ( NixStyleFlags (..) )
import Distribution.Client.Setup
         ( GlobalFlags(..), ConfigFlags(..) )
import Distribution.Client.Config
         ( getCabalDir )
import Distribution.Simple.Flag
         ( fromFlagOrDefault )
import Distribution.CabalSpecVersion (CabalSpecVersion (..), cabalSpecLatest)
import Distribution.Verbosity
         ( normal )
import Distribution.Simple.Utils
         ( warn, die'
         , createTempDirectory, handleDoesNotExist )
import Distribution.Client.ProjectConfig
         ( ProjectConfig(..), ProjectConfigShared(..)
         , withProjectOrGlobalConfig )
import Distribution.Client.ProjectFlags
         ( flagIgnoreProject )
import Distribution.Client.TargetSelector
         ( TargetSelectorProblem(..), TargetString(..) )
import Distribution.Client.Types
         ( PackageLocation(..), PackageSpecifier(..) )
import Distribution.FieldGrammar
         ( takeFields, parseFieldGrammar )
import Distribution.PackageDescription.FieldGrammar
         ( executableFieldGrammar )
import Distribution.PackageDescription.PrettyPrint
         ( writeGenericPackageDescription )
import Distribution.Parsec
         ( Position(..) )
import Distribution.Fields
         ( ParseResult, parseString, parseFatalFailure, readFields )
import qualified Distribution.SPDX.License as SPDX
import Distribution.Solver.Types.SourcePackage as SP
         ( SourcePackage(..) )
import Distribution.Types.BuildInfo
         ( BuildInfo(..) )
import Distribution.Types.CondTree
         ( CondTree(..) )
import Distribution.Types.Executable
         ( Executable(..) )
import Distribution.Types.GenericPackageDescription as GPD
         ( GenericPackageDescription(..), emptyGenericPackageDescription )
import Distribution.Types.PackageDescription
         ( PackageDescription(..), emptyPackageDescription )
import Distribution.Types.PackageName.Magic
         ( fakePackageId )
import Language.Haskell.Extension
         ( Language(..) )

import Control.Exception
        ( bracket )
import qualified Data.ByteString.Char8 as BS
import qualified Text.Parsec as P
import System.Directory
         ( getTemporaryDirectory, removeDirectoryRecursive, doesFileExist, makeAbsolute )
import System.FilePath
         ( (</>), takeExtension )

-- | Get the directory for caching a script build.
--
-- The only identity of a script is it's absolute path, so append that path
-- to <cabal_dir>/script-builds/ to get the cache directory.
getScriptCacheDirectory :: FilePath -> IO FilePath
getScriptCacheDirectory script = do
    scriptAbs <- dropWhile (\c -> c == '/' || c == '\\') <$> makeAbsolute script
    cabalDir <- getCabalDir
    return $ cabalDir </> "script-builds" </> scriptAbs

-- | Create a new temporary directory inside the directory for temporary files
-- and delete it after use.
withTempTempDirectory :: (FilePath -> IO a) -> IO a
withTempTempDirectory = bracket getTmp rmTmp
  where
    getTmp = getTemporaryDirectory >>= flip createTempDirectory "cabal-repl."
    rmTmp  = handleDoesNotExist () . removeDirectoryRecursive

-- | Determine whether the targets represent regular targets or a script
--  invocation and return the proper context and target selectors.
--  Report problems if selectors are valid as neither regular targets
--  or as a script.
getContextAndSelectorsWithScripts :: NixStyleFlags a -> [String] -> GlobalFlags -> FilePath -> IO (ProjectBaseContext, [TargetSelector])
getContextAndSelectorsWithScripts flags@NixStyleFlags {..} targetStrings globalFlags tmpDir = do
    let
      with =
        establishProjectBaseContext verbosity cliConfig OtherCommand
      without dir globalConfig = do
        distDirLayout <- establishDummyDistDirLayout verbosity (globalConfig <> cliConfig) dir
        establishDummyProjectBaseContext verbosity (globalConfig <> cliConfig) distDirLayout [] OtherCommand

    baseCtx <- withProjectOrGlobalConfig verbosity ignoreProject globalConfigFlag with (without tmpDir)

    let
      scriptOrError script err = do
        exists <- doesFileExist script
        let pol | takeExtension script == ".lhs" = LiterateHaskell
                | otherwise                      = PlainHaskell
        if exists
          then do
            cacheDir <- getScriptCacheDirectory script
            ctx <- withProjectOrGlobalConfig verbosity ignoreProject globalConfigFlag with (without cacheDir)
            BS.readFile script >>= handleScriptCase verbosity pol ctx cacheDir
          else reportTargetSelectorProblems verbosity err

    -- We pass the baseCtx made with tmpDir to readTargetSelectors and only create a ctx with cacheDir
    -- if no target is found because we want global targets to have higher priority than scripts.
    -- In case of a collision, `cabal run target` can be rewritten as `cabal run ./target`
    -- to specify the script, but there is no alternate way to specify the global target.
    readTargetSelectors (localPackages baseCtx) (Just ExeKind) (take 1 targetStrings)
        >>= \case
          Left err@(TargetSelectorNoTargetsInProject:_)
            | (script:_) <- targetStrings -> scriptOrError script err
          Left err@(TargetSelectorNoSuch t _:_)
            | TargetString1 script <- t   -> scriptOrError script err
          Left err@(TargetSelectorExpected t _ _:_)
            | TargetString1 script <- t   -> scriptOrError script err
          Left err   -> reportTargetSelectorProblems verbosity err
          Right sels -> return (baseCtx, sels)
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    ignoreProject = flagIgnoreProject projectFlags
    cliConfig = commandLineFlagsToProjectConfig globalFlags flags mempty
    globalConfigFlag = projectConfigConfigFile (projectConfigShared cliConfig)

parseScriptBlock :: BS.ByteString -> ParseResult Executable
parseScriptBlock str =
    case readFields str of
        Right fs -> do
            let (fields, _) = takeFields fs
            parseFieldGrammar cabalSpecLatest fields (executableFieldGrammar "script")
        Left perr -> parseFatalFailure pos (show perr) where
            ppos = P.errorPos perr
            pos  = Position (P.sourceLine ppos) (P.sourceColumn ppos)

readScriptBlock :: Verbosity -> BS.ByteString -> IO Executable
readScriptBlock verbosity = parseString parseScriptBlock verbosity "script block"

readScriptBlockFromScript :: Verbosity -> PlainOrLiterate -> BS.ByteString -> IO (Executable, BS.ByteString)
readScriptBlockFromScript verbosity pol str = do
    str' <- case extractScriptBlock pol str of
              Left e -> die' verbosity $ "Failed extracting script block: " ++ e
              Right x -> return x
    when (BS.all isSpace str') $ warn verbosity "Empty script block"
    (\x -> (x, noShebang)) <$> readScriptBlock verbosity str'
  where
    noShebang = BS.unlines . filter (not . BS.isPrefixOf "#!") . BS.lines $ str

-- | Extract the first encountered script metadata block started end
-- terminated by the tokens
--
-- * @{- cabal:@
--
-- * @-}@
--
-- appearing alone on lines (while tolerating trailing whitespace).
-- These tokens are not part of the 'Right' result.
--
-- In case of missing or unterminated blocks a 'Left'-error is
-- returned.
extractScriptBlock :: PlainOrLiterate -> BS.ByteString -> Either String BS.ByteString
extractScriptBlock _pol str = goPre (BS.lines str)
  where
    isStartMarker = (== startMarker) . stripTrailSpace
    isEndMarker   = (== endMarker) . stripTrailSpace

    stripTrailSpace = fst . BS.spanEnd isSpace

    -- before start marker
    goPre ls = case dropWhile (not . isStartMarker) ls of
                 [] -> Left $ "`" ++ BS.unpack startMarker ++ "` start marker not found"
                 (_:ls') -> goBody [] ls'

    goBody _ [] = Left $ "`" ++ BS.unpack endMarker ++ "` end marker not found"
    goBody acc (l:ls)
      | isEndMarker l = Right $! BS.unlines $ reverse acc
      | otherwise     = goBody (l:acc) ls

    startMarker, endMarker :: BS.ByteString
    startMarker = fromString "{- cabal:"
    endMarker   = fromString "-}"

data PlainOrLiterate
    = PlainHaskell
    | LiterateHaskell

handleScriptCase
  :: Verbosity
  -> PlainOrLiterate
  -> ProjectBaseContext
  -> FilePath
  -> BS.ByteString
  -> IO (ProjectBaseContext, [TargetSelector])
handleScriptCase verbosity pol baseCtx dir scriptContents = do
  (executable, contents') <- readScriptBlockFromScript verbosity pol scriptContents

  -- We need to create a dummy package that lives in our dummy project.
  let
    mainName = case pol of
      PlainHaskell    -> "Main.hs"
      LiterateHaskell -> "Main.lhs"

    sourcePackage = SourcePackage
      { srcpkgPackageId      = pkgId
      , srcpkgDescription    = genericPackageDescription
      , srcpkgSource         = LocalUnpackedPackage dir
      , srcpkgDescrOverride  = Nothing
      }
    genericPackageDescription  = emptyGenericPackageDescription
      { GPD.packageDescription = packageDescription
      , condExecutables        = [("script", CondNode executable' targetBuildDepends [])]
      }
    executable' = executable
      { modulePath = mainName
      , buildInfo = binfo
        { defaultLanguage =
          case defaultLanguage of
            just@(Just _) -> just
            Nothing       -> Just Haskell2010
        }
      }
    binfo@BuildInfo{..} = buildInfo executable
    packageDescription = emptyPackageDescription
      { package = pkgId
      , specVersion = CabalSpecV2_2
      , licenseRaw = Left SPDX.NONE
      }
    pkgId = fakePackageId

  writeGenericPackageDescription (dir </> "fake-package.cabal") genericPackageDescription
  BS.writeFile (dir </> mainName) contents'

  let
    baseCtx' = baseCtx
      { localPackages = localPackages baseCtx ++ [SpecificSourcePackage sourcePackage] }
    targetSelectors = [TargetPackage TargetExplicitNamed [pkgId] Nothing]

  return (baseCtx', targetSelectors)

