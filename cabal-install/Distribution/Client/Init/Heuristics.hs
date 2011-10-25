{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Init.Heuristics
-- Copyright   :  (c) Benedikt Huber 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Heuristics for creating initial cabal files.
--
-----------------------------------------------------------------------------
module Distribution.Client.Init.Heuristics (
    guessPackageName,
    scanForModules,     SourceFileEntry(..),
    neededBuildPrograms,
    guessAuthorNameMail,
    knownCategories,
) where
import Distribution.Text         (simpleParse)
import Distribution.Simple.Setup (Flag(..))
import Distribution.ModuleName
    ( ModuleName, fromString, toFilePath )
import Distribution.Client.PackageIndex
    ( allPackagesByName )
import qualified Distribution.PackageDescription as PD
    ( category, packageDescription )
import Distribution.Simple.Utils
         ( intercalate )

import Distribution.Client.Types ( packageDescription, SourcePackageDb(..) )
import Control.Monad (liftM )
import Data.Char   ( isUpper, isLower, isSpace )
#if MIN_VERSION_base(3,0,3)
import Data.Either ( partitionEithers )
#endif
import Data.List   ( isPrefixOf )
import Data.Maybe  ( catMaybes )
import Data.Monoid ( mempty, mappend )
import qualified Data.Set as Set ( fromList, toList )
import System.Directory ( getDirectoryContents, doesDirectoryExist, doesFileExist,
                          getHomeDirectory, canonicalizePath )
import System.Environment ( getEnvironment )
import System.FilePath ( takeExtension, takeBaseName, dropExtension,
                         (</>), (<.>), splitDirectories, makeRelative )

-- |Guess the package name based on the given root directory
guessPackageName :: FilePath -> IO String
guessPackageName = liftM (last . splitDirectories) . canonicalizePath

-- |Data type of source files found in the working directory
data SourceFileEntry = SourceFileEntry
    { relativeSourcePath :: FilePath
    , moduleName         :: ModuleName
    , fileExtension      :: String
    , imports            :: [ModuleName]
    } deriving Show

sfToFileName :: FilePath -> SourceFileEntry -> FilePath
sfToFileName projectRoot (SourceFileEntry relPath m ext _)
  = projectRoot </> relPath </> toFilePath m <.> ext

-- |Search for source files in the given directory
-- and return pairs of guessed haskell source path and
-- module names.
scanForModules :: FilePath -> IO [SourceFileEntry]
scanForModules rootDir = scanForModulesIn rootDir rootDir

scanForModulesIn :: FilePath -> FilePath -> IO [SourceFileEntry]
scanForModulesIn projectRoot srcRoot = scan srcRoot []
  where
    scan dir hierarchy = do
        entries <- getDirectoryContents (projectRoot </> dir)
        (files, dirs) <- liftM partitionEithers (mapM (tagIsDir dir) entries)
        let modules = catMaybes [ guessModuleName hierarchy file
                                | file <- files
                                , isUpper (head file) ]
        modules' <- mapM (findImports projectRoot) modules
        recMods <- mapM (scanRecursive dir hierarchy) dirs
        return $ concat (modules' : recMods)
    tagIsDir parent entry = do
        isDir <- doesDirectoryExist (parent </> entry)
        return $ (if isDir then Right else Left) entry
    guessModuleName hierarchy entry
        | takeBaseName entry == "Setup" = Nothing
        | ext `elem` sourceExtensions = Just $ SourceFileEntry relRoot modName ext []
        | otherwise = Nothing
      where
        relRoot = makeRelative projectRoot srcRoot
        unqualModName = dropExtension entry
        modName = fromString $ intercalate "." . reverse $ (unqualModName : hierarchy)
        ext = case takeExtension entry of '.':e -> e; e -> e
    scanRecursive parent hierarchy entry
      | isUpper (head entry) = scan (parent </> entry) (entry : hierarchy)
      | isLower (head entry) && not (ignoreDir entry) =
          scanForModulesIn projectRoot $ foldl (</>) srcRoot (entry : hierarchy)
      | otherwise = return []
    ignoreDir ('.':_)  = True
    ignoreDir dir      = dir `elem` ["dist", "_darcs"]

findImports :: FilePath -> SourceFileEntry -> IO SourceFileEntry
findImports projectRoot sf = do
  s <- readFile (sfToFileName projectRoot sf)

  let modules = catMaybes
              . map ( getModName
                    . drop 1
                    . filter (not . null)
                    . dropWhile (/= "import")
                    . words
                    )
              . filter (not . ("--" `isPrefixOf`)) -- poor man's comment filtering
              . lines
              $ s

      -- XXX we should probably make a better attempt at parsing
      -- comments above.  Unfortunately we can't use a full-fledged
      -- Haskell parser since cabal's dependencies must be kept at a
      -- minimum.

  return sf { imports = modules }

 where getModName :: [String] -> Maybe ModuleName
       getModName []               = Nothing
       getModName ("qualified":ws) = getModName ws
       getModName (ms:_)           = simpleParse ms



-- Unfortunately we cannot use the version exported by Distribution.Simple.Program
knownSuffixHandlers :: [(String,String)]
knownSuffixHandlers =
  [ ("gc",     "greencard")
  , ("chs",    "chs")
  , ("hsc",    "hsc2hs")
  , ("x",      "alex")
  , ("y",      "happy")
  , ("ly",     "happy")
  , ("cpphs",  "cpp")
  ]

sourceExtensions :: [String]
sourceExtensions = "hs" : "lhs" : map fst knownSuffixHandlers

neededBuildPrograms :: [SourceFileEntry] -> [String]
neededBuildPrograms entries =
    [ handler
    | ext <- nubSet (map fileExtension entries)
    , handler <- maybe [] (:[]) (lookup ext knownSuffixHandlers)
    ]

-- |Guess author and email
guessAuthorNameMail :: IO (Flag String, Flag String)
guessAuthorNameMail =
  update (readFromFile authorRepoFile) mempty >>=
  update (getAuthorHome >>= readFromFile) >>=
  update readFromEnvironment
  where
    update _ info@(Flag _, Flag _) = return info
    update extract info = liftM (`mappend` info) extract -- prefer info
    readFromFile file = do
      exists <- doesFileExist file
      if exists then liftM nameAndMail (readFile file) else return mempty
    readFromEnvironment = fmap extractFromEnvironment getEnvironment
    extractFromEnvironment env =
        let darcsEmailEnv = maybe mempty nameAndMail (lookup "DARCS_EMAIL" env)
            emailEnv      = maybe mempty (\e -> (mempty, Flag e)) (lookup "EMAIL" env)
        in darcsEmailEnv `mappend` emailEnv
    getAuthorHome   = liftM (</> (".darcs" </> "author")) getHomeDirectory
    authorRepoFile  = "_darcs" </> "prefs" </> "author"

-- |Get list of categories used in hackage. NOTE: Very slow, needs to be cached
knownCategories :: SourcePackageDb -> [String]
knownCategories (SourcePackageDb sourcePkgIndex _) = nubSet $
    [ cat | pkg <- map head (allPackagesByName sourcePkgIndex)
          , let catList = (PD.category . PD.packageDescription . packageDescription) pkg
          , cat <- splitString ',' catList
    ]

-- Parse name and email, from darcs pref files or environment variable
nameAndMail :: String -> (Flag String, Flag String)
nameAndMail str
  | all isSpace nameOrEmail = mempty
  | null erest = (mempty, Flag $ trim nameOrEmail)
  | otherwise  = (Flag $ trim nameOrEmail, Flag email)
  where
    (nameOrEmail,erest) = break (== '<') str
    (email,_)           = break (== '>') (tail erest)
    trim                = removeLeadingSpace . reverse . removeLeadingSpace . reverse
    removeLeadingSpace  = dropWhile isSpace

-- split string at given character, and remove whitespaces
splitString :: Char -> String -> [String]
splitString sep str = go str where
    go s = if null s' then [] else tok : go rest where
      s' = dropWhile (\c -> c == sep || isSpace c) s
      (tok,rest) = break (==sep) s'

nubSet :: (Ord a) => [a] -> [a]
nubSet = Set.toList . Set.fromList

{-
test db testProjectRoot = do
  putStrLn "Guessed package name"
  (guessPackageName >=> print) testProjectRoot
  putStrLn "Guessed name and email"
  guessAuthorNameMail >>= print

  mods <- scanForModules testProjectRoot

  putStrLn "Guessed modules"
  mapM_ print mods
  putStrLn "Needed build programs"
  print (neededBuildPrograms mods)

  putStrLn "List of known categories"
  print $ knownCategories db
-}

#if MIN_VERSION_base(3,0,3)
#else
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers = foldr (either left right) ([],[])
 where
   left  a (l, r) = (a:l, r)
   right a (l, r) = (l, a:r)
#endif
