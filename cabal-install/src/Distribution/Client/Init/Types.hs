{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      :  Distribution.Client.Init.Types
-- Copyright   :  (c) Brent Yorgey, Benedikt Huber 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Some types used by the 'cabal init' command.
module Distribution.Client.Init.Types
  ( -- * Data
    InitFlags (..)

    -- ** Targets and descriptions
  , PkgDescription (..)
  , LibTarget (..)
  , ExeTarget (..)
  , TestTarget (..)

    -- ** package types
  , PackageType (..)

    -- ** Main file
  , HsFilePath (..)
  , HsFileType (..)
  , fromHsFilePath
  , toHsFilePath
  , toLiterateHs
  , toStandardHs
  , mkLiterate
  , isHsFilePath

    -- * Typeclasses
  , Interactive (..)
  , BreakException (..)
  , PurePrompt (..)
  , evalPrompt
  , Severity (..)

    -- * Aliases
  , IsLiterate
  , IsSimple

    -- * File creator opts
  , WriteOpts (..)
  , ProjectSettings (..)

    -- * Formatters
  , FieldAnnotation (..)

    -- * Other conveniences
  , DefaultPrompt (..)
  ) where

import Distribution.Client.Compat.Prelude as P hiding (getLine, putStr, putStrLn)
import qualified Distribution.Client.Compat.Prelude as P
import Prelude (read)

import Control.Monad.Catch

import Data.List.NonEmpty (fromList)

import Distribution.CabalSpecVersion
import Distribution.Client.Utils as P
import Distribution.Fields.Pretty
import Distribution.ModuleName
import qualified Distribution.Package as P
import Distribution.Simple.Setup (Flag (..))
import Distribution.Verbosity (silent)
import Distribution.Version
import Language.Haskell.Extension (Extension, Language (..))
import qualified System.IO

import qualified Distribution.Compat.Environment as P
import Distribution.FieldGrammar.Newtypes (SpecLicense)
import qualified System.Directory as P
import System.FilePath
import qualified System.Process as Process

-- -------------------------------------------------------------------- --
-- Flags

-- | InitFlags is a subset of flags available in the
-- @.cabal@ file that represent options that are relevant to the
-- init command process.
data InitFlags = InitFlags
  { interactive :: Flag Bool
  , quiet :: Flag Bool
  , packageDir :: Flag FilePath
  , noComments :: Flag Bool
  , minimal :: Flag Bool
  , simpleProject :: Flag Bool
  , packageName :: Flag P.PackageName
  , version :: Flag Version
  , cabalVersion :: Flag CabalSpecVersion
  , license :: Flag SpecLicense
  , author :: Flag String
  , email :: Flag String
  , homepage :: Flag String
  , synopsis :: Flag String
  , category :: Flag String
  , extraSrc :: Flag [String]
  , extraDoc :: Flag [String]
  , packageType :: Flag PackageType
  , mainIs :: Flag FilePath
  , language :: Flag Language
  , exposedModules :: Flag [ModuleName]
  , otherModules :: Flag [ModuleName]
  , otherExts :: Flag [Extension]
  , dependencies :: Flag [P.Dependency]
  , applicationDirs :: Flag [String]
  , sourceDirs :: Flag [String]
  , buildTools :: Flag [String]
  , initializeTestSuite :: Flag Bool
  , testDirs :: Flag [String]
  , initHcPath :: Flag FilePath
  , initVerbosity :: Flag Verbosity
  , overwrite :: Flag Bool
  }
  deriving (Eq, Show, Generic)

instance Monoid InitFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup InitFlags where
  (<>) = gmappend

-- -------------------------------------------------------------------- --
-- Targets

-- | 'PkgDescription' represents the relevant options set by the
-- user when building a package description during the init command
-- process.
data PkgDescription = PkgDescription
  { _pkgCabalVersion :: CabalSpecVersion
  , _pkgName :: P.PackageName
  , _pkgVersion :: Version
  , _pkgLicense :: SpecLicense
  , _pkgAuthor :: String
  , _pkgEmail :: String
  , _pkgHomePage :: String
  , _pkgSynopsis :: String
  , _pkgCategory :: String
  , _pkgExtraSrcFiles :: Set String
  , _pkgExtraDocFiles :: Maybe (Set String)
  }
  deriving (Show, Eq)

-- | 'LibTarget' represents the relevant options set by the
-- user when building a library package during the init command
-- process.
data LibTarget = LibTarget
  { _libSourceDirs :: [String]
  , _libLanguage :: Language
  , _libExposedModules :: NonEmpty ModuleName
  , _libOtherModules :: [ModuleName]
  , _libOtherExts :: [Extension]
  , _libDependencies :: [P.Dependency]
  , _libBuildTools :: [P.Dependency]
  }
  deriving (Show, Eq)

-- | 'ExeTarget' represents the relevant options set by the
-- user when building an executable package.
data ExeTarget = ExeTarget
  { _exeMainIs :: HsFilePath
  , _exeApplicationDirs :: [String]
  , _exeLanguage :: Language
  , _exeOtherModules :: [ModuleName]
  , _exeOtherExts :: [Extension]
  , _exeDependencies :: [P.Dependency]
  , _exeBuildTools :: [P.Dependency]
  }
  deriving (Show, Eq)

-- | 'TestTarget' represents the relevant options set by the
-- user when building a library package.
data TestTarget = TestTarget
  { _testMainIs :: HsFilePath
  , _testDirs :: [String]
  , _testLanguage :: Language
  , _testOtherModules :: [ModuleName]
  , _testOtherExts :: [Extension]
  , _testDependencies :: [P.Dependency]
  , _testBuildTools :: [P.Dependency]
  }
  deriving (Show, Eq)

-- -------------------------------------------------------------------- --
-- File creator options

data WriteOpts = WriteOpts
  { _optOverwrite :: Bool
  , _optMinimal :: Bool
  , _optNoComments :: Bool
  , _optVerbosity :: Verbosity
  , _optPkgDir :: FilePath
  , _optPkgType :: PackageType
  , _optPkgName :: P.PackageName
  , _optCabalSpec :: CabalSpecVersion
  }
  deriving (Eq, Show)

data ProjectSettings = ProjectSettings
  { _pkgOpts :: WriteOpts
  , _pkgDesc :: PkgDescription
  , _pkgLibTarget :: Maybe LibTarget
  , _pkgExeTarget :: Maybe ExeTarget
  , _pkgTestTarget :: Maybe TestTarget
  }
  deriving (Eq, Show)

-- -------------------------------------------------------------------- --
-- Other types

-- | Enum to denote whether the user wants to build a library target,
-- executable target, library and executable targets, or a standalone test suite.
data PackageType = Library | Executable | LibraryAndExecutable | TestSuite
  deriving (Eq, Show, Generic)

data HsFileType
  = Literate
  | Standard
  | InvalidHsPath
  deriving (Eq, Show)

data HsFilePath = HsFilePath
  { _hsFilePath :: FilePath
  , _hsFileType :: HsFileType
  }
  deriving (Eq)

instance Show HsFilePath where
  show (HsFilePath fp ty) = case ty of
    Literate -> fp
    Standard -> fp
    InvalidHsPath -> "Invalid haskell source file: " ++ fp

fromHsFilePath :: HsFilePath -> Maybe FilePath
fromHsFilePath (HsFilePath fp ty) = case ty of
  Literate -> Just fp
  Standard -> Just fp
  InvalidHsPath -> Nothing

isHsFilePath :: FilePath -> Bool
isHsFilePath fp = case _hsFileType $ toHsFilePath fp of
  InvalidHsPath -> False
  _ -> True

toHsFilePath :: FilePath -> HsFilePath
toHsFilePath fp
  | takeExtension fp == ".lhs" = HsFilePath fp Literate
  | takeExtension fp == ".hs" = HsFilePath fp Standard
  | otherwise = HsFilePath fp InvalidHsPath

toLiterateHs :: HsFilePath -> HsFilePath
toLiterateHs (HsFilePath fp Standard) =
  HsFilePath
    (dropExtension fp ++ ".lhs")
    Literate
toLiterateHs a = a

toStandardHs :: HsFilePath -> HsFilePath
toStandardHs (HsFilePath fp Literate) =
  HsFilePath
    (dropExtension fp ++ ".hs")
    Standard
toStandardHs a = a

mkLiterate :: HsFilePath -> [String] -> [String]
mkLiterate (HsFilePath _ Literate) hs =
  (\line -> if null line then line else "> " ++ line) <$> hs
mkLiterate _ hs = hs

-- -------------------------------------------------------------------- --
-- Interactive prompt monad

newtype PurePrompt a = PurePrompt
  { _runPrompt
      :: NonEmpty String
      -> Either BreakException (a, NonEmpty String)
  }
  deriving (Functor)

evalPrompt :: PurePrompt a -> NonEmpty String -> a
evalPrompt act s = case _runPrompt act s of
  Left e -> error $ show e
  Right (a, _) -> a

instance Applicative PurePrompt where
  pure a = PurePrompt $ \s -> Right (a, s)
  PurePrompt ff <*> PurePrompt aa = PurePrompt $ \s -> case ff s of
    Left e -> Left e
    Right (f, s') -> case aa s' of
      Left e -> Left e
      Right (a, s'') -> Right (f a, s'')

instance Monad PurePrompt where
  return = pure
  PurePrompt a >>= k = PurePrompt $ \s -> case a s of
    Left e -> Left e
    Right (a', s') -> _runPrompt (k a') s'

class Monad m => Interactive m where
  -- input functions
  getLine :: m String
  readFile :: FilePath -> m String
  getCurrentDirectory :: m FilePath
  getHomeDirectory :: m FilePath
  getDirectoryContents :: FilePath -> m [FilePath]
  listDirectory :: FilePath -> m [FilePath]
  doesDirectoryExist :: FilePath -> m Bool
  doesFileExist :: FilePath -> m Bool
  canonicalizePathNoThrow :: FilePath -> m FilePath
  readProcessWithExitCode :: FilePath -> [String] -> String -> m (ExitCode, String, String)
  getEnvironment :: m [(String, String)]
  getCurrentYear :: m Integer
  listFilesInside :: (FilePath -> m Bool) -> FilePath -> m [FilePath]
  listFilesRecursive :: FilePath -> m [FilePath]

  -- output functions
  putStr :: String -> m ()
  putStrLn :: String -> m ()
  createDirectory :: FilePath -> m ()
  removeDirectory :: FilePath -> m ()
  writeFile :: FilePath -> String -> m ()
  removeExistingFile :: FilePath -> m ()
  copyFile :: FilePath -> FilePath -> m ()
  renameDirectory :: FilePath -> FilePath -> m ()
  hFlush :: System.IO.Handle -> m ()
  message :: Verbosity -> Severity -> String -> m ()

  -- misc functions
  break :: m Bool
  throwPrompt :: BreakException -> m a

instance Interactive IO where
  getLine = P.getLine
  readFile = P.readFile
  getCurrentDirectory = P.getCurrentDirectory
  getHomeDirectory = P.getHomeDirectory
  getDirectoryContents = P.getDirectoryContents
  listDirectory = P.listDirectory
  doesDirectoryExist = P.doesDirectoryExist
  doesFileExist = P.doesFileExist
  canonicalizePathNoThrow = P.canonicalizePathNoThrow
  readProcessWithExitCode = Process.readProcessWithExitCode
  getEnvironment = P.getEnvironment
  getCurrentYear = P.getCurrentYear
  listFilesInside = P.listFilesInside
  listFilesRecursive = P.listFilesRecursive

  putStr = P.putStr
  putStrLn = P.putStrLn
  createDirectory = P.createDirectory
  removeDirectory = P.removeDirectoryRecursive
  writeFile = P.writeFile
  removeExistingFile = P.removeExistingFile
  copyFile = P.copyFile
  renameDirectory = P.renameDirectory
  hFlush = System.IO.hFlush
  message q severity msg
    | q == silent = pure ()
    | otherwise = putStrLn $ "[" ++ displaySeverity severity ++ "] " ++ msg
  break = return False
  throwPrompt = throwM

instance Interactive PurePrompt where
  getLine = pop
  readFile !_ = pop
  getCurrentDirectory = popAbsolute
  getHomeDirectory = popAbsolute

  -- expects stack input of form "[\"foo\", \"bar\", \"baz\"]"
  getDirectoryContents !_ = popList
  listDirectory !_ = popList
  doesDirectoryExist !_ = popBool
  doesFileExist !_ = popBool
  canonicalizePathNoThrow !_ = popAbsolute
  readProcessWithExitCode !_ !_ !_ = do
    input <- pop
    return (ExitSuccess, input, "")
  getEnvironment = fmap (map read) popList
  getCurrentYear = fmap read pop
  listFilesInside pred' !_ = do
    input <- map splitDirectories <$> popList
    map joinPath <$> filterM (fmap and . traverse pred') input
  listFilesRecursive !_ = popList

  putStr !_ = return ()
  putStrLn !_ = return ()
  createDirectory !d = checkInvalidPath d ()
  removeDirectory !d = checkInvalidPath d ()
  writeFile !f !_ = checkInvalidPath f ()
  removeExistingFile !f = checkInvalidPath f ()
  copyFile !f !_ = checkInvalidPath f ()
  renameDirectory !d !_ = checkInvalidPath d ()
  hFlush _ = return ()
  message !_ !severity !msg = case severity of
    Error -> PurePrompt $ \_ ->
      Left $
        BreakException
          (displaySeverity severity ++ ": " ++ msg)
    _ -> return ()

  break = return True
  throwPrompt (BreakException e) = PurePrompt $ \s ->
    Left $
      BreakException
        ("Error: " ++ e ++ "\nStacktrace: " ++ show s)

pop :: PurePrompt String
pop = PurePrompt $ \(p :| ps) -> Right (p, fromList ps)

popAbsolute :: PurePrompt String
popAbsolute = do
  input <- pop
  return $ "/home/test/" ++ input

popBool :: PurePrompt Bool
popBool =
  pop >>= \case
    "True" -> pure True
    "False" -> pure False
    s -> throwPrompt $ BreakException $ "popBool: " ++ s

popList :: PurePrompt [String]
popList =
  pop >>= \a -> case P.safeRead a of
    Nothing -> throwPrompt $ BreakException ("popList: " ++ show a)
    Just as -> return as

checkInvalidPath :: String -> a -> PurePrompt a
checkInvalidPath path act =
  -- The check below is done this way so it's easier to append
  -- more invalid paths in the future, if necessary
  if path `elem` ["."]
    then throwPrompt $ BreakException $ "Invalid path: " ++ path
    else return act

-- | A pure exception thrown exclusively by the pure prompter
-- to cancel infinite loops in the prompting process.
--
-- For example, in order to break on parse errors, or user-driven
-- continuations that do not make sense to test.
newtype BreakException = BreakException String deriving (Eq, Show)

instance Exception BreakException

-- | Used to inform the intent of prompted messages.
data Severity = Info | Warning | Error deriving (Eq)

displaySeverity :: Severity -> String
displaySeverity severity = case severity of
  Info -> "Info"
  Warning -> "Warn"
  Error -> "Err"

-- | Convenience alias for the literate haskell flag
type IsLiterate = Bool

-- | Convenience alias for generating simple projects
type IsSimple = Bool

-- | Defines whether or not a prompt will have a default value,
--   is optional, or is mandatory.
data DefaultPrompt t
  = DefaultPrompt t
  | OptionalPrompt
  | MandatoryPrompt
  deriving (Eq, Functor)

-- -------------------------------------------------------------------- --
-- Field annotation for pretty formatters

-- | Annotations for cabal file PrettyField.
data FieldAnnotation = FieldAnnotation
  { annCommentedOut :: Bool
  -- ^ True iif the field and its contents should be commented out.
  , annCommentLines :: CommentPosition
  -- ^ Comment lines to place before the field or section.
  }
