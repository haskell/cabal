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
    ( ModuleName, toFilePath )
import Distribution.Client.PackageIndex
    ( allPackagesByName )
import qualified Distribution.PackageDescription as PD
    ( category, packageDescription )
import Distribution.Simple.Utils
         ( intercalate )
import Distribution.Client.Utils
         ( tryCanonicalizePath )
import Language.Haskell.Extension ( Extension )

import Distribution.Client.Types ( packageDescription, SourcePackageDb(..) )
import Control.Applicative ( pure, (<$>), (<*>) )
import Control.Monad ( liftM )
import Data.Char   ( isUpper, isLower, isSpace )
import Data.Either ( partitionEithers )
import Data.List   ( isPrefixOf )
import Data.Maybe  ( mapMaybe, catMaybes, maybeToList )
import Data.Monoid ( mempty, mconcat )
import qualified Data.Set as Set ( fromList, toList )
import System.Directory ( getDirectoryContents,
                          doesDirectoryExist, doesFileExist, getHomeDirectory, )
import Distribution.Compat.Environment ( getEnvironment )
import System.FilePath ( takeExtension, takeBaseName, dropExtension,
                         (</>), (<.>), splitDirectories, makeRelative )
import System.Process ( readProcessWithExitCode )
import System.Exit ( ExitCode(..) )

-- |Guess the package name based on the given root directory
guessPackageName :: FilePath -> IO String
guessPackageName = liftM (last . splitDirectories) . tryCanonicalizePath

-- |Data type of source files found in the working directory
data SourceFileEntry = SourceFileEntry
    { relativeSourcePath :: FilePath
    , moduleName         :: ModuleName
    , fileExtension      :: String
    , imports            :: [ModuleName]
    , extensions         :: [Extension]
    } deriving Show

sfToFileName :: FilePath -> SourceFileEntry -> FilePath
sfToFileName projectRoot (SourceFileEntry relPath m ext _ _)
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
        modules' <- mapM (findImportsAndExts projectRoot) modules
        recMods <- mapM (scanRecursive dir hierarchy) dirs
        return $ concat (modules' : recMods)
    tagIsDir parent entry = do
        isDir <- doesDirectoryExist (parent </> entry)
        return $ (if isDir then Right else Left) entry
    guessModuleName hierarchy entry
        | takeBaseName entry == "Setup" = Nothing
        | ext `elem` sourceExtensions   =
            SourceFileEntry <$> pure relRoot <*> modName <*> pure ext <*> pure [] <*> pure []
        | otherwise = Nothing
      where
        relRoot       = makeRelative projectRoot srcRoot
        unqualModName = dropExtension entry
        modName       = simpleParse
                      $ intercalate "." . reverse $ (unqualModName : hierarchy)
        ext           = case takeExtension entry of '.':e -> e; e -> e
    scanRecursive parent hierarchy entry
      | isUpper (head entry) = scan (parent </> entry) (entry : hierarchy)
      | isLower (head entry) && not (ignoreDir entry) =
          scanForModulesIn projectRoot $ foldl (</>) srcRoot (reverse (entry : hierarchy))
      | otherwise = return []
    ignoreDir ('.':_)  = True
    ignoreDir dir      = dir `elem` ["dist", "_darcs"]

findImportsAndExts :: FilePath -> SourceFileEntry -> IO SourceFileEntry
findImportsAndExts projectRoot sf = do
  s <- readFile (sfToFileName projectRoot sf)

  let modules = mapMaybe
                ( getModName
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

      -- A poor man's LANGUAGE pragma parser.
      exts = mapMaybe simpleParse
           . concatMap getPragmas
           . filter isLANGUAGEPragma
           . map fst
           . drop 1
           . takeWhile (not . null . snd)
           . iterate (takeBraces . snd)
           $ ("",s)

      takeBraces = break (== '}') . dropWhile (/= '{')

      isLANGUAGEPragma = ("{-# LANGUAGE " `isPrefixOf`)

      getPragmas = map trim . splitCommas . takeWhile (/= '#') . drop 13

      splitCommas "" = []
      splitCommas xs = x : splitCommas (drop 1 y)
        where (x,y) = break (==',') xs

  return sf { imports    = modules
            , extensions = exts
            }

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
    , handler <- maybeToList (lookup ext knownSuffixHandlers)
    ]

-- | Guess author and email using darcs and git configuration options. Use
-- the following in decreasing order of preference:
--
-- 1. vcs env vars ($DARCS_EMAIL, $GIT_AUTHOR_*)
-- 2. Local repo configs
-- 3. Global vcs configs
-- 4. The generic $EMAIL
--
-- Name and email are processed separately, so the guess might end up being
-- a name from DARCS_EMAIL and an email from git config.
--
-- Darcs has preference, for tradition's sake.
guessAuthorNameMail :: IO (Flag String, Flag String)
guessAuthorNameMail = fmap authorGuessPure authorGuessIO

-- Ordered in increasing preference, since Flag-as-monoid is identical to
-- Last.
authorGuessPure :: AuthorGuessIO -> AuthorGuess
authorGuessPure (AuthorGuessIO env darcsLocalF darcsGlobalF gitLocal gitGlobal)
    = mconcat
        [ emailEnv env
        , gitGlobal
        , darcsCfg darcsGlobalF
        , gitLocal
        , darcsCfg darcsLocalF
        , gitEnv env
        , darcsEnv env
        ]

authorGuessIO :: IO AuthorGuessIO
authorGuessIO = AuthorGuessIO
    <$> getEnvironment
    <*> (maybeReadFile $ "_darcs" </> "prefs" </> "author")
    <*> (maybeReadFile =<< liftM (</> (".darcs" </> "author")) getHomeDirectory)
    <*> gitCfg Local
    <*> gitCfg Global

-- Types and functions used for guessing the author are now defined:

type AuthorGuess   = (Flag String, Flag String)
type Enviro        = [(String, String)]
data GitLoc        = Local | Global
data AuthorGuessIO = AuthorGuessIO
    Enviro         -- ^ Environment lookup table
    (Maybe String) -- ^ Contents of local darcs author info
    (Maybe String) -- ^ Contents of global darcs author info
    AuthorGuess    -- ^ Git config --local
    AuthorGuess    -- ^ Git config --global

darcsEnv :: Enviro -> AuthorGuess
darcsEnv = maybe mempty nameAndMail . lookup "DARCS_EMAIL"

gitEnv :: Enviro -> AuthorGuess
gitEnv env = (name, email)
  where
    name  = maybeFlag "GIT_AUTHOR_NAME" env
    email = maybeFlag "GIT_AUTHOR_EMAIL" env

darcsCfg :: Maybe String -> AuthorGuess
darcsCfg = maybe mempty nameAndMail

emailEnv :: Enviro -> AuthorGuess
emailEnv env = (mempty, email)
  where
    email = maybeFlag "EMAIL" env

gitCfg :: GitLoc -> IO AuthorGuess
gitCfg which = do
  name <- gitVar which "user.name"
  mail <- gitVar which "user.email"
  return (name, mail)

gitVar :: GitLoc -> String -> IO (Flag String)
gitVar which = fmap happyOutput . gitConfigQuery which

happyOutput :: (ExitCode, a, t) -> Flag a
happyOutput v = case v of
  (ExitSuccess, s, _) -> Flag s
  _                   -> mempty

gitConfigQuery :: GitLoc -> String -> IO (ExitCode, String, String)
gitConfigQuery which key =
    fmap trim' $ readProcessWithExitCode "git" ["config", w, key] ""
  where
    w = case which of
        Local  -> "--local"
        Global -> "--global"
    trim' (a, b, c) = (a, trim b, c)

maybeFlag :: String -> Enviro -> Flag String
maybeFlag k = maybe mempty Flag . lookup k

maybeReadFile :: String -> IO (Maybe String)
maybeReadFile f = do
    exists <- doesFileExist f
    if exists
        then fmap Just $ readFile f
        else return Nothing

-- |Get list of categories used in hackage. NOTE: Very slow, needs to be cached
knownCategories :: SourcePackageDb -> [String]
knownCategories (SourcePackageDb sourcePkgIndex _) = nubSet
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

trim :: String -> String
trim = removeLeadingSpace . reverse . removeLeadingSpace . reverse
  where
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
