{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Init
-- Copyright   :  (c) Brent Yorgey 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Implementation of the 'cabal init' command, which creates an initial .cabal
-- file for a project.
--
-----------------------------------------------------------------------------

module Distribution.Client.Init (

    -- * Commands
    initCabal

  ) where

import System.IO
  ( hSetBuffering, stdout, BufferMode(..) )
import System.Directory
  ( getCurrentDirectory )
import Data.Time
  ( getCurrentTime, utcToLocalTime, toGregorian, localDay, getCurrentTimeZone )

import Data.List
  ( intersperse, (\\) )
import Data.Maybe
  ( fromMaybe, isJust )
import Data.Traversable
  ( traverse )
import Control.Monad
  ( when )
#if MIN_VERSION_base(3,0,0)
import Control.Monad
  ( (>=>), join )
#endif

import Text.PrettyPrint hiding (mode, cat)

import Data.Version
  ( Version(..) )
import Distribution.Version
  ( orLaterVersion )

import Distribution.Client.Init.Types
  ( InitFlags(..), PackageType(..), Category(..) )
import Distribution.Client.Init.Licenses
  ( bsd3, gplv2, gplv3, lgpl2, lgpl3 )
import Distribution.Client.Init.Heuristics
  ( guessPackageName, guessAuthorNameMail, SourceFileEntry(..), scanForModules, neededBuildPrograms )

import Distribution.License
  ( License(..), knownLicenses )
import Distribution.ModuleName
  ( ) -- for the Text instance

import Distribution.ReadE
  ( runReadE, readP_to_E )
import Distribution.Simple.Setup
  ( Flag(..), flagToMaybe )
import Distribution.Text
  ( display, Text(..) )

initCabal :: InitFlags -> IO ()
initCabal initFlags = do
  hSetBuffering stdout NoBuffering

  initFlags' <- extendFlags initFlags

  writeLicense initFlags'
  writeSetupFile initFlags'
  success <- writeCabalFile initFlags'

  when success $ generateWarnings initFlags'

---------------------------------------------------------------------------
--  Flag acquisition  -----------------------------------------------------
---------------------------------------------------------------------------

-- | Fill in more details by guessing, discovering, or prompting the
--   user.
extendFlags :: InitFlags -> IO InitFlags
extendFlags =  getPackageName
           >=> getVersion
           >=> getLicense
           >=> getAuthorInfo
           >=> getHomepage
           >=> getSynopsis
           >=> getCategory
           >=> getLibOrExec
           >=> getGenComments
           >=> getSrcDir
           >=> getModulesAndBuildTools

-- | Combine two actions which may return a value, preferring the first. That
--   is, run the second action only if the first doesn't return a value.
infixr 1 ?>>
(?>>) :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
f ?>> g = do
  ma <- f
  if isJust ma
    then return ma
    else g

-- | Witness the isomorphism between Maybe and Flag.
maybeToFlag :: Maybe a -> Flag a
maybeToFlag = maybe NoFlag Flag

-- | Get the package name: use the package directory (supplied, or the current
--   directory by default) as a guess.
getPackageName :: InitFlags -> IO InitFlags
getPackageName flags = do
  guess    <-     traverse guessPackageName (flagToMaybe $ packageDir flags)
              ?>> Just `fmap` (getCurrentDirectory >>= guessPackageName)

  pkgName' <-     return (flagToMaybe $ packageName flags)
              ?>> maybePrompt flags (promptStr "Package name" guess)
              ?>> return guess

  return $ flags { packageName = maybeToFlag pkgName' }

-- | Package version: use 0.1.0.0 as a last resort, but try prompting the user
--  if possible.
getVersion :: InitFlags -> IO InitFlags
getVersion flags = do
  let v = Just $ Version { versionBranch = [0,1,0,0], versionTags = [] }
  v' <-     return (flagToMaybe $ version flags)
        ?>> maybePrompt flags (prompt "Package version" v)
        ?>> return v
  return $ flags { version = maybeToFlag v' }

-- | Choose a license.
getLicense :: InitFlags -> IO InitFlags
getLicense flags = do
  lic <-     return (flagToMaybe $ license flags)
         ?>> fmap (fmap (either UnknownLicense id) . join)
                  (maybePrompt flags
                    (promptListOptional "Please choose a license" listedLicenses))
  return $ flags { license = maybeToFlag lic }
  where
    listedLicenses =
      knownLicenses \\ [GPL Nothing, LGPL Nothing, OtherLicense]

-- | The author's name and email. Prompt, or try to guess from an existing
--   darcs repo.
getAuthorInfo :: InitFlags -> IO InitFlags
getAuthorInfo flags = do
  (authorName, authorEmail)  <- (\(a,e) -> (flagToMaybe a, flagToMaybe e)) `fmap` guessAuthorNameMail
  authorName'  <-     return (flagToMaybe $ author flags)
                  ?>> maybePrompt flags (promptStr "Author name" authorName)
                  ?>> return authorName

  authorEmail' <-     return (flagToMaybe $ email flags)
                  ?>> maybePrompt flags (promptStr "Maintainer email" authorEmail)
                  ?>> return authorEmail

  return $ flags { author = maybeToFlag authorName'
                 , email  = maybeToFlag authorEmail'
                 }

-- | Prompt for a homepage URL.
getHomepage :: InitFlags -> IO InitFlags
getHomepage flags = do
  hp  <- queryHomepage
  hp' <-     return (flagToMaybe $ homepage flags)
         ?>> maybePrompt flags (promptStr "Project homepage URL" hp)
         ?>> return hp

  return $ flags { homepage = maybeToFlag hp' }

-- | Right now this does nothing, but it could be changed to do some
--   intelligent guessing.
queryHomepage :: IO (Maybe String)
queryHomepage = return Nothing     -- get default remote darcs repo?

-- | Prompt for a project synopsis.
getSynopsis :: InitFlags -> IO InitFlags
getSynopsis flags = do
  syn <-     return (flagToMaybe $ synopsis flags)
         ?>> maybePrompt flags (promptStr "Project synopsis" Nothing)

  return $ flags { synopsis = maybeToFlag syn }

-- | Prompt for a package category.
--   Note that it should be possible to do some smarter guessing here too, i.e.
--   look at the name of the top level source directory.
getCategory :: InitFlags -> IO InitFlags
getCategory flags = do
  cat <-     return (flagToMaybe $ category flags)
         ?>> fmap join (maybePrompt flags
                         (promptListOptional "Project category" [Codec ..]))
  return $ flags { category = maybeToFlag cat }

-- | Ask whether the project builds a library or executable.
getLibOrExec :: InitFlags -> IO InitFlags
getLibOrExec flags = do
  isLib <-     return (flagToMaybe $ packageType flags)
           ?>> maybePrompt flags (either (const Library) id `fmap`
                                   (promptList "What does the package build"
                                               [Library, Executable]
                                               Nothing display False))
           ?>> return (Just Library)

  return $ flags { packageType = maybeToFlag isLib }

-- | Ask whether to generate explanitory comments.
getGenComments :: InitFlags -> IO InitFlags
getGenComments flags = do
  genComments <-     return (flagToMaybe $ noComments flags)
                 ?>> maybePrompt flags (promptYesNo promptMsg (Just False))
                 ?>> return (Just False)
  return $ flags { noComments = maybeToFlag (fmap not genComments) }
  where
    promptMsg = "Include documentation on what each field means y/n"

-- | Try to guess the source root directory (don't prompt the user).
getSrcDir :: InitFlags -> IO InitFlags
getSrcDir flags = do
  srcDirs <-     return (sourceDirs flags)
             ?>> guessSourceDirs

  return $ flags { sourceDirs = srcDirs }

-- XXX
-- | Try to guess source directories.
guessSourceDirs :: IO (Maybe [String])
guessSourceDirs = return Nothing

-- | Get the list of exposed modules and extra tools needed to build them.
getModulesAndBuildTools :: InitFlags -> IO InitFlags
getModulesAndBuildTools flags = do
  dir <- fromMaybe getCurrentDirectory
                   (fmap return . flagToMaybe $ packageDir flags)

  -- XXX really should use guessed source roots.
  sourceFiles <- scanForModules dir

  mods <-      return (exposedModules flags)
           ?>> (return . Just . map moduleName $ sourceFiles)

  tools <-     return (buildTools flags)
           ?>> (return . Just . neededBuildPrograms $ sourceFiles)

  return $ flags { exposedModules = mods
                 , buildTools     = tools }

---------------------------------------------------------------------------
--  Prompting/user interaction  -------------------------------------------
---------------------------------------------------------------------------

-- | Run a prompt or not based on the nonInteractive flag of the
--   InitFlags structure.
maybePrompt :: InitFlags -> IO t -> IO (Maybe t)
maybePrompt flags p =
  case nonInteractive flags of
    Flag True -> return Nothing
    _         -> Just `fmap` p

-- | Create a prompt with optional default value that returns a
--   String.
promptStr :: String -> Maybe String -> IO String
promptStr = promptDefault' Just id

-- | Create a yes/no prompt with optional default value.
--
promptYesNo :: String -> Maybe Bool -> IO Bool
promptYesNo =
    promptDefault' recogniseYesNo showYesNo
  where
    recogniseYesNo s | s == "y" || s == "Y" = Just True
                     | s == "n" || s == "N" = Just False
                     | otherwise            = Nothing
    showYesNo True  = "y"
    showYesNo False = "n"

-- | Create a prompt with optional default value that returns a value
--   of some Text instance.
prompt :: Text t => String -> Maybe t -> IO t
prompt = promptDefault'
           (either (const Nothing) Just . runReadE (readP_to_E id parse))
           display

-- | Create a prompt with an optional default value.
promptDefault' :: (String -> Maybe t)       -- ^ parser
               -> (t -> String)             -- ^ pretty-printer
               -> String                    -- ^ prompt message
               -> Maybe t                   -- ^ optional default value
               -> IO t
promptDefault' parser pretty pr def = do
  putStr $ mkDefPrompt pr (pretty `fmap` def)
  inp <- getLine
  case (inp, def) of
    ("", Just d)  -> return d
    _  -> case parser inp of
            Just t  -> return t
            Nothing -> do putStrLn $ "Couldn't parse " ++ inp ++ ", please try again!"
                          promptDefault' parser pretty pr def

-- | Create a prompt from a prompt string and a String representation
--   of an optional default value.
mkDefPrompt :: String -> Maybe String -> String
mkDefPrompt pr def = pr ++ "?" ++ defStr def
  where defStr Nothing  = " "
        defStr (Just s) = " [default: " ++ s ++ "] "

promptListOptional :: (Text t, Eq t)
                   => String            -- ^ prompt
                   -> [t]               -- ^ choices
                   -> IO (Maybe (Either String t))
promptListOptional pr choices =
    fmap rearrange
  $ promptList pr (Nothing : map Just choices) (Just Nothing)
               (maybe "(none)" display) True
  where
    rearrange = either (Just . Left) (maybe Nothing (Just . Right))

-- | Create a prompt from a list of items.
promptList :: Eq t
           => String            -- ^ prompt
           -> [t]               -- ^ choices
           -> Maybe t           -- ^ optional default value
           -> (t -> String)     -- ^ show an item
           -> Bool              -- ^ whether to allow an 'other' option
           -> IO (Either String t)
promptList pr choices def displayItem other = do
  putStrLn $ pr ++ ":"
  let options1 = map (\c -> (Just c == def, displayItem c)) choices
      options2 = zip ([1..]::[Int])
                     (options1 ++ if other then [(False, "Other (specify)")]
                                           else [])
  mapM_ (putStrLn . \(n,(i,s)) -> showOption n i ++ s) options2
  promptList' displayItem (length options2) choices def other
 where showOption n i | n < 10 = " " ++ star i ++ " " ++ rest
                      | otherwise = " " ++ star i ++ rest
                  where rest = show n ++ ") "
                        star True = "*"
                        star False = " "

promptList' :: (t -> String) -> Int -> [t] -> Maybe t -> Bool -> IO (Either String t)
promptList' displayItem numChoices choices def other = do
  putStr $ mkDefPrompt "Your choice" (displayItem `fmap` def)
  inp <- getLine
  case (inp, def) of
    ("", Just d) -> return $ Right d
    _  -> case readMaybe inp of
            Nothing -> invalidChoice inp
            Just n  -> getChoice n
 where invalidChoice inp = do putStrLn $ inp ++ " is not a valid choice."
                              promptList' displayItem numChoices choices def other
       getChoice n | n < 1 || n > numChoices = invalidChoice (show n)
                   | n < numChoices ||
                     (n == numChoices && not other)
                                  = return . Right $ choices !! (n-1)
                   | otherwise    = Left `fmap` promptStr "Please specify" Nothing

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(a,"")] -> Just a
                _        -> Nothing

---------------------------------------------------------------------------
--  File generation  ------------------------------------------------------
---------------------------------------------------------------------------

writeLicense :: InitFlags -> IO ()
writeLicense flags = do
  message flags "Generating LICENSE..."
  year <- getYear
  let licenseFile =
        case license flags of
          Flag BSD3 -> Just $ bsd3 (fromMaybe "???"
                                  . flagToMaybe
                                  . author
                                  $ flags)
                              (show year)

          Flag (GPL (Just (Version {versionBranch = [2]})))
            -> Just gplv2

          Flag (GPL (Just (Version {versionBranch = [3]})))
            -> Just gplv3

          Flag (LGPL (Just (Version {versionBranch = [2]})))
            -> Just lgpl2

          Flag (LGPL (Just (Version {versionBranch = [3]})))
            -> Just lgpl3

          _ -> Nothing

  case licenseFile of
    Just licenseText -> writeFile "LICENSE" licenseText
    Nothing -> message flags "Warning: unknown license type, you must put a copy in LICENSE yourself."

getYear :: IO Integer
getYear = do
  u <- getCurrentTime
  z <- getCurrentTimeZone
  let l = utcToLocalTime z u
      (y, _, _) = toGregorian $ localDay l
  return y

writeSetupFile :: InitFlags -> IO ()
writeSetupFile flags = do
  message flags "Generating Setup.hs..."
  writeFile "Setup.hs" setupFile
 where
  setupFile = unlines
    [ "import Distribution.Simple"
    , "main = defaultMain"
    ]

writeCabalFile :: InitFlags -> IO Bool
writeCabalFile flags@(InitFlags{packageName = NoFlag}) = do
  message flags "Error: no package name provided."
  return False
writeCabalFile flags@(InitFlags{packageName = Flag p}) = do
  let cabalFileName = p ++ ".cabal"
  message flags $ "Generating " ++ cabalFileName ++ "..."
  writeFile cabalFileName (generateCabalFile cabalFileName flags)
  return True

-- | Generate a .cabal file from an InitFlags structure.  NOTE: this
--   is rather ad-hoc!  What we would REALLY like is to have a
--   standard low-level AST type representing .cabal files, which
--   preserves things like comments, and to write an *inverse*
--   parser/pretty-printer pair between .cabal files and this AST.
--   Then instead of this ad-hoc code we could just map an InitFlags
--   structure onto a low-level AST structure and use the existing
--   pretty-printing code to generate the file.
generateCabalFile :: String -> InitFlags -> String
generateCabalFile fileName c =
  renderStyle style { lineLength = 79, ribbonsPerLine = 1.1 } $
  (if (minimal c /= Flag True)
    then showComment (Just $ "Initial " ++ fileName ++ " generated by cabal "
                          ++ "init.  For further documentation, see "
                          ++ "http://haskell.org/cabal/users-guide/")
         $$ text ""
    else empty)
  $$
  vcat [ fieldS "name"          (packageName   c)
                (Just "The name of the package.")
                True

       , field  "version"       (version       c)
                (Just "The package version.  See the Haskell package versioning policy (http://www.haskell.org/haskellwiki/Package_versioning_policy) for standards guiding when and how versions should be incremented.")
                True

       , fieldS "synopsis"      (synopsis      c)
                (Just "A short (one-line) description of the package.")
                True

       , fieldS "description"   NoFlag
                (Just "A longer description of the package.")
                True

       , fieldS "homepage"      (homepage     c)
                (Just "URL for the project homepage or repository.")
                False

       , fieldS "bug-reports"   NoFlag
                (Just "A URL where users can report bugs.")
                False

       , field  "license"       (license      c)
                (Just "The license under which the package is released.")
                True

       , fieldS "license-file" (Flag "LICENSE")
                (Just "The file containing the license text.")
                True

       , fieldS "author"        (author       c)
                (Just "The package author(s).")
                True

       , fieldS "maintainer"    (email        c)
                (Just "An email address to which users can send suggestions, bug reports, and patches.")
                True

       , fieldS "copyright"     NoFlag
                (Just "A copyright notice.")
                True

       , fieldS "category"      (either id display `fmap` category c)
                Nothing
                True

       , fieldS "build-type"    (Flag "Simple")
                Nothing
                True

       , fieldS "extra-source-files" NoFlag
                (Just "Extra files to be distributed with the package, such as examples or a README.")
                False

       , field  "cabal-version" (Flag $ orLaterVersion (Version [1,8] []))
                (Just "Constraint on the version of Cabal needed to build this package.")
                False

       , case packageType c of
           Flag Executable ->
             text "\nexecutable" <+> text (fromMaybe "" . flagToMaybe $ packageName c) $$ (nest 2 $ vcat
             [ fieldS "main-is" NoFlag (Just ".hs or .lhs file containing the Main module.") True

             , generateBuildInfo Executable c
             ])
           Flag Library    -> text "\nlibrary" $$ (nest 2 $ vcat
             [ fieldS "exposed-modules" (listField (exposedModules c))
                      (Just "Modules exported by the library.")
                      True

             , generateBuildInfo Library c
             ])
           _               -> empty
       ]
 where
   generateBuildInfo :: PackageType -> InitFlags -> Doc
   generateBuildInfo pkgtype c' = vcat
     [ fieldS "other-modules" (listField (otherModules c'))
              (Just $ case pkgtype of
                 Library    -> "Modules included in this library but not exported."
                 Executable -> "Modules included in this executable, other than Main.")
              True

     , fieldS "build-depends" (listField (dependencies c'))
              (Just "Other library packages from which modules are imported.")
              True

     , fieldS "hs-source-dirs" (listFieldS (sourceDirs c'))
              (Just "Directories containing source files.")
              False

     , fieldS "build-tools" (listFieldS (buildTools c'))
              (Just "Extra tools (e.g. alex, hsc2hs, ...) needed to build the source.")
              False
     ]

   listField :: Text s => Maybe [s] -> Flag String
   listField = listFieldS . fmap (map display)

   listFieldS :: Maybe [String] -> Flag String
   listFieldS = Flag . maybe "" (concat . intersperse ", ")

   field :: Text t => String -> Flag t -> Maybe String -> Bool -> Doc
   field s f = fieldS s (fmap display f)

   fieldS :: String        -- ^ Name of the field
          -> Flag String   -- ^ Field contents
          -> Maybe String  -- ^ Comment to explain the field
          -> Bool          -- ^ Should the field be included (commented out) even if blank?
          -> Doc
   fieldS _ NoFlag _    inc | not inc || (minimal c == Flag True) = empty
   fieldS _ (Flag "") _ inc | not inc || (minimal c == Flag True) = empty
   fieldS s f com _ = case (isJust com, noComments c, minimal c) of
                        (_, _, Flag True) -> id
                        (_, Flag True, _) -> id
                        (True, _, _)      -> (showComment com $$) . ($$ text "")
                        (False, _, _)     -> ($$ text "")
                      $
                      comment f <> text s <> colon
                                <> text (take (20 - length s) (repeat ' '))
                                <> text (fromMaybe "" . flagToMaybe $ f)
   comment NoFlag    = text "-- "
   comment (Flag "") = text "-- "
   comment _         = text ""

   showComment :: Maybe String -> Doc
   showComment (Just t) = vcat . map text
                        . map ("-- "++) . lines
                        . renderStyle style {
                            lineLength = 76,
                            ribbonsPerLine = 1.05
                          }
                        . fsep . map text . words $ t
   showComment Nothing  = text ""

-- | Generate warnings for missing fields etc.
generateWarnings :: InitFlags -> IO ()
generateWarnings flags = do
  message flags ""
  when (synopsis flags `elem` [NoFlag, Flag ""])
       (message flags "Warning: no synopsis given. You should edit the .cabal file and add one.")

  message flags "You may want to edit the .cabal file and add a Description field."

-- | Possibly generate a message to stdout, taking into account the
--   --quiet flag.
message :: InitFlags -> String -> IO ()
message (InitFlags{quiet = Flag True}) _ = return ()
message _ s = putStrLn s

#if MIN_VERSION_base(3,0,0)
#else
(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> f x >>= g
#endif
