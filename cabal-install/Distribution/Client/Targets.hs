-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Targets
-- Copyright   :  (c) Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
--
-- Handling for user-specified targets
-----------------------------------------------------------------------------
module Distribution.Client.Targets (
  -- * User targets
  UserTarget(..),
  readUserTargets,

  -- * Package specifiers
  PackageSpecifier(..),
  pkgSpecifierTarget,
  pkgSpecifierConstraints,

  -- * Resolving user targets to package specifiers
  resolveUserTargets,

  -- ** Detailed interface
  UserTargetProblem(..),
  readUserTarget,
  reportUserTargetProblems,
  expandUserTarget,

  PackageTarget(..),
  fetchPackageTarget,
  readPackageTarget,

  PackageTargetProblem(..),
  reportPackageTargetProblems,

  disambiguatePackageTargets,
  disambiguatePackageName,

  -- * User constraints
  UserConstraint(..),
  readUserConstraint,
  userToPackageConstraint

  ) where

import Distribution.Package
         ( Package(..), PackageName(..)
         , PackageIdentifier(..), packageName, packageVersion
         , Dependency(Dependency) )
import Distribution.Client.Types
         ( SourcePackage(..), PackageLocation(..), OptionalStanza(..) )
import Distribution.Client.Dependency.Types
         ( PackageConstraint(..) )

import qualified Distribution.Client.World as World
import Distribution.Client.PackageIndex (PackageIndex)
import qualified Distribution.Client.PackageIndex as PackageIndex
import qualified Distribution.Client.Tar as Tar
import Distribution.Client.FetchUtils

import Distribution.PackageDescription
         ( GenericPackageDescription, FlagName(..), FlagAssignment )
import Distribution.PackageDescription.Parse
         ( readPackageDescription, parsePackageDescription, ParseResult(..) )
import Distribution.Version
         ( Version(Version), thisVersion, anyVersion, isAnyVersion
         , VersionRange )
import Distribution.Text
         ( Text(..), display )
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils
         ( die, warn, intercalate, tryFindPackageDesc, fromUTF8, lowercase )

import Data.List
         ( find, nub )
import Data.Maybe
         ( listToMaybe )
import Data.Either
         ( partitionEithers )
import Data.Monoid
         ( Monoid(..) )
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import qualified Distribution.Client.GZipUtils as GZipUtils
import Control.Monad (liftM)
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP
         ( (+++), (<++) )
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint
         ( (<>), (<+>) )
import Data.Char
         ( isSpace, isAlphaNum )
import System.FilePath
         ( takeExtension, dropExtension, takeDirectory, splitPath )
import System.Directory
         ( doesFileExist, doesDirectoryExist )
import Network.URI
         ( URI(..), URIAuth(..), parseAbsoluteURI )

-- ------------------------------------------------------------
-- * User targets
-- ------------------------------------------------------------

-- | Various ways that a user may specify a package or package collection.
--
data UserTarget =

     -- | A partially specified package, identified by name and possibly with
     -- an exact version or a version constraint.
     --
     -- > cabal install foo
     -- > cabal install foo-1.0
     -- > cabal install 'foo < 2'
     --
     UserTargetNamed Dependency

     -- | A special virtual package that refers to the collection of packages
     -- recorded in the world file that the user specifically installed.
     --
     -- > cabal install world
     --
   | UserTargetWorld

     -- | A specific package that is unpacked in a local directory, often the
     -- current directory.
     --
     -- > cabal install .
     -- > cabal install ../lib/other
     --
     -- * Note: in future, if multiple @.cabal@ files are allowed in a single
     -- directory then this will refer to the collection of packages.
     --
   | UserTargetLocalDir FilePath

     -- | A specific local unpacked package, identified by its @.cabal@ file.
     --
     -- > cabal install foo.cabal
     -- > cabal install ../lib/other/bar.cabal
     --
   | UserTargetLocalCabalFile FilePath

     -- | A specific package that is available as a local tarball file
     --
     -- > cabal install dist/foo-1.0.tar.gz
     -- > cabal install ../build/baz-1.0.tar.gz
     --
   | UserTargetLocalTarball FilePath

     -- | A specific package that is available as a remote tarball file
     --
     -- > cabal install http://code.haskell.org/~user/foo/foo-0.9.tar.gz
     --
   | UserTargetRemoteTarball URI
  deriving (Show,Eq)


-- ------------------------------------------------------------
-- * Package specifier
-- ------------------------------------------------------------

-- | A fully or partially resolved reference to a package.
--
data PackageSpecifier pkg =

     -- | A partially specified reference to a package (either source or
     -- installed). It is specified by package name and optionally some
     -- additional constraints. Use a dependency resolver to pick a specific
     -- package satisfying these constraints.
     --
     NamedPackage PackageName [PackageConstraint]

     -- | A fully specified source package.
     --
   | SpecificSourcePackage pkg
  deriving Show

pkgSpecifierTarget :: Package pkg => PackageSpecifier pkg -> PackageName
pkgSpecifierTarget (NamedPackage name _)       = name
pkgSpecifierTarget (SpecificSourcePackage pkg) = packageName pkg

pkgSpecifierConstraints :: Package pkg
                        => PackageSpecifier pkg -> [PackageConstraint]
pkgSpecifierConstraints (NamedPackage _ constraints) = constraints
pkgSpecifierConstraints (SpecificSourcePackage pkg)  =
  [PackageConstraintVersion (packageName pkg)
                            (thisVersion (packageVersion pkg))]


-- ------------------------------------------------------------
-- * Parsing and checking user targets
-- ------------------------------------------------------------

readUserTargets :: Verbosity -> [String] -> IO [UserTarget]
readUserTargets _verbosity targetStrs = do
    (problems, targets) <- liftM partitionEithers
                                 (mapM readUserTarget targetStrs)
    reportUserTargetProblems problems
    return targets


data UserTargetProblem
   = UserTargetUnexpectedFile      String
   | UserTargetNonexistantFile     String
   | UserTargetUnexpectedUriScheme String
   | UserTargetUnrecognisedUri     String
   | UserTargetUnrecognised        String
   | UserTargetBadWorldPkg
  deriving Show

readUserTarget :: String -> IO (Either UserTargetProblem UserTarget)
readUserTarget targetstr =
    case testNamedTargets targetstr of
      Just (Dependency (PackageName "world") verrange)
        | verrange == anyVersion -> return (Right UserTargetWorld)
        | otherwise              -> return (Left  UserTargetBadWorldPkg)
      Just dep                   -> return (Right (UserTargetNamed dep))
      Nothing -> do
        fileTarget <- testFileTargets targetstr
        case fileTarget of
          Just target -> return target
          Nothing     ->
            case testUriTargets targetstr of
              Just target -> return target
              Nothing     -> return (Left (UserTargetUnrecognised targetstr))
  where
    testNamedTargets = readPToMaybe parseDependencyOrPackageId

    testFileTargets filename = do
      isDir  <- doesDirectoryExist filename
      isFile <- doesFileExist filename
      parentDirExists <- case takeDirectory filename of
                           []  -> return False
                           dir -> doesDirectoryExist dir
      let result
            | isDir
            = Just (Right (UserTargetLocalDir filename))

            | isFile && extensionIsTarGz filename
            = Just (Right (UserTargetLocalTarball filename))

            | isFile && takeExtension filename == ".cabal"
            = Just (Right (UserTargetLocalCabalFile filename))

            | isFile
            = Just (Left (UserTargetUnexpectedFile filename))

            | parentDirExists
            = Just (Left (UserTargetNonexistantFile filename))

            | otherwise
            = Nothing
      return result

    testUriTargets str =
      case parseAbsoluteURI str of
        Just uri@URI {
            uriScheme    = scheme,
            uriAuthority = Just URIAuth { uriRegName = host }
          }
          | scheme /= "http:" ->
            Just (Left (UserTargetUnexpectedUriScheme targetstr))

          | null host ->
            Just (Left (UserTargetUnrecognisedUri targetstr))

          | otherwise ->
            Just (Right (UserTargetRemoteTarball uri))
        _ -> Nothing

    extensionIsTarGz f = takeExtension f                 == ".gz"
                      && takeExtension (dropExtension f) == ".tar"

    parseDependencyOrPackageId :: Parse.ReadP r Dependency
    parseDependencyOrPackageId = parse
                             +++ liftM pkgidToDependency parse
      where
        pkgidToDependency :: PackageIdentifier -> Dependency
        pkgidToDependency p = case packageVersion p of
          Version [] _ -> Dependency (packageName p) anyVersion
          version      -> Dependency (packageName p) (thisVersion version)

readPToMaybe :: Parse.ReadP a a -> String -> Maybe a
readPToMaybe p str = listToMaybe [ r | (r,s) <- Parse.readP_to_S p str
                                     , all isSpace s ]


reportUserTargetProblems :: [UserTargetProblem] -> IO ()
reportUserTargetProblems problems = do
    case [ target | UserTargetUnrecognised target <- problems ] of
      []     -> return ()
      target -> die
              $ unlines
                  [ "Unrecognised target '" ++ name ++ "'."
                  | name <- target ]
             ++ "Targets can be:\n"
             ++ " - package names, e.g. 'pkgname', 'pkgname-1.0.1', 'pkgname < 2.0'\n"
             ++ " - the special 'world' target\n"
             ++ " - cabal files 'pkgname.cabal' or package directories 'pkgname/'\n"
             ++ " - package tarballs 'pkgname.tar.gz' or 'http://example.com/pkgname.tar.gz'"

    case [ () | UserTargetBadWorldPkg <- problems ] of
      [] -> return ()
      _  -> die "The special 'world' target does not take any version."

    case [ target | UserTargetNonexistantFile target <- problems ] of
      []     -> return ()
      target -> die
              $ unlines
                  [ "The file does not exist '" ++ name ++ "'."
                  | name <- target ]

    case [ target | UserTargetUnexpectedFile target <- problems ] of
      []     -> return ()
      target -> die
              $ unlines
                  [ "Unrecognised file target '" ++ name ++ "'."
                  | name <- target ]
             ++ "File targets can be either package tarballs 'pkgname.tar.gz' "
             ++ "or cabal files 'pkgname.cabal'."

    case [ target | UserTargetUnexpectedUriScheme target <- problems ] of
      []     -> return ()
      target -> die
              $ unlines
                  [ "URL target not supported '" ++ name ++ "'."
                  | name <- target ]
             ++ "Only 'http://' URLs are supported."

    case [ target | UserTargetUnrecognisedUri target <- problems ] of
      []     -> return ()
      target -> die
              $ unlines
                  [ "Unrecognise URL target '" ++ name ++ "'."
                  | name <- target ]


-- ------------------------------------------------------------
-- * Resolving user targets to package specifiers
-- ------------------------------------------------------------

-- | Given a bunch of user-specified targets, try to resolve what it is they
-- refer to. They can either be specific packages (local dirs, tarballs etc)
-- or they can be named packages (with or without version info).
--
resolveUserTargets :: Package pkg
                   => Verbosity
                   -> FilePath
                   -> PackageIndex pkg
                   -> [UserTarget]
                   -> IO [PackageSpecifier SourcePackage]
resolveUserTargets verbosity worldFile available userTargets = do

    -- given the user targets, get a list of fully or partially resolved
    -- package references
    packageTargets <- mapM (readPackageTarget verbosity)
                  =<< mapM (fetchPackageTarget verbosity) . concat
                  =<< mapM (expandUserTarget worldFile) userTargets

    -- users are allowed to give package names case-insensitively, so we must
    -- disambiguate named package references
    let (problems, packageSpecifiers) =
           disambiguatePackageTargets available availableExtra packageTargets

        -- use any extra specific available packages to help us disambiguate
        availableExtra = [ packageName pkg
                         | PackageTargetLocation pkg <- packageTargets ]

    reportPackageTargetProblems verbosity problems

    return packageSpecifiers


-- ------------------------------------------------------------
-- * Package targets
-- ------------------------------------------------------------

-- | An intermediate between a 'UserTarget' and a resolved 'PackageSpecifier'.
-- Unlike a 'UserTarget', a 'PackageTarget' refers only to a single package.
--
data PackageTarget pkg =
     PackageTargetNamed      PackageName [PackageConstraint] UserTarget

     -- | A package identified by name, but case insensitively, so it needs
     -- to be resolved to the right case-sensitive name.
   | PackageTargetNamedFuzzy PackageName [PackageConstraint] UserTarget
   | PackageTargetLocation pkg
  deriving Show


-- ------------------------------------------------------------
-- * Converting user targets to package targets
-- ------------------------------------------------------------

-- | Given a user-specified target, expand it to a bunch of package targets
-- (each of which refers to only one package).
--
expandUserTarget :: FilePath
                 -> UserTarget
                 -> IO [PackageTarget (PackageLocation ())]
expandUserTarget worldFile userTarget = case userTarget of

    UserTargetNamed (Dependency name vrange) ->
      let constraints = [ PackageConstraintVersion name vrange
                        | not (isAnyVersion vrange) ]
      in  return [PackageTargetNamedFuzzy name constraints userTarget]

    UserTargetWorld -> do
      worldPkgs <- World.getContents worldFile
      --TODO: should we warn if there are no world targets?
      return [ PackageTargetNamed name constraints userTarget
             | World.WorldPkgInfo (Dependency name vrange) flags <- worldPkgs
             , let constraints = [ PackageConstraintVersion name vrange
                                 | not (isAnyVersion vrange) ]
                              ++ [ PackageConstraintFlags name flags
                                 | not (null flags) ] ]

    UserTargetLocalDir dir ->
      return [ PackageTargetLocation (LocalUnpackedPackage dir) ]

    UserTargetLocalCabalFile file -> do
      let dir = takeDirectory file
      _   <- tryFindPackageDesc dir -- just as a check
      return [ PackageTargetLocation (LocalUnpackedPackage dir) ]

    UserTargetLocalTarball tarballFile ->
      return [ PackageTargetLocation (LocalTarballPackage tarballFile) ]

    UserTargetRemoteTarball tarballURL ->
      return [ PackageTargetLocation (RemoteTarballPackage tarballURL ()) ]


-- ------------------------------------------------------------
-- * Fetching and reading package targets
-- ------------------------------------------------------------


-- | Fetch any remote targets so that they can be read.
--
fetchPackageTarget :: Verbosity
                   -> PackageTarget (PackageLocation ())
                   -> IO (PackageTarget (PackageLocation FilePath))
fetchPackageTarget verbosity target = case target of
    PackageTargetNamed      n cs ut -> return (PackageTargetNamed      n cs ut)
    PackageTargetNamedFuzzy n cs ut -> return (PackageTargetNamedFuzzy n cs ut)
    PackageTargetLocation location  -> do
      location' <- fetchPackage verbosity (fmap (const Nothing) location)
      return (PackageTargetLocation location')


-- | Given a package target that has been fetched, read the .cabal file.
--
-- This only affects targets given by location, named targets are unaffected.
--
readPackageTarget :: Verbosity
                  -> PackageTarget (PackageLocation FilePath)
                  -> IO (PackageTarget SourcePackage)
readPackageTarget verbosity target = case target of

    PackageTargetNamed pkgname constraints userTarget ->
      return (PackageTargetNamed pkgname constraints userTarget)

    PackageTargetNamedFuzzy pkgname constraints userTarget ->
      return (PackageTargetNamedFuzzy pkgname constraints userTarget)

    PackageTargetLocation location -> case location of

      LocalUnpackedPackage dir -> do
        pkg <- readPackageDescription verbosity =<< tryFindPackageDesc dir
        return $ PackageTargetLocation $
                   SourcePackage {
                     packageInfoId        = packageId pkg,
                     packageDescription   = pkg,
                     packageSource        = fmap Just location,
                     packageDescrOverride = Nothing
                   }

      LocalTarballPackage tarballFile ->
        readTarballPackageTarget location tarballFile tarballFile

      RemoteTarballPackage tarballURL tarballFile ->
        readTarballPackageTarget location tarballFile (show tarballURL)

      RepoTarballPackage _repo _pkgid _ ->
        error "TODO: readPackageTarget RepoTarballPackage"
        -- For repo tarballs this info should be obtained from the index.

  where
    readTarballPackageTarget location tarballFile tarballOriginalLoc = do
      (filename, content) <- extractTarballPackageCabalFile
                               tarballFile tarballOriginalLoc
      case parsePackageDescription' content of
        Nothing  -> die $ "Could not parse the cabal file "
                       ++ filename ++ " in " ++ tarballFile
        Just pkg ->
          return $ PackageTargetLocation $
                     SourcePackage {
                       packageInfoId        = packageId pkg,
                       packageDescription   = pkg,
                       packageSource        = fmap Just location,
                       packageDescrOverride = Nothing
                     }

    extractTarballPackageCabalFile :: FilePath -> String
                                   -> IO (FilePath, BS.ByteString)
    extractTarballPackageCabalFile tarballFile tarballOriginalLoc =
          either (die . formatErr) return
        . check
        . Tar.entriesIndex
        . Tar.filterEntries isCabalFile
        . Tar.read
        . GZipUtils.maybeDecompress
      =<< BS.readFile tarballFile
      where
        formatErr msg = "Error reading " ++ tarballOriginalLoc ++ ": " ++ msg

        check (Left e)  = Left e
        check (Right m) = case Map.elems m of
            []     -> Left noCabalFile
            [file] -> case Tar.entryContent file of
              Tar.NormalFile content _ -> Right (Tar.entryPath file, content)
              _                        -> Left noCabalFile
            _files -> Left multipleCabalFiles
          where
            noCabalFile        = "No cabal file found"
            multipleCabalFiles = "Multiple cabal files found"

        isCabalFile e = case splitPath (Tar.entryPath e) of
          [     _dir, file] -> takeExtension file == ".cabal"
          [".", _dir, file] -> takeExtension file == ".cabal"
          _                 -> False

    parsePackageDescription' :: BS.ByteString -> Maybe GenericPackageDescription
    parsePackageDescription' content =
      case parsePackageDescription . fromUTF8 . BS.Char8.unpack $ content of
        ParseOk _ pkg -> Just pkg
        _             -> Nothing


-- ------------------------------------------------------------
-- * Checking package targets
-- ------------------------------------------------------------

data PackageTargetProblem
   = PackageNameUnknown   PackageName               UserTarget
   | PackageNameAmbigious PackageName [PackageName] UserTarget
  deriving Show


-- | Users are allowed to give package names case-insensitively, so we must
-- disambiguate named package references.
--
disambiguatePackageTargets :: Package pkg'
                           => PackageIndex pkg'
                           -> [PackageName]
                           -> [PackageTarget pkg]
                           -> ( [PackageTargetProblem]
                              , [PackageSpecifier pkg] )
disambiguatePackageTargets availablePkgIndex availableExtra targets =
    partitionEithers (map disambiguatePackageTarget targets)
  where
    disambiguatePackageTarget packageTarget = case packageTarget of
      PackageTargetLocation pkg -> Right (SpecificSourcePackage pkg)

      PackageTargetNamed pkgname constraints userTarget
        | null (PackageIndex.lookupPackageName availablePkgIndex pkgname)
                    -> Left (PackageNameUnknown pkgname userTarget)
        | otherwise -> Right (NamedPackage pkgname constraints)

      PackageTargetNamedFuzzy pkgname constraints userTarget ->
        case disambiguatePackageName packageNameEnv pkgname of
          None                 -> Left  (PackageNameUnknown
                                          pkgname userTarget)
          Ambiguous   pkgnames -> Left  (PackageNameAmbigious
                                          pkgname pkgnames userTarget)
          Unambiguous pkgname' -> Right (NamedPackage pkgname' constraints')
            where
              constraints' = map (renamePackageConstraint pkgname') constraints

    -- use any extra specific available packages to help us disambiguate
    packageNameEnv :: PackageNameEnv
    packageNameEnv = mappend (indexPackageNameEnv availablePkgIndex)
                             (extraPackageNameEnv availableExtra)


-- | Report problems to the user. That is, if there are any problems
-- then raise an exception.
reportPackageTargetProblems :: Verbosity
                            -> [PackageTargetProblem] -> IO ()
reportPackageTargetProblems verbosity problems = do
    case [ pkg | PackageNameUnknown pkg originalTarget <- problems
               , not (isUserTagetWorld originalTarget) ] of
      []    -> return ()
      pkgs  -> die $ unlines
                       [ "There is no package named '" ++ display name ++ "'. "
                       | name <- pkgs ]
                  ++ "You may need to run 'cabal update' to get the latest "
                  ++ "list of available packages."

    case [ (pkg, matches) | PackageNameAmbigious pkg matches _ <- problems ] of
      []          -> return ()
      ambiguities -> die $ unlines
                             [    "The package name '" ++ display name
                               ++ "' is ambigious. It could be: "
                               ++ intercalate ", " (map display matches)
                             | (name, matches) <- ambiguities ]

    case [ pkg | PackageNameUnknown pkg UserTargetWorld <- problems ] of
      []   -> return ()
      pkgs -> warn verbosity $
                 "The following 'world' packages will be ignored because "
              ++ "they refer to packages that cannot be found: "
              ++ intercalate ", " (map display pkgs) ++ "\n"
              ++ "You can suppress this warning by correcting the world file."
  where
    isUserTagetWorld UserTargetWorld = True; isUserTagetWorld _ = False


-- ------------------------------------------------------------
-- * Disambiguating package names
-- ------------------------------------------------------------

data MaybeAmbigious a = None | Unambiguous a | Ambiguous [a]

-- | Given a package name and a list of matching names, figure out which one it
-- might be referring to. If there is an exact case-sensitive match then that's
-- ok. If it matches just one package case-insensitively then that's also ok.
-- The only problem is if it matches multiple packages case-insensitively, in
-- that case it is ambigious.
--
disambiguatePackageName :: PackageNameEnv
                        -> PackageName
                        -> MaybeAmbigious PackageName
disambiguatePackageName (PackageNameEnv pkgNameLookup) name =
    case nub (pkgNameLookup name) of
      []      -> None
      [name'] -> Unambiguous name'
      names   -> case find (name==) names of
                   Just name' -> Unambiguous name'
                   Nothing    -> Ambiguous names


newtype PackageNameEnv = PackageNameEnv (PackageName -> [PackageName])

instance Monoid PackageNameEnv where
  mempty = PackageNameEnv (const [])
  mappend (PackageNameEnv lookupA) (PackageNameEnv lookupB) =
    PackageNameEnv (\name -> lookupA name ++ lookupB name)

indexPackageNameEnv :: Package pkg => PackageIndex pkg -> PackageNameEnv
indexPackageNameEnv pkgIndex = PackageNameEnv pkgNameLookup
  where
    pkgNameLookup (PackageName name) =
      map fst (PackageIndex.searchByName pkgIndex name)

extraPackageNameEnv :: [PackageName] -> PackageNameEnv
extraPackageNameEnv names = PackageNameEnv pkgNameLookup
  where
    pkgNameLookup (PackageName name) =
      [ PackageName name'
      | let lname = lowercase name
      , PackageName name' <- names
      , lowercase name' == lname ]


-- ------------------------------------------------------------
-- * Package constraints
-- ------------------------------------------------------------

data UserConstraint =
     UserConstraintVersion   PackageName VersionRange
   | UserConstraintInstalled PackageName
   | UserConstraintSource    PackageName
   | UserConstraintFlags     PackageName FlagAssignment
   | UserConstraintStanzas   PackageName [OptionalStanza]
  deriving (Show,Eq)


userToPackageConstraint :: UserConstraint -> PackageConstraint
-- At the moment, the types happen to be directly equivalent
userToPackageConstraint uc = case uc of
  UserConstraintVersion   name ver   -> PackageConstraintVersion    name ver
  UserConstraintInstalled name       -> PackageConstraintInstalled  name
  UserConstraintSource    name       -> PackageConstraintSource     name
  UserConstraintFlags     name flags -> PackageConstraintFlags      name flags
  UserConstraintStanzas   name stanzas -> PackageConstraintStanzas  name stanzas

renamePackageConstraint :: PackageName -> PackageConstraint -> PackageConstraint
renamePackageConstraint name pc = case pc of
  PackageConstraintVersion   _ ver   -> PackageConstraintVersion    name ver
  PackageConstraintInstalled _       -> PackageConstraintInstalled  name
  PackageConstraintSource    _       -> PackageConstraintSource     name
  PackageConstraintFlags     _ flags -> PackageConstraintFlags      name flags
  PackageConstraintStanzas   _ stanzas -> PackageConstraintStanzas   name stanzas

readUserConstraint :: String -> Either String UserConstraint
readUserConstraint str =
    case readPToMaybe parse str of
      Nothing -> Left msgCannotParse
      Just c  -> Right c
  where
    msgCannotParse =
         "expected a package name followed by a constraint, which is "
      ++ "either a version range, 'installed', 'source' or flags"

--FIXME: use Text instance for FlagName and FlagAssignment
instance Text UserConstraint where
  disp (UserConstraintVersion   pkgname verrange) = disp pkgname
                                                    <+> disp verrange
  disp (UserConstraintInstalled pkgname)          = disp pkgname
                                                    <+> Disp.text "installed"
  disp (UserConstraintSource    pkgname)          = disp pkgname
                                                    <+> Disp.text "source"
  disp (UserConstraintFlags     pkgname flags)    = disp pkgname
                                                    <+> dispFlagAssignment flags
    where
      dispFlagAssignment = Disp.hsep . map dispFlagValue
      dispFlagValue (f, True)   = Disp.char '+' <> dispFlagName f
      dispFlagValue (f, False)  = Disp.char '-' <> dispFlagName f
      dispFlagName (FlagName f) = Disp.text f

  disp (UserConstraintStanzas   pkgname stanzas)  = disp pkgname
                                                    <+> dispStanzas stanzas
    where
      dispStanzas = Disp.hsep . map dispStanza
      dispStanza TestStanzas  = Disp.text "test"
      dispStanza BenchStanzas = Disp.text "bench"

  parse = parse >>= parseConstraint
    where
      spaces = Parse.satisfy isSpace >> Parse.skipSpaces

      parseConstraint pkgname =
            ((parse >>= return . UserConstraintVersion pkgname)
        +++ (do spaces
                _ <- Parse.string "installed"
                return (UserConstraintInstalled pkgname))
        +++ (do spaces
                _ <- Parse.string "source"
                return (UserConstraintSource pkgname))
        +++ (do spaces
                _ <- Parse.string "test"
                return (UserConstraintStanzas pkgname [TestStanzas]))
        +++ (do spaces
                _ <- Parse.string "bench"
                return (UserConstraintStanzas pkgname [BenchStanzas])))
        <++ (parseFlagAssignment >>= (return . UserConstraintFlags pkgname))

      parseFlagAssignment = Parse.many1 (spaces >> parseFlagValue)
      parseFlagValue =
            (do Parse.optional (Parse.char '+')
                f <- parseFlagName
                return (f, True))
        +++ (do _ <- Parse.char '-'
                f <- parseFlagName
                return (f, False))
      parseFlagName = liftM FlagName ident

      ident :: Parse.ReadP r String
      ident = Parse.munch1 identChar >>= \s -> check s >> return s
        where
          identChar c   = isAlphaNum c || c == '_' || c == '-'
          check ('-':_) = Parse.pfail
          check _       = return ()
