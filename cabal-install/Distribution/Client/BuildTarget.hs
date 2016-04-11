{-# LANGUAGE CPP, DeriveGeneric, DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.BuildTargets
-- Copyright   :  (c) Duncan Coutts 2012, 2015
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
--
-- Handling for user-specified build targets
-----------------------------------------------------------------------------
module Distribution.Client.BuildTarget (

    -- * Build targets
    BuildTarget(..),
    --showBuildTarget,
    QualLevel(..),
    buildTargetPackage,
    buildTargetComponentName,

    -- * Top level convenience
    readUserBuildTargets,
    resolveUserBuildTargets,

    -- * Parsing user build targets
    UserBuildTarget,
    parseUserBuildTargets,
    showUserBuildTarget,
    UserBuildTargetProblem(..),
    reportUserBuildTargetProblems,

    -- * Resolving build targets
    resolveBuildTargets,
    BuildTargetProblem(..),
    reportBuildTargetProblems,
  ) where

import Distribution.Package
         ( Package(..), PackageId, PackageName, packageName )
import Distribution.Client.Types
         ( PackageLocation(..) )

import Distribution.PackageDescription
         ( PackageDescription
         , Executable(..)
         , TestSuite(..), TestSuiteInterface(..), testModules
         , Benchmark(..), BenchmarkInterface(..), benchmarkModules
         , BuildInfo(..), libModules, exeModules )
import Distribution.ModuleName
         ( ModuleName, toFilePath )
import Distribution.Simple.LocalBuildInfo
         ( Component(..), ComponentName(..)
         , pkgComponents, componentName, componentBuildInfo )

import Distribution.Text
         ( display, simpleParse )
import Distribution.Simple.Utils
         ( die, lowercase )
import Distribution.Client.Utils
         ( makeRelativeToCwd )

import Data.List
         ( nub, nubBy, stripPrefix, partition, intercalate, sortBy, groupBy )
import Data.Maybe
         ( listToMaybe, maybeToList )
import Data.Either
         ( partitionEithers )
import Data.Function
         ( on )
import GHC.Generics (Generic)
#if MIN_VERSION_containers(0,5,0)
import qualified Data.Map.Lazy   as Map.Lazy
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
#else
import qualified Data.Map as Map.Lazy
import qualified Data.Map as Map
import Data.Map (Map)
#endif
import Control.Monad
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..), (<$>))
#endif
import Control.Applicative (Alternative(..))
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP
         ( (+++), (<++) )
import Data.Char
         ( isSpace, isAlphaNum )
import System.FilePath as FilePath
         ( takeExtension, dropExtension, addTrailingPathSeparator
         , splitDirectories, joinPath, splitPath )
import System.Directory
         ( doesFileExist, doesDirectoryExist, canonicalizePath
         , getCurrentDirectory )
import System.FilePath
         ( (</>), (<.>), normalise )


-- ------------------------------------------------------------
-- * User build targets
-- ------------------------------------------------------------

-- | Various ways that a user may specify a build target.
--
-- The main general form has lots of optional parts:
--
-- > [ package name | package dir | package .cabal file ]
-- > [ [lib:|exe:] component name ]
-- > [ module name | source file ]
--
-- There's also a special case of a package tarball. It doesn't take part in
-- the main general form since we always build a tarball package as a whole.
--
-- > [package tar.gz file]
--
data UserBuildTarget =

     -- | A simple target specified by a single part. This is any of the
     -- general forms that can be expressed using one part, which are:
     --
     -- > cabal build foo                      -- package name
     -- > cabal build ../bar ../bar/bar.cabal  -- package dir or package file
     -- > cabal build foo                      -- component name
     -- > cabal build Data.Foo                 -- module name
     -- > cabal build Data/Foo.hs bar/Main.hsc -- file name
     --
     -- It can also be a package tarball.
     --
     -- > cabal build bar.tar.gz
     --
     UserBuildTarget1 String

     -- | A qualified target with two parts. This is any of the general
     -- forms that can be expressed using two parts, which are:
     --
     -- > cabal build foo:foo              -- package : component
     -- > cabal build foo:Data.Foo         -- package : module
     -- > cabal build foo:Data/Foo.hs      -- package : filename
     --
     -- > cabal build ./foo:foo            -- package dir : component
     -- > cabal build ./foo:Data.Foo       -- package dir : module
     --
     -- > cabal build ./foo.cabal:foo      -- package file : component
     -- > cabal build ./foo.cabal:Data.Foo -- package file : module
     -- > cabal build ./foo.cabal:Main.hs  -- package file : filename
     --
     -- > cabal build lib:foo exe:foo      -- namespace : component
     -- > cabal build foo:Data.Foo         -- component : module
     -- > cabal build foo:Data/Foo.hs      -- component : filename
     --
   | UserBuildTarget2 String String

     -- A (very) qualified target with three parts. This is any of the general
     -- forms that can be expressed using three parts, which are:
     --
     -- > cabal build foo:lib:foo          -- package : namespace : component
     -- > cabal build foo:foo:Data.Foo     -- package : component : module
     -- > cabal build foo:foo:Data/Foo.hs  -- package : component : filename
     --
     -- > cabal build foo/:lib:foo         -- pkg dir : namespace : component
     -- > cabal build foo/:foo:Data.Foo    -- pkg dir : component : module
     -- > cabal build foo/:foo:Data/Foo.hs -- pkg dir : component : filename
     --
     -- > cabal build foo.cabal:lib:foo         -- pkg file : namespace : component
     -- > cabal build foo.cabal:foo:Data.Foo    -- pkg file : component : module
     -- > cabal build foo.cabal:foo:Data/Foo.hs -- pkg file : component : filename
     --
     -- > cabal build lib:foo:Data.Foo     -- namespace : component : module
     -- > cabal build lib:foo:Data/Foo.hs  -- namespace : component : filename
     --
   | UserBuildTarget3 String String String

     -- A (rediculously) qualified target with four parts. This is any of the
     -- general forms that can be expressed using all four parts, which are:
     --
     -- > cabal build foo:lib:foo:Data.Foo     -- package : namespace : component : module
     -- > cabal build foo:lib:foo:Data/Foo.hs  -- package : namespace : component : filename
     --
     -- > cabal build foo/:lib:foo:Data.Foo    -- pkg dir : namespace : component : module
     -- > cabal build foo/:lib:foo:Data/Foo.hs -- pkg dir : namespace : component : filename
     --
     -- > cabal build foo.cabal:lib:foo:Data.Foo    -- pkg file : namespace : component : module
     -- > cabal build foo.cabal:lib:foo:Data/Foo.hs -- pkg file : namespace : component : filename
     --
   | UserBuildTarget4 String String String String
  deriving (Show, Eq, Ord)


-- ------------------------------------------------------------
-- * Resolved build targets
-- ------------------------------------------------------------

-- | A fully resolved build target.
--
data BuildTarget pkg =

     -- | A package as a whole 
     --
     BuildTargetPackage pkg

     -- | A specific component
     --
   | BuildTargetComponent pkg ComponentName

     -- | A specific module within a specific component.
     --
   | BuildTargetModule pkg ComponentName ModuleName

     -- | A specific file within a specific component.
     --
   | BuildTargetFile pkg ComponentName FilePath
  deriving (Eq, Ord, Functor, Show, Generic)


-- | Get the package that the 'BuildTarget' is referring to.
--
buildTargetPackage :: BuildTarget pkg -> pkg
buildTargetPackage (BuildTargetPackage   p)         = p
buildTargetPackage (BuildTargetComponent p _cn)     = p
buildTargetPackage (BuildTargetModule    p _cn _mn) = p
buildTargetPackage (BuildTargetFile      p _cn _fn) = p


-- | Get the 'ComponentName' that the 'BuildTarget' is referring to, if any.
-- The 'BuildTargetPackage' target kind doesn't refer to any individual
-- component, while the component, module and file kinds do.
--
buildTargetComponentName :: BuildTarget pkg -> Maybe ComponentName
buildTargetComponentName (BuildTargetPackage   _p)        = Nothing
buildTargetComponentName (BuildTargetComponent _p cn)     = Just cn
buildTargetComponentName (BuildTargetModule    _p cn _mn) = Just cn
buildTargetComponentName (BuildTargetFile      _p cn _fn) = Just cn


-- ------------------------------------------------------------
-- * Top level, do everything
-- ------------------------------------------------------------


-- | Parse a bunch of command line args as user build targets, failing with an
-- error if any targets are unrecognised.
--
readUserBuildTargets :: [String] -> IO [UserBuildTarget]
readUserBuildTargets targetStrs = do
    let (uproblems, utargets) = parseUserBuildTargets targetStrs
    reportUserBuildTargetProblems uproblems
    return utargets


-- | A 'UserBuildTarget's is just a semi-structured string. We sill have quite
-- a bit of work to do to figure out which targets they refer to (ie packages,
-- components, file locations etc).
--
-- The possible targets are based on the available packages (and their
-- locations). It fails with an error if any user string cannot be matched to
-- a valid target.
--
resolveUserBuildTargets :: [(PackageDescription, PackageLocation a)]
                        -> [UserBuildTarget] -> IO [BuildTarget PackageName]
resolveUserBuildTargets pkgs utargets = do
    utargets' <- mapM getUserTargetFileStatus utargets
    pkgs'     <- mapM (uncurry selectPackageInfo) pkgs
    pwd       <- getCurrentDirectory
    let (primaryPkg, otherPkgs) = selectPrimaryLocalPackage pwd pkgs'
        (bproblems,  btargets)  = resolveBuildTargets
                                    primaryPkg otherPkgs utargets''
        -- default local dir target if there's no given target
        utargets''
          | not (null primaryPkg)
          , null utargets       = [UserBuildTargetFileStatus1 "./"
                                     (FileStatusExistsDir pwd)]
          | otherwise           = utargets'

    reportBuildTargetProblems bproblems
    return (map (fmap packageName) btargets)
  where
    selectPrimaryLocalPackage :: FilePath
                              -> [PackageInfo]
                              -> ([PackageInfo], [PackageInfo])
    selectPrimaryLocalPackage pwd pkgs' =
        let (primary, others) = partition isPrimary pkgs'
         in (primary, others)
      where
        isPrimary PackageInfo { pinfoDirectory = Just (dir,_) }
          | dir == pwd = True
        isPrimary _    = False


-- ------------------------------------------------------------
-- * Checking if targets exist as files
-- ------------------------------------------------------------

data UserBuildTargetFileStatus =
     UserBuildTargetFileStatus1 String FileStatus
   | UserBuildTargetFileStatus2 String FileStatus String
   | UserBuildTargetFileStatus3 String FileStatus String String
   | UserBuildTargetFileStatus4 String FileStatus String String String
  deriving (Eq, Ord, Show)

data FileStatus = FileStatusExistsFile FilePath -- the canonicalised filepath
                | FileStatusExistsDir  FilePath -- the canonicalised filepath
                | FileStatusNotExists  Bool -- does the parent dir exist even?
  deriving (Eq, Ord, Show)

getUserTargetFileStatus :: UserBuildTarget -> IO UserBuildTargetFileStatus
getUserTargetFileStatus t =
    case t of
      UserBuildTarget1 s1 ->
        (\f1 -> UserBuildTargetFileStatus1 s1 f1)          <$> fileStatus s1
      UserBuildTarget2 s1 s2 ->
        (\f1 -> UserBuildTargetFileStatus2 s1 f1 s2)       <$> fileStatus s1
      UserBuildTarget3 s1 s2 s3 ->
        (\f1 -> UserBuildTargetFileStatus3 s1 f1 s2 s3)    <$> fileStatus s1
      UserBuildTarget4 s1 s2 s3 s4 ->
        (\f1 -> UserBuildTargetFileStatus4 s1 f1 s2 s3 s4) <$> fileStatus s1
  where
    fileStatus f = do
      fexists <- doesFileExist f
      dexists <- doesDirectoryExist f
      case splitPath f of
        _ | fexists -> FileStatusExistsFile <$> canonicalizePath f
          | dexists -> FileStatusExistsDir  <$> canonicalizePath f
        (d:_)       -> FileStatusNotExists  <$> doesDirectoryExist d
        _           -> error "getUserTargetFileStatus: empty path"

forgetFileStatus :: UserBuildTargetFileStatus -> UserBuildTarget
forgetFileStatus t = case t of
    UserBuildTargetFileStatus1 s1 _          -> UserBuildTarget1 s1
    UserBuildTargetFileStatus2 s1 _ s2       -> UserBuildTarget2 s1 s2
    UserBuildTargetFileStatus3 s1 _ s2 s3    -> UserBuildTarget3 s1 s2 s3
    UserBuildTargetFileStatus4 s1 _ s2 s3 s4 -> UserBuildTarget4 s1 s2 s3 s4


-- ------------------------------------------------------------
-- * Parsing user targets
-- ------------------------------------------------------------


-- | Parse a bunch of 'UserBuildTarget's (purely without throwing exceptions).
--
parseUserBuildTargets :: [String] -> ([UserBuildTargetProblem]
                                     ,[UserBuildTarget])
parseUserBuildTargets = partitionEithers . map parseUserBuildTarget

parseUserBuildTarget :: String -> Either UserBuildTargetProblem
                                         UserBuildTarget
parseUserBuildTarget targetstr =
    case readPToMaybe parseTargetApprox targetstr of
      Nothing  -> Left  (UserBuildTargetUnrecognised targetstr)
      Just tgt -> Right tgt

  where
    parseTargetApprox :: Parse.ReadP r UserBuildTarget
    parseTargetApprox =
          (do a <- tokenQ
              return (UserBuildTarget1 a))
      +++ (do a <- tokenQ
              _ <- Parse.char ':'
              b <- tokenQ
              return (UserBuildTarget2 a b))
      +++ (do a <- tokenQ
              _ <- Parse.char ':'
              b <- tokenQ
              _ <- Parse.char ':'
              c <- tokenQ
              return (UserBuildTarget3 a b c))
      +++ (do a <- tokenQ
              _ <- Parse.char ':'
              b <- token
              _ <- Parse.char ':'
              c <- tokenQ
              _ <- Parse.char ':'
              d <- tokenQ
              return (UserBuildTarget4 a b c d))

    token  = Parse.munch1 (\x -> not (isSpace x) && x /= ':')
    tokenQ = parseHaskellString <++ token
    parseHaskellString :: Parse.ReadP r String
    parseHaskellString = Parse.readS_to_P reads

    readPToMaybe :: Parse.ReadP a a -> String -> Maybe a
    readPToMaybe p str = listToMaybe [ r | (r,s) <- Parse.readP_to_S p str
                                         , all isSpace s ]

-- | Syntax error when trying to parse a 'UserBuildTarget'.
data UserBuildTargetProblem
   = UserBuildTargetUnrecognised String
  deriving Show

-- | Throw an exception with a formatted message if there are any problems.
--
reportUserBuildTargetProblems :: [UserBuildTargetProblem] -> IO ()
reportUserBuildTargetProblems problems = do
    case [ target | UserBuildTargetUnrecognised target <- problems ] of
      []     -> return ()
      target ->
        die $ unlines
                [ "Unrecognised build target syntax for '" ++ name ++ "'."
                | name <- target ]
           ++ "Syntax:\n"
           ++ " - build [package]\n"
           ++ " - build [package:]component\n"
           ++ " - build [package:][component:]module\n"
           ++ " - build [package:][component:]file\n"
           ++ " where\n"
           ++ "  package is a package name, package dir or .cabal file\n\n"
           ++ "Examples:\n"
           ++ " - build foo            -- package name\n"
           ++ " - build tests          -- component name\n"
           ++ "    (name of library, executable, test-suite or benchmark)\n"
           ++ " - build Data.Foo       -- module name\n"
           ++ " - build Data/Foo.hsc   -- file name\n\n"
           ++ "An ambigious target can be qualified by package, component\n"
           ++ "and/or component kind (lib|exe|test|bench)\n"
           ++ " - build foo:tests      -- component qualified by package\n"
           ++ " - build tests:Data.Foo -- module qualified by component\n"
           ++ " - build lib:foo        -- component qualified by kind"


-- | Render a 'UserBuildTarget' back as the external syntax. This is mainly for
-- error messages.
--
showUserBuildTarget :: UserBuildTarget -> String
showUserBuildTarget = intercalate ":" . components
  where
    components (UserBuildTarget1 s1)          = [s1]
    components (UserBuildTarget2 s1 s2)       = [s1,s2]
    components (UserBuildTarget3 s1 s2 s3)    = [s1,s2,s3]
    components (UserBuildTarget4 s1 s2 s3 s4) = [s1,s2,s3,s4]

showBuildTarget :: QualLevel -> BuildTarget PackageInfo -> String
showBuildTarget ql = showUserBuildTarget . forgetFileStatus
                   . head . renderBuildTarget ql


-- ------------------------------------------------------------
-- * Resolving user targets to build targets
-- ------------------------------------------------------------


-- | Given a bunch of user-specified targets, try to resolve what it is they
-- refer to.
--
resolveBuildTargets :: [PackageInfo]     -- any primary pkg, e.g. cur dir
                    -> [PackageInfo]     -- all the other local packages
                    -> [UserBuildTargetFileStatus]
                    -> ([BuildTargetProblem], [BuildTarget PackageInfo])
resolveBuildTargets ppinfo opinfo =
    partitionEithers
  . map (resolveBuildTarget ppinfo opinfo)

resolveBuildTarget :: [PackageInfo] -> [PackageInfo]
                   -> UserBuildTargetFileStatus
                   -> Either BuildTargetProblem (BuildTarget PackageInfo)
resolveBuildTarget ppinfo opinfo userTarget =
    case findMatch (matcher userTarget) of
      Unambiguous          target  -> Right target
      None                 errs    -> Left (classifyMatchErrors errs)
      Ambiguous exactMatch targets ->
        case disambiguateBuildTargets
               matcher userTarget exactMatch
               targets of
          Right targets'   -> Left (BuildTargetAmbiguous  userTarget' targets')
          Left ((m, ms):_) -> Left (MatchingInternalError userTarget' m ms)
          Left []          -> internalError "resolveBuildTarget"
  where
    matcher = matchBuildTarget ppinfo opinfo

    userTarget' = forgetFileStatus userTarget

    classifyMatchErrors errs
      | not (null expected)
      = let (things, got:_) = unzip expected in
        BuildTargetExpected userTarget' things got

      | not (null nosuch)
      = BuildTargetNoSuch userTarget' nosuch

      | otherwise
      = internalError $ "classifyMatchErrors: " ++ show errs
      where
        expected = [ (thing, got) 
                   | (_, MatchErrorExpected thing got)
                           <- map (innerErr Nothing) errs ]
        nosuch   = [ (inside, thing, got, alts)
                   | (inside, MatchErrorNoSuch thing got alts)
                           <- map (innerErr Nothing) errs ]

        innerErr _ (MatchErrorIn kind thing m)
                     = innerErr (Just (kind,thing)) m
        innerErr c m = (c,m)


-- | The various ways that trying to resolve a 'UserBuildTarget' to a
-- 'BuildTarget' can fail.
--
data BuildTargetProblem
   = BuildTargetExpected   UserBuildTarget [String]  String
     -- ^  [expected thing] (actually got)
   | BuildTargetNoSuch     UserBuildTarget
                           [(Maybe (String, String), String, String, [String])]
     -- ^ [([in thing], no such thing,  actually got, alternatives)]
   | BuildTargetAmbiguous  UserBuildTarget
                           [(UserBuildTarget, BuildTarget PackageInfo)]

   | MatchingInternalError UserBuildTarget (BuildTarget PackageInfo)
                           [(UserBuildTarget, [BuildTarget PackageInfo])]


disambiguateBuildTargets
  :: (UserBuildTargetFileStatus -> Match (BuildTarget PackageInfo))
  -> UserBuildTargetFileStatus -> Bool
  -> [BuildTarget PackageInfo]
  -> Either [(BuildTarget PackageInfo,
              [(UserBuildTarget, [BuildTarget PackageInfo])])]
            [(UserBuildTarget, BuildTarget PackageInfo)]
disambiguateBuildTargets matcher matchInput exactMatch matchResults =
    case partitionEithers results of
      (errs@(_:_), _) -> Left errs
      ([], ok)        -> Right ok
  where
    -- So, here's the strategy. We take the original match results, and make a
    -- table of all their renderings at all qualification levels.
    -- Note there can be multiple renderings at each qualification level.
    matchResultsRenderings :: [(BuildTarget PackageInfo, [UserBuildTargetFileStatus])]
    matchResultsRenderings =
      [ (matchResult, matchRenderings)
      | matchResult <- matchResults
      , let matchRenderings =
              [ rendering
              | ql <- [QL1 .. QL4]
              , rendering <- renderBuildTarget ql matchResult ]
      ]

    -- Of course the point is that we're looking for renderings that are
    -- unambiguous matches. So we build another memo table of all the matches
    -- for all of those renderings. So by looking up in this table we can see
    -- if we've got an unambiguous match.

    memoisedMatches :: Map UserBuildTargetFileStatus
                           (Match (BuildTarget PackageInfo))
    memoisedMatches =
        -- avoid recomputing the main one if it was an exact match
        (if exactMatch then Map.insert matchInput (ExactMatch 0 matchResults)
                       else id)
      $ Map.Lazy.fromList
          [ (rendering, matcher rendering)
          | rendering <- concatMap snd matchResultsRenderings ]

    -- Finally, for each of the match results, we go through all their
    -- possible renderings (in order of qualification level, though remember
    -- there can be multiple renderings per level), and find the first one
    -- that has an unambiguous match.
    results :: [Either (BuildTarget PackageInfo,
                        [(UserBuildTarget, [BuildTarget PackageInfo])])
                       (UserBuildTarget, BuildTarget PackageInfo)]
    results =
      [ case findUnambiguous originalMatch matchRenderings of
          Just unambiguousRendering ->
            Right ( forgetFileStatus unambiguousRendering
                  , originalMatch)

          -- This case is an internal error, but we bubble it up and report it
          Nothing ->
            Left  ( originalMatch
                  , [ (forgetFileStatus rendering, matches)
                    | rendering <- matchRenderings
                    , let (ExactMatch _ matches) =
                            memoisedMatches Map.! rendering 
                    ] )

      | (originalMatch, matchRenderings) <- matchResultsRenderings ]

    findUnambiguous :: BuildTarget PackageInfo -> [UserBuildTargetFileStatus]
                    -> Maybe UserBuildTargetFileStatus
    findUnambiguous _ []     = Nothing
    findUnambiguous t (r:rs) =
      case memoisedMatches Map.! r of
        ExactMatch _ [t'] | fmap packageName t == fmap packageName t'
                         -> Just r
        ExactMatch _  _  -> findUnambiguous t rs
        InexactMatch _ _ -> internalError "InexactMatch"
        NoMatch      _ _ -> internalError "NoMatch"

internalError :: String -> a
internalError msg =
  error $ "BuildTargets: internal error: " ++ msg


data QualLevel = QL1 | QL2 | QL3 | QL4
  deriving (Enum, Show)

renderBuildTarget :: QualLevel -> BuildTarget PackageInfo
                  -> [UserBuildTargetFileStatus]
renderBuildTarget ql t =
    case t of
      BuildTargetPackage p ->
        case ql of
          QL1 -> [t1 (dispP p)]
          QL2 -> [t1' pf fs | (pf, fs) <- dispPF p]
          QL3 -> []
          QL4 -> []

      BuildTargetComponent p c ->
        case ql of
          QL1 -> [t1                     (dispC p c)]
          QL2 -> [t2 (dispP p)           (dispC p c),
                  t2           (dispK c) (dispC p c)]
          QL3 -> [t3 (dispP p) (dispK c) (dispC p c)]
          QL4 -> []

      BuildTargetModule p c m ->
        case ql of
          QL1 -> [t1                                 (dispM m)]
          QL2 -> [t2 (dispP p)                       (dispM m),
                  t2                     (dispC p c) (dispM m)]
          QL3 -> [t3 (dispP p)           (dispC p c) (dispM m),
                  t3           (dispK c) (dispC p c) (dispM m)]
          QL4 -> [t4 (dispP p) (dispK c) (dispC p c) (dispM m)]

      BuildTargetFile p c f ->
        case ql of
          QL1 -> [t1                                 f]
          QL2 -> [t2 (dispP p)                       f,
                  t2                     (dispC p c) f]
          QL3 -> [t3 (dispP p)           (dispC p c) f,
                  t3           (dispK c) (dispC p c) f]
          QL4 -> [t4 (dispP p) (dispK c) (dispC p c) f]
  where
    t1  s1 = UserBuildTargetFileStatus1 s1 none
    t1' s1 = UserBuildTargetFileStatus1 s1
    t2  s1 = UserBuildTargetFileStatus2 s1 none
    t3  s1 = UserBuildTargetFileStatus3 s1 none
    t4  s1 = UserBuildTargetFileStatus4 s1 none
    none   = FileStatusNotExists False

    dispP = display . packageName
    dispC = componentStringName . packageName
    dispK = showComponentKindShort . componentKind
    dispM = display

    dispPF p = [ (addTrailingPathSeparator drel, FileStatusExistsDir dabs)
               | PackageInfo { pinfoDirectory   = Just (dabs,drel) } <- [p] ]
            ++ [ (frel, FileStatusExistsFile fabs)
               | PackageInfo { pinfoPackageFile = Just (fabs,frel) } <- [p] ]


-- | Throw an exception with a formatted message if there are any problems.
--
reportBuildTargetProblems :: [BuildTargetProblem] -> IO ()
reportBuildTargetProblems problems = do

    case [ (t, m, ms) | MatchingInternalError t m ms <- problems ] of
      [] -> return ()
      ((target, originalMatch, renderingsAndMatches):_) ->
        die $ "Internal error in build target matching. It should always be "
           ++ "possible to find a syntax that's sufficiently qualified to "
           ++ "give an unambigious match. However when matching '"
           ++ showUserBuildTarget target ++ "'  we found "
           ++ showBuildTarget QL1 originalMatch
           ++ " (" ++ showBuildTargetKind originalMatch ++ ") which does not "
           ++ "have an unambigious syntax. The possible syntax and the "
           ++ "targets they match are as follows:\n"
           ++ unlines
                [ "'" ++ showUserBuildTarget rendering ++ "' which matches "
                  ++ intercalate ", "
                       [ showBuildTarget QL1 match ++
                         " (" ++ showBuildTargetKind match ++ ")"
                       | match <- matches ]
                | (rendering, matches) <- renderingsAndMatches ]

    case [ (t, e, g) | BuildTargetExpected t e g <- problems ] of
      []      -> return ()
      targets ->
        die $ unlines
          [    "Unrecognised build target '" ++ showUserBuildTarget target
            ++ "'.\n"
            ++ "Expected a " ++ intercalate " or " expected
            ++ ", rather than '" ++ got ++ "'."
          | (target, expected, got) <- targets ]

    case [ (t, e) | BuildTargetNoSuch t e <- problems ] of
      []      -> return ()
      targets ->
        die $ unlines
          [ "Unknown build target '" ++ showUserBuildTarget target ++
            "'.\n" ++ unlines
            [    (case inside of
                    Just (kind, thing)
                            -> "The " ++ kind ++ " " ++ thing ++ " has no "
                    Nothing -> "There is no ")
              ++ intercalate " or " [ mungeThing thing ++ " '" ++ got ++ "'"
                                    | (thing, got, _alts) <- nosuch' ] ++ "."
              ++ if null alternatives then "" else
                 "\nPerhaps you meant " ++ intercalate ";\nor "
                 [ "the " ++ thing ++ " " ++ intercalate " or " alts
                 | (thing, alts) <- alternatives ]
            | (inside, nosuch') <- groupByContainer nosuch
            , let alternatives =
                    [ (thing, take 10 alts) --TODO: select best ones
                    | (thing,_got,alts@(_:_)) <- nosuch' ]
            ]
          | (target, nosuch) <- targets
          , let groupByContainer =
                    map (\g@((inside,_,_,_):_) ->
                            (inside, [   (thing,got,alts)
                                     | (_,thing,got,alts) <- g ]))
                  . groupBy ((==)    `on` (\(x,_,_,_) -> x))
                  . sortBy  (compare `on` (\(x,_,_,_) -> x))
          ]
        where
          mungeThing "file" = "file target"
          mungeThing thing  = thing

    case [ (t, ts) | BuildTargetAmbiguous t ts <- problems ] of
      []      -> return ()
      targets ->
        die $ unlines
          [    "Ambiguous build target '" ++ showUserBuildTarget target
            ++ "'. It could be:\n "
            ++ unlines [ "   "++ showUserBuildTarget ut ++
                         " (" ++ showBuildTargetKind bt ++ ")"
                       | (ut, bt) <- amb ]
          | (target, amb) <- targets ]

  where
    showBuildTargetKind (BuildTargetPackage   _    ) = "package"
    showBuildTargetKind (BuildTargetComponent _ _  ) = "component"
    showBuildTargetKind (BuildTargetModule    _ _ _) = "module"
    showBuildTargetKind (BuildTargetFile      _ _ _) = "file"


----------------------------------
-- Top level BuildTarget matcher
--

matchBuildTarget :: [PackageInfo] -> [PackageInfo]
                 -> UserBuildTargetFileStatus
                 -> Match (BuildTarget PackageInfo)
matchBuildTarget ppinfo opinfo = \utarget ->
    nubMatchesBy ((==) `on` (fmap packageName)) $
    case utarget of
      UserBuildTargetFileStatus1 str1 fstatus1 ->
        matchBuildTarget1 ppinfo opinfo str1 fstatus1

      UserBuildTargetFileStatus2 str1 fstatus1 str2 ->
        matchBuildTarget2 pinfo str1 fstatus1 str2

      UserBuildTargetFileStatus3 str1 fstatus1 str2 str3 ->
        matchBuildTarget3 pinfo str1 fstatus1 str2 str3

      UserBuildTargetFileStatus4 str1 fstatus1 str2 str3 str4 ->
        matchBuildTarget4 pinfo str1 fstatus1 str2 str3 str4
  where
    pinfo  = ppinfo ++ opinfo
    --TODO: sort this out


matchBuildTarget1 :: [PackageInfo] -> [PackageInfo]
                  -> String -> FileStatus -> Match (BuildTarget PackageInfo)
matchBuildTarget1 ppinfo opinfo = \str1 fstatus1 ->
         match1Cmp pcinfo str1
    <//> match1Pkg pinfo  str1 fstatus1
    <//> match1Cmp ocinfo str1
    <//> match1Mod cinfo  str1
    <//> match1Fil pinfo  str1 fstatus1
  where
    pinfo  = ppinfo ++ opinfo
    cinfo  = concatMap pinfoComponents pinfo
    pcinfo = concatMap pinfoComponents ppinfo
    ocinfo = concatMap pinfoComponents opinfo


matchBuildTarget2 :: [PackageInfo] -> String -> FileStatus -> String
                  -> Match (BuildTarget PackageInfo)
matchBuildTarget2 pinfo str1 fstatus1 str2 =
        match2PkgCmp pinfo str1 fstatus1 str2
   <|>  match2KndCmp cinfo str1          str2
   <//> match2PkgMod pinfo str1 fstatus1 str2
   <//> match2CmpMod cinfo str1          str2
   <//> match2PkgFil pinfo str1 fstatus1 str2
   <//> match2CmpFil cinfo str1          str2
  where
    cinfo = concatMap pinfoComponents pinfo
    --TODO: perhaps we actually do want to prioritise local/primary components


matchBuildTarget3 :: [PackageInfo] -> String -> FileStatus -> String -> String
                  -> Match (BuildTarget PackageInfo)
matchBuildTarget3 pinfo str1 fstatus1 str2 str3 =
        match3PkgKndCmp pinfo str1 fstatus1 str2 str3 
   <//> match3PkgCmpMod pinfo str1 fstatus1 str2 str3
   <//> match3PkgCmpFil pinfo str1 fstatus1 str2 str3
   <//> match3KndCmpMod cinfo str1          str2 str3
   <//> match3KndCmpFil cinfo str1          str2 str3
  where
    cinfo = concatMap pinfoComponents pinfo


matchBuildTarget4 :: [PackageInfo]
                  -> String -> FileStatus -> String -> String -> String
                  -> Match (BuildTarget PackageInfo)
matchBuildTarget4 pinfo str1 fstatus1 str2 str3 str4 =
        match4PkgKndCmpMod pinfo str1 fstatus1 str2 str3 str4
   <//> match4PkgKndCmpFil pinfo str1 fstatus1 str2 str3 str4


------------------------------------
-- Individual BuildTarget matchers
--

match1Pkg :: [PackageInfo] -> String -> FileStatus
          -> Match (BuildTarget PackageInfo)
match1Pkg pinfo = \str1 fstatus1 -> do
    guardPackage            str1 fstatus1
    p <- matchPackage pinfo str1 fstatus1
    return (BuildTargetPackage p)

match1Cmp :: [ComponentInfo] -> String -> Match (BuildTarget PackageInfo)
match1Cmp cs = \str1 -> do
    guardComponentName str1
    c <- matchComponentName cs str1
    return (BuildTargetComponent (cinfoPackage c) (cinfoName c))

match1Mod :: [ComponentInfo] -> String -> Match (BuildTarget PackageInfo)
match1Mod cs = \str1 -> do
    guardModuleName str1
    let ms = [ (m,c) | c <- cs, m <- cinfoModules c ]
    (m,c) <- matchModuleNameAnd ms str1
    return (BuildTargetModule (cinfoPackage c) (cinfoName c) m)

match1Fil :: [PackageInfo] -> String -> FileStatus
          -> Match (BuildTarget PackageInfo)
match1Fil ps str1 fstatus1 =
    expecting "file" str1 $ do
    (pkgfile, p) <- matchPackageDirectoryPrefix ps fstatus1
    orNoThingIn "package" (display (packageName p)) $ do
      (filepath, c) <- matchComponentFile (pinfoComponents p) pkgfile
      return (BuildTargetFile p (cinfoName c) filepath)

---

match2PkgCmp :: [PackageInfo]
             -> String -> FileStatus -> String
             -> Match (BuildTarget PackageInfo)
match2PkgCmp ps = \str1 fstatus1 str2 -> do
    guardPackage         str1 fstatus1
    guardComponentName   str2
    p <- matchPackage ps str1 fstatus1
    orNoThingIn "package" (display (packageName p)) $ do
      c <- matchComponentName (pinfoComponents p) str2
      return (BuildTargetComponent p (cinfoName c))
    --TODO: the error here ought to say there's no component by that name in
    -- this package, and name the package

match2KndCmp :: [ComponentInfo] -> String -> String
             -> Match (BuildTarget PackageInfo)
match2KndCmp cs = \str1 str2 -> do
    ckind <- matchComponentKind str1
    guardComponentName str2
    c <- matchComponentKindAndName cs ckind str2
    return (BuildTargetComponent (cinfoPackage c) (cinfoName c))

match2PkgMod :: [PackageInfo] -> String -> FileStatus -> String
             -> Match (BuildTarget PackageInfo)
match2PkgMod ps = \str1 fstatus1 str2 -> do
    guardPackage         str1 fstatus1
    guardModuleName      str2
    p <- matchPackage ps str1 fstatus1
    orNoThingIn "package" (display (packageName p)) $ do
      let ms = [ (m,c) | c <- pinfoComponents p, m <- cinfoModules c ]
      (m,c) <- matchModuleNameAnd ms str2
      return (BuildTargetModule p (cinfoName c) m)

match2CmpMod :: [ComponentInfo] -> String -> String
             -> Match (BuildTarget PackageInfo)
match2CmpMod cs = \str1 str2 -> do
    guardComponentName str1
    guardModuleName    str2
    c <- matchComponentName cs str1
    orNoThingIn "component" (cinfoStrName c) $ do
      let ms = cinfoModules c
      m <- matchModuleName ms str2
      return (BuildTargetModule (cinfoPackage c) (cinfoName c) m)

match2PkgFil :: [PackageInfo] -> String -> FileStatus -> String
             -> Match (BuildTarget PackageInfo)
match2PkgFil ps str1 fstatus1 str2 = do
    guardPackage         str1 fstatus1
    p <- matchPackage ps str1 fstatus1
    orNoThingIn "package" (display (packageName p)) $ do
      (filepath, c) <- matchComponentFile (pinfoComponents p) str2
      return (BuildTargetFile p (cinfoName c) filepath)

match2CmpFil :: [ComponentInfo] -> String -> String
             -> Match (BuildTarget PackageInfo)
match2CmpFil cs str1 str2 = do
    guardComponentName str1
    c <- matchComponentName cs str1
    orNoThingIn "component" (cinfoStrName c) $ do
      (filepath, _) <- matchComponentFile [c] str2
      return (BuildTargetFile (cinfoPackage c) (cinfoName c) filepath)

---

match3PkgKndCmp :: [PackageInfo]
                -> String -> FileStatus -> String -> String
                -> Match (BuildTarget PackageInfo)
match3PkgKndCmp ps = \str1 fstatus1 str2 str3 -> do
    guardPackage         str1 fstatus1
    ckind <- matchComponentKind str2
    guardComponentName   str3
    p <- matchPackage ps str1 fstatus1
    orNoThingIn "package" (display (packageName p)) $ do
      c <- matchComponentKindAndName (pinfoComponents p) ckind str3
      return (BuildTargetComponent p (cinfoName c))

match3PkgCmpMod :: [PackageInfo]
                -> String -> FileStatus -> String -> String
                -> Match (BuildTarget PackageInfo)
match3PkgCmpMod ps = \str1 fstatus1 str2 str3 -> do
    guardPackage str1 fstatus1
    guardComponentName str2
    guardModuleName    str3
    p <- matchPackage ps str1 fstatus1
    orNoThingIn "package" (display (packageName p)) $ do
      c <- matchComponentName (pinfoComponents p) str2
      orNoThingIn "component" (cinfoStrName c) $ do
        let ms = cinfoModules c
        m <- matchModuleName ms str3
        return (BuildTargetModule p (cinfoName c) m)

match3KndCmpMod :: [ComponentInfo]
                -> String -> String -> String
                -> Match (BuildTarget PackageInfo)
match3KndCmpMod cs = \str1 str2 str3 -> do
    ckind <- matchComponentKind str1
    guardComponentName str2
    guardModuleName    str3
    c <- matchComponentKindAndName cs ckind str2
    orNoThingIn "component" (cinfoStrName c) $ do
      let ms = cinfoModules c
      m <- matchModuleName ms str3
      return (BuildTargetModule (cinfoPackage c) (cinfoName c) m)

match3PkgCmpFil :: [PackageInfo]
                -> String -> FileStatus -> String -> String
                -> Match (BuildTarget PackageInfo)
match3PkgCmpFil ps = \str1 fstatus1 str2 str3 -> do
    guardPackage         str1 fstatus1
    guardComponentName   str2
    p <- matchPackage ps str1 fstatus1
    orNoThingIn "package" (display (packageName p)) $ do
      c <- matchComponentName (pinfoComponents p) str2
      orNoThingIn "component" (cinfoStrName c) $ do
        (filepath, _) <- matchComponentFile [c] str3
        return (BuildTargetFile p (cinfoName c) filepath)

match3KndCmpFil :: [ComponentInfo] -> String -> String -> String
                -> Match (BuildTarget PackageInfo)
match3KndCmpFil cs = \str1 str2 str3 -> do
    ckind <- matchComponentKind str1
    guardComponentName str2
    c <- matchComponentKindAndName cs ckind str2
    orNoThingIn "component" (cinfoStrName c) $ do
      (filepath, _) <- matchComponentFile [c] str3
      return (BuildTargetFile (cinfoPackage c) (cinfoName c) filepath)

--

match4PkgKndCmpMod :: [PackageInfo]
                   -> String-> FileStatus -> String -> String -> String
                   -> Match (BuildTarget PackageInfo)
match4PkgKndCmpMod ps = \str1 fstatus1 str2 str3 str4 -> do
    guardPackage         str1 fstatus1
    ckind <- matchComponentKind str2
    guardComponentName   str3
    guardModuleName      str4
    p <- matchPackage ps str1 fstatus1
    orNoThingIn "package" (display (packageName p)) $ do
      c <- matchComponentKindAndName (pinfoComponents p) ckind str3
      orNoThingIn "component" (cinfoStrName c) $ do
        let ms = cinfoModules c
        m <- matchModuleName ms str4
        return (BuildTargetModule p (cinfoName c) m)

match4PkgKndCmpFil :: [PackageInfo]
                   -> String -> FileStatus -> String -> String -> String
                   -> Match (BuildTarget PackageInfo)
match4PkgKndCmpFil ps = \str1 fstatus1 str2 str3 str4 -> do
    guardPackage       str1 fstatus1
    ckind <- matchComponentKind str2
    guardComponentName str3
    p     <- matchPackage ps    str1 fstatus1
    orNoThingIn "package" (display (packageName p)) $ do
      c <- matchComponentKindAndName (pinfoComponents p) ckind str3
      orNoThingIn "component" (cinfoStrName c) $ do
        (filepath,_) <- matchComponentFile [c] str4
        return (BuildTargetFile p (cinfoName c) filepath)


-------------------------------
-- Package and component info
--

data PackageInfo = PackageInfo {
       pinfoId          :: PackageId,
       pinfoLocation    :: PackageLocation (),
       pinfoDirectory   :: Maybe (FilePath, FilePath),
       pinfoPackageFile :: Maybe (FilePath, FilePath),
       pinfoComponents  :: [ComponentInfo]
     }

data ComponentInfo = ComponentInfo {
       cinfoName    :: ComponentName,
       cinfoStrName :: ComponentStringName,
       cinfoPackage :: PackageInfo,
       cinfoSrcDirs :: [FilePath],
       cinfoModules :: [ModuleName],
       cinfoHsFiles :: [FilePath],   -- other hs files (like main.hs)
       cinfoCFiles  :: [FilePath],
       cinfoJsFiles :: [FilePath]
     }

type ComponentStringName = String

instance Package PackageInfo where
  packageId = pinfoId

--TODO: [required eventually] need the original GenericPackageDescription or
-- the flattening thereof because we need to be able to target modules etc
-- that are not enabled in the current configuration.
selectPackageInfo :: PackageDescription -> PackageLocation a -> IO PackageInfo
selectPackageInfo pkg loc = do
    (pkgdir, pkgfile) <-
      case loc of
        --TODO: local tarballs, remote tarballs etc
        LocalUnpackedPackage dir -> do 
          dirabs <- canonicalizePath dir
          dirrel <- makeRelativeToCwd dirabs
          --TODO: ought to get this earlier in project reading
          let fileabs = dirabs </> display (packageName pkg) <.> "cabal"
              filerel = dirrel </> display (packageName pkg) <.> "cabal"
          exists <- doesFileExist fileabs
          return ( Just (dirabs, dirrel)
                 , if exists then Just (fileabs, filerel) else Nothing
                 )
        _ -> return (Nothing, Nothing)
    let pinfo =
          PackageInfo {
            pinfoId          = packageId pkg,
            pinfoLocation    = fmap (const ()) loc,
            pinfoDirectory   = pkgdir,
            pinfoPackageFile = pkgfile,
            pinfoComponents  = selectComponentInfo pinfo pkg
          }
    return pinfo


selectComponentInfo :: PackageInfo -> PackageDescription -> [ComponentInfo]
selectComponentInfo pinfo pkg =
    [ ComponentInfo {
        cinfoName    = componentName c,
        cinfoStrName = componentStringName (packageName pkg) (componentName c),
        cinfoPackage = pinfo,
        cinfoSrcDirs = hsSourceDirs bi,
--                       [ pkgroot </> srcdir
--                       | (pkgroot,_) <- maybeToList (pinfoDirectory pinfo)
--                       , srcdir <- hsSourceDirs bi ],
        cinfoModules = componentModules c,
        cinfoHsFiles = componentHsFiles c,
        cinfoCFiles  = cSources bi,
        cinfoJsFiles = jsSources bi
      }
    | c <- pkgComponents pkg
    , let bi = componentBuildInfo c ]


componentStringName :: PackageName -> ComponentName -> ComponentStringName
componentStringName pkgname CLibName         = display pkgname
componentStringName _      (CExeName  name)  = name
componentStringName _      (CTestName  name) = name
componentStringName _      (CBenchName name) = name

componentModules :: Component -> [ModuleName]
componentModules (CLib   lib)   = libModules lib
componentModules (CExe   exe)   = exeModules exe
componentModules (CTest  test)  = testModules test
componentModules (CBench bench) = benchmarkModules bench

componentHsFiles :: Component -> [FilePath]
componentHsFiles (CExe exe) = [modulePath exe]
componentHsFiles (CTest  TestSuite {
                           testInterface = TestSuiteExeV10 _ mainfile
                         }) = [mainfile]
componentHsFiles (CBench Benchmark {
                           benchmarkInterface = BenchmarkExeV10 _ mainfile
                         }) = [mainfile]
componentHsFiles _          = []


------------------------------
-- Matching component kinds
--

data ComponentKind = LibKind | ExeKind | TestKind | BenchKind
  deriving (Eq, Ord, Show)

componentKind :: ComponentName -> ComponentKind
componentKind CLibName       = LibKind
componentKind (CExeName  _)  = ExeKind
componentKind (CTestName  _) = TestKind
componentKind (CBenchName _) = BenchKind

cinfoKind :: ComponentInfo -> ComponentKind
cinfoKind = componentKind . cinfoName

matchComponentKind :: String -> Match ComponentKind
matchComponentKind s
  | s `elem` ["lib", "library"]            = increaseConfidence >> return LibKind
  | s `elem` ["exe", "executable"]         = increaseConfidence >> return ExeKind
  | s `elem` ["tst", "test", "test-suite"] = increaseConfidence
                                             >> return TestKind
  | s `elem` ["bench", "benchmark"]        = increaseConfidence
                                             >> return BenchKind
  | otherwise                              = matchErrorExpected
                                             "component kind" s

showComponentKind :: ComponentKind -> String
showComponentKind LibKind   = "library"
showComponentKind ExeKind   = "executable"
showComponentKind TestKind  = "test-suite"
showComponentKind BenchKind = "benchmark"

showComponentKindShort :: ComponentKind -> String
showComponentKindShort LibKind   = "lib"
showComponentKindShort ExeKind   = "exe"
showComponentKindShort TestKind  = "test"
showComponentKindShort BenchKind = "bench"

------------------------------
-- Matching package targets
--

guardPackage :: String -> FileStatus -> Match ()
guardPackage str fstatus =
      guardPackageName str
  <|> guardPackageDir  str fstatus
  <|> guardPackageFile str fstatus


guardPackageName :: String -> Match ()
guardPackageName s
  | validPackgageName s = increaseConfidence
  | otherwise           = matchErrorExpected "package name" s
  where

validPackgageName :: String -> Bool
validPackgageName s =
       all validPackgageNameChar s
    && not (null s)
  where
    validPackgageNameChar c = isAlphaNum c || c == '-'


guardPackageDir :: String -> FileStatus -> Match ()
guardPackageDir _ (FileStatusExistsDir _) = increaseConfidence
guardPackageDir str _ = matchErrorExpected "package directory" str


guardPackageFile :: String -> FileStatus -> Match ()
guardPackageFile _ (FileStatusExistsFile file)
                       | takeExtension file == ".cabal"
                       = increaseConfidence
guardPackageFile str _ = matchErrorExpected "package .cabal file" str


matchPackage :: [PackageInfo] -> String -> FileStatus -> Match PackageInfo
matchPackage pinfo = \str fstatus ->
    orNoThingIn "project" "" $
          matchPackageName pinfo str
    <//> (matchPackageDir  pinfo str fstatus
     <|>  matchPackageFile pinfo str fstatus)


matchPackageName :: [PackageInfo] -> String -> Match PackageInfo
matchPackageName ps = \str -> do
    guard (validPackgageName str)
    orNoSuchThing "package" str
                  (map (display . packageName) ps) $
      increaseConfidenceFor $
        matchInexactly caseFold (display . packageName) ps str


matchPackageDir :: [PackageInfo]
                -> String -> FileStatus -> Match PackageInfo
matchPackageDir ps = \str fstatus ->
    case fstatus of
      FileStatusExistsDir canondir -> 
        orNoSuchThing "package directory" str (map (snd . fst) dirs) $
          increaseConfidenceFor $
            fmap snd $ matchExactly (fst . fst) dirs canondir
      _ -> mzero
  where
    dirs = [ ((dabs,drel),p)
           | p@PackageInfo{ pinfoDirectory = Just (dabs,drel) } <- ps ]


matchPackageFile :: [PackageInfo] -> String -> FileStatus -> Match PackageInfo
matchPackageFile ps = \str fstatus -> do
    case fstatus of
      FileStatusExistsFile canonfile -> 
        orNoSuchThing "package .cabal file" str (map (snd . fst) files) $
          increaseConfidenceFor $
            fmap snd $ matchExactly (fst . fst) files canonfile
      _ -> mzero
  where
    files = [ ((fabs,frel),p)
            | p@PackageInfo{ pinfoPackageFile = Just (fabs,frel) } <- ps ]

--TODO: test outcome when dir exists but doesn't match any known one

--TODO: perhaps need another distinction, vs no such thing, point is the
--      thing is not known, within the project, but could be outside project


------------------------------
-- Matching component targets
--


guardComponentName :: String -> Match ()
guardComponentName s
  | all validComponentChar s
    && not (null s)  = increaseConfidence
  | otherwise        = matchErrorExpected "component name" s
  where
    validComponentChar c = isAlphaNum c || c == '.'
                        || c == '_' || c == '-' || c == '\''


matchComponentName :: [ComponentInfo] -> String -> Match ComponentInfo
matchComponentName cs str =
    orNoSuchThing "component" str (map cinfoStrName cs)
  $ increaseConfidenceFor
  $ matchInexactly caseFold cinfoStrName cs str


matchComponentKindAndName :: [ComponentInfo] -> ComponentKind -> String
                          -> Match ComponentInfo
matchComponentKindAndName cs ckind str =
    orNoSuchThing (showComponentKind ckind ++ " component") str
                  (map render cs)
  $ increaseConfidenceFor
  $ matchInexactly (\(ck, cn) -> (ck, caseFold cn))
                   (\c -> (cinfoKind c, cinfoStrName c))
                   cs
                   (ckind, str)
  where
    render c = showComponentKindShort (cinfoKind c) ++ ":" ++ cinfoStrName c


------------------------------
-- Matching module targets
--

guardModuleName :: String -> Match ()
guardModuleName s =
  case simpleParse s :: Maybe ModuleName of
    Just _                   -> increaseConfidence
    _ | all validModuleChar s
        && not (null s)      -> return ()
      | otherwise            -> matchErrorExpected "module name" s
    where
      validModuleChar c = isAlphaNum c || c == '.' || c == '_' || c == '\''


matchModuleName :: [ModuleName] -> String -> Match ModuleName
matchModuleName ms str =
    orNoSuchThing "module" str (map display ms)
  $ increaseConfidenceFor
  $ matchInexactly caseFold display ms str


matchModuleNameAnd :: [(ModuleName, a)] -> String -> Match (ModuleName, a)
matchModuleNameAnd ms str =
    orNoSuchThing "module" str (map (display . fst) ms)
  $ increaseConfidenceFor
  $ matchInexactly caseFold (display . fst) ms str


------------------------------
-- Matching file targets
--

matchPackageDirectoryPrefix :: [PackageInfo] -> FileStatus
                            -> Match (FilePath, PackageInfo)
matchPackageDirectoryPrefix ps (FileStatusExistsFile filepath) =
    increaseConfidenceFor $
      matchDirectoryPrefix pkgdirs filepath
  where
    pkgdirs = [ (dir, p)
              | p@PackageInfo { pinfoDirectory = Just (dir,_) } <- ps ]
matchPackageDirectoryPrefix _ _ = mzero


matchComponentFile :: [ComponentInfo] -> String
                   -> Match (FilePath, ComponentInfo)
matchComponentFile cs str =
    orNoSuchThing "file" str [] $
        matchComponentModuleFile cs str
    <|> matchComponentOtherFile  cs str


matchComponentOtherFile :: [ComponentInfo] -> String
                        -> Match (FilePath, ComponentInfo)
matchComponentOtherFile cs =
    matchFile
      [ (file, c)
      | c    <- cs
      , file <- cinfoHsFiles c
             ++ cinfoCFiles  c
             ++ cinfoJsFiles c
      ]


matchComponentModuleFile :: [ComponentInfo] -> String
                         -> Match (FilePath, ComponentInfo)
matchComponentModuleFile cs str = do
    matchFile
      [ (normalise (d </> toFilePath m), c)
      | c <- cs
      , d <- cinfoSrcDirs c
      , m <- cinfoModules c
      ]
      (dropExtension (normalise str))

-- utils

matchFile :: [(FilePath, a)] -> FilePath -> Match (FilePath, a)
matchFile fs =
      increaseConfidenceFor
    . matchInexactly caseFold fst fs

matchDirectoryPrefix :: [(FilePath, a)] -> FilePath -> Match (FilePath, a)
matchDirectoryPrefix dirs filepath =
    tryEach $
      [ (file, x)
      | (dir,x) <- dirs
      , file <- maybeToList (stripDirectory dir) ]
  where
    stripDirectory :: FilePath -> Maybe FilePath
    stripDirectory dir =
      joinPath `fmap` stripPrefix (splitDirectories dir) filepathsplit

    filepathsplit = splitDirectories filepath


------------------------------
-- Matching monad
--

-- | A matcher embodies a way to match some input as being some recognised
-- value. In particular it deals with multiple and ambiguous matches.
--
-- There are various matcher primitives ('matchExactly', 'matchInexactly'),
-- ways to combine matchers ('matchPlus', 'matchPlusShadowing') and finally we
-- can run a matcher against an input using 'findMatch'.
--
data Match a = NoMatch      Confidence [MatchError]
             | ExactMatch   Confidence [a]
             | InexactMatch Confidence [a]
  deriving Show

type Confidence = Int

data MatchError = MatchErrorExpected String String            -- thing got
                | MatchErrorNoSuch   String String [String]   -- thing got alts
                | MatchErrorIn       String String MatchError -- kind  thing
  deriving (Show, Eq)


instance Functor Match where
    fmap _ (NoMatch      d ms) = NoMatch      d ms
    fmap f (ExactMatch   d xs) = ExactMatch   d (fmap f xs)
    fmap f (InexactMatch d xs) = InexactMatch d (fmap f xs)

instance Applicative Match where
    pure a = ExactMatch 0 [a]
    (<*>)  = ap

instance Alternative Match where
    empty = NoMatch 0 []
    (<|>) = matchPlus

instance Monad Match where
    return                  = pure
    NoMatch      d ms >>= _ = NoMatch d ms
    ExactMatch   d xs >>= f = addDepth d
                            $ msum (map f xs)
    InexactMatch d xs >>= f = addDepth d . forceInexact
                            $ msum (map f xs)

instance MonadPlus Match where
    mzero = empty
    mplus = matchPlus

(<//>) :: Match a -> Match a -> Match a
(<//>) = matchPlusShadowing

infixl 3 <//>

addDepth :: Confidence -> Match a -> Match a
addDepth d' (NoMatch      d msgs) = NoMatch      (d'+d) msgs
addDepth d' (ExactMatch   d xs)   = ExactMatch   (d'+d) xs
addDepth d' (InexactMatch d xs)   = InexactMatch (d'+d) xs

forceInexact :: Match a -> Match a
forceInexact (ExactMatch d ys) = InexactMatch d ys
forceInexact m                 = m

-- | Combine two matchers. Exact matches are used over inexact matches
-- but if we have multiple exact, or inexact then the we collect all the
-- ambiguous matches.
--
matchPlus :: Match a -> Match a -> Match a
matchPlus   (ExactMatch   d1 xs)   (ExactMatch   d2 xs') =
  ExactMatch (max d1 d2) (xs ++ xs')
matchPlus a@(ExactMatch   _  _ )   (InexactMatch _  _  ) = a
matchPlus a@(ExactMatch   _  _ )   (NoMatch      _  _  ) = a
matchPlus   (InexactMatch _  _ ) b@(ExactMatch   _  _  ) = b
matchPlus   (InexactMatch d1 xs)   (InexactMatch d2 xs') =
  InexactMatch (max d1 d2) (xs ++ xs')
matchPlus a@(InexactMatch _  _ )   (NoMatch      _  _  ) = a
matchPlus   (NoMatch      _  _ ) b@(ExactMatch   _  _  ) = b
matchPlus   (NoMatch      _  _ ) b@(InexactMatch _  _  ) = b
matchPlus a@(NoMatch      d1 ms) b@(NoMatch      d2 ms')
                                             | d1 >  d2  = a
                                             | d1 <  d2  = b
                                             | otherwise = NoMatch d1 (ms ++ ms')

-- | Combine two matchers. This is similar to 'matchPlus' with the
-- difference that an exact match from the left matcher shadows any exact
-- match on the right. Inexact matches are still collected however.
--
matchPlusShadowing :: Match a -> Match a -> Match a
matchPlusShadowing a@(ExactMatch _ _) (ExactMatch _ _) = a
matchPlusShadowing a                   b               = matchPlus a b


------------------------------
-- Various match primitives
--

matchErrorExpected :: String -> String -> Match a
matchErrorExpected thing got      = NoMatch 0 [MatchErrorExpected thing got]

matchErrorNoSuch :: String -> String -> [String] -> Match a
matchErrorNoSuch thing got alts = NoMatch 0 [MatchErrorNoSuch thing got alts]

expecting :: String -> String -> Match a -> Match a
expecting thing got (NoMatch 0 _) = matchErrorExpected thing got
expecting _     _   m             = m

orNoSuchThing :: String -> String -> [String] -> Match a -> Match a
orNoSuchThing thing got alts (NoMatch 0 _) = matchErrorNoSuch thing got alts
orNoSuchThing _     _   _    m             = m

orNoThingIn :: String -> String -> Match a -> Match a
orNoThingIn kind name (NoMatch n ms) =
    NoMatch n [ MatchErrorIn kind name m | m <- ms ]
orNoThingIn _ _ m = m

increaseConfidence :: Match ()
increaseConfidence = ExactMatch 1 [()]

increaseConfidenceFor :: Match a -> Match a
increaseConfidenceFor m = m >>= \r -> increaseConfidence >> return r

nubMatchesBy :: (a -> a -> Bool) -> Match a -> Match a
nubMatchesBy _  (NoMatch      d msgs) = NoMatch      d msgs
nubMatchesBy eq (ExactMatch   d xs)   = ExactMatch   d (nubBy eq xs)
nubMatchesBy eq (InexactMatch d xs)   = InexactMatch d (nubBy eq xs)

nubMatchErrors :: Match a -> Match a
nubMatchErrors (NoMatch      d msgs) = NoMatch      d (nub msgs)
nubMatchErrors (ExactMatch   d xs)   = ExactMatch   d xs
nubMatchErrors (InexactMatch d xs)   = InexactMatch d xs

-- | Lift a list of matches to an exact match.
--
exactMatches, inexactMatches :: [a] -> Match a

exactMatches [] = mzero
exactMatches xs = ExactMatch 0 xs

inexactMatches [] = mzero
inexactMatches xs = InexactMatch 0 xs

tryEach :: [a] -> Match a
tryEach = exactMatches


------------------------------
-- Top level match runner
--

-- | Given a matcher and a key to look up, use the matcher to find all the
-- possible matches. There may be 'None', a single 'Unambiguous' match or
-- you may have an 'Ambiguous' match with several possibilities.
--
findMatch :: Match a -> MaybeAmbiguous a
findMatch match =
    case nubMatchErrors match of
      NoMatch    _ msgs -> None msgs
      ExactMatch   _ [x] -> Unambiguous x
      InexactMatch _ [x] -> Unambiguous x
      ExactMatch   _  [] -> error "findMatch: impossible: ExactMatch []"
      InexactMatch _  [] -> error "findMatch: impossible: InexactMatch []"
      ExactMatch   _  xs -> Ambiguous True  xs
      InexactMatch _  xs -> Ambiguous False xs

data MaybeAmbiguous a = None [MatchError] | Unambiguous a | Ambiguous Bool [a]
  deriving Show


------------------------------
-- Basic matchers
--

-- | A primitive matcher that looks up a value in a finite 'Map'. The
-- value must match exactly.
--
matchExactly :: Ord k => (a -> k) -> [a] -> (k -> Match a)
matchExactly key xs =
    \k -> case Map.lookup k m of
            Nothing -> mzero
            Just ys -> exactMatches ys
  where
    m = Map.fromListWith (++) [ (key x, [x]) | x <- xs ]

-- | A primitive matcher that looks up a value in a finite 'Map'. It checks
-- for an exact or inexact match. We get an inexact match if the match
-- is not exact, but the canonical forms match. It takes a canonicalisation
-- function for this purpose.
--
-- So for example if we used string case fold as the canonicalisation
-- function, then we would get case insensitive matching (but it will still
-- report an exact match when the case matches too).
--
matchInexactly :: (Ord k, Ord k') => (k -> k') -> (a -> k)
               -> [a] -> (k -> Match a)
matchInexactly cannonicalise key xs =
    \k -> case Map.lookup k m of
            Just ys -> exactMatches ys
            Nothing -> case Map.lookup (cannonicalise k) m' of
                         Just ys -> inexactMatches ys
                         Nothing -> mzero
  where
    m = Map.fromListWith (++) [ (key x, [x]) | x <- xs ]

    -- the map of canonicalised keys to groups of inexact matches
    m' = Map.mapKeysWith (++) cannonicalise m


------------------------------
-- Utils
--

caseFold :: String -> String
caseFold = lowercase


------------------------------
-- Example inputs
--

{-
ex1pinfo :: [PackageInfo]
ex1pinfo =
  [ PackageInfo {
      pinfoName        = PackageName "foo",
      pinfoDirectory   = Just "/the/foo",
      pinfoPackageFile = Just "/the/foo/foo.cabal",
      pinfoComponents  = []
    }
  , PackageInfo {
      pinfoName        = PackageName "bar",
      pinfoDirectory   = Just "/the/bar",
      pinfoPackageFile = Just "/the/bar/bar.cabal",
      pinfoComponents  = []
    }
  ]
-}
{-
stargets =
  [ BuildTargetComponent (CExeName "foo")
  , BuildTargetModule    (CExeName "foo") (mkMn "Foo")
  , BuildTargetModule    (CExeName "tst") (mkMn "Foo")
  ]
    where
    mkMn :: String -> ModuleName
    mkMn  = fromJust . simpleParse

ex_pkgid :: PackageIdentifier
Just ex_pkgid = simpleParse "thelib"
-}

{-
ex_cs :: [ComponentInfo]
ex_cs =
  [ (mkC (CExeName "foo") ["src1", "src1/src2"] ["Foo", "Src2.Bar", "Bar"])
  , (mkC (CExeName "tst") ["src1", "test"]      ["Foo"])
  ]
    where
    mkC n ds ms = ComponentInfo n (componentStringName pkgid n) ds (map mkMn ms)
    mkMn :: String -> ModuleName
    mkMn  = fromJust . simpleParse
    pkgid :: PackageIdentifier
    Just pkgid = simpleParse "thelib"
-}

