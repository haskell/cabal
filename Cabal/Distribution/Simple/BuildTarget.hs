-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.BuildTargets
-- Copyright   :  (c) Duncan Coutts 2012
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
--
-- Handling for user-specified build targets
-----------------------------------------------------------------------------
module Distribution.Simple.BuildTarget (
  
    -- * Build targets
    BuildTarget(..),
    readBuildTargets,

    -- * Parsing user build targets
    UserBuildTarget,
    readUserBuildTargets,
    UserBuildTargetProblem(..),
    reportUserBuildTargetProblems,

    -- * Resolving build targets
    resolveBuildTargets,
    BuildTargetProblem(..),
    reportBuildTargetProblems,
  ) where

import Distribution.Package
         ( Package(..), PackageId, packageName )

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
         ( display )
import Distribution.Simple.Utils
         ( die, lowercase, equating )

import Data.List
         ( nub, stripPrefix, sortBy, groupBy, partition, intercalate )
import Data.Ord
import Data.Maybe
         ( listToMaybe, catMaybes )
import Data.Either
         ( partitionEithers )
import qualified Data.Map as Map
import Control.Monad
import Control.Applicative (Applicative(..))
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP
         ( (+++), (<++) )
import Data.Char
         ( isSpace, isAlphaNum )
import System.FilePath as FilePath
         ( dropExtension, normalise, splitDirectories, joinPath, splitPath
         , hasTrailingPathSeparator )
import System.Directory
         ( doesFileExist, doesDirectoryExist )

-- ------------------------------------------------------------
-- * User build targets
-- ------------------------------------------------------------

-- | Various ways that a user may specify a build target.
--
data UserBuildTarget =
     
     -- | A target specified by a single name. This could be a component
     -- module or file.
     --
     -- > cabal build foo
     -- > cabal build Data.Foo
     -- > cabal build Data/Foo.hs  Data/Foo.hsc
     --
     UserBuildTargetSingle String

     -- | A target specified by a qualifier and name. This could be a component
     -- name qualified by the component namespace kind, or a module or file
     -- qualified by the component name.
     --
     -- > cabal build lib:foo exe:foo
     -- > cabal build foo:Data.Foo
     -- > cabal build foo:Data/Foo.hs
     --
   | UserBuildTargetDouble String String

     -- A fully qualified target, either a module or file qualified by a
     -- component name with the component namespace kind.
     --
     -- > cabal build lib:foo:Data/Foo.hs exe:foo:Data/Foo.hs
     -- > cabal build lib:foo:Data.Foo exe:foo:Data.Foo
     --
   | UserBuildTargetTriple String String String
  deriving (Show, Eq, Ord)


-- ------------------------------------------------------------
-- * Resolved build targets
-- ------------------------------------------------------------

-- | A fully resolved build target.
--
data BuildTarget =

     -- | A specific component
     --
     BuildTargetComponent ComponentName

     -- | A specific module within a specific component.
     --
   | BuildTargetModule ComponentName ModuleName

     -- | A specific file within a specific component.
     --
   | BuildTargetFile ComponentName FilePath
  deriving (Show,Eq)


-- ------------------------------------------------------------
-- * Do everything
-- ------------------------------------------------------------

readBuildTargets :: PackageDescription -> [String] -> IO [BuildTarget]
readBuildTargets pkg targetStrs = do
    let (uproblems, utargets) = readUserBuildTargets targetStrs
    reportUserBuildTargetProblems uproblems

    utargets' <- mapM checkTargetExistsAsFile utargets

    let (bproblems, btargets) = resolveBuildTargets pkg utargets'
    reportBuildTargetProblems bproblems

    return btargets

checkTargetExistsAsFile :: UserBuildTarget -> IO (UserBuildTarget, Bool)
checkTargetExistsAsFile t = do
    fexists <- existsAsFile (fileComponentOfTarget t)
    return (t, fexists)
  
  where
    existsAsFile f = do
      exists <- doesFileExist f
      case splitPath f of
        (d:_)   | hasTrailingPathSeparator d -> doesDirectoryExist d
        (d:_:_) | not exists                 -> doesDirectoryExist d
        _                                    -> return exists

    fileComponentOfTarget (UserBuildTargetSingle     s1) = s1
    fileComponentOfTarget (UserBuildTargetDouble _   s2) = s2
    fileComponentOfTarget (UserBuildTargetTriple _ _ s3) = s3


-- ------------------------------------------------------------
-- * Parsing user targets
-- ------------------------------------------------------------

readUserBuildTargets :: [String] -> ([UserBuildTargetProblem]
                                    ,[UserBuildTarget])
readUserBuildTargets = partitionEithers . map readUserBuildTarget

readUserBuildTarget :: String -> Either UserBuildTargetProblem
                                        UserBuildTarget
readUserBuildTarget targetstr =
    case readPToMaybe parseTargetApprox targetstr of
      Nothing  -> Left  (UserBuildTargetUnrecognised targetstr)
      Just tgt -> Right tgt

  where
    parseTargetApprox :: Parse.ReadP r UserBuildTarget
    parseTargetApprox =
          (do a <- tokenQ
              return (UserBuildTargetSingle a))
      +++ (do a <- token
              _ <- Parse.char ':'
              b <- tokenQ
              return (UserBuildTargetDouble a b))
      +++ (do a <- token
              _ <- Parse.char ':'
              b <- token
              _ <- Parse.char ':'
              c <- tokenQ
              return (UserBuildTargetTriple a b c))

    token  = Parse.munch1 (\x -> not (isSpace x) && x /= ':')
    tokenQ = parseHaskellString <++ token
    parseHaskellString :: Parse.ReadP r String
    parseHaskellString = Parse.readS_to_P reads

    readPToMaybe :: Parse.ReadP a a -> String -> Maybe a
    readPToMaybe p str = listToMaybe [ r | (r,s) <- Parse.readP_to_S p str
                                         , all isSpace s ]

data UserBuildTargetProblem
   = UserBuildTargetUnrecognised String
  deriving Show

reportUserBuildTargetProblems :: [UserBuildTargetProblem] -> IO ()
reportUserBuildTargetProblems problems = do
    case [ target | UserBuildTargetUnrecognised target <- problems ] of
      []     -> return ()
      target ->
        die $ unlines
                [ "Unrecognised build target '" ++ name ++ "'."
                | name <- target ]
           ++ "Examples:\n"
           ++ " - build foo          -- component name (library, executable, test-suite or benchmark)\n"
           ++ " - build Data.Foo     -- module name\n"
           ++ " - build Data/Foo.hsc -- file name\n"
           ++ " - build lib:foo exe:foo   -- component qualified by kind\n"
           ++ " - build foo:Data.Foo      -- module qualified by component\n"
           ++ " - build foo:Data/Foo.hsc  -- file qualified by component"

showUserBuildTarget :: UserBuildTarget -> String
showUserBuildTarget = intercalate ":" . components
  where
    components (UserBuildTargetSingle s1)       = [s1]
    components (UserBuildTargetDouble s1 s2)    = [s1,s2]
    components (UserBuildTargetTriple s1 s2 s3) = [s1,s2,s3]


-- ------------------------------------------------------------
-- * Resolving user targets to build targets
-- ------------------------------------------------------------

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

-- | Given a bunch of user-specified targets, try to resolve what it is they
-- refer to.
--
resolveBuildTargets :: PackageDescription
                    -> [(UserBuildTarget, Bool)]
                    -> ([BuildTargetProblem], [BuildTarget])
resolveBuildTargets pkg = partitionEithers
                        . map (uncurry (resolveBuildTarget pkg))

resolveBuildTarget :: PackageDescription -> UserBuildTarget -> Bool
                   -> Either BuildTargetProblem BuildTarget
resolveBuildTarget pkg userTarget fexists =
    case findMatch (matchBuildTarget pkg userTarget fexists) of
      Unambiguous target  -> Right target
      Ambiguous   targets -> Left (BuildTargetAmbigious userTarget targets')
                               where targets' = disambiguateBuildTargets
                                                    (packageId pkg) userTarget targets
      None        errs    -> Left (classifyMatchErrors errs)

  where
    classifyMatchErrors errs
      | not (null expected) = let (things, got:_) = unzip expected in
                              BuildTargetExpected userTarget things got
      | not (null nosuch)   = BuildTargetNoSuch   userTarget nosuch
      | otherwise = error $ "resolveBuildTarget: internal error in matching"
      where
        expected = [ (thing, got) | MatchErrorExpected thing got <- errs ]
        nosuch   = [ (thing, got) | MatchErrorNoSuch   thing got <- errs ]


data BuildTargetProblem
   = BuildTargetExpected  UserBuildTarget [String]  String    --  [expected thing] (actually got)
   | BuildTargetNoSuch    UserBuildTarget [(String, String)]  --  [(no such thing,  actually got)]
   | BuildTargetAmbigious UserBuildTarget [(UserBuildTarget, BuildTarget)]
  deriving Show


disambiguateBuildTargets :: PackageId -> UserBuildTarget -> [BuildTarget] -> [(UserBuildTarget, BuildTarget)]
disambiguateBuildTargets pkgid original =
    disambiguate (userTargetQualLevel original)
  where
    disambiguate ql ts
        | null amb  = unamb
        | otherwise = unamb ++ disambiguate (succ ql) amb
      where
        (amb, unamb) = step ql ts

    userTargetQualLevel (UserBuildTargetSingle _    ) = QL1
    userTargetQualLevel (UserBuildTargetDouble _ _  ) = QL2
    userTargetQualLevel (UserBuildTargetTriple _ _ _) = QL3

    step  :: QualLevel -> [BuildTarget]
          -> ([BuildTarget], [(UserBuildTarget, BuildTarget)])
    step ql = (\(amb, unamb) -> (map snd $ concat amb, concat unamb))
            . partition (\g -> length g > 1)
            . groupBy (equating fst)
            . sortBy (comparing fst)
            . map (\t -> (renderBuildTarget ql t pkgid, t))

data QualLevel = QL1 | QL2 | QL3
  deriving (Enum, Show)

renderBuildTarget :: QualLevel -> BuildTarget -> PackageId -> UserBuildTarget
renderBuildTarget ql target pkgid =
    case ql of
      QL1 -> UserBuildTargetSingle s1        where  s1          = single target
      QL2 -> UserBuildTargetDouble s1 s2     where (s1, s2)     = double target
      QL3 -> UserBuildTargetTriple s1 s2 s3  where (s1, s2, s3) = triple target

  where
    single (BuildTargetComponent cn  ) = dispCName cn
    single (BuildTargetModule    _  m) = display m
    single (BuildTargetFile      _  f) = f
    
    double (BuildTargetComponent cn  ) = (dispKind cn, dispCName cn)
    double (BuildTargetModule    cn m) = (dispCName cn, display m)
    double (BuildTargetFile      cn f) = (dispCName cn, f)

    triple (BuildTargetComponent _   ) = error "triple BuildTargetComponent"
    triple (BuildTargetModule    cn m) = (dispKind cn, dispCName cn, display m)
    triple (BuildTargetFile      cn f) = (dispKind cn, dispCName cn, f)

    dispCName = componentStringName pkgid
    dispKind  = showComponentKindShort . componentKind

reportBuildTargetProblems :: [BuildTargetProblem] -> IO ()
reportBuildTargetProblems problems = do

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
          [    "Unknown build target '" ++ showUserBuildTarget target
            ++ "'.\nThere is no "
            ++ intercalate " or " [ mungeThing thing ++ " '" ++ got ++ "'"
                                  | (thing, got) <- nosuch ] ++ "."
          | (target, nosuch) <- targets ]
        where
          mungeThing "file" = "file target"
          mungeThing thing  = thing

    case [ (t, ts) | BuildTargetAmbigious t ts <- problems ] of
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
    showBuildTargetKind (BuildTargetComponent _  ) = "component"
    showBuildTargetKind (BuildTargetModule    _ _) = "module"
    showBuildTargetKind (BuildTargetFile      _ _) = "file"


----------------------------------
-- Top level BuildTarget matcher
--

matchBuildTarget :: PackageDescription
                 -> UserBuildTarget -> Bool -> Match BuildTarget
matchBuildTarget pkg = \utarget fexists ->
    case utarget of
      UserBuildTargetSingle str1 ->
        matchBuildTarget1 cinfo str1 fexists

      UserBuildTargetDouble str1 str2 ->
        matchBuildTarget2 cinfo str1 str2 fexists

      UserBuildTargetTriple str1 str2 str3 ->
        matchBuildTarget3 cinfo str1 str2 str3 fexists
  where
    cinfo = pkgComponentInfo pkg

matchBuildTarget1 :: [ComponentInfo] -> String -> Bool -> Match BuildTarget
matchBuildTarget1 cinfo str1 fexists =
                        matchComponent1 cinfo str1
   `matchPlusShadowing` matchModule1    cinfo str1
   `matchPlusShadowing` matchFile1      cinfo str1 fexists


matchBuildTarget2 :: [ComponentInfo] -> String -> String -> Bool -> Match BuildTarget
matchBuildTarget2 cinfo str1 str2 fexists =
                        matchComponent2 cinfo str1 str2
   `matchPlusShadowing` matchModule2    cinfo str1 str2
   `matchPlusShadowing` matchFile2      cinfo str1 str2 fexists


matchBuildTarget3 :: [ComponentInfo] -> String -> String -> String -> Bool -> Match BuildTarget
matchBuildTarget3 cinfo str1 str2 str3 fexists =
                        matchModule3    cinfo str1 str2 str3
   `matchPlusShadowing` matchFile3      cinfo str1 str2 str3 fexists


data ComponentInfo = ComponentInfo {
       cinfoName    :: ComponentName,
       cinfoStrName :: ComponentStringName,
       cinfoSrcDirs :: [FilePath],
       cinfoModules :: [ModuleName],
       cinfoHsFiles :: [FilePath],   -- other hs files (like main.hs)
       cinfoCFiles  :: [FilePath]
     }

type ComponentStringName = String

pkgComponentInfo :: PackageDescription -> [ComponentInfo]
pkgComponentInfo pkg =
    [ ComponentInfo {
        cinfoName    = componentName c,
        cinfoStrName = componentStringName pkg (componentName c),
        cinfoSrcDirs = hsSourceDirs bi,
        cinfoModules = componentModules c,
        cinfoHsFiles = componentHsFiles c,
        cinfoCFiles  = cSources bi
      }
    | c <- pkgComponents pkg
    , let bi = componentBuildInfo c ]

componentStringName :: Package pkg => pkg -> ComponentName -> ComponentStringName
componentStringName pkg CLibName          = display (packageName pkg)
componentStringName _   (CExeName  name)  = name
componentStringName _   (CTestName  name) = name
componentStringName _   (CBenchName name) = name

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
  | s `elem` ["tst", "test", "test-suite"] = increaseConfidence >> return TestKind
  | s `elem` ["bench", "benchmark"]        = increaseConfidence >> return BenchKind
  | otherwise                              = matchErrorExpected "component kind" s

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
-- Matching component targets
--

matchComponent1 :: [ComponentInfo] -> String -> Match BuildTarget
matchComponent1 cs = \str1 -> do
    guardComponentName str1
    c <- matchComponentName cs str1
    return (BuildTargetComponent (cinfoName c))

matchComponent2 :: [ComponentInfo] -> String -> String -> Match BuildTarget
matchComponent2 cs = \str1 str2 -> do
    ckind <- matchComponentKind str1
    guardComponentName str2
    c <- matchComponentKindAndName cs ckind str2
    return (BuildTargetComponent (cinfoName c))

-- utils:

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
    orNoSuchThing "component" str
  $ increaseConfidenceFor
  $ matchInexactly caseFold
      [ (cinfoStrName c, c) | c <- cs ]
      str

matchComponentKindAndName :: [ComponentInfo] -> ComponentKind -> String -> Match ComponentInfo
matchComponentKindAndName cs ckind str =
    orNoSuchThing (showComponentKind ckind ++ " component") str
  $ increaseConfidenceFor
  $ matchInexactly (\(ck, cn) -> (ck, caseFold cn))
      [ ((cinfoKind c, cinfoStrName c), c) | c <- cs ]
      (ckind, str)


------------------------------
-- Matching module targets
--

matchModule1 :: [ComponentInfo] -> String -> Match BuildTarget
matchModule1 cs = \str1 -> do
    guardModuleName str1
    nubMatchErrors $ do
      c <- tryEach cs
      let ms = cinfoModules c
      m <- matchModuleName ms str1
      return (BuildTargetModule (cinfoName c) m)

matchModule2 :: [ComponentInfo] -> String -> String -> Match BuildTarget
matchModule2 cs = \str1 str2 -> do
    guardComponentName str1
    guardModuleName    str2
    c <- matchComponentName cs str1
    let ms = cinfoModules c
    m <- matchModuleName ms str2
    return (BuildTargetModule (cinfoName c) m)

matchModule3 :: [ComponentInfo] -> String -> String -> String -> Match BuildTarget
matchModule3 cs str1 str2 str3 = do
    ckind <- matchComponentKind str1
    guardComponentName str2
    c <- matchComponentKindAndName cs ckind str2
    guardModuleName    str3
    let ms = cinfoModules c
    m <- matchModuleName ms str3
    return (BuildTargetModule (cinfoName c) m)

-- utils:

guardModuleName :: String -> Match ()
guardModuleName s
  | all validModuleChar s 
    && not (null s)       = increaseConfidence
  | otherwise             = matchErrorExpected "module name" s
  where
    validModuleChar c = isAlphaNum c || c == '.' || c == '_' || c == '\''

matchModuleName :: [ModuleName] -> String -> Match ModuleName
matchModuleName ms str =
    orNoSuchThing "module" str
  $ increaseConfidenceFor
  $ matchInexactly caseFold
      [ (display m, m)
      | m <- ms ]
      str


------------------------------
-- Matching file targets
--

matchFile1 :: [ComponentInfo] -> String -> Bool -> Match BuildTarget
matchFile1 cs str1 exists =
    nubMatchErrors $ do
      c <- tryEach cs
      filepath <- matchComponentFile c str1 exists
      return (BuildTargetFile (cinfoName c) filepath)


matchFile2 :: [ComponentInfo] -> String -> String -> Bool -> Match BuildTarget
matchFile2 cs str1 str2 exists = do
    guardComponentName str1
    c <- matchComponentName cs str1
    filepath <- matchComponentFile c str2 exists
    return (BuildTargetFile (cinfoName c) filepath)


matchFile3 :: [ComponentInfo] -> String -> String -> String -> Bool -> Match BuildTarget
matchFile3 cs str1 str2 str3 exists = do
    ckind <- matchComponentKind str1
    guardComponentName str2
    c <- matchComponentKindAndName cs ckind str2
    filepath <- matchComponentFile c str3 exists
    return (BuildTargetFile (cinfoName c) filepath)


matchComponentFile :: ComponentInfo -> String -> Bool -> Match FilePath
matchComponentFile c str fexists =
    expecting "file" str $
      matchPlus
        (matchFileExists str fexists)
        (matchPlusShadowing
          (msum [ matchModuleFileRooted   dirs ms      str 
                , matchOtherFileRooted    dirs hsFiles str ])
          (msum [ matchModuleFileUnrooted      ms      str
                , matchOtherFileUnrooted       hsFiles str
                , matchOtherFileUnrooted       cFiles  str ]))
  where
    dirs = cinfoSrcDirs c
    ms   = cinfoModules c
    hsFiles = cinfoHsFiles c
    cFiles  = cinfoCFiles c


-- utils

matchFileExists :: FilePath -> Bool -> Match a
matchFileExists _     False = mzero
matchFileExists fname True  = do increaseConfidence
                                 matchErrorNoSuch "file" fname

matchModuleFileUnrooted :: [ModuleName] -> String -> Match FilePath
matchModuleFileUnrooted ms str = do
    let filepath = normalise str
    _ <- matchModuleFileStem ms filepath
    return filepath

matchModuleFileRooted :: [FilePath] -> [ModuleName] -> String -> Match FilePath
matchModuleFileRooted dirs ms str = nubMatches $ do
    let filepath = normalise str
    filepath' <- matchDirectoryPrefix dirs filepath
    _ <- matchModuleFileStem ms filepath'
    return filepath

matchModuleFileStem :: [ModuleName] -> FilePath -> Match ModuleName
matchModuleFileStem ms =
      increaseConfidenceFor
    . matchInexactly caseFold
        [ (toFilePath m, m) | m <- ms ]
    . dropExtension

matchOtherFileRooted :: [FilePath] -> [FilePath] -> FilePath -> Match FilePath
matchOtherFileRooted dirs fs str = do
    let filepath = normalise str
    filepath' <- matchDirectoryPrefix dirs filepath
    _ <- matchFile fs filepath'
    return filepath

matchOtherFileUnrooted :: [FilePath] -> FilePath -> Match FilePath
matchOtherFileUnrooted fs str = do
    let filepath = normalise str
    _ <- matchFile fs filepath
    return filepath

matchFile :: [FilePath] -> FilePath -> Match FilePath
matchFile fs = increaseConfidenceFor
             . matchInexactly caseFold [ (f, f) | f <- fs ]

matchDirectoryPrefix :: [FilePath] -> FilePath -> Match FilePath
matchDirectoryPrefix dirs filepath =
    exactMatches $
      catMaybes
       [ stripDirectory (normalise dir) filepath | dir <- dirs ]
  where
    stripDirectory :: FilePath -> FilePath -> Maybe FilePath
    stripDirectory dir fp =
      joinPath `fmap` stripPrefix (splitDirectories dir) (splitDirectories fp)


------------------------------
-- Matching monad
--

-- | A matcher embodies a way to match some input as being some recognised
-- value. In particular it deals with multiple and ambigious matches.
--
-- There are various matcher primitives ('matchExactly', 'matchInexactly'),
-- ways to combine matchers ('ambigiousWith', 'shadows') and finally we can
-- run a matcher against an input using 'findMatch'.
--

data Match a = NoMatch      Confidence [MatchError]
             | ExactMatch   Confidence [a]
             | InexactMatch Confidence [a]
  deriving Show

type Confidence = Int

data MatchError = MatchErrorExpected String String
                | MatchErrorNoSuch   String String
  deriving (Show, Eq)


instance MonadPlus Match where
  mzero = matchZero
  mplus = matchPlus

matchZero :: Match a
matchZero = NoMatch 0 []

-- | Combine two matchers. Exact matches are used over inexact matches
-- but if we have multiple exact, or inexact then the we collect all the
-- ambigious matches.
--
matchPlus :: Match a -> Match a -> Match a
matchPlus   (ExactMatch   d1 xs)   (ExactMatch   d2 xs') = ExactMatch (max d1 d2) (xs ++ xs')
matchPlus a@(ExactMatch   _  _ )   (InexactMatch _  _  ) = a
matchPlus a@(ExactMatch   _  _ )   (NoMatch      _  _  ) = a
matchPlus   (InexactMatch _  _ ) b@(ExactMatch   _  _  ) = b
matchPlus   (InexactMatch d1 xs)   (InexactMatch d2 xs') = InexactMatch (max d1 d2) (xs ++ xs')
matchPlus a@(InexactMatch _  _ )   (NoMatch      _  _  ) = a
matchPlus   (NoMatch      _  _ ) b@(ExactMatch   _  _  ) = b
matchPlus   (NoMatch      _  _ ) b@(InexactMatch _  _  ) = b
matchPlus a@(NoMatch      d1 ms) b@(NoMatch      d2 ms')
                                             | d1 >  d2  = a
                                             | d1 <  d2  = b
                                             | otherwise = NoMatch d1 (ms ++ ms')

-- | Combine two matchers. This is similar to 'ambigiousWith' with the
-- difference that an exact match from the left matcher shadows any exact
-- match on the right. Inexact matches are still collected however.
--
matchPlusShadowing :: Match a -> Match a -> Match a
matchPlusShadowing a@(ExactMatch _ _) (ExactMatch _ _) = a
matchPlusShadowing a                   b               = matchPlus a b 

instance Functor Match where
  fmap _ (NoMatch      d ms) = NoMatch      d ms
  fmap f (ExactMatch   d xs) = ExactMatch   d (fmap f xs)
  fmap f (InexactMatch d xs) = InexactMatch d (fmap f xs)

instance Applicative Match where
  pure = return
  (<*>) = ap

instance Monad Match where
  return a                = ExactMatch 0 [a]
  NoMatch      d ms >>= _ = NoMatch d ms
  ExactMatch   d xs >>= f = addDepth d
                          $ foldr matchPlus matchZero (map f xs)
  InexactMatch d xs >>= f = addDepth d .  forceInexact
                          $ foldr matchPlus matchZero (map f xs)

addDepth :: Confidence -> Match a -> Match a
addDepth d' (NoMatch      d msgs) = NoMatch      (d'+d) msgs
addDepth d' (ExactMatch   d xs)   = ExactMatch   (d'+d) xs
addDepth d' (InexactMatch d xs)   = InexactMatch (d'+d) xs

forceInexact :: Match a -> Match a
forceInexact (ExactMatch d ys) = InexactMatch d ys
forceInexact m                 = m

------------------------------
-- Various match primitives
--

matchErrorExpected, matchErrorNoSuch :: String -> String -> Match a
matchErrorExpected thing got = NoMatch 0 [MatchErrorExpected thing got]
matchErrorNoSuch   thing got = NoMatch 0 [MatchErrorNoSuch   thing got]

expecting :: String -> String -> Match a -> Match a
expecting thing got (NoMatch 0 _) = matchErrorExpected thing got
expecting _     _   m             = m

orNoSuchThing :: String -> String -> Match a -> Match a
orNoSuchThing thing got (NoMatch 0 _) = matchErrorNoSuch thing got
orNoSuchThing _     _   m             = m

increaseConfidence :: Match ()
increaseConfidence = ExactMatch 1 [()]

increaseConfidenceFor :: Match a -> Match a
increaseConfidenceFor m = m >>= \r -> increaseConfidence >> return r

nubMatches :: Eq a => Match a -> Match a
nubMatches (NoMatch      d msgs) = NoMatch      d msgs
nubMatches (ExactMatch   d xs)   = ExactMatch   d (nub xs)
nubMatches (InexactMatch d xs)   = InexactMatch d (nub xs)

nubMatchErrors :: Match a -> Match a
nubMatchErrors (NoMatch      d msgs) = NoMatch      d (nub msgs)
nubMatchErrors (ExactMatch   d xs)   = ExactMatch   d xs
nubMatchErrors (InexactMatch d xs)   = InexactMatch d xs

-- | Lift a list of matches to an exact match.
--
exactMatches, inexactMatches :: [a] -> Match a

exactMatches [] = matchZero
exactMatches xs = ExactMatch 0 xs

inexactMatches [] = matchZero
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
findMatch :: Eq b => Match b -> MaybeAmbigious b
findMatch match =
    case match of
      NoMatch    _ msgs -> None (nub msgs)
      ExactMatch   _ xs -> checkAmbigious xs
      InexactMatch _ xs -> checkAmbigious xs
  where
    checkAmbigious xs = case nub xs of
                          [x] -> Unambiguous x
                          xs' -> Ambiguous   xs'

data MaybeAmbigious a = None [MatchError] | Unambiguous a | Ambiguous [a]
  deriving Show


------------------------------
-- Basic matchers
--

{-
-- | A primitive matcher that looks up a value in a finite 'Map'. The
-- value must match exactly.
--
matchExactly :: forall a b. Ord a => [(a, b)] -> (a -> Match b)
matchExactly xs =
    \x -> case Map.lookup x m of
            Nothing -> matchZero
            Just ys -> ExactMatch 0 ys
  where
    m :: Ord a => Map a [b]
    m = Map.fromListWith (++) [ (k,[x]) | (k,x) <- xs ]
-}

-- | A primitive matcher that looks up a value in a finite 'Map'. It checks
-- for an exact or inexact match. We get an inexact match if the match
-- is not exact, but the canonical forms match. It takes a canonicalisation
-- function for this purpose.
--
-- So for example if we used string case fold as the canonicalisation
-- function, then we would get case insensitive matching (but it will still
-- report an exact match when the case matches too).
--
matchInexactly :: (Ord a, Ord a') =>
                        (a -> a') ->
                        [(a, b)] -> (a -> Match b)
matchInexactly cannonicalise xs =
    \x -> case Map.lookup x m of
            Just ys -> exactMatches ys
            Nothing -> case Map.lookup (cannonicalise x) m' of
                         Just ys -> inexactMatches ys
                         Nothing -> matchZero
  where
    m = Map.fromListWith (++) [ (k,[x]) | (k,x) <- xs ]

    -- the map of canonicalised keys to groups of inexact matches
    m' = Map.mapKeysWith (++) cannonicalise m



------------------------------
-- Utils
--

caseFold :: String -> String
caseFold = lowercase

