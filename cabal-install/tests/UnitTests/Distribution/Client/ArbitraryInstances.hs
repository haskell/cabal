{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.Distribution.Client.ArbitraryInstances
  ( adjustSize
  , shortListOf
  , shortListOf1
  , arbitraryFlag
  , ShortToken (..)
  , arbitraryShortToken
  , NonMEmpty (..)
  , NoShrink (..)

    -- * Shrinker
  , Shrinker
  , runShrinker
  , shrinker
  , shrinkerPP
  , shrinkerAla
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Data.Char (isLetter)
import Data.List ((\\))

import Distribution.Simple.Setup
import Distribution.Types.Flag (mkFlagAssignment)

import Distribution.Client.BuildReports.Types (BuildReport, InstallOutcome, Outcome, ReportLevel (..))
import Distribution.Client.CmdInstall.ClientInstallFlags (InstallMethod)
import Distribution.Client.Glob (FilePathGlob (..), FilePathGlobRel (..), FilePathRoot (..), GlobPiece (..))
import Distribution.Client.IndexUtils.ActiveRepos (ActiveRepoEntry (..), ActiveRepos (..), CombineStrategy (..))
import Distribution.Client.IndexUtils.IndexState (RepoIndexState (..), TotalIndexState, makeTotalIndexState)
import Distribution.Client.IndexUtils.Timestamp (Timestamp, epochTimeToTimestamp)
import Distribution.Client.Targets
import Distribution.Client.Types (RepoName (..), WriteGhcEnvironmentFilesPolicy)
import Distribution.Client.Types.AllowNewer
import Distribution.Client.Types.OverwritePolicy (OverwritePolicy)
import Distribution.Solver.Types.OptionalStanza (OptionalStanza (..), OptionalStanzaMap, OptionalStanzaSet, optStanzaSetFromList, optStanzaTabulate)
import Distribution.Solver.Types.PackageConstraint (PackageProperty (..))

import Data.Coerce (Coercible, coerce)
import Network.URI (URI (..), URIAuth (..), isUnreserved)
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , NonEmptyList (..)
  , arbitraryBoundedEnum
  , choose
  , elements
  , frequency
  , genericShrink
  , liftArbitrary
  , listOf
  , oneof
  , resize
  , shrinkBoundedEnum
  , sized
  , suchThat
  , vectorOf
  )
import Test.QuickCheck.GenericArbitrary (genericArbitrary)
import Test.QuickCheck.Instances.Cabal ()

-- note: there are plenty of instances defined in ProjectConfig test file.
-- they should be moved here or into Cabal-quickcheck

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

data Shrinker a = Shrinker a [a]

instance Functor Shrinker where
  fmap f (Shrinker x xs) = Shrinker (f x) (map f xs)

instance Applicative Shrinker where
  pure x = Shrinker x []

  Shrinker f fs <*> Shrinker x xs = Shrinker (f x) (map f xs ++ map ($ x) fs)

runShrinker :: Shrinker a -> [a]
runShrinker (Shrinker _ xs) = xs

shrinker :: Arbitrary a => a -> Shrinker a
shrinker x = Shrinker x (shrink x)

shrinkerAla :: (Coercible a b, Arbitrary b) => (a -> b) -> a -> Shrinker a
shrinkerAla pack = shrinkerPP pack coerce

-- | shrinker with pre and post functions.
shrinkerPP :: Arbitrary b => (a -> b) -> (b -> a) -> a -> Shrinker a
shrinkerPP pack unpack x = Shrinker x (map unpack (shrink (pack x)))

-------------------------------------------------------------------------------
-- Non-Cabal instances
-------------------------------------------------------------------------------

instance Arbitrary URI where
  arbitrary =
    URI
      <$> elements ["file:", "http:", "https:"]
      <*> (Just <$> arbitrary)
      <*> (('/' :) <$> arbitraryURIToken)
      <*> (('?' :) <$> arbitraryURIToken)
      <*> pure ""

instance Arbitrary URIAuth where
  arbitrary =
    URIAuth
      <$> pure "" -- no password as this does not roundtrip
      <*> arbitraryURIToken
      <*> arbitraryURIPort

arbitraryURIToken :: Gen String
arbitraryURIToken =
  shortListOf1 6 (elements (filter isUnreserved ['\0' .. '\255']))

arbitraryURIPort :: Gen String
arbitraryURIPort =
  oneof [pure "", (':' :) <$> shortListOf1 4 (choose ('0', '9'))]

-------------------------------------------------------------------------------
-- cabal-install (and Cabal) types
-------------------------------------------------------------------------------

adjustSize :: (Int -> Int) -> Gen a -> Gen a
adjustSize adjust gen = sized (\n -> resize (adjust n) gen)

shortListOf :: Int -> Gen a -> Gen [a]
shortListOf bound gen =
  sized $ \n -> do
    k <- choose (0, (n `div` 2) `min` bound)
    vectorOf k gen

shortListOf1 :: Int -> Gen a -> Gen [a]
shortListOf1 bound gen =
  sized $ \n -> do
    k <- choose (1, 1 `max` ((n `div` 2) `min` bound))
    vectorOf k gen

newtype ShortToken = ShortToken {getShortToken :: String}
  deriving (Show)

instance Arbitrary ShortToken where
  arbitrary =
    ShortToken
      <$> ( shortListOf1 5 (choose ('#', '~'))
              `suchThat` (all (`notElem` "{}"))
              `suchThat` (not . ("[]" `isPrefixOf`))
          )

  -- TODO: [code cleanup] need to replace parseHaskellString impl to stop
  -- accepting Haskell list syntax [], ['a'] etc, just allow String syntax.
  -- Workaround, don't generate [] as this does not round trip.

  shrink (ShortToken cs) =
    [ShortToken cs' | cs' <- shrink cs, not (null cs')]

arbitraryShortToken :: Gen String
arbitraryShortToken = getShortToken <$> arbitrary

newtype NonMEmpty a = NonMEmpty {getNonMEmpty :: a}
  deriving (Eq, Ord, Show)

instance (Arbitrary a, Monoid a, Eq a) => Arbitrary (NonMEmpty a) where
  arbitrary = NonMEmpty <$> (arbitrary `suchThat` (/= mempty))
  shrink (NonMEmpty x) = [NonMEmpty x' | x' <- shrink x, x' /= mempty]

newtype NoShrink a = NoShrink {getNoShrink :: a}
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (NoShrink a) where
  arbitrary = NoShrink <$> arbitrary
  shrink _ = []

instance Arbitrary Timestamp where
  -- note: no negative timestamps
  --
  -- >>> utcTimeToPOSIXSeconds $ UTCTime (fromGregorian 100000 01 01) 0
  -- >>> 3093527980800s
  --
  arbitrary = maybe (toEnum 0) id . epochTimeToTimestamp . (`mod` 3093527980800) . abs <$> arbitrary

instance Arbitrary RepoIndexState where
  arbitrary =
    frequency
      [ (1, pure IndexStateHead)
      , (50, IndexStateTime <$> arbitrary)
      ]

instance Arbitrary TotalIndexState where
  arbitrary = makeTotalIndexState <$> arbitrary <*> arbitrary

instance Arbitrary WriteGhcEnvironmentFilesPolicy where
  arbitrary = arbitraryBoundedEnum

arbitraryFlag :: Gen a -> Gen (Flag a)
arbitraryFlag = liftArbitrary

instance Arbitrary RepoName where
  -- TODO: rename refinement?
  arbitrary = RepoName <$> (mk `suchThat` \x -> not $ "--" `isPrefixOf` x)
    where
      mk = (:) <$> lead <*> rest
      lead =
        elements
          [c | c <- ['\NUL' .. '\255'], isAlpha c || c `elem` "_-."]
      rest =
        listOf
          ( elements
              [c | c <- ['\NUL' .. '\255'], isAlphaNum c || c `elem` "_-."]
          )

instance Arbitrary ReportLevel where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OverwritePolicy where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary InstallMethod where
  arbitrary = arbitraryBoundedEnum

-------------------------------------------------------------------------------
-- ActiveRepos
-------------------------------------------------------------------------------

instance Arbitrary ActiveRepos where
  arbitrary = ActiveRepos <$> shortListOf 5 arbitrary

instance Arbitrary ActiveRepoEntry where
  arbitrary =
    frequency
      [ (10, ActiveRepo <$> arbitrary <*> arbitrary)
      , (1, ActiveRepoRest <$> arbitrary)
      ]

instance Arbitrary CombineStrategy where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

-------------------------------------------------------------------------------
-- AllowNewer
-------------------------------------------------------------------------------

instance Arbitrary AllowNewer where
  arbitrary = AllowNewer <$> arbitrary

instance Arbitrary AllowOlder where
  arbitrary = AllowOlder <$> arbitrary

instance Arbitrary RelaxDeps where
  arbitrary =
    oneof
      [ pure mempty
      , mkRelaxDepSome <$> shortListOf1 3 arbitrary
      , pure RelaxDepsAll
      ]

instance Arbitrary RelaxDepMod where
  arbitrary = elements [RelaxDepModNone, RelaxDepModCaret]

  shrink RelaxDepModCaret = [RelaxDepModNone]
  shrink _ = []

instance Arbitrary RelaxDepScope where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary RelaxDepSubject where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary RelaxedDep where
  arbitrary = genericArbitrary
  shrink = genericShrink

-------------------------------------------------------------------------------
-- UserConstraint
-------------------------------------------------------------------------------

instance Arbitrary UserConstraintScope where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary UserQualifier where
  arbitrary =
    oneof
      [ pure UserQualToplevel
      , UserQualSetup <$> arbitrary
      -- -- TODO: Re-enable UserQualExe tests once we decide on a syntax.
      -- , UserQualExe <$> arbitrary <*> arbitrary
      ]

instance Arbitrary UserConstraint where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary PackageProperty where
  arbitrary =
    oneof
      [ PackagePropertyVersion <$> arbitrary
      , pure PackagePropertyInstalled
      , pure PackagePropertySource
      , PackagePropertyFlags . mkFlagAssignment <$> shortListOf1 3 arbitrary
      , PackagePropertyStanzas . (\x -> [x]) <$> arbitrary
      ]

instance Arbitrary OptionalStanza where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary OptionalStanzaSet where
  arbitrary = fmap optStanzaSetFromList arbitrary

instance Arbitrary a => Arbitrary (OptionalStanzaMap a) where
  arbitrary = do
    x1 <- arbitrary
    x2 <- arbitrary
    return $ optStanzaTabulate $ \x -> case x of
      TestStanzas -> x1
      BenchStanzas -> x2

-------------------------------------------------------------------------------
-- BuildReport
-------------------------------------------------------------------------------

instance Arbitrary BuildReport where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary InstallOutcome where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Outcome where
  arbitrary = genericArbitrary
  shrink = genericShrink

-------------------------------------------------------------------------------
-- Glob
-------------------------------------------------------------------------------

instance Arbitrary FilePathGlob where
  arbitrary =
    (FilePathGlob <$> arbitrary <*> arbitrary)
      `suchThat` validFilePathGlob

  shrink (FilePathGlob root pathglob) =
    [ FilePathGlob root' pathglob'
    | (root', pathglob') <- shrink (root, pathglob)
    , validFilePathGlob (FilePathGlob root' pathglob')
    ]

validFilePathGlob :: FilePathGlob -> Bool
validFilePathGlob (FilePathGlob FilePathRelative pathglob) =
  case pathglob of
    GlobDirTrailing -> False
    GlobDir [Literal "~"] _ -> False
    GlobDir [Literal (d : ":")] _
      | isLetter d -> False
    _ -> True
validFilePathGlob _ = True

instance Arbitrary FilePathRoot where
  arbitrary =
    frequency
      [ (3, pure FilePathRelative)
      , (1, pure (FilePathRoot unixroot))
      , (1, FilePathRoot <$> windrive)
      , (1, pure FilePathHomeDir)
      ]
    where
      unixroot = "/"
      windrive = do d <- choose ('A', 'Z'); return (d : ":\\")

  shrink FilePathRelative = []
  shrink (FilePathRoot _) = [FilePathRelative]
  shrink FilePathHomeDir = [FilePathRelative]

instance Arbitrary FilePathGlobRel where
  arbitrary = sized $ \sz ->
    oneof $
      take
        (max 1 sz)
        [ pure GlobDirTrailing
        , GlobFile <$> (getGlobPieces <$> arbitrary)
        , GlobDir
            <$> (getGlobPieces <$> arbitrary)
            <*> resize (sz `div` 2) arbitrary
        ]

  shrink GlobDirTrailing = []
  shrink (GlobFile glob) =
    GlobDirTrailing
      : [GlobFile (getGlobPieces glob') | glob' <- shrink (GlobPieces glob)]
  shrink (GlobDir glob pathglob) =
    pathglob
      : GlobFile glob
      : [ GlobDir (getGlobPieces glob') pathglob'
        | (glob', pathglob') <- shrink (GlobPieces glob, pathglob)
        ]

newtype GlobPieces = GlobPieces {getGlobPieces :: [GlobPiece]}
  deriving (Eq)

instance Arbitrary GlobPieces where
  arbitrary = GlobPieces . mergeLiterals <$> shortListOf1 5 arbitrary

  shrink (GlobPieces glob) =
    [ GlobPieces (mergeLiterals (getNonEmpty glob'))
    | glob' <- shrink (NonEmpty glob)
    ]

mergeLiterals :: [GlobPiece] -> [GlobPiece]
mergeLiterals (Literal a : Literal b : ps) = mergeLiterals (Literal (a ++ b) : ps)
mergeLiterals (Union as : ps) = Union (map mergeLiterals as) : mergeLiterals ps
mergeLiterals (p : ps) = p : mergeLiterals ps
mergeLiterals [] = []

instance Arbitrary GlobPiece where
  arbitrary = sized $ \sz ->
    frequency
      [ (3, Literal <$> shortListOf1 10 (elements globLiteralChars))
      , (1, pure WildCard)
      , (1, Union <$> resize (sz `div` 2) (shortListOf1 5 (shortListOf1 5 arbitrary)))
      ]

  shrink (Literal str) =
    [ Literal str'
    | str' <- shrink str
    , not (null str')
    , all (`elem` globLiteralChars) str'
    ]
  shrink WildCard = []
  shrink (Union as) =
    [ Union (map getGlobPieces (getNonEmpty as'))
    | as' <- shrink (NonEmpty (map GlobPieces as))
    ]

globLiteralChars :: [Char]
globLiteralChars = ['\0' .. '\128'] \\ "*{},/\\"
