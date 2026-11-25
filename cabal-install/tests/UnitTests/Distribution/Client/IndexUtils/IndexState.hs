module UnitTests.Distribution.Client.IndexUtils.IndexState (tests) where

import Data.Coerce (coerce)
import Data.List (intercalate)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)

import Distribution.Client.IndexUtils.IndexState
import Distribution.Client.Types.RepoName (RepoName (..))

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup
      "an unscoped timestamp is for all repositories"
      [ testCase "a timestamp" $ do
          let value = unscoped may
          lookupIn value hackage @?= Just may
          lookupIn value headHackage @?= Just may
      , testCase "HEAD" $ do
          let value = unscoped IndexStateHead
          lookupIn value hackage @?= Just IndexStateHead
          lookupIn value headHackage @?= Just IndexStateHead
      ]
  , testGroup
      "a scoped timestamp is for the repository named before it"
      [ testCase "a repository not named is read at HEAD" $ do
          let value = scoped hackage may
          lookupIn value hackage @?= Just may
          lookupIn value headHackage @?= Just IndexStateHead
      , testCase "each repository has its own timestamp" $ do
          let value = commas [scoped hackage may, scoped headHackage april]
          lookupIn value hackage @?= Just may
          lookupIn value headHackage @?= Just april
      , testCase "a repository can be named head in lower case" $ do
          let value = scoped lowerHead may
          lookupIn value lowerHead @?= Just may
          lookupIn value hackage @?= Just IndexStateHead
          parse (scoped lowerHead IndexStateHead) @?= parse (unscoped IndexStateHead)
      , testCase "a leading comma is allowed" $
          parse (commas ["", scoped hackage may, scoped headHackage april])
            @?= parse (commas [scoped hackage may, scoped headHackage april])
      ]
  , testGroup
      "an unscoped timestamp followed by scoped ones"
      [ testCase "a repository not named gets the unscoped timestamp" $ do
          let value = commas [unscoped may, scoped headHackage IndexStateHead]
          lookupIn value hackage @?= Just may
          lookupIn value headHackage @?= Just IndexStateHead
      ]
  , testGroup
      "a scoped timestamp is lost if an unscoped one comes after it"
      [ testCase "an unscoped timestamp" $
          parse (commas [scoped headHackage IndexStateHead, unscoped may])
            @?= parse (unscoped may)
      , testCase "an unscoped HEAD" $
          parse (commas [scoped hackage may, unscoped IndexStateHead])
            @?= parse (unscoped IndexStateHead)
      , testCase "scoped ones after it are kept" $
          parse (commas [scoped headHackage april, unscoped may, scoped headHackage IndexStateHead])
            @?= parse (commas [unscoped may, scoped headHackage IndexStateHead])
      ]
  , testGroup
      "rejected"
      [ testCase "a space in place of the comma" $
          parse (unwords [unscoped may, scoped headHackage IndexStateHead]) @?= Nothing
      , testCase "a space between scoped timestamps" $
          parse (unwords [scoped hackage may, scoped headHackage IndexStateHead]) @?= Nothing
      , testCase "head in lower case" $
          parse (coerce lowerHead) @?= Nothing
      , testCase "a repository name with no timestamp" $
          parse (coerce hackage) @?= Nothing
      , testCase "a repository named HEAD" $ do
          parse (scoped upperHead may) @?= Nothing
          parse (scoped upperHead IndexStateHead) @?= Nothing
      ]
  ]

parse :: String -> Maybe TotalIndexState
parse = simpleParsec

-- | The index state a repository gets from an @index-state@ value.
lookupIn :: String -> RepoName -> Maybe RepoIndexState
lookupIn value repo = lookupIndexState repo <$> parse value

-- | A timestamp or @HEAD@ as written with no repository name before it.
unscoped :: RepoIndexState -> String
unscoped = prettyShow

-- | A timestamp or @HEAD@ as written with a repository name before it.
scoped :: RepoName -> RepoIndexState -> String
scoped repo state = unwords [coerce repo, prettyShow state]

commas :: [String] -> String
commas = intercalate ", "

hackage, headHackage, lowerHead, upperHead :: RepoName
hackage = RepoName "hackage.haskell.org"
headHackage = RepoName "head.hackage"
lowerHead = RepoName "head"
upperHead = RepoName "HEAD"

may, april :: RepoIndexState
may = timestamp "2020-05-06T22:33:27Z"
april = timestamp "2020-04-29T04:11:05Z"

timestamp :: String -> RepoIndexState
timestamp s = maybe (error ("not a timestamp: " ++ s)) IndexStateTime (simpleParsec s)
