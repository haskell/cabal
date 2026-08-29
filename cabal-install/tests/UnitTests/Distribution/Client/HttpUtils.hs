module UnitTests.Distribution.Client.HttpUtils
  ( tests
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.HttpUtils
  ( HttpTransport (..)
  , isOldHackageURI
  , remoteRepoTryUpgradeToHttps
  )
import Distribution.Client.Types (RemoteRepo (..), RepoName (..))
import Distribution.Verbosity (Verbosity (..), defaultVerbosityHandles, normal)
import Network.URI (URI, nullURI, parseURI)

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup
      "isOldHackageURI"
      [ testCase "old hackage archive URI" $
          case parseURI "http://hackage.haskell.org/packages/archive" of
            Just uri -> assertBool "should be old hackage URI" (isOldHackageURI uri)
            Nothing -> assertFailure "failed to parse URI"
      , testCase "modern hackage package URI" $
          case parseURI "http://hackage.haskell.org/package/foo" of
            Just uri -> assertBool "should not be old hackage URI" (not (isOldHackageURI uri))
            Nothing -> assertFailure "failed to parse URI"
      , testCase "non-hackage URI" $
          case parseURI "http://example.com/packages/archive" of
            Just uri -> assertBool "should not be old hackage URI" (not (isOldHackageURI uri))
            Nothing -> assertFailure "failed to parse URI"
      ]
  , testGroup
      "remoteRepoTryUpgradeToHttps"
      [ testCase "upgrade http to https when supported" $ do
          let dummyUri = fromMaybe nullURI (parseURI "http://hackage.haskell.org/")
              repo = emptyTestRemoteRepo (RepoName "hackage") dummyUri
              dummyTransport =
                HttpTransport
                  { getHttp = \_ _ _ _ _ -> error "unused"
                  , postHttp = \_ _ _ _ -> error "unused"
                  , postHttpFile = \_ _ _ _ -> error "unused"
                  , putHttpFile = \_ _ _ _ _ -> error "unused"
                  , transportSupportsHttps = True
                  , transportManuallySelected = False
                  }
              normalVerbosity = Verbosity normal defaultVerbosityHandles
          upgraded <- remoteRepoTryUpgradeToHttps normalVerbosity dummyTransport repo
          show (remoteRepoURI upgraded) @?= "https://hackage.haskell.org/"
      ]
  ]

emptyTestRemoteRepo :: RepoName -> URI -> RemoteRepo
emptyTestRemoteRepo name uri =
  RemoteRepo
    { remoteRepoName = name
    , remoteRepoURI = uri
    , remoteRepoSecure = Nothing
    , remoteRepoRootKeys = []
    , remoteRepoKeyThreshold = 0
    , remoteRepoShouldTryHttps = True
    }
