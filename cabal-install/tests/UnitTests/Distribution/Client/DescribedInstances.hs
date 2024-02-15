{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.Distribution.Client.DescribedInstances where

import Distribution.Client.Compat.Prelude

import Data.List ((\\))
import Distribution.Described

import Distribution.Types.PackageId (PackageIdentifier)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.VersionRange (VersionRange)

import Distribution.Client.BuildReports.Types (InstallOutcome, Outcome)
import Distribution.Client.Glob (RootedGlob)
import Distribution.Client.IndexUtils.ActiveRepos (ActiveRepoEntry, ActiveRepos, CombineStrategy)
import Distribution.Client.IndexUtils.IndexState (RepoIndexState, TotalIndexState)
import Distribution.Client.IndexUtils.Timestamp (Timestamp)
import Distribution.Client.Targets (UserConstraint)
import Distribution.Client.Types (RepoName)
import Distribution.Client.Types.AllowNewer (RelaxDepSubject, RelaxDeps, RelaxedDep)

-------------------------------------------------------------------------------
-- BuildReport
-------------------------------------------------------------------------------

instance Described InstallOutcome where
  describe _ =
    REUnion
      [ "PlanningFailed"
      , "DependencyFailed" <> RESpaces1 <> describe (Proxy :: Proxy PackageIdentifier)
      , "DownloadFailed"
      , "UnpackFailed"
      , "SetupFailed"
      , "ConfigureFailed"
      , "BuildFailed"
      , "TestsFailed"
      , "InstallFailed"
      , "InstallOk"
      ]
instance Described Outcome where
  describe _ =
    REUnion
      [ fromString (prettyShow o)
      | o <- [minBound .. maxBound :: Outcome]
      ]

-------------------------------------------------------------------------------
-- Glob
-------------------------------------------------------------------------------

-- This instance is incorrect as it may generate C:\dir\{foo,bar}
instance Described RootedGlob where
  describe _ = REUnion [root, relative, homedir]
    where
      root =
        REUnion
          [ fromString "/"
          , reChars (['a' .. 'z'] ++ ['A' .. 'Z']) <> ":" <> reChars "/\\"
          ]
          <> REOpt pieces
      homedir = "~/" <> REOpt pieces
      relative = pieces

      pieces :: GrammarRegex void
      pieces = REMunch1 sep piece <> REOpt "/"

      piece :: GrammarRegex void
      piece =
        RERec "glob" $
          REMunch1 mempty $
            REUnion
              [ normal
              , escape
              , wildcard
              , "{" <> REMunch1 "," (REVar Nothing) <> "}"
              ]

      sep :: GrammarRegex void
      sep = reChars "/\\"

      wildcard :: GrammarRegex void
      wildcard = "*"

      normal = reChars $ ['\0' .. '\128'] \\ "*{},/\\"
      escape = fromString "\\" <> reChars "*{},"

-------------------------------------------------------------------------------
-- AllowNewer
-------------------------------------------------------------------------------

instance Described RelaxedDep where
  describe _ =
    REOpt (describeRelaxDepScope <> ":" <> REOpt ("^"))
      <> describe (Proxy :: Proxy RelaxDepSubject)
    where
      describeRelaxDepScope =
        REUnion
          [ "*"
          , "all"
          , RENamed "package-name" (describe (Proxy :: Proxy PackageName))
          , RENamed "package-id" (describe (Proxy :: Proxy PackageIdentifier))
          ]

instance Described RelaxDepSubject where
  describe _ =
    REUnion
      [ "*"
      , "all"
      , RENamed "package-name" (describe (Proxy :: Proxy PackageName))
      ]

instance Described RelaxDeps where
  describe _ =
    REUnion
      [ "*"
      , "all"
      , "none"
      , RECommaNonEmpty (describe (Proxy :: Proxy RelaxedDep))
      ]

-------------------------------------------------------------------------------
-- ActiveRepos
-------------------------------------------------------------------------------

instance Described ActiveRepos where
  describe _ =
    REUnion
      [ ":none"
      , RECommaNonEmpty (describe (Proxy :: Proxy ActiveRepoEntry))
      ]

instance Described ActiveRepoEntry where
  describe _ =
    REUnion
      [ ":rest" <> strategy
      , REOpt ":repo:" <> describe (Proxy :: Proxy RepoName) <> strategy
      ]
    where
      strategy = REOpt $ ":" <> describe (Proxy :: Proxy CombineStrategy)

instance Described CombineStrategy where
  describe _ =
    REUnion
      [ "skip"
      , "merge"
      , "override"
      ]

-------------------------------------------------------------------------------
-- UserConstraint
-------------------------------------------------------------------------------

instance Described UserConstraint where
  describe _ =
    REAppend
      [ describeConstraintScope
      , describeConstraintProperty
      ]
    where
      describeConstraintScope :: GrammarRegex void
      describeConstraintScope =
        REUnion
          [ "any." <> describePN
          , "setup." <> describePN
          , describePN
          , describePN <> ":setup." <> describePN
          ]

      describeConstraintProperty :: GrammarRegex void
      describeConstraintProperty =
        REUnion
          [ RESpaces <> RENamed "version-range" (describe (Proxy :: Proxy VersionRange))
          , RESpaces1 <> describeConstraintProperty'
          ]

      describeConstraintProperty' :: GrammarRegex void
      describeConstraintProperty' =
        REUnion
          [ "installed"
          , "source"
          , "test"
          , "bench"
          , describeFlagAssignmentNonEmpty
          ]

      describePN :: GrammarRegex void
      describePN = RENamed "package-name" (describe (Proxy :: Proxy PackageName))

-------------------------------------------------------------------------------
-- IndexState
-------------------------------------------------------------------------------

instance Described TotalIndexState where
  describe _ =
    reCommaNonEmpty $
      REUnion
        [ describe (Proxy :: Proxy RepoName) <> RESpaces1 <> ris
        , ris
        ]
    where
      ris = describe (Proxy :: Proxy RepoIndexState)

instance Described RepoName where
  describe _ = lead <> rest
    where
      lead = RECharSet $ csAlpha <> "_-."
      rest = reMunchCS $ csAlphaNum <> "_-."

instance Described RepoIndexState where
  describe _ =
    REUnion
      [ "HEAD"
      , RENamed "timestamp" (describe (Proxy :: Proxy Timestamp))
      ]

instance Described Timestamp where
  describe _ =
    REUnion
      [ posix
      , utc
      ]
    where
      posix = reChar '@' <> reMunch1CS "0123456789"
      utc = RENamed "date" date <> reChar 'T' <> RENamed "time" time <> reChar 'Z'

      date =
        REOpt digit
          <> REUnion
            [ leapYear <> reChar '-' <> leapMD
            , commonYear <> reChar '-' <> commonMD
            ]

      -- leap year: either
      -- \* divisible by 400
      -- \* not divisible by 100 and divisible by 4
      leapYear =
        REUnion
          [ div4 <> "00"
          , digit <> digit <> div4not0
          ]

      -- common year: either
      -- \* not divisible by 400 but divisible by 100
      -- \* not divisible by 4
      commonYear =
        REUnion
          [ notDiv4 <> "00"
          , digit <> digit <> notDiv4
          ]

      div4 =
        REUnion
          [ "0" <> reChars "048"
          , "1" <> reChars "26"
          , "2" <> reChars "048"
          , "3" <> reChars "26"
          , "4" <> reChars "048"
          , "5" <> reChars "26"
          , "6" <> reChars "048"
          , "7" <> reChars "26"
          , "8" <> reChars "048"
          , "9" <> reChars "26"
          ]

      div4not0 =
        REUnion
          [ "0" <> reChars "48" -- no zero
          , "1" <> reChars "26"
          , "2" <> reChars "048"
          , "3" <> reChars "26"
          , "4" <> reChars "048"
          , "5" <> reChars "26"
          , "6" <> reChars "048"
          , "7" <> reChars "26"
          , "8" <> reChars "048"
          , "9" <> reChars "26"
          ]

      notDiv4 =
        REUnion
          [ "0" <> reChars "1235679"
          , "1" <> reChars "01345789"
          , "2" <> reChars "1235679"
          , "3" <> reChars "01345789"
          , "4" <> reChars "1235679"
          , "5" <> reChars "01345789"
          , "6" <> reChars "1235679"
          , "7" <> reChars "01345789"
          , "8" <> reChars "1235679"
          , "9" <> reChars "01345789"
          ]

      leapMD =
        REUnion
          [jan, fe', mar, apr, may, jun, jul, aug, sep, oct, nov, dec]

      commonMD =
        REUnion
          [jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec]

      jan = "01-" <> d31
      feb = "02-" <> d28
      fe' = "02-" <> d29
      mar = "03-" <> d31
      apr = "04-" <> d30
      may = "05-" <> d31
      jun = "06-" <> d30
      jul = "07-" <> d31
      aug = "08-" <> d31
      sep = "09-" <> d30
      oct = "10-" <> d31
      nov = "11-" <> d30
      dec = "12-" <> d31

      d28 =
        REUnion
          ["0" <> digit1, "1" <> digit, "2" <> reChars "012345678"]
      d29 =
        REUnion
          ["0" <> digit1, "1" <> digit, "2" <> digit]
      d30 =
        REUnion
          ["0" <> digit1, "1" <> digit, "2" <> digit, "30"]
      d31 =
        REUnion
          ["0" <> digit1, "1" <> digit, "2" <> digit, "30", "31"]

      time = ho <> reChar ':' <> minSec <> reChar ':' <> minSec

      -- 0..23
      ho =
        REUnion
          [ "0" <> digit
          , "1" <> digit
          , "2" <> reChars "0123"
          ]

      -- 0..59
      minSec = reChars "012345" <> digit

      digit = reChars "0123456789"
      digit1 = reChars "123456789"
