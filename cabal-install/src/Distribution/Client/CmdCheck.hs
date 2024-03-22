module Distribution.Client.CmdCheck
  ( checkCommand
  , checkAction
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.Check (multiCheck)

{-
import Distribution.Client.CmdErrorMessages
        -- xxx vedi questo!
  ( plural
  , renderListCommaAnd
  , renderTargetProblem
  , renderTargetProblemNoTargets
  , renderTargetSelector
  , showTargetSelector
  , targetSelectorFilter
  , targetSelectorPluralPkgs
  )
-}
import Distribution.Client.ProjectOrchestration
import Distribution.Client.Setup (GlobalFlags (..))
import Distribution.Client.Types.PackageLocation
  ( PackageLocation (..)
  , UnresolvedSourcePackage
  )
import Distribution.Client.Types.PackageSpecifier (PackageSpecifier (..))
import Distribution.PackageDescription.Check (CheckExplanationIDString)
import Distribution.Simple.Command (CommandUI (..), OptionField (..), ShowOrParseArgs, option, reqArg')
import Distribution.Simple.Setup (Flag (..), fromFlag, optionVerbosity)
import Distribution.Simple.Utils (wrapText)
import Distribution.Solver.Types.SourcePackage (srcpkgPackageId, srcpkgSource)
import Distribution.Types.PackageId
import Distribution.Verbosity (normal)

data CheckFlags = CheckFlags
  { checkVerbosity :: Flag Verbosity
  , checkIgnore :: [CheckExplanationIDString]
  }
  deriving (Show, Typeable)

defaultCheckFlags :: CheckFlags
defaultCheckFlags =
  CheckFlags
    { checkVerbosity = Flag normal
    , checkIgnore = []
    }

checkOptions' :: ShowOrParseArgs -> [OptionField CheckFlags]
checkOptions' _showOrParseArgs =
  [ optionVerbosity
      checkVerbosity
      (\v flags -> flags{checkVerbosity = v})
  , option
      ['i']
      ["ignore"]
      "ignore a specific warning (e.g. --ignore=missing-upper-bounds)"
      checkIgnore
      (\v c -> c{checkIgnore = v ++ checkIgnore c})
      (reqArg' "WARNING" (: []) (const []))
  ]

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

checkCommand :: CommandUI CheckFlags
checkCommand =
  CommandUI
    { commandName = "check"
    , commandSynopsis = "Check the package for common mistakes."
    , commandDescription = Just $ \_ ->
        wrapText $
          "If no targets are passed, expects a .cabal package file in the "
            ++ "current directory.\n"
            ++ "\n"
            ++ "Some checks correspond to the requirements to packages on Hackage. "
            ++ "If no `Error` is reported, Hackage should accept the "
            ++ "package. If errors are present, `check` exits with 1 and Hackage "
            ++ "will refuse the package.\n"
    , commandNotes = Nothing
    , commandUsage = usageFlags "check"
    , commandDefaultFlags = defaultCheckFlags
    , commandOptions = checkOptions'
    }

usageFlags :: String -> String -> String
usageFlags name pname =
  "Usage: " ++ pname ++ " " ++ name ++ " [FLAGS]\n"

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

checkAction :: CheckFlags -> [String] -> GlobalFlags -> IO ()
checkAction checkFlags extraArgs _globalFlags = do
  -- xxx questo solo se non è vuoto
  -- Compute base context.
  let verbosityFlag = checkVerbosity checkFlags
      verbosity = fromFlag verbosityFlag
  baseCtx <- establishProjectBaseContext verbosity mempty OtherCommand
  let localPkgs = localPackages baseCtx
  let localIdSrc = mapMaybe specifierPkgIdSrc localPkgs

  -- xxx qui astra con cmdinstall
  -- Get/process selectors. The only sensible selectors for `check`
  -- are only those which refer to a package as a whole.
  targetSelectors <-
    either (reportTargetSelectorProblems verbosity) return
      =<< readTargetSelectors localPkgs Nothing extraArgs
  let processedSels = concat $ mapMaybe (processSelector (map fst localIdSrc)) targetSelectors

      -- And finally go from selectors to a directory we can feed to `check`.
      selectedIdSrc = filter (flip elem processedSels . fst) localIdSrc
      namesSrcs = map (\(li, ls) -> (pkgName li, ls)) selectedIdSrc

  allOk <- multiCheck (fromFlag verbosityFlag) (checkIgnore checkFlags) namesSrcs
  unless allOk exitFailure
  where
    -- Good selectors for `check` are only those who refer to a
    -- package as a whole.
    processSelector :: [PackageId] -> TargetSelector -> Maybe [PackageId]
    processSelector _ (TargetPackage _ pIds _) = Just pIds
    processSelector _ (TargetPackageNamed{}) = Nothing
    processSelector allIds (TargetAllPackages{}) = Just allIds
    processSelector _ (TargetComponent{}) = Nothing
    processSelector _ (TargetComponentUnknown{}) = Nothing
    -- xxx errori qui
    -- xxx qua non solo Nothing, ma anche errori

    -- Only 'PackageSpecifier's with an actual directory.
    specifierPkgIdSrc
      :: PackageSpecifier UnresolvedSourcePackage
      -> Maybe (PackageId, FilePath)
    specifierPkgIdSrc (NamedPackage{}) = Nothing
    specifierPkgIdSrc (SpecificSourcePackage pkg) =
      let pId = srcpkgPackageId pkg
       in -- No interested in remote/compressed sources.
          case srcpkgSource pkg of
            (LocalUnpackedPackage fp) -> Just (pId, fp)
            (LocalTarballPackage{}) -> Nothing
            (RemoteTarballPackage{}) -> Nothing
            (RepoTarballPackage{}) -> Nothing
            (RemoteSourceRepoPackage{}) -> Nothing

-- xxx orribile cambia
