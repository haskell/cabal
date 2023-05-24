-----------------------------------------------------------------------------

-- This module is meant to be local-only to Distribution...

-- |
-- Module      :  Distribution.InstalledPackageInfo
-- Copyright   :  (c) The University of Glasgow 2004
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- This is the information about an /installed/ package that
-- is communicated to the @ghc-pkg@ program in order to register
-- a package.  @ghc-pkg@ now consumes this package format (as of version
-- 6.4). This is specific to GHC at the moment.
--
-- The @.cabal@ file format is for describing a package that is not yet
-- installed. It has a lot of flexibility, like conditionals and dependency
-- ranges. As such, that format is not at all suitable for describing a package
-- that has already been built and installed. By the time we get to that stage,
-- we have resolved all conditionals and resolved dependency version
-- constraints to exact versions of dependent packages. So, this module defines
-- the 'InstalledPackageInfo' data structure that contains all the info we keep
-- about an installed package. There is a parser and pretty printer. The
-- textual format is rather simpler than the @.cabal@ format: there are no
-- sections, for example.
module Distribution.InstalledPackageInfo
  ( InstalledPackageInfo (..)
  , installedComponentId
  , installedOpenUnitId
  , sourceComponentName
  , requiredSignatures
  , ExposedModule (..)
  , AbiDependency (..)
  , emptyInstalledPackageInfo
  , parseInstalledPackageInfo
  , showInstalledPackageInfo
  , showFullInstalledPackageInfo
  , showInstalledPackageInfoField
  , showSimpleInstalledPackageInfoField
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Backpack
import Distribution.CabalSpecVersion (cabalSpecLatest)
import Distribution.FieldGrammar
import Distribution.FieldGrammar.FieldDescrs
import Distribution.Fields.Pretty
import Distribution.ModuleName
import Distribution.Package hiding (installedUnitId)
import Distribution.Types.ComponentName
import Distribution.Utils.Generic (toUTF8BS)

import Data.ByteString (ByteString)

import qualified Data.Map as Map
import qualified Distribution.Fields as P
import qualified Text.PrettyPrint as Disp

import Distribution.Types.InstalledPackageInfo
import Distribution.Types.InstalledPackageInfo.FieldGrammar

-- $setup
-- >>> :set -XOverloadedStrings

installedComponentId :: InstalledPackageInfo -> ComponentId
installedComponentId ipi =
  case unComponentId (installedComponentId_ ipi) of
    "" -> mkComponentId (unUnitId (installedUnitId ipi))
    _ -> installedComponentId_ ipi

-- | Get the indefinite unit identity representing this package.
-- This IS NOT guaranteed to give you a substitution; for
-- instantiated packages you will get @DefiniteUnitId (installedUnitId ipi)@.
-- For indefinite libraries, however, you will correctly get
-- an @OpenUnitId@ with the appropriate 'OpenModuleSubst'.
installedOpenUnitId :: InstalledPackageInfo -> OpenUnitId
installedOpenUnitId ipi =
  mkOpenUnitId (installedUnitId ipi) (installedComponentId ipi) (Map.fromList (instantiatedWith ipi))

-- | Returns the set of module names which need to be filled for
-- an indefinite package, or the empty set if the package is definite.
requiredSignatures :: InstalledPackageInfo -> Set ModuleName
requiredSignatures ipi = openModuleSubstFreeHoles (Map.fromList (instantiatedWith ipi))

-- -----------------------------------------------------------------------------
-- Munging

sourceComponentName :: InstalledPackageInfo -> ComponentName
sourceComponentName = CLibName . sourceLibName

-- -----------------------------------------------------------------------------
-- Parsing

-- | Return either errors, or IPI with list of warnings
parseInstalledPackageInfo
  :: ByteString
  -> Either (NonEmpty String) ([String], InstalledPackageInfo)
parseInstalledPackageInfo s = case P.readFields s of
  Left err -> Left (show err :| [])
  Right fs -> case partitionFields fs of
    (fs', _) -> case P.runParseResult $ parseFieldGrammar cabalSpecLatest fs' ipiFieldGrammar of
      (ws, Right x) -> x `deepseq` Right (ws', x)
        where
          ws' =
            [ P.showPWarning "" w
            | w@(P.PWarning wt _ _) <- ws
            , -- filter out warnings about experimental features
            wt /= P.PWTExperimental
            ]
      (_, Left (_, errs)) -> Left errs'
        where
          errs' = fmap (P.showPError "") errs

-- -----------------------------------------------------------------------------
-- Pretty-printing

-- | Pretty print 'InstalledPackageInfo'.
--
-- @pkgRoot@ isn't printed, as ghc-pkg prints it manually (as GHC-8.4).
showInstalledPackageInfo :: InstalledPackageInfo -> String
showInstalledPackageInfo ipi =
  showFullInstalledPackageInfo ipi{pkgRoot = Nothing}

-- | The variant of 'showInstalledPackageInfo' which outputs @pkgroot@ field too.
showFullInstalledPackageInfo :: InstalledPackageInfo -> String
showFullInstalledPackageInfo = P.showFields (const NoComment) . prettyFieldGrammar cabalSpecLatest ipiFieldGrammar

-- |
--
-- >>> let ipi = emptyInstalledPackageInfo { maintainer = "Tester" }
-- >>> fmap ($ ipi) $ showInstalledPackageInfoField "maintainer"
-- Just "maintainer: Tester"
showInstalledPackageInfoField :: String -> Maybe (InstalledPackageInfo -> String)
showInstalledPackageInfoField fn =
  fmap (\g -> Disp.render . ppField fn . g) $ fieldDescrPretty ipiFieldGrammar (toUTF8BS fn)

showSimpleInstalledPackageInfoField :: String -> Maybe (InstalledPackageInfo -> String)
showSimpleInstalledPackageInfoField fn =
  fmap (Disp.renderStyle myStyle .) $ fieldDescrPretty ipiFieldGrammar (toUTF8BS fn)
  where
    myStyle = Disp.style{Disp.mode = Disp.LeftMode}

ppField :: String -> Disp.Doc -> Disp.Doc
ppField name fielddoc
  | Disp.isEmpty fielddoc = mempty
  | otherwise = Disp.text name <<>> Disp.colon Disp.<+> fielddoc
