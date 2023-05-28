module Distribution.Client.BuildReports.Lens
  ( BuildReport
  , module Distribution.Client.BuildReports.Lens
  ) where

import Distribution.Client.Compat.Prelude
import Distribution.Compat.Lens
import Prelude ()

import Distribution.Client.BuildReports.Types (BuildReport, InstallOutcome, Outcome)
import Distribution.Compiler (CompilerId)
import Distribution.System (Arch, OS)
import Distribution.Types.Flag (FlagAssignment)
import Distribution.Types.PackageId (PackageIdentifier)

import qualified Distribution.Client.BuildReports.Types as T

package :: Lens' BuildReport PackageIdentifier
package f s = fmap (\x -> s{T.package = x}) (f (T.package s))

os :: Lens' BuildReport OS
os f s = fmap (\x -> s{T.os = x}) (f (T.os s))

arch :: Lens' BuildReport Arch
arch f s = fmap (\x -> s{T.arch = x}) (f (T.arch s))

compiler :: Lens' BuildReport CompilerId
compiler f s = fmap (\x -> s{T.compiler = x}) (f (T.compiler s))

client :: Lens' BuildReport PackageIdentifier
client f s = fmap (\x -> s{T.client = x}) (f (T.client s))

flagAssignment :: Lens' BuildReport FlagAssignment
flagAssignment f s = fmap (\x -> s{T.flagAssignment = x}) (f (T.flagAssignment s))

dependencies :: Lens' BuildReport [PackageIdentifier]
dependencies f s = fmap (\x -> s{T.dependencies = x}) (f (T.dependencies s))

installOutcome :: Lens' BuildReport InstallOutcome
installOutcome f s = fmap (\x -> s{T.installOutcome = x}) (f (T.installOutcome s))

docsOutcome :: Lens' BuildReport Outcome
docsOutcome f s = fmap (\x -> s{T.docsOutcome = x}) (f (T.docsOutcome s))

testsOutcome :: Lens' BuildReport Outcome
testsOutcome f s = fmap (\x -> s{T.testsOutcome = x}) (f (T.testsOutcome s))
