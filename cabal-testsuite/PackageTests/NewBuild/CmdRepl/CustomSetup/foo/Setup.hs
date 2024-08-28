import           Control.Monad
import           Distribution.PackageDescription
import           Distribution.Simple
import qualified Distribution.Simple as DS
import           Distribution.Simple.Setup

main :: IO ()
main = DS.defaultMainWithHooks DS.simpleUserHooks
