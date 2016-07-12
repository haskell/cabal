import Distribution.Simple
import System.Environment
import P
main = getArgs >>= print >> defaultMain
