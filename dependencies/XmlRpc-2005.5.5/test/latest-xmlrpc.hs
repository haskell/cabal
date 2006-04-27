-- An XML-RPC server that has a method "haskellXmlRpc.latestVersion"
-- which gets the filename of the the latest haskell-xml-rpc release.

import Network.XmlRpc.Server

import Control.Monad
import Data.Maybe
import Data.List
import System.Directory
import Text.Regex

--dir = "/users/pub/users/dg/d00bring/www.dtek.chalmers.se/haskell-xml-rpc"
dir = "/home/bjorn/chalmers/www/haskell-xml-rpc"

releaseRegex = mkRegex "haskell-xml-rpc-([[:digit:]]*)\\.tar\\.gz"

matches :: Regex -> String -> Bool
matches r s = isJust (matchRegex r s)

latest :: [String] -> String
latest = head . reverse . sort

latestVersion :: IO String
latestVersion =
    do
    fs <- getDirectoryContents dir
    let rs = filter (matches releaseRegex) fs
    when (null rs) (fail "No XmlRpc release found.")
    return (latest rs)

main = cgiXmlRpcServer [("haskellXmlRpc.latestVersion", fun latestVersion)]