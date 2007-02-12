-- This is a quick hack for uploading packages to Hackage.
-- See http://hackage.haskell.org/trac/hackage/wiki/CabalUpload

import Network.Browser
import Network.HTTP

import Distribution.Compat.FilePath (joinFileName)

import Control.Monad
import Data.Char
import Data.Maybe
import Network.URI
import Numeric
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Random

type Username = String
type Password = String


uploadURI :: URI
uploadURI = fromJust $ parseURI "http://hackage.haskell.org/cgi-bin/hackage-scripts/protected/upload-pkg"

checkURI :: URI
checkURI = fromJust $ parseURI "http://hackage.haskell.org/cgi-bin/hackage-scripts/check-pkg"



main :: IO ()
main = do args <- getArgs
          (opts, paths) <- parseOptions args
          opts' <- if needsAuth opts then getAuth opts else return opts
          mapM_ (handlePackage opts') paths

handlePackage :: Options -> FilePath -> IO ()
handlePackage opts path =
  do (uri, auth) <- if optCheck opts 
                         then do putStr $ "Checking " ++ path ++ "... "
                                 hFlush stdout
                                 return (checkURI, return ())
                         else do putStr $ "Uploading " ++ path ++ "... "
                                 hFlush stdout
                                 return (uploadURI, 
                                         setAuth uploadURI 
                                                 (fromJust (optUsername opts))
                                                 (fromJust (optPassword opts)))
     req <- mkRequest uri path
     (_,resp) <- browse (setErrHandler ignoreMsg 
                      >> setOutHandler ignoreMsg 
                      >> auth 
                      >> request req)
     case rspCode resp of
       (2,0,0) -> do putStrLn "OK"
       (x,y,z) -> do hPutStrLn stderr $ "ERROR: " ++ map intToDigit [x,y,z] ++ " " ++ rspReason resp
                     hPutStrLn stderr $ rspBody resp

needsAuth :: Options -> Bool
needsAuth = not . optCheck

setAuth :: URI -> Username -> Password -> BrowserAction ()
setAuth uri user pwd = 
    addAuthority $ AuthBasic { auRealm    = "Hackage",
                               auUsername = user,
                               auPassword = pwd,
                               auSite     = uri }

getAuth :: Options -> IO Options
getAuth opts = case (optUsername opts, optPassword opts) of
                 (Just _, Just _  ) -> return opts
                 (Just _, Nothing ) -> do pwd <- promptPassword
                                          return $ opts { optPassword = Just pwd }
                 (Nothing  , Just _  ) -> die ["password given, but not username"]
                 (Nothing  , Nothing ) -> do (user,pwd) <- readAuthFile
                                             return $ opts { optUsername = Just user,
                                                             optPassword = Just pwd }

promptPassword :: IO Password
promptPassword = 
    do putStr "Hackage password: "
       hFlush stdout
       getLine

passwordFile :: IO FilePath
passwordFile = do dir <- getAppUserDataDirectory "cabal-upload"
                  return $ dir `joinFileName` "auth"

readAuthFile :: IO (Username,Password)
readAuthFile = 
    do file <- passwordFile
       s <- readFile file
       return $ read s

ignoreMsg :: String -> IO ()
ignoreMsg _ = return ()

mkRequest :: URI -> FilePath -> IO Request
mkRequest uri path = 
    do pkg <- readFile path
       boundary <- genBoundary
       let body = printMultiPart boundary (mkFormData path pkg)
       return $ Request {
                         rqURI = uri,
                         rqMethod = POST,
                         rqHeaders = [Header HdrContentType ("multipart/form-data; boundary="++boundary),
                                      Header HdrContentLength (show (length body)),
                                      Header HdrAccept ("text/plain")],
                         rqBody = body
                        }

genBoundary :: IO String
genBoundary = do i <- randomRIO (0x10000000000000,0xFFFFFFFFFFFFFF) :: IO Integer
                 return $ showHex i ""

mkFormData :: FilePath -> String -> [BodyPart]
mkFormData path pkg = 
    -- yes, web browsers are that stupid (re quoting)
    [BodyPart [Header hdrContentDisposition ("form-data; name=package; filename=\""++path++"\""),
               Header HdrContentType "application/x-gzip"] 
     pkg]

hdrContentDisposition :: HeaderName
hdrContentDisposition = HdrCustom "Content-disposition"

-- * Multipart, partly stolen from the cgi package.

data BodyPart = BodyPart [Header] String

printMultiPart :: String -> [BodyPart] -> String
printMultiPart boundary xs = 
    concatMap (printBodyPart boundary) xs ++ crlf ++ "--" ++ boundary ++ "--" ++ crlf

printBodyPart :: String -> BodyPart -> String
printBodyPart boundary (BodyPart hs c) = crlf ++ "--" ++ boundary ++ crlf ++ concatMap show hs ++ crlf ++ c

crlf :: String
crlf = "\r\n"

-- * Command-line options

data Options = Options {
                        optUsername  :: Maybe Username,
                        optPassword  :: Maybe Password,
                        optCheck     :: Bool,
                        optVerbosity :: Int
                       } deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
                          optUsername  = Nothing,
                          optPassword  = Nothing,
                          optCheck     = False,
                          optVerbosity = 1
                         }

optDescr :: [OptDescr (Options -> Options)]
optDescr = 
    [
     Option ['c'] ["check"] (NoArg (\o -> o { optCheck = True })) "Don't upload, just check.",
     Option ['u'] ["username"] (ReqArg (\u o -> o { optUsername = Just u}) "USERNAME") "Hackage username.",
     Option ['p'] ["password"] (ReqArg (\u o -> o { optPassword = Just u}) "PASSWORD") "Hackage password.",
     Option "v" ["verbose"] (OptArg (\u o -> o { optVerbosity = maybe 3 read u}) "N") "Control verbosity (N is 0--5, normal verbosity level is 1, -v alone is equivalent to -v3)",
     Option ['q'] ["quiet"] (NoArg (\o -> o { optVerbosity = 0 })) "Only essential output. Same as -v 0."
    ]

parseOptions :: [String] -> IO (Options, [FilePath])
parseOptions args = 
   do let (fs, files, nonopts, errs) = getOpt' RequireOrder optDescr args
      when (not (null errs)) $ die errs
      case nonopts of
        []         -> return $ (foldl (flip ($)) defaultOptions fs, files)
        ["--help"] -> usage
        _          -> die (map (("unrecognized option "++).show) nonopts)

die :: [String] -> IO a
die errs = do mapM_ (\e -> hPutStrLn stderr $ "cabal-upload: " ++ e) $ errs
              hPutStrLn stderr "Try `cabal-upload --help' for more information."
              exitFailure

usage :: IO a
usage = do pwdFile <- passwordFile
           let hdr = unlines ["cabal-upload uploads Cabal source packages to Hackage.",
                              "",
                              "You can store your Hackage login in " ++ pwdFile,
                              "using the format (\"username\",\"password\").",
                              "",
                              "Usage: cabal-upload [OPTION ...] [FILE ...]"]
           putStrLn (usageInfo hdr optDescr)
           exitWith ExitSuccess
