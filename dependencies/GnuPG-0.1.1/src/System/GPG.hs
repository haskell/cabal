module System.GPG
    ( Options (..)
    , stdOptions
    , mkDefaultOptions
    , signArmored
    , signArmoredFile
    , verify
    , verifyFile
    , exportArmored
    , importKey
    , optionsToArgs
    ) where

import Control.Monad

import System.Process
import System.Directory
import System.Exit
import System.IO

data Options
    = Options
    { gpgBinary        :: FilePath
    , dryRun           :: Bool
    , batch            :: Bool
    , noOptions        :: Bool
    , homeDirectory    :: Maybe FilePath
    , noDefaultKeyring :: Bool
    , keyrings         :: [FilePath]
    } deriving (Show,Eq)

stdOptions :: Options
stdOptions =
    Options
    { gpgBinary        = "gpg"
    , dryRun           = False
    , batch            = False
    , noOptions        = False
    , homeDirectory    = Nothing
    , noDefaultKeyring = False
    , keyrings         = []
    }

mkDefaultOptions :: IO Options
mkDefaultOptions =
    do mbGPG <- findExecutable "gpg"
       case mbGPG of
         Just gpg -> return $ stdOptions { gpgBinary = gpg }
         Nothing  -> error "Failed to locate the GnuPG binary."

optionsToArgs :: Options -> [String]
optionsToArgs opts
    = foldr boolOpt [] [(dryRun opts,"--dry-run")
                       ,(batch opts,"--batch")
                       ,(noDefaultKeyring opts,"--no-default-keyring")
                       ,(noOptions opts,"--no-options")
                       ] ++
      foldr (\f l -> "--keyring":f:l) [] (keyrings opts) ++
      maybe [] (\homeDir -> ["--homedir",homeDir]) (homeDirectory opts)
    where boolOpt (True,x) = (:) x
          boolOpt (False,_) = id

execGPG :: Options
        -> [String]
        -> String
        -> IO (String,ExitCode)
execGPG opts args input
    = do (inh,outh,_errh,handle) <- runInteractiveProcess
                                   (gpgBinary opts)
                                   (optionsToArgs opts ++ args)
                                   Nothing Nothing
         when (not (null input)) (hPutStr inh input)
         hClose inh
         output <- hGetContents outh
         length output `seq` hClose outh
         {-err <- hGetContents errh
         length err `seq` hClose errh
         writeFile "/var/www/gpgError" err -}
         eCode <- waitForProcess handle
         return (output,eCode)

switchResult :: IO (String,ExitCode) -> (String -> IO a) -> (Int -> IO a) -> IO a
switchResult action okHandle errHandle
    = action >>= \(output,code) ->
      case code of
        ExitSuccess     -> okHandle output
        ExitFailure err -> errHandle err

execGPGThrowOnError :: Options
                    -> [String]
                    -> String
                    -> IO String
execGPGThrowOnError opts args input
    = switchResult (execGPG opts args input)
                   (return)
                   (\err -> error $ unwords (gpgBinary opts:optionsToArgs opts ++ args) ++
                                    "\nGPG operation failed with code: " ++ show err)

signArmored :: Options -> String -> IO String
signArmored opts str
    = execGPGThrowOnError opts ["--sign","--detach","--armor"] str

signArmoredFile :: Options -> FilePath -> IO String
signArmoredFile opts filepath = signArmored opts =<< readFile filepath

verifyFile :: Options -> FilePath -> FilePath -> IO Bool
verifyFile opts signature file
    = switchResult (execGPG opts ["--verify",signature,file] "")
                   (const $ return True)
                   (const $ return False)

verify :: Options -> String -> FilePath -> IO Bool
verify opts signature file
    = switchResult (execGPG opts ["--verify","-",file] signature)
                   (const $ return True)
                   (const $ return False)

exportArmored :: Options -> IO String
exportArmored opts
    = execGPGThrowOnError opts ["--export","--armor"] ""

importKey :: Options -> FilePath -> IO Int
importKey opts pubkey
    = switchResult (execGPG opts ["--import",pubkey] "")
                   (const $ return 0)
                   (return)
