-- TODO:
-- make compilerCommand respect the --with-runhugs= stuff. doesn't work at all right now.
-- Don't continue if configure fails
-- integrate this prototype into cabal-get-bootstrap?

module Main where

import Control.Monad(when)
import Data.List(partition)
import System.Directory(getCurrentDirectory, setCurrentDirectory, getDirectoryContents, doesFileExist)
import System.Cmd(system)
import System.Environment(getArgs)
import System.Exit(ExitCode(..), exitWith)

-- from cabal:
import Distribution.Compat.FilePath (splitFileExt, splitFilePath, joinPaths)
import Distribution.Setup(defaultCompilerFlavor, parseConfigureArgs, emptyConfigFlags, ConfigFlags(..))
import Distribution.Program(defaultProgramConfiguration, simpleProgram, updateProgram,
                            lookupProgram, rawSystemProgram,
                            Program(..), ProgramLocation(..))
import Distribution.Simple (defaultMainArgs)
import Distribution.Simple.Configure(configCompilerAux)
import Distribution.Compiler (CompilerFlavor(..), Compiler(..))

-- ------------------------------------------------------------
-- * Guts
-- ------------------------------------------------------------

-- |Is there a Setup.hs or Setup.lhs in the given directory?
hasSetup :: FilePath -- the directory to check for the Setup module
         -> IO Bool
hasSetup dir = do
  e1 <- doesFileExist (dir `joinPaths` "Setup.lhs")
  e2 <- doesFileExist (dir `joinPaths` "Setup.hs")
  return $ e1 || e2

-- |perform runghc (or runhugs) Setup configure, (build, install). 
doInstall dir conf confArgs = do
  currDir <- getCurrentDirectory
  setCurrentDirectory dir
  hasSetupMod <- hasSetup dir

  p <- if hasSetupMod
        then do compM <- compilerCommand conf
                let comp = case compM of 
                             Nothing -> noCompErr
                             Just (Program _ _ _ EmptyLocation) -> noCompErr
                             Just c  -> c
                return $ \args -> (rawSystemProgram 3 comp args) >>= quitFail
        else do putStrLn $ "no Setup file found in " ++ dir ++ ". Using defaultMain."
                return defaultMainArgs
  p ("configure":confArgs)
  p ["build"]
  p ["install"]
  setCurrentDirectory currDir
  return ()
      where noCompErr
                = error "No compiler found during configure.  Please use --with-runghc= or --with-runhugs=."

-- |Call doInstall on the given argument, perhaps after unzipping it or whatever.
unPackOrGo tarOrDirIn conf confArgs = do
  currDir <- getCurrentDirectory
  let tarOrDir = currDir `joinPaths` tarOrDirIn
  let (packageIdentStr, ext) = splitFileExt tarOrDir
  putStrLn $ "package: " ++ (show packageIdentStr)
  case ext of
    "tgz" -> do system $ "tar -zxvf " ++ tarOrDir
                unPackOrGo packageIdentStr conf confArgs
    "gz"  -> do system $ "gunzip " ++ tarOrDir
                unPackOrGo packageIdentStr conf confArgs
    "tar" -> do system $ "tar -xvf " ++ tarOrDir
                unPackOrGo packageIdentStr conf confArgs
    "cabal" -> let (dir, _, _) = splitFilePath tarOrDir
                   in doInstall dir conf confArgs
    _     -> doInstall tarOrDir conf confArgs


-- ------------------------------------------------------------
-- * Helpers
-- ------------------------------------------------------------

-- |If this is an error, quit with that exit code
quitFail :: ExitCode -> IO ()
quitFail ExitSuccess = return ()
quitFail e = exitWith e

-- Probably delete this.
installerPrograms = defaultProgramConfiguration

-- |Figure out how to use ghc or runhugs.
compilerCommand :: ConfigFlags -> IO (Maybe Program)
compilerCommand conf =
  case compilerFlav conf of
         Hugs -> do let (progName, args) = ("runhugs", ["-98", "Setup"])
                    progM <- lookupProgram progName (configPrograms conf)
                    case progM of
                      Nothing   -> return Nothing
                      Just prog -> return $ Just prog{programArgs=args ++ (programArgs prog)}
         -- Yuck.  This would be much easier if the compilers used the Program interface.
         GHC  -> do let (progName, args) = ("ghc",   ["--make", "-package",
                                                      "Cabal", "Setup", "-o", "setup"])
                    ghc1 <- configCompilerAux conf
                    let ghc2 = Program "ghc" "ghc" [] (FoundOnSystem (compilerPath ghc1))
                    putStrLn "compiling Setup."
                    rawSystemProgram (configVerbose conf) ghc2 args
                    dir <- getCurrentDirectory
                    let path = dir `joinPaths` "setup"
                    return $ Just $ Program "setup" "setup" [] (FoundOnSystem path)

-- |Given the configure flags, get the compiler flavor.
compilerFlav :: ConfigFlags -> CompilerFlavor
compilerFlav conf = 
    case configHcFlavor conf of
      Just GHC  -> GHC
      Just Hugs -> Hugs
      Nothing -> case defaultCompilerFlavor of
                   Just GHC  -> GHC
                   Just Hugs -> Hugs
                   Nothing   -> error "please specify one of --ghc or --hugs"

-- ------------------------------------------------------------
-- * Main
-- ------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let (toInstall, configArgs) = partition (\x -> head x /= '-') args
  (conf, _, _) <- parseConfigureArgs
                    installerPrograms
                    (emptyConfigFlags installerPrograms)
                     configArgs
                     []
  when (toInstall == []) (error "please give a tarball or directory on the command-line (try --help)")
  mapM (\i -> unPackOrGo i conf configArgs) toInstall
  return ()
