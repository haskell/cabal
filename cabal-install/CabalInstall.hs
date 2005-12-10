-- TODO:
-- make compilerCommand respect the --with-runhugs= stuff. doesn't work at all right now.
-- Don't continue if configure fails
-- integrate this prototype into cabal-get-bootstrap?

module Main where

import Control.Monad(when)
import Data.List(partition)
import System.Directory(getCurrentDirectory, setCurrentDirectory, getDirectoryContents)
import System.Cmd(system)
import System.Environment(getArgs)
import System.Exit(ExitCode(..), exitWith)

-- from cabal:
import Distribution.Compat.FilePath (splitFileExt, splitFilePath)
import Distribution.Setup(defaultCompilerFlavor, parseConfigureArgs, emptyConfigFlags, ConfigFlags(..))
import Distribution.Program(defaultProgramConfiguration, simpleProgram, updateProgram,
                            lookupProgram, rawSystemProgram,
                            Program(..), ProgramLocation(..))
import Distribution.Compiler (CompilerFlavor(..))

-- |If this is an error, quit with that exit code
quitFail :: ExitCode -> IO ()
quitFail ExitSuccess = return ()
quitFail e = exitWith e

doInstall dir conf confArgs = do
  currDir <- getCurrentDirectory
  setCurrentDirectory dir
  compM <- compilerCommand conf
  let comp = case compM of 
               Nothing -> noCompErr
               Just (Program _ _ _ EmptyLocation) -> noCompErr
               Just c  -> c
  let p = rawSystemProgram 3 comp
  p ("configure":confArgs) >>= quitFail
  p ["build"]              >>= quitFail
  p ["install"]            >>= quitFail
  setCurrentDirectory currDir
  return ()
      where noCompErr = error "No compiler found during configure.  Please use --with-runghc= or --with-runhugs=."

installerPrograms = defaultProgramConfiguration

-- |Figure out how to use runghc or runhugs.
compilerCommand :: ConfigFlags -> IO (Maybe Program)
compilerCommand conf = do
  let (progName, args)
          = case compilerFlav conf of
              GHC -> ("runghc",   ["Setup"])
              Hugs -> ("runhugs", ["-98", "Setup"])
  progM <- lookupProgram progName (configPrograms conf)
  case progM of
    Nothing   -> return Nothing
    Just prog -> return $ Just prog{programArgs=args ++ (programArgs prog)}

compilerFlav :: ConfigFlags -> CompilerFlavor
compilerFlav conf = 
    case configHcFlavor conf of
      Just GHC  -> GHC
      Just Hugs -> Hugs
      Nothing -> case defaultCompilerFlavor of
                   Just GHC  -> GHC
                   Just Hugs -> Hugs
                   Nothing   -> error "please specify one of --ghc or --hugs"

unPackOrGo tarOrDir conf confArgs = do
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

main :: IO ()
main = do
  putStrLn $ "default compiler: " ++ (show defaultCompilerFlavor)
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
