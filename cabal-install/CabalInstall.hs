-- TODO:
-- make compilerCommand respect the --with-runhugs= stuff. doesn't work at all right now.
-- Don't continue if configure fails
-- integrate this prototype into cabal-get-bootstrap?

module Main where

import Control.Monad(when)
import Data.Maybe (fromJust) -- FIX: remove.
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
  comp <- compilerCommand conf
  -- FIX: remove fromJust
  let p = rawSystemProgram 0 (fromJust comp) 
  p ("configure":confArgs) >>= quitFail
  p ["build"]              >>= quitFail
  p ["install"]            >>= quitFail
  setCurrentDirectory currDir
  return ()

installerPrograms = 
    foldl (\pConf p -> updateProgram (Just p) pConf)
                        defaultProgramConfiguration
                        [Program "runghc" "runghc" ["Setup"] EmptyLocation,
                         Program "runhugs" "runhugs" ["-98", "Setup"] EmptyLocation]

-- FIX: 
-- * get program config from --with-runghc=, etc.
-- * some day be more flexible here; allow setup script to be built
-- w/ nhc or GHC, for instance.
compilerCommand :: ConfigFlags -> IO (Maybe Program)
compilerCommand conf = do
  let prog = (case configHcFlavor conf of
               Just GHC  -> "runghc"
               Just Hugs -> "runhugs"
               Nothing -> case defaultCompilerFlavor of
                           Just GHC  -> "runghc"
                           Just Hugs -> "runhugs"
                           Nothing   -> error "please specify one of --ghc or --hugs")
  lookupProgram prog (configPrograms conf)

unPackOrGo tarOrDir conf confArgs = do
  let (packageIdentStr, ext) = splitFileExt tarOrDir
  putStrLn $ "package; " ++ (show packageIdentStr)
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
  when (toInstall == []) (error "please give a tarball or directory on the command-line.")
  mapM (\i -> unPackOrGo i conf configArgs) toInstall
  return ()
