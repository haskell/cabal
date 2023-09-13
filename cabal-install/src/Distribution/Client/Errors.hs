{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

-----------------------------------------------------------------------------

-- Module      :  Distribution.Client.Errors
-- Copyright   :  Suganya Arun
-- License     :  BSD3
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- A collection of Exception Types in the Cabal-Install library package

module Distribution.Client.Errors
  ( CabalInstallException (..)
  , exceptionCodeCabalInstall
  , exceptionMessageCabalInstall
  ) where

import Distribution.Compat.Prelude
import Distribution.Pretty
import Distribution.Simple.Utils
import Distribution.Types.PackageId

data CabalInstallException
  = UnpackGet
  | NotTarballDir FilePath
  | DirectoryAlreadyExists FilePath
  | FileExists FilePath
  | FileAlreadyExists FilePath
  | DirectoryExists FilePath
  | SplitRunArgs String
  | CouldNotFindExecutable
  | FoundMultipleMatchingExes
  | NoRemoteRepositories
  | NotATarDotGzFile FilePath
  | ExpectedMatchingFileName
  | NoTargetProvided
  | OneTargetRequired
  | ThisIsABug
  | NoOrMultipleTargetsGiven
  | NoTargetFound
  | MultipleTargetsFound
  | UnexpectedNamedPkgSpecifiers
  | UnexpectedSourcePkgSpecifiers
  | UnableToPerformInplaceUpdate
  | EmptyValuePagerEnvVariable
  | FileDoesntExist FilePath
  | ParseError
  | CabalFileNotFound FilePath
  | FindOpenProgramLocationErr String
  | PkgConfParseFailed String
  | ErrorPackingSdist String
  | SdistException PackageIdentifier
  | SpecifyAnExecutable
  | TestCommandDoesn'tSupport
  | ReportTargetProblems String
  | ListBinTargetException String
  | ResolveWithoutDependency String
  | CannotReadCabalFile FilePath
  | ErrorUpdatingIndex FilePath IOException
  | InternalError FilePath
  | ReadIndexCache FilePath
  | ConfigStateFileException String
  | UploadAction
  | UploadActionDocumentation
  | UploadActionOnlyArchives [FilePath]
  | FileNotFound FilePath
  | CheckAction [String]
  | ReportAction [String]
  | InitAction
  | UserConfigAction FilePath
  | SpecifySubcommand
  | UnknownUserConfigSubcommand [String]
  | ManpageAction [String]
  | UnrecognizedResponse
  deriving (Show, Typeable)

exceptionCodeCabalInstall :: CabalInstallException -> Int
exceptionCodeCabalInstall e = case e of
  UnpackGet{} -> 7013
  NotTarballDir{} -> 7012
  DirectoryAlreadyExists{} -> 7014
  FileExists{} -> 7015
  FileAlreadyExists{} -> 7016
  DirectoryExists{} -> 7017
  SplitRunArgs{} -> 7018
  CouldNotFindExecutable{} -> 7019
  FoundMultipleMatchingExes{} -> 7020
  NoRemoteRepositories{} -> 7021
  NotATarDotGzFile{} -> 7022
  ExpectedMatchingFileName{} -> 7023
  NoTargetProvided{} -> 7024
  OneTargetRequired{} -> 7025
  ThisIsABug -> 7026
  NoOrMultipleTargetsGiven{} -> 7027
  NoTargetFound{} -> 7028
  MultipleTargetsFound{} -> 7029
  UnexpectedNamedPkgSpecifiers{} -> 7030
  UnexpectedSourcePkgSpecifiers{} -> 7031
  UnableToPerformInplaceUpdate{} -> 7032
  EmptyValuePagerEnvVariable{} -> 7033
  FileDoesntExist{} -> 7034
  ParseError{} -> 7035
  CabalFileNotFound{} -> 7036
  FindOpenProgramLocationErr{} -> 7037
  PkgConfParseFailed{} -> 7038
  ErrorPackingSdist{} -> 7039
  SdistException{} -> 7040
  SpecifyAnExecutable{} -> 7041
  TestCommandDoesn'tSupport{} -> 7042
  ReportTargetProblems{} -> 7043
  ListBinTargetException{} -> 7044
  ResolveWithoutDependency{} -> 7045
  CannotReadCabalFile{} -> 7046
  ErrorUpdatingIndex{} -> 7047
  InternalError{} -> 7048
  ReadIndexCache{} -> 7049
  ConfigStateFileException{} -> 7050
  UploadAction{} -> 7051
  UploadActionDocumentation{} -> 7052
  UploadActionOnlyArchives{} -> 7053
  FileNotFound{} -> 7054
  CheckAction{} -> 7055
  ReportAction{} -> 7056
  InitAction{} -> 7057
  UserConfigAction{} -> 7058
  SpecifySubcommand{} -> 7059
  UnknownUserConfigSubcommand{} -> 7060
  ManpageAction{} -> 7061
  UnrecognizedResponse{} -> 7062

exceptionMessageCabalInstall :: CabalInstallException -> String
exceptionMessageCabalInstall e = case e of
  UnpackGet ->
    "The 'get' command does no yet support targets "
      ++ "that are remote source repositories."
  NotTarballDir t ->
    "The 'get' command is for tarball packages. "
      ++ "The target '"
      ++ t
      ++ "' is not a tarball."
  DirectoryAlreadyExists pkgdir' -> "The directory \"" ++ pkgdir' ++ "\" already exists and is not empty, not unpacking."
  FileExists pkgdir -> "A file \"" ++ pkgdir ++ "\" is in the way, not unpacking."
  FileAlreadyExists pkgFile -> "The file \"" ++ pkgFile ++ "\" already exists, not overwriting."
  DirectoryExists pkgFile -> "A directory \"" ++ pkgFile ++ "\" is in the way, not unpacking."
  SplitRunArgs err -> err
  CouldNotFindExecutable -> "run: Could not find executable in LocalBuildInfo"
  FoundMultipleMatchingExes -> "run: Found multiple matching exes in LocalBuildInfo"
  NoRemoteRepositories -> "Cannot upload. No remote repositories are configured."
  NotATarDotGzFile path -> "Not a tar.gz file: " ++ path
  ExpectedMatchingFileName -> "Expected a file name matching the pattern <pkgid>-docs.tar.gz"
  NoTargetProvided -> "One target is required, none provided"
  OneTargetRequired -> "One target is required, given multiple"
  ThisIsABug ->
    "No or multiple targets given, but the run "
      ++ "phase has been reached. This is a bug."
  NoOrMultipleTargetsGiven -> "No or multiple targets given..."
  NoTargetFound -> "No target found"
  MultipleTargetsFound -> "Multiple targets found"
  UnexpectedNamedPkgSpecifiers ->
    "internal error: 'resolveUserTargets' returned "
      ++ "unexpected named package specifiers!"
  UnexpectedSourcePkgSpecifiers ->
    "internal error: 'resolveUserTargets' returned "
      ++ "unexpected source package specifiers!"
  UnableToPerformInplaceUpdate -> "local project file has conditional and/or import logic, unable to perform and automatic in-place update"
  EmptyValuePagerEnvVariable -> "man: empty value of the PAGER environment variable"
  FileDoesntExist fpath -> "Error Parsing: file \"" ++ fpath ++ "\" doesn't exist. Cannot continue."
  ParseError -> "parse error"
  CabalFileNotFound cabalFile -> "Package .cabal file not found in the tarball: " ++ cabalFile
  FindOpenProgramLocationErr err -> err
  PkgConfParseFailed perror ->
    "Couldn't parse the output of 'setup register --gen-pkg-config':"
      ++ show perror
  ErrorPackingSdist err -> "Error packing sdist: " ++ err
  SdistException pkgIdentifier -> "sdist of " ++ prettyShow pkgIdentifier
  SpecifyAnExecutable -> "Please specify an executable to run"
  TestCommandDoesn'tSupport ->
    "The test command does not support '--only-dependencies'. "
      ++ "You may wish to use 'build --only-dependencies' and then "
      ++ "use 'test'."
  ReportTargetProblems problemsMsg -> problemsMsg
  ListBinTargetException errorStr -> errorStr
  ResolveWithoutDependency errorStr -> errorStr
  CannotReadCabalFile file -> "Cannot read .cabal file inside " ++ file
  ErrorUpdatingIndex name ioe -> "Error while updating index for " ++ name ++ " repository " ++ show ioe
  InternalError msg ->
    "internal error when reading package index: "
      ++ msg
      ++ "The package index or index cache is probably "
      ++ "corrupt. Running cabal update might fix it."
  ReadIndexCache path -> show (path)
  ConfigStateFileException err -> err
  UploadAction -> "the 'upload' command expects at least one .tar.gz archive."
  UploadActionDocumentation ->
    "the 'upload' command can only upload documentation "
      ++ "for one package at a time."
  UploadActionOnlyArchives otherFiles ->
    "the 'upload' command expects only .tar.gz archives: "
      ++ intercalate ", " otherFiles
  FileNotFound tarfile -> "file not found: " ++ tarfile
  CheckAction extraArgs -> "'check' doesn't take any extra arguments: " ++ unwords extraArgs
  ReportAction extraArgs -> "'report' doesn't take any extra arguments: " ++ unwords extraArgs
  InitAction ->
    "'init' only takes a single, optional, extra "
      ++ "argument for the project root directory"
  UserConfigAction path -> path ++ " already exists."
  SpecifySubcommand -> "Please specify a subcommand (see 'help user-config')"
  UnknownUserConfigSubcommand extraArgs -> "Unknown 'user-config' subcommand: " ++ unwords extraArgs
  ManpageAction extraArgs -> "'man' doesn't take any extra arguments: " ++ unwords extraArgs
  UnrecognizedResponse -> "unrecognized response"

instance Exception (VerboseException CabalInstallException) where
  displayException :: VerboseException CabalInstallException -> [Char]
  displayException (VerboseException stack timestamp verb cabalInstallException) =
    withOutputMarker
      verb
      ( concat
          [ "Error: [Cabal-"
          , show (exceptionCodeCabalInstall cabalInstallException)
          , "]\n"
          ]
      )
      ++ exceptionWithMetadata stack timestamp verb (exceptionMessageCabalInstall cabalInstallException)
