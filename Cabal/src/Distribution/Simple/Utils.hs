{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Utils
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
-- License     :  BSD3
--                portions Copyright (c) 2007, Galois Inc.
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- A large and somewhat miscellaneous collection of utility functions used
-- throughout the rest of the Cabal lib and in other tools that use the Cabal
-- lib like @cabal-install@. It has a very simple set of logging actions. It
-- has low level functions for running programs, a bunch of wrappers for
-- various directory and file functions that do extra logging.
module Distribution.Simple.Utils
  ( cabalVersion

    -- * logging and errors
  , dieNoVerbosity
  , die'
  , dieWithException
  , dieWithLocation'
  , dieNoWrap
  , topHandler
  , topHandlerWith
  , warn
  , warnError
  , notice
  , noticeNoWrap
  , noticeDoc
  , setupMessage
  , info
  , infoNoWrap
  , debug
  , debugNoWrap
  , chattyTry
  , annotateIO
  , exceptionWithMetadata
  , withOutputMarker

    -- * exceptions
  , handleDoesNotExist
  , ignoreSigPipe

    -- * running programs
  , rawSystemExit
  , rawSystemExitCode
  , rawSystemProc
  , rawSystemProcAction
  , rawSystemExitWithEnv
  , rawSystemStdout
  , rawSystemStdInOut
  , rawSystemIOWithEnv
  , rawSystemIOWithEnvAndAction
  , fromCreatePipe
  , maybeExit
  , xargs
  , findProgramVersion

    -- ** 'IOData' re-export

  --
  -- These types are re-exported from
  -- "Distribution.Utils.IOData" for convenience as they're
  -- exposed in the API of 'rawSystemStdInOut'
  , IOData (..)
  , KnownIODataMode (..)
  , IODataMode (..)
  , VerboseException (..)

    -- * copying files
  , createDirectoryIfMissingVerbose
  , copyFileVerbose
  , copyFiles
  , copyFileTo

    -- * installing files
  , installOrdinaryFile
  , installExecutableFile
  , installMaybeExecutableFile
  , installOrdinaryFiles
  , installExecutableFiles
  , installMaybeExecutableFiles
  , installDirectoryContents
  , copyDirectoryRecursive

    -- * File permissions
  , doesExecutableExist
  , setFileOrdinary
  , setFileExecutable

    -- * file names
  , currentDir
  , shortRelativePath
  , dropExeExtension
  , exeExtensions

    -- * finding files
  , findFileEx
  , findFileCwd
  , findFirstFile
  , findFileWithExtension
  , findFileCwdWithExtension
  , findFileWithExtension'
  , findAllFilesWithExtension
  , findAllFilesCwdWithExtension
  , findModuleFileEx
  , findModuleFilesEx
  , getDirectoryContentsRecursive

    -- * environment variables
  , isInSearchPath
  , addLibraryPath

    -- * modification time
  , moreRecentFile
  , existsAndIsMoreRecentThan

    -- * temp files and dirs
  , TempFileOptions (..)
  , defaultTempFileOptions
  , withTempFile
  , withTempFileEx
  , withTempDirectory
  , withTempDirectoryEx
  , createTempDirectory

    -- * .cabal and .buildinfo files
  , defaultPackageDesc
  , findPackageDesc
  , findPackageDescCwd
  , tryFindPackageDesc
  , tryFindPackageDescCwd
  , findHookedPackageDesc

    -- * reading and writing files safely
  , withFileContents
  , writeFileAtomic
  , rewriteFileEx
  , rewriteFileLBS

    -- * Unicode
  , fromUTF8BS
  , fromUTF8LBS
  , toUTF8BS
  , toUTF8LBS
  , readUTF8File
  , withUTF8FileContents
  , writeUTF8File
  , normaliseLineEndings

    -- * BOM
  , ignoreBOM

    -- * generic utils
  , dropWhileEndLE
  , takeWhileEndLE
  , equating
  , comparing
  , isInfixOf
  , intercalate
  , lowercase
  , listUnion
  , listUnionRight
  , ordNub
  , ordNubBy
  , ordNubRight
  , safeHead
  , safeTail
  , safeLast
  , safeInit
  , unintersperse
  , wrapText
  , wrapLine

    -- * FilePath stuff
  , isAbsoluteOnAnyPlatform
  , isRelativeOnAnyPlatform
  , exceptionWithCallStackPrefix
  ) where

import Distribution.Compat.Async (waitCatch, withAsyncNF)
import Distribution.Compat.CopyFile
import Distribution.Compat.FilePath as FilePath
import Distribution.Compat.Internal.TempFile
import Distribution.Compat.Lens (Lens', over)
import Distribution.Compat.Prelude
import Distribution.Compat.Stack
import Distribution.ModuleName as ModuleName
import Distribution.Simple.Errors
import Distribution.System
import Distribution.Types.PackageId
import Distribution.Utils.Generic
import Distribution.Utils.IOData (IOData (..), IODataMode (..), KnownIODataMode (..))
import qualified Distribution.Utils.IOData as IOData
import Distribution.Verbosity
import Distribution.Version
import Prelude ()

#ifdef CURRENT_PACKAGE_KEY
#define BOOTSTRAPPED_CABAL 1
#endif

#ifdef BOOTSTRAPPED_CABAL
import qualified Paths_Cabal (version)
#endif

import Distribution.Parsec
import Distribution.Pretty

import qualified Data.ByteString.Lazy as BS
import Data.Typeable
  ( cast
  )

import qualified Control.Exception as Exception
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Distribution.Compat.Process (proc)
import Foreign.C.Error (Errno (..), ePIPE)
import qualified GHC.IO.Exception as GHC
import GHC.Stack (HasCallStack)
import Numeric (showFFloat)
import System.Directory
  ( Permissions (executable)
  , createDirectory
  , doesDirectoryExist
  , doesFileExist
  , getDirectoryContents
  , getModificationTime
  , getPermissions
  , removeDirectoryRecursive
  , removeFile
  )
import System.Environment
  ( getProgName
  )
import System.FilePath as FilePath
  ( getSearchPath
  , joinPath
  , normalise
  , searchPathSeparator
  , splitDirectories
  , splitExtension
  , takeDirectory
  , (<.>)
  , (</>)
  )
import System.IO
  ( BufferMode (..)
  , Handle
  , hClose
  , hFlush
  , hGetContents
  , hPutStr
  , hPutStrLn
  , hSetBinaryMode
  , hSetBuffering
  , stderr
  , stdout
  )
import System.IO.Error
import System.IO.Unsafe
  ( unsafeInterleaveIO
  )
import qualified System.Process as Process
import qualified Text.PrettyPrint as Disp

-- We only get our own version number when we're building with ourselves
cabalVersion :: Version
#if defined(BOOTSTRAPPED_CABAL)
cabalVersion = mkVersion' Paths_Cabal.version
#elif defined(CABAL_VERSION)
cabalVersion = mkVersion [CABAL_VERSION]
#else
cabalVersion = mkVersion [3,0]  --used when bootstrapping
#endif

-- ----------------------------------------------------------------------------
-- Exception and logging utils

-- Cabal's logging infrastructure has a few constraints:
--
--  * We must make all logging formatting and emissions decisions based
--    on the 'Verbosity' parameter, which is the only parameter that is
--    plumbed to enough call-sites to actually be used for this matter.
--    (One of Cabal's "big mistakes" is to have never have defined a
--    monad of its own.)
--
--  * When we 'die', we must raise an IOError.  This a backwards
--    compatibility consideration, because that's what we've raised
--    previously, and if we change to any other exception type,
--    exception handlers which match on IOError will no longer work.
--    One case where it is known we rely on IOError being catchable
--    is 'readPkgConfigDb' in cabal-install; there may be other
--    user code that also assumes this.
--
--  * The 'topHandler' does not know what 'Verbosity' is, because
--    it gets called before we've done command line parsing (where
--    the 'Verbosity' parameter would come from).
--
-- This leads to two big architectural choices:
--
--  * Although naively we might imagine 'Verbosity' to be a simple
--    enumeration type, actually it is a full-on abstract data type
--    that may contain arbitrarily complex information.  At the
--    moment, it is fully representable as a string, but we might
--    eventually also use verbosity to let users register their
--    own logging handler.
--
--  * When we call 'die', we perform all the formatting and addition
--    of extra information we need, and then ship this in the IOError
--    to the top-level handler.  Here are alternate designs that
--    don't work:
--
--      a) Ship the unformatted info to the handler.  This doesn't
--      work because at the point the handler gets the message,
--      we've lost call stacks, and even if we did, we don't have access
--      to 'Verbosity' to decide whether or not to render it.
--
--      b) Print the information at the 'die' site, then raise an
--      error.  This means that if the exception is subsequently
--      caught by a handler, we will still have emitted the output,
--      which is not the correct behavior.
--
--    For the top-level handler to "know" that an error message
--    contains one of these fully formatted packets, we set a sentinel
--    in one of IOError's extra fields.  This is handled by
--    'ioeSetVerbatim' and 'ioeGetVerbatim'.
--

dieNoVerbosity :: String -> IO a
dieNoVerbosity msg =
  ioError (userError msg)
  where
    _ = callStack -- TODO: Attach CallStack to exception

-- | Tag an 'IOError' whose error string should be output to the screen
-- verbatim.
ioeSetVerbatim :: IOError -> IOError
ioeSetVerbatim e = ioeSetLocation e "dieVerbatim"

-- | Check if an 'IOError' should be output verbatim to screen.
ioeGetVerbatim :: IOError -> Bool
ioeGetVerbatim e = ioeGetLocation e == "dieVerbatim"

-- | Create a 'userError' whose error text will be output verbatim
verbatimUserError :: String -> IOError
verbatimUserError = ioeSetVerbatim . userError

dieWithLocation' :: Verbosity -> FilePath -> Maybe Int -> String -> IO a
dieWithLocation' verbosity filename mb_lineno msg =
  die' verbosity $
    filename
      ++ ( case mb_lineno of
            Just lineno -> ":" ++ show lineno
            Nothing -> ""
         )
      ++ ": "
      ++ msg

die' :: Verbosity -> String -> IO a
die' verbosity msg = withFrozenCallStack $ do
  ioError . verbatimUserError
    =<< annotateErrorString verbosity
    =<< pure . wrapTextVerbosity verbosity
    =<< pure . addErrorPrefix
    =<< prefixWithProgName msg

-- Type which will be a wrapper for cabal -expections and cabal-install exceptions
data VerboseException a = VerboseException CallStack POSIXTime Verbosity a
  deriving (Show, Typeable)

-- Function which will replace the existing die' call sites
dieWithException :: (HasCallStack, Show a1, Typeable a1, Exception (VerboseException a1)) => Verbosity -> a1 -> IO a
dieWithException verbosity exception = do
  ts <- getPOSIXTime
  throwIO $ VerboseException callStack ts verbosity exception

-- Instance for Cabal Exception which will display error code and error message with callStack info
instance Exception (VerboseException CabalException) where
  displayException :: VerboseException CabalException -> [Char]
  displayException (VerboseException stack timestamp verb cabalexception) =
    withOutputMarker
      verb
      ( concat
          [ "Error: [Cabal-"
          , show (exceptionCode cabalexception)
          , "]\n"
          ]
      )
      ++ exceptionWithMetadata stack timestamp verb (exceptionMessage cabalexception)

dieNoWrap :: Verbosity -> String -> IO a
dieNoWrap verbosity msg = withFrozenCallStack $ do
  -- TODO: should this have program name or not?
  ioError . verbatimUserError
    =<< annotateErrorString
      verbosity
      (addErrorPrefix msg)

-- | Prefixing a message to indicate that it is a fatal error,
-- if the 'errorPrefix' is not already present.
addErrorPrefix :: String -> String
addErrorPrefix msg
  | errorPrefix `isPrefixOf` msg = msg
  -- Backpack prefixes its errors already with "Error:", see
  -- 'Distribution.Utils.LogProgress.dieProgress'.
  -- Taking it away there destroys the layout, so we rather
  -- check here whether the prefix is already present.
  | otherwise = unwords [errorPrefix, msg]

-- | A prefix indicating that a message is a fatal error.
errorPrefix :: String
errorPrefix = "Error:"

-- | Prefix an error string with program name from 'getProgName'
prefixWithProgName :: String -> IO String
prefixWithProgName msg = do
  pname <- getProgName
  return $ pname ++ ": " ++ msg

-- | Annotate an error string with timestamp and 'withMetadata'.
annotateErrorString :: Verbosity -> String -> IO String
annotateErrorString verbosity msg = do
  ts <- getPOSIXTime
  return $ withMetadata ts AlwaysMark VerboseTrace verbosity msg

-- | Given a block of IO code that may raise an exception, annotate
-- it with the metadata from the current scope.  Use this as close
-- to external code that raises IO exceptions as possible, since
-- this function unconditionally wraps the error message with a trace
-- (so it is NOT idempotent.)
annotateIO :: Verbosity -> IO a -> IO a
annotateIO verbosity act = do
  ts <- getPOSIXTime
  flip modifyIOError act $
    ioeModifyErrorString $
      withMetadata ts NeverMark VerboseTrace verbosity

-- | A semantic editor for the error message inside an 'IOError'.
ioeModifyErrorString :: (String -> String) -> IOError -> IOError
ioeModifyErrorString = over ioeErrorString

-- | A lens for the error message inside an 'IOError'.
ioeErrorString :: Lens' IOError String
ioeErrorString f ioe = ioeSetErrorString ioe <$> f (ioeGetErrorString ioe)

{-# NOINLINE topHandlerWith #-}
topHandlerWith :: forall a. (Exception.SomeException -> IO a) -> IO a -> IO a
topHandlerWith cont prog = do
  -- By default, stderr to a terminal device is NoBuffering. But this
  -- is *really slow*
  hSetBuffering stderr LineBuffering
  Exception.catches
    prog
    [ Exception.Handler rethrowAsyncExceptions
    , Exception.Handler rethrowExitStatus
    , Exception.Handler handle
    ]
  where
    -- Let async exceptions rise to the top for the default top-handler
    rethrowAsyncExceptions :: Exception.AsyncException -> IO a
    rethrowAsyncExceptions a = throwIO a

    -- ExitCode gets thrown asynchronously too, and we don't want to print it
    rethrowExitStatus :: ExitCode -> IO a
    rethrowExitStatus = throwIO

    -- Print all other exceptions
    handle :: Exception.SomeException -> IO a
    handle se = do
      hFlush stdout
      pname <- getProgName
      hPutStr stderr (message pname se)
      cont se

    message :: String -> Exception.SomeException -> String
    message pname (Exception.SomeException se) =
      case cast se :: Maybe Exception.IOException of
        Just ioe
          | ioeGetVerbatim ioe ->
              -- Use the message verbatim
              ioeGetErrorString ioe ++ "\n"
          | isUserError ioe ->
              let file = case ioeGetFileName ioe of
                    Nothing -> ""
                    Just path -> path ++ location ++ ": "
                  location = case ioeGetLocation ioe of
                    l@(n : _) | isDigit n -> ':' : l
                    _ -> ""
                  detail = ioeGetErrorString ioe
               in wrapText $ addErrorPrefix $ pname ++ ": " ++ file ++ detail
        _ ->
          displaySomeException se ++ "\n"

-- | BC wrapper around 'Exception.displayException'.
displaySomeException :: Exception.Exception e => e -> String
displaySomeException se = Exception.displayException se

topHandler :: IO a -> IO a
topHandler prog = topHandlerWith (const $ exitWith (ExitFailure 1)) prog

-- | Depending on 'isVerboseStderr', set the output handle to 'stderr' or 'stdout'.
verbosityHandle :: Verbosity -> Handle
verbosityHandle verbosity
  | isVerboseStderr verbosity = stderr
  | otherwise = stdout

-- | Non fatal conditions that may be indicative of an error or problem.
--
-- We display these at the 'normal' verbosity level.
warn :: Verbosity -> String -> IO ()
warn verbosity msg = warnMessage "Warning" verbosity msg

-- | Like 'warn', but prepend @Error: …@ instead of @Waring: …@ before the
-- the message. Useful when you want to highlight the condition is an error
-- but do not want to quit the program yet.
warnError :: Verbosity -> String -> IO ()
warnError verbosity message = warnMessage "Error" verbosity message

-- | Warning message, with a custom label.
warnMessage :: String -> Verbosity -> String -> IO ()
warnMessage l verbosity msg = withFrozenCallStack $ do
  when ((verbosity >= normal) && not (isVerboseNoWarn verbosity)) $ do
    ts <- getPOSIXTime
    hFlush stdout
    hPutStr stderr
      . withMetadata ts NormalMark FlagTrace verbosity
      . wrapTextVerbosity verbosity
      $ l ++ ": " ++ msg

-- | Useful status messages.
--
-- We display these at the 'normal' verbosity level.
--
-- This is for the ordinary helpful status messages that users see. Just
-- enough information to know that things are working but not floods of detail.
notice :: Verbosity -> String -> IO ()
notice verbosity msg = withFrozenCallStack $ do
  when (verbosity >= normal) $ do
    let h = verbosityHandle verbosity
    ts <- getPOSIXTime
    hPutStr h $
      withMetadata ts NormalMark FlagTrace verbosity $
        wrapTextVerbosity verbosity $
          msg

-- | Display a message at 'normal' verbosity level, but without
-- wrapping.
noticeNoWrap :: Verbosity -> String -> IO ()
noticeNoWrap verbosity msg = withFrozenCallStack $ do
  when (verbosity >= normal) $ do
    let h = verbosityHandle verbosity
    ts <- getPOSIXTime
    hPutStr h . withMetadata ts NormalMark FlagTrace verbosity $ msg

-- | Pretty-print a 'Disp.Doc' status message at 'normal' verbosity
-- level.  Use this if you need fancy formatting.
noticeDoc :: Verbosity -> Disp.Doc -> IO ()
noticeDoc verbosity msg = withFrozenCallStack $ do
  when (verbosity >= normal) $ do
    let h = verbosityHandle verbosity
    ts <- getPOSIXTime
    hPutStr h $
      withMetadata ts NormalMark FlagTrace verbosity $
        Disp.renderStyle defaultStyle $
          msg

-- | Display a "setup status message".  Prefer using setupMessage'
-- if possible.
setupMessage :: Verbosity -> String -> PackageIdentifier -> IO ()
setupMessage verbosity msg pkgid = withFrozenCallStack $ do
  noticeNoWrap verbosity (msg ++ ' ' : prettyShow pkgid ++ "...")

-- | More detail on the operation of some action.
--
-- We display these messages when the verbosity level is 'verbose'
info :: Verbosity -> String -> IO ()
info verbosity msg = withFrozenCallStack $
  when (verbosity >= verbose) $ do
    let h = verbosityHandle verbosity
    ts <- getPOSIXTime
    hPutStr h $
      withMetadata ts NeverMark FlagTrace verbosity $
        wrapTextVerbosity verbosity $
          msg

infoNoWrap :: Verbosity -> String -> IO ()
infoNoWrap verbosity msg = withFrozenCallStack $
  when (verbosity >= verbose) $ do
    let h = verbosityHandle verbosity
    ts <- getPOSIXTime
    hPutStr h $
      withMetadata ts NeverMark FlagTrace verbosity $
        msg

-- | Detailed internal debugging information
--
-- We display these messages when the verbosity level is 'deafening'
debug :: Verbosity -> String -> IO ()
debug verbosity msg = withFrozenCallStack $
  when (verbosity >= deafening) $ do
    let h = verbosityHandle verbosity
    ts <- getPOSIXTime
    hPutStr h $
      withMetadata ts NeverMark FlagTrace verbosity $
        wrapTextVerbosity verbosity $
          msg
    -- ensure that we don't lose output if we segfault/infinite loop
    hFlush stdout

-- | A variant of 'debug' that doesn't perform the automatic line
-- wrapping. Produces better output in some cases.
debugNoWrap :: Verbosity -> String -> IO ()
debugNoWrap verbosity msg = withFrozenCallStack $
  when (verbosity >= deafening) $ do
    let h = verbosityHandle verbosity
    ts <- getPOSIXTime
    hPutStr h $
      withMetadata ts NeverMark FlagTrace verbosity $
        msg
    -- ensure that we don't lose output if we segfault/infinite loop
    hFlush stdout

-- | Perform an IO action, catching any IO exceptions and printing an error
--   if one occurs.
chattyTry
  :: String
  -- ^ a description of the action we were attempting
  -> IO ()
  -- ^ the action itself
  -> IO ()
chattyTry desc action =
  catchIO action $ \exception ->
    hPutStrLn stderr $ "Error while " ++ desc ++ ": " ++ show exception

-- | Run an IO computation, returning @e@ if it raises a "file
-- does not exist" error.
handleDoesNotExist :: a -> IO a -> IO a
handleDoesNotExist e =
  Exception.handleJust
    (\ioe -> if isDoesNotExistError ioe then Just ioe else Nothing)
    (\_ -> return e)

-- -----------------------------------------------------------------------------
-- Helper functions

-- | Wraps text unless the @+nowrap@ verbosity flag is active
wrapTextVerbosity :: Verbosity -> String -> String
wrapTextVerbosity verb
  | isVerboseNoWrap verb = withTrailingNewline
  | otherwise = withTrailingNewline . wrapText

-- | Prepends a timestamp if @+timestamp@ verbosity flag is set
--
-- This is used by 'withMetadata'
withTimestamp :: Verbosity -> POSIXTime -> String -> String
withTimestamp v ts msg
  | isVerboseTimestamp v = msg'
  | otherwise = msg -- no-op
  where
    msg' = case lines msg of
      [] -> tsstr "\n"
      l1 : rest -> unlines (tsstr (' ' : l1) : map (contpfx ++) rest)

    -- format timestamp to be prepended to first line with msec precision
    tsstr = showFFloat (Just 3) (realToFrac ts :: Double)

    -- continuation prefix for subsequent lines of msg
    contpfx = replicate (length (tsstr " ")) ' '

-- | Wrap output with a marker if @+markoutput@ verbosity flag is set.
--
-- NB: Why is markoutput done with start/end markers, and not prefixes?
-- Markers are more convenient to add (if we want to add prefixes,
-- we have to 'lines' and then 'map'; here's it's just some
-- concatenates).  Note that even in the prefix case, we can't
-- guarantee that the markers are unambiguous, because some of
-- Cabal's output comes straight from external programs, where
-- we don't have the ability to interpose on the output.
--
-- This is used by 'withMetadata'
withOutputMarker :: Verbosity -> String -> String
withOutputMarker v xs | not (isVerboseMarkOutput v) = xs
withOutputMarker _ "" = "" -- Minor optimization, don't mark uselessly
withOutputMarker _ xs =
  "-----BEGIN CABAL OUTPUT-----\n"
    ++ withTrailingNewline xs
    ++ "-----END CABAL OUTPUT-----\n"

-- | Append a trailing newline to a string if it does not
-- already have a trailing newline.
withTrailingNewline :: String -> String
withTrailingNewline "" = ""
withTrailingNewline (x : xs) = x : go x xs
  where
    go _ (c : cs) = c : go c cs
    go '\n' "" = ""
    go _ "" = "\n"

-- | Prepend a call-site and/or call-stack based on Verbosity
withCallStackPrefix :: WithCallStack (TraceWhen -> Verbosity -> String -> String)
withCallStackPrefix tracer verbosity s =
  withFrozenCallStack $
    ( if isVerboseCallSite verbosity
        then
          parentSrcLocPrefix
            ++
            -- Hack: need a newline before starting output marker :(
            if isVerboseMarkOutput verbosity
              then "\n"
              else ""
        else ""
    )
      ++ ( case traceWhen verbosity tracer of
            Just pre -> pre ++ prettyCallStack callStack ++ "\n"
            Nothing -> ""
         )
      ++ s

-- | When should we emit the call stack?  We always emit
-- for internal errors, emit the trace for errors when we
-- are in verbose mode, and otherwise only emit it if
-- explicitly asked for using the @+callstack@ verbosity
-- flag.  (At the moment, 'AlwaysTrace' is not used.
data TraceWhen
  = AlwaysTrace
  | VerboseTrace
  | FlagTrace
  deriving (Eq)

-- | Determine if we should emit a call stack.
-- If we trace, it also emits any prefix we should append.
traceWhen :: Verbosity -> TraceWhen -> Maybe String
traceWhen _ AlwaysTrace = Just ""
traceWhen v VerboseTrace | v >= verbose = Just ""
traceWhen v FlagTrace | isVerboseCallStack v = Just "----\n"
traceWhen _ _ = Nothing

-- | When should we output the marker?  Things like 'die'
-- always get marked, but a 'NormalMark' will only be
-- output if we're not a quiet verbosity.
data MarkWhen = AlwaysMark | NormalMark | NeverMark

-- | Add all necessary metadata to a logging message
withMetadata :: WithCallStack (POSIXTime -> MarkWhen -> TraceWhen -> Verbosity -> String -> String)
withMetadata ts marker tracer verbosity x =
  withFrozenCallStack
    $
    -- NB: order matters.  Output marker first because we
    -- don't want to capture call stacks.
    withTrailingNewline
      . withCallStackPrefix tracer verbosity
      . ( case marker of
            AlwaysMark -> withOutputMarker verbosity
            NormalMark
              | not (isVerboseQuiet verbosity) ->
                  withOutputMarker verbosity
              | otherwise ->
                  id
            NeverMark -> id
        )
      -- Clear out any existing markers
      . clearMarkers
      . withTimestamp verbosity ts
    $ x

-- | Add all necessary metadata to a logging message
exceptionWithMetadata :: CallStack -> POSIXTime -> Verbosity -> String -> String
exceptionWithMetadata stack ts verbosity x =
  withTrailingNewline
    . exceptionWithCallStackPrefix stack verbosity
    . withOutputMarker verbosity
    . clearMarkers
    . withTimestamp verbosity ts
    $ x

clearMarkers :: String -> String
clearMarkers s = unlines . filter isMarker $ lines s
  where
    isMarker "-----BEGIN CABAL OUTPUT-----" = False
    isMarker "-----END CABAL OUTPUT-----" = False
    isMarker _ = True

-- | Append a call-site and/or call-stack based on Verbosity
exceptionWithCallStackPrefix :: CallStack -> Verbosity -> String -> String
exceptionWithCallStackPrefix stack verbosity s =
  s
    ++ withFrozenCallStack
      ( ( if isVerboseCallSite verbosity
            then
              parentSrcLocPrefix
                ++
                -- Hack: need a newline before starting output marker :(
                if isVerboseMarkOutput verbosity
                  then "\n"
                  else ""
            else ""
        )
          ++ ( if verbosity >= verbose
                then prettyCallStack stack ++ "\n"
                else ""
             )
      )

-- -----------------------------------------------------------------------------
-- rawSystem variants
--
-- These all use 'Distribution.Compat.Process.proc' to ensure we
-- consistently use process jobs on Windows and Ctrl-C delegation
-- on Unix.
--
-- Additionally, they take care of logging command execution.
--

-- | Helper to use with one of the 'rawSystem' variants, and exit
-- unless the command completes successfully.
maybeExit :: IO ExitCode -> IO ()
maybeExit cmd = do
  exitcode <- cmd
  unless (exitcode == ExitSuccess) $ exitWith exitcode

-- | Log a command execution (that's typically about to happen)
-- at info level, and log working directory and environment overrides
-- at debug level if specified.
logCommand :: Verbosity -> Process.CreateProcess -> IO ()
logCommand verbosity cp = do
  infoNoWrap verbosity $
    "Running: " <> case Process.cmdspec cp of
      Process.ShellCommand sh -> sh
      Process.RawCommand path args -> Process.showCommandForUser path args
  case Process.env cp of
    Just env -> debugNoWrap verbosity $ "with environment: " ++ show env
    Nothing -> return ()
  case Process.cwd cp of
    Just cwd -> debugNoWrap verbosity $ "with working directory: " ++ show cwd
    Nothing -> return ()
  hFlush stdout

-- | Execute the given command with the given arguments, exiting
-- with the same exit code if the command fails.
rawSystemExit :: Verbosity -> FilePath -> [String] -> IO ()
rawSystemExit verbosity path args =
  withFrozenCallStack $
    maybeExit $
      rawSystemExitCode verbosity path args

-- | Execute the given command with the given arguments, returning
-- the command's exit code.
rawSystemExitCode :: Verbosity -> FilePath -> [String] -> IO ExitCode
rawSystemExitCode verbosity path args =
  withFrozenCallStack $
    rawSystemProc verbosity $
      proc path args

-- | Execute the given command with the given arguments, returning
-- the command's exit code.
--
-- Create the process argument with 'Distribution.Compat.Process.proc'
-- to ensure consistent options with other 'rawSystem' functions in this
-- module.
rawSystemProc :: Verbosity -> Process.CreateProcess -> IO ExitCode
rawSystemProc verbosity cp = withFrozenCallStack $ do
  (exitcode, _) <- rawSystemProcAction verbosity cp $ \_ _ _ -> return ()
  return exitcode

-- | Execute the given command with the given arguments, returning
-- the command's exit code. 'action' is executed while the command
-- is running, and would typically be used to communicate with the
-- process through pipes.
--
-- Create the process argument with 'Distribution.Compat.Process.proc'
-- to ensure consistent options with other 'rawSystem' functions in this
-- module.
rawSystemProcAction
  :: Verbosity
  -> Process.CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> IO a)
  -> IO (ExitCode, a)
rawSystemProcAction verbosity cp action = withFrozenCallStack $ do
  logCommand verbosity cp
  (exitcode, a) <- Process.withCreateProcess cp $ \mStdin mStdout mStderr p -> do
    a <- action mStdin mStdout mStderr
    exitcode <- Process.waitForProcess p
    return (exitcode, a)
  unless (exitcode == ExitSuccess) $ do
    let cmd = case Process.cmdspec cp of
          Process.ShellCommand sh -> sh
          Process.RawCommand path _args -> path
    debug verbosity $ cmd ++ " returned " ++ show exitcode
  return (exitcode, a)

-- | fromJust for dealing with 'Maybe Handle' values as obtained via
-- 'System.Process.CreatePipe'. Creating a pipe using 'CreatePipe' guarantees
-- a 'Just' value for the corresponding handle.
fromCreatePipe :: Maybe Handle -> Handle
fromCreatePipe = maybe (error "fromCreatePipe: Nothing") id

-- | Execute the given command with the given arguments and
-- environment, exiting with the same exit code if the command fails.
rawSystemExitWithEnv
  :: Verbosity
  -> FilePath
  -> [String]
  -> [(String, String)]
  -> IO ()
rawSystemExitWithEnv verbosity path args env =
  withFrozenCallStack $
    maybeExit $
      rawSystemProc verbosity $
        (proc path args)
          { Process.env = Just env
          }

-- | Execute the given command with the given arguments, returning
-- the command's exit code.
--
-- Optional arguments allow setting working directory, environment
-- and input and output handles.
rawSystemIOWithEnv
  :: Verbosity
  -> FilePath
  -> [String]
  -> Maybe FilePath
  -- ^ New working dir or inherit
  -> Maybe [(String, String)]
  -- ^ New environment or inherit
  -> Maybe Handle
  -- ^ stdin
  -> Maybe Handle
  -- ^ stdout
  -> Maybe Handle
  -- ^ stderr
  -> IO ExitCode
rawSystemIOWithEnv verbosity path args mcwd menv inp out err = withFrozenCallStack $ do
  (exitcode, _) <-
    rawSystemIOWithEnvAndAction
      verbosity
      path
      args
      mcwd
      menv
      action
      inp
      out
      err
  return exitcode
  where
    action = return ()

-- | Execute the given command with the given arguments, returning
-- the command's exit code. 'action' is executed while the command
-- is running, and would typically be used to communicate with the
-- process through pipes.
--
-- Optional arguments allow setting working directory, environment
-- and input and output handles.
rawSystemIOWithEnvAndAction
  :: Verbosity
  -> FilePath
  -> [String]
  -> Maybe FilePath
  -- ^ New working dir or inherit
  -> Maybe [(String, String)]
  -- ^ New environment or inherit
  -> IO a
  -- ^ action to perform after process is created, but before 'waitForProcess'.
  -> Maybe Handle
  -- ^ stdin
  -> Maybe Handle
  -- ^ stdout
  -> Maybe Handle
  -- ^ stderr
  -> IO (ExitCode, a)
rawSystemIOWithEnvAndAction verbosity path args mcwd menv action inp out err = withFrozenCallStack $ do
  let cp =
        (proc path args)
          { Process.cwd = mcwd
          , Process.env = menv
          , Process.std_in = mbToStd inp
          , Process.std_out = mbToStd out
          , Process.std_err = mbToStd err
          }
  rawSystemProcAction verbosity cp (\_ _ _ -> action)
  where
    mbToStd :: Maybe Handle -> Process.StdStream
    mbToStd = maybe Process.Inherit Process.UseHandle

-- | Execute the given command with the given arguments, returning
-- the command's output. Exits if the command exits with error.
--
-- Provides control over the binary/text mode of the output.
rawSystemStdout :: forall mode. KnownIODataMode mode => Verbosity -> FilePath -> [String] -> IO mode
rawSystemStdout verbosity path args = withFrozenCallStack $ do
  (output, errors, exitCode) <-
    rawSystemStdInOut
      verbosity
      path
      args
      Nothing
      Nothing
      Nothing
      (IOData.iodataMode :: IODataMode mode)
  when (exitCode /= ExitSuccess) $
    dieWithException verbosity $
      RawSystemStdout errors
  return output

-- | Execute the given command with the given arguments, returning
-- the command's output, errors and exit code.
--
-- Optional arguments allow setting working directory, environment
-- and command input.
--
-- Provides control over the binary/text mode of the input and output.
rawSystemStdInOut
  :: KnownIODataMode mode
  => Verbosity
  -> FilePath
  -- ^ Program location
  -> [String]
  -- ^ Arguments
  -> Maybe FilePath
  -- ^ New working dir or inherit
  -> Maybe [(String, String)]
  -- ^ New environment or inherit
  -> Maybe IOData
  -- ^ input text and binary mode
  -> IODataMode mode
  -- ^ iodata mode, acts as proxy
  -> IO (mode, String, ExitCode)
  -- ^ output, errors, exit
rawSystemStdInOut verbosity path args mcwd menv input _ = withFrozenCallStack $ do
  let cp =
        (proc path args)
          { Process.cwd = mcwd
          , Process.env = menv
          , Process.std_in = Process.CreatePipe
          , Process.std_out = Process.CreatePipe
          , Process.std_err = Process.CreatePipe
          }

  (exitcode, (mberr1, mberr2)) <- rawSystemProcAction verbosity cp $ \mb_in mb_out mb_err -> do
    let (inh, outh, errh) = (fromCreatePipe mb_in, fromCreatePipe mb_out, fromCreatePipe mb_err)
    flip Exception.finally (hClose inh >> hClose outh >> hClose errh) $ do
      -- output mode depends on what the caller wants
      -- but the errors are always assumed to be text (in the current locale)
      hSetBinaryMode errh False

      -- fork off a couple threads to pull on the stderr and stdout
      -- so if the process writes to stderr we do not block.

      withAsyncNF (hGetContents errh) $ \errA -> withAsyncNF (IOData.hGetIODataContents outh) $ \outA -> do
        -- push all the input, if any
        ignoreSigPipe $ case input of
          Nothing -> hClose inh
          Just inputData -> IOData.hPutContents inh inputData

        -- wait for both to finish
        mberr1 <- waitCatch outA
        mberr2 <- waitCatch errA
        return (mberr1, mberr2)

  -- get the stderr, so it can be added to error message
  err <- reportOutputIOError mberr2

  unless (exitcode == ExitSuccess) $
    debug verbosity $
      path
        ++ " returned "
        ++ show exitcode
        ++ if null err
          then ""
          else
            " with error message:\n"
              ++ err
              ++ case input of
                Nothing -> ""
                Just d | IOData.null d -> ""
                Just (IODataText inp) -> "\nstdin input:\n" ++ inp
                Just (IODataBinary inp) -> "\nstdin input (binary):\n" ++ show inp

  -- Check if we hit an exception while consuming the output
  -- (e.g. a text decoding error)
  out <- reportOutputIOError mberr1

  return (out, err, exitcode)
  where
    reportOutputIOError :: Either Exception.SomeException a -> IO a
    reportOutputIOError (Right x) = return x
    reportOutputIOError (Left exc) = case fromException exc of
      Just ioe -> throwIO (ioeSetFileName ioe ("output of " ++ path))
      Nothing -> throwIO exc

-- | Ignore SIGPIPE in a subcomputation.
ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = Exception.handle $ \case
  GHC.IOError{GHC.ioe_type = GHC.ResourceVanished, GHC.ioe_errno = Just ioe}
    | Errno ioe == ePIPE -> return ()
  e -> throwIO e

-- | Look for a program and try to find it's version number. It can accept
-- either an absolute path or the name of a program binary, in which case we
-- will look for the program on the path.
findProgramVersion
  :: String
  -- ^ version args
  -> (String -> String)
  -- ^ function to select version
  --   number from program output
  -> Verbosity
  -> FilePath
  -- ^ location
  -> IO (Maybe Version)
findProgramVersion versionArg selectVersion verbosity path = withFrozenCallStack $ do
  str <-
    rawSystemStdout verbosity path [versionArg]
      `catchIO` (\_ -> return "")
      `catch` (\(_ :: VerboseException CabalException) -> return "")
      `catchExit` (\_ -> return "")
  let version :: Maybe Version
      version = simpleParsec (selectVersion str)
  case version of
    Nothing ->
      warn verbosity $
        "cannot determine version of "
          ++ path
          ++ " :\n"
          ++ show str
    Just v -> debug verbosity $ path ++ " is version " ++ prettyShow v
  return version

-- | Like the Unix xargs program. Useful for when we've got very long command
-- lines that might overflow an OS limit on command line length and so you
-- need to invoke a command multiple times to get all the args in.
--
-- Use it with either of the rawSystem variants above. For example:
--
-- > xargs (32*1024) (rawSystemExit verbosity) prog fixedArgs bigArgs
xargs
  :: Int
  -> ([String] -> IO ())
  -> [String]
  -> [String]
  -> IO ()
xargs maxSize rawSystemFun fixedArgs bigArgs =
  let fixedArgSize = sum (map length fixedArgs) + length fixedArgs
      chunkSize = maxSize - fixedArgSize
   in traverse_ (rawSystemFun . (fixedArgs ++)) (chunks chunkSize bigArgs)
  where
    chunks len = unfoldr $ \s ->
      if null s
        then Nothing
        else Just (chunk [] len s)

    chunk acc _ [] = (reverse acc, [])
    chunk acc len (s : ss)
      | len' < len = chunk (s : acc) (len - len' - 1) ss
      | otherwise = (reverse acc, s : ss)
      where
        len' = length s

-- ------------------------------------------------------------

-- * File Utilities

-- ------------------------------------------------------------

----------------
-- Finding files

-- | Find a file by looking in a search path. The file path must match exactly.
--
-- @since 3.4.0.0
findFileCwd
  :: Verbosity
  -> FilePath
  -- ^ cwd
  -> [FilePath]
  -- ^ relative search location
  -> FilePath
  -- ^ File Name
  -> IO FilePath
findFileCwd verbosity cwd searchPath fileName =
  findFirstFile
    (cwd </>)
    [ path </> fileName
    | path <- nub searchPath
    ]
    >>= maybe (dieWithException verbosity $ FindFileCwd fileName) return

-- | Find a file by looking in a search path. The file path must match exactly.
findFileEx
  :: Verbosity
  -> [FilePath]
  -- ^ search locations
  -> FilePath
  -- ^ File Name
  -> IO FilePath
findFileEx verbosity searchPath fileName =
  findFirstFile
    id
    [ path </> fileName
    | path <- nub searchPath
    ]
    >>= maybe (dieWithException verbosity $ FindFileEx fileName) return

-- | Find a file by looking in a search path with one of a list of possible
-- file extensions. The file base name should be given and it will be tried
-- with each of the extensions in each element of the search path.
findFileWithExtension
  :: [String]
  -> [FilePath]
  -> FilePath
  -> IO (Maybe FilePath)
findFileWithExtension extensions searchPath baseName =
  findFirstFile
    id
    [ path </> baseName <.> ext
    | path <- nub searchPath
    , ext <- nub extensions
    ]

-- | @since 3.4.0.0
findFileCwdWithExtension
  :: FilePath
  -> [String]
  -> [FilePath]
  -> FilePath
  -> IO (Maybe FilePath)
findFileCwdWithExtension cwd extensions searchPath baseName =
  findFirstFile
    (cwd </>)
    [ path </> baseName <.> ext
    | path <- nub searchPath
    , ext <- nub extensions
    ]

-- | @since 3.4.0.0
findAllFilesCwdWithExtension
  :: FilePath
  -- ^ cwd
  -> [String]
  -- ^ extensions
  -> [FilePath]
  -- ^ relative search locations
  -> FilePath
  -- ^ basename
  -> IO [FilePath]
findAllFilesCwdWithExtension cwd extensions searchPath basename =
  findAllFiles
    (cwd </>)
    [ path </> basename <.> ext
    | path <- nub searchPath
    , ext <- nub extensions
    ]

findAllFilesWithExtension
  :: [String]
  -> [FilePath]
  -> FilePath
  -> IO [FilePath]
findAllFilesWithExtension extensions searchPath basename =
  findAllFiles
    id
    [ path </> basename <.> ext
    | path <- nub searchPath
    , ext <- nub extensions
    ]

-- | Like 'findFileWithExtension' but returns which element of the search path
-- the file was found in, and the file path relative to that base directory.
findFileWithExtension'
  :: [String]
  -> [FilePath]
  -> FilePath
  -> IO (Maybe (FilePath, FilePath))
findFileWithExtension' extensions searchPath baseName =
  findFirstFile
    (uncurry (</>))
    [ (path, baseName <.> ext)
    | path <- nub searchPath
    , ext <- nub extensions
    ]

findFirstFile :: (a -> FilePath) -> [a] -> IO (Maybe a)
findFirstFile file = findFirst
  where
    findFirst [] = return Nothing
    findFirst (x : xs) = do
      exists <- doesFileExist (file x)
      if exists
        then return (Just x)
        else findFirst xs

findAllFiles :: (a -> FilePath) -> [a] -> IO [a]
findAllFiles file = filterM (doesFileExist . file)

-- | Finds the files corresponding to a list of Haskell module names.
--
-- As 'findModuleFile' but for a list of module names.
findModuleFilesEx
  :: Verbosity
  -> [FilePath]
  -- ^ build prefix (location of objects)
  -> [String]
  -- ^ search suffixes
  -> [ModuleName]
  -- ^ modules
  -> IO [(FilePath, FilePath)]
findModuleFilesEx verbosity searchPath extensions moduleNames =
  traverse (findModuleFileEx verbosity searchPath extensions) moduleNames

-- | Find the file corresponding to a Haskell module name.
--
-- This is similar to 'findFileWithExtension'' but specialised to a module
-- name. The function fails if the file corresponding to the module is missing.
findModuleFileEx
  :: Verbosity
  -> [FilePath]
  -- ^ build prefix (location of objects)
  -> [String]
  -- ^ search suffixes
  -> ModuleName
  -- ^ module
  -> IO (FilePath, FilePath)
findModuleFileEx verbosity searchPath extensions mod_name =
  maybe notFound return
    =<< findFileWithExtension'
      extensions
      searchPath
      (ModuleName.toFilePath mod_name)
  where
    notFound =
      dieWithException verbosity $ FindModuleFileEx mod_name extensions searchPath

-- | List all the files in a directory and all subdirectories.
--
-- The order places files in sub-directories after all the files in their
-- parent directories. The list is generated lazily so is not well defined if
-- the source directory structure changes before the list is used.
getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive topdir = recurseDirectories [""]
  where
    recurseDirectories :: [FilePath] -> IO [FilePath]
    recurseDirectories [] = return []
    recurseDirectories (dir : dirs) = unsafeInterleaveIO $ do
      (files, dirs') <- collect [] [] =<< getDirectoryContents (topdir </> dir)
      files' <- recurseDirectories (dirs' ++ dirs)
      return (files ++ files')
      where
        collect files dirs' [] =
          return
            ( reverse files
            , reverse dirs'
            )
        collect files dirs' (entry : entries)
          | ignore entry =
              collect files dirs' entries
        collect files dirs' (entry : entries) = do
          let dirEntry = dir </> entry
          isDirectory <- doesDirectoryExist (topdir </> dirEntry)
          if isDirectory
            then collect files (dirEntry : dirs') entries
            else collect (dirEntry : files) dirs' entries

        ignore ['.'] = True
        ignore ['.', '.'] = True
        ignore _ = False

------------------------
-- Environment variables

-- | Is this directory in the system search path?
isInSearchPath :: FilePath -> IO Bool
isInSearchPath path = fmap (elem path) getSearchPath

addLibraryPath
  :: OS
  -> [FilePath]
  -> [(String, String)]
  -> [(String, String)]
addLibraryPath os paths = addEnv
  where
    pathsString = intercalate [searchPathSeparator] paths
    ldPath = case os of
      OSX -> "DYLD_LIBRARY_PATH"
      _ -> "LD_LIBRARY_PATH"

    addEnv [] = [(ldPath, pathsString)]
    addEnv ((key, value) : xs)
      | key == ldPath =
          if null value
            then (key, pathsString) : xs
            else (key, value ++ (searchPathSeparator : pathsString)) : xs
      | otherwise = (key, value) : addEnv xs

--------------------
-- Modification time

-- | Compare the modification times of two files to see if the first is newer
-- than the second. The first file must exist but the second need not.
-- The expected use case is when the second file is generated using the first.
-- In this use case, if the result is True then the second file is out of date.
moreRecentFile :: FilePath -> FilePath -> IO Bool
moreRecentFile a b = do
  exists <- doesFileExist b
  if not exists
    then return True
    else do
      tb <- getModificationTime b
      ta <- getModificationTime a
      return (ta > tb)

-- | Like 'moreRecentFile', but also checks that the first file exists.
existsAndIsMoreRecentThan :: FilePath -> FilePath -> IO Bool
existsAndIsMoreRecentThan a b = do
  exists <- doesFileExist a
  if not exists
    then return False
    else a `moreRecentFile` b

----------------------------------------
-- Copying and installing files and dirs

-- | Same as 'createDirectoryIfMissing' but logs at higher verbosity levels.
createDirectoryIfMissingVerbose
  :: Verbosity
  -> Bool
  -- ^ Create its parents too?
  -> FilePath
  -> IO ()
createDirectoryIfMissingVerbose verbosity create_parents path0
  | create_parents = withFrozenCallStack $ createDirs (parents path0)
  | otherwise = withFrozenCallStack $ createDirs (take 1 (parents path0))
  where
    parents = reverse . scanl1 (</>) . splitDirectories . normalise

    createDirs [] = return ()
    createDirs (dir : []) = createDir dir throwIO
    createDirs (dir : dirs) =
      createDir dir $ \_ -> do
        createDirs dirs
        createDir dir throwIO

    createDir :: FilePath -> (IOException -> IO ()) -> IO ()
    createDir dir notExistHandler = do
      r <- tryIO $ createDirectoryVerbose verbosity dir
      case (r :: Either IOException ()) of
        Right () -> return ()
        Left e
          | isDoesNotExistError e -> notExistHandler e
          -- createDirectory (and indeed POSIX mkdir) does not distinguish
          -- between a dir already existing and a file already existing. So we
          -- check for it here. Unfortunately there is a slight race condition
          -- here, but we think it is benign. It could report an exception in
          -- the case that the dir did exist but another process deletes the
          -- directory and creates a file in its place before we can check
          -- that the directory did indeed exist.
          | isAlreadyExistsError e ->
              ( do
                  isDir <- doesDirectoryExist dir
                  unless isDir $ throwIO e
              )
                `catchIO` ((\_ -> return ()) :: IOException -> IO ())
          | otherwise -> throwIO e

createDirectoryVerbose :: Verbosity -> FilePath -> IO ()
createDirectoryVerbose verbosity dir = withFrozenCallStack $ do
  info verbosity $ "creating " ++ dir
  createDirectory dir
  setDirOrdinary dir

-- | Copies a file without copying file permissions. The target file is created
-- with default permissions. Any existing target file is replaced.
--
-- At higher verbosity levels it logs an info message.
copyFileVerbose :: Verbosity -> FilePath -> FilePath -> IO ()
copyFileVerbose verbosity src dest = withFrozenCallStack $ do
  info verbosity ("copy " ++ src ++ " to " ++ dest)
  copyFile src dest

-- | Install an ordinary file. This is like a file copy but the permissions
-- are set appropriately for an installed file. On Unix it is \"-rw-r--r--\"
-- while on Windows it uses the default permissions for the target directory.
installOrdinaryFile :: Verbosity -> FilePath -> FilePath -> IO ()
installOrdinaryFile verbosity src dest = withFrozenCallStack $ do
  info verbosity ("Installing " ++ src ++ " to " ++ dest)
  copyOrdinaryFile src dest

-- | Install an executable file. This is like a file copy but the permissions
-- are set appropriately for an installed file. On Unix it is \"-rwxr-xr-x\"
-- while on Windows it uses the default permissions for the target directory.
installExecutableFile :: Verbosity -> FilePath -> FilePath -> IO ()
installExecutableFile verbosity src dest = withFrozenCallStack $ do
  info verbosity ("Installing executable " ++ src ++ " to " ++ dest)
  copyExecutableFile src dest

-- | Install a file that may or not be executable, preserving permissions.
installMaybeExecutableFile :: Verbosity -> FilePath -> FilePath -> IO ()
installMaybeExecutableFile verbosity src dest = withFrozenCallStack $ do
  perms <- getPermissions src
  if (executable perms) -- only checks user x bit
    then installExecutableFile verbosity src dest
    else installOrdinaryFile verbosity src dest

-- | Given a relative path to a file, copy it to the given directory, preserving
-- the relative path and creating the parent directories if needed.
copyFileTo :: Verbosity -> FilePath -> FilePath -> IO ()
copyFileTo verbosity dir file = withFrozenCallStack $ do
  let targetFile = dir </> file
  createDirectoryIfMissingVerbose verbosity True (takeDirectory targetFile)
  installOrdinaryFile verbosity file targetFile

-- | Common implementation of 'copyFiles', 'installOrdinaryFiles',
-- 'installExecutableFiles' and 'installMaybeExecutableFiles'.
copyFilesWith
  :: (Verbosity -> FilePath -> FilePath -> IO ())
  -> Verbosity
  -> FilePath
  -> [(FilePath, FilePath)]
  -> IO ()
copyFilesWith doCopy verbosity targetDir srcFiles = withFrozenCallStack $ do
  -- Create parent directories for everything
  let dirs = map (targetDir </>) . nub . map (takeDirectory . snd) $ srcFiles
  traverse_ (createDirectoryIfMissingVerbose verbosity True) dirs

  -- Copy all the files
  sequence_
    [ let src = srcBase </> srcFile
          dest = targetDir </> srcFile
       in doCopy verbosity src dest
    | (srcBase, srcFile) <- srcFiles
    ]

-- | Copies a bunch of files to a target directory, preserving the directory
-- structure in the target location. The target directories are created if they
-- do not exist.
--
-- The files are identified by a pair of base directory and a path relative to
-- that base. It is only the relative part that is preserved in the
-- destination.
--
-- For example:
--
-- > copyFiles normal "dist/src"
-- >    [("", "src/Foo.hs"), ("dist/build/", "src/Bar.hs")]
--
-- This would copy \"src\/Foo.hs\" to \"dist\/src\/src\/Foo.hs\" and
-- copy \"dist\/build\/src\/Bar.hs\" to \"dist\/src\/src\/Bar.hs\".
--
-- This operation is not atomic. Any IO failure during the copy (including any
-- missing source files) leaves the target in an unknown state so it is best to
-- use it with a freshly created directory so that it can be simply deleted if
-- anything goes wrong.
copyFiles :: Verbosity -> FilePath -> [(FilePath, FilePath)] -> IO ()
copyFiles v fp fs = withFrozenCallStack (copyFilesWith copyFileVerbose v fp fs)

-- | This is like 'copyFiles' but uses 'installOrdinaryFile'.
installOrdinaryFiles :: Verbosity -> FilePath -> [(FilePath, FilePath)] -> IO ()
installOrdinaryFiles v fp fs = withFrozenCallStack (copyFilesWith installOrdinaryFile v fp fs)

-- | This is like 'copyFiles' but uses 'installExecutableFile'.
installExecutableFiles
  :: Verbosity
  -> FilePath
  -> [(FilePath, FilePath)]
  -> IO ()
installExecutableFiles v fp fs = withFrozenCallStack (copyFilesWith installExecutableFile v fp fs)

-- | This is like 'copyFiles' but uses 'installMaybeExecutableFile'.
installMaybeExecutableFiles
  :: Verbosity
  -> FilePath
  -> [(FilePath, FilePath)]
  -> IO ()
installMaybeExecutableFiles v fp fs = withFrozenCallStack (copyFilesWith installMaybeExecutableFile v fp fs)

-- | This installs all the files in a directory to a target location,
-- preserving the directory layout. All the files are assumed to be ordinary
-- rather than executable files.
installDirectoryContents :: Verbosity -> FilePath -> FilePath -> IO ()
installDirectoryContents verbosity srcDir destDir = withFrozenCallStack $ do
  info verbosity ("copy directory '" ++ srcDir ++ "' to '" ++ destDir ++ "'.")
  srcFiles <- getDirectoryContentsRecursive srcDir
  installOrdinaryFiles verbosity destDir [(srcDir, f) | f <- srcFiles]

-- | Recursively copy the contents of one directory to another path.
copyDirectoryRecursive :: Verbosity -> FilePath -> FilePath -> IO ()
copyDirectoryRecursive verbosity srcDir destDir = withFrozenCallStack $ do
  info verbosity ("copy directory '" ++ srcDir ++ "' to '" ++ destDir ++ "'.")
  srcFiles <- getDirectoryContentsRecursive srcDir
  copyFilesWith
    (const copyFile)
    verbosity
    destDir
    [ (srcDir, f)
    | f <- srcFiles
    ]

-------------------
-- File permissions

-- | Like 'doesFileExist', but also checks that the file is executable.
doesExecutableExist :: FilePath -> IO Bool
doesExecutableExist f = do
  exists <- doesFileExist f
  if exists
    then do
      perms <- getPermissions f
      return (executable perms)
    else return False

---------------------------
-- Temporary files and dirs

-- | Advanced options for 'withTempFile' and 'withTempDirectory'.
data TempFileOptions = TempFileOptions
  { optKeepTempFiles :: Bool
  -- ^ Keep temporary files?
  }

defaultTempFileOptions :: TempFileOptions
defaultTempFileOptions = TempFileOptions{optKeepTempFiles = False}

-- | Use a temporary filename that doesn't already exist.
withTempFile
  :: FilePath
  -- ^ Temp dir to create the file in
  -> String
  -- ^ File name template. See 'openTempFile'.
  -> (FilePath -> Handle -> IO a)
  -> IO a
withTempFile tmpDir template action =
  withTempFileEx defaultTempFileOptions tmpDir template action

-- | A version of 'withTempFile' that additionally takes a 'TempFileOptions'
-- argument.
withTempFileEx
  :: TempFileOptions
  -> FilePath
  -- ^ Temp dir to create the file in
  -> String
  -- ^ File name template. See 'openTempFile'.
  -> (FilePath -> Handle -> IO a)
  -> IO a
withTempFileEx opts tmpDir template action =
  Exception.bracket
    (openTempFile tmpDir template)
    ( \(name, handle) -> do
        hClose handle
        unless (optKeepTempFiles opts) $
          handleDoesNotExist () . removeFile $
            name
    )
    (withLexicalCallStack (\x -> uncurry action x))

-- | Create and use a temporary directory.
--
-- Creates a new temporary directory inside the given directory, making use
-- of the template. The temp directory is deleted after use. For example:
--
-- > withTempDirectory verbosity "src" "sdist." $ \tmpDir -> do ...
--
-- The @tmpDir@ will be a new subdirectory of the given directory, e.g.
-- @src/sdist.342@.
withTempDirectory :: Verbosity -> FilePath -> String -> (FilePath -> IO a) -> IO a
withTempDirectory verbosity targetDir template f =
  withFrozenCallStack $
    withTempDirectoryEx
      verbosity
      defaultTempFileOptions
      targetDir
      template
      (withLexicalCallStack (\x -> f x))

-- | A version of 'withTempDirectory' that additionally takes a
-- 'TempFileOptions' argument.
withTempDirectoryEx
  :: Verbosity
  -> TempFileOptions
  -> FilePath
  -> String
  -> (FilePath -> IO a)
  -> IO a
withTempDirectoryEx _verbosity opts targetDir template f =
  withFrozenCallStack $
    Exception.bracket
      (createTempDirectory targetDir template)
      ( unless (optKeepTempFiles opts)
          . handleDoesNotExist ()
          . removeDirectoryRecursive
      )
      (withLexicalCallStack (\x -> f x))

-----------------------------------
-- Safely reading and writing files

-- | Write a file but only if it would have new content. If we would be writing
-- the same as the existing content then leave the file as is so that we do not
-- update the file's modification time.
--
-- NB: Before Cabal-3.0 the file content was assumed to be
--     ASCII-representable. Since Cabal-3.0 the file is assumed to be
--     UTF-8 encoded.
rewriteFileEx :: Verbosity -> FilePath -> String -> IO ()
rewriteFileEx verbosity path =
  rewriteFileLBS verbosity path . toUTF8LBS

-- | Same as `rewriteFileEx` but for 'ByteString's.
rewriteFileLBS :: Verbosity -> FilePath -> BS.ByteString -> IO ()
rewriteFileLBS verbosity path newContent =
  flip catchIO mightNotExist $ do
    existingContent <- annotateIO verbosity $ BS.readFile path
    _ <- evaluate (BS.length existingContent)
    unless (existingContent == newContent) $
      annotateIO verbosity $
        writeFileAtomic path newContent
  where
    mightNotExist e
      | isDoesNotExistError e =
          annotateIO verbosity $ writeFileAtomic path newContent
      | otherwise =
          ioError e

-- | The path name that represents the current directory.
-- In Unix, it's @\".\"@, but this is system-specific.
-- (E.g. AmigaOS uses the empty string @\"\"@ for the current directory.)
currentDir :: FilePath
currentDir = "."

shortRelativePath :: FilePath -> FilePath -> FilePath
shortRelativePath from to =
  case dropCommonPrefix (splitDirectories from) (splitDirectories to) of
    (stuff, path) -> joinPath (map (const "..") stuff ++ path)
  where
    dropCommonPrefix :: Eq a => [a] -> [a] -> ([a], [a])
    dropCommonPrefix (x : xs) (y : ys)
      | x == y = dropCommonPrefix xs ys
    dropCommonPrefix xs ys = (xs, ys)

-- | Drop the extension if it's one of 'exeExtensions', or return the path
-- unchanged.
dropExeExtension :: FilePath -> FilePath
dropExeExtension filepath =
  -- System.FilePath's extension handling functions are horribly
  -- inconsistent, consider:
  --
  --     isExtensionOf "" "foo"  == False but
  --     isExtensionOf "" "foo." == True.
  --
  -- On the other hand stripExtension doesn't remove the empty extension:
  --
  --    stripExtension "" "foo." == Just "foo."
  --
  -- Since by "" in exeExtensions we mean 'no extension' anyways we can
  -- just always ignore it here.
  let exts = [ext | ext <- exeExtensions, ext /= ""]
   in fromMaybe filepath $ do
        ext <- find (`FilePath.isExtensionOf` filepath) exts
        ext `FilePath.stripExtension` filepath

-- | List of possible executable file extensions on the current build
-- platform.
exeExtensions :: [String]
exeExtensions = case (buildArch, buildOS) of
  -- Possible improvement: on Windows, read the list of extensions from the
  -- PATHEXT environment variable. By default PATHEXT is ".com; .exe; .bat;
  -- .cmd".
  (_, Windows) -> ["", "exe"]
  (_, Ghcjs) -> ["", "exe"]
  (Wasm32, _) -> ["", "wasm"]
  _ -> [""]

-- ------------------------------------------------------------

-- * Finding the description file

-- ------------------------------------------------------------

-- | Package description file (/pkgname/@.cabal@)
defaultPackageDesc :: Verbosity -> IO FilePath
defaultPackageDesc verbosity = tryFindPackageDesc verbosity currentDir

-- | Find a package description file in the given directory.  Looks for
--  @.cabal@ files.
findPackageDesc
  :: FilePath
  -- ^ Where to look
  -> IO (Either CabalException FilePath)
  -- ^ <pkgname>.cabal
findPackageDesc = findPackageDescCwd "."

-- | @since 3.4.0.0
findPackageDescCwd
  :: FilePath
  -- ^ project root
  -> FilePath
  -- ^ relative directory
  -> IO (Either CabalException FilePath)
  -- ^ <pkgname>.cabal relative to the project root
findPackageDescCwd cwd dir =
  do
    files <- getDirectoryContents (cwd </> dir)
    -- to make sure we do not mistake a ~/.cabal/ dir for a <pkgname>.cabal
    -- file we filter to exclude dirs and null base file names:
    cabalFiles <-
      filterM
        (doesFileExist . snd)
        [ (dir </> file, cwd </> dir </> file)
        | file <- files
        , let (name, ext) = splitExtension file
        , not (null name) && ext == ".cabal"
        ]
    case map fst cabalFiles of
      [] -> return (Left NoDesc)
      [cabalFile] -> return (Right cabalFile)
      multiple -> return (Left $ MultiDesc multiple)

-- | Like 'findPackageDesc', but calls 'die' in case of error.
tryFindPackageDesc :: Verbosity -> FilePath -> IO FilePath
tryFindPackageDesc verbosity dir =
  either (dieWithException verbosity) return =<< findPackageDesc dir

-- | Like 'findPackageDescCwd', but calls 'die' in case of error.
--
-- @since 3.4.0.0
tryFindPackageDescCwd :: Verbosity -> FilePath -> FilePath -> IO FilePath
tryFindPackageDescCwd verbosity cwd dir =
  either (dieWithException verbosity) return =<< findPackageDescCwd cwd dir

-- | Find auxiliary package information in the given directory.
--  Looks for @.buildinfo@ files.
findHookedPackageDesc
  :: Verbosity
  -> FilePath
  -- ^ Directory to search
  -> IO (Maybe FilePath)
  -- ^ /dir/@\/@/pkgname/@.buildinfo@, if present
findHookedPackageDesc verbosity dir = do
  files <- getDirectoryContents dir
  buildInfoFiles <-
    filterM
      doesFileExist
      [ dir </> file
      | file <- files
      , let (name, ext) = splitExtension file
      , not (null name) && ext == buildInfoExt
      ]
  case buildInfoFiles of
    [] -> return Nothing
    [f] -> return (Just f)
    _ -> dieWithException verbosity $ MultipleFilesWithExtension buildInfoExt

buildInfoExt :: String
buildInfoExt = ".buildinfo"
