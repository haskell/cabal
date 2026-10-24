-- | The @api-lint@ step: the public API tier of @cabal-install@.
--
-- Two invariants promised by the cabal-install API proposal are checked:
--
-- 1. /Namespace purity/: every module shipped in @cabal-install\/src@ lives
--    under @Distribution.Client.API.*@. The public package exposes exactly
--    the API tier (the layer 0 primitives plus the layer 1 command
--    facades) -- nothing else can accrete.
--
-- 2. /Facades are pure re-exports/: a module under @...\/API\/Command\/@
--    defines nothing of its own. Its export list is the frozen
--    \"programmatic cabal\" contract, sourced entirely from
--    @cabal-install-internal@.
module ApiLint
  ( apiLint
  ) where

import Control.Monad (forM, forM_)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (isExtensionOf, (</>))
import System.IO (hPutStrLn, stderr)

-- | Run the lint over @\<cwd\>\/cabal-install\/src@.
apiLint :: FilePath -> IO ()
apiLint cwd = do
  let srcDir = cwd </> "cabal-install/src"
      commandDir = srcDir </> "Distribution/Client/API/Command/"
  hsFiles <- collectHs srcDir
  let facades = [f | f <- hsFiles, commandDir `isPrefixOf` f]
  namespaceViolations <- traverse (namespaceViolation srcDir) hsFiles
  facadeViolations <- traverse facadeViolation facades
  let violations = concat namespaceViolations ++ concat facadeViolations
  case violations of
    [] ->
      putStrLn $
        "api-lint: OK ("
          ++ show (length hsFiles)
          ++ " modules, "
          ++ show (length facades)
          ++ " facades checked)"
    _ -> do
      hPutStrLn
        stderr
        "api-lint: the public API tier of cabal-install is violated:"
      forM_ violations (hPutStrLn stderr)
      exitWith (ExitFailure 1)

collectHs :: FilePath -> IO [FilePath]
collectHs dir = do
  entries <- listDirectory dir
  fmap concat $ forM entries $ \e -> do
    let p = dir </> e
    isDir <- doesDirectoryExist p
    if isDir
      then collectHs p
      else pure [p | isExtensionOf ".hs" p]

-- | Every module in the public package must live under
-- @Distribution.Client.API.*@ (namespace purity of the API tier).
namespaceViolation :: FilePath -> FilePath -> IO [String]
namespaceViolation srcDir f = do
  contents <- readContents f
  let rel = drop (length srcDir + 1) f
      headerLines = [l | l <- T.lines contents, "module" `T.isPrefixOf` l]
  pure $ case headerLines of
    (l : _)
      | Just m <- moduleName l
      , m == "Distribution.Client.API"
          || "Distribution.Client.API." `T.isPrefixOf` m ->
          []
      | Just m <- moduleName l ->
          [rel ++ ": module " ++ T.unpack m ++ " is not under Distribution.Client.API.*"]
    _ ->
      [rel ++ ": cannot parse the module header"]

moduleName :: T.Text -> Maybe T.Text
moduleName l = case T.words l of
  ("module" : m : _) -> Just m
  _ -> Nothing

-- | A facade must be a pure re-export shim: a module header, an export
-- list and imports -- nothing else.
facadeViolation :: FilePath -> IO [String]
facadeViolation f = do
  contents <- readContents f
  pure
    [ f ++ ":" ++ show n ++ ": facades must be pure re-exports: " ++ T.unpack l
    | (n, l) <- zip [1 :: Int ..] (T.lines contents)
    , not (allowedFacadeLine l)
    ]

allowedFacadeLine :: T.Text -> Bool
allowedFacadeLine l
  | T.null l = True -- blank
  | isSpace (T.head l) = True -- continuation line
  | "--" `T.isPrefixOf` l = True -- comment
  | "{-#" `T.isPrefixOf` l = True -- pragma
  | otherwise = T.takeWhile (not . isSpace) l `elem` ["module", "(", ") where", ")", "import"]

readContents :: FilePath -> IO T.Text
readContents f = decodeUtf8With lenientDecode <$> BS.readFile f
