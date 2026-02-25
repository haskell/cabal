{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Client.Errors.Parser where

import Distribution.Client.Compat.Prelude
import System.FilePath (normalise)
import Prelude ()

import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
import Distribution.Parsec
import Distribution.Parsec.Source
import Distribution.Simple.Utils (fromUTF8BS)
import Distribution.Solver.Types.ProjectConfigPath
import Distribution.Version (Version)
import Text.PrettyPrint (render)

-- Error when parsing a .cabal file

-- | Errors reported upon failing to parse a @.cabal@ file.
data CabalFileParseError
  = CabalFileParseError
      FilePath
      -- ^ @.cabal@ file path
      BS8.ByteString
      -- ^ @.cabal@ file contents
      (NonEmpty (PErrorWithSource CabalFileSource))
      -- ^ errors
      (Maybe Version)
      -- ^ We might discover the spec version the package needs
      [PWarningWithSource CabalFileSource]
      -- ^ warnings

-- | Manual instance which skips file contents
instance Show CabalFileParseError where
  showsPrec d (CabalFileParseError fp _ es mv ws) =
    showParen (d > 10) $
      showString "CabalFileParseError"
        . showChar ' '
        . showsPrec 11 fp
        . showChar ' '
        . showsPrec 11 ("" :: String)
        . showChar ' '
        . showsPrec 11 es
        . showChar ' '
        . showsPrec 11 mv
        . showChar ' '
        . showsPrec 11 ws

instance Exception CabalFileParseError where
  displayException = renderCabalFileParseError

renderCabalFileParseError :: CabalFileParseError -> String
renderCabalFileParseError (CabalFileParseError _filePath _contents errors _ warnings) =
  renderParseErrorCabalFile errors warnings

-- Error when parsing a project file

-- | Errors reported upon failing to parse a @cabal.project@ file.
data ProjectConfigParseError
  = ProjectConfigParseError
      (NonEmpty (PErrorWithSource ProjectFileSource))
      -- ^ errors
      [PWarningWithSource ProjectFileSource]
      -- ^ warnings

-- | Manual instance which skips file contents
instance Show ProjectConfigParseError where
  showsPrec d (ProjectConfigParseError es ws) =
    showParen (d > 10) $
      showString "ProjectConfigParseError"
        . showChar ' '
        . showsPrec 11 es
        . showChar ' '
        . showsPrec 11 ws

instance Exception ProjectConfigParseError where
  displayException = renderProjectConfigParseError

renderProjectConfigParseError :: ProjectConfigParseError -> String
renderProjectConfigParseError (ProjectConfigParseError errors warnings) =
  renderParseError displayProjectFileSource errors warnings
  where
    displayProjectFileSource (ProjectFileSource (path, contents)) =
      renderParseErrorFile "project" (currentProjectConfigPath path) (if isTopLevelConfigPath path then Nothing else Just $ render (docProjectImportedBy path)) contents

data ProjectFileSource = ProjectFileSource (ProjectConfigPath, BS8.ByteString) deriving (Show, Generic)

instance Eq ProjectFileSource where
  (ProjectFileSource (path1, _)) == (ProjectFileSource (path2, _)) = path1 == path2

instance Ord ProjectFileSource where
  (ProjectFileSource (path1, _)) `compare` (ProjectFileSource (path2, _)) = path1 `compare` path2

renderProjectFileSource :: ProjectFileSource -> String
renderProjectFileSource (ProjectFileSource (path, _contents)) =
  currentProjectConfigPath path

renderParseErrorCabalFile :: NonEmpty (PErrorWithSource CabalFileSource) -> [PWarningWithSource CabalFileSource] -> String
renderParseErrorCabalFile errors warnings =
  renderParseError renderCabalFileSourceMsgs errors warnings

-- | Render parse error highlighting the part of the input file.
renderParseError
  :: forall src
   . (Ord src, Eq src)
  => (src -> ([PError], [PWarning]) -> String)
  -> NonEmpty (PErrorWithSource src)
  -> [PWarningWithSource src]
  -> String
renderParseError display errors warnings =
  unlines [renderParseErrorsWarnings (fmap display source) ws | (source, ws) <- joinedErrorsWarningsList]
  where
    mkErrorGroup :: NonEmpty (PErrorWithSource src) -> (PSource src, [PError])
    mkErrorGroup (x :| xs) = (perrorSource x, perror x : map perror xs)

    mkWarningGroup :: NonEmpty (PWarningWithSource src) -> (PSource src, [PWarning])
    mkWarningGroup (x :| xs) = (pwarningSource x, pwarning x : map pwarning xs)

    groupedErrors =
      Map.fromListWith (++) $
        map mkErrorGroup $
          NE.groupBy
            (\a b -> perrorSource a == perrorSource b)
            errors
    groupedWarnings =
      Map.fromListWith (++) $
        map mkWarningGroup $
          NE.groupBy
            (\a b -> pwarningSource a == pwarningSource b)
            warnings

    joinedErrorsWarnings :: Map.Map (PSource src) ([PError], [PWarning])
    joinedErrorsWarnings = Map.merge (Map.mapMissing (\_ es -> (es, []))) (Map.mapMissing (\_ ps -> ([], ps))) (Map.zipWithMatched (\_ es ps -> (es, ps))) groupedErrors groupedWarnings

    joinedErrorsWarningsList = Map.toList joinedErrorsWarnings

renderParseErrorsWarnings :: PSource (([PError], [PWarning]) -> String) -> ([PError], [PWarning]) -> String
renderParseErrorsWarnings source (errors, warnings) =
  case source of
    PKnownSource src -> src (errors, warnings)
    PUnknownSource -> renderParseErrorNoFile "" errors warnings

renderCabalFileSourceMsgs :: CabalFileSource -> ([PError], [PWarning]) -> String
renderCabalFileSourceMsgs (PCabalFile (fpath, contents)) (errors, warnings) =
  renderParseErrorFile "cabal" fpath Nothing contents (errors, warnings)

renderInstalledPackageInfoSourceMsgs :: InstalledPackageInfoSource -> ([PError], [PWarning]) -> String
renderInstalledPackageInfoSourceMsgs PInstalledPackageInfo (errors, warnings) =
  renderParseErrorNoFile "installed package info" errors warnings

renderParseErrorNoFile :: String -> [PError] -> [PWarning] -> String
renderParseErrorNoFile herald errors warnings =
  renderParseErrorGeneral herald Nothing Nothing (const []) errors warnings

-- | Render a parse error which resulted from a file on disk
renderParseErrorFile
  :: String
  -- ^ Human name for the kind of file (i.e. cabal, project "file")
  -> FilePath
  -- ^ Path to the file
  -> Maybe String
  -- ^ Provenance, any additional contextual info to print
  -> BS8.ByteString
  -- ^ Contents of the file
  -> ([PError], [PWarning])
  -> String
renderParseErrorFile herald filepath provenance contents (errors, warnings) =
  renderParseErrorGeneral (herald <> " file " <> filepath) (Just (filepath' <> ":")) provenance formatInput errors warnings
  where
    filepath' = normalise filepath

    -- lines of the input file. 'lines' is taken, so they are called rows
    -- contents, line number, whether it's empty line
    rows :: [(String, Int, Bool)]
    rows = zipWith f (BS8.lines contents) [1 ..]
      where
        f bs i = let s = fromUTF8BS bs in (s, i, isEmptyOrComment s)

    rowsZipper = listToZipper rows

    isEmptyOrComment :: String -> Bool
    isEmptyOrComment s = case dropWhile (== ' ') s of
      "" -> True -- empty
      ('-' : '-' : _) -> True -- comment
      _ -> False

    -- format line: prepend the given line number
    formatInput :: Position -> [String]
    formatInput (Position row col) = case advance (row - 1) rowsZipper of
      Zipper xs ys -> before ++ after
        where
          before = case span (\(_, _, b) -> b) xs of
            (_, []) -> []
            (zs, z : _) -> map formatInputLine $ z : reverse zs

          after = case ys of
            [] -> []
            (z : _zs) ->
              [ formatInputLine z -- error line
              , "      | " ++ replicate (col - 1) ' ' ++ "^" -- pointer: ^
              ]
    -- do we need rows after?
    -- ++ map formatInputLine (take 1 zs)           -- one row after

    formatInputLine :: (String, Int, Bool) -> String
    formatInputLine (str, row, _) = leftPadShow row ++ " | " ++ str

    -- hopefully we don't need to work with over 99999 lines .cabal files
    -- at that point small glitches in error messages are hopefully fine.
    leftPadShow :: Int -> String
    leftPadShow n = let s = show n in replicate (5 - length s) ' ' ++ s

-- | A generic rendering function which can render from many sources.
renderParseErrorGeneral
  :: String
  -- ^ What we were parsing when the error occurred.
  -> Maybe String
  -- ^ A simpler/shorter header to display when displaying each error (normally a filepath)
  -> Maybe String
  -- ^ Provenance, used to print additional context about what file failed (used to print the import path of a project
  -- file which failed to parse)
  -> (Position -> [String])
  -- ^ Extra information to render based on the position
  -> [PError]
  -> [PWarning]
  -> String
renderParseErrorGeneral header err_header provenance extra_info errors warnings =
  unlines $
    [ warningsOrErrors <> " parsing" <> header' <> ":"
    ]
      ++ catMaybes [provenance]
      ++ [""] -- Place a newline between the header and the errors/warnings
      -- Place a newline between each error and warning
      ++ intersperse "" (renderedWarnings ++ renderedErrors)
  where
    warningsOrErrors = case errors of
      [] -> case warnings of
        [_] -> "Warning"
        _ -> "Warnings"
      [_] -> "Error"
      _ -> "Errors"

    header' = if null header then "" else (" " <> header)

    renderedErrors = map renderError (sortBy (comparing perrorPosition) errors)
    renderedWarnings = map renderWarning (sortBy (comparing pwarningPosition) warnings)

    renderErrorOrWarning :: String -> Position -> String -> String
    renderErrorOrWarning err_type pos msg
      -- if position is 0:0, then it doesn't make sense to show input
      -- looks like, Parsec errors have line-feed in them
      | pos == zeroPos = unlines (herald : map indent user_msg)
      | otherwise = unlines (herald : map indent (user_msg ++ extra_info pos))
      where
        herald = renderErrorHerald pos ++ err_type ++ ":"
        user_msg = lines (trimLF msg)

    indent :: String -> String
    indent s = replicate 2 ' ' ++ s

    -- Don't render the 0:0 position
    renderErrorHerald :: Position -> String
    renderErrorHerald pos =
      case (err_header, pos == zeroPos) of
        (Nothing, True) -> ""
        (Nothing, False) -> showPos pos ++ ": "
        (Just herald, True) -> herald ++ " "
        (Just herald, False) -> herald ++ showPos pos ++ ": "

    renderError :: PError -> String
    renderError (PError pos msg) = renderErrorOrWarning "error" pos msg

    renderWarning :: PWarning -> String
    renderWarning (PWarning _ pos msg) = renderErrorOrWarning "warning" pos msg

    -- sometimes there are (especially trailing) newlines.
    trimLF :: String -> String
    trimLF = dropWhile (== '\n') . reverse . dropWhile (== '\n') . reverse

data Zipper a = Zipper [a] [a]

listToZipper :: [a] -> Zipper a
listToZipper = Zipper []

advance :: Int -> Zipper a -> Zipper a
advance n z@(Zipper xs ys)
  | n <= 0 = z
  | otherwise = case ys of
      [] -> z
      (y : ys') -> advance (n - 1) $ Zipper (y : xs) ys'
