{- HLINT ignore "Use zipWith" -}

module Tool
  ( mkTool )
  where

-- base
import Control.Monad
  ( unless )
import Data.Char
  ( isSpace )
import Data.List
  ( dropWhileEnd )
import System.Environment
  ( getArgs )

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map

-- directory
import System.Directory
  ( doesFileExist )

--------------------------------------------------------------------------------

mkTool :: String -> FilePath -> IO ()
mkTool buildToolName customDataFile = do
  putStrLn $ "Starting " ++ buildToolName
  -- Get all the constants defined in the data file for the build tool.
  customDataFileExists <- doesFileExist customDataFile
  unless customDataFileExists $ do
    error $
      unlines
        [ "Custom preprocessor " ++ buildToolName ++ " could not access its data file."
        , "This is probably due to a missing _datadir environment variable when invoking the build tool."
        , "Tried to look in: " ++ customDataFile ]
  customDataLines <- lines <$> readFile customDataFile
  let customConstants :: Map String Int
      customConstants = Map.fromList $ map read customDataLines

  -- Obtain input/output file paths from arguments to the preprocessor.
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      inputFileExists <- doesFileExist inputFile
      unless inputFileExists $
        error $
          unlines
            [ "Custom preprocessor " ++ buildToolName ++ " could not read input file."
            , "Input file: " ++ inputFile ]
      -- Read the input file, substitute constants for their values,
      -- and write the result to the output file path.
      inputLines <- lines <$> readFile inputFile
      let outputLines = map ( preprocessLine customConstants ) ( zip [1..] inputLines )
      writeFile outputFile ( unlines outputLines )
    [] ->
      putStrLn $ "Custom preprocessor " ++ buildToolName ++ ": no arguments."
    _ ->
      error $
        unlines
          [ "Custom preprocessor " ++ buildToolName ++ " was given incorrect arguments."
          , "Expected input and output file paths, but got " ++ what ++ "." ]
      where
        what = case args of
          [_] -> "a single argument"
          _  -> show (length args) ++ " arguments"

-- | Substitute any occurrence of {# ConstantName #} with the value of ConstantName,
-- looked up in the data file for the preprocessor.
preprocessLine :: Map String Int -> ( Int, String ) -> String
preprocessLine constants ( ln_no, ln ) = go "" ln
  where
    go reversedPrev [] = reverse reversedPrev
    go reversedPrev ('{':'#':rest) = reverse reversedPrev ++ inner "" rest
    go reversedPrev (c:rest) = go (c:reversedPrev) rest

    inner reversedNm ('#':'}':rest) =
      let constName = trimWhitespace $ reverse reversedNm
       in case Map.lookup constName constants of
            Just val -> show val ++ go "" rest
            Nothing ->
              error $ unlines
                [ "Could not preprocess line " ++ show ln_no ++ ":"
                , "unknown constant \"" ++ constName ++ "\"." ]
    inner reversedNm (c:rest) = inner (c:reversedNm) rest
    inner reversedNm "" =
      error $ unlines
        [ "Could not preprocess line " ++ show ln_no ++ ":"
        , "unterminated constant \"{# " ++ reverse reversedNm ++ "\"." ]

trimWhitespace :: String -> String
trimWhitespace = dropWhile isSpace . dropWhileEnd isSpace
