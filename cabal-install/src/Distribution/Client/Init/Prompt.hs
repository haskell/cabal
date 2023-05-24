{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.Init.Prompt
-- Copyright   :  (c) Brent Yorgey 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- User prompt utility functions for use by the 'cabal init' command.
module Distribution.Client.Init.Prompt
  ( prompt
  , promptYesNo
  , promptStr
  , promptList
  ) where

import Prelude hiding (break, getLine, putStr, putStrLn)

import Distribution.Client.Compat.Prelude hiding (break, empty, getLine, putStr, putStrLn)
import Distribution.Client.Init.Types
import qualified System.IO

-- | Create a prompt with optional default value that returns a
-- String.
promptStr :: Interactive m => String -> DefaultPrompt String -> m String
promptStr = promptDefault Right id

-- | Create a yes/no prompt with optional default value.
promptYesNo
  :: Interactive m
  => String
  -- ^ prompt message
  -> DefaultPrompt Bool
  -- ^ optional default value
  -> m Bool
promptYesNo =
  promptDefault recogniseYesNo showYesNo
  where
    recogniseYesNo s
      | (toLower <$> s) == "y" = Right True
      | (toLower <$> s) == "n" || s == "N" = Right False
      | otherwise = Left $ "Cannot parse input: " ++ s

    showYesNo True = "y"
    showYesNo False = "n"

-- | Create a prompt with optional default value that returns a value
--   of some Text instance.
prompt :: (Interactive m, Parsec t, Pretty t) => String -> DefaultPrompt t -> m t
prompt = promptDefault eitherParsec prettyShow

-- | Create a prompt from a prompt string and a String representation
--   of an optional default value.
mkDefPrompt :: String -> DefaultPrompt String -> String
mkDefPrompt msg def = msg ++ "?" ++ format def
  where
    format MandatoryPrompt = " "
    format OptionalPrompt = " [optional] "
    format (DefaultPrompt s) = " [default: " ++ s ++ "] "

-- | Create a prompt from a list of strings
promptList
  :: Interactive m
  => String
  -- ^ prompt
  -> [String]
  -- ^ choices
  -> DefaultPrompt String
  -- ^ optional default value
  -> Maybe (String -> String)
  -- ^ modify the default value to present in-prompt
  -- e.g. empty string maps to "(none)", but only in the
  -- prompt.
  -> Bool
  -- ^ whether to allow an 'other' option
  -> m String
promptList msg choices def modDef hasOther = do
  putStrLn $ msg ++ ":"

  -- Output nicely formatted list of options
  for_ prettyChoices $ \(i, c) -> do
    let star =
          if DefaultPrompt c == def
            then "*"
            else " "

    let output =
          concat $
            if i < 10
              then [" ", star, " ", show i, ") ", c]
              else [" ", star, show i, ") ", c]

    putStrLn output

  go
  where
    prettyChoices =
      let cs =
            if hasOther
              then choices ++ ["Other (specify)"]
              else choices
       in zip [1 :: Int .. length choices + 1] cs

    numChoices = length choices

    invalidChoice input = do
      let msg' =
            if null input
              then "Empty input is not a valid choice."
              else
                concat
                  [ input
                  , " is not a valid choice. Please choose a number from 1 to "
                  , show (length prettyChoices)
                  , "."
                  ]

      putStrLn msg'
      breakOrContinue ("promptList: " ++ input) go

    go = do
      putStr $
        mkDefPrompt "Your choice" $
          maybe def (<$> def) modDef

      input <- getLine
      case def of
        DefaultPrompt d | null input -> return d
        _ -> case readMaybe input of
          Nothing -> invalidChoice input
          Just n
            | n > 0, n <= numChoices -> return $ choices !! (n - 1)
            | n == numChoices + 1
            , hasOther ->
                promptStr "Please specify" OptionalPrompt
            | otherwise -> invalidChoice (show n)

-- | Create a prompt with an optional default value.
promptDefault
  :: Interactive m
  => (String -> Either String t)
  -- ^ parser
  -> (t -> String)
  -- ^ pretty-printer
  -> String
  -- ^ prompt message
  -> (DefaultPrompt t)
  -- ^ optional default value
  -> m t
promptDefault parse pprint msg def = do
  putStr $ mkDefPrompt msg (pprint <$> def)
  hFlush System.IO.stdout
  input <- getLine
  case def of
    DefaultPrompt d | null input -> return d
    _ -> case parse input of
      Right t -> return t
      Left err -> do
        putStrLn $ "Couldn't parse " ++ input ++ ", please try again!"
        breakOrContinue
          ("promptDefault: " ++ err ++ " on input: " ++ input)
          (promptDefault parse pprint msg def)

-- | Prompt utility for breaking out of an interactive loop
-- in the pure case
breakOrContinue :: Interactive m => String -> m a -> m a
breakOrContinue msg act =
  break >>= \case
    True -> throwPrompt $ BreakException msg
    False -> act
