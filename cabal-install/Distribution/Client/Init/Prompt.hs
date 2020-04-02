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
--
-----------------------------------------------------------------------------

module Distribution.Client.Init.Prompt (

    -- * Commands
    prompt
  , promptYesNo
  , promptStr
  , promptList
  , promptListOptional
  , promptListOptional'
  , maybePrompt
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (empty)

import Distribution.Deprecated.ReadP (readP_to_E)

import Control.Monad
  ( mapM_ )

import Distribution.Client.Init.Types
  ( InitFlags(..) )
import Distribution.Deprecated.Text
  ( display, Text(..) )
import Distribution.ReadE
  ( runReadE )
import Distribution.Simple.Setup
  ( Flag(..) )


-- | Run a prompt or not based on the interactive flag of the
--   InitFlags structure.
maybePrompt :: InitFlags -> IO t -> IO (Maybe t)
maybePrompt flags p =
  case interactive flags of
    Flag True -> Just `fmap` p
    _         -> return Nothing

-- | Create a prompt with optional default value that returns a
--   String.
promptStr :: String -> Maybe String -> IO String
promptStr = promptDefault' Just id

-- | Create a yes/no prompt with optional default value.
promptYesNo :: String      -- ^ prompt message
            -> Maybe Bool  -- ^ optional default value
            -> IO Bool
promptYesNo =
    promptDefault' recogniseYesNo showYesNo
  where
    recogniseYesNo s | s == "y" || s == "Y" = Just True
                     | s == "n" || s == "N" = Just False
                     | otherwise            = Nothing
    showYesNo True  = "y"
    showYesNo False = "n"

-- | Create a prompt with optional default value that returns a value
--   of some Text instance.
prompt :: Text t => String -> Maybe t -> IO t
prompt = promptDefault'
           (either (const Nothing) Just . runReadE (readP_to_E id parse))
           display

-- | Create a prompt with an optional default value.
promptDefault' :: (String -> Maybe t)       -- ^ parser
               -> (t -> String)             -- ^ pretty-printer
               -> String                    -- ^ prompt message
               -> Maybe t                   -- ^ optional default value
               -> IO t
promptDefault' parser pretty pr def = do
  putStr $ mkDefPrompt pr (pretty `fmap` def)
  inp <- getLine
  case (inp, def) of
    ("", Just d)  -> return d
    _  -> case parser inp of
            Just t  -> return t
            Nothing -> do putStrLn $ "Couldn't parse " ++ inp ++ ", please try again!"
                          promptDefault' parser pretty pr def

-- | Create a prompt from a prompt string and a String representation
--   of an optional default value.
mkDefPrompt :: String -> Maybe String -> String
mkDefPrompt pr def = pr ++ "?" ++ defStr def
  where defStr Nothing  = " "
        defStr (Just s) = " [default: " ++ s ++ "] "

-- | Create a prompt from a list of items, where no selected items is
--   valid and will be represented as a return value of 'Nothing'.
promptListOptional :: (Text t, Eq t)
                   => String            -- ^ prompt
                   -> [t]               -- ^ choices
                   -> IO (Maybe (Either String t))
promptListOptional pr choices = promptListOptional' pr choices display

promptListOptional' :: Eq t
                   => String            -- ^ prompt
                   -> [t]               -- ^ choices
                   -> (t -> String)     -- ^ show an item
                   -> IO (Maybe (Either String t))
promptListOptional' pr choices displayItem =
    fmap rearrange
  $ promptList pr (Nothing : map Just choices) (Just Nothing)
               (maybe "(none)" displayItem) True
  where
    rearrange = either (Just . Left) (fmap Right)

-- | Create a prompt from a list of items.
promptList :: Eq t
           => String            -- ^ prompt
           -> [t]               -- ^ choices
           -> Maybe t           -- ^ optional default value
           -> (t -> String)     -- ^ show an item
           -> Bool              -- ^ whether to allow an 'other' option
           -> IO (Either String t)
promptList pr choices def displayItem other = do
  putStrLn $ pr ++ ":"
  let options1 = map (\c -> (Just c == def, displayItem c)) choices
      options2 = zip ([1..]::[Int])
                     (options1 ++ [(False, "Other (specify)") | other])
  mapM_ (putStrLn . \(n,(i,s)) -> showOption n i ++ s) options2
  promptList' displayItem (length options2) choices def other
 where showOption n i | n < 10 = " " ++ star i ++ " " ++ rest
                      | otherwise = " " ++ star i ++ rest
                  where rest = show n ++ ") "
                        star True = "*"
                        star False = " "

promptList' :: (t -> String) -> Int -> [t] -> Maybe t -> Bool -> IO (Either String t)
promptList' displayItem numChoices choices def other = do
  putStr $ mkDefPrompt "Your choice" (displayItem `fmap` def)
  inp <- getLine
  case (inp, def) of
    ("", Just d) -> return $ Right d
    _  -> case readMaybe inp of
            Nothing -> invalidChoice inp
            Just n  -> getChoice n
 where invalidChoice inp = do putStrLn $ inp ++ " is not a valid choice."
                              promptList' displayItem numChoices choices def other
       getChoice n | n < 1 || n > numChoices = invalidChoice (show n)
                   | n < numChoices ||
                     (n == numChoices && not other)
                                  = return . Right $ choices !! (n-1)
                   | otherwise    = Left `fmap` promptStr "Please specify" Nothing
