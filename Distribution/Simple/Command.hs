-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Command
-- Copyright   :  Duncan Coutts 2007
-- 
-- Maintainer  :  Duncan Coutts <duncan@haskell.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Data types and parser for the standard command-line
-- setup.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Simple.Command (

  -- * Command interface
  CommandUI(..),
  commandShowOptions,
    
  -- * Constructing commands
  ShowOrParseArgs(..),
  makeCommand,
  Option, option, liftOption,
  ArgDescr, noArg, reqArg, optArg,
  
  -- * Associating actions with commands
  Command,
  commandAddAction,
  
  -- * Running commands
  CommandParse(..),
  commandsRun,
  ) where

import qualified Distribution.GetOpt as GetOpt
import Data.Monoid (Monoid(..))

data CommandUI flags = CommandUI {
    -- | The name of the command as it would be entered on the command line.
    -- For example @\"build\"@.
    commandName        :: String,
    -- | A short, one line description of the command to use in help texts.
    commandSynopsis :: String,
    -- | The useage line summary for this command
    commandUsage    :: String -> String,
    -- | Additional explanation of the command to use in help texts.
    commandDescription :: Maybe (String -> String),
    -- | Initial \/ empty flags
    commandDefaultFlags :: flags,
    -- | All the GetOpt options for this command
    commandOptions     :: ShowOrParseArgs -> [Option flags]
  }

data ShowOrParseArgs = ShowArgs | ParseArgs

data Option a = Option [Char] [String] String (ArgDescr a)

option :: [Char] -> [String] -> String -> c -> d
       -> (c -> d -> ArgDescr a) -> Option a
option ss ls d get set arg = Option ss ls d (arg get set)

data ArgDescr a = NoArg         (                a -> a) (a ->           Bool)
                | ReqArg String (      String -> a -> a) (a ->       [String])
                | OptArg String (Maybe String -> a -> a) (a -> [Maybe String])

optionToGetOpt :: Option a -> GetOpt.OptDescr (a -> a)
optionToGetOpt (Option cs ss d arg) = GetOpt.Option cs ss (argDescrToGetOpt arg) d

argDescrToGetOpt :: ArgDescr a -> GetOpt.ArgDescr (a -> a)
argDescrToGetOpt (NoArg    f _) = GetOpt.NoArg f
argDescrToGetOpt (ReqArg d f _) = GetOpt.ReqArg f d
argDescrToGetOpt (OptArg d f _) = GetOpt.OptArg f d

liftOption :: (b -> a) -> (a -> (b -> b)) -> Option a -> Option b
liftOption get set (Option cs ss d arg) =
  Option cs ss d (liftArgDescr get set arg)

liftArgDescr :: (b -> a) -> (a -> (b -> b)) -> ArgDescr a -> ArgDescr b
liftArgDescr get set arg = case arg of
  NoArg    f t -> NoArg    (\  b -> set (f   (get b)) b) (t . get)
  ReqArg d f t -> ReqArg d (\s b -> set (f s (get b)) b) (t . get)
  OptArg d f t -> OptArg d (\s b -> set (f s (get b)) b) (t . get)

noArg :: Monoid a => a -> (a -> Bool)
      ->(b -> a) -> (a -> (b -> b)) -> ArgDescr b
noArg flag showflag get set = NoArg (\b -> set (get b `mappend` flag) b) (showflag . get)

reqArg :: Monoid a => String -> (String -> a) -> (a -> [String])
       -> (b -> a) -> (a -> (b -> b)) -> ArgDescr b
reqArg name mkflag showflag get set =
  ReqArg name (\v b -> set (get b `mappend` mkflag v) b) (showflag . get)

optArg :: Monoid a => String -> (Maybe String -> a) -> (a -> [Maybe String])
       -> (b -> a) -> (a -> (b -> b)) -> ArgDescr b
optArg name mkflag showflag get set =
  OptArg name (\v b -> set (get b `mappend` mkflag v) b) (showflag . get)

-- | Show flags in the standard long option command line format
commandShowOptions :: CommandUI flags -> flags -> [String]
commandShowOptions command v =
  concatMap (showOption v) (commandOptions command ParseArgs)
  where 
    showOption :: a -> Option a -> [String]
    showOption x (Option _ (name:_) _ (NoArg _ showflag)) | showflag x
      = ["--"++name]
    showOption x (Option _ (name:_) _ (ReqArg _ _ showflag))
      = [ "--"++name++"="++flag
        | flag <- showflag x ]
    showOption x (Option _ (name:_) _ (OptArg _ _ showflag))
      = [ case flag of
            Just s  -> "--"++name++"="++s
            Nothing -> "--"++name
        | flag <- showflag x ]
    showOption _ _ = []

commandListOptions :: CommandUI flags -> [String]
commandListOptions command =
  concatMap listOption $
    addCommonFlags ShowArgs $ -- This is a slight hack, we don't want
                              -- "--list-options" showing up in the
                              -- list options output, so use ShowArgs
      map optionToGetOpt (commandOptions command ParseArgs)
  where
    listOption (GetOpt.Option shortNames longNames _ _) =
         [ "-"  ++ [name] | name <- shortNames ]
      ++ [ "--" ++  name  | name <- longNames ]

-- | The help text for this command with descriptions of all the options.
commandHelp :: CommandUI flags -> String
commandHelp command =
    GetOpt.usageInfo ""
  . addCommonFlags ShowArgs
  . map optionToGetOpt
  $ commandOptions command ShowArgs

-- | Make a Command from standard 'GetOpt' options.
makeCommand :: String                         -- ^ name
            -> String                         -- ^ short description
            -> Maybe (String -> String)       -- ^ long description
            -> flags                          -- ^ initial\/empty flags
            -> (ShowOrParseArgs -> [Option flags]) -- ^ options
            -> CommandUI flags
makeCommand name shortDesc longDesc defaultFlags options =
  CommandUI {
    commandName         = name,
    commandSynopsis     = shortDesc,
    commandDescription  = longDesc,
    commandUsage        = usage,
    commandDefaultFlags = defaultFlags,
    commandOptions      = options
  }
  where usage pname = "Usage: " ++ pname ++ " " ++ name ++ " [FLAGS]\n\n"
                   ++ "Flags for " ++ name ++ ":"

-- | Common flags that apply to every command
data CommonFlag = HelpFlag | ListOptionsFlag

commonFlags :: ShowOrParseArgs -> [GetOpt.OptDescr CommonFlag]
commonFlags showOrParseArgs = case showOrParseArgs of
  ShowArgs  -> [help]
  ParseArgs -> [help, list]
  where
    help = GetOpt.Option helpShortFlags ["help"] (GetOpt.NoArg HelpFlag)
             "Show this help text"
    helpShortFlags = case showOrParseArgs of
      ShowArgs  -> ['h']
      ParseArgs -> ['h', '?']
    list = GetOpt.Option [] ["list-options"] (GetOpt.NoArg ListOptionsFlag)
             "Print a list of command line flags"

addCommonFlags :: ShowOrParseArgs
               -> [GetOpt.OptDescr a]
               -> [GetOpt.OptDescr (Either CommonFlag a)]
addCommonFlags showOrParseArgs options =
     map (fmapOptDesc Left)  (commonFlags showOrParseArgs)
  ++ map (fmapOptDesc Right) options
  where fmapOptDesc f (GetOpt.Option s l d m) =
                       GetOpt.Option s l (fmapArgDesc f d) m
        fmapArgDesc f (GetOpt.NoArg a)    = GetOpt.NoArg (f a)
        fmapArgDesc f (GetOpt.ReqArg s d) = GetOpt.ReqArg (f . s) d
        fmapArgDesc f (GetOpt.OptArg s d) = GetOpt.OptArg (f . s) d


commandParseArgs :: CommandUI flags -> Bool -> [String]
                 -> CommandParse (flags -> flags, [String])
commandParseArgs command ordered args =
  let options = addCommonFlags ParseArgs
              . map optionToGetOpt
              $ commandOptions command ParseArgs
      order | ordered   = GetOpt.RequireOrder
            | otherwise = GetOpt.Permute
  in case GetOpt.getOpt order options args of
    (flags, _,    _)
      | not (null [ () | Left ListOptionsFlag <- flags ])
                      -> CommandList (commandListOptions command)
      | not (null [ () | Left HelpFlag <- flags ])
                      -> CommandHelp help
    (flags, opts, []) -> CommandReadyToGo (accumFlags flags , opts)
    (_,     _,  errs) -> CommandErrors errs

  where help pname = commandUsage command pname
                  ++ commandHelp command
                  ++ case commandDescription command of
                       Nothing   -> ""
                       Just desc -> '\n': desc pname
        -- Note: It is crucial to use reverse function composition here or to
        -- reverse the flags here as we want to process the flags left to right
        -- but data flow in function compsition is right to left.
        accumFlags flags = foldr (flip (.)) id [ f | Right f <- flags ]

data CommandParse flags = CommandHelp (String -> String)
                        | CommandList [String]
                        | CommandErrors [String]
                        | CommandReadyToGo flags
instance Functor CommandParse where
  fmap _ (CommandHelp help)       = CommandHelp help
  fmap _ (CommandList opts)       = CommandList opts
  fmap _ (CommandErrors errs)     = CommandErrors errs
  fmap f (CommandReadyToGo flags) = CommandReadyToGo (f flags)


data Command action = Command String String ([String] -> CommandParse action)

commandAddAction :: CommandUI flags
                 -> (flags -> [String] -> action)
                 -> Command action
commandAddAction command action =
  Command (commandName command)
          (commandSynopsis command)
          (fmap (uncurry applyDefaultArgs)
         . commandParseArgs command False)

  where applyDefaultArgs mkflags args =
          let flags = mkflags (commandDefaultFlags command)
           in action flags args

commandsRun :: CommandUI a
            -> [Command action]
            -> [String]
            -> CommandParse (a, CommandParse action)
commandsRun globalCommand commands args =
  case commandParseArgs globalCommand' True args of
    CommandHelp      help          -> CommandHelp help
    CommandList      opts          -> CommandList (opts ++ commandNames)
    CommandErrors    errs          -> CommandErrors errs
    CommandReadyToGo (mkflags, args') -> case args' of
      (name:cmdArgs) -> case lookupCommand name of
        [Command _ _ action] -> CommandReadyToGo (flags, action cmdArgs)
        _                    -> CommandReadyToGo (flags, badCommand name)
      []                     -> CommandReadyToGo (flags, noCommand)
      where flags = mkflags (commandDefaultFlags globalCommand)

  where
    lookupCommand cname = [ cmd | cmd@(Command cname' _ _) <- commands
                          , cname'==cname ]
    noCommand        = CommandErrors ["no command given (try --help)\n"]
    badCommand cname = CommandErrors ["unrecognised command: " ++ cname
                                   ++ " (try --help)\n"]
    commandNames   = [ name | Command name _ _ <- commands ]
    globalCommand' = globalCommand {
      commandUsage = \pname ->
           "Usage: " ++ pname ++ " [GLOBAL FLAGS]\n"
        ++ "   or: " ++ pname ++ " COMMAND [FLAGS]\n\n"
        ++ "Global flags:",
      commandDescription = Just $ \pname ->
           "Commands:\n"
        ++ unlines [ "  " ++ align name ++ "    " ++ description
                   | Command name description _ <- commands ]
        ++ case commandDescription globalCommand of
             Nothing   -> ""
             Just desc -> '\n': desc pname
    }
      where maxlen = maximum [ length name | Command name _ _ <- commands]
            align str = str ++ replicate (maxlen - length str) ' '
