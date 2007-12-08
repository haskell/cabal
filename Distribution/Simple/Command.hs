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
  
  -- * Constructing commands
  CommandUI(..),
  ShowOrParseArgs(..),
  makeCommand,
  
  -- * Associating actions with commands
  Command,
  commandAddAction,
  
  -- * Running commands
  CommandParse(..),
  commandsRun,
  ) where

import Distribution.GetOpt

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
    commandEmptyFlags  :: flags,
    -- | All the GetOpt options for this command
    commandOptions     :: ShowOrParseArgs -> [OptDescr (flags -> flags)]
  }

data ShowOrParseArgs = ShowArgs | ParseArgs

-- | The help text for this command with descriptions of all the options.
commandHelp :: CommandUI flags -> String
commandHelp command =
  usageInfo "" (addCommonFlags (commandOptions command ShowArgs))

-- | Make a Command from standard 'GetOpt' options.
makeCommand :: String                         -- ^ name
            -> String                         -- ^ short description
            -> Maybe (String -> String)       -- ^ long description
            -> flags                          -- ^ initial\/empty flags
            -> (ShowOrParseArgs
              -> [OptDescr (flags -> flags)]) -- ^ options
            -> CommandUI flags
makeCommand name shortDesc longDesc emptyFlags options =
  CommandUI {
    commandName        = name,
    commandSynopsis    = shortDesc,
    commandDescription = longDesc,
    commandUsage       = usage,
    commandEmptyFlags  = emptyFlags,
    commandOptions     = options
  }
  where usage pname = "Usage: " ++ pname ++ " " ++ name ++ " [FLAGS]\n\n"
                   ++ "Flags for " ++ name ++ ":"

-- | Common flags that apply to every command
data CommonFlag = HelpFlag

commonFlags :: [OptDescr CommonFlag]
commonFlags =
  [Option ['h', '?'] ["help"] (NoArg HelpFlag) "Show this help text"]

addCommonFlags :: [OptDescr a]
               -> [OptDescr (Either CommonFlag a)]
addCommonFlags options = map (fmapOptDesc Left)  commonFlags
                      ++ map (fmapOptDesc Right) options
  where fmapOptDesc f (Option s l d m) = Option s l (fmapArgDesc f d) m
        fmapArgDesc f (NoArg a) = NoArg (f a)
        fmapArgDesc f (ReqArg s d) = ReqArg (f . s) d
        fmapArgDesc f (OptArg s d) = OptArg (f . s) d


commandParseArgs :: CommandUI flags -> [String] -> CommandParse (flags, [String])
commandParseArgs command args =
  let options = addCommonFlags (commandOptions command ParseArgs) in
  case getOpt RequireOrder options args of
    (flags, _,    [])
      | not (null [ () | Left HelpFlag <- flags ])
                      -> CommandHelp help
    (flags, opts, []) -> CommandReadyToGo (accumFlags flags , opts)
    (_,     _,  errs) -> CommandErrors errs

  where help pname = commandUsage command pname
                  ++ commandHelp command
                  ++ case commandDescription command of
                       Nothing   -> ""
                       Just desc -> '\n': desc pname
        accumFlags flags = foldr (.) id [ f | Right f <- flags ]
                             (commandEmptyFlags command)

data CommandParse flags = CommandHelp (String -> String)
                        | CommandErrors [String]
                        | CommandReadyToGo flags
instance Functor CommandParse where
  fmap _ (CommandHelp help)       = CommandHelp help
  fmap _ (CommandErrors errs)     = CommandErrors errs
  fmap f (CommandReadyToGo flags) = (CommandReadyToGo (f flags))


data Command action = Command String String ([String] -> CommandParse action)

commandAddAction :: CommandUI flags
                 -> (flags -> [String] -> action)
                 -> Command action
commandAddAction command action =
  Command (commandName command)
          (commandSynopsis command)
          (fmap (uncurry action) . commandParseArgs command)

commandsRun :: CommandUI a
            -> [Command action]
            -> [String]
            -> CommandParse (a, CommandParse action)
commandsRun globalCommand commands args =
  case commandParseArgs globalCommand args of
    CommandHelp      _             -> CommandHelp globalHelp
    CommandErrors    errs          -> CommandErrors errs
    CommandReadyToGo (flags, name:args') ->
      case lookupCommand name of
        [Command _ _ action]       -> CommandReadyToGo (flags, action args')
        _                          -> CommandReadyToGo (flags, badCommand name)
    (CommandReadyToGo (flags, [])) -> CommandReadyToGo (flags, noCommand)

  where
    lookupCommand cname = [ cmd | cmd@(Command cname' _ _) <- commands
                          , cname'==cname ]
    noCommand        = CommandErrors ["no command given (try --help)\n"]
    badCommand cname = CommandErrors ["unrecognised command: " ++ cname
                                   ++ " (try --help)\n"]
    globalHelp pname =
          "Usage: " ++ pname ++ " [GLOBAL FLAGS]\n"
       ++ "   or: " ++ pname ++ " COMMAND [FLAGS]\n\n"
       ++ "Global flags:"
       ++ commandHelp globalCommand
       ++ "\nCommands:\n"
       ++ unlines [ "  " ++ align name ++ "    " ++ description
                  | Command name description _ <- commands ]
       ++ case commandDescription globalCommand of
            Nothing   -> ""
            Just desc -> '\n': desc pname

      where maxlen = maximum [ length name | Command name _ _ <- commands]
            align str = str ++ replicate (maxlen - length str) ' '
