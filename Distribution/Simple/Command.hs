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
    
  -- ** Constructing commands
  ShowOrParseArgs(..),
  makeCommand,

  -- ** Associating actions with commands
  Command,
  commandAddAction,
  
  -- ** Running commands
  CommandParse(..),
  commandsRun,

-- * Option Fields
  OptionField(..), Name,

-- ** Constructing Option Fields
  option, multiOption,

-- ** Liftings & Projections
  liftOption, viewAsFieldDescr,

-- * Option Descriptions
  OptDescr(..), Description, SFlags, LFlags, OptFlags, ArgPlaceHolder,

-- ** OptDescr 'smart' constructors
  MkOptDescr,
  reqArg, reqArg', reqArg'', optArg, optArg', optArg'', noArg,
  boolOpt, boolOpt', choiceOpt, choiceOptFromEnum

  ) where

import Control.Monad
import qualified Control.Exception as Exception
import Data.Char (isAlpha, toLower)
import Data.List (sortBy, isPrefixOf)
import Data.Maybe
import Data.Monoid
import qualified Distribution.GetOpt as GetOpt
import Distribution.Text
         ( Text(parse) )
import Distribution.ParseUtils
import Distribution.ReadE
import Text.PrettyPrint.HughesPJ    ( punctuate, cat, comma, text, empty)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getEnv)

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
    -- | All the Option fields for this command
    commandOptions     :: ShowOrParseArgs -> [OptionField flags]
  }

data ShowOrParseArgs = ShowArgs | ParseArgs

type Name        = String
type Description = String

-- | We usually have a datatype for storing configuration values, where
--   every field stores a configuration option, and the user sets
--   the value either via command line flags or a configuration file.
--   An individual OptionField models such a field, and we usually
--   build a list of options associated to a configuration datatype.
data OptionField a = OptionField {
  optionName        :: Name,
  optionDescr       :: [OptDescr a] }

-- | An OptionField takes one or more OptDescrs, describing the command line interface for the field.
data OptDescr a  = ReqArg Description OptFlags ArgPlaceHolder (ReadE (a->a))         (a -> [String])
                 | OptArg Description OptFlags ArgPlaceHolder (ReadE (a->a)) (a->a)  (a -> [Maybe String])
                 | ChoiceOpt [(Description, OptFlags, a->a, a -> Bool)]
                 | BoolOpt Description OptFlags{-True-} OptFlags{-False-} (Bool -> a->a) (a->Bool)

-- | Short command line option strings
type SFlags   = [Char]
-- | Long command line option strings
type LFlags   = [String]
type OptFlags = (SFlags,LFlags)
type ArgPlaceHolder = String


-- | Create an option taking a single OptDescr.
--   No explicit Name is given for the Option, the name is the first LFlag given.
option :: SFlags -> LFlags -> Description -> get -> set -> MkOptDescr get set a -> OptionField a
option sf lf@(n:_) d get set arg = OptionField n [arg sf lf d get set]
option _ _ _ _ _ _ = error "Distribution.command.option: An OptionField must have at least one LFlag"

-- | Create an option taking several OptDescrs.
--   You will have to give the flags and description individually to the OptDescr constructor.
multiOption :: Name -> get -> set
            -> [get -> set -> OptDescr a]  -- ^MkOptDescr constructors partially applied to flags and description.
            -> OptionField a
multiOption n get set args = OptionField n [arg get set | arg <- args]

type MkOptDescr get set a = SFlags -> LFlags -> Description -> get -> set -> OptDescr a

-- | Create a string-valued command line interface.
reqArg :: Monoid b => ArgPlaceHolder -> ReadE b -> (b -> [String])
                   -> MkOptDescr (a -> b) (b -> a -> a) a
reqArg ad mkflag showflag sf lf d get set =
  ReqArg d (sf,lf) ad (fmap (\a b -> set (get b `mappend` a) b) mkflag) (showflag . get)

-- | Create a string-valued command line interface with a default value.
optArg :: Monoid b => ArgPlaceHolder -> ReadE b -> b -> (b -> [Maybe String])
                   -> MkOptDescr (a -> b) (b -> a -> a) a
optArg ad mkflag def showflag sf lf d get set  =
  OptArg d (sf,lf) ad (fmap (\a b -> set (get b `mappend` a) b) mkflag)
               (\b ->          set (get b `mappend` def) b)
               (showflag . get)

-- | (String -> a) variant of "reqArg"
reqArg' :: Monoid b => ArgPlaceHolder -> (String -> b) -> (b -> [String])
                    -> MkOptDescr (a -> b) (b -> a -> a) a
reqArg' ad mkflag showflag =
    reqArg ad (succeedReadE mkflag) showflag

-- | (String -> a) variant of "optArg"
optArg' :: Monoid b => ArgPlaceHolder -> (Maybe String -> b) -> (b -> [Maybe String])
                    -> MkOptDescr (a -> b) (b -> a -> a) a
optArg' ad mkflag showflag =
    optArg ad (succeedReadE (mkflag . Just)) def showflag
      where def = mkflag Nothing

-- | ReadS variant of "reqArg"
reqArg'' :: Monoid b => ArgPlaceHolder -> ReadS b -> (b -> [String])
                     -> MkOptDescr (a -> b) (b -> a -> a) a
reqArg'' ad mkflag showflag sf lf@(n:_) =
    reqArg ad (readS_to_E' n ad mkflag) showflag sf lf
reqArg'' _ _ _ _ _ = error "Distribution.command.reqArg'': unreachable"

-- | ReadS variant of "optArg"
optArg'' :: Monoid b => ArgPlaceHolder -> (Maybe String -> [(b,String)]) -> (b -> [Maybe String])
                     -> MkOptDescr (a -> b) (b -> a -> a) a
optArg'' ad mkflag showflag sf lf@(n:_) =
    optArg ad (readS_to_E' n ad (mkflag . Just)) def showflag sf lf
      where def = let [(x,"")] = mkflag Nothing in x
optArg'' _ _ _ _ _ = error "Distribution.command.optArg'': unreachable"

noArg :: (Eq b, Monoid b) => b -> MkOptDescr (a -> b) (b -> a -> a) a
noArg flag sf lf d = choiceOpt [(flag, (sf,lf), d)] sf lf d

boolOpt :: (b -> Bool) -> (Bool -> b) -> SFlags -> SFlags -> MkOptDescr (a -> b) (b -> a -> a) a
boolOpt g s sfT sfF _sf _lf@(n:_) d get set =
    BoolOpt d (sfT, ["enable-"++n]) (sfF, ["disable-"++n]) (set.s) (g.get)
boolOpt _ _ _ _ _ _ _ _ _ = error "Distribution.Simple.Setup.boolOpt: unreachable"

boolOpt' :: (b -> Bool) -> (Bool -> b) -> OptFlags -> OptFlags -> MkOptDescr (a -> b) (b -> a -> a) a
boolOpt' g s ffT ffF _sf _lf d get set = BoolOpt d ffT ffF (set.s) (g.get)

-- | create a Choice option
choiceOpt :: Eq b => [(b,OptFlags,Description)] -> MkOptDescr (a -> b) (b -> a -> a) a
choiceOpt aa_ff _sf _lf _d get set  = ChoiceOpt alts
    where alts = [(d,flags, set alt, (==alt) . get) | (alt,flags,d) <- aa_ff]

-- | create a Choice option out of an enumeration type.
--   As long flags, the Show output is used. As short flags, the first character
--   which does not conflict with a previous one is used.
choiceOptFromEnum :: (Bounded b, Enum b, Show b, Eq b) => MkOptDescr (a -> b) (b -> a -> a) a
choiceOptFromEnum _sf _lf d get = choiceOpt [ (x, (sf, [map toLower $ show x]), d')
                                                | (x, sf) <- sflags'
                                                , let d' = d ++ show x]
                                            _sf _lf d get
    where sflags' = foldl f [] [firstOne..]
          f prev x = let prevflags = concatMap snd prev in
                     prev ++ take 1 [(x, [toLower sf]) | sf <- show x, isAlpha sf
                                                       , toLower sf `notElem` prevflags]
          firstOne = minBound `asTypeOf` get undefined

commandGetOpts :: ShowOrParseArgs -> CommandUI flags -> [GetOpt.OptDescr (flags -> flags)]
commandGetOpts showOrParse command =
    concatMap (viewAsGetOpt (commandDefaultFlags command))
              (commandOptions command showOrParse)

viewAsGetOpt :: a -> OptionField a -> [GetOpt.OptDescr (a->a)]
viewAsGetOpt v (OptionField _n aa) = concatMap optDescrToGetOpt aa
  where
    optDescrToGetOpt (ReqArg d (cs,ss) arg_desc set _) =
         [GetOpt.Option cs ss (GetOpt.ReqArg set' arg_desc) d]
             where set' = readEOrFail set
    optDescrToGetOpt (OptArg d (cs,ss) arg_desc set def _) =
         [GetOpt.Option cs ss (GetOpt.OptArg set' arg_desc) d]
             where set' Nothing    = def
                   set' (Just txt) = readEOrFail set txt
    optDescrToGetOpt (ChoiceOpt alts) =
         [GetOpt.Option sf lf (GetOpt.NoArg set) (highlight (isSet v) d)
              | (d,(sf,lf),set,isSet) <- alts ]
    optDescrToGetOpt (BoolOpt d (sfT,lfT) (sfF, lfF) set isSet) =
         [ GetOpt.Option sfT lfT (GetOpt.NoArg (set True))
                             (highlight (isSet v) $ "Enable " ++ d)
         , GetOpt.Option sfF lfF (GetOpt.NoArg (set False))
                             (highlight (not$ isSet v) $ "Disable " ++ d) ]
    highlight False            = id
    highlight True | do_bold   = highlight_bold
                   | otherwise = highlight_textually
    highlight_bold = (start_bold ++) . (++ end_bold)
    highlight_textually = (++ " (default)")

-- For now, use ANSI bold on terminals that we know support it.
-- Otherwise, we add a line of carets under the active expression instead.
-- In particular, on Windows and when running the testsuite (which sets
-- TERM to vt100 for other reasons) we get carets.
-- We really ought to use a proper termcap/terminfo library.
do_bold :: Bool
do_bold = (`isPrefixOf` unsafePerformIO mTerm) `any` ["xterm", "linux"]
    where mTerm = getEnv "TERM" `Exception.catch` \_ -> return "TERM not set"
start_bold :: String
start_bold = "\ESC[1m"
end_bold :: String
end_bold   = "\ESC[0m"


-- | to view as a FieldDescr, we sort the list of interfaces (Req > Bool > Choice > Opt) and consider only the first one.
viewAsFieldDescr :: OptionField a -> FieldDescr a
viewAsFieldDescr (OptionField _n []) = error "Distribution.command.viewAsFieldDescr: unexpected"
viewAsFieldDescr (OptionField n dd) = FieldDescr n get set
    where optDescr = head $ sortBy cmp dd
          ReqArg{}    `cmp` ReqArg{}    = EQ
          ReqArg{}    `cmp` _           = GT
          BoolOpt{}   `cmp` ReqArg{}    = LT
          BoolOpt{}   `cmp` BoolOpt{}   = EQ
          BoolOpt{}   `cmp` _           = GT
          ChoiceOpt{} `cmp` ReqArg{}    = LT
          ChoiceOpt{} `cmp` BoolOpt{}   = LT
          ChoiceOpt{} `cmp` ChoiceOpt{} = EQ
          ChoiceOpt{} `cmp` _           = GT
          OptArg{}    `cmp` OptArg{}    = EQ
          OptArg{}    `cmp` _           = LT
          get t = case optDescr of
                    ReqArg _ _ _ _ ppr ->
                     (cat . punctuate comma . map text . ppr) t
                    OptArg _ _ _ _ _ ppr ->
                     case ppr t of
                        []        -> empty
                        (Nothing : _) -> text "True"
                        (Just a  : _) -> text a
                    ChoiceOpt alts ->
                     fromMaybe empty $ listToMaybe
                         [ text lf | (_,(_,lf:_), _,enabled) <- alts, enabled t]
                    BoolOpt _ _ _ _ enabled -> (text . show . enabled) t
          set line val a =
                  case optDescr of
                    ReqArg _ _ _ readE _    -> ($ a) `liftM` runE line n readE val
                                             -- We parse for a single value instead of a list,
                                             -- as one can't really implement parseList :: ReadE a -> ReadE [a]
                                             -- with the current ReadE definition
                    ChoiceOpt{}             -> case getChoiceByLongFlag optDescr val of
                                                 Just f -> return (f a)
                                                 _      -> syntaxError line val
                    BoolOpt _ _ _ setV _    -> (`setV` a) `liftM` runP line n parse val
                    OptArg _ _ _ _readE _ _ -> -- The behaviour in this case is not clear, and it has no use so far,
                                               -- so we avoid future surprises by not implementing it.
                                               error "Command.optionToFieldDescr: feature not implemented"

getChoiceByLongFlag :: OptDescr b -> String -> Maybe (b->b)
getChoiceByLongFlag (ChoiceOpt alts) val = listToMaybe [ set | (_,(_sf,lf:_), set, _) <- alts
                                                             , lf == val]

getChoiceByLongFlag _ _ = error "Distribution.command.getChoiceByLongFlag: expected a choice option"

getCurrentChoice :: OptDescr a -> a -> [String]
getCurrentChoice (ChoiceOpt alts) a =
    [ lf | (_,(_sf,lf:_), _, currentChoice) <- alts, currentChoice a]

getCurrentChoice _ _ = error "Command.getChoice: expected a Choice OptDescr"


liftOption :: (b -> a) -> (a -> (b -> b)) -> OptionField a -> OptionField b
liftOption get' set' opt = opt { optionDescr = liftOptDescr get' set' `map` optionDescr opt}


liftOptDescr :: (b -> a) -> (a -> (b -> b)) -> OptDescr a -> OptDescr b
liftOptDescr get' set' (ChoiceOpt opts) =
    ChoiceOpt [ (d, ff, liftSet get' set' set , (get . get'))
              | (d, ff, set, get) <- opts]

liftOptDescr get' set' (OptArg d ff ad set def get) =
    OptArg d ff ad (liftSet get' set' `fmap` set) (liftSet get' set' def) (get . get')

liftOptDescr get' set' (ReqArg d ff ad set get) =
    ReqArg d ff ad (liftSet get' set' `fmap` set) (get . get')

liftOptDescr get' set' (BoolOpt d ffT ffF set get) =
    BoolOpt d ffT ffF (liftSet get' set' . set) (get . get')

liftSet :: (b -> a) -> (a -> (b -> b)) -> (a -> a) -> b -> b
liftSet get' set' set x = set' (set $ get' x) x

-- | Show flags in the standard long option command line format
commandShowOptions :: CommandUI flags -> flags -> [String]
commandShowOptions command v = concat
  [ showOptDescr v  od | o <- commandOptions command ParseArgs
                       , od <- optionDescr o]
  where
    showOptDescr :: a -> OptDescr a -> [String]
    showOptDescr x (BoolOpt _ (_,lfT:_) (_,lfF:_) _ enabled)
      = ["--" ++ if enabled x then lfT else lfF]
    showOptDescr x c@ChoiceOpt{}
      = ["--" ++ val | val <- getCurrentChoice c x]
    showOptDescr x (ReqArg _ (_ssff,lf:_) _ _ showflag)
      = [ "--"++lf++"="++flag
        | flag <- showflag x ]
    showOptDescr x (OptArg _ (_ssff,lf:_) _ _ _ showflag)
      = [ case flag of
            Just s  -> "--"++lf++"="++s
            Nothing -> "--"++lf
        | flag <- showflag x ]
    showOptDescr _ _
      = error "Distribution.Simple.Command.showOptDescr: unreachable"


commandListOptions :: CommandUI flags -> [String]
commandListOptions command =
  concatMap listOption $
    addCommonFlags ShowArgs $ -- This is a slight hack, we don't want
                              -- "--list-options" showing up in the
                              -- list options output, so use ShowArgs
      commandGetOpts ShowArgs command
  where
    listOption (GetOpt.Option shortNames longNames _ _) =
         [ "-"  ++ [name] | name <- shortNames ]
      ++ [ "--" ++  name  | name <- longNames ]

-- | The help text for this command with descriptions of all the options.
commandHelp :: CommandUI flags -> String
commandHelp command =
    GetOpt.usageInfo ""
  . addCommonFlags ShowArgs
  $ commandGetOpts ShowArgs command

-- | Make a Command from standard 'GetOpt' options.
makeCommand :: String                         -- ^ name
            -> String                         -- ^ short description
            -> Maybe (String -> String)       -- ^ long description
            -> flags                          -- ^ initial\/empty flags
            -> (ShowOrParseArgs -> [OptionField flags]) -- ^ options
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
              $ commandGetOpts ParseArgs command
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
