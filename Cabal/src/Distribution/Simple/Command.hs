{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Command
-- Copyright   :  Duncan Coutts 2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  non-portable (ExistentialQuantification)
--
-- This is to do with command line handling. The Cabal command line is
-- organised into a number of named sub-commands (much like darcs). The
-- 'CommandUI' abstraction represents one of these sub-commands, with a name,
-- description, a set of flags. Commands can be associated with actions and
-- run. It handles some common stuff automatically, like the @--help@ and
-- command line completion flags. It is designed to allow other tools make
-- derived commands. This feature is used heavily in @cabal-install@.
module Distribution.Simple.Command
  ( -- * Command interface
    CommandUI (..)
  , commandShowOptions
  , CommandParse (..)
  , commandParseArgs
  , getNormalCommandDescriptions
  , helpCommandUI

    -- ** Constructing commands
  , ShowOrParseArgs (..)
  , usageDefault
  , usageAlternatives
  , mkCommandUI
  , hiddenCommand

    -- ** Associating actions with commands
  , Command
  , commandAddAction
  , noExtraFlags

    -- ** Building lists of commands
  , CommandType (..)
  , CommandSpec (..)
  , commandFromSpec

    -- ** Running commands
  , commandsRun
  , commandsRunWithFallback
  , defaultCommandFallback

    -- * Option Fields
  , OptionField (..)
  , Name

    -- ** Constructing Option Fields
  , option
  , multiOption

    -- ** Liftings & Projections
  , liftOption
  , liftOptionL

    -- * Option Descriptions
  , OptDescr (..)
  , Description
  , SFlags
  , LFlags
  , OptFlags
  , ArgPlaceHolder

    -- ** OptDescr 'smart' constructors
  , MkOptDescr
  , reqArg
  , reqArg'
  , optArg
  , optArg'
  , optArgDef'
  , noArg
  , boolOpt
  , boolOpt'
  , choiceOpt
  , choiceOptFromEnum
  ) where

import Distribution.Compat.Prelude hiding (get)
import Prelude ()

import qualified Data.Array as Array
import qualified Data.List as List
import Distribution.Compat.Lens (ALens', (#~), (^#))
import qualified Distribution.GetOpt as GetOpt
import Distribution.ReadE
import Distribution.Simple.Utils

data CommandUI flags = CommandUI
  { commandName :: String
  -- ^ The name of the command as it would be entered on the command line.
  -- For example @\"build\"@.
  , commandSynopsis :: String
  -- ^ A short, one line description of the command to use in help texts.
  , commandUsage :: String -> String
  -- ^ A function that maps a program name to a usage summary for this
  -- command.
  , commandDescription :: Maybe (String -> String)
  -- ^ Additional explanation of the command to use in help texts.
  , commandNotes :: Maybe (String -> String)
  -- ^ Post-Usage notes and examples in help texts
  , commandDefaultFlags :: flags
  -- ^ Initial \/ empty flags
  , commandOptions :: ShowOrParseArgs -> [OptionField flags]
  -- ^ All the Option fields for this command
  }

data ShowOrParseArgs = ShowArgs | ParseArgs
type Name = String
type Description = String

-- | We usually have a data type for storing configuration values, where
--   every field stores a configuration option, and the user sets
--   the value either via command line flags or a configuration file.
--   An individual OptionField models such a field, and we usually
--   build a list of options associated to a configuration data type.
data OptionField a = OptionField
  { optionName :: Name
  , optionDescr :: [OptDescr a]
  }

-- | An OptionField takes one or more OptDescrs, describing the command line
-- interface for the field.
data OptDescr a
  = ReqArg
      Description
      OptFlags
      ArgPlaceHolder
      (ReadE (a -> a))
      (a -> [String])
  | OptArg
      Description
      OptFlags
      ArgPlaceHolder
      (ReadE (a -> a))
      (String, a -> a)
      (a -> [Maybe String])
  | ChoiceOpt [(Description, OptFlags, a -> a, a -> Bool)]
  | BoolOpt
      Description
      OptFlags {-True-}
      OptFlags {-False-}
      (Bool -> a -> a)
      (a -> Maybe Bool)

-- | Short command line option strings
type SFlags = [Char]

-- | Long command line option strings
type LFlags = [String]

type OptFlags = (SFlags, LFlags)
type ArgPlaceHolder = String

-- | Create an option taking a single OptDescr.
--   No explicit Name is given for the Option, the name is the first LFlag given.
--
-- Example: @'option' sf lf d get set@
-- * @sf@: Short option name, for example: @[\'d\']@. No hyphen permitted.
-- * @lf@: Long option name, for example: @["debug"]@. No hyphens permitted.
-- * @d@: Description of the option, shown to the user in help messages.
-- * @get@: Get the current value of the flag.
-- * @set@: Set the value of the flag. Gets the current value of the flag as a
--          parameter.
option
  :: SFlags
  -> LFlags
  -> Description
  -> get
  -> set
  -> MkOptDescr get set a
  -> OptionField a
option sf lf@(n : _) d get set arg = OptionField n [arg sf lf d get set]
option _ _ _ _ _ _ =
  error $
    "Distribution.command.option: "
      ++ "An OptionField must have at least one LFlag"

-- | Create an option taking several OptDescrs.
--   You will have to give the flags and description individually to the
--   OptDescr constructor.
multiOption
  :: Name
  -> get
  -> set
  -> [get -> set -> OptDescr a]
  -- ^ MkOptDescr constructors partially
  --  applied to flags and description.
  -> OptionField a
multiOption n get set args = OptionField n [arg get set | arg <- args]

type MkOptDescr get set a =
  SFlags
  -> LFlags
  -> Description
  -> get
  -> set
  -> OptDescr a

-- | Create a string-valued command line interface.
-- Usually called in the context of 'option' or 'multiOption'.
--
-- Example: @'reqArg' ad mkflag showflag@
--
-- * @ad@: Placeholder shown to the user, e.g. @"FILES"@ if files are expected
--         parameters.
-- * @mkflag@: How to parse the argument into the option.
-- * @showflag@: If parsing goes wrong, display a useful error message to
--               the user.
reqArg
  :: Monoid b
  => ArgPlaceHolder
  -> ReadE b
  -> (b -> [String])
  -> MkOptDescr (a -> b) (b -> a -> a) a
reqArg ad mkflag showflag sf lf d get set =
  ReqArg
    d
    (sf, lf)
    ad
    (fmap (\a b -> set (get b `mappend` a) b) mkflag)
    (showflag . get)

-- | Create a string-valued command line interface with a default value.
optArg
  :: Monoid b
  => ArgPlaceHolder
  -> ReadE b
  -> (String, b)
  -> (b -> [Maybe String])
  -> MkOptDescr (a -> b) (b -> a -> a) a
optArg ad mkflag (dv, mkDef) showflag sf lf d get set =
  OptArg
    d
    (sf, lf)
    ad
    (fmap (\a b -> set (get b `mappend` a) b) mkflag)
    (dv, \b -> set (get b `mappend` mkDef) b)
    (showflag . get)

-- | (String -> a) variant of "reqArg"
reqArg'
  :: Monoid b
  => ArgPlaceHolder
  -> (String -> b)
  -> (b -> [String])
  -> MkOptDescr (a -> b) (b -> a -> a) a
reqArg' ad mkflag showflag =
  reqArg ad (succeedReadE mkflag) showflag

-- | (String -> a) variant of "optArg"
optArg'
  :: Monoid b
  => ArgPlaceHolder
  -> (Maybe String -> b)
  -> (b -> [Maybe String])
  -> MkOptDescr (a -> b) (b -> a -> a) a
optArg' ad mkflag showflag =
  optArg ad (succeedReadE (mkflag . Just)) ("", mkflag Nothing) showflag

optArgDef'
  :: Monoid b
  => ArgPlaceHolder
  -> (String, Maybe String -> b)
  -> (b -> [Maybe String])
  -> MkOptDescr (a -> b) (b -> a -> a) a
optArgDef' ad (dv, mkflag) showflag =
  optArg ad (succeedReadE (mkflag . Just)) (dv, mkflag Nothing) showflag

noArg :: Eq b => b -> MkOptDescr (a -> b) (b -> a -> a) a
noArg flag sf lf d = choiceOpt [(flag, (sf, lf), d)] sf lf d

boolOpt
  :: (b -> Maybe Bool)
  -> (Bool -> b)
  -> SFlags
  -> SFlags
  -> MkOptDescr (a -> b) (b -> a -> a) a
boolOpt g s sfT sfF _sf _lf@(n : _) d get set =
  BoolOpt d (sfT, ["enable-" ++ n]) (sfF, ["disable-" ++ n]) (set . s) (g . get)
boolOpt _ _ _ _ _ _ _ _ _ =
  error
    "Distribution.Simple.Setup.boolOpt: unreachable"

boolOpt'
  :: (b -> Maybe Bool)
  -> (Bool -> b)
  -> OptFlags
  -> OptFlags
  -> MkOptDescr (a -> b) (b -> a -> a) a
boolOpt' g s ffT ffF _sf _lf d get set = BoolOpt d ffT ffF (set . s) (g . get)

-- | create a Choice option
choiceOpt
  :: Eq b
  => [(b, OptFlags, Description)]
  -> MkOptDescr (a -> b) (b -> a -> a) a
choiceOpt aa_ff _sf _lf _d get set = ChoiceOpt alts
  where
    alts = [(d, flags, set alt, (== alt) . get) | (alt, flags, d) <- aa_ff]

-- | create a Choice option out of an enumeration type.
--   As long flags, the Show output is used. As short flags, the first character
--   which does not conflict with a previous one is used.
choiceOptFromEnum
  :: (Bounded b, Enum b, Show b, Eq b)
  => MkOptDescr (a -> b) (b -> a -> a) a
choiceOptFromEnum _sf _lf d get =
  choiceOpt
    [ (x, (sf, [map toLower $ show x]), d')
    | (x, sf) <- sflags'
    , let d' = d ++ show x
    ]
    _sf
    _lf
    d
    get
  where
    sflags' = foldl f [] [firstOne ..]
    f prev x =
      let prevflags = concatMap snd prev
       in prev
            ++ take
              1
              [ (x, [toLower sf])
              | sf <- show x
              , isAlpha sf
              , toLower sf `notElem` prevflags
              ]
    firstOne = minBound `asTypeOf` get undefined

commandGetOpts
  :: ShowOrParseArgs
  -> CommandUI flags
  -> [GetOpt.OptDescr (flags -> flags)]
commandGetOpts showOrParse command =
  concatMap viewAsGetOpt (commandOptions command showOrParse)

viewAsGetOpt :: OptionField a -> [GetOpt.OptDescr (a -> a)]
viewAsGetOpt (OptionField _n aa) = concatMap optDescrToGetOpt aa
  where
    optDescrToGetOpt (ReqArg d (cs, ss) arg_desc set _) =
      [GetOpt.Option cs ss (GetOpt.ReqArg (runReadE set) arg_desc) d]
    optDescrToGetOpt (OptArg d (cs, ss) arg_desc set (dv, def) _) =
      [GetOpt.Option cs ss (GetOpt.OptArg dv set' arg_desc) d]
      where
        set' Nothing = Right def
        set' (Just txt) = runReadE set txt
    optDescrToGetOpt (ChoiceOpt alts) =
      [GetOpt.Option sf lf (GetOpt.NoArg set) d | (d, (sf, lf), set, _) <- alts]
    optDescrToGetOpt (BoolOpt d (sfT, lfT) ([], []) set _) =
      [GetOpt.Option sfT lfT (GetOpt.NoArg (set True)) d]
    optDescrToGetOpt (BoolOpt d ([], []) (sfF, lfF) set _) =
      [GetOpt.Option sfF lfF (GetOpt.NoArg (set False)) d]
    optDescrToGetOpt (BoolOpt d (sfT, lfT) (sfF, lfF) set _) =
      [ GetOpt.Option sfT lfT (GetOpt.NoArg (set True)) ("Enable " ++ d)
      , GetOpt.Option sfF lfF (GetOpt.NoArg (set False)) ("Disable " ++ d)
      ]

getCurrentChoice :: OptDescr a -> a -> [String]
getCurrentChoice (ChoiceOpt alts) a =
  [lf | (_, (_sf, lf : _), _, currentChoice) <- alts, currentChoice a]
getCurrentChoice _ _ = error "Command.getChoice: expected a Choice OptDescr"

liftOption :: (b -> a) -> (a -> (b -> b)) -> OptionField a -> OptionField b
liftOption get' set' opt =
  opt{optionDescr = liftOptDescr get' set' `map` optionDescr opt}

-- | @since 3.4.0.0
liftOptionL :: ALens' b a -> OptionField a -> OptionField b
liftOptionL l = liftOption (^# l) (l #~)

liftOptDescr :: (b -> a) -> (a -> (b -> b)) -> OptDescr a -> OptDescr b
liftOptDescr get' set' (ChoiceOpt opts) =
  ChoiceOpt
    [ (d, ff, liftSet get' set' set, (get . get'))
    | (d, ff, set, get) <- opts
    ]
liftOptDescr get' set' (OptArg d ff ad set (dv, mkDef) get) =
  OptArg
    d
    ff
    ad
    (liftSet get' set' `fmap` set)
    (dv, liftSet get' set' mkDef)
    (get . get')
liftOptDescr get' set' (ReqArg d ff ad set get) =
  ReqArg d ff ad (liftSet get' set' `fmap` set) (get . get')
liftOptDescr get' set' (BoolOpt d ffT ffF set get) =
  BoolOpt d ffT ffF (liftSet get' set' . set) (get . get')

liftSet :: (b -> a) -> (a -> (b -> b)) -> (a -> a) -> b -> b
liftSet get' set' set x = set' (set $ get' x) x

-- | Show flags in the standard long option command line format
commandShowOptions :: CommandUI flags -> flags -> [String]
commandShowOptions command v =
  concat
    [ showOptDescr v od | o <- commandOptions command ParseArgs, od <- optionDescr o
    ]
  where
    maybePrefix [] = []
    maybePrefix (lOpt : _) = ["--" ++ lOpt]

    showOptDescr :: a -> OptDescr a -> [String]
    showOptDescr x (BoolOpt _ (_, lfTs) (_, lfFs) _ enabled) =
      case enabled x of
        Nothing -> []
        Just True -> maybePrefix lfTs
        Just False -> maybePrefix lfFs
    showOptDescr x c@ChoiceOpt{} =
      ["--" ++ val | val <- getCurrentChoice c x]
    showOptDescr x (ReqArg _ (_ssff, lf : _) _ _ showflag) =
      [ "--" ++ lf ++ "=" ++ flag
      | flag <- showflag x
      ]
    showOptDescr x (OptArg _ (_ssff, lf : _) _ _ _ showflag) =
      [ case flag of
        Just s -> "--" ++ lf ++ "=" ++ s
        Nothing -> "--" ++ lf
      | flag <- showflag x
      ]
    showOptDescr _ _ =
      error "Distribution.Simple.Command.showOptDescr: unreachable"

commandListOptions :: CommandUI flags -> [String]
commandListOptions command =
  concatMap listOption $
    addCommonFlags ShowArgs $ -- This is a slight hack, we don't want
    -- "--list-options" showing up in the
    -- list options output, so use ShowArgs
      commandGetOpts ShowArgs command
  where
    listOption (GetOpt.Option shortNames longNames _ _) =
      ["-" ++ [name] | name <- shortNames]
        ++ ["--" ++ name | name <- longNames]

-- | The help text for this command with descriptions of all the options.
commandHelp :: CommandUI flags -> String -> String
commandHelp command pname =
  commandSynopsis command
    ++ "\n\n"
    ++ commandUsage command pname
    ++ ( case commandDescription command of
          Nothing -> ""
          Just desc -> '\n' : desc pname
       )
    ++ "\n"
    ++ ( if cname == ""
          then "Global flags:"
          else "Flags for " ++ cname ++ ":"
       )
    ++ ( GetOpt.usageInfo ""
          . addCommonFlags ShowArgs
          $ commandGetOpts ShowArgs command
       )
    ++ ( case commandNotes command of
          Nothing -> ""
          Just notes -> '\n' : notes pname
       )
  where
    cname = commandName command

-- | Default "usage" documentation text for commands.
usageDefault :: String -> String -> String
usageDefault name pname =
  "Usage: "
    ++ pname
    ++ " "
    ++ name
    ++ " [FLAGS]\n\n"
    ++ "Flags for "
    ++ name
    ++ ":"

-- | Create "usage" documentation from a list of parameter
--   configurations.
usageAlternatives :: String -> [String] -> String -> String
usageAlternatives name strs pname =
  unlines
    [ start ++ pname ++ " " ++ name ++ " " ++ s
    | let starts = "Usage: " : repeat "   or: "
    , (start, s) <- zip starts strs
    ]

-- | Make a Command from standard 'GetOpt' options.
mkCommandUI
  :: String
  -- ^ name
  -> String
  -- ^ synopsis
  -> [String]
  -- ^ usage alternatives
  -> flags
  -- ^ initial\/empty flags
  -> (ShowOrParseArgs -> [OptionField flags])
  -- ^ options
  -> CommandUI flags
mkCommandUI name synopsis usages flags options =
  CommandUI
    { commandName = name
    , commandSynopsis = synopsis
    , commandDescription = Nothing
    , commandNotes = Nothing
    , commandUsage = usageAlternatives name usages
    , commandDefaultFlags = flags
    , commandOptions = options
    }

-- | Common flags that apply to every command
data CommonFlag = HelpFlag | ListOptionsFlag

commonFlags :: ShowOrParseArgs -> [GetOpt.OptDescr CommonFlag]
commonFlags showOrParseArgs = case showOrParseArgs of
  ShowArgs -> [help]
  ParseArgs -> [help, list]
  where
    help =
      GetOpt.Option
        helpShortFlags
        ["help"]
        (GetOpt.NoArg HelpFlag)
        "Show this help text"
    helpShortFlags = case showOrParseArgs of
      ShowArgs -> ['h']
      ParseArgs -> ['h', '?']
    list =
      GetOpt.Option
        []
        ["list-options"]
        (GetOpt.NoArg ListOptionsFlag)
        "Print a list of command line flags"

addCommonFlags
  :: ShowOrParseArgs
  -> [GetOpt.OptDescr a]
  -> [GetOpt.OptDescr (Either CommonFlag a)]
addCommonFlags showOrParseArgs options =
  map (fmap Left) (commonFlags showOrParseArgs)
    ++ map (fmap Right) options

-- | Parse a bunch of command line arguments
commandParseArgs
  :: CommandUI flags
  -> Bool
  -- ^ Is the command a global or subcommand?
  -> [String]
  -> CommandParse (flags -> flags, [String])
commandParseArgs command global args =
  let options =
        addCommonFlags ParseArgs $
          commandGetOpts ParseArgs command
      order
        | global = GetOpt.RequireOrder
        | otherwise = GetOpt.Permute
   in case GetOpt.getOpt' order options args of
        (flags, _, _, _)
          | any listFlag flags -> CommandList (commandListOptions command)
          | any helpFlag flags -> CommandHelp (commandHelp command)
          where
            listFlag (Left ListOptionsFlag) = True; listFlag _ = False
            helpFlag (Left HelpFlag) = True; helpFlag _ = False
        (flags, opts, opts', [])
          | global || null opts' -> CommandReadyToGo (accum flags, mix opts opts')
          | otherwise -> CommandErrors (unrecognised opts')
        (_, _, _, errs) -> CommandErrors errs
  where
    -- Note: It is crucial to use reverse function composition here or to
    -- reverse the flags here as we want to process the flags left to right
    -- but data flow in function composition is right to left.
    accum flags = foldr (flip (.)) id [f | Right f <- flags]
    unrecognised opts =
      [ "unrecognized "
        ++ "'"
        ++ (commandName command)
        ++ "'"
        ++ " option `"
        ++ opt
        ++ "'\n"
      | opt <- opts
      ]
    -- For unrecognised global flags we put them in the position just after
    -- the command, if there is one. This gives us a chance to parse them
    -- as sub-command rather than global flags.
    mix [] ys = ys
    mix (x : xs) ys = x : ys ++ xs

data CommandParse flags
  = CommandHelp (String -> String)
  | CommandList [String]
  | CommandErrors [String]
  | CommandReadyToGo flags
instance Functor CommandParse where
  fmap _ (CommandHelp help) = CommandHelp help
  fmap _ (CommandList opts) = CommandList opts
  fmap _ (CommandErrors errs) = CommandErrors errs
  fmap f (CommandReadyToGo flags) = CommandReadyToGo (f flags)

data CommandType = NormalCommand | HiddenCommand
data Command action
  = Command String String ([String] -> CommandParse action) CommandType

-- | Mark command as hidden. Hidden commands don't show up in the 'progname
-- help' or 'progname --help' output.
hiddenCommand :: Command action -> Command action
hiddenCommand (Command name synopsys f _cmdType) =
  Command name synopsys f HiddenCommand

commandAddAction
  :: CommandUI flags
  -> (flags -> [String] -> action)
  -> Command action
commandAddAction command action =
  Command
    (commandName command)
    (commandSynopsis command)
    (fmap (uncurry applyDefaultArgs) . commandParseArgs command False)
    NormalCommand
  where
    applyDefaultArgs mkflags args =
      let flags = mkflags (commandDefaultFlags command)
       in action flags args

-- Print suggested command if edit distance is < 5
badCommand :: [Command action] -> String -> CommandParse a
badCommand commands' cname =
  case eDists of
    [] -> CommandErrors [unErr]
    (s : _) ->
      CommandErrors
        [ unErr
        , "Maybe you meant `" ++ s ++ "`?\n"
        ]
  where
    eDists =
      map fst . List.sortBy (comparing snd) $
        [ (cname', dist)
        | -- Note that this is not commandNames, so close suggestions will show
        -- hidden commands
        (Command cname' _ _ _) <- commands'
        , let dist = editDistance cname' cname
        , dist < 5
        ]
    unErr = "unrecognised command: " ++ cname ++ " (try --help)"

commandsRun
  :: CommandUI a
  -> [Command action]
  -> [String]
  -> IO (CommandParse (a, CommandParse action))
commandsRun globalCommand commands args =
  commandsRunWithFallback globalCommand commands defaultCommandFallback args

defaultCommandFallback
  :: [Command action]
  -> String
  -> [String]
  -> IO (CommandParse action)
defaultCommandFallback commands' name _cmdArgs = pure $ badCommand commands' name

commandsRunWithFallback
  :: CommandUI a
  -> [Command action]
  -> ([Command action] -> String -> [String] -> IO (CommandParse action))
  -> [String]
  -> IO (CommandParse (a, CommandParse action))
commandsRunWithFallback globalCommand commands defaultCommand args =
  case commandParseArgs globalCommand True args of
    CommandHelp help -> pure $ CommandHelp help
    CommandList opts -> pure $ CommandList (opts ++ commandNames)
    CommandErrors errs -> pure $ CommandErrors errs
    CommandReadyToGo (mkflags, args') -> case args' of
      ("help" : cmdArgs) -> handleHelpCommand flags cmdArgs
      (name : cmdArgs) -> case lookupCommand name of
        [Command _ _ action _] ->
          pure $ CommandReadyToGo (flags, action cmdArgs)
        _ -> do
          final_cmd <- defaultCommand commands' name cmdArgs
          return $ CommandReadyToGo (flags, final_cmd)
      [] -> pure $ CommandReadyToGo (flags, noCommand)
      where
        flags = mkflags (commandDefaultFlags globalCommand)
  where
    lookupCommand cname =
      [ cmd | cmd@(Command cname' _ _ _) <- commands', cname' == cname
      ]

    noCommand = CommandErrors ["no command given (try --help)\n"]

    commands' = commands ++ [commandAddAction helpCommandUI undefined]
    commandNames = [name | (Command name _ _ NormalCommand) <- commands']

    -- A bit of a hack: support "prog help" as a synonym of "prog --help"
    -- furthermore, support "prog help command" as "prog command --help"
    handleHelpCommand flags cmdArgs =
      case commandParseArgs helpCommandUI True cmdArgs of
        CommandHelp help -> pure $ CommandHelp help
        CommandList list -> pure $ CommandList (list ++ commandNames)
        CommandErrors _ -> pure $ CommandHelp globalHelp
        CommandReadyToGo (_, []) -> pure $ CommandHelp globalHelp
        CommandReadyToGo (_, (name : cmdArgs')) ->
          case lookupCommand name of
            [Command _ _ action _] ->
              case action ("--help" : cmdArgs') of
                CommandHelp help -> pure $ CommandHelp help
                CommandList _ -> pure $ CommandList []
                _ -> pure $ CommandHelp globalHelp
            _ -> do
              fall_back <- defaultCommand commands' name ("--help" : cmdArgs')
              return $ CommandReadyToGo (flags, fall_back)
      where
        globalHelp = commandHelp globalCommand

-- Levenshtein distance, from https://wiki.haskell.org/Edit_distance
-- (Author: JeanPhilippeBernardy, Simple Permissive Licence)
editDistance :: Eq a => [a] -> [a] -> Int
editDistance xs ys = table Array.! (m, n)
  where
    (m, n) = (length xs, length ys)
    x = Array.array (1, m) (zip [1 ..] xs)
    y = Array.array (1, n) (zip [1 ..] ys)

    table :: Array.Array (Int, Int) Int
    table = Array.array bnds [(ij, dist ij) | ij <- Array.range bnds]
    bnds = ((0, 0), (m, n))

    dist (0, j) = j
    dist (i, 0) = i
    dist (i, j) =
      minimum
        [ table Array.! (i - 1, j) + 1
        , table Array.! (i, j - 1) + 1
        , if x Array.! i == y Array.! j
            then table Array.! (i - 1, j - 1)
            else 1 + table Array.! (i - 1, j - 1)
        ]

-- | Utility function, many commands do not accept additional flags. This
-- action fails with a helpful error message if the user supplies any extra.
noExtraFlags :: [String] -> IO ()
noExtraFlags [] = return ()
noExtraFlags extraFlags =
  dieNoVerbosity $ "Unrecognised flags: " ++ intercalate ", " extraFlags

-- TODO: eliminate this function and turn it into a variant on commandAddAction
--      instead like commandAddActionNoArgs that doesn't supply the [String]

-- | Helper function for creating globalCommand description
getNormalCommandDescriptions :: [Command action] -> [(String, String)]
getNormalCommandDescriptions cmds =
  [ (name, description)
  | Command name description _ NormalCommand <- cmds
  ]

helpCommandUI :: CommandUI ()
helpCommandUI =
  ( mkCommandUI
      "help"
      "Help about commands."
      ["[FLAGS]", "COMMAND [FLAGS]"]
      ()
      (const [])
  )
    { commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " help help\n"
          ++ "    Oh, apparently you already know this.\n"
    }

-- | wraps a @CommandUI@ together with a function that turns it into a @Command@.
-- By hiding the type of flags for the UI allows construction of a list of all UIs at the
-- top level of the program. That list can then be used for generation of manual page
-- as well as for executing the selected command.
data CommandSpec action
  = forall flags. CommandSpec (CommandUI flags) (CommandUI flags -> Command action) CommandType

commandFromSpec :: CommandSpec a -> Command a
commandFromSpec (CommandSpec ui action _) = action ui
