module Distribution.Deprecated.ViewAsFieldDescr
  ( viewAsFieldDescr
  ) where

import Distribution.Client.Compat.Prelude hiding (get)
import Prelude ()

import qualified Data.List.NonEmpty as NE
import Distribution.ReadE (parsecToReadE)
import Distribution.Simple.Command
import Text.PrettyPrint (cat, comma, punctuate, text)
import Text.PrettyPrint as PP (empty)

import Distribution.Deprecated.ParseUtils (FieldDescr (..), runE, syntaxError)

-- | to view as a FieldDescr, we sort the list of interfaces (Req > Bool >
-- Choice > Opt) and consider only the first one.
viewAsFieldDescr :: OptionField a -> FieldDescr a
viewAsFieldDescr (OptionField _n []) =
  error "Distribution.command.viewAsFieldDescr: unexpected"
viewAsFieldDescr (OptionField n (d : dd)) = FieldDescr n get set
  where
    optDescr = head $ NE.sortBy cmp (d :| dd)

    cmp :: OptDescr a -> OptDescr a -> Ordering
    ReqArg{} `cmp` ReqArg{} = EQ
    ReqArg{} `cmp` _ = GT
    BoolOpt{} `cmp` ReqArg{} = LT
    BoolOpt{} `cmp` BoolOpt{} = EQ
    BoolOpt{} `cmp` _ = GT
    ChoiceOpt{} `cmp` ReqArg{} = LT
    ChoiceOpt{} `cmp` BoolOpt{} = LT
    ChoiceOpt{} `cmp` ChoiceOpt{} = EQ
    ChoiceOpt{} `cmp` _ = GT
    OptArg{} `cmp` OptArg{} = EQ
    OptArg{} `cmp` _ = LT

    --    get :: a -> Doc
    get t = case optDescr of
      ReqArg _ _ _ _ ppr ->
        (cat . punctuate comma . map text . ppr) t
      OptArg _ _ _ _ _ ppr ->
        case ppr t of
          [] -> PP.empty
          (Nothing : _) -> text "True"
          (Just a : _) -> text a
      ChoiceOpt alts ->
        fromMaybe PP.empty $
          listToMaybe
            [text lf | (_, (_, lf : _), _, enabled) <- alts, enabled t]
      BoolOpt _ _ _ _ enabled -> (maybe PP.empty pretty . enabled) t

    --    set :: LineNo -> String -> a -> ParseResult a
    set line val a =
      case optDescr of
        ReqArg _ _ _ readE _ -> ($ a) `liftM` runE line n readE val
        -- We parse for a single value instead of a
        -- list, as one can't really implement
        -- parseList :: ReadE a -> ReadE [a] with
        -- the current ReadE definition
        ChoiceOpt{} ->
          case getChoiceByLongFlag optDescr val of
            Just f -> return (f a)
            _ -> syntaxError line val
        BoolOpt _ _ _ setV _ -> (`setV` a) `liftM` runE line n (parsecToReadE ("<viewAsFieldDescr>" ++) parsec) val
        OptArg _ _ _ readE _ _ -> ($ a) `liftM` runE line n readE val

-- Optional arguments are parsed just like
-- required arguments here; we don't
-- provide a method to set an OptArg field
-- to the default value.

getChoiceByLongFlag :: OptDescr a -> String -> Maybe (a -> a)
getChoiceByLongFlag (ChoiceOpt alts) val =
  listToMaybe
    [ set | (_, (_sf, lf : _), set, _) <- alts, lf == val
    ]
getChoiceByLongFlag _ _ =
  error "Distribution.command.getChoiceByLongFlag: expected a choice option"
