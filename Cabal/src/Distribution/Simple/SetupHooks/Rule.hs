{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Distribution.Simple.SetupHooks.Rule
--
-- Internal module that defines fine-grained rules for setup hooks.
-- Users should import 'Distribution.Simple.SetupHooks' instead.
module Distribution.Simple.SetupHooks.Rule
  ( -- * Rules

    -- ** Rule
    Rule (..)
  , RuleId (..)
  , staticRule
  , dynamicRule

    -- ** Commands
  , RuleCommands (..)
  , Command (..)
  , runCommand
  , mkCommand
  , Dict (..)

    -- *** Helpers for executing commands
  , RuleCmds
  , RuleDynDepsCmd
  , RuleExecCmd
  , DynDepsCmd (..)
  , DepsRes (..)
  , ruleDepsCmd
  , runRuleDynDepsCmd
  , ruleExecCmd
  , runRuleExecCmd

    -- ** Collections of rules
  , Rules (..)
  , Dependency (..)
  , RuleOutput (..)
  , rules
  , noRules

    -- ** Rule inputs/outputs
  , Location

    -- ** File/directory monitoring
  , MonitorFilePath (..)
  , MonitorKindFile (..)
  , MonitorKindDir (..)

    -- *** Monadic API for generation of 'ActionId'
  , RulesM
  , RulesT (..)
  , RulesEnv (..)
  , computeRules
  )
where

import qualified Distribution.Compat.Binary as Binary
import Distribution.Compat.Prelude

import Distribution.Simple.FileMonitor.Types
import Distribution.Types.UnitId
import Distribution.Utils.ShortText
  ( ShortText
  )
import Distribution.Verbosity
  ( Verbosity
  )

import Control.Monad.Fix
  ( MonadFix
  )
import Control.Monad.Trans
  ( MonadIO
  , MonadTrans (..)
  )
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.Writer.CPS as Writer
#else
import qualified Control.Monad.Trans.Writer.Strict as Writer
#endif
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
  ( empty
  )

import qualified Data.Kind as Hs
import Data.Type.Equality
  ( (:~:) (Refl)
  , (:~~:) (HRefl)
  )
import Data.Typeable
  ( eqT
  )
import GHC.Show (showCommaSpace)
import GHC.StaticPtr
import System.IO.Unsafe
  ( unsafePerformIO
  )
import qualified Type.Reflection as Typeable
  ( SomeTypeRep (..)
  , TypeRep
  , eqTypeRep
  , typeRep
  , typeRepKind
  , withTypeable
  )

--------------------------------------------------------------------------------

{- Note [Fine-grained hooks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To best understand how the framework of fine-grained build rules
fits into Cabal and the greater Haskell ecosystem, it is helpful to think
that we want build tools (such as cabal-install or HLS) to be able to call
individual build rules on-demand, so that e.g. when a user modifies a .xyz file
the associated preprocessor is re-run.

To do this, we need to perform two different kinds of invocations:

  Query: query the package for the rules that it provides, with their
         dependency information. This allows one to determine when each
         rule should be rerun.

         (For example, if one rule preprocesses *.xyz into *.hs, we need to
         re-run the rule whenever *.xyz is modified.)

  Run: run the relevant action, once one has determined that the rule
       has gone stale.

To do this, any Cabal package with Hooks build-type provides a SetupHooks
module which supports these queries; for example it can be compiled into
a separate executable which can be invoked in the manner described above.
-}

---------
-- Rules

-- | A unique identifier for a t'Rule'.
data RuleId = RuleId
  { ruleUnitId :: !UnitId
  , ruleName :: !ShortText
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, Structured)

-- | A rule consists of:
--
--  - an action to run to execute the rule,
--  - a description of the rule inputs and outputs.
--
-- Use 'staticRule' or 'dynamicRule' to construct a rule, overriding specific
-- fields, rather than directly using the 'Rule' constructor.
data Rule
  = -- | Please use the 'staticRule' or 'dynamicRule' smart constructors
    -- instead of this constructor, in order to avoid relying on internal
    -- implementation details.
    Rule
    { ruleCommands :: !RuleCmds
    -- ^ To run this rule, which t'Command's should we execute?
    , staticDependencies :: ![Dependency]
    -- ^ Static dependencies of this rule.
    , results :: !(NE.NonEmpty Location)
    -- ^ Results of this rule.
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

-- | A rule with static dependencies.
--
-- Prefer using this smart constructor instead of v'Rule' whenever possible.
staticRule
  :: Typeable arg
  => Command arg (IO ())
  -> [Dependency]
  -> NE.NonEmpty Location
  -> Rule
staticRule cmd dep res =
  Rule
    { ruleCommands = StaticRuleCommand{staticRuleCommand = cmd}
    , staticDependencies = dep
    , results = res
    }

-- | A rule with dynamic dependencies.
--
-- Prefer using this smart constructor instead of v'Rule' whenever possible.
dynamicRule
  :: (Typeable depsArg, Typeable depsRes, Typeable arg)
  => StaticPtr (Dict (Binary depsRes, Show depsRes, Eq depsRes))
  -> Command depsArg (IO ([Dependency], depsRes))
  -> Command arg (depsRes -> IO ())
  -> [Dependency]
  -> NE.NonEmpty Location
  -> Rule
dynamicRule dict depsCmd action dep res =
  Rule
    { ruleCommands =
        DynamicRuleCommands
          { dynamicRuleInstances = dict
          , dynamicDeps = DynDepsCmd{dynDepsCmd = depsCmd}
          , dynamicRuleCommand = action
          }
    , staticDependencies = dep
    , results = res
    }

-----------------------
-- Rule inputs/outputs

-- | A (fully resolved) location of a dependency or result of a rule,
-- consisting of a base directory and of a file path relative to that base
-- directory path.
--
-- In practice, this will be something like @( dir, toFilePath modName )@,
-- where:
--
--  - for a file dependency, @dir@ is one of the Cabal search directories,
--  - for an output, @dir@ is a directory such as @autogenComponentModulesDir@
--    or @componentBuildDir@.
type Location = (FilePath, FilePath)

-- The reason for splitting it up this way is that some pre-processors don't
-- simply generate one output @.hs@ file from one input file, but have
-- dependencies on other generated files (notably @c2hs@, where building one
-- @.hs@ file may require reading other @.chi@ files, and then compiling the
-- @.hs@ file may require reading a generated @.h@ file).
-- In these cases, the generated files need to embed relative path names to each
-- other (eg the generated @.hs@ file mentions the @.h@ file in the FFI imports).
-- This path must be relative to the base directory where the generated files
-- are located; it cannot be relative to the top level of the build tree because
-- the compilers do not look for @.h@ files relative to there, ie we do not use
-- @-I .@, instead we use @-I dist/build@ (or whatever dist dir has been set
-- by the user).

-- | A dependency of a rule.
data Dependency
  = -- | A dependency on an output of another rule.
    RuleDependency !RuleOutput
  | -- | A direct dependency on a file at a particular location on disk.
    --
    -- This should not be used for files that are generated by other rules;
    -- use 'RuleDependency' instead.
    FileDependency !Location
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, Structured)

-- | A reference to an output of another rule.
data RuleOutput = RuleOutput
  { outputOfRule :: !RuleId
  -- ^ which rule's outputs are we referring to?
  , outputIndex :: !Word
  -- ^ which particular output of that rule?
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, Structured)

---------
-- Rules

-- | Monad for constructing rules.
type RulesM a = RulesT IO a

-- | The environment within the monadic API.
data RulesEnv = RulesEnv
  { rulesEnvVerbosity :: !Verbosity
  , rulesEnvUnitId :: !UnitId
  }

-- | Monad transformer for defining rules. Usually wraps the 'IO' monad,
-- allowing @IO@ actions to be performed using @liftIO@.
newtype RulesT m a = RulesT
  { runRulesT
      :: Reader.ReaderT
          RulesEnv
          ( State.StateT
              (Map RuleId Rule)
              (Writer.WriterT [MonitorFilePath] m)
          )
          a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFix)

instance MonadTrans RulesT where
  lift = RulesT . lift . lift . lift

-- | A collection of t'Rule's.
--
-- Use the 'rules' smart constructor instead of directly using the v'Rules'
-- constructor.
--
--   - Rules are registered using 'registerRule',
--   - Monitored files or directories are declared using 'addRuleMonitors';
--     a change in these will trigger the recomputation of all rules.
--
-- The @env@ type parameter represents an extra argument, which usually
-- consists of information known to Cabal such as 'LocalBuildInfo' and
-- 'ComponentLocalBuildInfo'.
newtype Rules env = Rules {runRules :: env -> RulesM ()}

-- | __Warning__: this 'Semigroup' instance is not commutative.
instance Semigroup (Rules env) where
  (Rules rs1) <> (Rules rs2) =
    Rules $ \inputs -> do
      y1 <- rs1 inputs
      y2 <- rs2 inputs
      return $ y1 <> y2

instance Monoid (Rules env) where
  mempty = Rules $ const noRules

-- | An empty collection of rules.
noRules :: RulesM ()
noRules = return ()

-- | Construct a collection of rules.
--
-- Usage:
--
-- > myRules :: Rules env
-- > myRules = rules $ static f
-- >   where
-- >     f :: env -> RulesM ()
-- >     f env = do { ... } -- use the monadic API here
rules
  :: StaticPtr (env -> RulesM ())
  -- ^ a static computation of rules
  -> Rules env
rules f = Rules $ \env -> RulesT $ do
  Reader.withReaderT (\rulesEnv -> rulesEnv{rulesEnvUnitId = unitId}) $
    runRulesT $
      deRefStaticPtr f env
  where
    unitId = mkUnitId $ spInfoUnitId $ staticPtrInfo f

-- | Internal function: run the monadic 'Rules' computations in order
-- to obtain all the 'Rule's with their 'RuleId's.
computeRules
  :: Verbosity
  -> env
  -> Rules env
  -> IO (Map RuleId Rule, [MonitorFilePath])
computeRules verbosity inputs (Rules rs) = do
  -- Bogus UnitId to start with. This will be the first thing
  -- to be set when users use the 'rules' smart constructor.
  let noUnitId = mkUnitId ""
      env0 =
        RulesEnv
          { rulesEnvVerbosity = verbosity
          , rulesEnvUnitId = noUnitId
          }
  Writer.runWriterT $
    (`State.execStateT` Map.empty) $
      (`Reader.runReaderT` env0) $
        runRulesT $
          rs inputs

------------
-- Commands

-- | A command consists of a statically-known action together with a
-- (possibly dynamic) argument to that action.
--
-- For example, the action can consist of running an executable
-- (such as @happy@ or @c2hs@), while the argument consists of the variable
-- component of the command, e.g. the specific file to run @happy@ on.
data Command arg res = Command
  { actionPtr :: !(StaticPtr (arg -> res))
  -- ^ The (statically-known) action to execute.
  , actionArg :: !arg
  -- ^ The (possibly dynamic) argument to pass to the action.
  , cmdInstances :: !(StaticPtr (Dict (Binary arg, Show arg)))
  -- ^ Static evidence that the argument can be serialised and deserialised.
  }

-- | Construct a command.
--
-- Prefer using this smart constructor instead of v'Command' whenever possible.
mkCommand
  :: forall arg res
   . StaticPtr (Dict (Binary arg, Show arg))
  -> StaticPtr (arg -> res)
  -> arg
  -> Command arg res
mkCommand dict actionPtr arg =
  Command
    { actionPtr = actionPtr
    , actionArg = arg
    , cmdInstances = dict
    }

-- | Run a 'Command'.
runCommand :: Command args res -> res
runCommand (Command{actionPtr = ptr, actionArg = arg}) =
  deRefStaticPtr ptr arg

-- | Commands to execute a rule:
--
--   - for a rule with static dependencies, a single command,
--   - for a rule with dynamic dependencies, a command for computing dynamic
--     dependencies, and a command for executing the rule.
data
  RuleCommands
    (deps :: Hs.Type -> Hs.Type -> Hs.Type)
    (ruleCmd :: Hs.Type -> Hs.Type -> Hs.Type)
  where
  -- | A rule with statically-known dependencies.
  StaticRuleCommand
    :: forall arg deps ruleCmd
     . Typeable arg
    => { staticRuleCommand :: !(ruleCmd arg (IO ()))
        -- ^ The command to execute the rule.
       }
    -> RuleCommands deps ruleCmd
  DynamicRuleCommands
    :: forall depsArg depsRes arg deps ruleCmd
     . (Typeable depsArg, Typeable depsRes, Typeable arg)
    => { dynamicRuleInstances :: !(StaticPtr (Dict (Binary depsRes, Show depsRes, Eq depsRes)))
        -- ^ A rule with dynamic dependencies, which consists of two parts:
        --
        --  - a dynamic dependency computation, that returns additional edges to
        --    be added to the build graph together with an additional piece of data,
        --  - the command to execute the rule itself, which receives the additional
        --    piece of data returned by the dependency computation.
       , -- \^ Static evidence used for serialisation, in order to pass the result
         -- of the dependency computation to the main rule action.
         dynamicDeps :: !(deps depsArg depsRes)
        -- ^ A dynamic dependency computation. The resulting dependencies
        -- will be injected into the build graph, and the result of the computation
        -- will be passed on to the command that executes the rule.
       , dynamicRuleCommand :: !(ruleCmd arg (depsRes -> IO ()))
        -- ^ The command to execute the rule. It will receive the result
        -- of the dynamic dependency computation.
       }
    -> RuleCommands deps ruleCmd

-- | A placeholder for a command that has been omitted, e.g. when we don't
-- care about serialising/deserialising one particular command in a datatype.
data NoCmd arg res = CmdOmitted
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (Binary)

-- | A dynamic dependency command.
newtype DynDepsCmd depsArg depsRes = DynDepsCmd {dynDepsCmd :: Command depsArg (IO ([Dependency], depsRes))}
  deriving newtype (Show, Eq, Binary)

-- | The result of a dynamic dependency computation.
newtype DepsRes depsArg depsRes = DepsRes {depsRes :: depsRes}
  deriving newtype (Show, Eq, Binary)

-- | Both the rule command and the (optional) dynamic dependency command.
type RuleCmds = RuleCommands DynDepsCmd Command

-- | Only the (optional) dynamic dependency command.
type RuleDynDepsCmd = RuleCommands DynDepsCmd NoCmd

-- | The rule command together with the result of the (optional) dynamic
-- dependency computation.
type RuleExecCmd = RuleCommands DepsRes Command

-- | Project out the (optional) dependency computation command, so that
-- it can be serialised without serialising anything else.
ruleDepsCmd :: RuleCmds -> RuleDynDepsCmd
ruleDepsCmd = \case
  StaticRuleCommand{staticRuleCommand = _ :: Command args (IO ())} ->
    StaticRuleCommand{staticRuleCommand = CmdOmitted :: NoCmd args (IO ())}
  DynamicRuleCommands
    { dynamicRuleCommand = _ :: Command args (depsRes -> IO ())
    , dynamicRuleInstances = instsPtr
    , dynamicDeps = deps
    } ->
      DynamicRuleCommands
        { dynamicRuleInstances = instsPtr
        , dynamicDeps = deps
        , dynamicRuleCommand = CmdOmitted :: NoCmd args (depsRes -> IO ())
        }

-- | Obtain the (optional) 'IO' action that computes dynamic dependencies.
runRuleDynDepsCmd :: RuleDynDepsCmd -> Maybe (IO ([Dependency], LBS.ByteString))
runRuleDynDepsCmd = \case
  StaticRuleCommand{} -> Nothing
  DynamicRuleCommands
    { dynamicRuleInstances = instsPtr
    , dynamicDeps = DynDepsCmd{dynDepsCmd = depsCmd}
    }
      | Dict <- deRefStaticPtr instsPtr ->
          Just $ do
            (deps, depsRes) <- runCommand depsCmd
            return $ (deps, Binary.encode depsRes)

-- | Project out the command for running the rule, passing in the result of
-- the dependency computation if there was one.
ruleExecCmd :: RuleCmds -> Maybe LBS.ByteString -> RuleExecCmd
ruleExecCmd (StaticRuleCommand{staticRuleCommand = cmd}) _ =
  StaticRuleCommand{staticRuleCommand = cmd}
ruleExecCmd
  ( DynamicRuleCommands
      { dynamicRuleInstances = instsPtr
      , dynamicRuleCommand = cmd :: Command arg (depsRes -> IO ())
      , dynamicDeps = _ :: DynDepsCmd depsArg depsRes
      }
    )
  mbDepsResBinary =
    case mbDepsResBinary of
      Nothing ->
        error $
          unlines
            [ "Missing ByteString argument in 'ruleExecCmd'."
            , "Run 'runRuleDynDepsCmd' on the rule to obtain this data."
            ]
      Just depsResBinary
        | Dict <- deRefStaticPtr instsPtr ->
            DynamicRuleCommands
              { dynamicRuleInstances = instsPtr
              , dynamicRuleCommand = cmd
              , dynamicDeps = DepsRes (Binary.decode depsResBinary) :: DepsRes depsArg depsRes
              }

-- | Obtain the 'IO' action that executes a rule.
runRuleExecCmd :: RuleExecCmd -> IO ()
runRuleExecCmd = \case
  StaticRuleCommand{staticRuleCommand = cmd} -> runCommand cmd
  DynamicRuleCommands{dynamicDeps = DepsRes res, dynamicRuleCommand = cmd} ->
    runCommand cmd res

--------------------------------------------------------------------------------
-- Instances

-- | A wrapper used to pass evidence of a constraint as an explicit value.
data Dict c where
  Dict :: c => Dict c

instance Show (Command arg res) where
  showsPrec prec (Command{actionPtr = cmdPtr, actionArg = arg, cmdInstances = insts})
    | Dict <- deRefStaticPtr insts =
        showParen (prec >= 11) $
          showString "Command {"
            . showString "actionPtrKey = "
            . shows (staticKey cmdPtr)
            . showCommaSpace
            . showString "actionArg = "
            . shows arg
            . showString "}"

instance Eq (Command arg res) where
  Command{actionPtr = cmdPtr1, actionArg = arg1, cmdInstances = insts1}
    == Command{actionPtr = cmdPtr2, actionArg = arg2, cmdInstances = insts2}
      | staticKey cmdPtr1 == staticKey cmdPtr2
      , staticKey insts1 == staticKey insts2
      , Dict <- deRefStaticPtr insts1 =
          Binary.encode arg1 == Binary.encode arg2
      | otherwise =
          False

instance Binary (Command arg res) where
  put (Command{actionPtr = cmdPtr, actionArg = arg, cmdInstances = insts})
    | Dict <- deRefStaticPtr insts =
        do
          put (staticKey cmdPtr)
          put (staticKey insts)
          put arg
  get = do
    cmdKey <- get @StaticKey
    instsKey <- get @StaticKey
    case unsafePerformIO $ unsafeLookupStaticPtr cmdKey of
      Just cmdPtr
        | Just instsPtr <- unsafePerformIO $ unsafeLookupStaticPtr instsKey
        , Dict <- deRefStaticPtr @(Dict (Binary arg, Show arg)) instsPtr ->
            do
              arg <- get
              return $ Command{actionPtr = cmdPtr, actionArg = arg, cmdInstances = instsPtr}
      _ -> error "failed to look up static pointer key for action"

instance
  ( forall arg res. Show (ruleCmd arg res)
  , forall depsArg depsRes. Show depsRes => Show (deps depsArg depsRes)
  )
  => Show (RuleCommands deps ruleCmd)
  where
  showsPrec prec (StaticRuleCommand{staticRuleCommand = cmd}) =
    showParen (prec >= 11) $
      showString "StaticRuleCommand {"
        . showString "staticRuleCommand = "
        . shows cmd
        . showString "}"
  showsPrec
    prec
    ( DynamicRuleCommands
        { dynamicDeps = deps
        , dynamicRuleCommand = cmd
        , dynamicRuleInstances = instsPtr
        }
      )
      | Dict <- deRefStaticPtr instsPtr =
          showParen (prec >= 11) $
            showString "DynamicRuleCommands {"
              . showString "dynamicDeps = "
              . shows deps
              . showCommaSpace
              . showString "dynamicRuleCommand = "
              . shows cmd
              . showString "}"

instance
  ( forall arg res. Eq (ruleCmd arg res)
  , forall depsArg depsRes. Eq depsRes => Eq (deps depsArg depsRes)
  )
  => Eq (RuleCommands deps ruleCmd)
  where
  StaticRuleCommand{staticRuleCommand = ruleCmd1 :: ruleCmd arg1 (IO ())}
    == StaticRuleCommand{staticRuleCommand = ruleCmd2 :: ruleCmd arg2 (IO ())}
      | Just Refl <- eqT @arg1 @arg2 =
          ruleCmd1 == ruleCmd2
  DynamicRuleCommands
    { dynamicDeps = depsCmd1 :: deps depsArg1 depsRes1
    , dynamicRuleCommand = ruleCmd1 :: ruleCmd arg1 (depsRes1 -> IO ())
    , dynamicRuleInstances = instsPtr1
    }
    == DynamicRuleCommands
      { dynamicDeps = depsCmd2 :: deps depsArg2 depsRes2
      , dynamicRuleCommand = ruleCmd2 :: ruleCmd arg2 (depsRes2 -> IO ())
      , dynamicRuleInstances = instsPtr2
      }
      | Just Refl <- eqT @depsArg1 @depsArg2
      , Just Refl <- eqT @depsRes1 @depsRes2
      , Just Refl <- eqT @arg1 @arg2
      , Dict <- deRefStaticPtr instsPtr1 =
          depsCmd1 == depsCmd2
            && ruleCmd1 == ruleCmd2
            && staticKey instsPtr1 == staticKey instsPtr2
  _ == _ = False

instance
  ( forall arg res. Binary (ruleCmd arg res)
  , forall depsArg depsRes. Binary depsRes => Binary (deps depsArg depsRes)
  )
  => Binary (RuleCommands deps ruleCmd)
  where
  put = \case
    StaticRuleCommand{staticRuleCommand = ruleCmd :: ruleCmd arg (IO ())} -> do
      put @Word 0
      put $ Typeable.SomeTypeRep (Typeable.typeRep @arg)
      put ruleCmd
    DynamicRuleCommands
      { dynamicDeps = deps :: deps depsArg depsRes
      , dynamicRuleCommand = ruleCmd :: ruleCmd arg (depsRes -> IO ())
      , dynamicRuleInstances = instsPtr
      } | Dict <- deRefStaticPtr instsPtr ->
        do
          put @Word 1
          put $ Typeable.SomeTypeRep (Typeable.typeRep @depsArg)
          put $ Typeable.SomeTypeRep (Typeable.typeRep @depsRes)
          put $ Typeable.SomeTypeRep (Typeable.typeRep @arg)
          put $ staticKey instsPtr
          put ruleCmd
          put deps
  get = do
    tag <- get @Word
    case tag of
      0 -> do
        Typeable.SomeTypeRep (trArg :: Typeable.TypeRep arg) <- get
        if
            | Just HRefl <- Typeable.eqTypeRep (Typeable.typeRepKind trArg) (Typeable.typeRep @Hs.Type) ->
                do
                  ruleCmd <- get @(ruleCmd arg (IO ()))
                  return $
                    Typeable.withTypeable trArg $
                      StaticRuleCommand
                        { staticRuleCommand = ruleCmd
                        }
            | otherwise ->
                error "internal error when decoding static rule command"
      _ -> do
        Typeable.SomeTypeRep (trDepsArg :: Typeable.TypeRep depsArg) <- get
        Typeable.SomeTypeRep (trDepsRes :: Typeable.TypeRep depsRes) <- get
        Typeable.SomeTypeRep (trArg :: Typeable.TypeRep arg) <- get
        instsKey <- get @StaticKey
        if
            | Just HRefl <- Typeable.eqTypeRep (Typeable.typeRepKind trDepsArg) (Typeable.typeRep @Hs.Type)
            , Just HRefl <- Typeable.eqTypeRep (Typeable.typeRepKind trDepsRes) (Typeable.typeRep @Hs.Type)
            , Just HRefl <- Typeable.eqTypeRep (Typeable.typeRepKind trArg) (Typeable.typeRep @Hs.Type)
            , Just instsPtr <- unsafePerformIO $ unsafeLookupStaticPtr instsKey
            , Dict :: Dict (Binary depsRes, Show depsRes, Eq depsRes) <-
                deRefStaticPtr instsPtr ->
                do
                  ruleCmd <- get @(ruleCmd arg (depsRes -> IO ()))
                  deps <- get @(deps depsArg depsRes)
                  return $
                    Typeable.withTypeable trDepsArg $
                      Typeable.withTypeable trDepsRes $
                        Typeable.withTypeable trArg $
                          DynamicRuleCommands
                            { dynamicDeps = deps
                            , dynamicRuleCommand = ruleCmd
                            , dynamicRuleInstances = instsPtr
                            }
            | otherwise ->
                error "internal error when decoding dynamic rule commands"
