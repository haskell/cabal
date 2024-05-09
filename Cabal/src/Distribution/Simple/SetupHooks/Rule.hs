{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Distribution.Simple.SetupHooks.Rule
--
-- Internal module that defines fine-grained rules for setup hooks.
-- Users should import 'Distribution.Simple.SetupHooks' instead.
module Distribution.Simple.SetupHooks.Rule
  ( -- * Rules

    -- ** Rule
    Rule
  , RuleData (..)
  , RuleId (..)
  , staticRule
  , dynamicRule

    -- ** Commands
  , RuleCommands (..)
  , Command
  , CommandData (..)
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
  , Location (..)
  , location

    -- ** File/directory monitoring
  , MonitorFilePath (..)
  , MonitorKindFile (..)
  , MonitorKindDir (..)

    -- *** Monadic API for generation of 'ActionId'
  , RulesM
  , RulesT (..)
  , RulesEnv (..)
  , computeRules

    -- * Internals
  , Scope (..)
  , SScope (..)
  , Static (..)
  , RuleBinary
  , ruleBinary
  )
where

import qualified Distribution.Compat.Binary as Binary
import Distribution.Compat.Prelude

import Distribution.ModuleName
  ( ModuleName
  )
import Distribution.Simple.FileMonitor.Types
import Distribution.Types.UnitId
import Distribution.Utils.Path
  ( FileOrDir (..)
  , Pkg
  , RelativePath
  , SymbolicPath
  , getSymbolicPath
  , (</>)
  )
import Distribution.Utils.ShortText
  ( ShortText
  )
import Distribution.Utils.Structured
  ( Structure (..)
  , Structured (..)
  , nominalStructure
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
import Data.Type.Bool
  ( If
  )
import Data.Type.Equality
  ( (:~~:) (HRefl)
  , type (==)
  )
import GHC.Show
  ( showCommaSpace
  )
import GHC.StaticPtr
import GHC.TypeLits
  ( Symbol
  )
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
  , pattern App
  )

import System.FilePath
  ( normalise
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
  { ruleNameSpace :: !RulesNameSpace
  , ruleName :: !ShortText
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, Structured)

data RulesNameSpace = RulesNameSpace
  { rulesUnitId :: !UnitId
  , rulesModuleName :: !ModuleName
  , rulesSrcLoc :: !(Int, Int)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, Structured)

-- | Internal function: create a 'RulesNameSpace' from a 'StaticPtrInfo'.
staticPtrNameSpace :: StaticPtrInfo -> RulesNameSpace
staticPtrNameSpace
  StaticPtrInfo
    { spInfoUnitId = unitId
    , spInfoModuleName = modName
    , spInfoSrcLoc = srcLoc
    } =
    RulesNameSpace
      { rulesUnitId = mkUnitId unitId
      , rulesModuleName = fromString modName
      , rulesSrcLoc = srcLoc
      }

-- | 'Rule's are defined with rich types by the package.
--
-- The build system only has a limited view of these; most data consists of
-- opaque 'ByteString's.
--
-- The 'Scope' data-type describes which side of this divide we are on.
data Scope
  = -- | User space (with rich types).
    User
  | -- | Build-system space (manipulation of raw data).
    System

data SScope (scope :: Scope) where
  SUser :: SScope User
  SSystem :: SScope System

type Rule = RuleData User
type RuleBinary = RuleData System

-- | A rule consists of:
--
--  - an action to run to execute the rule,
--  - a description of the rule inputs and outputs.
--
-- Use 'staticRule' or 'dynamicRule' to construct a rule, overriding specific
-- fields, rather than directly using the 'Rule' constructor.
data RuleData (scope :: Scope)
  = -- | Please use the 'staticRule' or 'dynamicRule' smart constructors
    -- instead of this constructor, in order to avoid relying on internal
    -- implementation details.
    Rule
    { ruleCommands :: !(RuleCmds scope)
    -- ^ To run this rule, which t'Command's should we execute?
    , staticDependencies :: ![Dependency]
    -- ^ Static dependencies of this rule.
    , results :: !(NE.NonEmpty Location)
    -- ^ Results of this rule.
    }
  deriving stock (Generic)

deriving stock instance Show (RuleData User)
deriving stock instance Eq (RuleData User)
deriving stock instance Eq (RuleData System)
deriving anyclass instance Binary (RuleData User)
deriving anyclass instance Binary (RuleData System)
deriving anyclass instance Structured (RuleData User)
deriving anyclass instance Structured (RuleData System)

-- | Trimmed down 'Show' instance, mostly for error messages.
instance Show RuleBinary where
  show (Rule{staticDependencies = deps, results = reslts, ruleCommands = cmds}) =
    what ++ ": " ++ showDeps deps ++ " --> " ++ show (NE.toList reslts)
    where
      what = case cmds of
        StaticRuleCommand{} -> "Rule"
        DynamicRuleCommands{} -> "Rule (dyn-deps)"
      showDeps :: [Dependency] -> String
      showDeps ds = "[" ++ intercalate ", " (map showDep ds) ++ "]"
      showDep :: Dependency -> String
      showDep = \case
        RuleDependency (RuleOutput{outputOfRule = rId, outputIndex = i}) ->
          "(" ++ show rId ++ ")[" ++ show i ++ "]"
        FileDependency loc -> show loc

-- | A rule with static dependencies.
--
-- Prefer using this smart constructor instead of v'Rule' whenever possible.
staticRule
  :: forall arg
   . Typeable arg
  => Command arg (IO ())
  -> [Dependency]
  -> NE.NonEmpty Location
  -> Rule
staticRule cmd dep res =
  Rule
    { ruleCommands =
        StaticRuleCommand
          { staticRuleCommand = cmd
          , staticRuleArgRep = Typeable.typeRep @arg
          }
    , staticDependencies = dep
    , results = res
    }

-- | A rule with dynamic dependencies.
--
-- Prefer using this smart constructor instead of v'Rule' whenever possible.
dynamicRule
  :: forall depsArg depsRes arg
   . (Typeable depsArg, Typeable depsRes, Typeable arg)
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
          { dynamicRuleInstances = UserStatic dict
          , dynamicDeps = DynDepsCmd{dynDepsCmd = depsCmd}
          , dynamicRuleCommand = action
          , dynamicRuleTypeRep = Typeable.typeRep @(depsArg, depsRes, arg)
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
-- In practice, this will be something like @'Location' dir ('moduleNameSymbolicPath' mod <.> "hs")@,
-- where:
--
--  - for a file dependency, @dir@ is one of the Cabal search directories,
--  - for an output, @dir@ is a directory such as @autogenComponentModulesDir@
--    or @componentBuildDir@.
data Location where
  Location
    :: { locationBaseDir :: !(SymbolicPath Pkg (Dir baseDir))
        -- ^ Base directory.
       , locationRelPath :: !(RelativePath baseDir File)
        -- ^ File path relative to base directory (including file extension).
       }
    -> Location

instance Eq Location where
  Location b1 l1 == Location b2 l2 =
    (getSymbolicPath b1 == getSymbolicPath b2)
      && (getSymbolicPath l1 == getSymbolicPath l2)
instance Ord Location where
  compare (Location b1 l1) (Location b2 l2) =
    compare
      (getSymbolicPath b1, getSymbolicPath l1)
      (getSymbolicPath b2, getSymbolicPath l2)
instance Binary Location where
  put (Location base loc) = put (base, loc)
  get = Location <$> get <*> get
instance Structured Location where
  structure _ =
    Structure
      tr
      0
      (show tr)
      [
        ( "Location"
        ,
          [ nominalStructure $ Proxy @(SymbolicPath Pkg (Dir (Tok "baseDir")))
          , nominalStructure $ Proxy @(RelativePath (Tok "baseDir") File)
          ]
        )
      ]
    where
      tr = Typeable.SomeTypeRep $ Typeable.typeRep @Location

-- | Get a (relative or absolute) un-interpreted path to a 'Location'.
location :: Location -> SymbolicPath Pkg File
location (Location base rel) = base </> rel

instance Show Location where
  showsPrec p (Location base rel) =
    showParen (p > 5) $
      showString (normalise $ getSymbolicPath base)
        . showString " </> "
        . showString (normalise $ getSymbolicPath rel)

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
  , rulesEnvNameSpace :: !RulesNameSpace
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

-- | Construct a collection of rules with a given label.
--
-- A label for the rules can be constructed using the @static@ keyword,
-- using the @StaticPointers@ extension.
-- NB: separate calls to 'rules' should have different labels.
--
-- Example usage:
--
-- > myRules :: Rules env
-- > myRules = rules (static ()) $ \ env -> do { .. } -- use the monadic API here
rules
  :: StaticPtr label
  -- ^ unique label for this collection of rules
  -> (env -> RulesM ())
  -- ^ the computation of rules
  -> Rules env
rules label = rulesInNameSpace (staticPtrNameSpace $ staticPtrInfo label)

-- | Internal function to create a collection of rules.
--
-- API users should go through the 'rules' function instead.
rulesInNameSpace
  :: RulesNameSpace
  -- ^ rule namespace
  -> (env -> RulesM ())
  -- ^ the computation of rules
  -> Rules env
rulesInNameSpace nameSpace f =
  Rules $ \env -> RulesT $ do
    Reader.withReaderT (\rulesEnv -> rulesEnv{rulesEnvNameSpace = nameSpace}) $
      runRulesT $
        f env

-- | Internal function: run the monadic 'Rules' computations in order
-- to obtain all the 'Rule's with their 'RuleId's.
computeRules
  :: Verbosity
  -> env
  -> Rules env
  -> IO (Map RuleId Rule, [MonitorFilePath])
computeRules verbosity inputs (Rules rs) = do
  -- Bogus namespace to start with. This will be the first thing
  -- to be set when users use the 'rules' smart constructor.
  let noNameSpace =
        RulesNameSpace
          { rulesUnitId = mkUnitId ""
          , rulesModuleName = fromString ""
          , rulesSrcLoc = (0, 0)
          }
      env0 =
        RulesEnv
          { rulesEnvVerbosity = verbosity
          , rulesEnvNameSpace = noNameSpace
          }
  Writer.runWriterT $
    (`State.execStateT` Map.empty) $
      (`Reader.runReaderT` env0) $
        runRulesT $
          rs inputs

------------
-- Commands

-- | A static pointer (in user scope) or its key (in system scope).
data family Static (scope :: Scope) :: Hs.Type -> Hs.Type

newtype instance Static User fnTy = UserStatic {userStaticPtr :: StaticPtr fnTy}
newtype instance Static System fnTy = SystemStatic {userStaticKey :: StaticKey}
  deriving newtype (Eq, Ord, Show, Binary)

systemStatic :: Static User fnTy -> Static System fnTy
systemStatic (UserStatic ptr) = SystemStatic (staticKey ptr)

instance Show (Static User fnTy) where
  showsPrec p ptr = showsPrec p (systemStatic ptr)
instance Eq (Static User fnTy) where
  (==) = (==) `on` systemStatic
instance Ord (Static User fnTy) where
  compare = compare `on` systemStatic
instance Binary (Static User fnTy) where
  put = put . systemStatic
  get = do
    ptrKey <- get @StaticKey
    case unsafePerformIO $ unsafeLookupStaticPtr ptrKey of
      Just ptr -> return $ UserStatic ptr
      Nothing ->
        fail $
          unlines
            [ "Failed to look up static pointer key for action."
            , "NB: Binary instances for 'User' types cannot be used in external executables."
            ]

-- | A command consists of a statically-known action together with a
-- (possibly dynamic) argument to that action.
--
-- For example, the action can consist of running an executable
-- (such as @happy@ or @c2hs@), while the argument consists of the variable
-- component of the command, e.g. the specific file to run @happy@ on.
type Command = CommandData User

-- | Internal datatype used for commands, both for the Hooks API ('Command')
-- and for the build system.
data CommandData (scope :: Scope) (arg :: Hs.Type) (res :: Hs.Type) = Command
  { actionPtr :: !(Static scope (arg -> res))
  -- ^ The (statically-known) action to execute.
  , actionArg :: !(ScopedArgument scope arg)
  -- ^ The (possibly dynamic) argument to pass to the action.
  , cmdInstances :: !(Static scope (Dict (Binary arg, Show arg)))
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
    { actionPtr = UserStatic actionPtr
    , actionArg = ScopedArgument arg
    , cmdInstances = UserStatic dict
    }

-- | Run a 'Command'.
runCommand :: Command args res -> res
runCommand (Command{actionPtr = UserStatic ptr, actionArg = ScopedArgument arg}) =
  deRefStaticPtr ptr arg

-- | Commands to execute a rule:
--
--   - for a rule with static dependencies, a single command,
--   - for a rule with dynamic dependencies, a command for computing dynamic
--     dependencies, and a command for executing the rule.
data
  RuleCommands
    (scope :: Scope)
    (deps :: Scope -> Hs.Type -> Hs.Type -> Hs.Type)
    (ruleCmd :: Scope -> Hs.Type -> Hs.Type -> Hs.Type)
  where
  -- | A rule with statically-known dependencies.
  StaticRuleCommand
    :: forall arg deps ruleCmd scope
     . If
        (scope == System)
        (arg ~ LBS.ByteString)
        (() :: Hs.Constraint)
    => { staticRuleCommand :: !(ruleCmd scope arg (IO ()))
        -- ^ The command to execute the rule.
       , staticRuleArgRep :: !(If (scope == System) Typeable.SomeTypeRep (Typeable.TypeRep arg))
        -- ^ A 'TypeRep' for 'arg'.
       }
    -> RuleCommands scope deps ruleCmd
  DynamicRuleCommands
    :: forall depsArg depsRes arg deps ruleCmd scope
     . If
        (scope == System)
        (depsArg ~ LBS.ByteString, depsRes ~ LBS.ByteString, arg ~ LBS.ByteString)
        (() :: Hs.Constraint)
    => { dynamicRuleInstances :: !(Static scope (Dict (Binary depsRes, Show depsRes, Eq depsRes)))
        -- ^ A rule with dynamic dependencies, which consists of two parts:
        --
        --  - a dynamic dependency computation, that returns additional edges to
        --    be added to the build graph together with an additional piece of data,
        --  - the command to execute the rule itself, which receives the additional
        --    piece of data returned by the dependency computation.
       , -- \^ Static evidence used for serialisation, in order to pass the result
         -- of the dependency computation to the main rule action.
         dynamicDeps :: !(deps scope depsArg depsRes)
        -- ^ A dynamic dependency computation. The resulting dependencies
        -- will be injected into the build graph, and the result of the computation
        -- will be passed on to the command that executes the rule.
       , dynamicRuleCommand :: !(ruleCmd scope arg (depsRes -> IO ()))
        -- ^ The command to execute the rule. It will receive the result
        -- of the dynamic dependency computation.
       , dynamicRuleTypeRep
          :: !( If
                  (scope == System)
                  Typeable.SomeTypeRep
                  (Typeable.TypeRep (depsArg, depsRes, arg))
              )
        -- ^ A 'TypeRep' for the triple @(depsArg,depsRes,arg)@.
       }
    -> RuleCommands scope deps ruleCmd

-- NB: whenever you change this datatype, you **must** also update its
-- 'Structured' instance. The structure hash is used as a handshake when
-- communicating with an external hooks executable.

{- Note [Hooks Binary instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Hooks API is strongly typed: users can declare rule commands with varying
types, e.g.

  staticRule
  :: forall arg
   . Typeable arg
  => Command arg (IO ())
  -> [Dependency]
  -> NE.NonEmpty Location
  -> Rule

allows a user to declare a 'Command' that receives an argument of type 'arg'
of their choosing.

This all makes sense within the Hooks API, but when communicating with an
external build system (such as cabal-install or HLS), these arguments are
treated as opaque blobs of data (in particular if the Hooks are compiled into
a separate executable, then the static pointers that contain the relevant
instances for these user-chosen types can only be dereferenced from within that
executable, and not on the side of the build system).

This means that, to enable Hooks to be communicated between the package and the
build system, we need:

  1. Two representations of rules: one for the package author using the Hooks API,
     and one for the build system.
  2. Compatibility in the 'Binary' instances for these two types. One needs to be
     able to serialise a 'User'-side 'Rule', and de-serialise it on the build system
     into a 'System'-side 'Rule' which contains some opaque bits of data, and
     vice-versa.

(1) is achieved using the 'Scope' parameter to the 'RuleData' datatype.
@Rule = RuleData User@ is the API-side representation, whereas
@RuleBinary = RuleData System@ is the build-system-side representation.

For (2), note that when we serialise a value of known type and known size, e.g.
an 'Int64', we are nevertheless required to also serialise its size. This is because,
on the build-system side, we don't have access to any of the types, and thus don't know
how much to read in order to reconstruct the associated opaque 'ByteString'.
To ensure we always serialise/deserialise including the length of the data,
the 'ScopedArgument' newtype is used, with a custom 'Binary' instance that always
incldues the length. We use this newtype:

  - in the definition of 'CommandData', for arguments to rules,
  - in the definition of 'DepsRes', for the result of dynamic dependency computations.
-}

newtype ScopedArgument (scope :: Scope) arg = ScopedArgument {getArg :: arg}
  deriving newtype (Eq, Ord, Show)

-- | Serialise/deserialise, always including the length of the payload.
instance Binary arg => Binary (ScopedArgument User arg) where
  put (ScopedArgument arg) = put @LBS.ByteString (Binary.encode arg)
  get = do
    dat <- get @LBS.ByteString
    case Binary.decodeOrFail dat of
      Left (_, _, err) -> fail err
      Right (_, _, res) -> return $ ScopedArgument res

-- | Serialise and deserialise a raw ByteString, leaving it untouched.
instance arg ~ LBS.ByteString => Binary (ScopedArgument System arg) where
  put (ScopedArgument arg) = put arg
  get = ScopedArgument <$> get

-- | A placeholder for a command that has been omitted, e.g. when we don't
-- care about serialising/deserialising one particular command in a datatype.
data NoCmd (scope :: Scope) arg res = CmdOmitted
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (Binary)

-- | A dynamic dependency command.
newtype DynDepsCmd scope depsArg depsRes = DynDepsCmd
  { dynDepsCmd
      :: CommandData scope depsArg (IO ([Dependency], depsRes))
  }

deriving newtype instance Show (DynDepsCmd User depsArg depsRes)
deriving newtype instance Eq (DynDepsCmd User depsArg depsRes)
deriving newtype instance Binary (DynDepsCmd User depsArg depsRes)
deriving newtype instance
  (arg ~ LBS.ByteString, depsRes ~ LBS.ByteString)
  => Eq (DynDepsCmd System arg depsRes)
deriving newtype instance
  (arg ~ LBS.ByteString, depsRes ~ LBS.ByteString)
  => Binary (DynDepsCmd System arg depsRes)

-- | The result of a dynamic dependency computation.
newtype DepsRes (scope :: Scope) depsArg depsRes = DepsRes
  { depsRes
      :: ScopedArgument scope depsRes -- See Note [Hooks Binary instances]
  }
  deriving newtype (Show, Eq, Ord)

deriving newtype instance
  Binary (ScopedArgument scope depsRes)
  => Binary (DepsRes scope depsArg depsRes)

-- | Both the rule command and the (optional) dynamic dependency command.
type RuleCmds scope = RuleCommands scope DynDepsCmd CommandData

-- | Only the (optional) dynamic dependency command.
type RuleDynDepsCmd scope = RuleCommands scope DynDepsCmd NoCmd

-- | The rule command together with the result of the (optional) dynamic
-- dependency computation.
type RuleExecCmd scope = RuleCommands scope DepsRes CommandData

-- | Project out the (optional) dependency computation command, so that
-- it can be serialised without serialising anything else.
ruleDepsCmd :: RuleCmds scope -> RuleDynDepsCmd scope
ruleDepsCmd = \case
  StaticRuleCommand
    { staticRuleCommand = _ :: CommandData scope args (IO ())
    , staticRuleArgRep = tr
    } ->
      StaticRuleCommand
        { staticRuleCommand = CmdOmitted :: NoCmd scope args (IO ())
        , staticRuleArgRep = tr
        }
  DynamicRuleCommands
    { dynamicRuleCommand = _ :: CommandData scope args (depsRes -> IO ())
    , dynamicRuleInstances = instsPtr
    , dynamicDeps = deps
    , dynamicRuleTypeRep = tr
    } ->
      DynamicRuleCommands
        { dynamicRuleInstances = instsPtr
        , dynamicDeps = deps
        , dynamicRuleCommand = CmdOmitted :: NoCmd scope args (depsRes -> IO ())
        , dynamicRuleTypeRep = tr
        }

-- | Obtain the (optional) 'IO' action that computes dynamic dependencies.
runRuleDynDepsCmd :: RuleDynDepsCmd User -> Maybe (IO ([Dependency], LBS.ByteString))
runRuleDynDepsCmd = \case
  StaticRuleCommand{} -> Nothing
  DynamicRuleCommands
    { dynamicRuleInstances = UserStatic instsPtr
    , dynamicDeps = DynDepsCmd{dynDepsCmd = depsCmd}
    }
      | Dict <- deRefStaticPtr instsPtr ->
          Just $ do
            (deps, depsRes) <- runCommand depsCmd
            -- See Note [Hooks Binary instances]
            return $ (deps, Binary.encode $ ScopedArgument @User depsRes)

-- | Project out the command for running the rule, passing in the result of
-- the dependency computation if there was one.
ruleExecCmd :: SScope scope -> RuleCmds scope -> Maybe LBS.ByteString -> RuleExecCmd scope
ruleExecCmd
  _
  StaticRuleCommand{staticRuleCommand = cmd, staticRuleArgRep = tr}
  _ =
    StaticRuleCommand{staticRuleCommand = cmd, staticRuleArgRep = tr}
ruleExecCmd
  scope
  DynamicRuleCommands
    { dynamicRuleInstances = instsPtr
    , dynamicRuleCommand = cmd :: CommandData scope arg (depsRes -> IO ())
    , dynamicDeps = _ :: DynDepsCmd scope depsArg depsRes
    , dynamicRuleTypeRep = tr
    }
  mbDepsResBinary =
    case mbDepsResBinary of
      Nothing ->
        error $
          unlines
            [ "Missing ByteString argument in 'ruleExecCmd'."
            , "Run 'runRuleDynDepsCmd' on the rule to obtain this data."
            ]
      Just depsResBinary ->
        case scope of
          SUser
            | Dict <- deRefStaticPtr (userStaticPtr instsPtr) ->
                DynamicRuleCommands
                  { dynamicRuleInstances = instsPtr
                  , dynamicRuleCommand = cmd
                  , dynamicDeps = Binary.decode depsResBinary :: DepsRes User depsArg depsRes
                  , dynamicRuleTypeRep = tr
                  }
          SSystem ->
            DynamicRuleCommands
              { dynamicRuleInstances = instsPtr
              , dynamicRuleCommand = cmd
              , dynamicDeps = DepsRes $ ScopedArgument depsResBinary
              , dynamicRuleTypeRep = tr
              }

-- | Obtain the 'IO' action that executes a rule.
runRuleExecCmd :: RuleExecCmd User -> IO ()
runRuleExecCmd = \case
  StaticRuleCommand{staticRuleCommand = cmd} -> runCommand cmd
  DynamicRuleCommands
    { dynamicDeps = DepsRes (ScopedArgument{getArg = res})
    , dynamicRuleCommand = cmd
    } ->
      runCommand cmd res

--------------------------------------------------------------------------------
-- Instances

-- | A wrapper used to pass evidence of a constraint as an explicit value.
data Dict c where
  Dict :: c => Dict c

instance Show (CommandData User arg res) where
  showsPrec prec (Command{actionPtr = cmdPtr, actionArg = arg, cmdInstances = insts})
    | Dict <- deRefStaticPtr (userStaticPtr insts) =
        showParen (prec >= 11) $
          showString "Command {"
            . showString "actionPtrKey = "
            . shows cmdPtr
            . showCommaSpace
            . showString "actionArg = "
            . shows arg
            . showString "}"

instance Eq (CommandData User arg res) where
  Command{actionPtr = cmdPtr1, actionArg = arg1, cmdInstances = insts1}
    == Command{actionPtr = cmdPtr2, actionArg = arg2, cmdInstances = insts2}
      | cmdPtr1 == cmdPtr2
      , insts1 == insts2
      , Dict <- deRefStaticPtr (userStaticPtr insts1) =
          Binary.encode arg1 == Binary.encode arg2
      | otherwise =
          False
instance arg ~ LBS.ByteString => Eq (CommandData System arg res) where
  Command a1 b1 c1 == Command a2 b2 c2 =
    a1 == a2 && b1 == b2 && c1 == c2

instance Binary (CommandData User arg res) where
  put (Command{actionPtr = cmdPtr, actionArg = arg, cmdInstances = insts})
    | Dict <- deRefStaticPtr (userStaticPtr insts) =
        do
          put cmdPtr
          put insts
          put arg
  get = do
    cmdPtr <- get
    instsPtr <- get
    case deRefStaticPtr @(Dict (Binary arg, Show arg)) $ userStaticPtr instsPtr of
      Dict -> do
        arg <- get
        return $
          Command
            { actionPtr = cmdPtr
            , actionArg = arg
            , cmdInstances = instsPtr
            }
instance arg ~ LBS.ByteString => Binary (CommandData System arg res) where
  put (Command{actionPtr = cmdPtr, actionArg = arg, cmdInstances = insts}) =
    do
      put cmdPtr
      put insts
      put arg
  get = do
    cmdKey <- get
    instsKey <- get
    arg <- get
    return $ Command{actionPtr = cmdKey, actionArg = arg, cmdInstances = instsKey}

instance
  ( forall arg res. Show (ruleCmd User arg res)
  , forall depsArg depsRes. Show depsRes => Show (deps User depsArg depsRes)
  )
  => Show (RuleCommands User deps ruleCmd)
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
        , dynamicRuleInstances = UserStatic instsPtr
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
  ( forall arg res. Eq (ruleCmd User arg res)
  , forall depsArg depsRes. Eq depsRes => Eq (deps User depsArg depsRes)
  )
  => Eq (RuleCommands User deps ruleCmd)
  where
  StaticRuleCommand{staticRuleCommand = ruleCmd1 :: ruleCmd User arg1 (IO ()), staticRuleArgRep = tr1}
    == StaticRuleCommand{staticRuleCommand = ruleCmd2 :: ruleCmd User arg2 (IO ()), staticRuleArgRep = tr2}
      | Just HRefl <- Typeable.eqTypeRep tr1 tr2 =
          ruleCmd1 == ruleCmd2
  DynamicRuleCommands
    { dynamicDeps = depsCmd1 :: deps User depsArg1 depsRes1
    , dynamicRuleCommand = ruleCmd1 :: ruleCmd User arg1 (depsRes1 -> IO ())
    , dynamicRuleInstances = UserStatic instsPtr1
    , dynamicRuleTypeRep = tr1
    }
    == DynamicRuleCommands
      { dynamicDeps = depsCmd2 :: deps User depsArg2 depsRes2
      , dynamicRuleCommand = ruleCmd2 :: ruleCmd User arg2 (depsRes2 -> IO ())
      , dynamicRuleInstances = UserStatic instsPtr2
      , dynamicRuleTypeRep = tr2
      }
      | Just HRefl <- Typeable.eqTypeRep tr1 tr2
      , Dict <- deRefStaticPtr instsPtr1 =
          depsCmd1 == depsCmd2
            && ruleCmd1 == ruleCmd2
            && staticKey instsPtr1 == staticKey instsPtr2
  _ == _ = False

instance
  ( forall res. Eq (ruleCmd System LBS.ByteString res)
  , Eq (deps System LBS.ByteString LBS.ByteString)
  )
  => Eq (RuleCommands System deps ruleCmd)
  where
  StaticRuleCommand c1 d1 == StaticRuleCommand c2 d2 = c1 == c2 && d1 == d2
  DynamicRuleCommands a1 b1 c1 d1 == DynamicRuleCommands a2 b2 c2 d2 =
    a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2
  _ == _ = False

instance
  ( forall arg res. Binary (ruleCmd User arg res)
  , forall depsArg depsRes. Binary depsRes => Binary (deps User depsArg depsRes)
  )
  => Binary (RuleCommands User deps ruleCmd)
  where
  put = \case
    StaticRuleCommand
      { staticRuleCommand = ruleCmd :: ruleCmd User arg (IO ())
      , staticRuleArgRep = tr
      } -> do
        put @Word 0
        put (Typeable.SomeTypeRep tr)
        put ruleCmd
    DynamicRuleCommands
      { dynamicDeps = deps :: deps User depsArg depsRes
      , dynamicRuleCommand = ruleCmd :: ruleCmd User arg (depsRes -> IO ())
      , dynamicRuleInstances = instsPtr
      , dynamicRuleTypeRep = tr
      } | Dict <- deRefStaticPtr (userStaticPtr instsPtr) ->
        do
          put @Word 1
          put (Typeable.SomeTypeRep tr)
          put instsPtr
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
                  ruleCmd <- get @(ruleCmd User arg (IO ()))
                  return $
                    Typeable.withTypeable trArg $
                      StaticRuleCommand
                        { staticRuleCommand = ruleCmd
                        , staticRuleArgRep = trArg
                        }
            | otherwise ->
                error "internal error when decoding static rule command"
      _ -> do
        Typeable.SomeTypeRep (tr :: Typeable.TypeRep ty) <- get
        case tr of
          Typeable.App
            ( Typeable.App
                (Typeable.App (tup3Tr :: Typeable.TypeRep tup3) (trDepsArg :: Typeable.TypeRep depsArg))
                (trDepsRes :: Typeable.TypeRep depsRes)
              )
            (trArg :: Typeable.TypeRep arg)
              | Just HRefl <- Typeable.eqTypeRep tup3Tr (Typeable.typeRep @(,,)) -> do
                  instsPtr <- get
                  case deRefStaticPtr $ userStaticPtr instsPtr of
                    (Dict :: Dict (Binary depsRes, Show depsRes, Eq depsRes)) ->
                      do
                        ruleCmd <- get @(ruleCmd User arg (depsRes -> IO ()))
                        deps <- get @(deps User depsArg depsRes)
                        return $
                          Typeable.withTypeable trDepsArg $
                            Typeable.withTypeable trDepsRes $
                              Typeable.withTypeable trArg $
                                DynamicRuleCommands
                                  { dynamicDeps = deps
                                  , dynamicRuleCommand = ruleCmd
                                  , dynamicRuleInstances = instsPtr
                                  , dynamicRuleTypeRep = tr
                                  }
          _ -> error "internal error when decoding dynamic rule commands"

-- | A token constructor used to define 'Structured' instances on types
-- that involve existential quantification.
data family Tok (arg :: Symbol) :: k

instance
  (Typeable scope, Typeable ruleCmd, Typeable deps)
  => Structured (RuleCommands scope deps ruleCmd)
  where
  structure _ =
    Structure
      tr
      0
      (show tr)
      [
        ( "StaticRuleCommand"
        ,
          [ nominalStructure $ Proxy @(ruleCmd scope (Tok "arg") (IO ()))
          , nominalStructure $ Proxy @(Typeable.TypeRep (Tok "arg" :: Hs.Type))
          ]
        )
      ,
        ( "DynamicRuleCommands"
        ,
          [ nominalStructure $ Proxy @(Static scope (Dict (Binary (Tok "depsRes"), Show (Tok "depsRes"), Eq (Tok "depsRes"))))
          , nominalStructure $ Proxy @(deps scope (Tok "depsArg") (Tok "depsRes"))
          , nominalStructure $ Proxy @(ruleCmd scope (Tok "arg") (Tok "depsRes" -> IO ()))
          , nominalStructure $ Proxy @(Typeable.TypeRep (Tok "depsArg", Tok "depsRes", Tok "arg"))
          ]
        )
      ]
    where
      tr = Typeable.SomeTypeRep $ Typeable.typeRep @(RuleCommands scope deps ruleCmd)

instance
  ( forall res. Binary (ruleCmd System LBS.ByteString res)
  , Binary (deps System LBS.ByteString LBS.ByteString)
  )
  => Binary (RuleCommands System deps ruleCmd)
  where
  put = \case
    StaticRuleCommand{staticRuleCommand = ruleCmd, staticRuleArgRep = sTr} -> do
      put @Word 0
      put sTr
      put ruleCmd
    DynamicRuleCommands
      { dynamicDeps = deps
      , dynamicRuleCommand = ruleCmd
      , dynamicRuleInstances = instsKey
      , dynamicRuleTypeRep = sTr
      } ->
        do
          put @Word 1
          put sTr
          put instsKey
          put ruleCmd
          put deps
  get = do
    tag <- get @Word
    case tag of
      0 -> do
        sTr <- get @Typeable.SomeTypeRep
        ruleCmd <- get
        return $
          StaticRuleCommand
            { staticRuleCommand = ruleCmd
            , staticRuleArgRep = sTr
            }
      _ -> do
        sTr <- get @Typeable.SomeTypeRep
        instsKey <- get
        ruleCmd <- get
        deps <- get
        return $
          DynamicRuleCommands
            { dynamicDeps = deps
            , dynamicRuleCommand = ruleCmd
            , dynamicRuleInstances = instsKey
            , dynamicRuleTypeRep = sTr
            }

--------------------------------------------------------------------------------
-- Showing rules

ruleBinary :: Rule -> RuleBinary
ruleBinary = Binary.decode . Binary.encode
