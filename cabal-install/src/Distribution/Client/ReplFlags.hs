module Distribution.Client.ReplFlags (EnvFlags(..), ReplFlags(..), topReplOptions, multiReplOption, defaultReplFlags) where

import Prelude ()
import Distribution.Client.Compat.Prelude


import Distribution.Client.Setup
         ( liftOptions )
import Distribution.Simple.Setup
         ( ReplOptions(..), replOptions
         , Flag(..), toFlag, falseArg, boolOpt, trueArg )
import Distribution.Simple.Command
         ( option
         , ShowOrParseArgs, OptionField, reqArg, liftOption )
import Distribution.Parsec
         ( parsecCommaList )
import Distribution.ReadE
         ( ReadE, parsecToReadE )
import Distribution.Types.Dependency
         ( Dependency(..) )

data EnvFlags = EnvFlags
  { envPackages :: [Dependency]
  , envIncludeTransitive :: Flag Bool
  }

instance Semigroup EnvFlags where
  (EnvFlags a1 a2) <> (EnvFlags b1 b2) = EnvFlags (a1 <> b1) (a2 <> b2)

instance Monoid EnvFlags where
  mempty = defaultEnvFlags

defaultEnvFlags :: EnvFlags
defaultEnvFlags = EnvFlags
  { envPackages = []
  , envIncludeTransitive = toFlag True
  }

data ReplFlags = ReplFlags {
    configureReplOptions :: ReplOptions
  , replEnvFlags :: EnvFlags
  , replUseMulti :: Flag Bool
  , replKeepTempFiles :: Flag Bool
  }

instance Semigroup ReplFlags where
  (ReplFlags a1 a2 a3 a4) <> (ReplFlags b1 b2 b3 b4) = ReplFlags (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

instance Monoid ReplFlags where
  mempty = defaultReplFlags

defaultReplFlags :: ReplFlags
defaultReplFlags = ReplFlags { configureReplOptions = mempty
                             , replEnvFlags = defaultEnvFlags
                             , replUseMulti = NoFlag
                             , replKeepTempFiles = NoFlag
                             }

topReplOptions :: ShowOrParseArgs -> [OptionField ReplFlags]
topReplOptions showOrParseArgs =
    liftOptions configureReplOptions set1 (replOptions showOrParseArgs) ++
    liftOptions replEnvFlags set2 (envOptions showOrParseArgs) ++
    [ liftOption replUseMulti set3 multiReplOption

    -- keeping temporary files is important functionality for HLS,
    -- which runs @cabal repl@ with fake GHC to get cli arguments.
    -- It will need the temporary files (incl. multi unit repl response files)
    -- to stay, even after the @cabal repl@ command exits.
    --
    , option [] ["keep-temp-files"]
      "Keep temporary files"
      replKeepTempFiles (\b flags -> flags { replKeepTempFiles = b })
      trueArg
    ]
    where
      set1 a x = x { configureReplOptions = a }
      set2 a x = x { replEnvFlags = a }
      set3 a x = x { replUseMulti = a }

multiReplOption :: OptionField (Flag Bool)
multiReplOption =
    option [] ["multi-repl"]
        "multi-component repl sessions"
        id (\v _ -> v)
        (boolOpt [] [])

envOptions :: ShowOrParseArgs -> [OptionField EnvFlags]
envOptions _ =
  [ option ['b'] ["build-depends"]
    "Include additional packages in the environment presented to GHCi."
    envPackages (\p flags -> flags { envPackages = p ++ envPackages flags })
    (reqArg "DEPENDENCIES" dependenciesReadE (fmap prettyShow :: [Dependency] -> [String]))
  , option [] ["no-transitive-deps"]
    "Don't automatically include transitive dependencies of requested packages."
    envIncludeTransitive (\p flags -> flags { envIncludeTransitive = p })
    falseArg
  ]
  where
    dependenciesReadE :: ReadE [Dependency]
    dependenciesReadE =
      parsecToReadE
        ("couldn't parse dependencies: " ++)
        (parsecCommaList parsec)
