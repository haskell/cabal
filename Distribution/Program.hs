module Distribution.Program( Program(..)
                           , ProgramLocation(..)
                           , ProgramConfiguration
                           , withProgramFlag
                           , programOptsFlag
                           , programOptsField
                           , defaultProgramConfiguration
                           , lookupProgram) where
{-
                           , ghcProgram
                           , ghcPkgProgram
                           , nhcProgram
                           , hugsProgram
                           , alexProgram
                           , hsc2hsProgram
                           , c2hsProgram
                           , cpphsProgram
                           , haddockProgram
                           , greencardProgram
                           , ldProgram
                           , cppProgram
                           , pfesetupProgram
-}

import Data.List(find)
import Distribution.Compat.Directory(findExecutable)

-- |Represents a program which cabal may call.
data Program
    = Program { -- |The simple name of the program, eg ghc
               programName :: String
                -- |The name of this program's binary, eg ghc-6.4
              ,programBinName :: String
                -- |Default command-line args for this program
              ,programDefaultArgs :: [String]
                -- |Location of the program.  eg. /usr/bin/ghc-6.4
              ,programLocation :: ProgramLocation
              } deriving (Read, Show)

-- |Similar to Maybe, but tells us whether it's specifed by user or
-- not.
data ProgramLocation = EmptyLocation
                     | UserSpecified FilePath
                     | FoundOnSystem FilePath
      deriving (Read, Show)
type ProgramConfiguration = [Program]

defaultProgramConfiguration :: ProgramConfiguration
defaultProgramConfiguration = [ ghcProgram
                              , ghcPkgProgram
                              , nhcProgram
                              , hugsProgram
                              , alexProgram
                              , hsc2hsProgram
                              , c2hsProgram
                              , cpphsProgram
                              , haddockProgram
                              , greencardProgram
                              , ldProgram
                              , cppProgram
                              , pfesetupProgram
                              ]

-- |The flag for giving a path to this program.  eg --with-alex=/usr/bin/alex
withProgramFlag :: Program -> String
withProgramFlag Program{programName=n} = "with-" ++ n

-- |The flag for giving args for this program.
--  eg --haddock-options=-s http://foo
programOptsFlag :: Program -> String
programOptsFlag Program{programName=n} = n ++ "-options"

-- |The foo.cabal field for  giving args for this program.
--  eg haddock-options: -s http://foo
programOptsField :: Program -> String
programOptsField = programOptsFlag

-- ------------------------------------------------------------
-- * cabal programs
-- ------------------------------------------------------------

ghcProgram :: Program
ghcProgram = simpleProgram "ghc"

ghcPkgProgram :: Program
ghcPkgProgram = simpleProgram "ghc-pkg"

nhcProgram :: Program
nhcProgram = simpleProgram "nhc"

hugsProgram :: Program
hugsProgram = simpleProgram "hugs"

alexProgram :: Program
alexProgram = simpleProgram "alex"

hsc2hsProgram :: Program
hsc2hsProgram = simpleProgram "hsc2hs"

c2hsProgram :: Program
c2hsProgram = simpleProgram "c2hs"

cpphsProgram :: Program
cpphsProgram = simpleProgram "cpphs"

haddockProgram :: Program
haddockProgram = simpleProgram "haddock"

greencardProgram :: Program
greencardProgram = simpleProgram "greencard"

ldProgram :: Program
ldProgram = simpleProgram "ld"

cppProgram :: Program
cppProgram = simpleProgram "cpp"

pfesetupProgram :: Program
pfesetupProgram = simpleProgram "pfesetup"

-- ------------------------------------------------------------
-- * helpers
-- ------------------------------------------------------------

-- |Looks up a program in the given configuration.  If the user
-- provides a location, then we use that location in the returned
-- program.  If no location is given then we check in the
-- configuration for a location.  If there's none in the
-- configuration, then we use IO to look on the system.  Do we want a
-- way to specify NOT to find it on the system (populate
-- programLocation).

lookupProgram :: String -- simple name of program
              -> ProgramConfiguration
              -> ProgramLocation  -- find location on system in PATH, if EmptyLocation
              -> IO (Maybe Program) -- the full program
lookupProgram name conf inLoc =
  case lookupProgram' name conf of
    Nothing   -> return Nothing
    Just p@Program{ programLocation= configLoc
                  , programBinName = binName}
        -> do newLoc <- case (inLoc, configLoc) of
                         (EmptyLocation, EmptyLocation)
                             -> do maybeLoc <- findExecutable binName
                                   return $ maybe EmptyLocation FoundOnSystem maybeLoc
                         (EmptyLocation, a) -> return a
                         (a, _)             -> return a
              return $ Just p{programLocation=newLoc}


-- |Populate the "programLocation" field in this configuration.
{-lookupAllPrograms :: ProgramConfiguration
                  -> CommandLine?
                  -> IO ProgramConfiguration
lookupAllPrograms conf = mapM 
-}
-- ------------------------------------------------------------
-- * Internal helpers
-- ------------------------------------------------------------

-- Export?
lookupProgram' :: String -> ProgramConfiguration -> Maybe Program
lookupProgram' name = find (\(Program {programName=n}) -> n == name)

simpleProgram :: String -> Program
simpleProgram s = Program s s [] EmptyLocation

