module Distribution.Client.Types.CurrentCommand where

-- | Tracks what command is being executed, because we need to hide this somewhere
-- for cases that need special handling (usually for error reporting).
data CurrentCommand = InstallCommand | HaddockCommand | BuildCommand | ReplCommand | OtherCommand
  deriving (Show, Eq)
