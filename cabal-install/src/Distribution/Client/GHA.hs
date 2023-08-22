module Distribution.Client.GHA (checkIfcurrentlyRunningInGHA) where

import System.Environment (lookupEnv)

checkIfcurrentlyRunningInGHA :: IO Bool
checkIfcurrentlyRunningInGHA = do
  githubActionsEnv <- lookupEnv "GITHUB_ACTIONS"
  pure $ case githubActionsEnv of
    Just "true" -> True
    _ -> False
