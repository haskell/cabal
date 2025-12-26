{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Prelude (String, concat, error, otherwise, show)
import GitHash

panic :: String -> a
panic msg = error panicMsg
  where panicMsg =
          concat [ "[panic ", giBranch gi, "@", giHash gi
                 , " (", giCommitDate gi, ")"
                 , " (", show (giCommitCount gi), " commits in HEAD)"
                 , dirty, "] ", msg ]
        dirty | giDirty gi = " (uncommitted files present)"
              | otherwise   = ""
        gi = $$tGitInfoCwd

main = panic "oh no!"
