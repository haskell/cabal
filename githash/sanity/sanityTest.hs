{-# LANGUAGE TemplateHaskell #-}

import System.Exit ( exitFailure )
import GitHash

main = do
    let gitInfo = $$tGitInfoCwdTry
    case gitInfo of
        Left _ -> print gitInfo >> exitFailure
        Right info -> putStrLn . concat $
            ["commit "
            ,take 7 (giHash info)
            ," on "
            ,giBranch info
            ,"\n"
            ]
