import Distribution.Simple.Utils

main = topHandler $ readFile "does-not-exist"
