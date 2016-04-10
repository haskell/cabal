import Distribution.Simple.Utils

main = topHandler $ dieWithLocation "MyFile.hs" (Just 42) "oopsies"
