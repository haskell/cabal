Cabal 3.16.x.0 changelog and release notes.

This file will be edited and the changes incorporated into the official
3.16.x.0 Cabal and Cabal-syntax release notes.

---

### Migration guide

- `data Flag a = Flag a | NoFlag` has been replaced with `type Flag = Data.Monoid.Last`,
  while `pattern Flag a = Last (Just a)` and `pattern NoFlag = Last Nothing` are provided
  for backward compatibility.
  Imports of form `import Distribution.Simple.Flag (Flag (..))` should be replaced
  with `import Distribution.Simple.Flag (Flag, pattern Flag, pattern NoFlag)`
  (the latter form is backwards compatible with older versions of Cabal).
  Enable `{-# LANGUAGE PatternSynonyms #-}` if required. In the unlikely case
  of defining instances for `Flag`, `{-# LANGUAGE TypeSynonymInstances #-}`
  is needed.
