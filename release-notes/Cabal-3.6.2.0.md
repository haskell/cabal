### Significant changes


- Make Paths_ modules work with non-standard preludes again [#5962](https://github.com/haskell/cabal/issues/5962)

  - Generate Paths_ module with qualified Data.List.last import so that compatibility with non-standard preludes is not regressed compared to cabal 3.4.

- Windows: redo the fix to breakage caused by new autoconf; the wrong fix made cabal sometimes fail with old autoconf [#7494](https://github.com/haskell/cabal/issues/7494) [#7649](https://github.com/haskell/cabal/issues/7649)

  - Reverts #7510 that failed on Windows when used with pre-generated scripts included in packages such as network, time, process.
  - Adds a subtler fix/workaround for the deficiencies of new autoconf versions on Windows.

### Other changes


- Regenerate bootstrap/ with GHC updated from 8.10.4 to 8.10.7

  - This regenerates bootstrap plans using new package versions and also updating GHC from 8.10.4 to 8.10.7, as requested by GHC HQ.
