# ForeignOptsCxx

This asserts that cabal passes `cxx-options` to the C++ compiler (and NOT `cc-options`).

Since GHC 8.10, they are passed through GHC with `-optcxx`. Before that, they were passed with `-optc`.

See the additional case `ForeignOptsC`.
