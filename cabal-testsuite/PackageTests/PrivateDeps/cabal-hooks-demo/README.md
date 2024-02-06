Proof of concept for Cabal hooks compatability scheme, which tests private
dependencies

* `lib01` - The library which defines the hooks interface, which can have different versions.

* `main-prog` - A program written against the `lib01` interface, but needs to work with multiple versions.

* `hooks-exe` - The executable which can be compiled against multiple versions of `lib01`.

* `hooks-lib` - A compability library which depends on all versions of lib01 which are supported and provides conversions between the new and old datatypes.
