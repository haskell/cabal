Closure property test

package a-0.1
 :private-build-depends: G0 with (b, d)

package a-0.2
 :build-depends: c
 :private-build-depends: G0 with (b, d)

package b-0.1
 :build-depends: x

package b-0.2
 :build-depends: x, d

package b-0.3
 :build-depends: x, c, d

package c-0.1
 :build-depends: x

package c-0.2
 :build-depends: x, d


Closure property violated by `b == 0.3` and `c == 0.2` THEN closure property is violated.

Need to be able to implicitly introduce c into the private scope so that the closure property holds.
Or otherwise pick an older version of C which does not depend on D

