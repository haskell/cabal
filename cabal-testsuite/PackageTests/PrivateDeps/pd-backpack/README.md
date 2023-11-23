Backpack + Private Dependencies test

exeA
    main-is: Main.hs
    private-build-depends: G1 with (libB == 0.1.0.0)
    private-build-depends: G2 with (libB == 0.2.0.0)
    build-depends: libB == 0.3.0.0
    mixins:
        libA (A as A.G1) requires (AHole as G1.Fill)
        libA (A as A.G2) requires (AHole as G2.Fill)
        libA (A as A.NoScope) requires (AHole as Fill)

libA
    exposed-modules: A
    signatures: AHole
