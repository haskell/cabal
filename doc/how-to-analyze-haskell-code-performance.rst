How to analyze Haskell performance
==================================

When a Haskell application is slow or uses too much memory,
Cabal and `GHC <https://downloads.haskell.org/ghc/latest/docs/users_guide/>`__
can help you understand why.

The main steps are to

1. let GHC insert performance measuring code into your application (or insert them `manually <https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#inserting-cost-centres-by-hand>`__),
2. run the application with the right RTS flags to produce a performance report and
3. visualize and analyze that report.

The process of inserting performance measuring code and collecting performance information
is called "profiling". The main work is done by GHC while Cabal only acts
as a build configuration interface and passes corresponding compiler flags to GHC.

Profiling CPU performance
-------------------------

The first step to build your application, e.g. ``my-app``, with profiling enabled, and
the second step to run it to collect a report, can be done with a single ``cabal run`` command:

.. code-block:: console

      $ cabal run --enable-profiling --profiling-detail=late my-app -- +RTS -pj -RTS
      <program runs and finishes>

Finally, a profiling JSON report is written to a ``<app-name>.prof`` file,
i.e. ``my-app.prof``, in the current directory.
Load the profiling report file  ``my-app.prof`` into a visualizer
and look for performance bottlenecks. One popular open-source
`flame graph <https://www.brendangregg.com/flamegraphs.html>`__
visualizer is
`Speedscope <https://speedscope.app>`__,
which runs in the browser and can open this JSON file directly.
See the
`Haskell Optimization Handbook <https://haskell.foundation/hs-opt-handbook.github.io>`__
on how to optimize your code based on the profiling results afterwards.

The ``cabal run`` command above is essentially a shorthand for

.. code-block:: console

    $ cabal build --enable-profiling --profiling-detail=late my-app
    $ cabal list-bin my-app
    /path/to/my-app
    $ /path/to/my-app +RTS -pj -RTS
    <program runs and finishes>

In the ``cabal build`` command the first general ``--enable-profiling`` build flag tells GHC
to insert performance measuring code into your application,
while the second profiling option flag ``--profiling-detail=late`` further instructs GHC to use so-called
`late-cost-center profiling <https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#ghc-flag--fprof-late>`__
and insert measuring code only after important optimisations have been applied to your application code.
This reduces the performance slow-down of profiling itself and gives you more realistic measurements.
Further in-depth information on profiling with GHC and its compiler options
can be found in the
`GHC profiling guide <https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html>`__

.. note::

    Cabal comes with a few convenient :ref:`profiling options <profiling-options>`.
    However, you can also pass a lot more
    `profiling compiler options <https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#compiler-options-for-profiling>`__
    to GHC via the ``ghc-prof-options`` field in your ``my-app.cabal`` file.

    Add the following, recommended profiling options to your ``my-app.cabal`` file
    to control where performance measuring code is inserted into your application:

    ::

        executable my-app
          ...
          ghc-prof-options:
            -fprof-auto
            -fno-prof-count-entries
            -fprof-auto-calls
            ...

    You can find more information and further options in the
    `GHC "cost-center" guide <https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#automatically-placing-cost-centres>`__.

In the ``/path/to/my-app +RTS -pj -RTS`` command we run the application with the necessary
`runtime system (RTS) option <https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html>`__
``-pj`` to actually produce a
`"profile JSON" (pj) file report <https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#rts-flag--pj>`__.
The configuration options for many other report output formats can be found in the
`GHC format documentation. <https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#time-and-allocation-profiling>`__.

Profiling your dependencies too
-------------------------------

The setup so far only profiles your main application, which is usually what you want.
This happens by default, because Cabal command line options only apply to local packages
and dependencies are usually not local.
However, the bottlenecks may be in your dependencies, so you would want to profile those too.

First, to enable ``late``-cost-center profiling of all packages/dependencies in your project,
add the following to your project’s ``cabal.project`` file:

.. code-block:: cabal

    package *
        profiling: true
        profiling-detail: late

Second, rerun your application with ``cabal run``, which also automatically rebuilds your application:

.. code-block:: console

    $ cabal run my-app -- +RTS -pj -RTS
    Resolving dependencies...
    Build profile: -w ghc-9.10.1 -O1
    In order, the following will be built (use -v for more details):
     - base64-bytestring-1.2.1.0 (lib)  --enable-profiling (requires build)
     - cryptohash-sha256-0.11.102.1 (lib)  --enable-profiling (requires build)
     ...

There's no need to pass profiling flags like ``--enable-profiling``
to the build or run commands manually this time (as seen in the build log),
because these settings are now determined via the ``cabal.project`` file.

You should now find more information in the profiling report ``my-app.prof``
to analyze. More information on how to configure Cabal options can be found in the
:ref:`Cabal options sections <package-configuration-options>`.
