.. _spec-history:

==================================================
 Package Description Format Specification History
==================================================

:ref:`pkg-desc` need to specify the version of the
specification they need to be interpreted in via the
:pkg-field:`cabal-version` declaration. The following list describes
changes that occurred in each version of the cabal specification
relative to the respective preceding *published* version.

.. note::

    The sequence of specification version numbers is *not*
    contiguous because it's synchronised with the version of the
    ``Cabal`` library. As a consequence, only *even* versions are
    considered proper published versions of the specification as *odd*
    versions of the ``Cabal`` library denote unreleased development
    branches which have no stability guarantee.

``cabal-version: 3.14``
-----------------------

* Added field :pkg-field:`extra-files` for specifying extra files to be included
  in ``sdist`` without adding any other semantics (compare,
  :pkg-field:`extra-source-files` is tracked by ``cabal build``).

* License fields use identifiers from SPDX License List version
  ``3.25 2024-08-19``.

* The :pkg-field:`build-type` field allows the new build type ``Hooks`` to be
  specified.

``cabal-version: 3.12``
-----------------------

* License fields use identifiers from SPDX License List version
  ``3.23 2024-02-08``.

* The :pkg-field:`autogen-modules` field allows ``PackageInfo_*`` modules to be
  specified.

  .. Note::

     Before ``Cabal-3.12.0.0``, from ``Cabal-3.10.1.0``, ``PackageInfo_*``
     modules were supported irrespective of the :pkg-field:`cabal-version`. From
     ``Cabal-3.12.0.0``, such modules require ``cabal-version: 3.12`` or later.

``cabal-version: 3.8``
----------------------

* Added field ``code-generators`` to :pkg-section:`test-suite` stanzas. This
  enumerates executables (possibly brought into scope by  :pkg-field:`build-tool-depends`) that are run after all other
  preprocessors. These executables are invoked with a target dir for
  output, a sequence of all source directories with source files of
  local lib components that the given test stanza depends on, and
  following a double dash, all options cabal would pass to ghc for a
  build. They are expected to output a newline-separated list of
  generated modules which have been written to the targetdir. This can
  be used for driving doctests and other discover-style tests generated
  from source code.

* Added fields :pkg-field:`extra-libraries-static` and
  :pkg-field:`extra-lib-dirs-static` to allow Haskell libraries to remember
  linker flags needed for fully static linking of system libraries into
  executables.
  The existing field :pkg-field:`pkgconfig-depends` can used to append
  the relevant output of ``pkg-config --libs --static`` to these new fields
  automatically.
  When :pkg-field:`extra-libraries-static` is not given, it defaults to
  :pkg-field:`extra-libraries`.
  When :pkg-field:`extra-lib-dirs-static` is not given, it defaults to
  :pkg-field:`extra-lib-dirs`.

* Wildcard matching has been slightly expanded. Matches are now
  allowed of the form ``foo/**/literalFile``. Prior, double-star
  wildcards required the trailing filename itself be a wildcard.

* Allow the omission of the `type` field in `test-suite` and `benchmark` stanzas
  when the type can be inferred by the presence of `main-is` or `test-module`.

* License fields use identifiers from SPDX License List version
  ``3.16 2022-02-06``

``cabal-version: 3.6``
----------------------

* License fields use identifiers from SPDX License List version
  ``3.10 2020-08-03``

* Add :pkg-field:`hsc2hs-options`

``cabal-version: 3.4``
----------------------


* License fields use identifiers from SPDX License List version
  ``3.9 2020-05-15``

* Dependencies to sublibraries must be specified explicitly,
  even for current package.
  For example: ``build-depends: mypackage:my-sublib``
  This way you can have a sublibrary with the same
  name as some external dependency.

* Remove ``-any`` and ``-none`` syntax for version ranges
  Use ``>=0`` and ``<0`` respectively.

* :pkg-field:`default-language` is optional.
  The Default value is to use the compiler's default language.

* :pkg-field:`mixins` field allow specifying a sublibrary.

``cabal-version: 3.0``
----------------------

* New :pkg-field:`library:visibility` for exposing sublibraries.

* New ``pkg:lib`` and ``pkg:{lib1,lib2}`` syntax in :pkg-field:`build-depends`
  for depending on public sublibraries from other packages.

* Added the :pkg-field:`extra-dynamic-library-flavours` field to specify non-trivial
  variants of dynamic flavours. It is :pkg-field:`extra-library-flavours` but for
  shared libraries. Mainly useful for GHC's RTS library.

* Free text fields (e.g. :pkg-field:`description`) preserve empty lines
  and indentation. In other words, you don't need to add dots for blank lines.

* License fields use identifiers from SPDX License List version
  ``3.6 2019-07-10``

* Remove deprecated ``hs-source-dir``, :pkg-field:`extensions` and
  :pkg-field:`build-tools` fields.

* Common stanzas are now allowed also in the beginning of conditional
  sections.  In other words, the following is valid

    ::

        library
            import deps

            if flag(foo)
                import foo-deps

* Allow redundant leading or trailing commas in package fields with
  optional commas, such as :pkg-field:`library:exposed-modules`

* Require fields with optional commas to consistently omit or place
  commas between elements.

* Changed the behavior of :pkg-field:`extra-bundled-libraries` field. The naming convention
  of dynamic library files (e.g. generated by a custom build script) has
  changed. For library names prefixed with "C", the dynamic library file
  name(s) must be of the form `lib<library-name>.<dyn-library-extension>*`
  instead of the old `libC<library-name>-ghc<ghc-flavour><ghc-version>.<dyn-library-extension>`

* New set-notation syntax for ``==`` and ``^>=`` operators, see
  :pkg-field:`build-depends` field documentation for examples.

* Allow more whitespace in :pkg-field:`mixins` field

* Wildcards are disallowed in :pkg-field:`pkgconfig-depends`,
  Yet the pkgconfig format is relaxed to accept e.g. versions like ``1.1.0h``.

* New :pkg-field:`autogen-includes` for specifying :pkg-field:`install-includes`
  which are autogenerated (e.g. by a ``configure`` script).

* New :pkg-field:`asm-sources` and :pkg-field:`asm-options` fields
  added for supporting bundled foreign routines implemented in
  assembler.

* New :pkg-field:`cmm-sources` and :pkg-field:`cmm-options` fields
  added for supporting bundled foreign primops implemented in
  C--.

``cabal-version: 2.4``
----------------------

* Wildcard matching has been expanded. All previous wildcard
  expressions are still valid; some will match strictly more files
  than before. Specifically:

  * Double-star (``**``) wildcards are now accepted for recursive
    matching immediately before the final slash; they must be followed
    by a filename wildcard (e.g., ``foo/**/*.html`` is valid;
    ``foo/**/bar/*.html`` and ``foo/**/**/*.html``,
    ``foo/**/bar.html`` are all invalid). As ``**`` was an error in
    globs before, this does not affect any existing ``.cabal`` files
    that previously worked.
    (Caveat: Double-star wildcards in :pkg-field:`data-files` directories,
    e.g. ``data-files: data/**/*.csv``,
    `are only supported correctly from Cabal 3.0 <https://github.com/haskell/cabal/issues/6125#issuecomment-1379878419>`_.)


  * Wildcards now match when the pattern's extensions form a suffix of
    the candidate file's extension, rather than requiring strict
    equality (e.g., previously ``*.html`` did not match
    ``foo.en.html``, but now it does).

* License fields use identifiers from SPDX License List version
  ``3.2 2018-07-10``

* Deprecate ``jhc-options`` field.


``cabal-version: 2.2``
----------------------

* New :pkg-section:`common` stanzas and :pkg-field:`import`
  pseudo-field added.

* New :pkg-field:`library:virtual-modules` field added.

* New :pkg-field:`cxx-sources` and :pkg-field:`cxx-options` fields
  added for supporting bundled foreign routines implemented in C++.

* New :pkg-field:`extra-bundled-libraries` field for specifying
  additional custom library objects to be installed.

* Extended ``if`` control structure with support for ``elif`` keyword.

* Changed default rules of :pkg-field:`build-type` field to infer
  "build-type:" for "Simple"/"Custom" automatically.

* :pkg-field:`license` field syntax changed to require SPDX
  expression syntax (using SPDX license list version ``3.0 2017-12-28``).

* Allow redundant leading or trailing commas in package fields (which
  require commas) such as :pkg-field:`build-depends`.


``cabal-version: 2.0``
----------------------

* New :pkg-field:`library:signatures` and :pkg-field:`mixins` fields
  added for supporting :ref:`Backpack`.

* New :pkg-field:`build-tool-depends` field added for adding
  build-time dependencies of executable components.

* New :pkg-field:`custom-setup:autogen-modules` field added for declaring modules
  which are generated at build time.

* Support for new PVP_ caret-style version operator (``^>=``) added to
  :pkg-field:`build-depends`.

* Add support for new :pkg-section:`foreign-library` stanza.

* Add support for :ref:`sublibrary stanzas <sublibs>`.

* New CPP Macro ``CURRENT_PACKAGE_VERSION``.

``cabal-version: 1.24``
-----------------------

* New :pkg-section:`custom-setup` stanza and
  :pkg-field:`custom-setup:setup-depends` field added for specifying dependencies
  of custom ``Setup.hs`` scripts.

* CPP Macros ``VERSION_$pkgname`` and ``MIN_VERSION_$pkgname`` are now
  also generated for the current package.

* New CPP Macros ``CURRENT_COMPONENT_ID`` and ``CURRENT_PACKAGE_KEY``.

* New :pkg-field:`extra-framework-dirs` field added for specifying
  extra locations to find OS X frameworks.

``cabal-version: 1.22``
-----------------------

* New :pkg-field:`library:reexported-modules` field.

* Support for ``-none`` version constraint added to
  :pkg-field:`build-depends`.

* New :pkg-field:`license` type ``ISC`` added.

* Deprecate ``hugs-options`` and ``nhc98-options`` fields.

``cabal-version: 1.20``
-----------------------

* Add support for new :pkg-field:`license-files` field for declaring
  multiple license documents.

* New CPP Macro ``MIN_TOOL_VERSION_$buildtool``.

* New :pkg-field:`license` types ``BSD2`` and ``MPL-2.0`` added.

``cabal-version: 1.18``
-----------------------

* Add support for new :pkg-field:`extra-doc-files` field for
  specifying extra file assets referenced by the Haddock
  documentation.

* New :pkg-field:`license` type ``AGPL`` and ``AGPL-3`` added.

* Add support for specifying a C/C++/obj-C source file in
  :pkg-field:`executable:main-is` field.

* Add ``getSysconfDir`` operation to ``Paths_`` API.

``cabal-version: 1.14``
-----------------------

* New :pkg-section:`benchmark` stanza for describing a package benchmark added.

* ``exitcode-stdio-1.0`` is a valid value of the `type` field in a
  :pkg-section:`benchmark` stanza.

* ``detailed-0.9`` added as a valid value of the `type` field in a
  :pkg-section:`test-suite` stanza.

``cabal-version: 1.12``
-----------------------

* Change the syntax of :pkg-field:`cabal-version` to support the new recommended
  ``cabal-version: x.y`` style

``cabal-version: ==1.10``
-------------------------

* Change the syntax of :pkg-field:`cabal-version` to require a version range of
  the form ``cabal-version: >= x.y``. (Consequently, ``cabal-version: ==1.10``
  is, itself, not valid syntax.)

* New :pkg-field:`default-language` (to specify a default language standard when
  one is not explicitly specified) and :pkg-field:`other-languages`
  (for language standards used by some modules) added.

* New :pkg-field:`default-extensions` (for Haskell language extensions used by
  every module) and :pkg-field:`other-extensions` (for extensions used by some
  modules) added. :pkg-field:`extensions` deprecated.

* New :pkg-section:`test-suite` stanza for describing a package test suite
  added.

* ``exitcode-stdio-1.0`` is a valid value of the `type` field in a
  :pkg-section:`test-suite` stanza.

``cabal-version: ==1.8``
------------------------

* Added support for the :pkg-field:`build-depends` of a
  :pkg-section:`executable` stanza being able to specify the library in the same
  package (if the package provides one) by the name of the package (without a
  version constraint). Cabal then treats the executable as if it were in another
  package that depended on the package providing the executable and the library.

* The syntax for specifying package version ranges is expanded.

* New :pkg-field:`license` types ``MIT`` and versioned ``GPL`` and ``LGPL``
  added.

``cabal-version: ==1.6``
------------------------

* New :pkg-section:`source-repository` stanza for information about the
  location of the package's source code within a source code repository.

* Add support for new :pkg-field:`bug-reports` field, to specify the URL where
  users should direct bug reports.

* Add support for wildcards in :pkg-field:`data-files` and
  :pkg-field:`extra-source-files` fields.

* Add support for ``foo ==1.2.*`` syntax to :pkg-field:`build-depends` field.

* Add support for new :pkg-field:`library:exposed` field (default: ``True``), to
  be able to specify that the package should not be exposed.

* :pkg-field:`cpp-options`, :pkg-field:`cc-options` and :pkg-field:`ld-options`
  fields no longer use ``,`` as a separator.

``cabal-version: ==1.2.1``
--------------------------

* New format for the :pkg-section:`flag`, :pkg-section:`library` and
  :pkg-section:`executable` stanzas.

* Add support for new :pkg-field:`cpp-options` field, to specify options used
  when pre-processing Haskell modules.

* Replace ``nhc-options`` field with ``nhc98-options`` field.

``cabal-version: ==1.2.0``
--------------------------

* The :pkg-field:`cabal-version` field is now required.

* New :pkg-section:`flag` stanza for specifying configuration flags and add
  support for conditional blocks in :pkg-section:`library` and
  :pkg-section:`executable` stanzas.

* Add distinct :pkg-section:`library` stanza.

* New format for :pkg-section:`executable` stanzas.

* Add support for new :pkg-field:`build-type` field (default: ``Custom``), to
  specify the type of build used by this package.

* Add support for new :pkg-field:`build-tools` field, to specify tools needed to
  build the package.

* Add support for new :pkg-field:`pkgconfig-depends` field, to specify
  ``pkg-config`` packages needed to build the package.

* Add support for new :pkg-field:`ghc-shared-options` field, to specify
  additional options for GHC when the package is built as a shared library.

``cabal-version: ==1.1.6``
--------------------------

* Add support for new :pkg-field:`install-includes` field, distinct from the
  :pkg-field:`includes` field, to specify header files from the package.

* Add support for new ``jhc-options`` field, to specify additional options for
  John Meacham's Haskell compiler (JHC).

``cabal-version: ==1.1.4``
--------------------------

* Add support for new optional :pkg-field:`cabal-version` field, to specify the
  version of ``Cabal`` required for the package.

* Add support for new :pkg-field:`author`, :pkg-field:`homepage`,
  :pkg-field:`package-url`, :pkg-field:`synopsis`, :pkg-field:`description` and
  :pkg-field:`category` fields.

* Add support for new :pkg-field:`tested-with` field to specify compilers and
  versions against which the package has been tested.

* Add support for new :pkg-field:`data-files` field to specify files for use by
  the package at run-time.

* Add support for new :pkg-field:`extra-source-files` field to specify files to
  be included in source distributions.

* Add support for new :pkg-field:`extra-tmp-files` field to specify files to
  be removed when cleaning up.

* Replace :pkg-field:`hs-source-dir` field with :pkg-field:`hs-source-dirs`
  field.

* Add support for new :pkg-field:`ghc-prof-options` field, to specify
  additional options for GHC when the package is built with profiling.

* Add support for extension fields beginning with ``x-``.

``Cabal-1.0``
-------------

``Cabal-1.0``, released March 2005, came with a
`specification <https://downloads.haskell.org/~cabal/Cabal-1.0/doc/pkg-spec-html/>`__
for 'The Haskell Cabal: A Common Architecture for Building Applications and
Tools', including `Section 5.2 <https://downloads.haskell.org/~cabal/Cabal-1.0/doc/pkg-spec-html/x611.html#SBI-PKG-DESC>`__
'Package description in the simple build infrastructure'.

.. include:: references.inc
