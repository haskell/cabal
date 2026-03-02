.. _buildinfo-field-reference:

Field Syntax Reference
======================

Notation
---------------

Field syntax is described as they are in the latest cabal file format version.

* terminals are enclosed in quotes and type set in typewriter script:

  .. math::

      \mathord{``}\mathtt{example}\mathord{"}

* non-terminals are type set in italic:

  .. math::

      \mathit{version\text-range}

* character sets are type set resembling regular expression notation:


  .. math::

      [ \mathord{``}\mathtt{1}\mathord{"} \cdots \mathord{``}\mathtt{9}\mathord{"} ]

  Character set complements have :math:`c` superscript:

  .. math::

      [ \mathord{``}\mathtt{1}\mathord{"} \cdots \mathord{``}\mathtt{9}\mathord{"} ]^c

* repetition is type set using regular expression inspired notation.
  Superscripts tell how many time to repeat:
  The generic notation is :math:`\in[n\ldots5]`, however there
  are common shorthands:
  :math:`\ast` for :math:`\in[0\ldots\infty]` (``many``),
  :math:`+` for :math:`\in[1\ldots\infty]` (``some``),
  :math:`?` for :math:`\in[0\ldots1]` (``optional``).

  Subscripts tell the used separator:

  .. math::

      \mathit{digit}^+_{\mathord{"}\mathtt{.}\mathord{"}}

  Would be ``digit(\.digit)*`` in common regex syntax.

* alternatives are listed in braces separated by vertical bar:

  .. math::

      \{ \mathit{foo} \mid \mathit{bar} \}

  In case of multiple alternatives, the stacked notation is used

  .. math::

      \left\{\begin{gathered}
      \mathit{one} \\
      \mathit{two} \\
      \mathit{three} \\
      \mathit{four} \\
      \mathit{five}
      \end{gathered}\right\}

* parenthesis are used only for grouping:

  .. math::

      \left(\mathit{foo} \mid \mathit{bar}\right)^+

* any amount of spaces, and at least single space are type set using
  :math:`\circ` and :math:`\bullet` respectively.
  They may appear standalone, not only as binary operators.

  .. math::

      \mathit{module} \bullet \mathord{``}\mathtt{as}\mathord{"} \bullet \mathit{module}

* While notation is heavily regular expression inspired, there
  are also fixed points, which allow represent recursive grammars


  .. math::

      \mathbf{fix}\; \mathit{expr}\; \mathbf{in}\; \mathit{digit}
      \mid \mathit{expr} \circ \mathord{``}\mathtt{+}\mathord{"} \circ \mathit{expr}
      \mid \mathord{``}\mathtt{(} \mathord{"} \circ \mathit{expr} \circ \mathord{``}\mathtt{)}\mathord{"}

Lists
-----

Many fields in cabal file format are lists. There are three variations:

Space separated
    Are used for lists of things with simple grammars, for example :pkg-field:`ghc-options`

    .. math::
        {\mathop{\mathit{element}}}^\ast_{\bullet}

Comma separated
    Are used for lists of things with complicated grammars, for example :pkg-field:`build-depends`
    There can be leading or trailing comma (but not both) since ``cabal-version: 2.2``.
    Note, the comma cannot exist alone.

    .. math::
        \mathrm{commalist}(\mathit{element}) =
        \left\{ {\mathop{\mathit{element}}}^\ast_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\mid\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ{\mathop{\mathit{element}}}^+_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\mid{\mathop{\mathit{element}}}^+_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}} \right\}

Optional comma separated
    Surprisingly many fields can have optional comma separator.
    Since ``cabal-version: 3.0`` comma usage have to be consistent,
    in other words either used everywhere or nowhere.
    It's recommended to avoid using comma in these fields,
    an example field is :pkg-field:`default-extensions`.

    .. math::
        \mathrm{optcommalist}(\mathit{element}) =
        \left\{ \begin{gathered}{\mathop{\mathit{element}}}^\ast_{\bullet}\\{\mathop{\mathit{element}}}^\ast_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\\\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ{\mathop{\mathit{element}}}^+_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\\{\mathop{\mathit{element}}}^+_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\end{gathered} \right\}

Non-terminals
-------------

In the syntax definitions below the following non-terminal symbols are used. In addition:

    .. math::
        {\mathop{\mathit{alpha\text{-}num\text{-}not\text{-}digit}}} = {\mathop{\mathit{alpha\text{-}num}}}\cap{[ \mathord{``}\mathtt{0}\mathord{"} \cdots \mathord{``}\mathtt{9}\mathord{"} ]^c}

hs-string
    String as in Haskell; it's recommended to avoid using Haskell-specific escapes.

    .. math::
        \mathop{\mathord{``}\mathtt{\text{"}}\mathord{"}}{\left\{ {[\mathop{\mathord{``}\mathtt{\text{"}}\mathord{"}}\mathop{\mathord{``}\mathtt{\text{\\}}\mathord{"}}]^c}\mid\left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{\text{\\}\text{&}}\mathord{"}}\\\mathop{\mathord{``}\mathtt{\text{\\}\text{\\}}\mathord{"}}\\\left\{ \mathop{\mathord{``}\mathtt{\text{\\}n}\mathord{"}}\mid\mathop{\mathit{escapes}} \right\}\\\mathop{\mathord{``}\mathtt{\text{\\}}\mathord{"}}[\mathop{\mathord{``}\mathtt{0}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}]\\\mathop{\mathord{``}\mathtt{\text{\\}o}\mathord{"}}[\mathop{\mathord{``}\mathtt{0}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{7}\mathord{"}}]\\\mathop{\mathord{``}\mathtt{\text{\\}x}\mathord{"}}[\mathop{\mathord{``}\mathtt{0}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}\mathop{\mathord{``}\mathtt{A}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{F}\mathord{"}}\mathop{\mathord{``}\mathtt{a}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{f}\mathord{"}}]\\\left\{ \mathop{\mathord{``}\mathtt{\text{\\}\text{^}\text{@}}\mathord{"}}\mid\mathop{\mathit{control}} \right\}\\\left\{ \mathop{\mathord{``}\mathtt{\text{\\}NUL}\mathord{"}}\mid\mathop{\mathit{ascii}} \right\}\end{gathered} \right\} \right\}}^\ast_{}\mathop{\mathord{``}\mathtt{\text{"}}\mathord{"}}

unqual-name
    Unqualified component names are used for package names, component names etc. but not flag names. An unqualified component name consists of components separated by a hyphen, each component is a non-empty alphanumeric string, with at least one character that is not the digits ``0`` to ``9``. In other words, a component may not look like a number.

    .. math::
        {\left({\mathop{\mathit{alpha\text{-}num}}}^\ast_{}\mathop{\mathit{alpha\text{-}num\text{-}not\text{-}digit}}{\mathop{\mathit{alpha\text{-}num}}}^\ast_{}\right)}^+_{\mathop{\mathord{``}\mathtt{\text{-}}\mathord{"}}}

module-name
    Haskell module name as recognized by Cabal parser.

    .. math::
        {\left(\mathop{\mathit{upper}}{\left\{ \mathop{\mathit{alpha\text{-}num}}\mid[\mathop{\mathord{``}\mathtt{\text{'}}\mathord{"}}\mathop{\mathord{``}\mathtt{\text{_}}\mathord{"}}] \right\}}^\ast_{}\right)}^+_{\mathop{\mathord{``}\mathtt{\text{.}}\mathord{"}}}

version
    Version is to first approximation numbers separated by dots, where leading zero is not allowed and each version digit is consists at most of nine characters.

    .. math::
        {\left\{ \mathop{\mathord{``}\mathtt{0}\mathord{"}}\mid[\mathop{\mathord{``}\mathtt{1}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}]{[\mathop{\mathord{``}\mathtt{0}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}]}^{\in [0\ldots8]}_{} \right\}}^+_{\mathop{\mathord{``}\mathtt{\text{.}}\mathord{"}}}

version-range
    Version range syntax is recursive. Also note the set syntax added in ``cabal-version: 3.0``, set cannot be empty.

    .. math::
        \mathbf{fix}\;\mathop{\mathit{version\text{-}range}}\;\mathbf{in}\;\left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{\text{=}\text{=}}\mathord{"}}\circ\mathop{\mathit{version}}\\\mathop{\mathord{``}\mathtt{\text{>}}\mathord{"}}\circ\mathop{\mathit{version}}\\\mathop{\mathord{``}\mathtt{\text{<}}\mathord{"}}\circ\mathop{\mathit{version}}\\\mathop{\mathord{``}\mathtt{\text{<}\text{=}}\mathord{"}}\circ\mathop{\mathit{version}}\\\mathop{\mathord{``}\mathtt{\text{>}\text{=}}\mathord{"}}\circ\mathop{\mathit{version}}\\\mathop{\mathord{``}\mathtt{\text{^}\text{>}\text{=}}\mathord{"}}\circ\mathop{\mathit{version}}\\\mathop{\mathord{``}\mathtt{\text{=}\text{=}}\mathord{"}}\circ{\left\{ \mathop{\mathord{``}\mathtt{0}\mathord{"}}\mid[\mathop{\mathord{``}\mathtt{1}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}]{[\mathop{\mathord{``}\mathtt{0}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}]}^{\in [0\ldots8]}_{} \right\}}^+_{\mathop{\mathord{``}\mathtt{\text{.}}\mathord{"}}}\mathop{\mathord{``}\mathtt{\text{.}\text{*}}\mathord{"}}\\\mathop{\mathit{version\text{-}range}}\circ\mathop{\mathord{``}\mathtt{\text{|}\text{|}}\mathord{"}}\circ\mathop{\mathit{version\text{-}range}}\\\mathop{\mathit{version\text{-}range}}\circ\mathop{\mathord{``}\mathtt{\text{&}\text{&}}\mathord{"}}\circ\mathop{\mathit{version\text{-}range}}\\\mathop{\mathord{``}\mathtt{\text{(}}\mathord{"}}\circ\mathop{\mathit{version\text{-}range}}\circ\mathop{\mathord{``}\mathtt{\text{)}}\mathord{"}}\\\mathop{\mathord{``}\mathtt{\text{=}\text{=}}\mathord{"}}\circ\mathop{\mathord{``}\mathtt{\{}\mathord{"}}\circ{\mathop{\mathit{version}}}^+_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\circ\mathop{\mathord{``}\mathtt{\}}\mathord{"}}\\\mathop{\mathord{``}\mathtt{\text{^}\text{>}\text{=}}\mathord{"}}\circ\mathop{\mathord{``}\mathtt{\{}\mathord{"}}\circ{\mathop{\mathit{version}}}^+_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\circ\mathop{\mathord{``}\mathtt{\}}\mathord{"}}\end{gathered} \right\}


Build info fields
-----------------

asm-options
    * Monoidal field
    * Available since ``cabal-version: 3.0``.
    * Documentation of :pkg-field:`library:asm-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

asm-sources
    * Monoidal field
    * Available since ``cabal-version: 3.0``.
    * Documentation of :pkg-field:`library:asm-sources`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

autogen-includes
    * Monoidal field
    * Available since ``cabal-version: 3.0``.
    * Documentation of :pkg-field:`library:autogen-includes`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

autogen-modules
    * Monoidal field
    * Available since ``cabal-version: 2.0``.
    * Documentation of :pkg-field:`library:autogen-modules`

    .. math::
        \mathrm{commalist}\left({\left(\mathop{\mathit{upper}}{\left\{ \mathop{\mathit{alpha\text{-}num}}\mid[\mathop{\mathord{``}\mathtt{\text{'}}\mathord{"}}\mathop{\mathord{``}\mathtt{\text{_}}\mathord{"}}] \right\}}^\ast_{}\right)}^+_{\mathop{\mathord{``}\mathtt{\text{.}}\mathord{"}}}\right)

build-depends
    * Monoidal field
    * Documentation of :pkg-field:`library:build-depends`

    .. math::
        \mathrm{commalist}\left(\mathop{\mathit{pkg\text{-}name}}{\left(\mathop{\mathord{``}\mathtt{\text{:}}\mathord{"}}\left\{ \mathop{\mathit{unqual\text{-}name}}\mid\mathop{\mathord{``}\mathtt{\{}\mathord{"}}\circ{\mathop{\mathit{unqual\text{-}name}}}^+_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\circ\mathop{\mathord{``}\mathtt{\}}\mathord{"}} \right\}\right)}^?{\left(\circ\mathop{\mathit{version\text{-}range}}\right)}^?\right)

build-tool-depends
    * Monoidal field
    * Documentation of :pkg-field:`library:build-tool-depends`

    .. math::
        \mathrm{commalist}\mathsf{\color{red}{TODO}}

build-tools
    * Monoidal field
    * Deprecated since ``cabal-version: 2.0``: Please use 'build-tool-depends' field
    * Removed in ``cabal-version: 3.0``: Please use 'build-tool-depends' field.

    .. math::
        \mathrm{commalist}\mathsf{\color{red}{TODO}}

buildable
    * Boolean field
    * Default: ``True``
    * Documentation of :pkg-field:`library:buildable`

    .. math::
        \left\{ \mathop{\mathord{``}\mathtt{True}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{False}\mathord{"}} \right\}

c-sources
    * Monoidal field
    * Documentation of :pkg-field:`library:c-sources`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

cc-options
    * Monoidal field
    * Documentation of :pkg-field:`library:cc-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

cmm-options
    * Monoidal field
    * Available since ``cabal-version: 3.0``.
    * Documentation of :pkg-field:`library:cmm-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

cmm-sources
    * Monoidal field
    * Available since ``cabal-version: 3.0``.
    * Documentation of :pkg-field:`library:cmm-sources`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

cpp-options
    * Monoidal field
    * Documentation of :pkg-field:`library:cpp-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

cxx-options
    * Monoidal field
    * Available since ``cabal-version: 2.2``.
    * Documentation of :pkg-field:`library:cxx-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

cxx-sources
    * Monoidal field
    * Available since ``cabal-version: 2.2``.
    * Documentation of :pkg-field:`library:cxx-sources`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

default-extensions
    * Monoidal field
    * Available since ``cabal-version: 1.10``.
    * Documentation of :pkg-field:`library:default-extensions`

    .. math::
        \mathrm{optcommalist}\mathsf{\color{red}{TODO}}

default-language
    * Optional field
    * Available since ``cabal-version: 1.10``.
    * Documentation of :pkg-field:`library:default-language`

    .. math::
        \left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{GHC2024}\mathord{"}}\\\mathop{\mathord{``}\mathtt{GHC2021}\mathord{"}}\\\mathop{\mathord{``}\mathtt{Haskell2010}\mathord{"}}\\\mathop{\mathord{``}\mathtt{Haskell98}\mathord{"}}\end{gathered} \right\}

extensions
    * Monoidal field
    * Deprecated since ``cabal-version: 1.12``: Please use 'default-extensions' or 'other-extensions' fields.
    * Removed in ``cabal-version: 3.0``: Please use 'default-extensions' or 'other-extensions' fields.

    .. math::
        \mathrm{optcommalist}\mathsf{\color{red}{TODO}}

extra-bundled-libraries
    * Monoidal field
    * Documentation of :pkg-field:`library:extra-bundled-libraries`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-dynamic-library-flavours
    * Monoidal field
    * Available since ``cabal-version: 3.0``.
    * Documentation of :pkg-field:`library:extra-dynamic-library-flavours`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-framework-dirs
    * Monoidal field
    * Documentation of :pkg-field:`library:extra-framework-dirs`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-ghci-libraries
    * Monoidal field
    * Documentation of :pkg-field:`library:extra-ghci-libraries`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-lib-dirs
    * Monoidal field
    * Documentation of :pkg-field:`library:extra-lib-dirs`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-lib-dirs-static
    * Monoidal field
    * Available since ``cabal-version: 3.8``.
    * Documentation of :pkg-field:`library:extra-lib-dirs-static`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-libraries
    * Monoidal field
    * Documentation of :pkg-field:`library:extra-libraries`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-libraries-static
    * Monoidal field
    * Available since ``cabal-version: 3.8``.
    * Documentation of :pkg-field:`library:extra-libraries-static`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-library-flavours
    * Monoidal field
    * Documentation of :pkg-field:`library:extra-library-flavours`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

frameworks
    * Monoidal field
    * Documentation of :pkg-field:`library:frameworks`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

ghc-options
    * Monoidal field
    * Documentation of :pkg-field:`library:ghc-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

ghc-prof-options
    * Monoidal field
    * Documentation of :pkg-field:`library:ghc-prof-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

ghc-prof-shared-options
    * Monoidal field
    * Available since ``cabal-version: 3.14``.
    * Documentation of :pkg-field:`library:ghc-prof-shared-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

ghc-shared-options
    * Monoidal field
    * Documentation of :pkg-field:`library:ghc-shared-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

ghcjs-options
    * Monoidal field
    * Documentation of :pkg-field:`library:ghcjs-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

ghcjs-prof-options
    * Monoidal field
    * Documentation of :pkg-field:`library:ghcjs-prof-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

ghcjs-prof-shared-options
    * Monoidal field
    * Available since ``cabal-version: 3.14``.
    * Documentation of :pkg-field:`library:ghcjs-prof-shared-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

ghcjs-shared-options
    * Monoidal field
    * Documentation of :pkg-field:`library:ghcjs-shared-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

hs-source-dir
    * Monoidal field
    * Deprecated since ``cabal-version: 1.2``: Please use 'hs-source-dirs'
    * Removed in ``cabal-version: 3.0``: Please use 'hs-source-dirs' field.

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

hs-source-dirs
    * Monoidal field
    * Documentation of :pkg-field:`library:hs-source-dirs`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

hsc2hs-options
    * Monoidal field
    * Available since ``cabal-version: 3.6``.
    * Documentation of :pkg-field:`library:hsc2hs-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

include-dirs
    * Monoidal field
    * Documentation of :pkg-field:`library:include-dirs`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

includes
    * Monoidal field
    * Documentation of :pkg-field:`library:includes`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

install-includes
    * Monoidal field
    * Documentation of :pkg-field:`library:install-includes`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

js-sources
    * Monoidal field
    * Documentation of :pkg-field:`library:js-sources`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

jspp-options
    * Monoidal field
    * Available since ``cabal-version: 3.16``.
    * Documentation of :pkg-field:`library:jspp-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

ld-options
    * Monoidal field
    * Documentation of :pkg-field:`library:ld-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

mixins
    * Monoidal field
    * Available since ``cabal-version: 2.0``.
    * Documentation of :pkg-field:`library:mixins`

    .. math::
        \mathrm{commalist}\left(\mathop{\mathit{package\text{-}name}}{\left(\mathop{\mathord{``}\mathtt{\text{:}}\mathord{"}}\mathop{\mathit{library\text{-}name}}\right)}^?{\left(\bullet\left\{ \mid\mathop{\mathord{``}\mathtt{hiding}\mathord{"}}\circ\mathop{\mathord{``}\mathtt{\text{(}}\mathord{"}}\circ{\mathop{\mathit{module\text{-}name}}}^\ast_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\circ\mathop{\mathord{``}\mathtt{\text{)}}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{\text{(}}\mathord{"}}\circ{\left(\mathop{\mathit{module\text{-}name}}{\left(\bullet\mathop{\mathord{``}\mathtt{as}\mathord{"}}\bullet\mathop{\mathit{module\text{-}name}}\right)}^?\right)}^\ast_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\circ\mathop{\mathord{``}\mathtt{\text{)}}\mathord{"}} \right\}{\left(\circ\mathop{\mathord{``}\mathtt{requires}\mathord{"}}\bullet\left\{ \mid\mathop{\mathord{``}\mathtt{hiding}\mathord{"}}\circ\mathop{\mathord{``}\mathtt{\text{(}}\mathord{"}}\circ{\mathop{\mathit{module\text{-}name}}}^\ast_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\circ\mathop{\mathord{``}\mathtt{\text{)}}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{\text{(}}\mathord{"}}\circ{\left(\mathop{\mathit{module\text{-}name}}{\left(\bullet\mathop{\mathord{``}\mathtt{as}\mathord{"}}\bullet\mathop{\mathit{module\text{-}name}}\right)}^?\right)}^\ast_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\circ\mathop{\mathord{``}\mathtt{\text{)}}\mathord{"}} \right\}\right)}^?\right)}^?\right)

other-extensions
    * Monoidal field
    * Documentation of :pkg-field:`library:other-extensions`

    .. math::
        \mathrm{optcommalist}\mathsf{\color{red}{TODO}}

other-languages
    * Monoidal field
    * Available since ``cabal-version: 1.10``.
    * Documentation of :pkg-field:`library:other-languages`

    .. math::
        \mathrm{optcommalist}\left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{GHC2024}\mathord{"}}\\\mathop{\mathord{``}\mathtt{GHC2021}\mathord{"}}\\\mathop{\mathord{``}\mathtt{Haskell2010}\mathord{"}}\\\mathop{\mathord{``}\mathtt{Haskell98}\mathord{"}}\end{gathered} \right\}

other-modules
    * Monoidal field
    * Documentation of :pkg-field:`library:other-modules`

    .. math::
        \mathrm{commalist}\left({\left(\mathop{\mathit{upper}}{\left\{ \mathop{\mathit{alpha\text{-}num}}\mid[\mathop{\mathord{``}\mathtt{\text{'}}\mathord{"}}\mathop{\mathord{``}\mathtt{\text{_}}\mathord{"}}] \right\}}^\ast_{}\right)}^+_{\mathop{\mathord{``}\mathtt{\text{.}}\mathord{"}}}\right)

pkgconfig-depends
    * Monoidal field
    * Documentation of :pkg-field:`library:pkgconfig-depends`

    .. math::
        \mathrm{commalist}\mathsf{\color{red}{TODO}}

virtual-modules
    * Monoidal field
    * Available since ``cabal-version: 2.2``.
    * Documentation of :pkg-field:`library:virtual-modules`

    .. math::
        \mathrm{commalist}\left({\left(\mathop{\mathit{upper}}{\left\{ \mathop{\mathit{alpha\text{-}num}}\mid[\mathop{\mathord{``}\mathtt{\text{'}}\mathord{"}}\mathop{\mathord{``}\mathtt{\text{_}}\mathord{"}}] \right\}}^\ast_{}\right)}^+_{\mathop{\mathord{``}\mathtt{\text{.}}\mathord{"}}}\right)


Package description fields
--------------------------

author
    * Free text field
    * Documentation of :pkg-field:`author`

bug-reports
    * Free text field
    * Documentation of :pkg-field:`bug-reports`

build-type
    * Optional field
    * Documentation of :pkg-field:`build-type`

    .. math::
        \left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{Simple}\mathord{"}}\\\mathop{\mathord{``}\mathtt{Configure}\mathord{"}}\\\mathop{\mathord{``}\mathtt{Custom}\mathord{"}}\\\mathop{\mathord{``}\mathtt{Hooks}\mathord{"}}\\\mathop{\mathord{``}\mathtt{Make}\mathord{"}}\\\mathop{\mathord{``}\mathtt{Default}\mathord{"}}\end{gathered} \right\}

cabal-version
    * Optional field
    * Default: ``>=1.0``
    * Documentation of :pkg-field:`cabal-version`

    .. math::
        \mathop{\mathord{``}\mathtt{3\text{.}4}\mathord{"}}

category
    * Free text field
    * Documentation of :pkg-field:`category`

copyright
    * Free text field
    * Documentation of :pkg-field:`copyright`

data-dir
    * Optional field
    * Default: ``.``
    * Documentation of :pkg-field:`data-dir`

    .. math::
        \left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

data-files
    * Monoidal field
    * Documentation of :pkg-field:`data-files`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

description
    * Free text field
    * Documentation of :pkg-field:`description`

extra-doc-files
    * Monoidal field
    * Documentation of :pkg-field:`extra-doc-files`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-files
    * Monoidal field
    * Available since ``cabal-version: 3.14``.
    * Documentation of :pkg-field:`extra-files`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-source-files
    * Monoidal field
    * Documentation of :pkg-field:`extra-source-files`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-tmp-files
    * Monoidal field
    * Documentation of :pkg-field:`extra-tmp-files`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

homepage
    * Free text field
    * Documentation of :pkg-field:`homepage`

license
    * Optional field
    * Default: ``NONE``
    * Documentation of :pkg-field:`license`

    .. math::
        \mathsf{\color{red}{TODO}}

license-file
    * Monoidal field
    * Documentation of :pkg-field:`license-file`

    .. math::
        \left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

maintainer
    * Free text field
    * Documentation of :pkg-field:`maintainer`

name
    * Required field
    * Documentation of :pkg-field:`name`

    .. math::
        \mathop{\mathit{unqual\text{-}name}}

package-url
    * Free text field
    * Documentation of :pkg-field:`package-url`

stability
    * Free text field
    * Documentation of :pkg-field:`stability`

synopsis
    * Free text field
    * Documentation of :pkg-field:`synopsis`

tested-with
    * Monoidal field
    * Documentation of :pkg-field:`tested-with`

    .. math::
        \mathrm{optcommalist}\mathsf{\color{red}{TODO}}

version
    * Required field
    * Documentation of :pkg-field:`version`

    .. math::
        {\left\{ \mathop{\mathord{``}\mathtt{0}\mathord{"}}\mid[\mathop{\mathord{``}\mathtt{1}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}]{[\mathop{\mathord{``}\mathtt{0}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}]}^{\in [0\ldots8]}_{} \right\}}^+_{\mathop{\mathord{``}\mathtt{\text{.}}\mathord{"}}}


Test-suite fields
-----------------

code-generators
    * Monoidal field
    * Available since ``cabal-version: 3.8``.
    * Documentation of :pkg-field:`test-suite:code-generators`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

main-is
    * Optional field
    * Documentation of :pkg-field:`test-suite:main-is`

    .. math::
        \left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

test-module
    * Optional field
    * Documentation of :pkg-field:`test-suite:test-module`

    .. math::
        {\left(\mathop{\mathit{upper}}{\left\{ \mathop{\mathit{alpha\text{-}num}}\mid[\mathop{\mathord{``}\mathtt{\text{'}}\mathord{"}}\mathop{\mathord{``}\mathtt{\text{_}}\mathord{"}}] \right\}}^\ast_{}\right)}^+_{\mathop{\mathord{``}\mathtt{\text{.}}\mathord{"}}}

type
    * Optional field
    * Documentation of :pkg-field:`test-suite:type`

    .. math::
        \left\{ \mathop{\mathord{``}\mathtt{exitcode\text{-}stdio\text{-}1\text{.}0}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{detailed\text{-}0\text{.}9}\mathord{"}} \right\}


