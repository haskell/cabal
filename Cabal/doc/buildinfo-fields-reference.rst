.. _buildinfo-field-reference:

==================================================
 BuildInfo field reference
==================================================

Notation
---------------

Field syntax is described as they are in the latest cabal file format version.

* terminals are enclosed in quotes and type set in typewriter script:

  .. math::

      \mathord{"}\mathtt{example}\mathord{"}

* non-terminals are type set in italic:

  .. math::

      \mathit{version\text-range}

* character sets are type set resembling regular expression notation:


  .. math::

      [ \mathord{"}\mathtt{1}\mathord{"} \cdots \mathord{"}\mathtt{9}\mathord{"} ]

  Character set compelements have :math:`c` superscript:

  .. math::

      [ \mathord{"}\mathtt{1}\mathord{"} \cdots \mathord{"}\mathtt{9}\mathord{"} ]^c

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

Comma semarted
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

In the syntax definitions below the following non-terminal symbols are used:

hs-string
    String as in Haskell; it's recommended to avoid using Haskell-specific escapes.

    .. math::
        \mathop{\mathord{``}\mathtt{\text{"}}\mathord{"}}{\left\{ {[\mathop{\mathord{``}\mathtt{\text{"}}\mathord{"}}\mathop{\mathord{``}\mathtt{\text{\\}}\mathord{"}}]^c}\mid\left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{\text{\\}\text{&}}\mathord{"}}\\\mathop{\mathord{``}\mathtt{\text{\\}\text{\\}}\mathord{"}}\\\left\{ \mathop{\mathord{``}\mathtt{\text{\\}n}\mathord{"}}\mid\mathop{\mathit{escapes}} \right\}\\\mathop{\mathord{``}\mathtt{\text{\\}}\mathord{"}}[\mathop{\mathord{``}\mathtt{0}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}]\\\mathop{\mathord{``}\mathtt{\text{\\}o}\mathord{"}}[\mathop{\mathord{``}\mathtt{0}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{7}\mathord{"}}]\\\mathop{\mathord{``}\mathtt{\text{\\}x}\mathord{"}}[\mathop{\mathord{``}\mathtt{0}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}\mathop{\mathord{``}\mathtt{A}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{F}\mathord{"}}\mathop{\mathord{``}\mathtt{a}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{f}\mathord{"}}]\\\left\{ \mathop{\mathord{``}\mathtt{\text{\\}\text{^}\text{@}}\mathord{"}}\mid\mathop{\mathit{control}} \right\}\\\left\{ \mathop{\mathord{``}\mathtt{\text{\\}NUL}\mathord{"}}\mid\mathop{\mathit{ascii}} \right\}\end{gathered} \right\} \right\}}^\ast_{}\mathop{\mathord{``}\mathtt{\text{"}}\mathord{"}}

unqual-name
    Unqualified component names are used for package names, component names etc. but not flag names. Unqualified component name consist of components separated by dash, each component is non-empty alphanumeric string, with at least one alphabetic character. In other words, component may not look like a number.

    .. math::
        {\left({\mathop{\mathit{alpha\text{-}num}}}^\ast_{}\mathop{\mathit{alpha}}{\mathop{\mathit{alpha\text{-}num}}}^\ast_{}\right)}^+_{\mathop{\mathord{``}\mathtt{\text{-}}\mathord{"}}}

module-name
    Haskell module name as recognized by Cabal parser.

    .. math::
        \mathsf{\color{red}{TODO}}

version
    Version is to first approximation numbers separated by dots, where leading zero is not allowed and each version digit is consists at most of nine characters.

    .. math::
        {\left\{ \mathop{\mathord{``}\mathtt{0}\mathord{"}}\mid[\mathop{\mathord{``}\mathtt{1}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}]{[\mathop{\mathord{``}\mathtt{0}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}]}^{\in [0\ldots8]}_{} \right\}}^+_{\mathop{\mathord{``}\mathtt{\text{.}}\mathord{"}}}

version-range
    Version range syntax is recursive. Also note the set syntax added in ``cabal-version: 3.0``, set cannot be empty.

    .. math::
        \mathbf{fix}\;\mathop{\mathit{version\text{-}range}}\;\mathbf{in}\;\left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{\text{-}any}\mathord{"}}\\\mathop{\mathord{``}\mathtt{\text{-}none}\mathord{"}}\\\mathop{\mathord{``}\mathtt{\text{=}\text{=}}\mathord{"}}\circ\mathop{\mathit{version}}\\\mathop{\mathord{``}\mathtt{\text{>}}\mathord{"}}\circ\mathop{\mathit{version}}\\\mathop{\mathord{``}\mathtt{\text{<}}\mathord{"}}\circ\mathop{\mathit{version}}\\\mathop{\mathord{``}\mathtt{\text{<}\text{=}}\mathord{"}}\circ\mathop{\mathit{version}}\\\mathop{\mathord{``}\mathtt{\text{>}\text{=}}\mathord{"}}\circ\mathop{\mathit{version}}\\\mathop{\mathord{``}\mathtt{\text{^}\text{>}\text{=}}\mathord{"}}\circ\mathop{\mathit{version}}\\\mathop{\mathit{version\text{-}range}}\circ\mathop{\mathord{``}\mathtt{\text{|}\text{|}}\mathord{"}}\circ\mathop{\mathit{version\text{-}range}}\\\mathop{\mathit{version\text{-}range}}\circ\mathop{\mathord{``}\mathtt{\text{&}\text{&}}\mathord{"}}\circ\mathop{\mathit{version\text{-}range}}\\\mathop{\mathord{``}\mathtt{\text{(}}\mathord{"}}\circ\mathop{\mathit{version\text{-}range}}\circ\mathop{\mathord{``}\mathtt{\text{)}}\mathord{"}}\\\mathop{\mathord{``}\mathtt{\text{=}\text{=}}\mathord{"}}\circ{\left\{ \mathop{\mathord{``}\mathtt{0}\mathord{"}}\mid[\mathop{\mathord{``}\mathtt{1}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}]{[\mathop{\mathord{``}\mathtt{0}\mathord{"}}\cdots\mathop{\mathord{``}\mathtt{9}\mathord{"}}]}^{\in [0\ldots8]}_{} \right\}}^+_{\mathop{\mathord{``}\mathtt{\text{.}}\mathord{"}}}\mathop{\mathord{``}\mathtt{\text{.}\text{*}}\mathord{"}}\\\mathop{\mathord{``}\mathtt{\text{=}\text{=}}\mathord{"}}\circ\mathop{\mathord{``}\mathtt{\{}\mathord{"}}\circ{\mathop{\mathit{version}}}^+_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\circ\mathop{\mathord{``}\mathtt{\}}\mathord{"}}\\\mathop{\mathord{``}\mathtt{\text{^}\text{>}\text{=}}\mathord{"}}\circ\mathop{\mathord{``}\mathtt{\{}\mathord{"}}\circ{\mathop{\mathit{version}}}^+_{\left(\circ\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}\circ\right)}\circ\mathop{\mathord{``}\mathtt{\}}\mathord{"}}\end{gathered} \right\}


Build info fields
-----------------

asm-options
    * Monoidal field
    * Available since ``cabal-version: 3.0``.
    * Documentation of :pkg-field:`asm-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

asm-sources
    * Monoidal field
    * Available since ``cabal-version: 3.0``.
    * Documentation of :pkg-field:`asm-sources`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

autogen-includes
    * Monoidal field
    * Available since ``cabal-version: 3.0``.
    * Documentation of :pkg-field:`autogen-includes`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

autogen-modules
    * Monoidal field
    * Documentation of :pkg-field:`autogen-modules`

    .. math::
        \mathrm{commalist}\mathsf{\color{red}{TODO}}

build-depends
    * Monoidal field
    * Documentation of :pkg-field:`build-depends`

    .. math::
        \mathrm{commalist}\left(\mathop{\mathit{pkg\text{-}name}}{\left(\circ\mathop{\mathord{``}\mathtt{\text{:}}\mathord{"}}\circ\left\{ \mathop{\mathit{unqual\text{-}name}}\mid\mathop{\mathord{``}\mathtt{\{}\mathord{"}}\circ\mathrm{commalist}\mathop{\mathit{unqual\text{-}name}}\circ\mathop{\mathord{``}\mathtt{\}}\mathord{"}} \right\}\right)}^?{\left(\circ\mathop{\mathit{version\text{-}range}}\right)}^?\right)

build-tool-depends
    * Monoidal field
    * Documentation of :pkg-field:`build-tool-depends`

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
    * Documentation of :pkg-field:`buildable`

    .. math::
        \left\{ \mathop{\mathord{``}\mathtt{True}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{False}\mathord{"}} \right\}

c-sources
    * Monoidal field
    * Documentation of :pkg-field:`c-sources`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

cc-options
    * Monoidal field
    * Documentation of :pkg-field:`cc-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

cmm-options
    * Monoidal field
    * Available since ``cabal-version: 3.0``.
    * Documentation of :pkg-field:`cmm-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

cmm-sources
    * Monoidal field
    * Available since ``cabal-version: 3.0``.
    * Documentation of :pkg-field:`cmm-sources`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

cpp-options
    * Monoidal field
    * Documentation of :pkg-field:`cpp-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

cxx-options
    * Monoidal field
    * Available since ``cabal-version: 2.2``.
    * Documentation of :pkg-field:`cxx-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

cxx-sources
    * Monoidal field
    * Available since ``cabal-version: 2.2``.
    * Documentation of :pkg-field:`cxx-sources`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

default-extensions
    * Monoidal field
    * Documentation of :pkg-field:`default-extensions`

    .. math::
        \mathrm{optcommalist}\mathsf{\color{red}{TODO}}

default-language
    * Optional field
    * Documentation of :pkg-field:`default-language`

    .. math::
        \left\{ \mathop{\mathord{``}\mathtt{Haskell98}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{Haskell2010}\mathord{"}} \right\}

extensions
    * Monoidal field
    * Deprecated since ``cabal-version: 1.12``: Please use 'default-extensions' or 'other-extensions' fields.
    * Removed in ``cabal-version: 3.0``: Please use 'default-extensions' or 'other-extensions' fields.

    .. math::
        \mathrm{optcommalist}\mathsf{\color{red}{TODO}}

extra-bundled-libraries
    * Monoidal field
    * Documentation of :pkg-field:`extra-bundled-libraries`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-dynamic-library-flavours
    * Monoidal field
    * Available since ``cabal-version: 3.0``.
    * Documentation of :pkg-field:`extra-dynamic-library-flavours`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-framework-dirs
    * Monoidal field
    * Documentation of :pkg-field:`extra-framework-dirs`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-ghci-libraries
    * Monoidal field
    * Documentation of :pkg-field:`extra-ghci-libraries`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-lib-dirs
    * Monoidal field
    * Documentation of :pkg-field:`extra-lib-dirs`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-libraries
    * Monoidal field
    * Documentation of :pkg-field:`extra-libraries`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

extra-library-flavours
    * Monoidal field
    * Documentation of :pkg-field:`extra-library-flavours`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

frameworks
    * Monoidal field
    * Documentation of :pkg-field:`frameworks`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

ghc-options
    * Monoidal field
    * Documentation of :pkg-field:`ghc-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

ghc-prof-options
    * Monoidal field
    * Documentation of :pkg-field:`ghc-prof-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

ghc-shared-options
    * Monoidal field
    * Documentation of :pkg-field:`ghc-shared-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

ghcjs-options
    * Monoidal field
    * Documentation of :pkg-field:`ghcjs-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

ghcjs-prof-options
    * Monoidal field
    * Documentation of :pkg-field:`ghcjs-prof-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

ghcjs-shared-options
    * Monoidal field
    * Documentation of :pkg-field:`ghcjs-shared-options`

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
    * Documentation of :pkg-field:`hs-source-dirs`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

include-dirs
    * Monoidal field
    * Documentation of :pkg-field:`include-dirs`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

includes
    * Monoidal field
    * Documentation of :pkg-field:`includes`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

install-includes
    * Monoidal field
    * Documentation of :pkg-field:`install-includes`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

js-sources
    * Monoidal field
    * Documentation of :pkg-field:`js-sources`

    .. math::
        \mathrm{commalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

ld-options
    * Monoidal field
    * Documentation of :pkg-field:`ld-options`

    .. math::
        {\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}]^c}}^+_{} \right\}}^\ast_{\bullet}

mixins
    * Monoidal field
    * Available since ``cabal-version: 2.0``.
    * Documentation of :pkg-field:`mixins`

    .. math::
        \mathrm{commalist}\mathsf{\color{red}{TODO}}

other-extensions
    * Monoidal field
    * Documentation of :pkg-field:`other-extensions`

    .. math::
        \mathrm{optcommalist}\mathsf{\color{red}{TODO}}

other-languages
    * Monoidal field
    * Documentation of :pkg-field:`other-languages`

    .. math::
        \mathrm{optcommalist}\left\{ \mathop{\mathord{``}\mathtt{Haskell98}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{Haskell2010}\mathord{"}} \right\}

other-modules
    * Monoidal field
    * Documentation of :pkg-field:`other-modules`

    .. math::
        \mathrm{commalist}\mathsf{\color{red}{TODO}}

pkgconfig-depends
    * Monoidal field
    * Documentation of :pkg-field:`pkgconfig-depends`

    .. math::
        \mathrm{commalist}\mathsf{\color{red}{TODO}}

virtual-modules
    * Monoidal field
    * Available since ``cabal-version: 2.2``.
    * Documentation of :pkg-field:`virtual-modules`

    .. math::
        \mathrm{commalist}\mathsf{\color{red}{TODO}}


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
        \left\{ \begin{gathered}\mathop{\mathord{``}\mathtt{Simple}\mathord{"}}\\\mathop{\mathord{``}\mathtt{Configure}\mathord{"}}\\\mathop{\mathord{``}\mathtt{Custom}\mathord{"}}\\\mathop{\mathord{``}\mathtt{Make}\mathord{"}}\\\mathop{\mathord{``}\mathtt{Default}\mathord{"}}\end{gathered} \right\}

cabal-version
    * Optional field
    * Default: ``-any``
    * Documentation of :pkg-field:`cabal-version`

    .. math::
        \mathop{\mathord{``}\mathtt{3\text{.}0}\mathord{"}}

category
    * Free text field
    * Documentation of :pkg-field:`category`

copyright
    * Free text field
    * Documentation of :pkg-field:`copyright`

data-dir
    * Optional field
    * Default: ``""``
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
        \mathrm{optcommalist}\left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

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

main-is
    * Optional field
    * Documentation of :pkg-field:`main-is`

    .. math::
        \left\{ \mathop{\mathit{hs\text{-}string}}\mid{{[\mathop{\mathord{``}\mathtt{\ }\mathord{"}}\mathop{\mathord{``}\mathtt{\text{,}}\mathord{"}}]^c}}^+_{} \right\}

test-module
    * Optional field
    * Documentation of :pkg-field:`test-module`

    .. math::
        \mathsf{\color{red}{TODO}}

type
    * Optional field
    * Documentation of :pkg-field:`type`

    .. math::
        \left\{ \mathop{\mathord{``}\mathtt{exitcode\text{-}stdio\text{-}1\text{.}0}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{detailed\text{-}0\text{.}9}\mathord{"}} \right\}


