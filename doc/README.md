Cabal documentation
===================

### Where to read it
These docs will be built and deployed whenever a release is made,
and can be read at: https://www.haskell.org/cabal/users-guide/

In addition, the docs are taken directly from git and hosted at:
http://cabal.readthedocs.io/


### How to build it

Building the documentation requires uv and Python 3. Run the following command
either from the root of the cabal repository or from the `docs/` subdirectory:

``` console
> make users-guide
```

Note: Python on Mac OS X dislikes `LC_CTYPE=UTF-8`, so unset the variable
and instead set `LC_ALL=en_US.UTF-8`.

### How to update dependencies

The list of transitive dependencies (`requirements.txt`) is generated from the
list of direct dependencies in `pyproject.toml`. Find outdated dependencies with:

```console
> cd doc
> uv pip list --outdated
Package            Version    Latest   Type
------------------ ---------- -------- -----
certifi            2025.11.12 2026.1.4 wheel
docutils           0.21.2     0.22.4   wheel
sphinx             8.2.3      9.1.0    wheel
sphinxnotes-strike 1.5        2.0      wheel
urllib3            2.6.2      2.6.3    wheel
```

Upgrade the lock file to the latest satisfiable requirements with:

```console
> uv sync --upgrade
...
 - certifi==2025.11.12
 + certifi==2026.1.4
 - sphinxnotes-strike==1.5
 + sphinxnotes-strike==2.0
 - urllib3==2.6.2
 + urllib3==2.6.3
```

In some cases, you may have to add a bound manually to `pyproject.toml`, e.g. `requests >= 2.31.0`.

### How to check spelling

To check for typos, run `make typos` and to fix them, run `make fix-typos`. Fixing might fail.

> If there is any ambiguity (multiple possible corrections),
> `typos` will just report it to the user and move on.
>
> SOURCE: [typos/Getting Started](https://github.com/crate-ci/typos#getting-started)

```
# spellchecker:off
$ make users-guide-typos
cd doc && find . -type f -name '*.rst' | xargs typos
error: `managable` should be `manageable`, `manageably`
  --> doc/getting-started.rst:75:6
   |
75 | more managable building blocks.
   |      ^^^^^^^^^
   |
make: *** [Makefile: users-guide-typos] Error 2
# spellchecker:on
```

### Caveats, for newcomers to RST from MD

RST does not allow you to skip section levels when nesting, like MD
does.
So, you cannot have

```
    Section heading
    ===============

    Some unimportant block
    """"""""""""""""""""""
```

  instead you need to observe order and either promote your block:

```
    Section heading
    ===============

    Some not quite so important block
    ---------------------------------
```

  or introduce more subsections:

```
    Section heading
    ===============

    Subsection
    ----------

    Subsubsection
    ^^^^^^^^^^^^^

    Some unimportant block
    """"""""""""""""""""""
```

* RST simply parses a file and interprets headings to indicate the
  start of a new block,
  * at the level implied by the header's *adornment*, if the adornment was
  previously encountered in this file,
  * at one level deeper than the previous block, otherwise.

  This means that a lot of confusion can arise when people use
  different adornments to signify the same depth in different files.

  To eliminate this confusion, please stick to the adornment order
  recommended by the Sphinx team:

```
    ####
    Part
    ####

    *******
    Chapter
    *******

    Section
    =======

    Subsection
    ----------

    Subsubsection
    ^^^^^^^^^^^^^

    Paragraph
    """""""""
```

* The Read-The-Docs stylesheet does not support multiple top-level
  sections in a file that is linked to from the top-most TOC (in
  `index.rst`). It will mess up the sidebar.
  E.g. you cannot link to a `cabal.rst` with sections "Introduction",
  "Using Cabal", "Epilogue" from `index.rst`.

  One solution is to have a single section, e.g. "All About Cabal", in
  `cabal.rst` and make the other blocks subsections of that.

  Another solution is to link via an indirection, e.g. create
  `all-about-cabal.rst`, where you include `cabal.rst` using  the
  `.. toctree::` command and then link to `all-about-cabal.rst` from
  `index.rst`.
  This will effectively "push down" all blocks by one layer and solve
  the problem without having to change `cabal.rst`.


* We use [`extlinks`](http://www.sphinx-doc.org/en/stable/ext/extlinks.html)
  to shorten links to commonly referred resources (wiki, issue trackers).

  E.g. you can use the more convenient short syntax

        :issue:`123`

  which is expanded into a hyperlink

        `#123 <https://github.com/haskell/cabal/issues/123>`__

  See `conf.py` for list of currently defined link shorteners.
