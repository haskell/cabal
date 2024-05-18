Cabal documentation
===================

### Where to read it
These docs will be built and deployed whenever a release is made,
and can be read at: https://www.haskell.org/cabal/users-guide/

In addition, the docs are taken directly from git and hosted at:
http://cabal.readthedocs.io/


### How to build it

Building the documentation requires Python 3, PIP, and `pip-tools` (see the second note below for how to install it). Run the following command either from the root of the cabal repository or from the `docs/` subdirectory:

``` console
> make users-guide
```

Note: Python on Mac OS X dislikes `LC_CTYPE=UTF-8`, so unset the variable
and instead set `LC_ALL=en_US.UTF-8`.

Note: You can use a vendor package for `pip-tools`, or run

``` console
> pip install pip-tools
```

Make sure the installation directory (often `$HOME/.local/bin`) is on your `$PATH`.

### How to update dependencies

The list of transitive dependencies (`requirements.txt`) is generated from the list of direct dependencies in `requirements.in`. To perform the generation step, run

```console
> make users-guide-requirements
```

either from the root of the cabal repository or from the `docs/` subdirectory. You will need to do this before building documentation the first time, but should only need to repeat it after a `git clean` or if the dependencies in `requirements.in` change.

In some cases, you may have to add a bound manually to `requirements.in`, e.g. `requests >= 2.31.0`.

### Gitpod workflow

From a fork of cabal, these docs can be edited online with
[gitpod](https://www.gitpod.io/):

* Open in gitpod https://gitpod.io/#https://github.com/username/cabal
* Install the virtual environment prerequisite.
  `> sudo apt install python3.8-venv`
* Build the user guide `> make users-guide`.
* Open the guide in a local browser.
  `> python -m http.server 8000 --directory=dist-newstyle/doc/users-guide`

Make your edits, rebuild the guide and refresh the browser to preview the
changes. When happy, commit your changes with git in the included terminal.

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
