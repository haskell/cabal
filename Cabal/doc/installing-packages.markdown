% Cabal User Guide


# Building and installing packages #

After you've unpacked a Cabal package, you can build it by moving into
the root directory of the package and using the `Setup.hs` or
`Setup.lhs` script there:

> `_runhaskell_ Setup.hs` [_command_] [_option_...]

The _command_ argument selects a particular step in the build/install
process. You can also get a summary of the command syntax with

> `runhaskell Setup.hs --help`

## Building and installing a system package ##

~~~~~~~~~~~~~~~~
runhaskell Setup.hs configure --ghc
runhaskell Setup.hs build
runhaskell Setup.hs install
~~~~~~~~~~~~~~~~

The first line readies the system to build the tool using GHC; for
example, it checks that GHC exists on the system.  The second line
performs the actual building, while the last both copies the build
results to some permanent place and registers the package with GHC.

## Building and installing a user package ##

~~~~~~~~~~~~~~~~
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install
~~~~~~~~~~~~~~~~

The package is installed under the user's home directory and is
registered in the user's package database (`--user`).

## Creating a binary package ##

When creating binary packages (e.g. for RedHat or Debian) one needs to
create a tarball that can be sent to another system for unpacking in the
root directory:

~~~~~~~~~~~~~~~~
runhaskell Setup.hs configure --prefix=/usr
runhaskell Setup.hs build
runhaskell Setup.hs copy --destdir=/tmp/mypkg
tar -czf mypkg.tar.gz /tmp/mypkg/
~~~~~~~~~~~~~~~~

If the package contains a library, you need two additional steps:

~~~~~~~~~~~~~~~~
runhaskell Setup.hs register --gen-script
runhaskell Setup.hs unregister --gen-script
~~~~~~~~~~~~~~~~

This creates shell scripts `register.sh` and `unregister.sh`, which must
also be sent to the target system.  After unpacking there, the package
must be registered by running the `register.sh` script. The
`unregister.sh` script would be used in the uninstall procedure of the
package. Similar steps may be used for creating binary packages for
Windows.


The following options are understood by all commands:

`--help`, `-h` or `-?`
:   List the available options for the command.

`--verbose=`_n_ or `-v`_n_
:   Set the verbosity level (0-3). The normal level is 1; a missing _n_
    defaults to 2.

The various commands and the additional options they support are
described below. In the simple build infrastructure, any other options
will be reported as errors.

## setup configure ##

Prepare to build the package.  Typically, this step checks that the
target platform is capable of building the package, and discovers
platform-specific features that are needed during the build.

The user may also adjust the behaviour of later stages using the options
listed in the following subsections.  In the simple build
infrastructure, the values supplied via these options are recorded in a
private file read by later stages.

If a user-supplied `configure` script is run (see the section on
[system-dependent parameters](#system-dependent-parameters) or on
[complex packages](#complex-packages)), it is passed the
`--with-hc-pkg`, `--prefix`, `--bindir`, `--libdir`, `--datadir` and
`--libexecdir` options. In addition the value of the `--with-compiler`
option is passed in a `--with-hc` option and all options specified with
`--configure-option=` are passed on.

### Programs used for building ###

The following options govern the programs used to process the source
files of a package:

`--ghc` or `-g`, `--nhc`, `--jhc`, `--hugs`
:   Specify which Haskell implementation to use to build the package.
    At most one of these flags may be given. If none is given, the
    implementation under which the setup script was compiled or
    interpreted is used.

`--with-compiler=`_path_ or `-w`_path_
:   Specify the path to a particular compiler. If given, this must match
    the implementation selected above. The default is to search for the
    usual name of the selected implementation.

    This flag also sets the default value of the `--with-hc-pkg` option
    to the package tool for this compiler. Check the output of `setup
    configure -v` to ensure that it finds the right package tool (or use
    `--with-hc-pkg` explicitly).


`--with-hc-pkg=`_path_
:   Specify the path to the package tool, e.g. `ghc-pkg`. The package
    tool must be compatible with the compiler specified by
    `--with-compiler`. If this option is omitted, the default value is
    determined from the compiler selected.

`--with-`_`prog`_`=`_path_
:   Specify the path to the program _prog_. Any program known to Cabal
    can be used in place of _prog_. It can either be a fully path or the
    name of a program that can be found on the program search path. For
    example: `--with-ghc=ghc-6.6.1` or
    `--with-cpphs=/usr/local/bin/cpphs`.

`--`_`prog`_`-options=`_options_
:   Specify additional options to the program _prog_. Any program known
    to Cabal can be used in place of _prog_. For example:
    `--alex-options="--template=mytemplatedir/"`. The _options_ is split
    into program options based on spaces. Any options containing embeded
    spaced need to be quoted, for example
    `--foo-options='--bar="C:\Program File\Bar"'`. As an alternative
    that takes only one option at a time but avoids the need to quote,
    use `--`_`prog`_`-option` instead.

`--`_`prog`_`-option=`_option_
:   Specify a single additional option to the program _prog_. For
    passing an option that contain embeded spaces, such as a file name
    with embeded spaces, using this rather than `--`_`prog`_`-options`
    means you do not need an additional level of quoting. Of course if
    you are using a command shell you may still need to quote, for
    example `--foo-options="--bar=C:\Program File\Bar"`.

All of the options passed with either `--`_`prog`_`-options` or
`--`_`prog`_`-option` are passed in the order they were specified on the
configure command line.

### Installation paths ###

The following options govern the location of installed files from a
package:

`--prefix=`_dir_
:   The root of the installation. For example for a global install you
    might use `/usr/local` on a Unix system, or `C:\Program Files` on a
    Windows system. The other installation paths are usually
    subdirectories of _prefix_, but they don't have to be.

    In the simple build system, _dir_ may contain the following path
    variables: `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`,
    `$arch`

`--bindir=`_dir_
:   Executables that the user might invoke are installed here.

    In the simple build system, _dir_ may contain the following path
    variables: `$prefix`, `$pkgid`, `$pkg`, `$version`, `$compiler`,
    `$os`, `$arch`

`--libdir=`_dir_
:   Object-code libraries are installed here.

    In the simple build system, _dir_ may contain the following path
    variables: `$prefix`, `$bindir`, `$pkgid`, `$pkg`, `$version`,
    `$compiler`, `$os`, `$arch`

`--libexecdir=`_dir_
:   Executables that are not expected to be invoked directly by the user
    are installed here.

    In the simple build system, _dir_ may contain the following path
    variables: `$prefix`, `$bindir`, `$libdir`, `$libsubdir`, `$pkgid`,
    `$pkg`, `$version`, `$compiler`, `$os`, `$arch`

`--datadir`=_dir_
:   Architecture-independent data files are installed here.

    In the simple build system, _dir_ may contain the following path
    variables: `$prefix`, `$bindir`, `$libdir`, `$libsubdir`, `$pkgid`, `$pkg`,
    `$version`, `$compiler`, `$os`, `$arch`

In addition the simple build system supports the following installation path options:

`--libsubdir=`_dir_
:   A subdirectory of _libdir_ in which libraries are actually
    installed. For example, in the simple build system on Unix, the
    default _libdir_ is `/usr/local/lib`, and _libsubdir_ contains the
    package identifier and compiler, e.g. `mypkg-0.2/ghc-6.4`, so
    libraries would be installed in `/usr/local/lib/mypkg-0.2/ghc-6.4`.

    _dir_ may contain the following path variables: `$pkgid`, `$pkg`,
    `$version`, `$compiler`, `$os`, `$arch`

`--datasubdir=`_dir_
:   A subdirectory of _datadir_ in which data files are actually
    installed.

    _dir_ may contain the following path variables: `$pkgid`, `$pkg`,
    `$version`, `$compiler`, `$os`, `$arch`

`--docdir=`_dir_
:   Documentation files are installed relative to this directory.

    _dir_ may contain the following path variables: `$prefix`, `$bindir`,
    `$libdir`, `$libsubdir`, `$datadir`, `$datasubdir`, `$pkgid`, `$pkg`,
    `$version`, `$compiler`, `$os`, `$arch`

`--htmldir=`_dir_
:   HTML documentation files are installed relative to this directory.

    _dir_ may contain the following path variables: `$prefix`, `$bindir`,
    `$libdir`, `$libsubdir`, `$datadir`, `$datasubdir`, `$docdir`, `$pkgid`,
    `$pkg`, `$version`, `$compiler`, `$os`, `$arch`

`--program-prefix=`_prefix_
:   Prepend _prefix_ to installed program names.

    _prefix_ may contain the following path variables: `$pkgid`, `$pkg`,
    `$version`, `$compiler`, `$os`, `$arch`

`--program-suffix=`_suffix_
:   Append _suffix_ to installed program names. The most obvious use for
    this is to append the program's version number to make it possible
    to install several versions of a program at once:
    `--program-suffix='$version'`.

    _suffix_ may contain the following path variables: `$pkgid`, `$pkg`,
    `$version`, `$compiler`, `$os`, `$arch`

#### Path variables in the simple build system ####

For the simple build system, there are a number of variables that can be
used when specifying installation paths. The defaults are also specified
in terms of these variables. A number of the variables are actually for
other paths, like `$prefix`. This allows paths to be specified relative
to each other rather than as absolute paths, which is important for
building relocatable packages (see [prefix
independence](#prefix-independence)).

`$prefix`
:   The path variable that stands for the root of the installation. For
    an installation to be relocatable, all other instllation paths must
    be relative to the `$prefix` variable.

`$bindir`
:   The path variable that expands to the path given by the `--bindir`
    configure option (or the default).

`$libdir`
:   As above but for `--libdir`

`$libsubdir`
:   As above but for `--libsubdir`

`$datadir`
:   As above but for `--datadir`

`$datasubdir`
:   As above but for `--datasubdir`

`$docdir`
:   As above but for `--docdir`

`$pkgid`
:   The name and version of the package, eg `mypkg-0.2`

`$pkg`
:   The name of the package, eg `mypkg`

`$version`
:   The version of the package, eg `0.2`

`$compiler`
:   The compiler being used to build the package, eg `ghc-6.6.1`

`$os`
:   The operating system of the computer being used to build the
    package, eg `linux`, `windows`, `osx`, `freebsd` or `solaris`

`$arch`
:   The architecture of the computer being used to build the package, eg
    `i386`, `x86_64`, `ppc` or `sparc`

#### Paths in the simple build system ####

For the simple build system, the following defaults apply:

Option                     Windows Default                                           Unix Default
-------                    ----------------                                          -------------
`--prefix` (global)        `C:\Program Files\Haskell`                                `/usr/local`
`--prefix` (per-user)      `C:\Documents And Settings\user\Application Data\cabal`   `$HOME/.cabal`
`--bindir`                 `$prefix\bin`                                             `$prefix/bin`
`--libdir`                 `$prefix`                                                 `$prefix/lib`
`--libsubdir` (Hugs)       `hugs\packages\$pkg`                                      `hugs/packages/$pkg`
`--libsubdir` (others)     `$pkgid\$compiler`                                        `$pkgid/$compiler`
`--libexecdir`             `$prefix\$pkgid`                                          `$prefix/libexec`
`--datadir` (executable)   `$prefix`                                                 `$prefix/share`
`--datadir` (library)      `C:\Program Files\Haskell`                                `$prefix/share`
`--datasubdir`             `$pkgid`                                                  `$pkgid`
`--docdir`                 `$prefix\doc\$pkgid`                                      `$datadir/doc/$pkgid`
`--htmldir`                `$docdir\html`                                            `$docdir/html`
`--program-prefix`         (empty)                                                   (empty)
`--program-suffix`         (empty)                                                   (empty)


#### Prefix-independence ####

On Windows, and when using Hugs on any system, it is possible to obtain
the pathname of the running program. This means that we can construct an
installable executable package that is independent of its absolute
install location. The executable can find its auxiliary files by finding
its own path and knowing the location of the other files relative to
`$bindir`.  Prefix-independence is particularly
useful: it means the user can choose the install location (i.e. the
value of `$prefix`) at install-time, rather than
having to bake the path into the binary when it is built.

In order to achieve this, we require that for an executable on Windows,
all of `$bindir`, `$libdir`, `$datadir` and `$libexecdir` begin with
`$prefix`. If this is not the case then the compiled executable will
have baked-in all absolute paths.

The application need do nothing special to achieve prefix-independence.
If it finds any files using `getDataFileName` and the [other functions
provided for the purpose](#accessing-data-files-from-package-code), the
files will be accessed relative to the location of the current
executable.

A library cannot (currently) be prefix-independent, because it will be
linked into an executable whose file system location bears no relation
to the library package.

### Controlling Flag Assignments ###

Flag assignments (see the [resolution of conditions and
flags](#resolution-of-conditions-and-flags)) can be controlled with the
followingcommand line options.

`-f` _flagname_ or `-f` `-`_flagname_
:   Force the specified flag to `true` or `false` (if preceded with a `-`). Later
    specifications for the same flags will override earlier, i.e.,
    specifying `-fdebug -f-debug` is equivalent to `-f-debug`

`--flags=`_flagspecs_
:   Same as `-f`, but allows specifying multiple flag assignments at
    once. The parameter is a space-separated list of flag names (to
    force a flag to `true`), optionally preceded by a `-` (to force a
    flag to `false`). For example, `--flags="debug -feature1 feature2"` is
    equivalent to `-fdebug -f-feature1 -ffeature2`.

### Building Test Suites ###

`--enable-tests`
:   Build the test suites defined in the package description file during the
    `build` stage. Check for dependencies required by the test suites. If the
    package is configured with this option, it will be possible to run the test
    suites with the `test` command after the package is built.

`--disable-tests`
:   (default) Do not build any test suites during the `build` stage.
    Do not check for dependencies required only by the test suites. It will not
    be possible to invoke the `test` command without reconfiguring the package.

### Miscellaneous options ##

`--user`
:   Does a per-user installation. This changes the [default installation
    prefix](#paths-in-the-simple-build-system). It also allow
    dependencies to be satisfied by the user's package database, in
    addition to the global database. This also implies a default of
    `--user` for any subsequent `install` command, as packages
    registered in the global database should not depend on packages
    registered in a user's database.

`--global`
:   (default) Does a global installation. In this case package
    dependencies must be satisfied by the global package database. All
    packages in the user's package database will be ignored. Typically
    the final instllation step will require administrative privileges.

`--package-db=`_db_
:   Allows package dependencies to be satisfied from this additional
    package database _db_ in addition to the global package database.
    All packages in the user's package database will be ignored. The
    interpretation of _db_ is implementation-specific. Typically it will
    be a file or directory. Not all implementations support arbitrary
    package databases.

`--enable-optimization`[=_n_] or `-O`[_n_]
:   (default) Build with optimization flags (if available). This is
    appropriate for production use, taking more time to build faster
    libraries and programs.

    The optional _n_ value is the optimisation level. Some compilers
    support multiple optimisation levels. The range is 0 to 2. Level 0
    is equivalent to `--disable-optimization`, level 1 is the default if
    no _n_ parameter is given. Level 2 is higher optimisation if the
    compiler supports it. Level 2 is likely to lead to longer compile
    times and bigger generated code.

`--disable-optimization`
:   Build without optimization. This is suited for development: building
    will be quicker, but the resulting library or programs will be slower.

`--enable-library-profiling` or `-p`
:   Request that an additional version of the library with profiling
    features enabled be built and installed (only for implementations
    that support profiling).

`--disable-library-profiling`
:   (default) Do not generate an additional profiling version of the
    library.

`--enable-executable-profiling`
:   Any executables generated should have profiling enabled (only for
    implementations that support profiling). For this to work, all
    libraries used by these executables must also have been built with
    profiling support.

`--disable-executable-profiling`
:   (default) Do not enable profiling in generated executables.

`--enable-library-vanilla`
:   (default) Build ordinary libraries (as opposed to profiling
    libraries). This is independent of the `--enable-library-profiling`
    option. If you enable both, you get both.

`--disable-library-vanilla`
:   Do not build ordinary libraries. This is useful in conjunction with
    `--enable-library-profiling` to build only profiling libraries,
    rather than profiling and ordinary libraries.

`--enable-library-for-ghci`
:   (default) Build libraries suitable for use with GHCi.

`--disable-library-for-ghci`
:   Not all platforms support GHCi and indeed on some platforms, trying
    to build GHCi libs fails. In such cases this flag can be used as a
    workaround.

`--enable-split-objs`
:   Use the GHC `-split-objs` feature when building the library. This
    reduces the final size of the executables that use the library by
    allowing them to link with only the bits that they use rather than
    the entire library. The downside is that building the library takes
    longer and uses considerably more memory.

`--disable-split-objs`
:   (default) Do not use the GHC `-split-objs` feature. This makes
    building the library quicker but the final executables that use the
    library will be larger.

`--enable-executable-stripping`
:   (default) When installing binary executable programs, run the
    `strip` program on the binary. This can considerably reduce the size
    of the executable binary file. It does this by removing debugging
    information and symbols. While such extra information is useful for
    debugging C programs with traditional debuggers it is rarely helpful
    for debugging binaries produced by Haskell compilers.

    Not all Haskell implementations generate native binaries. For such
    implementations this option has no effect.

`--disable-executable-stripping`
:   Do not strip binary executables during installation. You might want
    to use this option if you need to debug a program using gdb, for
    example if you want to debug the C parts of a program containing
    both Haskell and C code. Another reason is if your are building a
    package for a system which has a policy of managing the stripping
    itself (such as some linux distributions).

`--enable-shared`
:   Build shared library. This implies a seperate compiler run to
    generate position independent code as required on most platforms.

`--disable-shared`
:   (default) Do not build shared library.

`--configure-option=`_str_
:   An extra option to an external `configure` script, if one is used
    (see the section on [system-dependent
    parameters](#system-dependent-parameters)).  There can be several of
    these options.

`--extra-include-dirs`[=_dir_]
:   An extra directory to search for C header files. You can use this
    flag multiple times to get a list of directories.

    You might need to use this flag if you have standard system header
    files in a non-standard location that is not mentioned in the
    package's `.cabal` file. Using this option has the same affect as
    appending the directory _dir_ to the `include-dirs` field in each
    library and executable in the package's `.cabal` file. The advantage
    of course is that you do not have to modify the package at all.
    These extra directories will be used while building the package and
    for libraries it is also saved in the package registration
    information and used when compiling modules that use the library.

`--extra-lib-dirs`[=_dir_]
:   An extra directory to search for system libraries files. You can use
    this flag multiple times to get a list of directories.

    You might need to use this flag if you have standard system
    libraries in a non-standard location that is not mentioned in the
    package's `.cabal` file. Using this option has the same affect as
    appending the directory _dir_ to the `extra-lib-dirs` field in each
    library and executable in the package's `.cabal` file. The advantage
    of course is that you do not have to modify the package at all.
    These extra directories will be used while building the package and
    for libraries it is also saved in the package registration
    information and used when compiling modules that use the library.

In the simple build infrastructure, an additional option is recognized:

`--scratchdir=`_dir_
:   Specify the directory into which the Hugs output will be placed
    (default: `dist/scratch`).

## setup build ##

Perform any preprocessing or compilation needed to make this package ready for installation.

This command takes the following options:

--_prog_-options=_options_, --_prog_-option=_option_
:   These are mostly the same as the [options configure
    step](#setup-configure). Unlike the options specified at the
    configure step, any program options specified at the build step are
    not persistent but are used for that invocation only. They options
    specified at the build step are in addition not in replacement of
    any options specified at the configure step.

## setup haddock ##

Build the documentation for the package using [haddock][]. By default,
only the documentation for the exposed modules is generated (but see the
`--executables` and `--internal` flags below).

This command takes the following options:

`--hoogle`
:   Generate a file `dist/doc/html/`_pkgid_`.txt`, which can be
    converted by [Hoogle](http://www.haskell.org/hoogle/) into a
    database for searching. This is equivalent to running  [haddock][]
    with the `--hoogle` flag.

`--html-location=`_url_
:   Specify a template for the location of HTML documentation for
    prerequisite packages.  The substitutions ([see
    listing](#paths-in-the-simple-build-system)) are applied to the
    template to obtain a location for each package, which will be used
    by hyperlinks in the generated documentation. For example, the
    following command generates links pointing at [Hackage] pages:

    > setup haddock --html-location='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html'

    Here the argument is quoted to prevent substitution by the shell. If
    this option is omitted, the location for each package is obtained
    using the package tool (e.g. `ghc-pkg`).

`--executables`
:   Also run [haddock][] for the modules of all the executable programs.
    By default [haddock][] is run only on the exported modules.

`--internal`
:   Run [haddock][] for the all modules, including unexposed ones, and
    make [haddock][] generate documentation for unexported symbols as
    well.

`--css=`_path_
:   The argument _path_ denotes a CSS file, which is passed to
    [haddock][] and used to set the style of the generated
    documentation. This is only needed to override the default style
    that [haddock][] uses.

`--hyperlink-source`
:   Generate [haddock][] documentation integrated with [HsColour][].
    First, [HsColour][] is run to generate colourised code. Then
    [haddock][] is run to generate HTML documentation.  Each entity
    shown in the documentation is linked to its definition in the
    colourised code.

`--hscolour-css=`_path_
:   The argument _path_ denotes a CSS file, which is passed to [HsColour][] as in

    > runhaskell Setup.hs hscolour --css=_path_

## setup hscolour ##

Produce colourised code in HTML format using [HsColour][]. Colourised
code for exported modules is put in `dist/doc/html/`_pkgid_`/src`.

This command takes the following options:

`--executables`
:   Also run [HsColour][] on the sources of all executable programs.
    Colourised code is put in `dist/doc/html/`_pkgid_/_executable_`/src`.

`--css=`_path_
:   Use the given CSS file for the generated HTML files. The CSS file
    defines the colours used to colourise code. Note that this copies
    the given CSS file to the directory with the generated HTML files
    (renamed to `hscolour.css`) rather than linking to it.

## setup install ##

Copy the files into the install locations and (for library packages)
register the package with the compiler, i.e. make the modules it
contains available to programs.

The [install locations](#installation-paths) are determined by options
to `setup configure`.

This command takes the following options:

`--global`
:   Register this package in the system-wide database. (This is the
    default, unless the `--user` option was supplied to the `configure`
    command.)

`--user`
:   Register this package in the user's local package database. (This is
    the default if the `--user` option was supplied to the `configure`
    command.)

## setup copy ##

Copy the files without registering them.  This command is mainly of use
to those creating binary packages.

This command takes the following option:

`--destdir=`_path_

Specify the directory under which to place installed files.  If this is
not given, then the root directory is assumed.

## setup register ##

Register this package with the compiler, i.e. make the modules it
contains available to programs. This only makes sense for library
packages. Note that the `install` command incorporates this action.  The
main use of this separate command is in the post-installation step for a
binary package.

This command takes the following options:

`--global`
:   Register this package in the system-wide database. (This is the default.)


`--user`
:   Register this package in the user's local package database.


`--gen-script`
:   Instead of registering the package, generate a script containing
    commands to perform the registration.  On Unix, this file is called
    `register.sh`, on Windows, `register.bat`.  This script might be
    included in a binary bundle, to be run after the bundle is unpacked
    on the target system.

`--gen-pkg-config`[=_path_]
:   Instead of registering the package, generate a package registration
    file. This only applies to compilers that support package
    registration files which at the moment is only GHC. The file should
    be used with the compiler's mechanism for registering packages. This
    option is mainly intended for packaging systems. If possible use the
    `--gen-script` option instead since it is more portable across
    Haskell implementations. The _path_ is
    optional and can be used to specify a particular output file to
    generate. Otherwise, by default the file is the package name and
    version with a `.conf` extension.

`--inplace`
:   Registers the package for use directly from the build tree, without
    needing to install it.  This can be useful for testing: there's no
    need to install the package after modifying it, just recompile and
    test.

    This flag does not create a build-tree-local package database.  It
    still registers the package in one of the user or global databases.

    However, there are some caveats.  It only works with GHC
    (currently).  It only works if your package doesn't depend on having
    any supplemental files installed --- plain Haskell libraries should
    be fine.

## setup unregister ##

Deregister this package with the compiler.

This command takes the following options:

`--global`
:   Deregister this package in the system-wide database. (This is the default.)

`--user`
:   Deregister this package in the user's local package database.

`--gen-script`
:   Instead of deregistering the package, generate a script containing
    commands to perform the deregistration.  On Unix, this file is
    called `unregister.sh`, on Windows, `unregister.bat`. This script
    might be included in a binary bundle, to be run on the target
    system.

## setup clean ##

Remove any local files created during the `configure`, `build`,
`haddock`, `register` or `unregister` steps, and also any files and
directories listed in the `extra-tmp-files` field.

This command takes the following options:

`--save-configure` or `-s`
:   Keeps the configuration information so it is not necessary to run
    the configure step again before building.

## setup test ##

Run the test suites specified in the package description file.  Aside from
the following flags, Cabal accepts the name of one or more test suites on the
command line after `test`.  When supplied, Cabal will run only the named test
suites, otherwise, Cabal will run all test suites in the package.

`--builddir=`_dir_
:   The directory where Cabal puts generated build files (default: `dist`).
    Test logs will be located in the `test` subdirectory.

`--human-log=`_path_
:   The template used to name human-readable test logs; the path is relative
    to `dist/test`.  By default, logs are named according to the template
    `$pkgid-$test-suite.log`, so that each test suite will be logged to its own
    human-readable log file.  Template variables allowed are: `$pkgid`,
    `$compiler`, `$os`, `$arch`, `$test-suite`, and `$result`.

`--machine-log=`_path_
:   The path to the machine-readable log, relative to `dist/test`.  The default
    template is `$pkgid.log`.  Template variables allowed are: `$pkgid`,
    `$compiler`, `$os`, `$arch`, and `$result`.

`--show-details=`_filter_
:   Determines if the results of individual test cases are shown on the
    terminal.  May be `always` (always show), `never` (never show), or
    `failures` (show only the test cases of failing test suites).

`--test-options=`_options_
:   Give extra options to the test executables.

`--test-option=`_option_
:   give an extra option to the test executables.  There is no need to quote
    options containing spaces because a single option is assumed, so options
    will not be split on spaces.

## setup sdist ##

Create a system- and compiler-independent source distribution in a file
_package_-_version_`.tar.gz` in the `dist` subdirectory, for
distribution to package builders.  When unpacked, the commands listed in
this section will be available.

The files placed in this distribution are the package description file,
the setup script, the sources of the modules named in the package
description file, and files named in the `license-file`, `main-is`,
`c-sources`, `data-files` and `extra-source-files` fields.

This command takes the following option:

`--snapshot`
:   Append today's date (in "YYYYMMDD" format) to the version number for
    the generated source package.  The original package is unaffected.


[dist-simple]:  ../libraries/Cabal/Distribution-Simple.html
[dist-make]:    ../libraries/Cabal/Distribution-Make.html
[dist-license]: ../libraries/Cabal/Distribution-License.html#t:License
[extension]:    ../libraries/Cabal/Language-Haskell-Extension.html#t:Extension
[BuildType]:    ../libraries/Cabal/Distribution-PackageDescription.html#t:BuildType
[alex]:       http://www.haskell.org/alex/
[autoconf]:   http://www.gnu.org/software/autoconf/
[c2hs]:       http://www.cse.unsw.edu.au/~chak/haskell/c2hs/
[cpphs]:      http://www.haskell.org/cpphs/
[greencard]:  http://www.haskell.org/greencard/
[haddock]:    http://www.haskell.org/haddock/
[HsColour]:   http://www.cs.york.ac.uk/fp/darcs/hscolour/
[happy]:      http://www.haskell.org/happy/
[Hackage]:    http://hackage.haskell.org/
[pkg-config]: http://pkg-config.freedesktop.org/
