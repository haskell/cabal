Build Hooks
===========

Build hooks are programs that are run before (pre-build hook) and
after (post-build hook) a package (including package dependencies)
is built. The hooks are completely generic and can even be absent
(their absence is ignored). Regardless of the return code of the
pre-build hook, the normal build is executed. In the case where
the pre-build hook provides a pre-built version of what the build
step would provide, the build step is still run, but should be
little more than a NOOP.

Build hooks are project local rather than global to the user
because a single user may want to use one set of hooks in one
project and another set of hooks (or even none at all) for another
project.

Since the hook files are project local (and hence likely to be committed
to revision control), a naive implementation of these hooks would be
a potential security issue. A solution to this potential security
issue has been implemented and described in the Hook Security section
below.


Possible Use Cases
------------------

Possible use cases include:

* Fine grained benchmarking of individual package build times.
* Build product caching.


Location of Hook Files
----------------------

The two hook files are `cabalHooks/preBuildHook` and
`cabalHooks/postBuildHook` where the `cabalHooks` directory is in
the same directory as the `cabal.project` file. On UNIX style
systems, these hooks need to be marked as user executable programs.


Hook Parameters Exit Codes
--------------------------

The pre-build hook is passed three parameters; the unit id (from cabal),
the source directory and the build directory. The post-build hook is
passed the same three parameters, plus the exit code of the pre-build
hook.

The exit codes for the two hooks are ignored by cabal apart from cabal
capturing the exit code for the pre-build hook and passing it to the
post-build hook.


Hook Security
-------------
These hook files are generic executable programs and are stored local to
the project. To prevent the running of malicious hook files, the
hook files are only run, it they are mentioned in the `hooks-security`
file which is located in the users home `.cabal` directory, usually
either `$HOME/.cabal/` or `$HOME/.config/cabal/`.

The `hooks-security` file should contain one entry per line. Blank lines
are ignored, as are Haskell style single line comments (starts with "--"
and goes until the end of the line). Each entry should contain the full
hook file path, at least one space and either "AcceptAlways" or
"AcceptHash" followed by at least one space and the hexadecimal encoded
hash of the file. The `hooks-security` file is read once when `cabal` is
started and the entries inserted into a `Map`.

When `cabal` detects a "preBuildHook" or "postBuildHook" it looks up
the full file path in the `Map`. If the path is not found in the `Map`,
`cabal` will die with an error message suggesting that the hook file
be manually inspected and if deemed safe, added to the `hooks-security`
file.

If the hook file path is in the `Map` and it was specified as
"AcceptAlways" the hook will be run. If the `Map` entry is "AcceptHash"
with a hash, the hash of the hook file will be calculated and compared
against the supplied hash. If the hashes match, the hook will be run.
If there is a hash mismatch, `cabal` will abort with an error message
about the hash mismatch.
