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


Security Considerations
-----------------------

These build hooks are generic executable programs. They can potentially
be malicious. For example, one might clone a Haskell project from
say Github, that includes malicious build hooks so that when the user runs
`cabal build all` these hooks will be run as the user. The most obvious
malicious behaviour would be to delete all the user's files.

For this reason, it is highly advisable to check for the existence
of and the contents of any build hook files.
