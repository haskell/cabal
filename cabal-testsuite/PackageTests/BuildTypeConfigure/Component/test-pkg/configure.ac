AC_INIT([Haskell Test package], [1.1], [libraries@haskell.org], [Test])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([test-pkg.cabal])

# Header file to place defines in
AC_CONFIG_HEADERS([include/test.h])
echo "CWD=$PWD"

AC_SUBST([LIB_OPTS], [-DLIB="\"Hi-Lib\""])
AC_SUBST([EXE_OPTS], [-DEXE="\"Hi-Exe\""])

AC_CONFIG_FILES([test-pkg.buildinfo])
AC_OUTPUT

touch configure.done
