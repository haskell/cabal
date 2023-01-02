AC_INIT([Haskell zlib package], [1.1], [libraries@haskell.org], [zlib])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([zlib.cabal])

# Header file to place defines in
AC_CONFIG_HEADERS([include/HsZlibConfig.h])

# Check for zlib include
AC_CHECK_HEADER(zlib.h, [ZLIB_HEADER=yes], [], [])

# Check that flag assignment has been propagated correctly
if test "$CABAL_FLAG_true_flag" != "1"; then
    echo "true flag incorrectly set: got '$CABAL_FLAG_true_flag'"
    exit 1
fi
if test "$CABAL_FLAG_false_flag" != "0"; then
    echo "false flag incorrectly set: got '$CABAL_FLAG_false_flag'"
    exit 1
fi

if test "$CABAL_FLAGS" != "+con-flict +con_flict -false-flag +true-flag"; then
    echo "CABAL_FLAGS incorrectly set: got '$CABAL_FLAGS'"
    exit 1
fi

# Build the package if we found X11 stuff
if test "x$ZLIB_HEADER" = "x"
then BUILD_PACKAGE_BOOL=False
else BUILD_PACKAGE_BOOL=True
fi
AC_SUBST([BUILD_PACKAGE_BOOL])

AC_CONFIG_FILES([zlib.buildinfo])
AC_OUTPUT
