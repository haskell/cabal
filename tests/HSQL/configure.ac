AC_INIT([HSQL],[1.0],[htoolkit-users@lists.sourceforge.net],[hsql])

AC_CONFIG_FILES([config.mk:config.mk.in hsql.pkg:hsql.pkg.in])

AC_CANONICAL_SYSTEM

dnl ***********************************************
dnl Enable/Disable ODBC binding
dnl ***********************************************
AC_ARG_ENABLE(odbc,
[  --enable-odbc
        Build an ODBC binding for Haskell.
],
[WithODBC=YES],
[WithODBC=NO]
)
AC_SUBST(WithODBC)

dnl ***********************************************
dnl Enable/Disable PostgreSQL binding
dnl ***********************************************
AC_ARG_ENABLE(postgres,
[  --enable-postgres
        Build a PostgreSQL binding for Haskell.
],
[WithPostgreSQL=YES],
[WithPostgreSQL=NO]
)
AC_SUBST(WithPostgreSQL)

dnl ***********************************************
dnl Enable/Disable MySQL binding
dnl ***********************************************
AC_ARG_ENABLE(mysql,
[  --enable-mysql
        Build a MySQL binding for Haskell.
],
[WithMySQL=YES],
[WithMySQL=NO]
)
AC_SUBST(WithMySQL)

dnl ***********************************************
dnl Enable/Disable SQLite binding
dnl ***********************************************
AC_ARG_ENABLE(sqlite,
[  --enable-sqlite
        Build a SQLite binding for Haskell.
],
[WithSQLite=YES],
[WithSQLite=NO]
)
AC_SUBST(WithSQLite)

dnl ***********************************************
dnl GHC
dnl ***********************************************
AC_ARG_WITH(ghc,
[  --with-ghc=<ghc command>
        Use a different command instead of 'ghc' for the Haskell compiler.
],
[GHC="$withval"],
[AC_PATH_PROG(GHC,ghc)]
)

if test "$GHC" = "" || test ! -f $GHC; then
  AC_MSG_RESULT([The build for GHC will be skiped.])
  GHC=""
fi

if test "x$prefix" != xNONE; then
    GHC_DIR="$prefix/lib/HSQL/GHC"
else
    GHC_DIR="$ac_default_prefix/lib/HSQL/Hugs"
fi

AC_SUBST(GHC)
AC_SUBST(GHC_DIR)

dnl ***********************************************
dnl hsc2hs
dnl ***********************************************
AC_ARG_WITH(hsc2hs,
[  --with-hsc2hs=<hsc2hs command>
        Use a different command instead of 'hsc2hs'
],
[HSC2HS="$withval"],
[AC_PATH_PROG(HSC2HS,hsc2hs)]
)

if test "$HSC2HS" = "" || test ! -f $HSC2HS; then
  AC_MSG_ERROR([HSC2HS is required to build the package])
fi

AC_SUBST(HSC2HS)

dnl ***********************************************
dnl ghc-pkg
dnl ***********************************************
AC_ARG_WITH(ghc-pkg,
[  --with-ghc-pkg=<ghc-pkg command>
        Use a different command instead of 'ghc-pkg'
],
[GHC_PKG="$withval"],
[AC_PATH_PROG(GHC_PKG,ghc-pkg)]
)

if test "$GHC_PKG" = "" || test ! -f $GHC_PKG; then
  AC_MSG_ERROR([ghc-pkg is required to build the package])
fi

AC_SUBST(GHC_PKG)

dnl ***********************************************
dnl HUGS
dnl ***********************************************
AC_ARG_WITH(hugs,
[  --with-hugs=<hugs command>
        Use a different command instead of 'hugs' for the Hugs interpreter.
],
[HUGS="$withval"],
[AC_PATH_PROG(HUGS,hugs)]
)

if test "$HUGS" = "" || test ! -f $HUGS; then
  AC_MSG_RESULT([The build for HUGS will be skiped.])
  HUGS=""
fi

if test "x$prefix" != xNONE; then
    HUGS_DIR="$prefix/lib/HSQL/Hugs"
else
    HUGS_DIR="$ac_default_prefix/lib/HSQL/Hugs"
fi

case $ac_cv_target_alias in
    i[[3456]]86-*-cygwin*|i[[3456]]86-*-mingw32*)
        SO_EXT=dll;;
    *)  SO_EXT=so;;
esac

AC_SUBST(HUGS)
AC_SUBST(HUGS_DIR)
AC_SUBST(SO_EXT)

dnl ***********************************************
dnl FFIHUGS
dnl ***********************************************
AC_ARG_WITH(ffihugs,
[  --with-ffihugs=<ffihugs command>
        Use a different command instead of 'ffihugs' for the Hugs FFI compiler.
],
[FFIHUGS="$withval"],
[AC_PATH_PROG(FFIHUGS,ffihugs)]
)

if test "$HUGS" != ""; then
	if test "$FFIHUGS" = "" || test ! -f $FFIHUGS; then
	   AC_MSG_ERROR([ffihugs is required to build the Hugs libraries])
	fi
fi

AC_SUBST(FFIHUGS)

dnl ***********************************************
dnl HADDOCK
dnl ***********************************************
AC_ARG_WITH(haddock,
[  --with-haddock=<haddock command>
        Use a different command instead of 'haddock' for the documentation builder.
],
[HADDOCK="$withval"],
[AC_PATH_PROG(HADDOCK,haddock)]
)

if test "$HADDOCK" = "" || test ! -f $HADDOCK; then
  AC_MSG_RESULT([HADDOCK is required to build the documentations])
fi

if test "x$prefix" != xNONE; then
    DOC_DIR="$prefix/doc/HSQL"
else
    DOC_DIR="$ac_default_prefix/doc/HSQL"
fi

AC_SUBST(HADDOCK)
AC_SUBST(DOC_DIR)

dnl ***********************************************
dnl other progs
dnl ***********************************************

AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL

AC_PATH_PROG(AR,ar)
AC_SUBST(AR)

AC_PATH_PROG(LD,ld)
AC_SUBST(LD)

case $ac_cv_target_alias in
    i[[3456]]86-*-cygwin*|i[[3456]]86-*-mingw32*)
        AC_CHECK_HEADER(windows.h)
        CFLAGS="$CFLAGS -mno-cygwin"
        CPPFLAGS="$CPPFLAGS -D_WIN32_"
        ac_includes_default="$ac_includes_default
#include <windows.h>"
        WIN32=YES
        ;;
    *)  WIN32=NO
        ;;
esac

AC_SUBST(WIN32)

dnl ***********************************************
dnl check for headers and libraries for ODBC
dnl ***********************************************


if test $WithODBC = YES; then
    AC_CHECK_HEADER(sqlext.h,,AC_MSG_ERROR([sqlext.h and libodbc required to build ODBC building.]))
    case $WIN32 in
        YES)
            AC_COMPILE_IFELSE(
            [
            #include <windows.h>
            #include <sqlext.h>
          
            int main()
            {
        	SQLAllocEnv (NULL);
        	return 0;
            }
            ],
            [LIBS="${LIBS} -lodbc32"],
            AC_MSG_ERROR([sqlext.h and libodbc required to build ODBC building.]))
            ;;
        NO)
            AC_CHECK_LIB(odbc,SQLAllocEnv,,AC_MSG_ERROR([sqlext.h and libodbc required to build ODBC building.]))
            ;;
    esac

    CPPFLAGS="$CPPFLAGS -Isrc/HSQL"
fi

dnl ***********************************************
dnl check for headers and libraries for PostgreSQL
dnl ***********************************************

if test $WithPostgreSQL = YES; then
  if test $WIN32 = NO; then
    AC_PATH_PROG(PG_CONFIG, pg_config)

    if test "$PG_CONFIG" = "" || test ! -f $PG_CONFIG; then
      AC_MSG_ERROR([pg_config is required to build PostgreSQL binding])
    fi

    incdir=`$PG_CONFIG --includedir`
    incdir_server=`$PG_CONFIG --includedir-server`
    case $ac_cv_target_alias in
      i[[3456]]86-*-cygwin*|i[[3456]]86-*-mingw32*)
          LDFLAGS="$LDFLAGS -L$(cygpath -m `$PG_CONFIG --libdir`)"
          incdir=$(cygpath -m $incdir)
          CPPFLAGS="$CPPFLAGS -I$(cygpath -m /usr/include) -I$incdir -I$incdir_server"
          ;;
      *)  LDFLAGS="$LDFLAGS -L`$PG_CONFIG --libdir`"
          CPPFLAGS="$CPPFLAGS  -I$incdir -I$incdir_server"
          ;;
    esac
  else
    CPPFLAGS="$CPPFLAGS  -D_MSC_VER"  
  fi

  AC_CHECK_HEADER(libpq-fe.h,,AC_MSG_ERROR([libpq-fe.h header not found]))
  AC_CHECK_HEADER(postgres.h,,AC_MSG_ERROR([postgres.h header not found]))
  
  case $WIN32 in
    YES) AC_CHECK_LIB(libpq,PQsetdbLogin,,AC_MSG_ERROR([liblibpq.a library not found]));;
     NO) AC_CHECK_LIB(pq,   PQsetdbLogin,,AC_MSG_ERROR([libpq.a library not found]));;
  esac
fi

dnl ***********************************************
dnl check for headers and libraries for MySQL
dnl ***********************************************

if test $WithMySQL = YES; then
    if test $WIN32 = NO; then
        AC_PATH_PROG(MYSQL_CONFIG, mysql_config)
        
        if test "$MYSQL_CONFIG" = "" || test ! -f $MYSQL_CONFIG; then
          AC_MSG_ERROR([mysql_config is required to build MySQL binding])
        fi
        
        LDFLAGS="$LDFLAGS `$MYSQL_CONFIG --libs`"
        
        for mysql_opt in `$MYSQL_CONFIG --cflags`
        do
          case $mysql_opt in
            -I*)
              CPPFLAGS="$CPPFLAGS ${mysql_opt}";;
          esac
        done
    fi

    AC_CHECK_HEADER(mysql.h,, AC_MSG_ERROR([mysql.h header not found]))
    
    case $WIN32 in
        YES)
            AC_COMPILE_IFELSE(
            [
            #include <windows.h>
            #include <mysql.h>

            int main ()
            {
                mysql_init (NULL);
                return 0;
            }
            ],
            [LIBS="${LIBS} -llibmysql"],
            AC_MSG_ERROR([liblibmysql.a library not found]))
            ;;
        NO)
            AC_CHECK_LIB(mysqlclient,mysql_init,,AC_MSG_ERROR([libmysql.a library not found]))
            ;;
    esac

fi

dnl ***********************************************
dnl check for headers and libraries for SQLite
dnl ***********************************************

if test $WithSQLite = YES; then
AC_CHECK_HEADER(sqlite.h,, AC_MSG_ERROR([sqlite.h header not found]))
AC_CHECK_LIB(sqlite,sqlite_open,,AC_MSG_ERROR([sqlite.h and libsqlite required to build SQLite building.]))

LDFLAGS="$LDFLAGS -lsqlite"
fi

dnl ***********************************************
dnl subst
dnl ***********************************************

LDFLAGS="${LIBS} ${LDFLAGS}"
LIB_DIRS='"'${GHC_DIR}'"'

for lib_opt in ${LDFLAGS}
do
   case $lib_opt in
	 -l*)
			if test x$DEP_LIBS = x; then
				DEP_LIBS='"'`echo ${lib_opt} | sed s,-l,,`'"'
			else
				DEP_LIBS=$DEP_LIBS,'"'`echo ${lib_opt} | sed s,-l,,`'"'
			fi;;
	 -L*)
			LIB_DIRS=$LIB_DIRS,'"'`echo ${lib_opt} | sed s,-L,, | sed s,"'",, | sed s,"'",,`'"'
   esac
done

AC_SUBST(CPPFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(DEP_LIBS)
AC_SUBST(LIB_DIRS)

dnl ***********************************************
dnl Create directories and output files
dnl ***********************************************

AC_OUTPUT

echo "creating output directories:"
echo " - build/Database/HSQL"
mkdir -p build/Database/HSQL

echo
echo "Backends"
echo "--------"
echo
echo "MySQL:       $WithMySQL"
echo "PostgreSQL:  $WithPostgreSQL"
echo "SQLite:      $WithSQLite"
echo "ODBC:        $WithODBC"
echo
