Summary: Haskell database interaction library
Name: HSQL
Version: 1.4
Release: 1
Copyright: BSD
vendor: HToolkit Team
packager: Conny Andersson <forester@dtek.chalmers.se>
Group: Development/Libraries
Source: HSQL.tar.gz
Requires: ghc >= 6.2.1
Requires: hugs98 = Nov2003
Requires: sqlite >= 2.8.12
Requires: postgresql-libs >= 7.4.1
Requires: unixODBC >= 2.2.5
Requires: MySQL-shared >= 4.1.1
%description
HSQL allows haskell programmers to interact with databases using MySQL, PostgreSQL, ODBC and SQLite.

%prep
%setup -n HSQL

%post
ghc-pkg -u --auto-ghci-libs <<- \EOF
Package
    {name = "hsql",
     auto=True,
     import_dirs = ["/usr/lib/ghc-6.2/imports"],
     source_dirs = [],
     library_dirs = ["/usr/lib/ghc-6.2","/usr/lib","/usr/lib/mysql"],
     hs_libraries = ["HSsql"],
     extra_libraries = ["sqlite","pq","odbc","mysqlclient","z","crypt","nsl","m","c","nss_files","nss_dns","resolv","c","nss_files","nss_dns","resolv","sqlite"],
     include_dirs = [],
     c_includes = [],
     package_deps = ["base"],
     extra_ghc_opts = [],
     extra_cc_opts = [],
     extra_ld_opts = [],
     framework_dirs = [],
     extra_frameworks = []
    }
EOF

%postun
ghc-pkg -r hsql

%build
./configure --enable-mysql --enable-postgres --enable-odbc --enable-sqlite
make
make docs

%install
make install

%files
%defattr (-,root,root)

#GHC
/usr/lib/ghc-6.2/libHSsql.a
/usr/lib/ghc-6.2/imports/Database/HSQL
/usr/lib/ghc-6.2/imports/Database/HSQL.hi

#Hugs
/usr/lib/hugs/libraries/Database/HSQL
/usr/lib/hugs/libraries/Database/HSQL.hs
/usr/lib/hugs/libraries/Database/HSQL.so

#GHC-Docs
/usr/lib/ghc-6.2/doc/html/libraries/hsql
