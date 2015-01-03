#!/usr/bin/env sh

# A script to bootstrap cabal-install.

# It works by downloading and installing any needed dependencies for
# cabal-install to a local sandbox. It then installs cabal-install
# itself into this sandbox.
# It expects to be run inside the cabal-install directory.

# Known portability: tested on debian unstable in early 2015
# tested with ghc 7.2.2, 7.4.x, 7.6.[23], 7.8.x, 7.10.0-20141222
# fails with ghc 7.6.1 on a hang compiling stm

# It should work on other posix systems ...

die () { printf "\nError during cabal-install bootstrap:\n$1\n" >&2 && exit 2 ;}

# you can override this variable for debugging
#VERBOSE
# programs, you can override these by setting environment vars
GHC="${GHC:-ghc}"
GHC_PKG="${GHC_PKG:-ghc-pkg}"
GHC_VER="$(${GHC} --numeric-version)"
HADDOCK=${HADDOCK:-haddock}
WGET="${WGET:-wget}"
CURL="${CURL:-curl}"
FETCH="${FETCH:-fetch}"
TAR="${TAR:-tar}"
GZIP_PROGRAM="${GZIP_PROGRAM:-gzip}"

###############################
# preparation

# Try to respect $TMPDIR but override if needed - see #1710.
[ -"$TMPDIR"- = -""- ] || echo "$TMPDIR" | grep -q ld &&
  export TMPDIR=/tmp/cabal-$(echo $(od -XN4 -An /dev/random)) && mkdir $TMPDIR

# Check for a C compiler.
[ ! -x "$CC" ] && for ccc in gcc clang cc icc; do
  ${ccc} --version > /dev/null 2>&1 && CC=$ccc &&
  echo "Using $CC for C compiler. If this is not what you want, set CC." >&2 &&
  break
done

# None found.
[ ! -x `which "$CC"` ] &&
  die "C compiler not found (or could not be run).
       If a C compiler is installed make sure it is on your PATH,
       or set the CC variable."

# Check the C compiler/linker work.
LINK="$(for link in collect2 ld; do
  echo 'main;' | ${CC} -v -x c - -o /dev/null -\#\#\# 2>&1 | grep -q $link &&
  echo 'main;' | ${CC} -v -x c - -o /dev/null -\#\#\# 2>&1 | grep    $link |
  sed -e "s|\(.*$link\).*|\1|" -e 's/ //g' -e 's|"||' && break
done)"

# They don't.
[ -z "$LINK" ] &&
  die "C compiler and linker could not compile a simple test program.
       Please check your toolchain."

## Warn that were's overriding $LD if set (if you want).

[ -x "$LD" ] && [ "$LD" != "$LINK" ] &&
  echo "Warning: value set in $LD is not the same as C compiler's $LINK." >&2
  echo "Using $LINK instead." >&2

# Set LD, overriding environment if necessary.
LD=$LINK

# Check we're in the right directory, etc.
grep "cabal-install" ./cabal-install.cabal > /dev/null 2>&1 ||
  die "The bootstrap.sh script must be run in the cabal-install directory"

${GHC} --numeric-version > /dev/null 2>&1  ||
  die "${GHC} not found (or could not be run).
       If ghc is installed,  make sure it is on your PATH,
       or set the GHC and GHC_PKG vars."

${GHC_PKG} --version     > /dev/null 2>&1  || die "${GHC_PKG} not found."

GHC_VER="$(${GHC} --numeric-version)"
GHC_PKG_VER="$(${GHC_PKG} --version | cut -d' ' -f 5)"

# use -j to compile Setup.hs files directly with ghc if ghc version is
# 7.8 or greater. This is purely an optimisation for the build time

GHC_JOBS=""

[ -n "$(echo ${GHC_VER} | egrep '^(7\.8)|^(7\.1[0-9]|^8)')" ] && GHC_JOBS=-j

[ ${GHC_VER} = ${GHC_PKG_VER} ] ||
  die "Version mismatch between ${GHC} and ${GHC_PKG}.
       If you set the GHC variable then set GHC_PKG too."

####################################
# create the 'sandbox'

abspath () { case "$1" in /*)printf "%s\n" "$1";; *)printf "%s\n" "$PWD/$1";;
             esac; }

SANDBOX=$(abspath ".cabal-sandbox")
# Get the name of the package database which cabal sandbox would use.
GHC_ARCH=$(ghc --info |
    sed -n 's/.*"Target platform".*"\([^-]\+\)-[^-]\+-\([^"]\+\)".*/\1-\2/p')
PACKAGEDB="$SANDBOX/${GHC_ARCH}-ghc-${GHC_VER}-packages.conf.d"
# Assume that if the directory is already there, it is already a
# package database. We will get an error immediately below if it
# isn't. Uses -r to try to be compatible with Solaris, and allow
# symlinks as well as a normal dir/file.
[ ! -r "$PACKAGEDB" ] && ${GHC_PKG} init "$PACKAGEDB"

GHC_PACKAGE_ARGS="-package-db $PACKAGEDB"
CABAL_PACKAGE_ARGS="--user --package-db=$PACKAGEDB"
GHC_PKG_PACKAGE_ARGS=$CABAL_PACKAGE_ARGS
# support the old argument names in ghc and ghc-pkg in ghc-7.4 and
# earlier
if [ -n "$(echo ${GHC_VER} | egrep '^7\.[24]\.')" ]
then
    GHC_PKG_PACKAGE_ARGS="--package-conf=$PACKAGEDB"
    GHC_PACKAGE_ARGS="-package-conf=$PACKAGEDB"
fi

####################################
# main functions

HACKAGE_URL="https://hackage.haskell.org/package"

# Cache the list of packages:
echo "Checking installed packages for ghc-${GHC_VER}..."
${GHC_PKG} list --global $GHC_PKG_PACKAGE_ARGS > ghc-pkg.list ||
  die "running '${GHC_PKG} list --global $GHC_PKG_PACKAGE_ARGS' failed"

# Will we need to install this package, or is it already installed?
need_pkg () {
  PKG=$1
  VER_MATCH=$(echo $2 | sed "s/\./\\\./g")
  if egrep "${PKG}-${VER_MATCH}" ghc-pkg.list > /dev/null 2>&1
  then
    return 1;
  else
    return 0;
  fi
  #Note: we cannot use "! grep" here as Solaris 9 /bin/sh doesn't like it.
}

info_pkg () {
  PKG=$1
  VER=$2

  if need_pkg ${PKG} ${VER}
  then
    if [ -r "${PKG}-${VER}.tar.gz" ]
    then
        echo "${PKG}-${VER} will be installed from local tarball."
    else
        echo "${PKG}-${VER} will be downloaded and installed."
    fi
  else
    echo "${PKG} is already installed and the version is ok."
  fi
}

fetch_pkg () {
  PKG=$1
  VER=$2

  URL=${HACKAGE_URL}/${PKG}-${VER}/${PKG}-${VER}.tar.gz
  if which ${CURL} > /dev/null
  then
    # TODO: switch back to resuming curl command once
    #       https://github.com/haskell/hackage-server/issues/111 is resolved
    #${CURL} -L --fail -C - -O ${URL} || die "Failed to download ${PKG}."
    ${CURL} -L --fail -O ${URL} || die "Failed to download ${PKG}."
  elif which ${WGET} > /dev/null
  then
    ${WGET} -c ${URL} || die "Failed to download ${PKG}."
  elif which ${FETCH} > /dev/null
    then
      ${FETCH} ${URL} || die "Failed to download ${PKG}."
  else
    die "Failed to find a downloader. 'curl', 'wget' or 'fetch' is required."
  fi
  [ -f "${PKG}-${VER}.tar.gz" ] ||
     die "Downloading ${URL} did not create ${PKG}-${VER}.tar.gz"
}

unpack_pkg () {
  PKG=$1
  VER=$2

  rm -rf "${PKG}-${VER}.tar" "${PKG}-${VER}"
  ${GZIP_PROGRAM} -d < "${PKG}-${VER}.tar.gz" | ${TAR} -xf -
  [ -d "${PKG}-${VER}" ] || die "Failed to unpack ${PKG}-${VER}.tar.gz"
}


# the constraints variable is used to try to limit calling the Setup
# configure with just the correct versions of the dependency
# packages. This is to avoid situations where Cabal doesn't pick the
# version of a dependency we want which can happen since we install
# the packages one by one. This is only relevant when you run
# bootstrap on a installation of ghc with extra packages in the global
# and/or packages in the user package database. I'm not 100% sure this
# is still needed since we compile any needed packages in a separate
# package database.

CONSTRAINTS=""

# use -j when running Setup.hs files when we have a new enough Cabal
CABAL_JOBS=""

[ -n "$(${GHC_PKG} list --global $GHC_PKG_PACKAGE_ARGS | egrep 'Cabal-1\.18|Cabal-1\.[2-9]|Cabal-[2-9]')" ] && CABAL_JOBS="-j"

install_pkg () {
  PKG=$1
  VER=$2

  [ -x Setup ] && ./Setup clean
  [ -f Setup ] && rm Setup

  [ -n "$VERBOSE" ] && echo ${GHC} --make Setup -o Setup ${GHC_PACKAGE_ARGS} $GHC_JOBS
  ${GHC} --make Setup -o Setup ${GHC_PACKAGE_ARGS} $GHC_JOBS ||
    die "Compiling the Setup script failed."

  [ -x Setup ] || die "The Setup script does not exist or cannot be run"

  args="${CABAL_PACKAGE_ARGS} --prefix=${SANDBOX} --with-compiler=${GHC}"
  args="$args --with-hc-pkg=${GHC_PKG} --with-gcc=${CC} --with-ld=${LD}"
  args="$args --disable-library-profiling --disable-shared"
  args="$args --disable-split-objs --enable-executable-stripping"
  args="$args --disable-tests ${VERBOSE} $CONSTRAINTS"

  [ -n "$VERBOSE" ] && echo ${GHC} ./Setup configure $args
  ./Setup configure $args || die "Configuring the ${PKG} package failed."

  [ -n "$VERBOSE" ] && echo ./Setup build $CABAL_JOBS ${VERBOSE}
  ./Setup build $CABAL_JOBS ${VERBOSE} ||
     die "Building the ${PKG} package failed."

  [ -n "$VERBOSE" ] && echo ./Setup install ${VERBOSE}
  ./Setup install ${VERBOSE} ||
     die "Installing the ${PKG} package failed."
}

do_pkg () {
  PKG=$1
  VER=$2

  if need_pkg ${PKG} ${VER}
  then
    if [ -r "${PKG}-${VER}.tar.gz" ]
    then
        echo "Using local tarball for ${PKG}-${VER}."
    else
        echo "Downloading ${PKG}-${VER}..."
        fetch_pkg ${PKG} ${VER}
    fi
    unpack_pkg ${PKG} ${VER}
    cd "${PKG}-${VER}"
    install_pkg ${PKG} ${VER}
    cd ..
  fi
}

############################
# Actually do something!

if [ -n "$(echo ${GHC_VER} | egrep '^7\.10\.')" ]
then
    DEPENDENCY_FILE=bootstrap-ghc-7.10.1-deps
elif [ -n "$(echo ${GHC_VER} | egrep '^7\.8\.')" ]
then
    DEPENDENCY_FILE=bootstrap-ghc-7.8.1-deps
elif [ -n "$(echo ${GHC_VER} | egrep '^7\.6\.')" ]
then
    DEPENDENCY_FILE=bootstrap-ghc-7.6.1-deps
elif [ -n "$(echo ${GHC_VER} | egrep '^7\.4\.')" ]
then
    DEPENDENCY_FILE=bootstrap-ghc-7.4.1-deps
elif [ -n "$(echo ${GHC_VER} | egrep '^7\.2\.')" ]
then
    DEPENDENCY_FILE=bootstrap-ghc-7.2.1-deps
else
    echo "Warning: unsupported version of GHC $GHC_VER, using latest GHC deps"
    DEPENDENCY_FILE=bootstrap-ghc-7.8.1-deps
fi

echo Using dependencies from file $DEPENDENCY_FILE

while read dep; do
    package_name=${dep%%" "*}
    package_version=${dep#*" "}
    info_pkg $package_name $package_version
done <$DEPENDENCY_FILE

while read dep; do
    package_name=${dep%%" "*}
    package_version=${dep#*" "}
    do_pkg $package_name $package_version
    CONSTRAINTS="$CONSTRAINTS --constraint=$package_name==$package_version"
done <$DEPENDENCY_FILE

install_pkg "cabal-install"

# Use the newly built cabal to turn the prefix/package database into a
# legit cabal sandbox. This works because 'cabal sandbox init' will
# reuse the already existing package database and other files if they
# are in the expected locations. This isn't really useful for
# bootstrapping except for debugging
$SANDBOX/bin/cabal sandbox init --sandbox $SANDBOX

echo
echo "==========================================="
CABAL_BIN="$SANDBOX/bin"
if [ -x "$CABAL_BIN/cabal" ]
then
    echo "The 'cabal' program has been installed in $CABAL_BIN/"
    echo "You should either add $CABAL_BIN to your PATH"
    echo "or copy the cabal program to a directory that is on your PATH."
    echo
    echo "The first thing to do is to get the latest list of packages with:"
    echo "  cabal update"
    echo "This will also create a default config file (if it does not already"
    echo "exist) at $HOME/.cabal/config"
    echo
    echo "By default cabal will install programs to $HOME/.cabal/bin"
    echo "If you do not want to add this directory to your PATH then you can"
    echo "change the setting in the config file, for example you could use:"
    echo "symlink-bindir: $HOME/bin"
else
    echo "Sorry, something went wrong."
    echo "The 'cabal' executable was not successfully installed into"
    echo "$CABAL_BIN/"
fi
echo

rm ghc-pkg.list
