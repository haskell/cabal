#!/usr/bin/env sh

# A script to bootstrap cabal-install.

# It works by downloading and installing the Cabal, zlib and
# HTTP packages. It then installs cabal-install itself.
# It expects to be run inside the cabal-install directory.

# Install settings, you can override these by setting environment vars. E.g. if
# you don't want profiling and dynamic versions of libraries to be installed in
# addition to vanilla, run 'EXTRA_CONFIGURE_OPTS="" ./bootstrap.sh'

#VERBOSE
DEFAULT_CONFIGURE_OPTS="--enable-library-profiling --enable-shared"
EXTRA_CONFIGURE_OPTS=${EXTRA_CONFIGURE_OPTS-$DEFAULT_CONFIGURE_OPTS}
#EXTRA_BUILD_OPTS
#EXTRA_INSTALL_OPTS

die () { printf "\nError during cabal-install bootstrap:\n$1\n" >&2 && exit 2 ;}

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

# The variable SCOPE_OF_INSTALLATION can be set on the command line to
# use/install the libaries needed to build cabal-install to a custom package
# database instead of the user or global package database.
#
# Example:
#
# $ ghc-pkg init /my/package/database
# $ SCOPE_OF_INSTALLATION='--package-db=/my/package/database' ./bootstrap.sh
#
# You can also combine SCOPE_OF_INSTALLATION with PREFIX:
#
# $ ghc-pkg init /my/prefix/packages.conf.d
# $ SCOPE_OF_INSTALLATION='--package-db=/my/prefix/packages.conf.d' \
#   PREFIX=/my/prefix ./bootstrap.sh
#
# If you use the --global,--user or --sandbox arguments, this will
# override the SCOPE_OF_INSTALLATION setting and not use the package
# database you pass in the SCOPE_OF_INSTALLATION variable.

SCOPE_OF_INSTALLATION="${SCOPE_OF_INSTALLATION:---user}"
DEFAULT_PREFIX="${HOME}/.cabal"

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
  echo 'main;' | ${CC} -v -x c - -o /dev/null -\#\#\# 2>&1 | grep -qw $link &&
  echo 'main;' | ${CC} -v -x c - -o /dev/null -\#\#\# 2>&1 | grep -w  $link |
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

[ ${GHC_VER} = ${GHC_PKG_VER} ] ||
  die "Version mismatch between ${GHC} and ${GHC_PKG}.
       If you set the GHC variable then set GHC_PKG too."

while [ "$#" -gt 0 ]; do
  case "${1}" in
    "--user")
      SCOPE_OF_INSTALLATION="${1}"
      shift;;
    "--global")
      SCOPE_OF_INSTALLATION="${1}"
      DEFAULT_PREFIX="/usr/local"
      shift;;
    "--sandbox")
      shift
      # check if there is another argument which doesn't start with --
      if [ "$#" -le 0 ] || [ ! -z $(echo "${1}" | grep "^--") ]
      then
          SANDBOX=".cabal-sandbox"
      else
          SANDBOX="${1}"
          shift
      fi;;
    "--no-doc")
      NO_DOCUMENTATION=1
      shift;;
    *)
      echo "Unknown argument or option, quitting: ${1}"
      echo "usage: bootstrap.sh [OPTION]"
      echo
      echo "options:"
      echo "   --user          Install for the local user (default)"
      echo "   --global        Install systemwide (must be run as root)"
      echo "   --no-doc        Do not generate documentation for installed "\
           "packages"
      echo "   --sandbox       Install to a sandbox in the default location"\
           "(.cabal-sandbox)"
      echo "   --sandbox path  Install to a sandbox located at path"
      exit;;
  esac
done

abspath () { case "$1" in /*)printf "%s\n" "$1";; *)printf "%s\n" "$PWD/$1";;
             esac; }

if [ ! -z "$SANDBOX" ]
then # set up variables for sandbox bootstrap
  # Make the sandbox path absolute since it will be used from
  # different working directories when the dependency packages are
  # installed.
  SANDBOX=$(abspath "$SANDBOX")
  # Get the name of the package database which cabal sandbox would use.
  GHC_ARCH=$(ghc --info |
    sed -n 's/.*"Target platform".*"\([^-]\+\)-[^-]\+-\([^"]\+\)".*/\1-\2/p')
  PACKAGEDB="$SANDBOX/${GHC_ARCH}-ghc-${GHC_VER}-packages.conf.d"
  # Assume that if the directory is already there, it is already a
  # package database. We will get an error immediately below if it
  # isn't. Uses -r to try to be compatible with Solaris, and allow
  # symlinks as well as a normal dir/file.
  [ ! -r "$PACKAGEDB" ] && ghc-pkg init "$PACKAGEDB"
  PREFIX="$SANDBOX"
  SCOPE_OF_INSTALLATION="--package-db=$PACKAGEDB"
  echo Bootstrapping in sandbox at \'$SANDBOX\'.
fi

# Check for haddock unless no documentation should be generated.
if [ ! ${NO_DOCUMENTATION} ]
then
  ${HADDOCK} --version     > /dev/null 2>&1  || die "${HADDOCK} not found."
fi

PREFIX=${PREFIX:-${DEFAULT_PREFIX}}

# Versions of the packages to install.
# The version regex says what existing installed versions are ok.
PARSEC_VER="3.1.7";    PARSEC_VER_REGEXP="[3]\.[01]\."
                       # >= 3.0 && < 3.2
DEEPSEQ_VER="1.4.0.0"; DEEPSEQ_VER_REGEXP="1\.[1-9]\."
                       # >= 1.1 && < 2
BINARY_VER="0.7.2.3";  BINARY_VER_REGEXP="[0]\.[7]\."
                       # == 0.7.*
TEXT_VER="1.2.0.3";    TEXT_VER_REGEXP="((1\.[012]\.)|(0\.([2-9]|(1[0-1]))\.))"
                       # >= 0.2 && < 1.3
NETWORK_VER="2.6.0.2"; NETWORK_VER_REGEXP="2\.[0-6]\."
                       # >= 2.0 && < 2.7
NETWORK_URI_VER="2.6.0.1"; NETWORK_URI_VER_REGEXP="2\.6\."
                       # >= 2.6 && < 2.7
CABAL_VER="1.22.4.0";  CABAL_VER_REGEXP="1\.22"
                       # >= 1.22 && < 1.23
TRANS_VER="0.4.2.0";   TRANS_VER_REGEXP="0\.[4]\."
                       # >= 0.2.* && < 0.5
MTL_VER="2.2.1";       MTL_VER_REGEXP="[2]\."
                       #  >= 2.0 && < 3
HTTP_VER="4000.2.19";  HTTP_VER_REGEXP="4000\.2\.([5-9]|1[0-9]|2[0-9])"
                       # >= 4000.2.5 < 4000.3
ZLIB_VER="0.5.4.2";    ZLIB_VER_REGEXP="0\.[45]\."
                       # == 0.4.* || == 0.5.*
TIME_VER="1.5"         TIME_VER_REGEXP="1\.[12345]\.?"
                       # >= 1.1 && < 1.6
RANDOM_VER="1.1"       RANDOM_VER_REGEXP="1\.[01]\.?"
                       # >= 1 && < 1.2
STM_VER="2.4.4";       STM_VER_REGEXP="2\."
                       # == 2.*
OLD_TIME_VER="1.1.0.3"; OLD_TIME_VER_REGEXP="1\.[01]\.?"
                       # >=1.0.0.0 && <1.2
OLD_LOCALE_VER="1.0.0.7"; OLD_LOCALE_VER_REGEXP="1\.0\.?"
                       # >=1.0.0.0 && <1.1

HACKAGE_URL="https://hackage.haskell.org/package"

# Haddock fails for network-2.5.0.0.
NO_DOCS_PACKAGES_VER_REGEXP="network-uri-2\.5\.[0-9]+\.[0-9]+"

# Cache the list of packages:
echo "Checking installed packages for ghc-${GHC_VER}..."
${GHC_PKG} list --global ${SCOPE_OF_INSTALLATION} > ghc-pkg.list ||
  die "running '${GHC_PKG} list' failed"

# Will we need to install this package, or is a suitable version installed?
need_pkg () {
  PKG=$1
  VER_MATCH=$2
  if egrep " ${PKG}-${VER_MATCH}" ghc-pkg.list > /dev/null 2>&1
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
  VER_MATCH=$3

  if need_pkg ${PKG} ${VER_MATCH}
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

install_pkg () {
  PKG=$1
  VER=$2

  [ -x Setup ] && ./Setup clean
  [ -f Setup ] && rm Setup

  ${GHC} --make Setup -o Setup ||
    die "Compiling the Setup script failed."

  [ -x Setup ] || die "The Setup script does not exist or cannot be run"

  args="${SCOPE_OF_INSTALLATION} --prefix=${PREFIX} --with-compiler=${GHC}"
  args="$args --with-hc-pkg=${GHC_PKG} --with-gcc=${CC} --with-ld=${LD}"
  args="$args ${EXTRA_CONFIGURE_OPTS} ${VERBOSE}"

  ./Setup configure $args || die "Configuring the ${PKG} package failed."

  ./Setup build ${EXTRA_BUILD_OPTS} ${VERBOSE} ||
     die "Building the ${PKG} package failed."

  if [ ! ${NO_DOCUMENTATION} ]
  then
    if echo "${PKG}-${VER}" | egrep ${NO_DOCS_PACKAGES_VER_REGEXP} > /dev/null 2>&1
    then
      echo "Skipping documentation for the ${PKG} package."
    else
      ./Setup haddock --with-ghc=${GHC} --with-haddock=${HADDOCK} ${VERBOSE} ||
        die "Documenting the ${PKG} package failed."
    fi
  fi

  ./Setup install ${EXTRA_INSTALL_OPTS} ${VERBOSE} ||
     die "Installing the ${PKG} package failed."
}

do_pkg () {
  PKG=$1
  VER=$2
  VER_MATCH=$3

  if need_pkg ${PKG} ${VER_MATCH}
  then
    echo
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

# Replicate the flag selection logic for network-uri in the .cabal file.
do_network_uri_pkg () {
  # Refresh installed package list.
  ${GHC_PKG} list --global ${SCOPE_OF_INSTALLATION} > ghc-pkg-stage2.list \
    || die "running '${GHC_PKG} list' failed"

  NETWORK_URI_DUMMY_VER="2.5.0.0"; NETWORK_URI_DUMMY_VER_REGEXP="2\.5\." # < 2.6
  if egrep " network-2\.[6-9]\." ghc-pkg-stage2.list > /dev/null 2>&1
  then
    # Use network >= 2.6 && network-uri >= 2.6
    info_pkg "network-uri" ${NETWORK_URI_VER} ${NETWORK_URI_VER_REGEXP}
    do_pkg   "network-uri" ${NETWORK_URI_VER} ${NETWORK_URI_VER_REGEXP}
  else
    # Use network < 2.6 && network-uri < 2.6
    info_pkg "network-uri" ${NETWORK_URI_DUMMY_VER} ${NETWORK_URI_DUMMY_VER_REGEXP}
    do_pkg   "network-uri" ${NETWORK_URI_DUMMY_VER} ${NETWORK_URI_DUMMY_VER_REGEXP}
  fi
}

# Actually do something!

info_pkg "deepseq"      ${DEEPSEQ_VER} ${DEEPSEQ_VER_REGEXP}
info_pkg "binary"       ${BINARY_VER}  ${BINARY_VER_REGEXP}
info_pkg "time"         ${TIME_VER}    ${TIME_VER_REGEXP}
info_pkg "Cabal"        ${CABAL_VER}   ${CABAL_VER_REGEXP}
info_pkg "transformers" ${TRANS_VER}   ${TRANS_VER_REGEXP}
info_pkg "mtl"          ${MTL_VER}     ${MTL_VER_REGEXP}
info_pkg "text"         ${TEXT_VER}    ${TEXT_VER_REGEXP}
info_pkg "parsec"       ${PARSEC_VER}  ${PARSEC_VER_REGEXP}
info_pkg "network"      ${NETWORK_VER} ${NETWORK_VER_REGEXP}
info_pkg "old-locale"   ${OLD_LOCALE_VER} ${OLD_LOCALE_VER_REGEXP}
info_pkg "old-time"     ${OLD_TIME_VER} ${OLD_TIME_VER_REGEXP}
info_pkg "HTTP"         ${HTTP_VER}    ${HTTP_VER_REGEXP}
info_pkg "zlib"         ${ZLIB_VER}    ${ZLIB_VER_REGEXP}
info_pkg "random"       ${RANDOM_VER}  ${RANDOM_VER_REGEXP}
info_pkg "stm"          ${STM_VER}     ${STM_VER_REGEXP}

do_pkg   "deepseq"      ${DEEPSEQ_VER} ${DEEPSEQ_VER_REGEXP}
do_pkg   "binary"       ${BINARY_VER}  ${BINARY_VER_REGEXP}
do_pkg   "time"         ${TIME_VER}    ${TIME_VER_REGEXP}
do_pkg   "Cabal"        ${CABAL_VER}   ${CABAL_VER_REGEXP}
do_pkg   "transformers" ${TRANS_VER}   ${TRANS_VER_REGEXP}
do_pkg   "mtl"          ${MTL_VER}     ${MTL_VER_REGEXP}
do_pkg   "text"         ${TEXT_VER}    ${TEXT_VER_REGEXP}
do_pkg   "parsec"       ${PARSEC_VER}  ${PARSEC_VER_REGEXP}
do_pkg   "network"      ${NETWORK_VER} ${NETWORK_VER_REGEXP}

# We conditionally install network-uri, depending on the network version.
do_network_uri_pkg

do_pkg   "old-locale"   ${OLD_LOCALE_VER} ${OLD_LOCALE_VER_REGEXP}
do_pkg   "old-time"     ${OLD_TIME_VER} ${OLD_TIME_VER_REGEXP}
do_pkg   "HTTP"         ${HTTP_VER}    ${HTTP_VER_REGEXP}
do_pkg   "zlib"         ${ZLIB_VER}    ${ZLIB_VER_REGEXP}
do_pkg   "random"       ${RANDOM_VER}  ${RANDOM_VER_REGEXP}
do_pkg   "stm"          ${STM_VER}     ${STM_VER_REGEXP}

install_pkg "cabal-install"

# Use the newly built cabal to turn the prefix/package database into a
# legit cabal sandbox. This works because 'cabal sandbox init' will
# reuse the already existing package database and other files if they
# are in the expected locations.
[ ! -z "$SANDBOX" ] && $SANDBOX/bin/cabal sandbox init --sandbox $SANDBOX

echo
echo "==========================================="
CABAL_BIN="$PREFIX/bin"
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
