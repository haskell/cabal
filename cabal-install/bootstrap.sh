#!/bin/sh

# A script to bootstrap cabal-install.

# It works by downloading and installing the Cabal, zlib and
# HTTP packages. It then installs cabal-install itself.
# It expects to be run inside the cabal-install directory.

# install settings, you can override these by setting environment vars
#VERBOSE
#EXTRA_CONFIGURE_OPTS
#EXTRA_BUILD_OPTS
#EXTRA_INSTALL_OPTS

# programs, you can override these by setting environment vars
GHC=${GHC:-ghc}
GHC_PKG=${GHC_PKG:-ghc-pkg}
WGET=${WGET:-wget}
CURL=${CURL:-curl}
FETCH=${FETCH:-fetch}
TAR=${TAR:-tar}
GUNZIP=${GUNZIP:-gunzip}
SCOPE_OF_INSTALLATION="--user"
DEFAULT_PREFIX="${HOME}/.cabal"


for arg in $*
do
  case "${arg}" in
    "--user")
      SCOPE_OF_INSTALLATION=${arg}
      shift;;
    "--global")
      SCOPE_OF_INSTALLATION=${arg}
      DEFAULT_PREFIX="/usr/local"
      shift;;
    *)
      echo "Unknown argument or option, quitting: ${arg}"
      echo "usage: bootstrap.sh [OPTION]"
      echo
      echo "options:"
      echo "   --user    Install for the local user (default)"
      echo "   --global  Install systemwide (must be run as root)"
      exit;;
  esac
done

PREFIX=${PREFIX:-${DEFAULT_PREFIX}}

# Versions of the packages to install.
# The version regex says what existing installed versions are ok.
PARSEC_VER="3.1.3";    PARSEC_VER_REGEXP="[23]\."              # == 2.* || == 3.*
DEEPSEQ_VER="1.3.0.1"; DEEPSEQ_VER_REGEXP="1\.[1-9]\."         # >= 1.1 && < 2
TEXT_VER="0.11.3.1";   TEXT_VER_REGEXP="0\.([2-9]|(1[0-1]))\." # >= 0.2 && < 0.12
NETWORK_VER="2.4.1.2"; NETWORK_VER_REGEXP="2\."                # == 2.*
CABAL_VER="1.19.0";    CABAL_VER_REGEXP="1\.1[9]\."            # >= 1.19 && < 1.20
TRANS_VER="0.3.0.0";   TRANS_VER_REGEXP="0\.[23]\."            # >= 0.2.* && < 0.4.*
MTL_VER="2.1.2";       MTL_VER_REGEXP="[2]\."                  #  == 2.*
HTTP_VER="4000.2.8";   HTTP_VER_REGEXP="4000\.[012]\."         # == 4000.0.* || 4000.1.* || 4000.2.*
ZLIB_VER="0.5.4.1";    ZLIB_VER_REGEXP="0\.[45]\."             # == 0.4.* || == 0.5.*
TIME_VER="1.4.1"       TIME_VER_REGEXP="1\.[1234]\.?"          # >= 1.1 && < 1.5
RANDOM_VER="1.0.1.1"   RANDOM_VER_REGEXP="1\.0\."              # >= 1 && < 1.1
STM_VER="2.4.2";       STM_VER_REGEXP="2\."                    # == 2.*

HACKAGE_URL="http://hackage.haskell.org/packages/archive"

die () {
  echo
  echo "Error during cabal-install bootstrap:"
  echo $1 >&2
  exit 2
}

# Check we're in the right directory:
grep "cabal-install" ./cabal-install.cabal > /dev/null 2>&1 \
  || die "The bootstrap.sh script must be run in the cabal-install directory"

${GHC} --numeric-version > /dev/null \
  || die "${GHC} not found (or could not be run). If ghc is installed make sure it is on your PATH or set the GHC and GHC_PKG vars."
${GHC_PKG} --version     > /dev/null \
  || die "${GHC_PKG} not found."
GHC_VER=`${GHC} --numeric-version`
GHC_PKG_VER=`${GHC_PKG} --version | cut -d' ' -f 5`
[ ${GHC_VER} = ${GHC_PKG_VER} ] \
  || die "Version mismatch between ${GHC} and ${GHC_PKG} If you set the GHC variable then set GHC_PKG too"

# Cache the list of packages:
echo "Checking installed packages for ghc-${GHC_VER}..."
${GHC_PKG} list --global ${SCOPE_OF_INSTALLATION} > ghc-pkg.list \
  || die "running '${GHC_PKG} list' failed"

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
    echo "${PKG}-${VER} will be downloaded and installed."
  else
    echo "${PKG} is already installed and the version is ok."
  fi
}

fetch_pkg () {
  PKG=$1
  VER=$2

  URL=${HACKAGE_URL}/${PKG}/${VER}/${PKG}-${VER}.tar.gz
  if which ${CURL} > /dev/null
  then
    ${CURL} -L --fail -C - -O ${URL} || die "Failed to download ${PKG}."
  elif which ${WGET} > /dev/null
  then
    ${WGET} -c ${URL} || die "Failed to download ${PKG}."
  elif which ${FETCH} > /dev/null
    then
      ${FETCH} ${URL} || die "Failed to download ${PKG}."
  else
    die "Failed to find a downloader. 'curl', 'wget' or 'fetch' is required."
  fi
  [ -f "${PKG}-${VER}.tar.gz" ] \
    || die "Downloading ${URL} did not create ${PKG}-${VER}.tar.gz"
}

unpack_pkg () {
  PKG=$1
  VER=$2

  rm -rf "${PKG}-${VER}.tar" "${PKG}-${VER}"/
  ${GUNZIP} -f "${PKG}-${VER}.tar.gz" \
    || die "Failed to gunzip ${PKG}-${VER}.tar.gz"
  ${TAR} -xf "${PKG}-${VER}.tar" \
    || die "Failed to untar ${PKG}-${VER}.tar.gz"
  [ -d "${PKG}-${VER}" ] \
    || die "Unpacking ${PKG}-${VER}.tar.gz did not create ${PKG}-${VER}/"
}

install_pkg () {
  PKG=$1

  [ -x Setup ] && ./Setup clean
  [ -f Setup ] && rm Setup

  ${GHC} --make Setup -o Setup \
    || die "Compiling the Setup script failed"
  [ -x Setup ] || die "The Setup script does not exist or cannot be run"

  ./Setup configure ${SCOPE_OF_INSTALLATION} "--prefix=${PREFIX}" \
    --with-compiler=${GHC} --with-hc-pkg=${GHC_PKG} \
    ${EXTRA_CONFIGURE_OPTS} ${VERBOSE} \
    || die "Configuring the ${PKG} package failed"

  ./Setup build ${EXTRA_BUILD_OPTS} ${VERBOSE} \
    || die "Building the ${PKG} package failed"

  ./Setup install ${SCOPE_OF_INSTALLATION} ${EXTRA_INSTALL_OPTS} ${VERBOSE} \
    || die "Installing the ${PKG} package failed"
}

do_pkg () {
  PKG=$1
  VER=$2
  VER_MATCH=$3

  if need_pkg ${PKG} ${VER_MATCH}
  then
    echo
    echo "Downloading ${PKG}-${VER}..."
    fetch_pkg ${PKG} ${VER}
    unpack_pkg ${PKG} ${VER}
    cd "${PKG}-${VER}"
    install_pkg ${PKG} ${VER}
    cd ..
  fi
}

# Actually do something!

info_pkg "deepseq"      ${DEEPSEQ_VER} ${DEEPSEQ_VER_REGEXP}
info_pkg "time"         ${TIME_VER}    ${TIME_VER_REGEXP}
info_pkg "Cabal"        ${CABAL_VER}   ${CABAL_VER_REGEXP}
info_pkg "transformers" ${TRANS_VER}   ${TRANS_VER_REGEXP}
info_pkg "mtl"          ${MTL_VER}     ${MTL_VER_REGEXP}
info_pkg "text"         ${TEXT_VER}    ${TEXT_VER_REGEXP}
info_pkg "parsec"       ${PARSEC_VER}  ${PARSEC_VER_REGEXP}
info_pkg "network"      ${NETWORK_VER} ${NETWORK_VER_REGEXP}
info_pkg "HTTP"         ${HTTP_VER}    ${HTTP_VER_REGEXP}
info_pkg "zlib"         ${ZLIB_VER}    ${ZLIB_VER_REGEXP}
info_pkg "random"       ${RANDOM_VER}  ${RANDOM_VER_REGEXP}
info_pkg "stm"          ${STM_VER}     ${STM_VER_REGEXP}

do_pkg   "deepseq"      ${DEEPSEQ_VER} ${DEEPSEQ_VER_REGEXP}
do_pkg   "time"         ${TIME_VER}    ${TIME_VER_REGEXP}
do_pkg   "Cabal"        ${CABAL_VER}   ${CABAL_VER_REGEXP}
do_pkg   "transformers" ${TRANS_VER}   ${TRANS_VER_REGEXP}
do_pkg   "mtl"          ${MTL_VER}     ${MTL_VER_REGEXP}
do_pkg   "text"         ${TEXT_VER}    ${TEXT_VER_REGEXP}
do_pkg   "parsec"       ${PARSEC_VER}  ${PARSEC_VER_REGEXP}
do_pkg   "network"      ${NETWORK_VER} ${NETWORK_VER_REGEXP}
do_pkg   "HTTP"         ${HTTP_VER}    ${HTTP_VER_REGEXP}
do_pkg   "zlib"         ${ZLIB_VER}    ${ZLIB_VER_REGEXP}
do_pkg   "random"       ${RANDOM_VER}  ${RANDOM_VER_REGEXP}
do_pkg   "stm"          ${STM_VER}     ${STM_VER_REGEXP}

install_pkg "cabal-install"

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
