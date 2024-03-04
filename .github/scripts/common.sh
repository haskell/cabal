#!/bin/bash

# shellcheck disable=SC1091
. .github/scripts/env.sh

# Colors
RED="0;31"
LT_BROWN="1;33"
LT_BLUE="1;34"

ecabal() {
	cabal "$@"
}

nonfatal() {
	"$@" || "$* failed"
}

sha_sum() {
	if [ "${RUNNER_OS}" = "FreeBSD" ] ; then
		sha256 "$@"
	else
		sha256sum "$@"
	fi
}

git_describe() {
	git config --global --get-all safe.directory | grep '^\*$' || git config --global --add safe.directory "*"
	git describe --always
}

install_ghcup() {
	# find "$GHCUP_INSTALL_BASE_PREFIX"
	mkdir -p "$GHCUP_BIN"
	mkdir -p "$GHCUP_BIN"/../cache

	if [ "${RUNNER_OS}" = "FreeBSD" ] ; then
		curl -o ghcup https://downloads.haskell.org/ghcup/tmp/x86_64-portbld-freebsd-ghcup-0.1.18.1
		chmod +x ghcup
		mv ghcup "$HOME/.local/bin/ghcup"
	else
		curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_MINIMAL=1 sh
		source "$(dirname "${GHCUP_BIN}")/env"
		ghcup install cabal --set "${BOOTSTRAP_HASKELL_CABAL_VERSION}"
	fi
}

strip_binary() {
	(
	set -e
	local binary=$1
	case "$(uname -s)" in
		"Darwin"|"darwin")
			;;
		MSYS_*|MINGW*)
			;;
		*)
			strip -s "${binary}"
		   ;;
   esac
	)
}

# GitLab Pipelines log section delimiters
# https://gitlab.com/gitlab-org/gitlab-foss/issues/14664
start_section() {
  name="$1"
  echo -e "section_start:$(date +%s):$name\015\033[0K"
}

end_section() {
  name="$1"
  echo -e "section_end:$(date +%s):$name\015\033[0K"
}

echo_color() {
  local color="$1"
  local msg="$2"
  echo -e "\033[${color}m${msg}\033[0m"
}

error() { echo_color "${RED}" "$1"; }
warn() { echo_color "${LT_BROWN}" "$1"; }
info() { echo_color "${LT_BLUE}" "$1"; }

fail() { error "error: $1"; exit 1; }

run() {
  info "Running $*..."
  "$@" || ( error "$* failed"; return 1; )
}

emake() {
	if command -v gmake >/dev/null 2>&1 ; then
		gmake "$@"
	else
		make "$@"
	fi
}

mktempdir() {
	case "$(uname -s)" in
		"Darwin"|"darwin")
			mktemp -d -t cabal_ci.XXXXXXX
			;;
		*)
			mktemp -d
			;;
	esac
}
