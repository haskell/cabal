#!/usr/bin/env bash

set -Eeuo pipefail

# Install brew locally in the project dir. Packages will also be installed here.
# FIXME: Use brew in supported way. See
# https://docs.brew.sh/Installation#untar-anywhere-unsupported
brew_dir="${CI_PROJECT_DIR}/.brew"

if [ ! -e "${brew_dir}" ]; then
    mkdir -p "${brew_dir}"
    curl --fail-with-body -L "https://github.com/Homebrew/brew/archive/refs/tags/${BREW_VERSION}.tar.gz" | tar xz --strip 1 -C "${brew_dir}"
fi

export PATH="${brew_dir}/bin:${brew_dir}/sbin:$PATH"

# make sure to not pollute the machine with temp files etc
mkdir -p $CI_PROJECT_DIR/.brew_cache
export HOMEBREW_CACHE=$CI_PROJECT_DIR/.brew_cache
mkdir -p $CI_PROJECT_DIR/.brew_logs
export HOMEBREW_LOGS=$CI_PROJECT_DIR/.brew_logs
mkdir -p /private/tmp/.brew_tmp
export HOMEBREW_TEMP=/private/tmp/.brew_tmp

# update and install packages
brew update
brew install ${1+"$@"}
