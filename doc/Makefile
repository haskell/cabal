# Build and safety-check requirements.txt

# skjold needs a personal github access token.  This needs no permissions,
# it is only required to query the GitHub GraphQL API v4.
# See: https://pythonawesome.com/security-audit-python-project-dependencies-against-security-advisory-databases/
# We attempt to get it from the environment variable SKJOLD_GITHUB_API_TOKEN
# or GITHUB_TOKEN.
# It can also be passed to this Makefile via either:
#
#   make GITHUB_TOKEN=... (build-and-)check-requirements
#   make SKJOLD_GITHUB_API_TOKEN=... (build-and-)check-requirements
#
#
SKJOLD_GITHUB_API_TOKEN ?= ${GITHUB_TOKEN}
# TODO: when we have sphinx-build2 ?
SPHINXCMD:=sphinx-build
# Flag -n ("nitpick") warns about broken references
# Flag -W turns warnings into errors
# Flag --keep-going continues after errors
SPHINX_FLAGS:=-n -W --keep-going -E
SPHINX_HTML_OUTDIR:=../dist-newstyle/doc/users-guide
USERGUIDE_STAMP:=$(SPHINX_HTML_OUTDIR)/index.html
PYTHON_VIRTUALENV_ACTIVATE:=../.python-sphinx-virtualenv/bin/activate

# Python virtual environment
##############################################################################

# Create a python virtual environment in the root of the cabal repository.
$(PYTHON_VIRTUALENV_ACTIVATE):
	python3 -m venv ../.python-sphinx-virtualenv
	(. $(PYTHON_VIRTUALENV_ACTIVATE))

# Users guide
##############################################################################

# do pip install every time so we have up to date requirements when we build
users-guide: $(PYTHON_VIRTUALENV_ACTIVATE) $(USERGUIDE_STAMP)
$(USERGUIDE_STAMP) : *.rst
	mkdir -p $(SPHINX_HTML_OUTDIR)
	(. $(PYTHON_VIRTUALENV_ACTIVATE) && pip install -r requirements.txt && $(SPHINXCMD) $(SPHINX_FLAGS) . $(SPHINX_HTML_OUTDIR))

# Requirements
##############################################################################

##
# This goal is intended for manual invocation, always rebuilds.
.PHONY: users-guide-requirements
users-guide-requirements: requirements.txt

.PHONY: build-and-check-requirements
build-and-check-requirements: requirements.txt check-requirements

# Always rebuild requirements.txt
.PHONY: requirements.txt
# requirements.txt is generated from requirements.in
# via pip-compile included in the pip-tools package.
# See https://modelpredict.com/wht-requirements-txt-is-not-enough
requirements.txt: requirements.in $(PYTHON_VIRTUALENV_ACTIVATE)
	. $(PYTHON_VIRTUALENV_ACTIVATE) \
	  && pip install --upgrade pip \
	  && pip install pip-tools \
	  && pip-compile requirements.in

# Check requirements.txt for security violations via skjold,
# configured in pyproject.toml.
# See: https://pythonawesome.com/security-audit-python-project-dependencies-against-security-advisory-databases/
.PHONY: check-requirements
check-requirements:
	@if [ -z "$${SKJOLD_GITHUB_API_TOKEN}" ] \
	; then \
	  echo "WARNING: Neither SKJOLD_GITHUB_API_TOKEN nor GITHUB_TOKEN is set." \
	; echo "Vulnerability check via skjold might fail when using the GitHub GraphQL API." \
	; fi
	. $(PYTHON_VIRTUALENV_ACTIVATE) \
	  && pip install skjold \
	  && skjold audit
# NB: For portability, we use '.' (sh etc.) instead of 'source' (bash).

# Debug print environment variables
debug:
	@echo "GITHUB_TOKEN = ${GITHUB_TOKEN}"
	@echo "SKJOLD_GITHUB_API_TOKEN = $${SKJOLD_GITHUB_API_TOKEN}"
	@echo "Is SKJOLD_GITHUB_API_TOKEN set? $${SKJOLD_GITHUB_API_TOKEN:+yes}"

# EOF
