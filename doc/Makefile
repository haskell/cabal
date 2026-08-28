# Build and safety-check requirements.txt

# Remove a target if its recipe fails, so a failed `cabal run` can't leave a
# truncated *-help/*.txt behind for `git diff` to find.
.DELETE_ON_ERROR:

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
# Flag -n ("nitpick") warns about broken references
# Flag -W turns warnings into errors
# Flag --keep-going continues after errors
SPHINX_FLAGS:=-n -W --keep-going -E
SPHINX_HTML_OUTDIR:=../dist-newstyle/doc/users-guide
USERGUIDE_STAMP:=$(SPHINX_HTML_OUTDIR)/index.html

# Users guide
##############################################################################

users-guide: $(USERGUIDE_STAMP)
$(USERGUIDE_STAMP) : *.rst
	mkdir -p $(SPHINX_HTML_OUTDIR) \
	&& uv sync \
	&& uv run sphinx-build $(SPHINX_FLAGS) . $(SPHINX_HTML_OUTDIR)

# Requirements
##############################################################################

requirements.txt:
	uv export --frozen --format requirements.txt > requirements.txt

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
	uvx skjold -c pyproject.toml config \
	&& uv pip list --format freeze | uvx skjold -c pyproject.toml audit

# Debug print environment variables
debug:
	@echo "GITHUB_TOKEN = ${GITHUB_TOKEN}"
	@echo "SKJOLD_GITHUB_API_TOKEN = $${SKJOLD_GITHUB_API_TOKEN}"
	@echo "Is SKJOLD_GITHUB_API_TOKEN set? $${SKJOLD_GITHUB_API_TOKEN:+yes}"

CABAL_V1_COMMANDS := \
	bench \
	build \
	clean \
	configure \
	copy \
	freeze \
	gen-bounds \
	haddock \
	install \
	reconfigure \
	register \
	repl \
	run \
	test

CABAL_COMMANDS := \
	bench \
	build \
	clean \
	configure \
	exec \
	fetch \
	freeze \
	gen-bounds \
	get \
	haddock \
	haddock-project \
	hscolour \
	info \
	init \
	install \
	list \
	list-bin \
	outdated \
	path \
	repl \
	report \
	run \
	sdist \
	target \
	test \
	unpack \
	update \
	upload \
	user-config

CABAL_HIDDEN_COMMANDS := \
	act-as-setup \
	format \
	man

# Distribution.Simple.defaultMain's commands, for build-type: Simple.
SETUP_COMMANDS := \
	bench \
	build \
	clean \
	configure \
	copy \
	haddock \
	hscolour \
	install \
	register \
	repl \
	sdist \
	test \
	unregister

# NOTE: To silence the following warning, redirect stderr to /dev/null:
#   Warning: this is a debug build of cabal-install with assertions enabled.
cmd-v1-help/%.txt:
	@mkdir -p $(@D)
	@cabal run -v0 cabal-install:exe:cabal -- v1-$* --help > $@ 2>/dev/null

cmd-v2-help/%.txt:
	@mkdir -p $(@D)
	@cabal run -v0 cabal-install:exe:cabal -- $* --help > $@ 2>/dev/null

# The "You can edit the cabal configuration file..." footer embeds an
# absolute, machine-specific config path, so strip it for reproducibility.
cmd-v2-help/help.txt:
	@mkdir -p $(@D)
	@cabal run -v0 cabal-install:exe:cabal -- --help 2>/dev/null \
	  | sed '/^You can edit the cabal configuration file to set defaults:$$/,$$d' > $@

cmd-v2-hidden-help/%.txt:
	@mkdir -p $(@D)
	@cabal run -v0 cabal-install:exe:cabal -- $* --help > $@ 2>/dev/null

# act-as-setup runs Distribution.Simple.defaultMainArgs directly; the "--"
# forwards args after it verbatim instead of being parsed by cabal itself.
setup-help/%.txt:
	@mkdir -p $(@D)
	@cabal run -v0 cabal-install:exe:cabal -- act-as-setup -- $* --help > $@ 2>/dev/null

setup-help/help.txt:
	@mkdir -p $(@D)
	@cabal run -v0 cabal-install:exe:cabal -- act-as-setup -- --help > $@ 2>/dev/null

.PHONY: cmd-v1-help
cmd-v1-help: $(addsuffix .txt,$(addprefix cmd-v1-help/,$(CABAL_V1_COMMANDS)))

.PHONY: cmd-v2-help
cmd-v2-help: cmd-v2-help/help.txt $(addsuffix .txt,$(addprefix cmd-v2-help/,$(CABAL_COMMANDS)))

.PHONY: cmd-v2-hidden-help
cmd-v2-hidden-help: $(addsuffix .txt,$(addprefix cmd-v2-hidden-help/,$(CABAL_HIDDEN_COMMANDS)))

.PHONY: setup-help
setup-help: setup-help/help.txt $(addsuffix .txt,$(addprefix setup-help/,$(SETUP_COMMANDS)))

.PHONY: cmd-help
cmd-help: cmd-v2-help cmd-v1-help cmd-v2-hidden-help setup-help

# EOF
