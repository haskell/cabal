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

# EOF
