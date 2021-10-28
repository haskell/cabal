# Build and safety-check requirements.txt

# skjold needs a personal github access token.  This needs no permissions,
# it is only required to query the GitHub GraphQL API v4.
# See: https://pythonawesome.com/security-audit-python-project-dependencies-against-security-advisory-databases/
# We attempt to get it from the environment variable GITHUB_TOKEN.
# It can also be passed to this Makefile via either:
#
#   make GITHUB_TOKEN=... (build-and-)check-requirements
#   make SKJOLD_GITHUB_API_TOKEN=... (build-and-)check-requirements
#
#
SKJOLD_GITHUB_API_TOKEN=${GITHUB_TOKEN}

.PHONY: build-and-check-requirements
build-and-check-requirements: requirements.txt check-requirements

# Always rebuild requirements.txt
.PHONY: requirements.txt
# requirements.txt is generated from requirements.in
# via pip-compile included in the pip-tools package.
# See https://modelpredict.com/wht-requirements-txt-is-not-enough
requirements.txt: requirements.in
	. ../.python-sphinx-virtualenv/bin/activate \
	  && pip install pip-tools \
	  && pip-compile requirements.in

# Check requirements.txt for security violations via skjold,
# configured in pyproject.toml.
# See: https://pythonawesome.com/security-audit-python-project-dependencies-against-security-advisory-databases/
.PHONY: check-requirements
check-requirements:
	@if [ "\'${SKJOLD_GITHUB_API_TOKEN}\'" == "\'\'" ] \
	; then \
	  echo "WARNING: Neither SKOLD_GITHUB_API_TOKEN nor GITHUB_TOKEN is set." \
	; echo "Vulnerability check via skjold might fail when using the GitHub GraphQL API." \
	; fi
	. ../.python-sphinx-virtualenv/bin/activate \
	  && pip install skjold \
	  && skjold audit
# NB: For portability, we use '.' (sh etc.) instead of 'source' (bash).

# EOF
