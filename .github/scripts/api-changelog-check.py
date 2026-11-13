import os
import re
import sys

# Checks that an API change found by packdiff is documented in a changelog
# entry added by the PR:
#   1. (blocking) some changelog file added/modified by the PR must list the
#      package in its `packages:` frontmatter field;
#   2. (warning) the entries matching the package should mention at least one
#      of the changed module/declaration names from the packdiff output.
#
# Usage: api-changelog-check.py PACKAGE API_DIFF_FILE [CHANGELOG_FILE ...]

package = sys.argv[1]
diff_path = sys.argv[2]
changelog_files = sys.argv[3:]

# ---- collect the changed names from the packdiff output ---------------------

names = set()
for line in open(diff_path, encoding="utf-8"):
    m = re.match(r"^\[[ARC]\] ([\w'.]+)", line)  # top level: a changed module
    if m:
        names.add(m.group(1))
        continue
    m = re.match(r"^\s+\[[ARCD]\] ([\w'.]+)", line)  # nested: a declaration
    if m:
        names.add(m.group(1).rsplit(".", 1)[-1])

# ---- parse the `packages:` field of each changelog file ---------------------


def front_packages(path):
    text = open(path, encoding="utf-8").read()
    # Entries are either .cabal-style key/value files or markdown files with a
    # YAML front matter; in both cases `packages:` is a top-level line.
    m = re.search(r"^packages:\s*(.+?)\s*$", text, re.M)
    if not m:
        return []
    return [p for p in re.split(r"[\s,\[\]#]+", m.group(1)) if p]


matching = [p for p in changelog_files if package in front_packages(p)]

if not matching:
    listing = "\n".join(f"  - {p}" for p in changelog_files) or "  (none)"
    print(
        f"::error::The API of {package} changed with respect to the base"
        f" revision, but none of the changelog entries added by this PR lists"
        f" {package} in its `packages:` field. See the job summary for the"
        f" diff. Add or update an entry under changelog.d/ with"
        f" `packages: [{package}]`, or revert the API change."
        f"\nChangelog files changed by this PR:\n{listing}"
    )
    sys.exit(1)

# ---- soft check: do the entries mention the changed API? --------------------

text = "\n".join(open(p, encoding="utf-8").read() for p in matching)


def mentioned(name):
    pattern = r"(?<![A-Za-z0-9_'])" + re.escape(name) + r"(?![A-Za-z0-9_'])"
    return re.search(pattern, text) is not None


missing = sorted(n for n in names if not mentioned(n))

if names and len(missing) == len(names):
    print(
        "::warning::The API of {0} changed and a changelog entry for {0} is"
        " present, but it does not mention any of the changed modules or"
        " declarations: {1}. Consider mentioning them explicitly, and check"
        " the PVP version bump and backport implications.".format(
            package, ", ".join(f"`{n}`" for n in missing[:10])
        )
    )
else:
    print(f"The changelog entry for {package} mentions the changed API.")
