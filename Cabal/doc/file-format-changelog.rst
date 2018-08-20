Cabal file format changelog
===========================

Changes in 2.4
--------------

* Wildcard matching has been expanded. All previous wildcard
  expressions are still valid; some will match strictly more files
  than before. Specifically:

  * Double-star (``**``) wildcards are now accepted for recursive
    matching immediately before the final slash; they must be followed
    by a filename wildcard (e.g., ``foo/**/*.html`` is valid;
    ``foo/**/bar/*.html`` and ``foo/**/**/*.html``,
    ``foo/**/bar.html`` are all invalid). As ``**`` was an error in
    globs before, this does not affect any existing ``.cabal`` files
    that previously worked.

  * Wildcards now match when the pattern's extensions form a suffix of
    the candidate file's extension, rather than requiring strict
    equality (e.g., previously ``*.html`` did not match
    ``foo.en.html``, but now it does).

* License fields use identifiers from SPDX License List version
  ``3.2 2018-07-10``
