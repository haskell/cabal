..
  VCS common fields

Most of the version control system (VCS) fields types are common to both
``source-repository`` and ``source-repository-package`` stanzas.

.. list-table::
    :header-rows: 1
    :widths: 30 30 40

    * - Field Name
      - source-repository (head|this)
      - source-repository-package
    * - type
      - [x]
      - [x]
    * - location
      - [x]
      - [x]
    * - branch
      - [x]
      - [x]
    * - tag
      - [x]
      - [x]
    * - subdir
      - [x] (0 or 1)
      - [x] (0 or 1 for each dependency)
    * - module (CVS only)
      - [x]
      - [_]
    * - post-checkout-command
      - [_]
      - [x]
