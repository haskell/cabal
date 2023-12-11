# Version Priority Tests

The `1-` prefix projects have an import depth of 1, the `2-` prefix projects
have a depth of 2 and `3-` prefix has depth 3. The `0-` prefix project have any
imports.  Only projects have the `.project` extension. Imported configuration
has a `.config` extension.

- *0-local.project*
    ```
    .
    └── 0-local.project
    ```

- *1-local.project*
    ```
    .
    └── 1-local.project
        └── stackage-local.config
    ```

- *2-local.project*
    ```
    .
    └── 2-local.project
        └── hop-local.config
            └── stackage-local.config
    ```

- *1-web.project*
    ```
    .
    └── 1-web.project
        └── https://www.stackage.org/nightly-2023-12-07/cabal.config
    ```

- *2-web.project*
    ```
    .
    └── 2-web.project
        └── stackage-web.config
            └── https://www.stackage.org/nightly-2023-12-07/cabal.config
    ```

- *3-web.project*
    ```
    .
    └── 3-web.project
        └── hop-web.config
            └── stackage-web.config
                └── https://www.stackage.org/nightly-2023-12-07/cabal.config
    ```
