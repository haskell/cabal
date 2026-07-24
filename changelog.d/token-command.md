synopsis: Add option to cabal upload to run a command to get Hackage token
packages: cabal-install
significance: significant
prs: #12132
description:{
Introduce `--token-command` option to `cabal upload`, to run a command to get a Hackage authentication token rather than providing it in plaintext.

This allows the use of a secrets manager to store Hackage tokens, like `--password-command`.

The recommended way is to add the following to your cabal config file:

```
token-command: sh -c "..."
```
}
