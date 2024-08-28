Please read [Github PR Conventions](https://github.com/haskell/cabal/blob/master/CONTRIBUTING.md#github-pull-request-conventions) and then fill in *one* of these two templates.

---

**Template Α: This PR modifies [behaviour or interface](https://github.com/cabalism/cabal/blob/master/CONTRIBUTING.md#changelog)**

Include the following checklist in your PR:

* [ ] Patches conform to the [coding conventions](https://github.com/haskell/cabal/blob/master/CONTRIBUTING.md#other-conventions).
* [ ] Any changes that could be relevant to users [have been recorded in the changelog](https://github.com/haskell/cabal/blob/master/CONTRIBUTING.md#changelog).
* [ ] The documentation has been updated, if necessary.
* [ ] [Manual QA notes](https://github.com/haskell/cabal/blob/master/CONTRIBUTING.md#qa-notes) have been included.
* [ ] Tests have been added. (*Ask for help if you don’t know how to write them! Ask for an exemption if tests are too complex for too little coverage!*)

---

**Template B: This PR does not modify behaviour or interface**

*E.g. the PR only touches documentation or tests, does refactorings, etc.*

Include the following checklist in your PR:

* [ ] Patches conform to the [coding conventions](https://github.com/haskell/cabal/blob/master/CONTRIBUTING.md#other-conventions).
* [ ] Is this a PR that fixes CI? If so, it will need to be backported to older cabal release branches (ask maintainers for directions).
