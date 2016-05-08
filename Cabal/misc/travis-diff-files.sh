#!/bin/sh
git status > /dev/null # See 09a71929e433f36b27fd6a4938469d3bbbd5e191
git diff-files -p --exit-code
