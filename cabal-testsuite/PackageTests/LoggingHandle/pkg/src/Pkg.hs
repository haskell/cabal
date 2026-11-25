module Pkg (pkg) where

import Dep (dep)
import HDep (hdep)

pkg :: Int
pkg = dep + hdep
