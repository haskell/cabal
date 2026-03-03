-- |
-- Module:     Foo
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE CPP #-}

module Foo (foo) where

import Foo01
import Foo02
import Foo03
import Foo04
import Foo05
import Foo06
import Foo07
import Foo08
import Foo09
import Foo10

#include <include_Foo01.h>
#include <include_Foo02.h>
#include <include_Foo03.h>
#include <include_Foo04.h>
#include <include_Foo05.h>
#include <include_Foo06.h>
#include <include_Foo07.h>
#include <include_Foo08.h>
#include <include_Foo09.h>
#include <include_Foo10.h>
#include <include_FooDep01.h>
#include <include_FooDep02.h>
#include <include_FooDep03.h>
#include <include_FooDep04.h>
#include <include_FooDep05.h>
#include <include_FooDep06.h>
#include <include_FooDep07.h>
#include <include_FooDep08.h>
#include <include_FooDep09.h>
#include <include_FooDep10.h>
#include <include_FooDepDep01.h>
#include <include_FooDepDep02.h>
#include <include_FooDepDep03.h>
#include <include_FooDepDep04.h>
#include <include_FooDepDep05.h>
#include <include_FooDepDep06.h>
#include <include_FooDepDep07.h>
#include <include_FooDepDep08.h>
#include <include_FooDepDep09.h>
#include <include_FooDepDep10.h>

foo :: Int
foo = sum
  [ #{const TEST_OPTION}
  , foo01
  , foo02
  , foo03
  , foo04
  , foo05
  , foo06
  , foo07
  , foo08
  , foo09
  , foo10

  , #{const DEF_foo01}
  , #{const DEF_foo02}
  , #{const DEF_foo03}
  , #{const DEF_foo04}
  , #{const DEF_foo05}
  , #{const DEF_foo06}
  , #{const DEF_foo07}
  , #{const DEF_foo08}
  , #{const DEF_foo09}
  , #{const DEF_foo10}

  , #{const DEF_fooDep01}
  , #{const DEF_fooDep02}
  , #{const DEF_fooDep03}
  , #{const DEF_fooDep04}
  , #{const DEF_fooDep05}
  , #{const DEF_fooDep06}
  , #{const DEF_fooDep07}
  , #{const DEF_fooDep08}
  , #{const DEF_fooDep09}
  , #{const DEF_fooDep10}

  , #{const DEF_fooDepDep01}
  , #{const DEF_fooDepDep02}
  , #{const DEF_fooDepDep03}
  , #{const DEF_fooDepDep04}
  , #{const DEF_fooDepDep05}
  , #{const DEF_fooDepDep06}
  , #{const DEF_fooDepDep07}
  , #{const DEF_fooDepDep08}
  , #{const DEF_fooDepDep09}
  , #{const DEF_fooDepDep10}
  ]
