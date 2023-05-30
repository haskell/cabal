{-# LANGUAGE CPP #-}

module UnitTests.Distribution.Solver.Types.OptionalStanza
  ( tests
  ) where

import Distribution.Solver.Types.OptionalStanza
import UnitTests.Distribution.Client.ArbitraryInstances ()

import Test.Tasty
import Test.Tasty.QuickCheck

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

tests :: [TestTree]
tests =
  [ testProperty "fromList . toList = id" $ \xs ->
      optStanzaSetFromList (optStanzaSetToList xs) === xs
  , testProperty "member x (insert x xs) = True" $ \x xs ->
      optStanzaSetMember x (optStanzaSetInsert x xs) === True
  , testProperty "member x (singleton y) = (x == y)" $ \x y ->
      optStanzaSetMember x (optStanzaSetSingleton y) === (x == y)
  , testProperty "(subset xs ys, member x xs) ==> member x ys" $ \x xs ys ->
      optStanzaSetIsSubset xs ys && optStanzaSetMember x xs ==>
        optStanzaSetMember x ys
  , testProperty "tabulate index = id" $ \xs ->
      optStanzaTabulate (optStanzaIndex xs) === (xs :: OptionalStanzaMap Int)
  , testProperty "keysFilteredByValue" $ \xs ->
      let set i = if optStanzaIndex xs i then optStanzaSetSingleton i else mempty
       in optStanzaKeysFilteredByValue id xs === set TestStanzas `mappend` set BenchStanzas
  ]
