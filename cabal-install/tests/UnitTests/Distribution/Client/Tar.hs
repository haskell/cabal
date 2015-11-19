module UnitTests.Distribution.Client.Tar (
  tests
  ) where

import Distribution.Client.Tar (foldrEntries
                               , filterEntries
                               , foldrEntriesW
                               , filterEntriesW
                               , EntryContent(..)
                               , simpleEntry
                               , Entry(..)
                               , Entries(..)
                               , toTarPath
                               )

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Control.Monad.Writer.Lazy (runWriterT, tell)

tests :: [TestTree]
tests = [ testCase "foldrEntries" foldrTest
        , testCase "filterEntries" filterTest
        , testCase "foldrEntriesW" foldrWTest
        , testCase "filterEntriesW" filterWTest
        ]

foldrTest :: Assertion
foldrTest = do
  assertEqual "Unexpected result for Done" "x" $
    foldrEntries undefined "x" undefined Done
  assertEqual "Unexpected result for Fail" "x" $
    foldrEntries undefined undefined id $ Fail "x"
  let e1 = getFileEntry "path1" "x"
      e2 = getFileEntry "path2" "y"
      next = (\e acc -> let (NormalFile dta _) = entryContent e
                            str = BS.Char8.unpack dta
                        in str ++ acc)
  assertEqual "Unexpected result for Next" "xyz" $
    foldrEntries next "z" undefined $ Next e1 $ Next e2 Done
  assertEqual "Unexpected result for Fail" "xyf" $
    foldrEntries next "z" id $ Next e1 $ Next e2 $ Fail "f"

filterTest :: Assertion
filterTest = do
  let e1 = getFileEntry "file1" "x"
      e2 = getFileEntry "file2" "y"
      p = (\e -> let (NormalFile dta _) = entryContent e
                     str = BS.Char8.unpack dta
                 in not . (=="y") $ str)
  assertEqual "Unexpected result for filter" "xz" $
    entriesToString $ filterEntries p $ Next e1 $ Next e2 Done
  assertEqual "Unexpected result for filter" "z" $
    entriesToString $ filterEntries p $ Done
  assertEqual "Unexpected result for filter" "xf" $
    entriesToString $ filterEntries p $ Next e1 $ Next e2 $ Fail "f"

foldrWTest :: Assertion
foldrWTest =  do
  (r, w) <- runWriterT $ foldrEntriesW undefined
            (tell [1::Int] >> tell [2::Int] >> return "x") undefined Done
  assertEqual "Unexpected result for Done" "x" r
  assertEqual "Unexpected result for Done w" [1,2] w

  (r1, w1) <- runWriterT $ foldrEntriesW undefined undefined
              (return . id) $ Fail "x"
  assertEqual "Unexpected result for Fail" "x" r1
  assertEqual "Unexpected result for Fail w" "" w1

  let e1 = getFileEntry "path1" "x"
      e2 = getFileEntry "path2" "y"
      next = (\e acc -> let (NormalFile dta _) = entryContent e
                            str = BS.Char8.unpack dta
                        in tell "a" >> return (str ++ acc))
      done = tell "b" >> return "z"
  (r2, w2) <- runWriterT $ foldrEntriesW next done undefined $
              Next e1 $ Next e2 Done
  assertEqual "Unexpected result for Next" "xyz" r2
  assertEqual "Unexpected result for Next w" "baa" w2

  let fail' = (\f -> tell "c" >> return f) . id
  (r3, w3) <- runWriterT $ foldrEntriesW next done fail' $
              Next e1 $ Next e2 $ Fail "f"
  assertEqual "Unexpected result for Next" "xyf" r3
  assertEqual "Unexpected result for Next w" "caa" w3

filterWTest :: Assertion
filterWTest = do
  let e1 = getFileEntry "file1" "x"
      e2 = getFileEntry "file2" "y"
      p = (\e -> let (NormalFile dta _) = entryContent e
                     str = BS.Char8.unpack dta
                 in tell "t" >> return (not . (=="y") $ str))

  (r, w) <- runWriterT $ filterEntriesW p $ Next e1 $ Next e2 Done
  assertEqual "Unexpected result for filterW" "xz" $ entriesToString r
  assertEqual "Unexpected result for filterW w" "tt" w

  (r1, w1) <- runWriterT $ filterEntriesW p $ Done
  assertEqual "Unexpected result for filterW" "z" $ entriesToString r1
  assertEqual "Unexpected result for filterW w" "" w1

  (r2, w2) <- runWriterT $ filterEntriesW p $ Next e1 $ Next e2 $ Fail "f"
  assertEqual "Unexpected result for filterW" "xf" $ entriesToString r2
  assertEqual "Unexpected result for filterW w" "tt" w2

getFileEntry :: FilePath -> [Char] -> Entry
getFileEntry pth dta =
  simpleEntry tp $ NormalFile dta' $ BS.length dta'
  where  tp = case toTarPath False pth of
           Right tp' -> tp'
           Left e -> error e
         dta' = BS.Char8.pack dta

entriesToString :: Entries -> String
entriesToString =
  foldrEntries (\e acc -> let (NormalFile dta _) = entryContent e
                              str = BS.Char8.unpack dta
                          in str ++ acc) "z" id
