module DetailedTests (tests) where

import Distribution.TestSuite

tests :: IO [Test]
tests =
  return
    [ Test
        TestInstance
          { run = return (Finished Pass)
          , name = "always-passes"
          , tags = []
          , options = []
          , setOption = \_ _ -> Left "no options"
          }
    ]
