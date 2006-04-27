module DTypes where

import Text.XML.HaXml.Haskell2Xml hiding (Name)

-- data types for a simple test program

data Person = Person Name Email [Rating] Version {-! derive :Haskell2Xml !-}

newtype Name = Name String {-! derive :Haskell2Xml !-}
newtype Email = Email String {-! derive :Haskell2Xml !-}
newtype Version = Version Int {-! derive :Haskell2Xml !-}

data Rating = Rating SubjectID Interest Skill {-! derive :Haskell2Xml !-}

newtype SubjectID = SubjectID Int {-! derive :Haskell2Xml !-}
newtype Interest = Interest Score {-! derive :Haskell2Xml !-}
newtype Skill = Skill Score {-! derive :Haskell2Xml !-}

data Score = ScoreNone | ScoreLow | ScoreMedium | ScoreHigh {-! derive :Haskell2Xml !-}

