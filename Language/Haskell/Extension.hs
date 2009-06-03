-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Extension
-- Copyright   :  Isaac Jones 2003-2004
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- Haskell language extensions

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Language.Haskell.Extension (
        Extension(..),
        knownExtensions,
        deprecatedExtensions
  ) where

import Distribution.Text (Text(..))
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import qualified Data.Char as Char (isAlphaNum)
import Data.Array (Array, accumArray, bounds, Ix(inRange), (!))

-- ------------------------------------------------------------
-- * Extension
-- ------------------------------------------------------------

-- Note: if you add a new 'Extension':
--
-- * also add it to the Distribution.Simple.X.languageExtensions lists
--   (where X is each compiler: GHC, JHC, Hugs, NHC)
--
-- * also to the 'knownExtensions' list below.

-- |This represents language extensions beyond Haskell 98 that are
-- supported by some implementations, usually in some special mode.

data Extension
  = OverlappingInstances
  | UndecidableInstances
  | IncoherentInstances
  | RecursiveDo
  | ParallelListComp
  | MultiParamTypeClasses
  | NoMonomorphismRestriction
  | FunctionalDependencies
  | Rank2Types
  | RankNTypes
  | PolymorphicComponents
  | ExistentialQuantification
  | ScopedTypeVariables
  -- | Deprecated, use ScopedTypeVariables instead.
  | PatternSignatures
  | ImplicitParams
  | FlexibleContexts
  | FlexibleInstances
  | EmptyDataDecls
  | CPP

  | KindSignatures
  | BangPatterns
  | TypeSynonymInstances
  | TemplateHaskell
  | ForeignFunctionInterface
  | Arrows
  | Generics
  | NoImplicitPrelude
  | NamedFieldPuns
  | PatternGuards
  | GeneralizedNewtypeDeriving

  | ExtensibleRecords
  | RestrictedTypeSynonyms
  | HereDocuments
  | MagicHash
  | TypeFamilies
  | StandaloneDeriving

  | UnicodeSyntax
  | UnliftedFFITypes
  | LiberalTypeSynonyms
  | TypeOperators
--PArr -- not ready yet, and will probably be renamed to ParallelArrays
  | RecordWildCards
  | RecordPuns
  | DisambiguateRecordFields
  | OverloadedStrings
  | GADTs
  | NoMonoPatBinds
  | RelaxedPolyRec
  | ExtendedDefaultRules
  | UnboxedTuples
  | DeriveDataTypeable
  | ConstrainedClassMethods

  -- | Allow imports to be qualified by the package name that the module
  -- is intended to be imported from, e.g.
  --
  -- > import "network" Network.Socket
  | PackageImports

  | ImpredicativeTypes
  | NewQualifiedOperators
  | PostfixOperators
  | QuasiQuotes
  | TransformListComp
  | ViewPatterns

  -- | Allow concrete XML syntax to be used in expressions and patterns,
  -- as per the Haskell Server Pages extension language: 
  -- <http://www.haskell.org/haskellwiki/HSP>. The ideas behind it are 
  -- discussed in the paper "Haskell Server Pages through Dynamic Loading"
  -- by Niklas Broberg, from Haskell Workshop '05.
  | XmlSyntax

  -- | Allow regular pattern matching over lists, as discussed in the
  -- paper "Regular Expression Patterns" by Niklas Broberg, Andreas Farre
  -- and Josef Svenningsson, from ICFP '04.
  | RegularPatterns

  | UnknownExtension String
  deriving (Show, Read, Eq)

-- | Extensions that have been deprecated, possibly paired with another
-- extension that replaces it.
--
deprecatedExtensions :: [(Extension, Maybe Extension)]
deprecatedExtensions =
  [ (RecordPuns, Just NamedFieldPuns)
  , (PatternSignatures, Just ScopedTypeVariables)
  ]

knownExtensions :: [Extension]
knownExtensions =
  [ OverlappingInstances
  , UndecidableInstances
  , IncoherentInstances
  , RecursiveDo
  , ParallelListComp
  , MultiParamTypeClasses
  , NoMonomorphismRestriction
  , FunctionalDependencies
  , Rank2Types
  , RankNTypes
  , PolymorphicComponents
  , ExistentialQuantification
  , ScopedTypeVariables
  , ImplicitParams
  , FlexibleContexts
  , FlexibleInstances
  , EmptyDataDecls
  , CPP

  , KindSignatures
  , BangPatterns
  , TypeSynonymInstances
  , TemplateHaskell
  , ForeignFunctionInterface
  , Arrows
  , Generics
  , NoImplicitPrelude
  , NamedFieldPuns
  , PatternGuards
  , GeneralizedNewtypeDeriving

  , ExtensibleRecords
  , RestrictedTypeSynonyms
  , HereDocuments
  , MagicHash
  , TypeFamilies
  , StandaloneDeriving

  , UnicodeSyntax
  , PatternSignatures
  , UnliftedFFITypes
  , LiberalTypeSynonyms
  , TypeOperators
--PArr -- not ready yet, and will probably be renamed to ParallelArrays
  , RecordWildCards
  , RecordPuns
  , DisambiguateRecordFields
  , OverloadedStrings
  , GADTs
  , NoMonoPatBinds
  , RelaxedPolyRec
  , ExtendedDefaultRules
  , UnboxedTuples
  , DeriveDataTypeable
  , ConstrainedClassMethods
  , PackageImports
  , ImpredicativeTypes
  , NewQualifiedOperators
  , PostfixOperators
  , QuasiQuotes
  , TransformListComp
  , ViewPatterns
  , XmlSyntax
  , RegularPatterns
  ]

instance Text Extension where
  disp (UnknownExtension other) = Disp.text other
  disp other                    = Disp.text (show other)

  parse = do
    extension <- Parse.munch1 Char.isAlphaNum
    return (classifyExtension extension)

-- | 'read' for 'Extension's is really really slow so for the Text instance
-- what we do is make a simple table indexed off the first letter in the
-- extension name. The extension names actually cover the range @'A'-'Z'@
-- pretty densely and the biggest bucket is 7 so it's not too bad. We just do
-- a linear search within each bucket.
--
-- This gives an order of magnitude improvement in parsing speed, and it'll
-- also allow us to do case insensitive matches in future if we prefer.
--
classifyExtension :: String -> Extension
classifyExtension string@(c:_)
  | inRange (bounds extensionTable) c
  = case lookup string (extensionTable ! c) of
      Just extension    -> extension
      Nothing           -> UnknownExtension string
classifyExtension string = UnknownExtension string

extensionTable :: Array Char [(String, Extension)]
extensionTable =
  accumArray (flip (:)) [] ('A', 'Z')
    [ (head str, (str, extension))
    | extension <- knownExtensions
    , let str = show extension ]
