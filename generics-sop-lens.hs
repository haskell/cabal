{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
import qualified GHC.Generics as GHC
import Data.List (stripPrefix)
import Data.Typeable
import Generics.SOP
import Generics.SOP.GGP

-- | An example of generic deriving of lens code.
--
-- >>> putStrLn $ genericLenses (Proxy :: Proxy Foobar)
-- fooBar :: Lens' Foobar Int
-- fooBar f s = fmap (\x -> s { T.fooBar = x }) (T.fooBar s)
-- {-# INLINE fooBar #-}
-- <BLANKLINE>
-- fooXyzzy :: Lens' Foobar [[Char]]
-- fooXyzzy f s = fmap (\x -> s { T.fooXyzzy = x }) (T.fooXyzzy s)
-- {-# INLINE fooXyzzy #-}
-- ...
--
-- /Note:/ 'FilePath' i.e @type@ aliases are lost.
--
data Foobar = Foobar
    { fooBar   :: Int
    , fooXyzzy :: [FilePath]
    , fooQuux  :: Bool
    }
  deriving (GHC.Generic)

genericLenses
    :: forall a xs proxy. (GDatatypeInfo a, GCode a ~ '[xs], All Typeable xs)
    => proxy a 
    -> String
genericLenses p = case gdatatypeInfo p of
    Newtype _ _ _                   -> "-- newtype deriving not implemented"
    ADT _ dn (Constructor _ :* Nil) -> "-- fieldnameless deriving not implemented"
    ADT _ dn (Infix _ _ _ :* Nil)   -> "-- infix consturctor deriving not implemented"
    ADT _ dn (Record _ fis :* Nil) ->
        unlines $ concatMap replaceTypes $ hcollapse $ hcmap (Proxy :: Proxy Typeable) derive fis
      where
        derive :: forall x. Typeable x => FieldInfo x -> K [String] x
        derive (FieldInfo fn) = K
            [ fn ++ " :: Lens' " ++ dn ++ " " ++ showsPrec 11 (typeRep (Proxy :: Proxy x)) []
            , fn ++ " f s = fmap (\\x -> s { T." ++ fn ++ " = x }) (f (T." ++  fn ++ " s))"
            , "{-# INLINE " ++ fn ++ " #-}"
            , ""
            ]

        replaceTypes = map
            $ replace "[Char]" "String"

        replace needle replacement = go where
            go [] = []
            go xs@(x:xs')
                | Just ys <- stripPrefix needle xs = replacement ++ go ys
                | otherwise                        = x : go xs'
