{-# LANGUAGE DeriveFunctor #-}
module Distribution.FieldGrammar.Pretty (
    PrettyFieldGrammar,
    prettyFieldGrammar,
    ) where

import           Distribution.Compat.Lens
import           Distribution.Compat.Newtype
import           Distribution.Compat.Prelude
import           Distribution.Parsec.Field   (FieldName)
import           Distribution.Pretty         (Pretty (..))
import           Distribution.Pretty.Field   (Field (..))
import           Distribution.Simple.Utils   (toUTF8BS)
import           Prelude ()
import           Text.PrettyPrint            (Doc)
import qualified Text.PrettyPrint            as PP

import Distribution.FieldGrammar.Class

newtype PrettyFieldGrammar s a = PrettyFG
    { fieldGrammarPretty :: s -> [Field]
    }
  deriving (Functor)

instance Applicative (PrettyFieldGrammar s) where
    pure _ = PrettyFG (\_ -> mempty)
    PrettyFG f <*> PrettyFG x = PrettyFG (\s -> f s <> x s)

-- | We can use 'PrettyFieldGrammar' to pp print the @s@.
--
-- /Note:/ there is not trailing @($+$ text "")@.
prettyFieldGrammar :: PrettyFieldGrammar s a -> s -> [Field]
prettyFieldGrammar = fieldGrammarPretty

instance FieldGrammar PrettyFieldGrammar where
    blurFieldGrammar f (PrettyFG pp) = PrettyFG (pp . aview f)

    uniqueFieldAla fn _pack l = PrettyFG $ \s ->
        ppField fn (pretty (pack' _pack (aview l s)))

    booleanFieldDef fn l def = PrettyFG pp
      where
        pp s
            | b == def  = mempty
            | otherwise = ppField fn (PP.text (show b))
          where
            b = aview l s

    optionalFieldAla fn _pack l = PrettyFG pp
      where
        pp s = case aview l s of
            Nothing -> mempty
            Just a  -> ppField fn (pretty (pack' _pack a))

    optionalFieldDefAla fn _pack l def = PrettyFG pp
      where
        pp s
            | x == def  = mempty
            | otherwise = ppField fn (pretty (pack' _pack x))
          where
            x = aview l s

    monoidalFieldAla fn _pack l = PrettyFG pp
      where
        pp s = ppField fn (pretty (pack' _pack (aview l s)))

    prefixedFields _fnPfx l = PrettyFG (pp . aview l)
      where
        pp xs =
            -- always print the field, even its Doc is empty.
            -- i.e. don't use ppField
            [ Field (toUTF8BS n) $ PP.vcat $ map PP.text $ lines s
            | (n, s) <- xs
            -- fnPfx `isPrefixOf` n
            ]

    knownField _           = pure ()
    deprecatedSince _  _ x = x
    availableSince _ _     = id
    hiddenField _          = PrettyFG (\_ -> mempty)

ppField :: FieldName -> Doc -> [Field]
ppField name fielddoc
    | PP.isEmpty fielddoc = []
    | otherwise        = [ Field name fielddoc ]
