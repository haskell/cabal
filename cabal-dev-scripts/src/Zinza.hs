-- |
-- SPDX-Identifier-Id: GPL-2.0-or-later AND BSD-3-Clause
--
-- Zinza - a small jinja-syntax-inspired template compiler.
--
-- Zinza compiles a template into as Haskell module. The template arguments are
-- ordinary Haskell records. Type-safe (text) templates without Template
-- Haskell, but using a preprocessor.
--
-- Zinza is very minimalistic. Features are added when needed.
--
-- == Examples
--
-- For an example see SPDX LicenseId module generation.
--
-- == Executable usage
--
-- @
-- ./zinza INPUT.tmpl OUTPUT.hs [--module-name=Template]
-- @
--
-- == Syntax
--
-- === Expressions
--
-- @
-- {{ expression }}
-- @
--
-- Expression syntax has two structures
--
-- * negated: @!foo@
--
-- * field access @foo.bar@
--
-- === Control structures
--
-- The __for__ and __if__ statements are supported:
--
-- @
-- {% for value in values %}
-- ...
-- {% endfor %}
-- @
--
-- @
-- {% if boolExpression %}
-- ...
-- {% endif %}
-- @
--
-- If a control structure tag starts at the first column, the possible
-- trailing new line feed is stripped. This way full-line control tags
-- don't introduce new lines in the output.
--
-- == Miscellanea
--
-- A plan is to iterate @zinza@ for some time in @Cabal@ code-base
-- and later release as a separate package.
--
-- Zinza could produce invalid Haskell code.
-- In some cases it could be smarter, or you can edit your template.
--
-- The license is @GPL-2.0-or-later@ with an exception
-- that the code embedded into a generate template is free of it.
-- I use @Bison-exception-2.2@ to indicate that.
--
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Main (main) where

import Control.Applicative              (many, optional, some, (<|>))
import Control.Monad                    (ap, void, when)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.State.Strict (State, get, modify', put, runState)
import Data.Char                        (isAlphaNum, isLower, toLower, toUpper)
import Data.Functor.Identity            (Identity (..))
import Data.Maybe                       (fromMaybe, isJust)
import Data.Semialign                   (alignWith)
import Data.These                       (these)
import Data.Void                        (Void, absurd)
import Text.Parsec
       (eof, getPosition, lookAhead, notFollowedBy, parse, satisfy, sourceColumn, try)
import Text.Parsec.Char                 (char, space, spaces, string)
import Text.Parsec.String               (Parser)

import qualified Control.Monad.EitherK      as U
import qualified Control.Unification        as U
import qualified Control.Unification.IntVar as U
import qualified Control.Unification.Types  as U

import qualified Data.Map            as M
import qualified Options.Applicative as O

main :: IO ()
main = do
    Opts input output mn <- O.execParser $ O.info (O.helper <*> optsP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Generate template module"
        ]

    contents <- readFile input
    nodes <- either (fail . show) return $
        parse (nodesP <* eof) input contents

    (nodes', rootTy) <- either (fail . show) return $ inference nodes

    let (strTypes, types) = generateTypes rootTy
    let types' = concatMap (\t -> ", " ++ t ++ "(..)") $ reverse types

    let contentsO :: String
        contentsO = unlines
            [ "module " ++ mn ++ " (instantiate" ++ types' ++ ") where"
            , ""
            , strTypes
            , generateTemplate nodes'
            , prelude
            ]

    if output == "-"
    then putStrLn contentsO
    else writeFile output contentsO

-------------------------------------------------------------------------------
-- Opts
-------------------------------------------------------------------------------

data Opts = Opts FilePath FilePath String

optsP :: O.Parser Opts
optsP = Opts
    <$> strArgument
        [ O.metavar "TEMPLATE"
        , O.help    "Template input"
        ]
    <*> strArgument
        [ O.metavar "OUTPUT"
        , O.help    "Haskell module output"
        ]
    <*> strOption
        [ O.long    "module-name"
        , O.metavar "MODULE"
        , O.help    "Module name"
        , O.value   "Main"
        , O.showDefault
        ]
  where
    strArgument = O.strArgument . mconcat
    strOption   = O.strOption   . mconcat

-------------------------------------------------------------------------------
-- Node syntax
-------------------------------------------------------------------------------

type Var = String

type Nodes a = [Node a]

-- | Template parts.
--
-- We use polymorphic recursion for de Bruijn indices.
-- See materials on @bound@ library.
data Node a
    = NRaw  String                          -- ^ raw text block
    | NExpr (Expr a)                        -- ^ expression @expr : String@
    | NIf   (Expr a) (Nodes a)              -- ^ conditional block, @expr : Bool@
    | NFor  Var (Expr a) (Nodes (Maybe a))  -- ^ for loop, @expr : List a@
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Expressions
data Expr a
    = EVar a               -- ^ variable
    | ERoot                -- ^ root variable
    | EField (Expr a) Var  -- ^ field accessor
    | ENot (Expr a)        -- ^ negation
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | 'Monad' instances give substitution.
instance Monad Expr where
    return = EVar

    EVar x          >>= k = k x
    ERoot           >>= _ = ERoot
    EField expr var >>= k = EField (expr >>= k) var
    ENot expr       >>= k = ENot (expr >>= k)

instance Applicative Expr where
    pure = return
    (<*>) = ap

-- | Substitution.
(>>==) :: Node a -> (a -> Expr b) -> Node b
NRaw s           >>== _ = NRaw s
NExpr expr       >>== k = NExpr (expr >>= k)
NIf expr ns      >>== k = NIf (expr >>= k) (map (>>== k) ns)
NFor var expr ns >>== k = NFor var (expr >>= k) (map (>>== traverse k) ns)

-------------------------------------------------------------------------------
-- "bound"
-------------------------------------------------------------------------------

-- | Abstraction.
abstract1 :: (Functor f, Eq a) => a -> f a -> f (Maybe a)
abstract1 x = fmap $ \y ->
    if x == y
    then Nothing
    else Just y

-- | Instantiate with a variable type
instantiate1ret :: Functor f => a -> f (Maybe a) -> f a
instantiate1ret = fmap . fromMaybe

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

varP :: Parser Var
varP = (:) <$> satisfy isLower <*> many (satisfy isVarChar)

isVarChar :: Char -> Bool
isVarChar c = isAlphaNum c || c == '_'

nodeP :: Parser (Node Var)
nodeP = directiveP <|> exprNodeP <|> newlineN <|> rawP

nodesP :: Parser (Nodes Var)
nodesP = many nodeP

newlineN :: Parser (Node Var)
newlineN = NRaw . pure <$> char '\n'

rawP :: Parser (Node Var)
rawP = mk <$> some rawCharP <*> optional (char '\n') where
    rawCharP   = notBrace <|> try (char '{' <* lookAhead notSpecial)
    notBrace   = satisfy $ \c -> c /= '{' && c /= '\n'
    notSpecial = satisfy $ \c -> c /= '{' && c /= '\n' && c /= '%'

    mk s Nothing  = NRaw s
    mk s (Just c) = NRaw (s ++ [c])

exprNodeP :: Parser (Node Var)
exprNodeP = do
    _ <- try (string "{{")
    spaces
    expr <- exprP
    spaces
    _ <- string "}}"
    return (NExpr expr)

exprP :: Parser (Expr Var)
exprP =  do
    b <- optional (char '!')
    v <- varP
    vs <- many (char '.' *> varP)
    let expr = foldl EField (EVar v) vs
    return $
        if isJust b
        then ENot expr
        else expr

directiveP :: Parser (Node Var)
directiveP = forP <|> ifP

spaces1 :: Parser ()
spaces1 = space *> spaces

open :: String -> Parser Bool
open n = do
    pos <- getPosition
    _ <- try $ string "{%" *> spaces *> string n *> spaces
    return $ sourceColumn pos == 1  -- parsec counts pos from 1, not zero.

close :: String -> Parser ()
close n = do
    on0 <- open ("end" ++ n)
    close' on0

close' :: Bool -> Parser ()
close' on0 = do
    _ <- string "%}"
    when on0 $ void $ optional (char '\n')

forP :: Parser (Node Var)
forP = do
    on0 <- open "for"
    var <- varP
    spaces1
    _ <- string "in"
    notFollowedBy $ satisfy isAlphaNum
    spaces1
    expr <- exprP
    spaces1
    close' on0
    ns <- nodesP
    close "for"
    return $ NFor var expr (abstract1 var <$> ns)

ifP :: Parser (Node Var)
ifP = do
    on0 <- open "if"
    expr <- exprP
    spaces
    close' on0
    ns <- nodesP
    close "if"
    return $ NIf expr ns

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Ty
    = TyUnit
    | TyBool
    | TyString
    | TyList Ty
    | TyRecord (M.Map Var Ty)
  deriving (Eq, Ord, Show)

data TyF a
    = TyVarF a
    | TyUnitF
    | TyBoolF
    | TyStringF
    | TyListF a
    | TyRecordF (M.Map Var a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance U.Unifiable TyF where
    zipMatch (TyVarF a)    (TyVarF b)    = Just (TyVarF (Right (a, b)))
    zipMatch TyUnitF       TyUnitF       = Just TyUnitF
    zipMatch TyBoolF       TyBoolF       = Just TyBoolF
    zipMatch TyStringF     TyStringF     = Just TyStringF
    zipMatch (TyListF a)   (TyListF b)   = Just (TyListF (Right (a, b)))
    zipMatch (TyRecordF a) (TyRecordF b) = Just $ TyRecordF $
        alignWith (these Left Left (curry Right)) a b

    zipMatch _ _ = Nothing

type UTy = U.UTerm TyF U.IntVar

-- | Convert type used in unification into normal type.
-- If a type contains free unification variables, we default them to 'TyUnit'.
--
-- /Note:/ use 'freeze' only after 'U.applyBindings'.
freeze :: UTy -> Ty
freeze (U.UVar _)              = TyUnit
freeze (U.UTerm (TyVarF x))    = freeze x
freeze (U.UTerm TyUnitF)       = TyUnit
freeze (U.UTerm TyBoolF)       = TyBool
freeze (U.UTerm TyStringF)     = TyString
freeze (U.UTerm (TyListF a))   = TyList (freeze a)
freeze (U.UTerm (TyRecordF m)) = TyRecord (fmap freeze m)

-------------------------------------------------------------------------------
-- Type inference.
-------------------------------------------------------------------------------

type Error = U.UFailure TyF U.IntVar
type Unify = U.EitherKT Error (U.IntBindingT TyF Identity)

-- | Type-inference for used variables.
inference
    :: [Node Var]                      -- ^ input nodes
    -> Either Error ([Node Void], Ty)  -- ^ elaborated nodes and type of the root record.
inference ns0 =
    fmap (fmap freeze) $ runIdentity $ U.evalIntBindingT $ U.runEitherKT $ do
        -- all non binded variables are considered to be fields of "root".
        rootTy <- newVar
        let ns1 :: [Node (Void, UTy)]  -- Void, i.e. no free variables.
                                       -- each variable knows own type
                                       -- (if there were any variables :)
            ns1 = fmap (>>== rootField) ns0

        -- traverse the nodes, inferring the type of root.
        ns3 <- inferenceM rootTy ns1

        -- finalise the unifications
        rootTy' <- U.applyBindings rootTy

        -- done.
        return (ns3, rootTy')
  where
    rootField :: Var -> Expr (Void, UTy)
    rootField = EField ERoot

newVar :: Unify UTy
newVar = lift $ U.UVar <$> U.freeVar

inferenceM
    :: UTy               -- ^ root type
    -> [Node (a, UTy)]
    -> Unify [Node a]
inferenceM rootTy = goMany where
    goMany :: [Node (a, UTy)] -> Unify [Node a]
    goMany = traverse go

    go :: Node (a, UTy) -> Unify (Node a)

    go (NRaw t)  = return $ NRaw t

    go (NExpr expr) = do
        (expr', _) <- checkExpr rootTy expr (U.UTerm TyStringF)
        return $ NExpr expr'

    go (NFor var expr ns) = do
        elTy <- newVar
        (expr', _) <- checkExpr rootTy expr (U.UTerm (TyListF elTy))
        let fwd :: Maybe (a, UTy) -> (Maybe a, UTy)
            fwd Nothing        = (Nothing, elTy)
            fwd (Just (x, ty)) = (Just x, ty)

        let nsB = fmap2 fwd ns
        nsB' <- goMany nsB

        return $ NFor var expr' nsB'

    go (NIf expr ns) = do
        (expr', _) <- checkExpr rootTy expr (U.UTerm TyBoolF)
        ns' <- goMany ns

        return $ NIf expr' ns'

-- | Expressions are checked, as statements require specific types.
checkExpr
    :: UTy                  -- ^ type of root expression
    -> Expr (a, UTy)        -- ^ expression
    -> UTy                  -- ^ expected type.
    -> Unify (Expr a, UTy)  -- ^ elaborated expression and its type
checkExpr rootTy ERoot         ty' = do
    rootTy' <- U.unify rootTy ty'
    return (ERoot, rootTy')
checkExpr _     (EVar (v, ty)) ty' = do
    ty'' <- U.unify ty ty'
    return (EVar v, ty'')
checkExpr rootTy (EField expr field) ty' = do
    (expr', exprTy) <- checkExpr rootTy expr $ U.UTerm $ TyRecordF $ M.singleton field ty'
    return (EField expr' field, exprTy)
checkExpr rootTy (ENot expr) ty = do
    let boolTy = U.UTerm TyBoolF
    _ <- U.unify ty boolTy
    (expr', _exprTy) <- checkExpr rootTy expr boolTy
    return (ENot expr', boolTy)

-------------------------------------------------------------------------------
-- Generate types
-------------------------------------------------------------------------------

generateTypes :: Ty -> (String, [TyName])
generateTypes ty0 =
    ( unlines $ "-- data types" : ls
    , types
    )
  where
    ls :: [String]
    types :: [TyName]
    (ls, (_, types)) = runState loop ([("Cfg", ty0)], [])

    loop :: State ([(String, Ty)], [TyName]) [String]
    loop = do
        x <- get
        case x of
            ([], _) -> return []
            ((name, ty):tys, tys') -> do
                put (tys, tys')
                (++) <$> single name ty <*> loop

    single name (TyRecord m) = do
        modify' $ \(x, y) ->
            ( [ (typeNameRec k, ty) | (k, ty) <- M.toList m ]  ++ x
            , name : y
            )
        return $
            [ ""
            , "data " ++ name ++ " = " ++ name
            ] ++ mapHeadTail ("  { " ++) ("  , " ++)
            [ fieldName name k ++ " :: !" ++ typeName k ty
            | (k, ty) <- M.toList m
            ] ++
            [ "  } deriving (Show)"
            ]
    single name (TyList ty) = single name ty
    single _ _ = return []

fieldName :: TyName -> String -> String
fieldName tyName field = mapFirst toLower tyName ++ mapFirst toUpper field

typeName :: String -> Ty -> TyName
typeName name (TyRecord _) = typeNameRec name
typeName _    TyUnit       = "()"
typeName _    TyBool       = "Bool"
typeName _    TyString     = "String"
typeName name (TyList el)  = "[" ++ typeName name el ++ "]"

typeNameRec :: String -> String
typeNameRec = ("Cfg" ++) . mapFirst toUpper

mapHeadTail :: (a -> a) -> (a -> a) -> [a] -> [a]
mapHeadTail _ _ []     = []
mapHeadTail f g (x:xs) = f x : map g xs

mapFirst f = mapHeadTail f id
mapFirst :: (a -> a) -> [a] -> [a]

-------------------------------------------------------------------------------
-- Generate template
-------------------------------------------------------------------------------

type TyName = String

generateTemplate :: [Node Void] -> String
generateTemplate ns0 = unlines $
    "-- template instantiation" :
    "" :
    "instantiate :: Cfg -> String" :
    "instantiate cfg = ($ []) $ id" :
    strs
  where
    ns1 :: [Node (Var, TyName)]
    ns1 = fmap2 absurd ns0

    strs = goMany "  " ns1

    goMany :: String -> [Node (Var, TyName)] -> [String]
    goMany pfx = concatMap (go pfx)

    go :: String -> Node (Var, TyName) -> [String]
    go pfx (NRaw  s)          = [pfx ++ ". showString " ++ show s]
    go pfx (NExpr e)          = [pfx ++ ". showString (" ++ fst (showExpr e) ++ ")"]
    go pfx (NIf expr ns)    =
        [ pfx ++ ". ( _ifImpl (" ++ varCode ++ ") $ id"
        ] ++
        goMany ("  " ++ pfx) ns ++
        [ pfx ++ "  )"
        ]
      where
        (varCode, _) = showExpr expr
    go pfx (NFor var expr ns) =
        [ pfx ++ ". _forImpl (" ++ varCode ++ ") (\\" ++ var ++ " -> id"
        ] ++
        goMany ("  " ++ pfx) (map (instantiate1ret (var, varTyName)) ns) ++
        [ pfx ++ "  )"
        ]
      where
        (varCode, varTyName) = showExpr expr

showExpr :: Expr (Var, TyName) -> (String, TyName)
showExpr ERoot               = ("cfg", "Cfg")
showExpr (EVar varTy)        = varTy
showExpr (EField expr field) =
    (fieldName exprTyName field ++ " $ " ++ exprCode, typeNameRec field)
  where
    (exprCode, exprTyName) = showExpr expr
showExpr (ENot expr)         =
    ("not $ " ++ fst (showExpr expr), "Bool")

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

-------------------------------------------------------------------------------
-- Prelude
-------------------------------------------------------------------------------

-- | Functions used by generate template module.
--
-- Their names begin with underscores for two reasons:
--
-- * If they are not used, there are no warnings
--
-- * Less likely to clash with some identifier.
--
-- The included code, i.e. @_ifImpl@ and @_forImpl@ 
-- are licensed under the @BSD-3-Clause@ license.
--
prelude :: String
prelude = unlines
    [ "-- 'prelude'"
    , ""
    , "_ifImpl :: Bool -> ShowS -> ShowS"
    , "_ifImpl True  s = s"
    , "_ifImpl False _ = id"
    , ""
    , "_forImpl :: [a] -> (a -> ShowS) -> ShowS"
    , "_forImpl xs f = foldr (\\x s -> f x . s) id xs"
    ]
