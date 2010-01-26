{-# LANGUAGE PatternGuards #-}

import qualified Graphics.Vty.Widgets.All as W
import qualified Graphics.Vty as W
import Control.Applicative
import ESP.UI
import ESP.Command
import ESP.AST
import qualified ESP.Concrete as C
import ESP.WidgetUtils
import qualified Data.Char as Char
import Data.List (sort,nub)

type Editor = Command UserInput C.ConcreteExp

data Complexity = Simple | Application | Lambda
    deriving (Eq,Ord)

concrete :: AST -> C.ConcreteExp
concrete (AApp t u) = C.app (concrete t) (concrete u)
concrete (ALam v t) = C.lam v (concrete t)
concrete (AVar v) = C.var v
concrete AHole = C.hole

type Pattern = UserInput -> Bool

ascii :: Char -> Pattern
ascii ch (UserInput (W.KASCII key) mods)
    | Char.isUpper ch && ch == key && mods == [W.MShift] = True
    | ch == key && mods == [] = True
    | otherwise = False
ascii _ _ = False

key :: W.Key -> [W.Modifier] -> Pattern
key key mods (UserInput key' mods')
    | key == key' && norm mods == norm mods' = True
    | otherwise = False
    where
    norm = nub . sort

__ :: Pattern
__ = const True

match :: UserInput -> [(Pattern,a)] -> a
match ui = foldr match (error "No match")
    where
    match (pat,r) def
        | pat ui = r
        | otherwise = def

pattern = flip match

infix 0 -->
(-->) = (,)

identifier :: [Char] -> String -> Command UserInput String String
identifier seps = go
    where
    go inp = do
        UserInput key _ <- input inp
        case key of
            W.KEnter -> return inp
            W.KASCII ch | ch `elem` seps -> return inp
                        | otherwise      -> go (inp ++ [ch])
            W.KBS -> go (safeInit inp)
            _ -> go inp
    safeInit [] = []
    safeInit xs = init xs

mkEditor :: AST -> Editor AST
mkEditor ast = do
        ui <- input (C.highlight $ concrete ast)
        match ui [
            ascii ' ' --> mkEditor . (ast `AApp`) =<< mapCommand (concrete ast `C.app`) (mkEditor AHole),
            key W.KDel [] --> mkEditor AHole,
            key W.KEnter [] --> return ast,
            __ --> go ast ui
         ]
    where
    go (AApp t u) = pattern [
            ascii 'h' --> mkEditor . (`AApp` u) =<< mapCommand (`C.app` concrete u) (mkEditor t),
            ascii 'l' --> mkEditor . (t `AApp`) =<< mapCommand (concrete t `C.app`) (mkEditor u),
            ascii 'H' --> mkEditor t,
            ascii 'L' --> mkEditor u,
            ascii 'b' --> mkEditor (betaExpand (AApp t u)),
            __        --> mkEditor (AApp t u)
        ]
    go (ALam v t) = pattern [
            ascii 'h' --> do
                v' <- mapCommand (\v' -> v' `C.lam` concrete (alphaConvert v v' t)) (identifier " ." v)
                mkEditor (ALam v' (alphaConvert v v' t)),
            ascii 'l' --> mkEditor . (ALam v) =<< mapCommand (C.lam v) (mkEditor t),
            __        --> mkEditor (ALam v t)
        ]
    go (AVar v) = pattern [
            ascii 'c' --> activeVarEditor "",
            ascii 'e' --> activeVarEditor v,
            __        --> mkEditor (AVar v)
        ]
    go AHole = \(UserInput key _) -> case key of
            W.KASCII ch | Char.isAlpha ch -> activeVarEditor [ch]
            W.KASCII '\\' -> activeLambdaEditor
            _ -> mkEditor AHole

activeLambdaEditor :: Editor AST
activeLambdaEditor = do
    v <- mapCommand (`C.lam` concrete AHole) (identifier " ." "")
    t <- mapCommand (C.lam v) (mkEditor AHole)
    mkEditor (ALam v t)

activeVarEditor :: String -> Editor AST
activeVarEditor v = mkEditor . AVar =<< mapCommand (C.highlight . C.var) (identifier " " v)

commandTest :: Editor a -> UI UserInput W.AnyWidget
commandTest editor = (\c -> C.getWidget c <++> text defaultAttr " ") <$> repeatCommand editor

main = runVty (commandTest (mkEditor AHole))
