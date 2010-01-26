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

type Editor = Command UserInput C.ConcreteExp

data Complexity = Simple | Application | Lambda
    deriving (Eq,Ord)

concrete :: AST -> C.ConcreteExp
concrete (AApp t u) = C.app (concrete t) (concrete u)
concrete (ALam v t) = C.lam v (concrete t)
concrete (AVar v) = C.var v
concrete AHole = C.hole


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
        ui@(UserInput key _) <- input (C.highlight $ concrete ast)
        case key of
            W.KASCII ' ' -> mkEditor . (ast `AApp`) =<< mapCommand (concrete ast `C.app`) (mkEditor AHole)
            W.KDel -> mkEditor AHole
            W.KEnter -> return ast
            _ -> go ast ui
    where
    go (AApp t u) (UserInput key _) =
        case key of
            W.KASCII 'h' -> mkEditor . (`AApp` u) =<< mapCommand (`C.app` concrete u) (mkEditor t)
            W.KASCII 'l' -> mkEditor . (t `AApp`) =<< mapCommand (concrete t `C.app`) (mkEditor u)
            W.KASCII 'H' -> mkEditor t
            W.KASCII 'L' -> mkEditor u
            W.KASCII 'b' -> mkEditor (betaExpand (AApp t u))
            _ -> mkEditor (AApp t u)
    go (ALam v t) (UserInput key _) =
        case key of
            W.KASCII 'h' -> do
                v' <- mapCommand (\v' -> v' `C.lam` concrete (alphaConvert v v' t)) (identifier " ." v)
                mkEditor (ALam v' (alphaConvert v v' t))
            W.KASCII 'l' -> mkEditor . (ALam v) =<< mapCommand (C.lam v) (mkEditor t)
            _ -> mkEditor (ALam v t)
    go (AVar v) (UserInput key _) =
        case key of
            W.KASCII 'c' -> activeVarEditor ""
            W.KASCII 'e' -> activeVarEditor v
            _ -> mkEditor (AVar v)
    go AHole ui@(UserInput key _) =
        case key of
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
