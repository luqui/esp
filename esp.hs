{-# LANGUAGE PatternGuards #-}

import qualified Graphics.Vty.Widgets.All as W
import qualified Graphics.Vty as W
import Control.Applicative
import ESP.UI
import ESP.Command
import ESP.AST
import qualified Data.Char as Char

type Editor = Command UserInput CSyn

data Complexity = Simple | Application | Lambda
    deriving (Eq,Ord)

data CSyn = CSyn W.AnyWidget Complexity

cApp :: CSyn -> CSyn -> CSyn
cApp t u = CSyn (parensC Application t <++> text defaultAttr " " <++> parensC Simple u) Application

cLam :: String -> CSyn -> CSyn
cLam v t = CSyn (text defaultAttr ("\\" ++ v ++ ". ") <++> parensC Lambda t) Lambda

cVar :: String -> CSyn
cVar v = CSyn (text defaultAttr v) Simple

cHole :: CSyn
cHole = CSyn (text defaultAttr "[]") Simple

concrete :: AST -> CSyn
concrete (AApp t u) = cApp (concrete t) (concrete u)
concrete (ALam v t) = cLam v (concrete t)
concrete (AVar v) = cVar v
concrete AHole = cHole

highlight :: CSyn -> CSyn
highlight (CSyn w c) = CSyn (W.withAttribute w highlightAttr) c

parensC c (CSyn t c') 
    | c' <= c = t
    | otherwise  = text defaultAttr "(" <++> t <++> text defaultAttr ")"

defaultAttr = W.Attr { W.style = W.SetTo 0, W.fore_color = W.SetTo W.white, W.back_color = W.SetTo W.black }
highlightAttr = defaultAttr { W.style = W.SetTo 0, W.fore_color = W.SetTo W.black, W.back_color = W.SetTo W.white }

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
        ui@(UserInput key _) <- input (highlight $ concrete ast)
        case key of
            W.KASCII ' ' -> mkEditor . (ast `AApp`) =<< mapCommand (concrete ast `cApp`) (mkEditor AHole)
            W.KDel -> mkEditor AHole
            W.KEnter -> return ast
            _ -> go ast ui
    where
    go (AApp t u) (UserInput key _) =
        case key of
            W.KASCII 'h' -> mkEditor . (`AApp` u) =<< mapCommand (`cApp` concrete u) (mkEditor t)
            W.KASCII 'l' -> mkEditor . (t `AApp`) =<< mapCommand (concrete t `cApp`) (mkEditor u)
            W.KASCII 'H' -> mkEditor t
            W.KASCII 'L' -> mkEditor u
            W.KASCII 'b' -> mkEditor (betaExpand (AApp t u))
            _ -> mkEditor (AApp t u)
    go (ALam v t) (UserInput key _) =
        case key of
            W.KASCII 'h' -> do
                v' <- mapCommand (\v' -> v' `cLam` concrete (alphaConvert v v' t)) (identifier " ." v)
                mkEditor (ALam v' (alphaConvert v v' t))
            W.KASCII 'l' -> mkEditor . (ALam v) =<< mapCommand (cLam v) (mkEditor t)
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
    v <- mapCommand (`cLam` concrete AHole) (identifier " ." "")
    t <- mapCommand (cLam v) (mkEditor AHole)
    mkEditor (ALam v t)

activeVarEditor :: String -> Editor AST
activeVarEditor v = mkEditor . AVar =<< mapCommand (addCursor . cVar) (identifier " " v)
    where
    addCursor (CSyn w c) = CSyn (w <++> text highlightAttr " ") c

commandTest :: Editor a -> UI UserInput W.AnyWidget
commandTest editor = (\(CSyn w _) -> w <++> text defaultAttr " ") <$> repeatCommand editor

(<++>) :: W.AnyWidget -> W.AnyWidget -> W.AnyWidget
x <++> y = W.anyWidget (x W.<++> y)
(<-->) :: W.AnyWidget -> W.AnyWidget -> W.AnyWidget
x <--> y = W.anyWidget (x W.<--> y)

itext :: String -> W.AnyWidget
itext s = text defaultAttr s <++> text highlightAttr " " <++> text defaultAttr " "

text :: W.Attr -> String -> W.AnyWidget
text attr = W.anyWidget . W.text attr

main = runVty (commandTest (mkEditor AHole))
