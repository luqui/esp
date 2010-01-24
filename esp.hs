{-# LANGUAGE PatternGuards #-}

import qualified Graphics.Vty.Widgets.All as W
import qualified Graphics.Vty as W
import Control.Applicative
import ESP.UI
import ESP.Command
import qualified Data.Char as Char

type Editor = Command UserInput W.AnyWidget

data AST 
    = App AST AST
    | Lam String AST
    | Var String
    deriving (Show)

showAST :: Bool -> Bool -> AST -> String
showAST lp ap (App t u) = parensS ap $ showAST True False t ++ " " ++ showAST True True u
showAST lp ap (Lam v t) = parensS lp $ "\\" ++ v ++ ". " ++ showAST False False t
showAST lp ap (Var v) = v

parensS True x = "(" ++ x ++ ")"
parensS False x = x

defaultAttr = W.Attr { W.style = W.SetTo 0, W.fore_color = W.SetTo W.white, W.back_color = W.SetTo W.black }
highlightAttr = defaultAttr { W.style = W.SetTo 0, W.fore_color = W.SetTo W.black, W.back_color = W.SetTo W.white }

identifier :: [Char] -> String -> Editor String
identifier seps = go
    where
    go inp = More (text defaultAttr inp <++> text highlightAttr " ") $ \(UserInput key _) ->
        case key of
            W.KEnter -> Done inp
            W.KASCII ch | ch `elem` seps -> Done inp
                        | otherwise      -> go (inp ++ [ch])
            W.KBS -> go (safeInit inp)
            _ -> go inp
    safeInit [] = []
    safeInit xs = init xs

lambda :: Editor AST
lambda = do
    var <- mapCommand (\e -> text defaultAttr "\\" <++> e <++> text defaultAttr ".") $ identifier " ." ""
    body <- mapCommand (text defaultAttr ("\\" ++ var ++ ". ") <++>) $ expr
    return $ Lam var body

parens :: Editor a -> Editor a
parens = mapCommand (\e -> text defaultAttr "(" <++> e <++> text defaultAttr ")")

simpleExpr :: Editor AST
simpleExpr = More (text defaultAttr "") $ \(UserInput key _) ->
        case key of
            W.KASCII '(' -> parens expr
            W.KASCII '\\' -> lambda
            W.KASCII ch | Char.isAlpha ch -> Var <$> identifier " ()" [ch]
            _ -> simpleExpr

expr :: Editor AST
expr = go =<< simpleExpr
    where
    go exp = More (text highlightAttr (showAST False False exp)) $ \inp@(UserInput key _) ->
        case key of
            W.KEnter -> Done exp
            _ -> go =<< (exp `App`) <$> mapCommand (text defaultAttr (showAST True False exp) <++> text defaultAttr " " <++>) (putback inp simpleExpr)
    

commandTest :: (Show a) => Command UserInput W.AnyWidget a -> UI UserInput W.AnyWidget
commandTest (Done a) = UI (text highlightAttr (show a)) (const (commandTest (Done a)))
commandTest (More w c) = UI w (commandTest . c)

(<++>) :: W.AnyWidget -> W.AnyWidget -> W.AnyWidget
x <++> y = W.anyWidget (x W.<++> y)
(<-->) :: W.AnyWidget -> W.AnyWidget -> W.AnyWidget
x <--> y = W.anyWidget (x W.<--> y)

itext :: String -> W.AnyWidget
itext s = text defaultAttr s <++> text highlightAttr " " <++> text defaultAttr " "

text :: W.Attr -> String -> W.AnyWidget
text attr = W.anyWidget . W.text attr

main = runVty (commandTest expr)
