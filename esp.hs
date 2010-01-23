{-# LANGUAGE PatternGuards #-}

import qualified Graphics.Vty.Widgets.All as W
import qualified Graphics.Vty as W
import Control.Applicative
import Data.Monoid (Monoid(..))
import ESP.UI
import ESP.Command
import qualified ESP.Parser as P
import Data.Function (fix)
import Debug.Trace
import Control.Monad (MonadPlus(..))


defaultAttr = W.Attr { W.style = W.SetTo 0, W.fore_color = W.SetTo W.white, W.back_color = W.SetTo W.black }
highlightAttr = defaultAttr { W.style = W.SetTo 0, W.fore_color = W.SetTo W.black, W.back_color = W.SetTo W.white }

inputParser :: P.Parser Char a -> Command UserInput String a
inputParser = \p -> fix (go "" p)
    where
    go inp (P.Get c) undo = More inp $ \(UserInput key _) ->
        let this = go inp (P.Get c) undo in
        case key of
            W.KASCII ch -> go (inp ++ [ch]) (c ch) this
            W.KBS -> undo
            _ -> this
    go inp (P.Result r P.Fail) undo = Done r
    go inp (P.Result r p') undo = More inp $ \i@(UserInput key _) -> 
            case key of
                W.KEnter -> Done r
                _ -> putback i (go inp p' undo )
    go inp P.Fail undo = undo

parseDefnLhs :: P.Parser Char String
parseDefnLhs = do
    x <- P.identifier
    P.whitespace
    P.symbol "="
    return x

inputDecl :: UI UserInput W.AnyWidget
inputDecl =
    runCommand (\s -> (text defaultAttr (s ++ " = ") <++>) <$> inputExpr) $ 
        mapCommand itext (inputParser parseDefnLhs)

data ExprHead
    = Lambda String
    | Parens
    | Variable String

parseExprHead :: P.Parser Char ExprHead
parseExprHead = (P.symbol "(" *> pure Parens) 
        `mplus` (Lambda <$> lambda) 
        `mplus` (Variable <$> P.identifier)
    where
    lambda = P.symbol "\\" *> P.identifier <* P.symbol "."

inputExpr :: UI UserInput W.AnyWidget
inputExpr =
    runCommand cont $ mapCommand itext (inputParser parseExprHead)
    where
    cont (Lambda vname) = (text defaultAttr ("\\" ++ vname ++ ". ") <++>) <$> inputExpr
    cont Parens = parenify <$> inputExpr
    cont (Variable s) = (text defaultAttr (s ++ " ") <++>) <$> inputExpr
    parenify x = text defaultAttr "(" <++> x <++> text defaultAttr ")"

nothing :: UI UserInput W.AnyWidget
nothing = UI (text defaultAttr "") (const nothing)

(<++>) :: W.AnyWidget -> W.AnyWidget -> W.AnyWidget
x <++> y = W.anyWidget (x W.<++> y)
(<-->) :: W.AnyWidget -> W.AnyWidget -> W.AnyWidget
x <--> y = W.anyWidget (x W.<--> y)

itext :: String -> W.AnyWidget
itext s = text defaultAttr s <++> text highlightAttr " " <++> text defaultAttr " "

text :: W.Attr -> String -> W.AnyWidget
text attr = W.anyWidget . W.text attr

main = runVty inputDecl
