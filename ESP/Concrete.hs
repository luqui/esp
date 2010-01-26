module ESP.Concrete (ConcreteExp, app, lam, var, hole, let_, highlight, getWidget) where

import qualified Graphics.Vty.Widgets.All as W
import ESP.WidgetUtils

data Prec = PSimple | PApp | PLam
    deriving (Eq,Ord)

data ConcreteExp = ConcreteExp Prec W.AnyWidget

app :: ConcreteExp -> ConcreteExp -> ConcreteExp
app t u = ConcreteExp PApp ((parensC PApp t <++> text defaultAttr " ") <++> parensC PSimple u)

lam :: String -> ConcreteExp -> ConcreteExp
lam v t = ConcreteExp PLam (text defaultAttr ("\\" ++ v ++ ". ") <++> parensC PLam t)

var :: String -> ConcreteExp
var v = ConcreteExp PSimple (text defaultAttr v)

hole :: ConcreteExp
hole = ConcreteExp PSimple (text defaultAttr "[]")

let_ :: String -> ConcreteExp -> ConcreteExp -> ConcreteExp
let_ var t body = ConcreteExp PLam $
    (text defaultAttr ("let " ++ var ++ " = ") <++> parensC PLam t <++> text defaultAttr " in")
                        <--> parensC PLam body

highlight :: ConcreteExp -> ConcreteExp
highlight (ConcreteExp c w) = ConcreteExp c (W.withAttribute w highlightAttr)

parensC c (ConcreteExp c' t)
    | c' <= c = t
    | otherwise = text defaultAttr "(" <++> t <++> text defaultAttr ")"

getWidget :: ConcreteExp -> W.AnyWidget
getWidget (ConcreteExp _ w) = w
