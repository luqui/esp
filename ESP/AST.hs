module ESP.AST (AST(..), alphaConvert, freeVars, substitute, betaExpand) where

import qualified Data.Set as Set

data AST 
    = AApp AST AST
    | ALam String AST
    | AVar String
    | AHole
    | ALet String AST AST
    deriving (Show)

alphaConvert :: String -> String -> AST -> AST
alphaConvert from to t | from == to = t
alphaConvert from to (AApp t u) = AApp (alphaConvert from to t) (alphaConvert from to u)
alphaConvert from to (ALam v t)
    | v == from = ALam v t
    | v == to && from `Set.member` freeVars t  = 
        let v' = primeId (freeVars t) v in 
        alphaConvert from to (ALam v' (alphaConvert v v' t))
    | v == to = ALam v t
    | otherwise = ALam v (alphaConvert from to t)
alphaConvert from to (AVar v)
    | v == to = AVar ("*!" ++ v ++ "!*")  -- indicates that we have captured this variable
    | v == from = AVar to
    | otherwise = AVar v
alphaConvert from to AHole = AHole
alphaConvert from to (ALet v t body) = 
    let ALam v' body' = alphaConvert from to (ALam v body)
    in ALet v' (alphaConvert from to t) body'

freeVars :: AST -> Set.Set String
freeVars (AApp t u) = freeVars t `Set.union` freeVars u
freeVars (ALam v t) = Set.delete v (freeVars t)
freeVars (AVar v) = Set.singleton v
freeVars AHole = Set.empty
freeVars (ALet v t body) = Set.delete v (freeVars body) `Set.union` freeVars t

primeId :: Set.Set String -> String -> String
primeId frees x | x `Set.member` frees = primeId frees (x ++ "'")
                | otherwise = x

substitute :: String -> AST -> AST -> AST
substitute var to (AApp t u) = AApp (substitute var to t) (substitute var to u)
substitute var to (ALam v t) 
    | var == v = ALam v t
    | v `Set.member` freeVars to = 
        let v' = primeId (freeVars to `Set.union` freeVars t) v in
        substitute var to (ALam v' (alphaConvert v v' t))
    | otherwise = ALam v (substitute var to t)
substitute var to (AVar v)
    | var == v = to
    | otherwise = AVar v
substitute var to AHole = AHole
substitute var to (ALet v t body) =
    let ALam v' body' = substitute var to (ALam v body)
    in ALet v' (substitute var to t) body'

betaExpand :: AST -> AST
betaExpand (AApp (ALam v t) u) = substitute v u t
betaExpand t = t

