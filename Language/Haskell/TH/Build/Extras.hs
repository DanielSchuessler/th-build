{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall #-}
module Language.Haskell.TH.Build.Extras where

import Language.Haskell.TH.Build.Convertible
import Language.Haskell.TH.Build.Wrappers
import Language.Haskell.TH


(\->) :: (Convertible a [PatQ], Convertible a1 ExpQ) =>
                        a -> a1 -> ExpQ
(\->) = lamE' 

infixr 0 \->

getFieldE :: (Convertible a Name) => 
    a       -- ^ Ctor name
    -> Int  -- ^ Ctor arity
    -> Int  -- ^ 0-based index of field to get
    -> Q Exp
getFieldE ctor n i = do
    x <- newName "_x"
    lamE' 
        (conP (name ctor) (map (\j -> if i==j then varP x else wildP) [0..n-1]))     
        x


htuple' :: Convertible a TypeQ => Int -> a -> TypeQ
htuple' n t = foldl appT (tupleT n) (replicate n (typeQ t))

svalD
  :: (Convertible a1 ExpQ, Convertible a PatQ) => a -> a1 -> DecQ
svalD = preconvert2 (\p e -> valD p (normalB e) [])

smatch
  :: (Convertible a1 ExpQ, Convertible a PatQ) => a -> a1 -> MatchQ
smatch = preconvert2 (\p e -> match p (normalB e) [])

