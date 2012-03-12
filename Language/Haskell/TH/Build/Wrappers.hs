{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}
module Language.Haskell.TH.Build.Wrappers where

import Language.Haskell.TH.Build.Convertible
import Language.Haskell.TH


lamE'
  :: (Convertible a [PatQ], Convertible a1 ExpQ) => a -> a1 -> ExpQ
lamE' = preconvert2 lamE

appT'
  :: (Convertible a TypeQ, Convertible a1 TypeQ) => a -> a1 -> TypeQ
appT' = preconvert2 appT

newtypeD'
  :: (Convertible a CxtQ,
      Convertible a1 Name,
      Convertible a2 ConQ) =>
     a -> a1 -> [TyVarBndr] -> a2 -> [Name] -> DecQ
newtypeD' c na bndrs con derivs = newtypeD (cxtQ c) (name na) bndrs (conQ con) derivs

normalC'
  :: (Convertible a Name, Convertible a1 [StrictTypeQ]) =>
     a -> a1 -> ConQ
normalC' na _strictTypeQs = normalC (name na) (strictTypeQs _strictTypeQs)

appE'
  :: (Convertible a ExpQ, Convertible a1 ExpQ) => a -> a1 -> ExpQ
appE' x y = expQ x `appE` expQ y


tupE' :: Convertible a [ExpQ] => a -> ExpQ
tupE' = preconvert1 tupE

tupP' :: Convertible a [PatQ] => a -> PatQ
tupP' = preconvert1 tupP




conP'
  :: (Convertible a Name, Convertible a1 [PatQ]) => a -> a1 -> PatQ
conP' = preconvert2 conP

classP'
  :: (Convertible a Name, Convertible a1 [TypeQ]) => a -> a1 -> PredQ
classP' = preconvert2 classP

tySynInstD'
  :: (Convertible a Name,
      Convertible a1 [TypeQ],
      Convertible a2 TypeQ) =>
     a -> a1 -> a2 -> DecQ
tySynInstD' = preconvert3 tySynInstD

listE' :: Convertible a [ExpQ] => a -> ExpQ
listE' = preconvert1 listE

instanceD'
  :: (Convertible a1 [DecQ],
      Convertible a2 TypeQ,
      Convertible a CxtQ) =>
     a -> a2 -> a1 -> DecQ
instanceD' = preconvert3 instanceD



