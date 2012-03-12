{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS -Wall #-}
module Language.Haskell.TH.Build.Convertible where

import Language.Haskell.TH
import Data.Char

isUpperName :: Name -> Bool
isUpperName = isUpper . head . nameBase

ifUpperThenElse :: (Name -> t) -> (Name -> t) -> Name -> t
ifUpperThenElse ku kl n = (if isUpperName n then ku else kl) n

class Convertible a b where
    convert :: a -> b

expQ :: Convertible a ExpQ => a -> ExpQ
expQ = convert 

expQs :: Convertible a [ ExpQ ] => a -> [ ExpQ ]
expQs = convert 

patQ :: Convertible a PatQ => a -> PatQ
patQ = convert
patQs :: Convertible a [PatQ] => a -> [PatQ]
patQs = convert
typeQ :: Convertible a TypeQ => a -> TypeQ
typeQ = convert
typeQs :: Convertible a [ TypeQ ] => a -> [ TypeQ ]
typeQs = convert
name :: Convertible a Name => a -> Name
name = convert
tyVarBndr :: Convertible a TyVarBndr => a -> TyVarBndr
tyVarBndr = convert

conQ :: Convertible a ConQ => a -> ConQ
conQ = convert

cxtQ :: Convertible a CxtQ => a -> CxtQ
cxtQ = convert

strictTypeQ :: Convertible a StrictTypeQ => a -> StrictTypeQ
strictTypeQ = convert

strictTypeQs :: Convertible a [StrictTypeQ] => a -> [StrictTypeQ]
strictTypeQs = convert

instance Convertible ExpQ ExpQ where convert = id
instance Convertible Name ExpQ where convert = ifUpperThenElse conE varE
instance Convertible String ExpQ where convert = expQ . name
instance Convertible Lit ExpQ where convert = litE 
instance Convertible Integer ExpQ where convert = litE . integerL 

instance Convertible [ ExpQ ] [ ExpQ ] where convert = id
instance Convertible [ Name ] [ ExpQ ] where convert = map expQ
instance Convertible [ String ] [ ExpQ ] where convert = map expQ

instance Convertible PatQ PatQ where convert = id
instance Convertible Name PatQ where convert = ifUpperThenElse (flip conP []) varP
instance Convertible String PatQ where convert = patQ . name


instance Convertible [PatQ] [PatQ] where convert = id
instance Convertible [ Name ] [PatQ] where convert = map patQ
instance Convertible [ String ] [PatQ] where convert = map patQ
instance Convertible PatQ [PatQ] where convert = return
instance Convertible Name [PatQ] where convert = return . patQ
instance Convertible String [PatQ] where convert = return . patQ


instance Convertible TypeQ TypeQ where convert = id
instance Convertible Name TypeQ where convert = ifUpperThenElse conT varT
instance Convertible String TypeQ where convert = typeQ . name

instance Convertible [TypeQ] [TypeQ] where convert = id
instance Convertible TypeQ [TypeQ] where convert = return
instance Convertible Name [TypeQ] where convert = return . typeQ
instance Convertible String [TypeQ] where convert = return . typeQ

instance Convertible Name Name where convert = id
instance Convertible String Name where convert = mkName


instance Convertible TyVarBndr TyVarBndr where convert = id
instance Convertible Name TyVarBndr where convert = PlainTV
instance Convertible String TyVarBndr where convert = tyVarBndr . name

instance Convertible ConQ ConQ where convert = id

instance Convertible CxtQ CxtQ where convert = id
instance Convertible [PredQ] CxtQ where convert = sequence

instance Convertible StrictTypeQ StrictTypeQ where convert = id
instance Convertible TypeQ StrictTypeQ where convert = strictType notStrict
instance Convertible Name StrictTypeQ where convert = strictTypeQ . typeQ
instance Convertible String StrictTypeQ where convert = strictTypeQ . typeQ

instance Convertible [ StrictTypeQ ] [ StrictTypeQ ] where convert = id
instance Convertible StrictTypeQ [StrictTypeQ] where convert = return
instance Convertible TypeQ [StrictTypeQ] where convert = return . strictTypeQ
instance Convertible Name [StrictTypeQ] where convert = return . strictTypeQ
instance Convertible String [StrictTypeQ] where convert = return . strictTypeQ 

instance Convertible [ DecQ ] [ DecQ ] where convert = id
instance Convertible DecQ [ DecQ ] where convert = return

(&) ::  Convertible a1 a => a1 -> [a] -> [a]
a & b = convert a : b
infixr 5 &

preconvert1 :: Convertible a b => (b -> c) -> a -> c
preconvert1 = (. convert) 

preconvert2
  :: (Convertible a1 b, Convertible a b1) =>
     (b1 -> b -> c) -> a -> a1 -> c
preconvert2 f = preconvert1 . preconvert1 f

preconvert3
  :: (Convertible a1 b, Convertible a2 b1, Convertible a b2) =>
     (b2 -> b1 -> b -> c) -> a -> a2 -> a1 -> c
preconvert3 f = preconvert2 . preconvert1 f

preconvert4
  :: (Convertible a1 b,
      Convertible a2 b1,
      Convertible a3 b2,
      Convertible a b3) =>
     (b3 -> b2 -> b1 -> b -> c) -> a -> a3 -> a2 -> a1 -> c
preconvert4 f = preconvert3 . preconvert1 f

