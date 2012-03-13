{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS -Wall #-}
module Language.Haskell.TH.Build.Convertible where

import Language.Haskell.TH
import Data.Char
import Control.Monad

isUpperName :: Name -> Bool
isUpperName = liftM2 (||) (==':') isUpper . head . nameBase

ifUpperThenElse :: (Name -> t) -> (Name -> t) -> Name -> t
ifUpperThenElse ku kl n = (if isUpperName n then ku else kl) n

class Convertible a b where
    convert :: a -> b


-- instance Convertible ExpQ ExpQ where convert = id
-- instance Convertible [ ExpQ ] [ ExpQ ] where convert = id
-- instance Convertible [ StrictTypeQ ] [ StrictTypeQ ] where convert = id
-- instance Convertible DecsQ DecsQ where convert = id
-- instance Convertible [ DecQ ] [ DecQ ] where convert = id
-- instance Convertible [PatQ] [PatQ] where convert = id
-- instance Convertible TypeQ TypeQ where convert = id
-- instance Convertible [ TypeQ ] [TypeQ] where convert = id
-- instance Convertible Name Name where convert = id
-- instance Convertible TyVarBndr TyVarBndr where convert = id
-- instance Convertible ConQ ConQ where convert = id
-- instance Convertible CxtQ CxtQ where convert = id
-- instance Convertible StrictTypeQ StrictTypeQ where convert = id

#define TRANS(A,B,C) instance Convertible (A) (C) where convert = (convert :: (B) -> (C)) . (convert :: (A) -> (B))
#define MAP(A,C) instance Convertible (A) (C) where convert = map convert
#define SINGLETON(A) -- instance Convertible (A) [A] where convert = return

instance Convertible a a where convert = id
-- | Singleton
instance Convertible a [a] where convert = return
-- instance Convertible a b => Convertible a [b] where convert = return . convert 
-- | Empty list
instance Convertible () [a] where convert = const mzero
-- | Empty list
instance Convertible () (Q [a]) where convert = const (return mzero)
-- | 'Nothing'
instance Convertible () (Maybe a) where convert = const mzero
-- | 'Nothing'
instance Convertible () (Q (Maybe a)) where convert = const (return mzero)

instance Convertible Integer Lit where convert = integerL 


-- | 'conE' or 'varE', determined by capitalization.
instance Convertible Name ExpQ where convert = ifUpperThenElse conE varE
-- | 'conE' or 'varE', determined by capitalization.
instance Convertible String Name where convert = mkName
instance Convertible Lit ExpQ where convert = litE 
instance Convertible RangeQ ExpQ where convert = arithSeqE
TRANS(String,Name,ExpQ)
TRANS(Integer,Lit,ExpQ)

MAP([ Name ],[ ExpQ ])
MAP([ String ],[ ExpQ ])
MAP([ Lit ],[ ExpQ ])
MAP([ Integer ],[ ExpQ ])
MAP([ RangeQ ],[ ExpQ ])

-- | 'conP' or 'varP', determined by capitalization.
instance Convertible Name PatQ where convert = ifUpperThenElse (flip conP []) varP
-- | 'conP' or 'varP', determined by capitalization.
TRANS(String,Name,PatQ)

MAP([ Name ],[PatQ])
MAP([ String ],[PatQ])
SINGLETON(PatQ)
TRANS(Name,PatQ,[PatQ])
TRANS(String,PatQ,[PatQ])

-- | 'conT' or 'varT', determined by capitalization.
instance Convertible Name TypeQ where convert = ifUpperThenElse conT varT
-- | 'conT' or 'varT', determined by capitalization.
TRANS(String,Name,TypeQ)

MAP([ Name ],[TypeQ])
MAP([ String ],[TypeQ])
SINGLETON(TypeQ)
TRANS(Name,TypeQ,[TypeQ])
TRANS(String,TypeQ,[TypeQ])


instance Convertible Name TyVarBndr where convert = PlainTV
TRANS(String,Name,TyVarBndr)

SINGLETON(TyVarBndr)
TRANS(Name,TyVarBndr,[TyVarBndr])


instance Convertible [PredQ] CxtQ where convert = sequence

-- | Uses 'NotStrict'.
instance Convertible TypeQ StrictTypeQ where convert = strictType notStrict
TRANS(Name,TypeQ,StrictTypeQ)
TRANS(String,TypeQ,StrictTypeQ)

SINGLETON(StrictTypeQ)
TRANS(TypeQ,StrictTypeQ,[StrictTypeQ])
TRANS(Name,StrictTypeQ,[StrictTypeQ])
TRANS(String,StrictTypeQ,[StrictTypeQ])

instance Convertible [ DecQ ] DecsQ where convert = sequence
instance Convertible DecQ DecsQ where convert = fmap return
instance Convertible [DecsQ] DecsQ where convert = fmap join . sequence

SINGLETON(DecQ)


instance Convertible ExpQ BodyQ where convert = normalB 
TRANS(Name,ExpQ,BodyQ)
TRANS(String,ExpQ,BodyQ)
TRANS(Lit,ExpQ,BodyQ)
TRANS(Integer,ExpQ,BodyQ)
TRANS(RangeQ,ExpQ,BodyQ)


#undef MAP
#undef TRANS

(&) ::  Convertible a1 a => a1 -> [a] -> [a]
a & b = convert a : b
infixr 5 &



-- * Type restrictions of 'convert'
#define MAKE_CONVERT_TO(N,T) N :: Convertible a T => a -> T; N = convert

MAKE_CONVERT_TO(expQ,ExpQ)
MAKE_CONVERT_TO(expQs,[ ExpQ ])
MAKE_CONVERT_TO(patQ,PatQ)
MAKE_CONVERT_TO(patQs,[PatQ])
MAKE_CONVERT_TO(typeQ,TypeQ)
MAKE_CONVERT_TO(typeQs,[ TypeQ ])
MAKE_CONVERT_TO(name,Name)
MAKE_CONVERT_TO(tyVarBndr,TyVarBndr)
MAKE_CONVERT_TO(conQ,ConQ)
MAKE_CONVERT_TO(cxtQ,CxtQ)
MAKE_CONVERT_TO(strictTypeQ,StrictTypeQ)
MAKE_CONVERT_TO(strictTypeQs,[StrictTypeQ])
MAKE_CONVERT_TO(decsQ,DecsQ)

#undef MAKE_CONVERT_TO

-- * Function transformers
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

preconvert5
  :: (Convertible a1 b, Convertible a2 b1, Convertible a3 b2,
      Convertible a4 b3, Convertible a b4) =>
     (b4 -> b3 -> b2 -> b1 -> b -> c) -> a -> a4 -> a3 -> a2 -> a1 -> c
preconvert5 f = preconvert4 . preconvert1 f
preconvert6
  :: (Convertible a1 b, Convertible a2 b1, Convertible a3 b2,
      Convertible a4 b3, Convertible a5 b4, Convertible a b5) =>
     (b5 -> b4 -> b3 -> b2 -> b1 -> b -> c)
     -> a -> a5 -> a4 -> a3 -> a2 -> a1 -> c
preconvert6 f = preconvert5 . preconvert1 f
preconvert7
  :: (Convertible a1 b, Convertible a2 b1, Convertible a3 b2,
      Convertible a4 b3, Convertible a5 b4, Convertible a6 b5,
      Convertible a b6) =>
     (b6 -> b5 -> b4 -> b3 -> b2 -> b1 -> b -> c)
     -> a -> a6 -> a5 -> a4 -> a3 -> a2 -> a1 -> c
preconvert7 f = preconvert6 . preconvert1 f
