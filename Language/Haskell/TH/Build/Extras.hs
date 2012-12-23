{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, FunctionalDependencies, NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall #-}
module Language.Haskell.TH.Build.Extras where

import Language.Haskell.TH.Build.Convertible
import Language.Haskell.TH.Build.Convertible.Restr
import Language.Haskell.TH.Build.Wrappers
import Language.Haskell.TH
import Control.Monad


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

-- * Sugar


-- | = 'lamE''
(\->) :: (Convertible a [PatQ], Convertible a1 ExpQ) =>
                        a -> a1 -> ExpQ
(\->) = lamE' 

infixr 1 \->

class Arrows a b | a -> b, b -> a where
    arrow :: a -> b -> b 

instance Arrows Exp Pat where arrow = ViewP
instance Arrows Type Type where arrow x y = AppT (AppT ArrowT x) y

(-->) :: (Convertible qa (Q a), Convertible qb (Q b), Arrows a b) =>
                        qa -> qb -> Q b
(-->) = preconvert2 (liftM2 arrow) 

infixr 1 -->

class Sigs a b c | c -> a b, a -> b c where
    signature :: a -> b -> c

(.::) :: (Convertible qa (Q a'), Convertible qb (Q b'), Sigs a' b' c) => qa -> qb -> Q c
(.::) = preconvert2 (liftM2 signature)

infixl 1 .::

instance Sigs Name Type Dec where signature = SigD
instance Sigs Exp Type Exp where signature = SigE
instance Sigs Pat Type Pat where signature = SigP
instance Sigs Type Kind Type where signature = SigT


-- withLocalNames :: ((String -> Q Name) -> Q b) -> Q b
-- withLocalNames f = do
--     ref <- runIO (newIORef M.empty)
--     let mkLocalName s = do
--         m <- runIO (readIORef ref)
--         case M.lookup s m of
--              Just n -> return n
--              Nothing -> do
--                  n <- newName s
--                  runIO (writeIORef ref (M.insert s n m)) 
--                  return n
-- 
--     f mkLocalName





-- * Variants without usually-empty parameters

-- | Value decl without a @where@-clause
svalD
  :: (Convertible patQ PatQ, Convertible bodyQ BodyQ) =>
     patQ -> bodyQ -> DecQ
svalD p e = valD' p e ()

-- | @case@ match without a @where@-clause
smatch
  :: (Convertible patQ PatQ, Convertible bodyQ BodyQ) =>
     patQ -> bodyQ -> MatchQ
smatch p e = match' p e ()

-- | 'Clause' without a @where@-clause
sclause
  :: (Convertible patQs [PatQ], Convertible bodyQ BodyQ) =>
     patQs -> bodyQ -> ClauseQ
sclause p e = clause' p e ()

-- | @data@ decl with no context
sdataD
  :: (Convertible name Name, Convertible tyVarBndrs [TyVarBndr],
      Convertible conQs [ConQ], Convertible names [Name]) =>
     name -> tyVarBndrs -> conQs -> names -> DecQ
sdataD n vars cons derivs = dataD' () n vars cons derivs

-- | @newtype@ decl with no context
snewtypeD
  :: (Convertible name Name, Convertible tyVarBndrs [TyVarBndr],
      Convertible conQ ConQ, Convertible names [Name]) =>
     name -> tyVarBndrs -> conQ -> names -> DecQ
snewtypeD n vars con derivs = newtypeD' () n vars con derivs
