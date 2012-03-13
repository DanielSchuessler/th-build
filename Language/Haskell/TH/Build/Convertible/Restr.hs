{-# LANGUAGE CPP, FlexibleContexts #-}
-- | Type restrictions of 'convert'
module Language.Haskell.TH.Build.Convertible.Restr where

import Language.Haskell.TH
import Language.Haskell.TH.Build.Convertible

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

