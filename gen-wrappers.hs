import Language.Haskell.Exts
import System.Process
import Control.Exception
import Debug.Trace

main = do
    bro <- readProcess "ghc" [
                "-e",":m Language.Haskell.TH.Lib Language.Haskell.TH",
                "-e",":bro  Language.Haskell.TH.Lib"] ""

    putStrLn (process bro)


process bro = 
    case parseModule bro of
         ParseOk mod -> 
            unlines ( 
                "module Language.Haskell.TH.Build.Wrappers where" :
                "import Language.Haskell.TH.Build.Convertible" :
                processModule mod
                )
            
         e -> error ("Failed to parse :bro output: " ++ show e)


noSrcLoc = undefined

processModule (Module _ _ _ _ _ _ decls) = concatMap processDecl decls 


processDecl (TypeSig _ [n] t)
    | arity t == 0 = []
    | otherwise = 
        case n of
            Ident n' -> [n' ++ "' = preconvert"++show (arity t)++" "++n'] 
            _ -> trace ("Ignoring operator "++show n) []

processDecl (TypeSig _ _ _) = assert False undefined
processDecl _ = []

arity (TyFun a b) = 1 + arity b 
arity _ = 0
    
