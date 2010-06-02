module Main where

import Language.Haskell.Exts
import Text.Show.Pretty
import Data.Generics.Uniplate hiding (universe)
import Data.Generics.Uniplate.Data

main :: IO ()
main = do
    spec <- parseFile "spec.hs"

    case spec of
        ParseOk m -> useModule m
        _ -> error "No Parse"

-- Ment to represent (FunBind,TypeSig)
type FunPair = (Decl,Decl)

useModule (Module sl name prags warn exports imports decls)
    = putStrLn $ ppShow prims
    where
        typesigs = [t | t@(TypeSig _ _ _)          <- decls]
        dats     = [d | d@(DataDecl _ _ _ _ _ _ _) <- decls]
        funbinds = [f | f@(FunBind _)              <- decls]
        patbinds = [p | p@(PatBind _ _ _ _ _)      <- decls]
        binds    = funbinds ++ patbinds
        undefs   = filter isUndef binds
        prims    = filter (not . isUndef) binds

isUndef d =
    case d of
        (FunBind [Match _ _ _ _ (UnGuardedRhs r) _]) -> chk r
        (PatBind _ _ _ (UnGuardedRhs r) _) -> chk r
        _ -> False
    where
        chk r = case r of
                    (Var (UnQual (Ident "undefined"))) -> True
                    _ -> False
