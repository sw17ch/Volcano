module Main where

import Data.List
import Data.Maybe
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

-- Ment to represent (Bind,Type)
type FunPair = (Decl, Type)

useModule (Module sl name prags warn exports imports decls)
    = do
        putStrLn "Undefined:"
        putStrLn $ ppShow $ mkPairs undefs
        putStrLn "\nPrimitives:"
        putStrLn $ ppShow $ mkPairs prims
    where
        typesigs = [t | t@(TypeSig _ _ _)          <- decls]
        dats     = [d | d@(DataDecl _ _ _ _ _ _ _) <- decls]
        funbinds = [f | f@(FunBind _)              <- decls]
        patbinds = [p | p@(PatBind _ _ _ _ _)      <- decls]
        binds    = funbinds ++ patbinds
        undefs   = filter isUndef binds
        prims    = filter (not . isUndef) binds

        -- Pairs defs with types found in type signatures
        mkPairs ds = zip ds $ map ((findType typesigs) . declName) ds

isUndef d =
    case d of
        (FunBind [Match _ _ _ _ (UnGuardedRhs r) _]) -> chk r
        (PatBind _ _ _ (UnGuardedRhs r) _) -> chk r
        _ -> False
    where
        chk r = case r of
                    (Var (UnQual (Ident "undefined"))) -> True
                    _ -> False


-- Expects a list of type signatures.
findType :: [Decl] -> String -> Type
findType ts n = case find (hasName n) ts of
                    Just (TypeSig _ _ t) -> t
                    Nothing -> error $ "Unable to find a type for " ++ n
    where ts' = [t | t@(TypeSig _ _ _) <- ts]
          hasName n (TypeSig _ ns _) = (Ident n) `elem` ns

declName :: Decl -> String
declName (PatBind _ (PVar (Ident n)) _ _ _) = n
declName (FunBind ms) = let getN (Match _ (Ident n) _ _ _ _) = n
                        in  head (map getN ms) -- I think the name will always be the same

matchBind :: [Decl] -> Decl -> FunPair
matchBind typesigs bind = let bn = declName bind
                              ts = findType typesigs bn
                          in (bind,ts)
