module Builtins where
import qualified Data.Map as M
import TcSyntax
import Syntax hiding (Expr)
import Tc (lit)

builtins :: M.Map Name (Expr, Forall)
builtins = M.fromList $ fmap (\(n, t@(Forall _ (FunT ts _ _))) ->
        (n, (Global n, t)))
    [("numToStr", [lit Num] --> lit Str)
    ,("fracToStr", [lit Frac] --> lit Str)
    ,("numToFrac", [lit Num] --> lit Frac)
    ,("fracToNum", [lit Frac] --> lit Num)
    ,("parseNum", failable $ [lit Str] --> lit Num)
    ,("parseFrac", failable $ [lit Str] --> lit Frac)

    ,("add", bin Num)
    ,("sub", bin Num)
    ,("mul", bin Num)
    ,("div", failable $ bin Num)
    ,("mod", failable $ bin Num)
    
    ,("addF", bin Frac)
    ,("subF", bin Frac)
    ,("mulF", bin Frac)
    ,("divF", bin Frac)

    ,("length", [lit Str] --> lit Num)
    ,("ord", failable $ [lit Str, lit Num] --> lit Num)
    ,("chr", [lit Num] --> lit Str)
    ,("substring", failable $ [lit Str, lit Num, lit Num] --> lit Str)
    ,("setSubstring", failable $ [lit Str, lit Num, lit Num, lit Str] --> lit Str)

    ,("absurd", Forall mempty{bindT=["a"], bindEs=["e"]} .
        FunT [VarT "Void"] (Effs [] "e") $ VarT "a")

    ,("print", console $ [lit Str] --> lit Unit)
    ,("getLine", console $ [] --> lit Str)]

(-->) :: [Type] -> Type -> Forall
xs --> x = Forall mempty{bindEs = ["e"]} $ FunT xs (Effs [] "e") x

effect :: Name -> Forall -> Forall 
effect e (Forall b (FunT xs (Effs es n) x)) = Forall b $ FunT xs (Effs (Eff e [] [] [] : es) n) x
effect _ _ = error "Unexpected builtin"

console :: Forall -> Forall
console = effect "Console"

failable :: Forall -> Forall
failable = effect "Failable"

bin :: (a -> Lit) -> Forall
bin f = [lit f, lit f] --> lit f
