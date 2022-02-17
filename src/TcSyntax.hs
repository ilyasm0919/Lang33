module TcSyntax where
import qualified Syntax as S
import Data.List (intercalate)

type Var = S.Name
data Code = Code Var Stmt

data Expr
    = Arg Var
    | Global S.Name
    | Lambda Fun
    | Handler Var Code [(S.Name, Fun)]
    | Lit S.Lit

data Stmt
    = EffCall S.Lift S.Name [Expr] Code
    | Call S.Lift Expr [Expr] Code
    | GetLocal S.Lift Code
    | SetLocal S.Lift Expr Stmt
    | LetLocal Expr Stmt
    | Handle S.Lift Stmt Expr Code
    | EffPure Expr
    | If Expr Stmt Stmt Stmt
    | While Stmt Stmt Stmt

defaultPure :: Var -> Code
defaultPure n = Code n . EffPure $ Arg n

data Fun = Fun [Var] Stmt

instance Show Expr where
    show (Arg v) = v
    show (Global v) = "fun." <> show v
    show (Lambda x) = show x
    show (Handler n p xs) = n <> "{\n" <> show p <> "\n\n" <> intercalate "\n\n" (show <$> xs) <> "\n}"
    show (Lit x) = show x

instance Show Stmt where
    show (EffCall l e xs x) = show l <> ":" <> show e <> "(" <> intercalate "," (show <$> xs) <> ")" <> show x
    show (Call l x xs y) = show l <> ":" <> show x <> "(" <> intercalate "," (show <$> xs) <> ")" <> show y
    show (GetLocal l s) = show l <> ":get" <> "\n" <> show s
    show (SetLocal l x s) = show l <> ":set" <> show x <> "\n" <> show s
    show (LetLocal x s) = "local" <> show x <> "\n" <> show s
    show (Handle l x y z) = show x <> " " <> show l <> ":with " <> show y <> show z
    show (EffPure x) = "eff.pure " <> show x
    show (If c t f x) = "if " <> show c <> " { " <>
        show t <> " } else { " <>
        show f <>  " }\n" <> show x
    show (While c b x) = "while " <> show c <> " { " <>
        show b <> " }\n" <> show x

instance Show Fun where
    show (Fun vs x) = "[" <> intercalate "," vs <> "]{\n" <> show x <> "\n}"

instance Show Code where
    show (Code v x) = "[" <> v <> "]{\n" <> show x <> "\n}"
