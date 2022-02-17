module Syntax where
import Data.List (nub)

type Name = String

type Lift = Int

data Arg = Arg Name (Maybe Type) deriving Show

data Lit = Str String | Num Integer | Frac Double | Unit ()

instance Show Lit where
    show (Str x) = show x
    show (Num x) = show x
    show (Frac x) = show x
    show (Unit x) = "null"

data Expr
    = Var Name
    | Call Expr Lift [Expr]
    | EffCall Name [Expr]
    | Lambda [Arg] Body
    | Handler Name [Fun]
    | Handle Expr Expr
    | Lit Lit
    deriving Show

data Stmt
    = LetS Name Expr
    | IfS Expr Body Body
    | WhileS Expr Body
    | ExprS Expr
    deriving Show

type Body = [Stmt]

data Type
    = FunT [Type] Effs Type
    | VarT Name
    | ConT Name
    | HandlerT Eff Type Effs Type
    deriving Show

data Bind = Bind { bindT :: [Name], bindE :: [Name], bindEs :: [Name] } deriving Show

instance Semigroup Bind where
    Bind a b c <> Bind x y z = Bind (nub $ a <> x) (nub $ b <> y) (nub $ c <> z)

instance Monoid Bind where
    mempty = Bind [] [] []

data Forall = Forall Bind Type deriving Show

data Eff = EffVar Name | Eff Name [Type] [Eff] [Effs] deriving Show

data Effs = Effs [Eff] Name deriving Show

data Fun = Fun Name [Arg] Body deriving Show

data Effect = Effect Name [Type] Type deriving Show -- Polymorphism currently is not supported

data Interface = Interf Name Bind [Effect] deriving Show

data Decl
    = Function Fun
    | Interface Interface
    deriving Show
