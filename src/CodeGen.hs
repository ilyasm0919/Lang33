module CodeGen where
import TcSyntax as S (Fun(..), Stmt(..), Expr(..), Code(..))
import Language.Haskell.Exts as E hiding (Lambda, Name)
import Syntax as S (Name, Lit (..), Lift, Interface (Interf), Bind(..), Effect (Effect), Type(..), Effs (Effs), Eff(..), Forall (Forall))
import Language.Haskell.Exts (qualStmt)

compileProgram :: [Interface] -> [(Name, Forall, Fun)] -> Module ()
compileProgram is fs = Module () Nothing
    [LanguagePragma () $ name . prettyExtension <$>
        [EnableExtension LambdaCase
        ,EnableExtension GADTs
        ,DisableExtension ImplicitPrelude]]
    [ImportDecl
        { importAnn=()
        , importModule=ModuleName () "L33"
        , importQualified=False
        , importSrc=False
        , importSafe=False
        , importPkg=Nothing
        , importAs=Nothing
        , importSpecs=Nothing}] $
    fmap compileInterface is <> concatMap compileFun fs

compileInterface :: Interface -> Decl ()
compileInterface (Interf n b es) =
    let ns = (bindT <> bindE <> bindEs) b in
    GDataDecl () (DataType ()) Nothing
    (foldl (DHApp ()) (DHead () $ name n) $
        UnkindedVar () . name <$> ns <> ["_res"])
    Nothing (compileEffect n ns <$> es) []

compileEffect :: Name -> [Name] -> Effect -> GadtDecl ()
compileEffect m ns (Effect n xs x) =
    GadtDecl () (name n) Nothing Nothing Nothing .
    foldr (TyFun ()) (foldl (TyApp ()) (TyCon () . UnQual () $ name m) $
        fmap (TyVar () . name) ns <> [compileType x]) $
    compileType <$> xs

compileType :: S.Type -> E.Type ()
compileType (FunT xs e x) =
    foldr (TyFun ()) (TyApp () (compileEffs e) $ compileType x) $
    compileType <$> xs
compileType (VarT n) = TyVar () $ name n
compileType (ConT n) = TyCon () . UnQual () $ name n
compileType (HandlerT e x es y) =
    foldl (TyApp ()) (TyCon () . UnQual () $ name "Handler")
    [compileEff e, compileType x, compileEffs es, compileType y]

compileEff :: Eff -> E.Type ()
compileEff (EffVar n) = TyVar () $ name n
compileEff (Eff n xs e es) = foldl (TyApp ()) (TyCon () . UnQual () $ name n) $
    fmap compileType xs <> fmap compileEff e <> fmap compileEffs es

compileEffs :: Effs -> E.Type ()
compileEffs (Effs es n) =
    foldr (TyApp () . TyApp () (TyCon () . UnQual () $ name "Eff"))
    (TyVar () $ name n) $ compileEff <$> es

compileFun :: (Name, Forall, Fun) -> [Decl ()]
compileFun ("main", _, Fun [] e) =
    [TypeSig () [name "main"] . TyForall () Nothing Nothing .
            TyApp () (TyCon () . UnQual () $ name "IO") $ TyTuple () Boxed []
    ,sfun (name "main") [] (UnGuardedRhs () . metaFunction "runMain" . pure
        $ compileBody e) Nothing]
compileFun (n, Forall b t, Fun vs e) =
    [TypeSig () [name n] . TyForall () Nothing (Just . CxTuple () .
        fmap (TypeA () . TyApp () (TyCon () . UnQual () $ name "Monad")
            . TyVar () . name) $ bindEs b) $ compileType t
    ,sfun (name n) (name <$> vs) (UnGuardedRhs () $ compileBody e) Nothing]

compileBody :: S.Stmt -> Exp ()
compileBody = doE . compileStmt

compileStmt :: S.Stmt -> [E.Stmt ()]
compileStmt (EffCall l n xs c) = lift l (metaFunction "eff"
    [Con () (UnQual () $ name n) `appFun` fmap compileExpr xs]) `with` c
compileStmt (Call l x xs c) =
    lift l (compileExpr x `appFun` fmap compileExpr xs) `with` c
compileStmt (GetLocal l x) = lift l (metaFunction "get" []) `with` x
compileStmt (SetLocal l e x) =
    qualStmt (lift l $ metaFunction "put" [compileExpr e]) : compileStmt x
compileStmt (LetLocal e x) =
    [qualStmt $ metaFunction "evalStateT" [compileBody x, compileExpr e]]
compileStmt (Handle l s x c) = lift l (compileExpr x `app` compileBody s) `with` c
compileStmt (EffPure x) = pure . qualStmt $ metaFunction "pure" [compileExpr x]
compileStmt (S.If x t f c) = qualStmt (E.If () (metaFunction "cond" [compileExpr x])
    (compileBody t) (compileBody f)) : compileStmt c
compileStmt (While x b c) =
    qualStmt (metaFunction "while" [compileBody x, compileBody b]) : compileStmt c

with :: Exp () -> Code -> [E.Stmt ()]
with e (Code n x) = genStmt (pvar $ name n) e : compileStmt x

lift :: S.Lift -> Exp () -> Exp ()
lift 0 e = e
lift l e = lift (l - 1) $ metaFunction "lift" [e]

compileExpr :: Expr -> Exp ()
compileExpr (Arg n) = var $ name n
compileExpr (Global n) = function n
compileExpr (Lambda (Fun vs e)) = lamE (pvar . name <$> vs) $ compileBody e
compileExpr (Handler n (Code p e) xs) = metaFunction "handle"
    [lamE [pvar $ name n] . LCase () $
        alt (metaConPat "Pure" [pvar $ name p]) (compileBody e) :
        fmap (\(n, Fun ns x) ->
            alt (metaConPat "Eff"
                [metaConPat n $ pvar . name <$> init ns
                ,pvar . name $ last ns]) $ compileBody x) xs]
compileExpr (S.Lit l) = compileLit l

compileLit :: Lit -> Exp ()
compileLit (Num x) = intE x
compileLit (S.Frac x) = E.Lit () $ E.Frac () (toRational x) (show x)
compileLit (Str x) = strE x
compileLit (Unit ()) = tuple []
