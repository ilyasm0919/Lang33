{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module Tc where
import Syntax
import qualified TcSyntax as S
import qualified Data.Map as M
import Control.Applicative (liftA2, Alternative ((<|>), empty))
import Data.Maybe (maybeToList, fromMaybe, listToMaybe, fromJust)
import Control.Monad (join, ap, when, zipWithM_, foldM, zipWithM, replicateM)
import Data.Functor (($>))
import Data.Bifunctor (Bifunctor(first, bimap))
import Data.List (partition, uncons, sortOn, findIndex, find)
import Data.Foldable (for_, traverse_)
import Data.Traversable (for)
import Control.Monad.State (StateT(..), modify, evalStateT, state, lift)
import Data.Either (partitionEithers, rights)
import Control.Monad.Except (throwError)

type Uniq = Integer
data Env = Env { effects :: M.Map Name Interface, locals :: M.Map Name (Int, Type), vars :: M.Map Name (S.Expr, Forall), uniq :: Uniq }

type EId = Integer
type Deep = Int
data EQ = Type :=: Type | Eff :~: Eff | Effs :~~: Effs | (EId, Eff) :<: Effs deriving Show

newtype Tc a = Tc
    { runTc :: M.Map EId Deep -> Env ->
        Either String (a, S.Stmt -> S.Stmt, [EQ], Either [(EId, Eff)] Effs, Env)
    } deriving Functor

instance Applicative Tc where
    pure x = Tc $ \_ e -> pure (x, id, empty, Left empty, e)
    (<*>) = ap

instance Monad Tc where
    tx >>= f = Tc $ \ns e -> do
        (x, fx, eqsx, esx, ex) <- runTc tx ns e
        (y, fy, eqsy, esy, ey) <- runTc (f x) ns ex
        let (eqs, es) = case (esx, esy) of
                (Left xs, Left ys) -> ([], Left $ xs <> ys)
                (Left xs, Right ys) -> ((:<: ys) <$> xs, Right ys)
                (Right xs, Left ys) -> ((:<: xs) <$> ys, Right xs)
                (Right xs, Right ys) -> ([xs :~~: ys], Right xs)
        pure (y
            , fx . fy
            , eqs <> eqsx <> eqsy
            , es
            , ey)

type TUnifier = M.Map Name Type
type EUnifier = M.Map Name Eff
type EsUnifier = M.Map Name Effs
data Unifier = Unifier { unifT :: TUnifier, unifE :: EUnifier, unifEs :: EsUnifier } deriving Show

instance Semigroup Unifier where
    x <> y = Unifier
        { unifT=(subsitute y <$> unifT x) <> unifT y
        , unifE=(subsituteE y <$> unifE x) <> unifE y
        , unifEs=(subsituteEs y <$> unifEs x) <> unifEs y }

instance Monoid Unifier where
    mempty = Unifier mempty mempty mempty

subsitute :: Unifier -> Type -> Type
subsitute u (FunT xs e x) = FunT (subsitute u <$> xs) (subsituteEs u e) (subsitute u x)
subsitute u (VarT n) = fromMaybe (VarT n) $ unifT u M.!? n
subsitute u (ConT n) = ConT n
subsitute u (HandlerT e x es y) =
    HandlerT (subsituteE u e) (subsitute u x) (subsituteEs u es) (subsitute u y)

subsituteEs :: Unifier -> Effs -> Effs
subsituteEs u (Effs xs n) = Effs (subsituteE u <$> xs <> ys) m
    where Effs ys m = fromMaybe (Effs [] n) $ unifEs u M.!? n

subsituteE :: Unifier -> Eff -> Eff
subsituteE u (EffVar n) = fromMaybe (EffVar n) $ unifE u M.!? n
subsituteE u (Eff n t e es) = Eff n (subsitute u <$> t) (subsituteE u <$> e) (subsituteEs u <$> es)

subsituteEQ :: Unifier -> EQ -> EQ
subsituteEQ u (x :=:  y) = subsitute   u x :=:  subsitute   u y
subsituteEQ u (x :~:  y) = subsituteE  u x :~:  subsituteE  u y
subsituteEQ u (x :~~: y) = subsituteEs u x :~~: subsituteEs u y
subsituteEQ u (x :<:  y) = (subsituteE u <$> x) :<: subsituteEs u y

swap :: (a, (b, c)) -> ((a, b), c)
swap (a, (b, c)) = ((a, b), c)

generalize :: Type -> Forall
generalize = free >>= Forall

free :: Type -> Bind
free (FunT xs es x) = foldMap free xs <> freeEs es <> free x
free (VarT n) = mempty{bindT = [n]}
free (ConT m) = mempty
free (HandlerT e from es to) = freeE e <> free from <> freeEs es <> free to

freeE :: Eff -> Bind
freeE (EffVar n) = mempty{bindE = [n]}
freeE (Eff _ xs e es) = foldMap free xs <> foldMap freeE e <> foldMap freeEs es

freeEs :: Effs -> Bind
freeEs (Effs es n) = mempty{bindEs = [n]} <> foldMap freeE es

getEnv :: Tc Env
getEnv = Tc $ \_ e -> pure (e, id, empty, Left empty, e)

putEnv :: Env -> Tc ()
putEnv e = Tc $ \_ _ -> pure (mempty, id, empty, Left empty, e)

freezeVars :: Tc a -> Tc a
freezeVars m = Tc $ \ns env -> (\(x, f, eqs, es, env') ->
    (x, f, eqs, es, env {uniq=uniq env'})) <$> runTc m ns env

freshed :: Tc String
freshed = do
    e <- getEnv
    putEnv e{uniq = succ $ uniq e}
    pure . ('_':) . show $ uniq e

err :: String -> Tc a
err = Tc . pure . pure . Left

tc :: Expr -> Tc (S.Expr, Type)
tc (Var n) = tcVar n
tc (Call x l xs) = join $ call l <$> tc x <*> traverse tc xs
tc (EffCall n xs) = traverse tc xs >>= effCall n
tc (Lambda as x) = freezeVars $ lambda <$> traverse tcArg as <*> tcBody x
tc (Handler n xs) = buildHandlerT n xs
tc (Handle x y) = join $ handle <$> tcBody [ExprS x] <*> tc y
tc (Lit x) = pure (S.Lit x, ConT $ litT x)

handle :: (S.Stmt, Effs, Type) -> (S.Expr, Type) -> Tc (S.Expr, Type)
handle (xe, es, x) (ye, y) = do
    m <- freshed
    e <- EffVar <$> freshed
    n <- freshed
    to <- VarT <$> freshed
    eq $ es :~~: Effs [e] n
    eq $ y :=: HandlerT e x (Effs [] n) to
    eff $ Effs [] n
    l <- M.size . locals <$> getEnv
    post $ S.Handle l xe ye . S.Code m
    return (S.Arg m, to)

tcFun :: Fun -> Tc (Name, (S.Fun, Type))
tcFun (Fun n as b) = freezeVars $
    (\as (e, es, t) -> (n, (S.Fun (fst <$> as) e, FunT (snd <$> as) es t)))
    <$> traverse tcArg as <*> tcBody b

buildHandlerT :: Name -> [Fun] -> Tc (S.Expr, Type)
buildHandlerT h xs = freezeVars $ do
    res <- VarT <$> freshed
    getEnv >>= \env -> putEnv env{vars = M.insert h (S.Arg h, Forall mempty res) $ vars env}
    (ps, sortOn fst -> fs) <- partition ((== "pure") . fst) <$> traverse tcFun xs
    (p, from, es, to) <- case ps of
        [] -> do
            n <- freshed
            t <- VarT <$> freshed
            es <- Effs [] <$> freshed
            pure (S.defaultPure n, t, es, t)
        [(_, (S.Fun [p] m, FunT [from] es to))] -> pure (S.Code p m, from, es, to)
        [_] -> err "Incorrect pure"
        _ -> err "Multiple declaration of pure"
    (Interf n b fs', is) <- for fs (\(n, _) ->
        getEnv >>= maybe (err $ "Unknown effect: " <> n) pure . M.lookup n . effects)
        >>= maybe (err "Empty handler") pure . uncons
    for_ is $ \(Interf m _ _) -> when (m /= n) $ err "Different effects"
    when (length fs /= length fs') $ err "Effects mismatch"
    u <- unifier b
    let e = Eff n
            ((unifT u M.!) <$> bindT b)
            ((unifE u M.!) <$> bindE b)
            ((unifEs u M.!) <$> bindEs b)
        e_es = case es of Effs es m -> Effs (e : es) m
    zipWithM_ (\(m', (_, FunT ts' es' t')) (Effect m ts t) -> do
        when (m /= m') $ err "Effects mismatch"
        when (length ts' /= length ts + 1) . err $ "Incorrect effect: " <> m
        traverse eq . zipWith (:=:) ts' $ (subsitute u <$> ts) <> [FunT [t] e_es from]
        eq $ es :~~: es'
        eq $ t' :=: subsitute u to) fs fs'
    eq $ res :=: HandlerT e from es to
    pure (S.Handler h p $ fmap fst <$> fs, res)

litT :: Lit -> String
litT (Str _) = "Str"
litT (Num _) = "Num"
litT (Frac _) = "Frac"
litT (Unit _) = "Unit"

lit :: (a -> Lit) -> Type
lit f = ConT . litT $ f undefined

lambda :: [(Name, Type)] -> (S.Stmt, Effs, Type) -> (S.Expr, Type)
lambda as (e, es, t) =
    (S.Lambda $ S.Fun (fst <$> as) e, FunT (snd <$> as) es t)

tcArg :: Arg -> Tc (Name, Type)
tcArg (Arg n t) = do
    t' <- maybe (VarT <$> freshed) pure t
    getEnv >>= \env -> putEnv $ env{vars = M.insert n (S.Arg n, Forall mempty t') $ vars env}
    pure (n, t')

tcBody :: Body -> Tc (S.Stmt, Effs, Type)
tcBody = fmap (\(f, es, (x, t)) -> (f $ S.EffPure x, es, t)) . undo . tcBody'

tcBody' :: Body -> Tc (S.Expr, Type)
tcBody' [] = err "Empty body"
tcBody' [x] = tcStmt x
tcBody' (x : xs) = tcStmt x >> tcBody' xs

tcStmt :: Stmt -> Tc (S.Expr, Type)
tcStmt (LetS n x) = do
    (x', t) <- tc x
    getEnv >>= \env -> case locals env M.!? n of
        Just (l, t') -> do
            eq $ t :=: t'
            post $ S.SetLocal (M.size (locals env) - l - 1) x'
        _ -> do
            putEnv env{locals = M.insert n (M.size $ locals env, t) $ locals env}
            post $ S.LetLocal x'
    pure (S.Lit $ Unit (), lit Unit)
tcStmt (IfS c t f) = do
    (c', ct) <- tc c
    (t', te, _) <- tcBody t
    (f', fe, _) <- tcBody f
    eff te
    eff fe
    eq $ ct :=: lit Num
    post $ S.If c' t' f'
    pure (S.Lit $ Unit (), lit Unit)
tcStmt (WhileS c b) = do
    (f, ce, (c', t)) <- undo $ tc c
    (b', be, _) <- tcBody b
    eff ce
    eff be
    eq $ t :=: lit Num
    post $ S.While (f $ S.EffPure c') b'
    pure (S.Lit $ Unit (), lit Unit)
tcStmt (ExprS x) = tc x

effCall :: Name -> [(S.Expr, Type)] -> Tc (S.Expr, Type)
effCall n (unzip -> (es, xs)) = do
    s <- freshed
    env <- getEnv
    Interf m b (fromJust . find (\(_, Effect n' _ _) -> n == n')
        . zip [0..] -> (k, Effect _ x y))
        <- maybe (err $ "Effect not found : " <> n) pure . M.lookup n $ effects env
    u <- unifier b
    Tc $ \ns env -> pure
        ((S.Arg s, subsitute u y)
        , S.EffCall (ns M.! uniq env + M.size (locals env)) n es . S.Code s
        , zipWith (:=:) (subsitute u <$> x) xs
        , Left [(uniq env, Eff m
            ((unifT u M.!) <$> bindT b)
            ((unifE u M.!) <$> bindE b)
            ((unifEs u M.!) <$> bindEs b))]
        , env{uniq = succ $ uniq env})

call :: Lift -> (S.Expr, Type) -> [(S.Expr, Type)] -> Tc (S.Expr, Type)
call l (e, x) (unzip -> (es, xs)) = do
    s <- freshed
    y <- VarT <$> freshed
    effs <- freshed
    replicateM l (EffVar <$> freshed) >>= eff . flip Effs effs
    eq $ x :=: FunT xs (Effs [] effs) y
    l' <- M.size . locals <$> getEnv
    post $ S.Call (l + l') e es . S.Code s
    pure (S.Arg s, y)

post :: (S.Stmt -> S.Stmt) -> Tc ()
post f = Tc $ \_ env -> pure (mempty, f, empty, Left empty, env)

undo :: Tc a -> Tc (S.Stmt -> S.Stmt, Effs, a)
undo m = Tc $ \ns env -> do
    (x, f, eqs, es, env') <- runTc m ns env
    let (env'', eqs', es') = either (\effs ->
            let es' = Effs [] $ '_' : show (uniq env') in
            (env'{uniq=succ $ uniq env'}, (:<: es') <$> effs, es'))
            (env',[],) es
    pure ((f, es', x), id, eqs' <> eqs, Left empty, env'')

eq :: EQ -> Tc ()
eq e = Tc $ \_ env -> pure (mempty, id, [e], Left empty, env)

eff :: Effs -> Tc ()
eff e = Tc $ \_ env -> pure (mempty, id, empty, Right e, env)

tcVar :: Name -> Tc (S.Expr, Type)
tcVar n = getEnv >>= \e ->
    fromMaybe (err $ "Name not found: " <> n) $
    getLocal (M.size $ locals e) <$> M.lookup n (locals e) <|>
    traverse instantinated <$> M.lookup n (vars e)
    where
    instantinated :: Forall -> Tc Type
    instantinated (Forall b x) = (`subsitute` x) <$> unifier b

    getLocal :: Int -> (Int, Type) -> Tc (S.Expr, Type)
    getLocal l' (l, t) = do
        m <- freshed
        post $ S.GetLocal (l' - l - 1) . S.Code m
        pure (S.Arg m, t)

unifier :: Bind -> Tc Unifier
unifier (Bind t e es) = do
    t' <- fmap M.fromList . for t $ \n -> (,) n . VarT <$> freshed
    e' <- fmap M.fromList . for e $ \n -> (,) n . EffVar <$> freshed
    es' <- fmap M.fromList . for es $ \n -> (,) n . Effs [] <$> freshed
    pure Unifier {unifT=t', unifE=e', unifEs=es'}


solve :: [EQ] -> StateT Uniq (Either String) (Unifier, M.Map EId Deep)
solve xs = solve' ys zs where
    (ys, zs) = partition (\case
        _ :<: _ -> False
        _ -> True) xs


solve' :: [EQ] -> [EQ] -> StateT Uniq (Either String) (Unifier, M.Map EId Deep)
solve' [] [] = pure mempty
solve' [] (y : ys) = do
    (u, xs, k, n) <- solveElem y
    bimap (u <>) (M.insert k n) <$> solve' xs (subsituteEQ u <$> ys)
solve' (x : xs) ys = do
    (u, xs') <- lift $ solve1 x
    first (u <>) <$> solve' (xs' <> fmap (subsituteEQ u) xs) (subsituteEQ u <$> ys)

solve1 :: EQ -> Either String (Unifier, [EQ])

solve1 (VarT n :=: VarT m) | n == m = pure mempty
solve1 (VarT n :=: x) = (,mempty) <$> solveVar n x
solve1 (x :=: VarT n) = (,mempty) <$> solveVar n x
solve1 (ConT n :=: ConT m) | n == m = pure mempty
solve1 (FunT xs xe x :=: FunT ys ye y) | length xs == length ys =
    pure (mempty, x :=: y : xe :~~: ye : zipWith (:=:) xs ys)
solve1 (HandlerT xe xfrom xes xto :=: HandlerT ye yfrom yes yto) =
    pure (mempty, [xe :~: ye, xfrom :=: yfrom, xes :~~: yes, xto :=: yto])
solve1 (x :=: y) = Left $ "Type mismatch: " <> show x <> " and " <> show y

solve1 (EffVar n :~: EffVar m) | n == m = pure mempty
solve1 (EffVar n :~: x) = (,mempty) <$> solveVarE n x
solve1 (x :~: EffVar n) = (,mempty) <$> solveVarE n x
solve1 (Eff x xs xe xes :~: Eff y ys ye yes) | x == y =
    pure (mempty, zipWith (:=:) xs ys <> zipWith (:~:) xe ye <> zipWith (:~~:) xes yes)
solve1 (x :~: y) = Left $ "Effect mismatch: " <> show x <> " and " <> show y

solve1 (Effs xs x :~~: Effs ys y) = solveEs xs x ys y
solve1 (_ :<: _) = Left "Unexpected (:<:) constraint. This must not happen"

solveVar :: Name -> Type -> Either String Unifier
solveVar n x
    | n `elem` bindT (free x) = Left $ "Infinite type: " <> n <> " = " <> show x
    | otherwise = pure mempty{unifT = M.singleton n x}

solveVarE :: Name -> Eff -> Either String Unifier
solveVarE n x
    | n `elem` bindE (freeE x) = Left $ "Infinite effect: " <> n <> " = " <> show x
    | otherwise = pure mempty{unifE = M.singleton n x}


solveVarEs :: Name -> Effs -> Either String Unifier
solveVarEs n x
    | n `elem` bindEs (freeEs x) = Left $ "Infinite effects: " <> n <> " = " <> show x
    | otherwise = pure mempty{unifEs = M.singleton n x}

solveEs :: [Eff] -> Name -> [Eff] -> Name -> Either String (Unifier, [EQ])
solveEs [] n [] m | n == m = Right mempty
solveEs [] n ys m = fmap (,mempty) . solveVarEs n $ Effs ys m
solveEs xs n [] m = fmap (,mempty) . solveVarEs m $ Effs xs n
solveEs (x : xs) n (y : ys) m = (\(u, es) -> (u, subsituteEQ u (x :~: y) : es)) <$> solveEs xs n ys m

solveElem :: EQ -> StateT Uniq (Either String) (Unifier, [EQ], EId, Deep)
solveElem ((k, e) :<: Effs es n) = case find (match . snd) $ zip [0..] es of
    Just (i, e') -> pure (mempty, [e :~: e'], k, i)
    Nothing -> state $ \m -> ((mempty{unifEs = M.singleton n . Effs [e] $ '_' : show m}, [], k, length es), m+1)
    where
        match :: Eff -> Bool
        match (Eff n _ _ _) | Eff m _ _ _ <- e = n == m
        match _ = True
solveElem _ = throwError "Unexpected not (:<:) constraint. This must not happen"


runAndSolve :: Tc (a, Type) -> StateT Env (Either String) (a, Forall)
runAndSolve m = StateT $ \env -> mdo
    ((x, t), _, eqs, _, env') <- runTc m n env
    ((u, n), env'') <- runStateT (solve eqs) $ uniq env'
    pure ((x, generalize $ subsitute u t), env'{uniq = env''})

checkFun :: Fun -> StateT Env (Either String) (Name, Forall, S.Fun)
checkFun x@(Fun _ as _) = do
    ((n, f), t) <- runAndSolve (swap <$> tcFun x)
    modify $ \env -> env{vars = M.insert n (S.Global n, t) $ vars env}
    pure (n, t, f)

checkInterface :: Interface -> StateT Env (Either String) ()
checkInterface (Interf n b xs) = modify $ \env -> env{effects = M.fromAscList (fmap (const i) <$> ys) <> effects env}
    where
        ys = sortOn fst $ fmap (\x@(Effect n _ _) -> (n, x)) xs
        i = Interf n b $ snd <$> ys

checkDecls :: [Decl] -> StateT Env (Either String) ([Interface], [(Name, Forall, S.Fun)])
checkDecls (partitionEithers . fmap (\case
    Interface x -> Left x
    Function x -> Right x) -> (is, fs)) = traverse_ checkInterface is >>
        (,) is <$> traverse checkFun fs

checkProgram :: M.Map Name (S.Expr, Forall) -> [Decl] -> Either String ([Interface], [(Name, Forall, S.Fun)])
checkProgram e ds = evalStateT (checkDecls ds) $ Env
    { effects=M.singleton "Fail" $
        Interf "Failable" mempty
            [Effect "Fail" [lit Str] $ VarT "Void"]
    , locals=mempty
    , vars=e
    , uniq=0}
