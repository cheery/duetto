module Solver where

import Trail
import Rep
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Evaluator as E
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable

type Ctx = Trail Type
type Signature = Map Meta Type
type Subst = Map Meta Term

data Solver = Solver {
    constants   :: Map String (Bool, E.Value),
    types       :: Map String Type,
    signature   :: Signature,
    subst       :: Subst,
    constraints :: [(Bool,Constraint)] }

data Constraint = D String (Ctx -> Type -> Context ()) Ctx Type
                | C Ctx Ctx Term Term Type Type

instance Eq Constraint where
    D name _ ctx ty == D name' _ ctx' ty'
        | ctx == ctx', name == name', ty == ty' = True
    C ctx1 ctx1' t1 t1' ty1 ty1' == C ctx2 ctx2' t2 t2' ty2 ty2'
        | ctx1 == ctx2 , ctx1' == ctx2'
        , t1 == t2 , t1' == t2'
        , ty1 == ty2 , ty1' == ty2' = True
    _ == _ = False

sym :: Constraint -> Constraint
sym (C ctx ctx' t t' ty ty') = C ctx' ctx t' t ty' ty

nfConstraint :: Constraint -> Context Constraint
nfConstraint (C ctx ctx' t t' ty ty') = do
    ctx <- nfContext ctx
    ctx' <- nfContext ctx'
    let depth = length ctx
    let depth' = length ctx'
    ty <- nfTerm ty True depth
    ty' <- nfTerm ty' True depth'
    t <- nfTerm t (ty == Star) depth
    t' <- nfTerm t' (ty == Star) depth'
    pure $ C ctx ctx' t t' ty ty'
nfConstraint (D name fn ctx ty) = do
    ctx <- nfContext ctx
    let depth = length ctx
    ty <- nfTerm ty True depth
    pure $ D name fn ctx ty

nfContext :: Ctx -> Context Ctx
nfContext B0 = pure B0
nfContext (xs:<x) = do
    xs' <- nfContext xs
    let depth = length xs'
    let env = E.vunquoteds depth
    ns <- getNs True
    let result = E.quote depth (E.eval ns env x)
    pure (xs':<result)

nfTerm :: Term -> Bool -> Int -> Context Term
nfTerm term active ctxDepth = do
    ns <- getNs active
    pure (nf ns term ctxDepth)

getNs :: Bool -> Context E.Ns
getNs active = do
    const <- gets constants
    subs <- gets subst
    pure (E.Ns const subs active)

toNs :: Solver -> Bool -> E.Ns
toNs sol active = E.Ns (constants sol) (subst sol) active

nf :: E.Ns -> Term -> Int -> Term
nf ns term ctxDepth =
    let env = E.vunquoteds ctxDepth in
    E.quote ctxDepth (E.eval ns env term)
 
type Context a = State Solver a

(<||) :: Monad m => m Bool -> m () -> m ()
a <|| b = do x <- a
             unless x b
 
infixr 5 <||

(<||*) :: Monad m => m Bool -> m Bool -> m Bool
a <||* b = do x <- a
              if x then pure True
                   else b
 
infixr 5 <||*

postpone :: Bool -> Constraint -> Context ()
postpone status constraint = do
    modify (\sol -> sol { constraints = (status, constraint) : constraints sol })

active :: Constraint -> Context ()
active = postpone True

block :: Constraint -> Context ()
block = postpone False

fresh :: Type -> Context Meta
fresh ty = do
    sig <- gets signature
    let nextVar = Meta (Map.size sig)
    modify (\sol -> sol { signature = Map.insert nextVar ty (signature sol) })
    pure nextVar

runContext :: Solver -> Context a -> (a, Solver) 
runContext solver state = runState state solver

eq :: Ctx -> Term -> Type -> Term -> Type -> Context ()
eq ctx s _S t _T = do
    let c = C ctx ctx s t _S _T
    modify (\ctx -> ctx { constraints = (True, c) : constraints ctx })
    let c' = C ctx ctx _S _T Star Star
    modify (\ctx -> ctx { constraints = (True, c') : constraints ctx })

define :: Meta -> Term -> Context ()
define alpha v = do
    sol <- get
    put sol { subst = Map.insert alpha v (subst sol) }

solve :: Solver -> Solver
solve sol = snd $ runContext sol (reps solvingSteps)

reps :: Context Bool -> Context ()
reps p = p >>= \b -> if b then reps p else pure ()

solvingSteps :: Context Bool
solvingSteps = do
    sol <- get
    let cons = constraints sol
    put sol { constraints = [] }
    solveConstraints cons <||* unblockConstraints

-- solveSteps :: Solving ()
-- solveSteps = do
--     guard progress <|> (gets signature >>= lowering . Map.toList >>= guard)
--     -- <|> TODO: unblockConstraints by eta-expanding
--     -- uninstantiated metavariables

solveConstraints :: [(Bool, Constraint)] -> Context Bool
solveConstraints [] = pure False
solveConstraints ((active,x):xs) = do
    progress <- solveConstraints xs
    x' <- nfConstraint x
    if active || (x' /= x) then do
        refine x'
        pure True
    else do
        block x'
        pure progress

refine :: Constraint -> Context ()
refine (C ctx1 ctx2 t1 t2 ty1 ty2) | (t1 == t2) = pure ()
refine q@(C ctx1 ctx2 (Flex a e1) (Flex b e2) ty1 ty2) = do
    {--tryPrune q <|| tryPrune (sym q) <||--} tryInvert q <|| tryInvert (sym q) <|| block q
refine q@(C ctx1 ctx2 t1 (Flex b e2) ty1 ty2) = do
    {--tryPrune (sym q) <||--} tryInvert (sym q) <|| block q
refine q@(C ctx1 ctx2 (Flex a e1) t2 ty1 ty2) = do
    {--tryPrune q <||--} tryInvert q <|| block q
refine q@(C ctx1 ctx2 (Pi a b) (Pi a' b') Star Star) = do
    active (C ctx1 ctx2 a a' Star Star)
    active (C (ctx1:<a) (ctx2:<a') b b' Star Star)
refine q@(C ctx1 ctx2 t1 t2 ty1 ty2) = do
    block q
refine (D name fn ctx ty) = fn ctx ty

-- splitDown :: Ctx -> Ctx -> Term -> Term -> Type -> Type -> Solving ()
-- splitDown ctx ctx' (Pi a (Bind b)) (Pi a' (Bind b')) Star Star = do
--     let c1 = C ctx ctx' a a' Star Star
--     let c2 = C (ctx:<a) (ctx':<a') b b' Star Star
--     active c1
--     active c2
-- splitDown ctx1 ctx2 s t _S _T = block (C ctx1 ctx2 s t _S _T)

--             (_, _) -> do
--                 (t1''', ty1') <- etaExpand ctx1 t1'' (nf (Env sig (ctxToScope ctx1)) ty1)
--                 (t2''', ty2') <- etaExpand ctx2 t2'' (nf (Env sig (ctxToScope ctx2)) ty2)
--                 splitDown ctx1 ctx2 t1''' t2''' ty1' ty2'
--                 -- by rule schema 11 (λ-abstraction)
--                 -- by rule schema 12 (pairs)
--                 -- by rule schema 13 (booleans)
--                 -- by rule schema 3 (injectivity of ∏)
--                 -- by rule schema 4 (injectivity of Σ)
--                 -- by rule schema 5 (bool)
--                 -- by rule schema 6 set
--                 --pure [(Never, C ctx1 ctx2 t1''' t2''' ty1' ty2')]
--         sig <- gets signature
--         --trace ("refine " ++ (show (C ctx1 ctx2 t1 t2 ty1 ty2))) $
--         --  pure ()
--         -- The if case of Definition 2.158 (strongly neutral term) has a
--         -- condition on the number of argumetns which may be fullfilled
--         -- by n-expanding the terms.
--         let t1'' = etaExpandDefHeaded t1
--         let t2'' = etaExpandDefHeaded t2
--         case (t1'', t2'') of
--             (Rigid a e1, Rigid b e2) -> do -- by rule schema 14 (strongly neutral term)
--                 optional (matchSpine ctx1 a e1 ctx2 b e2)
--                 block (C ctx1 ctx2 t1'' t2'' ty1 ty2)
--             (Flex a e1, Flex b e2) | a == b -> do
--                 flexFlexSame ctx1 ctx2 a e1 e2 ty1 ty2
--             (Flex a e1, Flex b e2) -> do
--                 let t1''' = etaContractWhnf t1''
--                 let t2''' = etaContractWhnf t2''
--                 let q = (C ctx1 ctx2 t1''' t2''' ty1 ty2)
--                 tryPrune q <|| tryPrune (sym q) <|| tryInvert q <|| tryInvert (sym q) <|| block q
--             (Flex a e1, u) -> do
--                 let t1''' = etaContractWhnf t1''
--                 let t2''' = etaContractWhnf t2''
--                 let q = (C ctx1 ctx2 t1''' t2''' ty1 ty2)
--                 tryPrune q <|| tryInvert q <|| block q
--             (u, Flex b e2) -> do
--                 let t1''' = etaContractWhnf t1''
--                 let t2''' = etaContractWhnf t2''
--                 let q = (C ctx2 ctx1 t2''' t1''' ty2 ty1)
--                 tryPrune q <|| tryInvert q <|| block q
 
tryInvert :: Constraint -> Context Bool
tryInvert (C ctx1 ctx2 (Flex alpha e1) t2 ty1 ty2) = do
    ty <- getType alpha
    m <- invert alpha ty e1 ctx2 t2
    case m of
        Nothing ->
           pure False
        Just t2' -> do
           active (C ctx1 ctx2 (Flex alpha e1) t2 ty1 ty2)
           define alpha t2'
           pure True
 
invert :: Meta -> Type -> Elims Term -> Ctx -> Term -> Context (Maybe Term)
invert alpha _T e ctx t | alpha `Set.notMember` fmvs t,
                          Just xs <- varList e,
                          fv t `Set.isSubsetOf` fvd xs = pure (Just (remap (length ctx) t xs))
                        | otherwise = pure Nothing

fvd :: Trail Term -> Set Int
fvd B0 = Set.empty
fvd (xs:<x) = (fvd xs `Set.difference` fv x) `Set.union` (fv x `Set.difference` fvd xs)

-- -- This alone should be sufficient for flowing values inside metavars.
remap :: Int -> Term -> Trail Term -> Term
remap ctxsz t xs = let xsl = length xs in
    wrapAbs xsl (E.quote xsl (E.eval (E.Ns Map.empty Map.empty False) (fmap (locate xs) [0,1..ctxsz-1]) t))
--     where help :: Int -> RName -> RName
--           help i (Closed j) | (i <= j) = Closed (locate xs (j-i) 0+i)
--           help i head = head

locate :: Trail Term -> Int -> E.Value
locate B0 i = E.VRigid (Unavailable "bug") B0
locate (xs:<Rigid (Closed j) B0) i | (i == j) = E.vunquoted (length xs)
locate (xs:<x) i = locate xs i
 
getType :: Meta -> Context Type
getType name = do
    sig <- gets signature
    case Map.lookup name sig of
        Just ty -> pure ty
        Nothing -> error "variable not installed"

tryPrune :: Constraint -> Context Bool
tryPrune q@(C _ _ (Flex alpha e) t _ _) = do
    u <- pruneTerm (foldMap (foldMap fv) e) t
    case u of
        (d:_) ->
            --instantiate d
            --sig <- gets signature
            --let q' = normalize sig q
            --trace "pruned" (active q')
            --pure True
            active q >> instantiate d >> pure True
        [] -> pure False

pruneTerm :: Set Int -> Term -> Context [Instantiation]
pruneTerm vs (Pi s t) = (++) <$> pruneTerm vs s <*> pruneUnder vs t
--pruneTerm vs (Sigma s t) = (++) <$> pruneTerm vs s <*> pruneUnder vs t
--pruneTerm vs (Pair s t) = (++) <$> pruneTerm vs s <*> pruneTerm vs t
pruneTerm vs (Abs b) = pruneUnder vs b
pruneTerm vs Star = pure []
pruneTerm vs (Rigid r e) = pruneElims vs e
pruneTerm vs (Flex beta e) = pruneMeta vs beta e
pruneTerm vs (RecordType rec) = fold <$> traverse (pruneTerm vs) (map snd rec)
pruneTerm vs (Record rec) = fold <$> traverse (pruneTerm vs) rec

pruneUnder :: Set Int -> Term -> Context [Instantiation]
pruneUnder vs term = pruneTerm (Set.insert 0 (Set.map (+1) vs)) term

pruneElims :: Set Int -> Trail (Elim Term) -> Context [Instantiation]
pruneElims vs e = fold <$> traverse pruneElim e
    where pruneElim :: Elim Term -> Context [Instantiation]
          pruneElim (App x) = pruneTerm vs x
          pruneElim (Field i) = pure []

pruneMeta :: Set Int -> Meta -> Trail (Elim Term) -> Context [Instantiation]
pruneMeta vs alpha e = do
    (tel, ty) <- telescope <$> getType alpha
    case pruneVars vs (fv ty) tel e of
        Just dropList | any (/=0) dropList -> do
            let ty' = E.tighten' dropList ty
            let tel' = tightenCtx dropList tel
            pure [(alpha, fromTelescope tel' ty',
                  \beta -> wrapAbs (length tel) (Flex beta (revec dropList (vector 0 (length tel)))))]
        _ -> pure []

tightenCtx :: Trail Int -> Ctx -> Ctx
tightenCtx _ B0 = B0
tightenCtx (ts:<_) (xs:<x) = tightenCtx ts xs :< E.tighten' ts x
 
revec :: Trail Int -> Trail (Elim Term) -> Trail (Elim Term)
revec _ B0 = B0
revec (ts:<0) (xs:<x) = revec ts xs :< x
revec (ts:<_) (xs:<x) = revec ts xs

pruneVars :: Set Int -> Set Int -> Ctx -> Trail (Elim Term) -> Maybe (Trail Int)
pruneVars vs ks B0 B0 = Just B0
pruneVars vs ks (xs :< _S) (e :< App s) =
    if 0 `Set.member` ks
    then do dropList <- pruneVars vs (Set.map (subtract 1) ks `Set.union` fv _S) xs e
            Just (dropList :< 0)
    else case toVar s of
        Just y | y `Set.member` vs -> do
            dropList <- pruneVars vs (Set.map (subtract 1) ks `Set.union` fv _S) xs e
            Just (dropList :< 0)
        _      | not (fvr s `Set.isSubsetOf` vs) -> do
            dropList <- pruneVars vs (Set.map (subtract 1) ks) xs e
            Just (dropList :< 1)
               | otherwise -> Nothing
pruneVars vs ks _ _ = Nothing
 
-- -- Similar to prune, so it's down here
-- flexFlexSame :: Ctx -> Ctx -> Name
--              -> Trail (Elim Term) -> Trail (Elim Term)
--              -> Type -> Type -> Solving ()
-- flexFlexSame ctx ctx' alpha e e' ty ty' = do
--     (tel, _T) <- telescope <$> getType alpha
--     case intersect (fv _T) tel e e' of
--         Just dropList | any (/=0) dropList -> let _T' = tighten' dropList _T
--                                                   tel' = tightenCtx dropList tel
--                                               in instantiate (alpha, fromTelescope tel' _T',
--                                                               \beta -> abss (Flex beta (revec dropList (vector 0 tel))) tel)
--         _                                  -> block (C ctx ctx' (Flex alpha e) (Flex alpha e') ty ty')
-- 
-- -- Given a telescope and the two evaluation contexts, |intersect|
-- -- checks the evaluation contexts are lists of variables and produces the
-- -- telescope on which they agree.
--  
-- intersect :: Set Int -> Trail Type -> Trail (Elim Term) -> Trail (Elim Term) -> Maybe (Trail Int)
-- intersect ks B0 B0 B0 = pure B0
-- intersect ks (tel :< _S) (e :< App s) (e' :< App s') = do
--     x <- toVar s
--     y <- toVar s'
--     if x == y then do
--         tel <- intersect (Set.map (subtract 1) ks `union` fv _S) tel e e'
--         pure (tel :< 0)
--     else if 0 `member` ks then empty
--          else do tel <- intersect (Set.map (subtract 1) ks) tel e e'
--                  pure (tel :< 1)
--         
-- -- For rigid-rigid interaction.
-- matchSpine :: Ctx -> RName -> Trail (Elim Term)
--            -> Ctx -> RName -> Trail (Elim Term)
--            -> Solving (Type, Type)
-- matchSpine ctx x B0 ctx' x' B0
--     | x == x' = do
--         a <- lookupVar ctx x
--         b <- lookupVar ctx' x'
--         pure (a, b)
--     | otherwise = empty
-- matchSpine ctx x (e :< App a) ctx' x' (e' :< App a') = do
--     (Pi _A _B, Pi _S _T) <- matchSpine ctx x e ctx' x' e'
--     active $ C ctx ctx' a a' _A _S
--     pure (inst (Env Map.empty (ctxToScope ctx)) _B a,
--           inst (Env Map.empty (ctxToScope ctx')) _T a')
-- matchSpine ctx x (e :< Fst) ctx' x' (e' :< Fst) = do
--     (Sigma _A _B, Sigma _S _T) <- matchSpine ctx x e ctx' x' e'
--     pure (_A, _S)
-- matchSpine ctx x (e :< Snd) ctx' x' (e' :< Snd) = do
--     (Sigma _A _B, Sigma _S _T) <- matchSpine ctx x e ctx' x' e'
--     pure (inst (Env Map.empty (ctxToScope ctx))  _B (Rigid x (e :< Fst)),
--           inst (Env Map.empty (ctxToScope ctx')) _T (Rigid x' (e' :< Fst)))
-- matchSpine _ _ _ _ _ _ = empty
-- 
unblockConstraints :: Context Bool
unblockConstraints = do
    sol <- get
    let open = Map.toList $ signature sol `Map.difference` subst sol
    progress <- mapM lowering open
    pure (any id progress)

lowering :: (Meta, Type) -> Context Bool
lowering (alpha, ty) = do
    ty <- nfTerm ty True 0
    lower B0 alpha ty

-- lowering :: [(Name, Entry)] -> Solving Bool
-- lowering [] = pure False
-- lowering ((alpha,(ty,HOLE)):xs) = do
--     status <- lowering xs
--     status' <- lower B0 alpha ty
--     pure (status || status')
-- lowering (_:xs) = lowering xs
-- 
-- -- Given the name and type of a metavariable, |lower| attempts to
-- -- simplify it by removing $\Sigma$-types, according to the metavariable
-- -- simplification steps \eqref{eqn:miller:metasimp:sigma} and
-- -- \eqref{eqn:miller:metasimp:pi-sigma} in
-- -- Figure~\longref{fig:miller:solve}, as described in
-- -- Subsection~\longref{subsec:miller:spec:metasimp}.
-- 
lower :: Ctx -> Meta -> Type -> Context Bool
lower phi alpha (Pi _S _T) = lower (phi:<_S) alpha _T
lower phi alpha (RecordType rec) = do
    stuff <- mapM (lowerhandle phi) rec
    define alpha (wrapAbs (length phi) (Record (map snd stuff)))
    pure True
lower phi alpha other = pure False

lowerhandle :: Ctx -> (String, Type) -> Context (String, Term)
lowerhandle ctx (name, ty) = do
    a0 <- fresh (fromTelescope ctx ty)
    let u = Flex a0 (vector 0 (length ctx))
    pure (name, u)

-- lower :: Ctx -> Name -> Type -> Solving Bool
-- lower phi alpha (Sigma _S _T) = do
--     beta <- hole phi _S
--     let betaTerm = Flex beta (vector 0 phi)
--     ceta <- hole phi (inst (Env Map.empty (ctxToScope phi)) _T betaTerm)
--     let cetaTerm = Flex ceta (vector 0 phi)
--     define phi alpha (Sigma _S _T) (Pair betaTerm cetaTerm)
--     pure True
-- lower phi alpha (Pi _S (Bind _T)) = lower (phi :< _S) alpha _T
-- lower _ _ _ = pure False
-- 
-- -- > lower :: Telescope -> Nom -> Type -> Contextual Bool
-- -- > lower _Phi alpha (Sig _S _T) =  hole _Phi _S $ \ s ->
-- -- >                                 hole _Phi (inst _T s) $ \ t ->
-- -- >                                 define _Phi alpha (Sig _S _T) (PAIR s t) >>
-- -- >                                 return True
-- -- >
-- -- > lower _Phi alpha (Pi _S _T) = do  x <- fresh (s2n "x")
-- -- >                                   splitSig B0 x _S >>= maybe
-- -- >                                       (lower (_Phi :< (x, _S)) alpha (inst _T (var x)))
-- -- >                                       (\ (y, _A, z, _B, s, (u, v)) ->
-- -- >                                           hole _Phi (_Pi y _A  (_Pi z _B (inst _T s))) $ \ w ->
-- -- >                                           define _Phi alpha (Pi _S _T) (lam x (w $$ u $$ v)) >>
-- -- >                                           return True)      
-- -- >             
-- -- > lower _Phi alpha _T = return False
-- -- 
-- -- Lowering a metavariable needs to split $\Sigma$-types (possibly
-- -- underneath a bunch of parameters) into their components.  For example,
-- -- $[[y : Pi x : X . Sigma z : S . T]]$ splits into
-- -- $[[y0 : Pi x : X . S]]$ and $[[y1 : Pi x : X . {y0 x/z} T]]$.  Given
-- -- the name of a variable and its type, |splitSig| attempts to split it,
-- -- returning fresh variables for the two components of the $\Sigma$-type,
-- -- an inhabitant of the original type in terms of the new variables and
-- -- inhabitants of the new types by projecting the original variable.
-- -- 
-- -- > splitSig ::  Telescope -> Nom -> Type ->
-- -- >                  Contextual (Maybe  (Nom, Type, Nom, Type, Tm, (Tm, Tm)))
-- -- > splitSig _Phi x (Sig _S _T)  = do  y  <- fresh (s2n "y")
-- -- >                                    z  <- fresh (s2n "z")
-- -- >                                    return $ Just  (y, _Pis _Phi _S, z, _Pis _Phi (inst _T (var y $*$ _Phi)),
-- -- >                                                   lams' _Phi (PAIR (var y $*$ _Phi) (var z $*$ _Phi)),
-- -- >                                                   (lams' _Phi (var x $*$ _Phi %% Hd), 
-- -- >                                                        lams' _Phi (var x $*$ _Phi %% Tl)))
-- -- > splitSig _Phi x (Pi _A _B)   = do  a <- fresh (s2n "a")
-- -- >                                    splitSig (_Phi :< (a, _A)) x (inst _B (var a))
-- -- > splitSig _ _ _ = return Nothing

type Instantiation = (Meta, Type, Meta -> Term)

instantiate :: Instantiation -> Context ()
instantiate d@(alpha, ty, f) = do
    beta <- fresh ty
    define alpha (f beta)
    
-- 
-- tZip :: Trail a -> Trail b -> Trail (a, b)
-- tZip B0 _ = B0
-- tZip _ B0 = B0
-- tZip (xs:<x) (ys:<y) = tZip xs ys :< (x, y)
 
toVar :: Term -> Maybe Int
toVar (Rigid (Closed n) B0) = Just n
toVar _ = Nothing
 
varList :: Elims Term -> Maybe (Trail Term)
varList (xs:<App (Flex a B0)) = (:<Flex a B0) <$> varList xs
varList (xs:<App (Rigid n B0)) = (:<Rigid n B0) <$> varList xs
--varList (xs:<App _) = (:<Rigid (Unavailable "") B0) <$> varList xs
varList B0 = pure B0
varList _ = Nothing
 
-- TODO: without nextVar, this is bad.
-- resetSubst :: Solver -> Solver
-- resetSubst sol = sol { signature = fmap (\a -> nf ns a 0) i, subst = mempty }
--     where ns = toNs sol True
--           i = signature sol `Map.difference` s
--           s = subst sol

-- 
-- etaContractWhnf :: Term -> Term
-- etaContractWhnf t = t
-- --etaContractWhnf (Abs (Bind (VRigid head (elems:<App (VRigid (Closed 0) B0)))))
-- --    | not (0 `Set.member` fv (head, elems)) = 
-- 
-- etaExpand :: Ctx -> Term -> Type -> Solving (Term, Type)
-- etaExpand ctx (Abs x) (Pi a b) = pure (Abs x, Pi a b)
-- etaExpand ctx (Pair x y) (Sigma a b) = pure (Pair x y, Sigma a b)
-- etaExpand ctx f (Pi a b) = pure (Abs (Bind expr), Pi a b)
--     where expr = quote (length scop) $
--                        eval (Env Map.empty scop) (loosen f) `vapp` (vunquoted (length scop))
--           scop = (ctxToScope ctx)
-- etaExpand ctx p (Sigma a b) = pure (Pair e1 e2, Sigma a b)
--     where e1 = quote (length scop) $ vfst $ eval (Env Map.empty scop) p
--           e2 = quote (length scop) $ vsnd $ eval (Env Map.empty scop) p
--           scop = (ctxToScope ctx)
-- etaExpand ctx p q = pure (p, q)
-- 
-- etaExpandDefHeaded :: Term -> Term
-- etaExpandDefHeaded t = t
-- 
--
 
telescope :: Type -> (Trail Type, Type)
telescope t = telescope' B0 t
    where telescope' :: Trail Type -> Type -> (Trail Type, Type)
          telescope' xs (Pi a b) = telescope' (xs :< a) b
          telescope' xs a = (xs, a)
