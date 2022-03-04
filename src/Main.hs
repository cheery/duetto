module Main where

import Control.Monad.State
import System.Environment
import Trail
import LR
import Tokenize
import Rep
import Evaluator
import Solver
import qualified Solver as S
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace
import Transform (Entry(..), scc', preorder, Graph, Table)
--import Data.Foldable (all)

readStatements :: String -> Either String [Statement]
readStatements contents = do
    tokens <- tokenize contents
    case parse tokens of
        (Just statements, []) -> pure statements
        (_, ((_,(x,y),_):_)) -> Left $ "could not parse col " ++ show x ++ " line " ++ show y
        (_, []) -> Left $ "file truncated"

main :: IO ()
main = do
    args <- getArgs
    case args of
        (x:xs) -> do
            contents <- readFile x
            case readStatements contents of
                Left s -> putStrLn s
                Right stmts -> do_stuff_with stmts

runSample = do
    contents <- readFile "../sample"
    case readStatements contents of
        Left s -> putStrLn s
        Right stmts -> do_stuff_with stmts

do_stuff_with stmts = do
    let table = statement_table stmts
    let order = (map preorder $ scc' $ dependency_graph stmts)
    let sol = Solver Map.empty Map.empty Map.empty Map.empty []
    proto_elaborate' order table sol 
    -- putStrLn (show order)
    -- proto_elaborate stmts

dependency_graph :: [Statement] -> Graph
dependency_graph [] = mempty
dependency_graph (TypeDecl name term:stmts) =
    Map.insert (TypeOf name) depList dep
    where depList = map DeclOf (Set.toList (fcv term))
          dep = dependency_graph stmts
dependency_graph (Decl name term:stmts) =
    Map.insert (DeclOf name) depList dep
    where depList = TypeOf name
                  : map DeclOf (Set.toList (fcv term))
          dep = dependency_graph stmts
dependency_graph (_:stmts) = dependency_graph stmts

statement_table :: [Statement] -> Table Statement
statement_table [] = mempty
statement_table (TypeDecl name term:stmts) =
    Map.insert (TypeOf name) (TypeDecl name term) (statement_table stmts)
statement_table (Decl name term:stmts) =
    Map.insert (DeclOf name) (Decl name term) (statement_table stmts)
statement_table (_:stmts) = statement_table stmts

proto_elaborate' :: [[Entry]] -> Table Statement -> Solver -> IO ()
proto_elaborate' [] table sol = pure ()
proto_elaborate' ([e]:rest) table sol = do
    case Map.lookup e table of
        Just (TypeDecl name term) -> do
            putStrLn $ "elab on tdecl " ++ name
            let (u, sol') = runContext sol (elaborate B0 baseMapping term Star)
            let sol'' = solve sol'
            let u' = nf (toNs sol'' True) u 0
            if Set.size (fmvs u') + length (constraints sol) == 0
            then do
                putStrLn $ "success: " ++ show u'
                let sol''' = sol'' { types = Map.insert name u' (types sol'') }
                proto_elaborate' rest table sol'''
            else do 
                putStrLn $ "failure: " ++ show u'
                mapM_ (\(_,C _ _ t t' _ _) -> do
                    putStrLn $ show t ++ " = " ++ show t'
                    ) (constraints sol'')
        Just (Decl name term) -> do
            putStrLn $ "elab on decl " ++ name
            case Map.lookup name (types sol) of
                Just ty -> do
                    let (u, sol') = runContext sol (elaborate B0 baseMapping term ty)
                    let sol'' = solve sol'
                    let u' = nf (toNs sol'' False) u 0
                    if Set.size (fmvs u') + length (constraints sol) == 0
                    then do
                        putStrLn $ "success: " ++ show u'
                        let u'' = eval (toNs sol'' True) [] u'
                        let sol''' = sol'' { Solver.constants = Map.insert name (False, u'') (Solver.constants sol'') }
                        proto_elaborate' rest table sol'''
                    else do
                        putStrLn $ "failure: " ++ show u'
                        mapM_ (\(_,C _ _ t t' _ _) -> do
                            putStrLn $ show t ++ " = " ++ show t'
                            ) (constraints sol'')
                        proto_elaborate' rest table sol''
                Nothing -> do
                    putStrLn $ "no type for " ++ name
                    proto_elaborate' rest table sol
        Nothing -> pure ()

proto_elaborate :: [Statement] -> IO ()
proto_elaborate [] = pure ()
proto_elaborate (TypeDecl name term:rest) = do
    putStrLn $ "elab on " ++ name
    let sol = Solver Map.empty Map.empty Map.empty Map.empty []
    let (u, sol') = runContext sol (do
                        let m = baseMapping
                        elaborate B0 m term Star)
    let sol'' = solve sol'
    putStrLn $ "result " ++ show (nf (toNs sol'' False) u 0)
    mapM_ (\(_,C _ _ t t' _ _) -> do
        putStrLn $ show t ++ " = " ++ show t'
        ) (constraints sol'')
    proto_elaborate rest
proto_elaborate (stmt:rest) =
    proto_elaborate rest

-- import Data.Set as Set (Set, singleton, unions, union, member, isSubsetOf)
-- import qualified Data.Set as Set
-- import Data.Monoid (Any(..), getAny)
-- import Data.List (unionBy)
-- import Data.Function (on)
-- import Data.Foldable (fold)
-- import Data.Map (Map)
-- 
-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Control.Monad.Except
-- import Control.Monad.Identity
-- import Control.Monad.Trans.Maybe
-- import Control.Applicative (Alternative, (<|>), empty, optional, many)
-- import Debug.Trace

-- emptySolver :: Int -> Solver
-- emptySolver = Solver (Map.empty) []

evalAdjust :: Int -> ((Term -> Value) -> Value) -> Type
evalAdjust depth f =
    let env = vunquoteds depth
        ns = Ns Map.empty Map.empty False
    in quote depth (f (eval ns env))

raise :: Type -> Int -> Int -> (Value -> Value) -> Type
raise ty i depth f =
    let env = vunquoteds depth
        ns = Ns Map.empty Map.empty False
    in quote (depth+i) (f (eval ns env ty))

lookupVar :: Ctx -> Int -> Type
lookupVar ctx i = raise (ctx !!! i) (i+1) (length ctx-i-1) id

type Mapping = Ctx -> String -> Term -> Type -> Context ()

baseMapping :: Ctx -> String -> Term -> Type -> Context ()
baseMapping ctx name t ty = do
    types <- gets S.types
    case Map.lookup name types of
        Just ty' -> do
            eq ctx (Rigid (Const name) B0) ty'
                   t ty
        Nothing -> 
            eq ctx (Rigid (Const name) B0) (Rigid (Unavailable name) B0)
                   t ty

elaborate :: Ctx -> Mapping -> STerm -> Type -> Context Term
elaborate ctx m t ty = do
    a0 <- fresh (fromTelescope ctx ty)
    let u = Flex a0 (vector 0 (length ctx))
    case t of
        SName name -> m ctx name u ty >> pure u
        SCall a r b -> do
            y0 <- fresh (fromTelescope ctx Star)
            let _Y0 = Flex y0 (vector 0 (length ctx))
            y1 <- fresh (fromTelescope (ctx:<_Y0) Star)
            let _Y1 = Flex y1 (vector 0 (length ctx+1))
            y2 <- fresh (fromTelescope (ctx:<_Y0:<_Y1) Star)
            let _Y2 = Flex y2 (vector 0 (length ctx+2))
            a' <- elaborate ctx m a (Pi _Y0 (Pi _Y1 _Y2))
            q <- elaborateRecord ctx m r _Y0
            v <- elaborate ctx m b (Flex y1 (vector 0 (length ctx):<App q))
            let ty' = (Flex y2 (vector 0 (length ctx):<App q:<App v))
            let p = evalAdjust (length ctx) (\ev -> ev a' `vapp` ev q `vapp` ev v)
            eq ctx u ty p ty'
            pure u
-- inferElim (App x) (f, e, ty) = do
--     ctx <- ask
--     y1 <- fresh
--     y2 <- fresh
--     let _Y1 = (Flex y1 (vector 0 ctx))
--     let _Y2 = (Flex y2 (vector 0 (ctx:<_Y1)))
--     addSignature y1 (fromTelescope ctx Star, HOLE)
--     addSignature y2 (fromTelescope (ctx:<_Y1) Star, HOLE)
--     u <- elaborate x _Y1
--     eq ty Star (Pi _Y1 (Bind _Y2)) Star
--     pure (f, e:<App u, Flex y2 (vector 0 ctx:<App u))

        SAbs r n body -> do
            case ty of
                Pi (RecordType rec) (Pi a b) | length r <= length rec -> do
                    let ctx' = ctx:<RecordType rec:<a
                    let rec' = zip r (map snd rec)
                    let m' = mappingRecord (length ctx) 0 rec' m
                    let m'' = mappingVar (length ctx+1) n m'
                    body <- elaborate ctx' m'' body b
                    eq ctx u ty (Abs (Abs body)) ty
                    pure u
                _ -> error $ "TODO " ++ show ty
--         Abs (Bind body) -> do
--             y1 <- fresh
--             y2 <- fresh
--             let _Y1 = (Flex y1 (vector 0 ctx))
--             let _Y2 = (Flex y2 (vector 0 (ctx:<_Y1)))
--             addSignature y1 (fromTelescope ctx Star, HOLE)
--             addSignature y2 (fromTelescope (ctx:<_Y1) Star, HOLE)
--             u' <- expand _Y1 (elaborate body _Y2)
--             eq u ty (Abs (Bind u')) (Pi _Y1 (Bind _Y2))
        SPi r n a b -> do
            rec <- elaborateRecordType ctx m 0 r
            let ctx' = ctx:<RecordType rec
            let m' = mappingRecord (length ctx) 0 rec m
            s <- elaborate ctx' m' a Star
            let ctx'' = ctx':<s
            let m'' = mappingVar (length ctx') n m'
            t <- elaborate ctx'' m'' b Star
            eq ctx u ty (Pi (RecordType rec) (Pi s t)) Star
            pure u
        SAnn a b -> error "TODO"
        SHole -> pure u
        SStar -> eq ctx u ty Star Star >> pure u
        SEq a x y -> error "TODO"
        SIntegerConst i -> error "TODO"
        SRealConst r -> error "TODO"
        SStringConst s -> error "TODO"
        SBind n a b -> error "TODO"
        SField a n -> error "TODO"
        SCase x mot cases -> error "TODO"
        SCocase cases -> error "TODO"
        SORecord terms -> error "TODO"
        SLRecord lterms ext -> error "TODO"

elaborateRecord :: Ctx -> Mapping -> [(String, STerm)] -> Type -> Context Term
elaborateRecord ctx m xs ty = do
    a0 <- fresh (fromTelescope ctx ty)
    let u = Flex a0 (vector 0 (length ctx))
    active (D "record" (elaborateRecord' m xs u) ctx ty)
    pure u

elaborateRecord' :: Mapping -> [(String, STerm)] -> Term -> Ctx -> Type -> Context ()
elaborateRecord' m xs t ctx (RecordType rec) = do
    mapM_ (\(name, val) -> case lookup name rec of
        Just a -> pure ()
        Nothing -> error $ show name <> " not part of record") xs
    r <- elaborateFields ctx m B0 xs rec
    eq ctx t (RecordType rec) (Record r) (RecordType rec)
elaborateRecord' m xs t ctx ty = do
    block (D "record" (elaborateRecord' m xs t) ctx ty)

elaborateFields :: Ctx -> Mapping -> Elims Term -> [(String, STerm)] -> [(String, Type)] -> Context [Term]
elaborateFields ctx m el xs [] = pure []
elaborateFields ctx m el xs ((name,ty):rec) = do
    let ty' = evalAdjust (length ctx) (\e -> vsp (e ty) (fmap (fmap e) el))
    t <- case lookup name xs of
        Just sterm -> elaborate ctx m sterm ty'
        Nothing -> do
            a0 <- fresh (fromTelescope ctx ty')
            pure (Flex a0 (vector 0 (length ctx) <.> el))
    (t:) <$> elaborateFields ctx m (el:<App t) xs rec

elaborateRecordType :: Ctx -> Mapping -> Int -> [(String, STerm)]
                    -> Context [(String, Term)]
elaborateRecordType ctx m i [] = pure []
elaborateRecordType ctx m i ((name,term):terms) = do
    u <- elaborate ctx m term Star
    let entry = (name,wrapAbs i u)
    let ctx' = ctx:<u
    let m' = mappingVar (length ctx) name m
    r <- elaborateRecordType ctx' m' (i+1) terms
    pure (entry:r)

mappingRecord :: Int -> Int -> [(String, Type)] -> Mapping -> Mapping
mappingRecord i j [] m = m
mappingRecord i j ((name,ty):xs) m =
    mappingRecord i (j+1) xs m' 
    where m' :: Mapping
          m' ctx name' term' ty' | name == name' = do
              let k = (length ctx - i - 1)
              let ty'' = raise ty (k+1) (length ctx-k-1) (rig k j)
              eq ctx (Rigid (Closed k) (B0:<Field j)) ty'' term' ty'
          m' ctx name' term' ty' = m ctx name' term' ty'
          rig :: Int -> Int -> Value -> Value
          rig k 0 v = v
          rig k j v = rig k (j-1) v `vapp` VRigid (Unquoted i) (B0:<Field (j-1))

mappingVar :: Int -> String -> Mapping -> Mapping
mappingVar i name m ctx name' term ty | name == name' = do
    let k = (length ctx - i - 1)
    let ty' = lookupVar ctx k
    eq ctx (Rigid (Closed k) B0) ty' term ty
mappingVar i name m ctx name' term ty = m ctx name' term ty

-- elaborate :: Term -> Type -> Context Term
-- elaborate t ty = do
--     a0 <- fresh
--     ctx <- ask
--     addSignature a0 (fromTelescope ctx ty, HOLE)
--     let u = Flex a0 (vector 0 ctx)
--     case t of
--         Rigid name elim -> do
--             (f, elim, ty') <- inferRName name >>= inferElims elim
--             eq u ty (f elim) ty'
--         Flex alpha elim -> do
--             (f, elim, ty') <- inferMeta alpha >>= inferElims elim
--             eq u ty (f elim) ty'
--         Sigma a (Bind b) -> do
--             s <- elaborate a Star
--             t <- expand s (elaborate b Star)
--             eq u ty (Sigma s (Bind t)) Star
--         Pair a b -> do
--             y1 <- fresh
--             y2 <- fresh
--             let _Y1 = (Flex y1 (vector 0 ctx))
--             let _Y2 = (Flex y2 (vector 0 (ctx:<_Y1)))
--             addSignature y1 (fromTelescope ctx Star, HOLE)
--             addSignature y2 (fromTelescope (ctx:<_Y1) Star, HOLE)
--             s <- elaborate a _Y1
--             t <- elaborate b (Flex y2 (vector 0 ctx:<App s))
--             eq u ty (Pair s t) (Sigma _Y1 (Bind _Y2))
--     pure u
-- 
-- type HeadTrail = (Trail (Elim Term) -> Term, Trail (Elim Term), Type)
-- 
-- inferRName :: RName -> Context HeadTrail
-- inferRName var = do
--     ctx <- ask
--     ty <- lookupVar ctx var
--     pure (Rigid var, B0, ty)
-- 
-- inferMeta :: Name -> Context HeadTrail
-- inferMeta alpha = do
--     isFresh <- (not . member alpha) <$> support
--     if isFresh then do
--         ctx <- ask
--         ty <- fresh
--         let _TY = Flex ty (vector 0 ctx)
--         addSignature ty (fromTelescope ctx Star, HOLE)
--         addSignature alpha (fromTelescope ctx _TY, HOLE)
--         pure (Flex alpha, vector 0 ctx, _TY)
--     else error "unfresh meta"
-- 
-- inferElims :: Trail (Elim Term) -> HeadTrail -> Context HeadTrail
-- inferElims B0 ht = pure ht
-- inferElims (xs:<x) ht = inferElims xs ht >>= inferElim x
-- 
-- inferElim :: Elim Term -> HeadTrail -> Context HeadTrail
-- inferElim Fst (f, e, ty) = do
--     ctx <- ask
--     y1 <- fresh
--     y2 <- fresh
--     let _Y1 = (Flex y1 (vector 0 ctx))
--     let _Y2 = (Flex y2 (vector 0 (ctx:<_Y1)))
--     addSignature y1 (fromTelescope ctx Star, HOLE)
--     addSignature y2 (fromTelescope (ctx:<_Y1) Star, HOLE)
--     eq ty Star (Sigma _Y1 (Bind _Y2)) Star
--     pure (f, e:<Fst, _Y1)
-- inferElim Snd (f, e, ty) = do
--     ctx <- ask
--     y1 <- fresh
--     y2 <- fresh
--     let _Y1 = (Flex y1 (vector 0 ctx))
--     let _Y2 = (Flex y2 (vector 0 (ctx:<_Y1)))
--     addSignature y1 (fromTelescope ctx Star, HOLE)
--     addSignature y2 (fromTelescope (ctx:<_Y1) Star, HOLE)
--     eq ty Star (Sigma _Y1 (Bind _Y2)) Star
--     pure (f, e:<Snd, Flex y2 (vector 0 ctx:<App (f (e:<Fst))))
-- 
-- vector :: Int -> Trail a -> Trail (Elim Term)
-- vector i B0 = B0 
-- vector i (xs:<x) = vector (i+1) xs :< App (Rigid (Closed  i) B0)
-- 
-- id_ty :: Term
-- id_ty = Pi Star (Bind (Pi (Rigid (Closed 0) B0) (Bind (Rigid (Closed 1) B0))))
-- 
-- id_term :: Term
-- id_term = Abs (Bind (Abs (Bind (Rigid (Closed 0) B0))))
-- 
-- var :: Int -> Term
-- var i = Rigid (Closed i) B0
-- 
-- ab :: Term -> Term
-- ab t = Abs (Bind t)
-- 
-- pi' :: Term -> Term -> Term
-- pi' t u = Pi t (Bind u)
-- 
-- const_ty :: Term
-- const_ty = Pi Star (Bind (Pi Star (Bind
--               (Pi (var 1) (Bind (Pi (var 1) (Bind (var 3))))))))
-- 
-- const_term :: Term
-- const_term = Abs (Bind (Abs (Bind (Abs (Bind (Abs (Bind (var 1))))))))
-- 
-- num_ty = pi' Star (pi' (pi' (var 0) (var 1)) (pi' (var 1) (var 2)))
-- zero_term = ab (ab (ab (var 0)))
-- one_term = ab (ab (ab (Rigid (Closed 1) (B0:<App (var 0)))))
-- two_term = ab (ab (ab (Rigid (Closed 1) (B0:<App (Rigid (Closed 1) (B0:<App (var 0)))))))
-- 
-- --call_id_term = ab (ab (ab (var 0)))
-- 
-- call_id_ty = (pi' id_ty (pi' Star Star))
-- call_id_term = (ab (ab (var 0)))
-- call_id_term2 = (ab (ab (Rigid (Closed 1) (B0:<App Star:<App (var 0)))))
-- 
-- call_id_ty2 = pi' Star (pi' id_ty (pi' (var 1) (var 2)))
-- call_id_term3 = ab (ab (ab (Rigid (Closed 1) (B0:<App (Flex (Name (-1)) B0):<App (var 0)))))
-- 
-- --call_id_term = ab (ab (ab (Rigid (Closed 1) (B0:<App (Flex (Name (-1)) B0):<App (var 0)))))
-- 
-- runSolver test = do
--     (u, solver) <- runContext B0 (emptySolver 0) test
--     let solver' = solve solver
--     case solver' of
--         (Solver sig [] _) -> Right (nf (Env sig []) u, sig)
--         (Solver sig cons _) -> Left (show (sig, cons))
-- 
-- runTest1Print :: IO ()
-- runTest1Print = do
--     case runSolver (elaborate const_ty Star) of
--         Left error -> putStrLn ("error: " ++ error)
--         Right (ty, solver) -> do
--             putStrLn ("success: " ++ show ty)
--             -- let result = runContext B0 (emptySolver 0) (elaborate id_term ty)
--             -- putStrLn (show result)
--             case runSolver (elaborate const_term ty) of
--                 Left error -> putStrLn ("error: " ++ error)
--                 Right (term, solver) -> do
--                     putStrLn ("success: " ++ show term)
--     case runSolver (elaborate id_ty Star) of
--         Left error -> putStrLn ("error: " ++ error)
--         Right (ty, solver) -> do
--             putStrLn ("success: " ++ show ty)
--             -- let result = runContext B0 (emptySolver 0) (elaborate id_term ty)
--             -- putStrLn (show result)
--             case runSolver (elaborate id_term ty) of
--                 Left error -> putStrLn ("error: " ++ error)
--                 Right (term, solver) -> do
--                     putStrLn ("success: " ++ show term)
--     case runSolver (elaborate num_ty Star) of
--         Left error -> putStrLn ("error: " ++ error)
--         Right (ty, solver) -> do
--             putStrLn ("success: " ++ show ty)
--             case runSolver (elaborate zero_term ty) of
--                 Left error -> putStrLn ("error: " ++ error)
--                 Right (term, solver) -> do
--                     putStrLn ("success: " ++ show term)
--             case runSolver (elaborate one_term ty) of
--                 Left error -> putStrLn ("error: " ++ error)
--                 Right (term, solver) -> do
--                     putStrLn ("success: " ++ show term)
--             case runSolver (elaborate two_term ty) of
--                 Left error -> putStrLn ("error: " ++ error)
--                 Right (term, solver) -> do
--                     putStrLn ("success: " ++ show term)
--     case runSolver (elaborate call_id_ty Star) of
--         Left error -> putStrLn ("error: " ++ error)
--         Right (ty, solver) -> do
--             putStrLn ("success: " ++ show ty)
--             case runSolver (elaborate call_id_term ty) of
--                 Left error -> putStrLn ("error: " ++ error)
--                 Right (term, solver) -> do
--                     putStrLn ("success: " ++ show term)
--             case runSolver (elaborate call_id_term2 ty) of
--                 Left error -> putStrLn ("error: " ++ error)
--                 Right (term, solver) -> do
--                     putStrLn ("success: " ++ show term)
--     case runSolver (elaborate call_id_ty2 Star) of
--         Left error -> putStrLn ("error: " ++ error)
--         Right (ty, solver) -> do
--             putStrLn ("success: " ++ show ty)
--             case runSolver (elaborate call_id_term3 ty) of
--                 Left error -> putStrLn ("error: " ++ error)
--                 Right (term, solver) -> do
--                     putStrLn ("success: " ++ show term)
