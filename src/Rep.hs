module Rep where

import Trail
import Data.Set (Set)
import qualified Data.Set as Set

data Statement
    = Import String [(String, String)]
    | Export [(String,String)]
    | TypeDecl String STerm
    | Decl String STerm
    deriving (Show,Eq)

data STerm = SName String
           | SCall STerm [(String, STerm)] STerm
           | SAbs [String] String STerm
           | SPi [(String, STerm)] String STerm STerm
           | SAnn STerm STerm
           | SHole
           | SStar
           | SEq STerm STerm STerm
           | SIntegerConst Integer
           | SRealConst String
           | SStringConst String
           | SBind String STerm STerm
           | SField STerm String
           | SCase STerm STerm [(String, STerm)]
           | SCocase [(String, STerm)]
           | SORecord [STerm]
           | SLRecord [(String, STerm)] (Maybe STerm)
    deriving (Show,Eq)

-- #lexemes id, integer, real, string
-- # data Statements
-- DataDecl = lambda name, parms, strucs: obj.Data(4, [name, parms, strucs])
-- CodataDecl = lambda name, parms, strucs: obj.Data(5, [name, parms, strucs])
-- RecordDecl = lambda head, parms, strucs: obj.Data(6, [head, parms, strucs])
-- ClassDecl = lambda head, parms, strucs: obj.Data(7, [head, parms, strucs])
-- 
-- # data Term
-- Rigid = lambda rigid,trail: obj.Data(0, [rigid, trail])
-- Flex = lambda meta,trail: obj.Data(1, [meta, trail])
-- Abs = lambda body: obj.Data(2, [body])
-- Pi = lambda a, b, hidden: obj.Data(3, [a, b, hidden])
-- Star = obj.Data(4, [])
-- Cocase = lambda binds: obj.Data(5, [binds])
-- Record = lambda fields: obj.Data(6, [fields])
-- Eq = lambda a,x,y: obj.Data(7, [a,x,y])
-- Let = lambda arg,body: obj.Data(8, [arg,body])
-- IntegerConst = lambda num: obj.Data(9, [num])
-- RealConst = lambda num: obj.Data(10, [num])
-- StringConst = lambda string: obj.Data(11, [string])

data Term
    = Rigid Head (Elims Term)
    | Flex Meta (Elims Term)
    | Abs Term
    | Record [Term]
    | Pi Term Term
    | Star
    | RecordType [(String, Term)]
    deriving (Eq,Show)

type Elims a = Trail (Elim a)
data Elim a = App a | Field Int
    deriving (Eq,Show)

data Head
    = Unquoted Int
    | Closed Int
    | Const String
    | Unavailable String
    deriving (Eq,Show)

newtype Meta = Meta Int
    deriving (Eq,Show,Ord)

type Type = Term
 
instance Functor Elim where
    fmap f (App a) = App (f a)
    fmap f (Field i) = Field i
 
instance Foldable Elim where
    foldMap f (App a) = (f a)
    foldMap f (Field i) = mempty


wrapAbs :: Int -> Term -> Term
wrapAbs 0 term = term
wrapAbs n term = wrapAbs (n-1) (Abs term)

-- ------------------------------------------------------------
-- -- Binding
-- ------------------------------------------------------------
-- class Binding t where
--     binding :: Int -> (Int -> RName -> RName) -> t -> t
-- 
-- instance Binding Term where
--     binding i f (Rigid h e) = Rigid (f i h) (fmap (fmap (binding i f)) e)
--     binding i f (Flex m e) = Flex m (fmap (fmap (binding i f)) e)
--     binding i f (Abs e) = Abs (binding i f e)
--     binding i f (Pi t t') = Pi (binding i f t) (binding i f t')
--     binding i f (Sigma t t') = Sigma (binding i f t) (binding i f t')
--     binding i f (Pair a b) = Pair (binding i f a) (binding i f b)
--     binding i f Star = Star
--  
-- instance Binding t => Binding (Bind t) where
--     binding i f (Bind t) = Bind (binding (i+1) f t)
--  
-- bind :: Binding t => Name -> t -> Bind t
-- bind k v = Bind (binding 0 help v)
--     where help :: Int -> RName -> RName
--           help i (Bound k') | (k == k') = (Closed i)
--           --help i (Meta k') | (k == k') = Var (Closed i)
--           help i head = head
-- 
-- unbind :: Binding t => Name -> Bind t -> t
-- unbind name (Bind v) = binding 0 help v
--     where help :: Int -> RName -> RName
--           help i (Closed j) | (i == j) = (Bound name)
--           help i head = head
-- 
-- loosen :: Binding t => t -> t
-- loosen v = binding 0 help v
--     where help :: Int -> RName -> RName
--           help i (Closed j) | (i <= j) = (Closed (j+1))
--           help i head = head
-- 
-- tighten :: Binding t => Int -> t -> t
-- tighten k v = binding k help v
--     where help :: Int -> RName -> RName
--           help i (Closed j) | (i < j) = (Closed (j-1))
--           help i (Closed j) | (i == j) = error "bug, did you use fv?"
--           help i head = head
-- 
-- tighten' :: Binding t => Trail Int -> t -> t
-- tighten' ts v = binding 0 help v
--     where help :: Int -> RName -> RName
--           help i (Closed j) | (i <= j) = Closed (j - sum (take' (j-i) ts))
--           help i head = head
-- 
-- take' :: Int -> Trail a -> [a]
-- take' n B0 = []
-- take' 0 (xs:<x) = []
-- take' n (xs:<x) = x : take' (n-1) xs
-- 
-- raise v k = binding 0 help v
--     where help :: Int -> RName -> RName
--           help i (Closed j) | (i <= j) = (Closed (j+k))
--           help i head = head
-- 
-- -- subst :: Binding t => Head -> Bind t -> t
-- -- subst head (Bind v) = binding 0 help v
-- --     where help :: Int -> Head -> Head
-- --           help i (Var (Closed j)) | (i == j) = head
-- --           help i head = head
-- 
-- -- ------------------------------------------------------------
-- -- -- Value forcing
-- -- ------------------------------------------------------------
-- -- force :: Subs -> Value -> Value
-- -- force ctx (VFlex m sp) = case lookup ( m) ctx of
-- --     Just v -> vsp v sp
-- --     Nothing -> VFlex m sp
-- -- force ctx v = v
--  
-- ------------------------------------------------------------
-- -- Quotation
-- ------------------------------------------------------------
-- quote0 :: Value -> Term
-- quote0 = quote 0
-- 
-- ctxToScope :: Ctx -> [Value]
-- ctxToScope ctx = snd (help ctx)
--     where help :: Ctx -> (Int, [Value])
--           help B0 = (0, [])
--           help (xs:<x) = let (n, ys) = help xs
--                          in (n+1, vunquoted n : ys)
-- 
-- class Nf a where
--     nf :: Env -> a -> a
-- 
-- instance Nf Term where
--     nf env t = quote (length (scope env)) (eval env t)
-- 
-- instance Nf a => Nf (Bind a) where
--     nf env (Bind a) = Bind (nf (extend (vunquoted (length (scope env))) env) a)
-- 
-- instance Nf a => Nf [a] where
--     nf env xs = fmap (nf env) xs
-- 
-- -- Some weirdness
-- -- bonk :: Env
-- -- bonk = Env Map.empty []
-- -- 
-- -- ($$) :: Term -> Term -> Term
-- -- f $$ a = quote0 $ vapp (eval bonk f) (eval bonk a)
-- -- 
-- -- hd :: Term -> Term
-- -- hd a = quote0 $ vfst (eval bonk a)
-- -- 
-- -- tl :: Term -> Term
-- -- tl a = quote0 $ vsnd (eval bonk a)
--  
-- inst :: Env -> Bind Term -> Term -> Term
-- inst env a b = quote (length (scope env)) $
--                  vapp (eval env (Abs a)) (eval env b)
-- -- 
-- -- (%%) :: Term -> Elim Term -> Term
-- -- t %% e = quote0 $ vsp (eval bonk t) (B0:<(fmap (eval bonk) e))
--  
-- -- Rather than definining functions to determine the free metavariables
-- -- and variables of terms directly, I use a typeclass to make them
-- -- available on the whole syntax.
-- data Flavour = Vars | RigVars | Metas
--  
-- class Occurs t where
--     free   :: Flavour -> t -> Set Name
-- 
--
--
-- fvs, fvrigs :: Occurs t => t -> Set Name
-- fvs       = free Vars
-- fvrigs    = free RigVars

fcv :: STerm -> Set String
fcv (SName name) = Set.singleton name
fcv (SCall a record b) = fcv a <> fcv b <> foldMap fcv (map snd record)
fcv (SAbs ss s a) = Set.delete s (fcv a) `Set.difference` Set.fromList ss
fcv (SPi ss s a b) = scopeDance ss (fcv a <> (Set.delete s (fcv b)))
fcv (SAnn a b) = fcv a <> fcv b
fcv SHole = mempty
fcv SStar = mempty
fcv (SEq a x y) = fcv a <> fcv x <> fcv y
fcv (SIntegerConst i) = mempty
fcv (SRealConst i) = mempty
fcv (SStringConst i) = mempty
fcv (SBind name a b) = fcv a <> Set.delete name (fcv b)
fcv (SField a _) = fcv a
fcv (SCase a b ss) = fcv a <> fcv b <> foldMap fcv (map snd ss)
fcv (SCocase ss) = foldMap fcv (map snd ss)
fcv (SORecord terms) = foldMap fcv terms
fcv (SLRecord fts (Just a)) = foldMap fcv (map snd fts) <> fcv a
fcv (SLRecord fts Nothing) = foldMap fcv (map snd fts)

scopeDance :: [(String, STerm)] -> Set String -> Set String
scopeDance [] f = f
scopeDance ((name,sterm):xs) f = let f' = scopeDance xs f in
    fcv sterm <> (Set.delete name f')


fmvs :: Term -> Set Meta
fmvs (Rigid head elims) = foldMap (foldMap fmvs) elims
fmvs (Flex name elims) = Set.singleton name <> foldMap (foldMap fmvs) elims
fmvs (Abs term) = fmvs term
fmvs (Record terms) = foldMap fmvs terms
fmvs (Pi a b) = fmvs a <> fmvs b
fmvs Star = mempty
fmvs (RecordType fterms) = foldMap fmvs (map snd fterms)

fv :: Term -> Set Int
fv (Rigid (Closed i) elims) = Set.singleton i <> foldMap (foldMap fv) elims
fv (Rigid head elims) = foldMap (foldMap fv) elims
fv (Flex name elims) = foldMap (foldMap fv) elims
fv (Abs term) = bindFv term
fv (Record terms) = foldMap fv terms
fv (Pi a b) = fv a <> bindFv b
fv Star = mempty
fv (RecordType fterms) = foldMap fv (map snd fterms)

bindFv a = (-1) `Set.delete` Set.map (\x -> x-1) (fv a)

-- instance Occurs Term where
--     free RigVars  (Rigid (Bound x) e) = singleton x `union` free RigVars e
--     free RigVars  (Flex i _) = Set.empty
--     free Vars       (Flex _ e)      = free Vars e
--     --free RigVars    (Flex _ e)      = free RigVars e
--     free Metas      (Flex alpha e)  = singleton alpha `union` free Metas e
--     free Vars       (Rigid (Bound x) e) = singleton x `union` free Vars e
--     --free RigVars    (Rigid (Bound x) e) = singleton x `union` free RigVars e
--     free Metas      (Rigid (Bound _) e) = Set.empty `union` free Metas e
--     free l          (Rigid _ e)         = Set.empty `union` free l e
--     free l (Abs b) = free l b
--     free l (Pi s t) = free l s `union` free l t
--     free l (Sigma s t) = free l s `union` free l t
--     free l (Pair s t) = free l s `union` free l t
--     free l Star = Set.empty
--  
-- instance (Occurs t, Occurs u, Occurs v) => Occurs (t,u,v) where
--     free l (a,b,c) = unions [free l a, free l b, free l c]
-- 
-- instance Occurs t => Occurs [t] where
--     free l = unions . map (free l)
-- 
-- instance Occurs t => Occurs (Trail t) where
--     free l B0 = Set.empty
--     free l (xs :< x) = free l xs `union` free l x
-- 
-- instance Occurs t => Occurs (Elim t) where
--     free l (App a)       = free l a
--     free l Fst           = Set.empty
--     free l Snd           = Set.empty
-- 
-- instance Occurs a => Occurs (Bind a) where
--     free l (Bind a) = free l a
-- 
-- class Fv a where
--     fv :: a -> Set Int
--     fvr :: a -> Set Int
-- 

fvr :: Term -> Set Int
fvr (Rigid (Closed i) elims) = Set.singleton i <> foldMap (foldMap fvr) elims
fvr (Rigid head elims) = foldMap (foldMap fvr) elims
fvr (Flex name elims) = mempty
fvr (Abs term) = bindFvr term
fvr (Record terms) = foldMap fvr terms
fvr (Pi a b) = fvr a <> bindFvr b
fvr Star = mempty
fvr (RecordType fterms) = foldMap fvr (map snd fterms)

bindFvr a = (-1) `Set.delete` Set.map (\x -> x-1) (fvr a)

-- instance Fv Term where
--     fv (Rigid h e) = fv h `union` fv e
--     fv (Flex _ e) = fv e
--     fv (Abs b) = fv b
--     fv (Pi s t) = fv s `union` fv t
--     fv (Sigma s t) = fv s `union` fv t
--     fv (Pair s t) = fv s `union` fv t
--     fv Star = Set.empty
-- 
--     fvr (Rigid h e) = fvr h `union` fvr e
--     fvr (Flex _ e) = Set.empty
--     fvr (Abs b) = fvr b
--     fvr (Pi s t) = fvr s `union` fvr t
--     fvr (Sigma s t) = fvr s `union` fvr t
--     fvr (Pair s t) = fvr s `union` fvr t
--     fvr Star = Set.empty
-- 
-- instance Fv RName where
--     fv (Closed i) = singleton i
--     fv _ = Set.empty
-- 
--     fvr (Closed i) = singleton i
--     fvr _ = Set.empty
-- 
-- instance Fv a => Fv (Trail a) where
--     fv B0 = Set.empty
--     fv (xs:<x) = fv xs `union` fv x
--     fvr B0 = Set.empty
--     fvr (xs:<x) = fvr xs `union` fvr x
-- 
-- instance Fv a => Fv (Elim a) where
--     fv (App a) = fv a
--     fv Fst     = Set.empty
--     fv Snd     = Set.empty
--     fvr (App a) = fvr a
--     fvr Fst     = Set.empty
--     fvr Snd     = Set.empty
-- 
-- instance Fv a => Fv (Bind a) where
--     fvr (Bind a) = (-1) `Set.delete` Set.map (\x -> x-1) (fvr a)
-- 
-- 
-- data Decl v    = HOLE | DEFN v
--     deriving (Eq, Show)
-- type Entry = (Type, Decl Term)
-- 
-- -- Metavariables in the signature
-- --support (xs:<_) = support xs
-- 
-- -- I don't have atom decls, for now, but they would go here.
-- -- atomDecls :: Signature -> Set Name
-- -- atomDecls B0 = Set.empty
-- -- atomDecls (xs:<SA name _) = Set.insert name (atomDecls xs)
-- -- atomDecls (xs:<_) = atomDecls xs
-- 
-- -- decls :: Signature -> Set Name
-- -- decls xs = support xs `union` atomDecls xs
-- 
-- metas :: Occurs t => t -> Set Name
-- metas = fmvs
-- 
-- -- atoms :: Occurs t => t -> Set Name
-- -- atoms = fvs -- I'm not certain these are atoms.
-- 
-- -- consts :: Occurs t => t -> Set Name
-- -- consts t = metas t `union` atoms t


fromTelescope :: Trail Type -> Type -> Type
fromTelescope B0 ty = ty
fromTelescope (xs:<x) ty = fromTelescope xs (Pi x ty)

vector :: Int -> Int -> Elims Term
vector i 0 = B0
vector i n = vector (i+1) (n-1) :< App (Rigid (Closed i) B0)
