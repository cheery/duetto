module Evaluator where

import Trail
import Rep
import qualified Data.Map as Map
import Data.Map (Map)

data Ns = Ns {
    constants :: Map String (Bool, Value),
    subst :: Map Meta Term,
    activated :: Bool }
type Env = [Value]

data Value
    = VRigid Head (Elims Value)
    | VFlex Meta (Elims Value)
    | VAbs (Value -> Value)
    | VRecord [Value]
    | VPi Value (Value -> Value)
    | VStar
    | VRecordType [(String, Value)]
 
eval :: Ns -> Env -> Term -> Value
eval ns env (Rigid h e) = vsp (rigidEval ns env h) (fmap (fmap (eval ns env)) e)
eval ns env (Flex m e) = vsp (metaEval ns m) (fmap (fmap (eval ns env)) e)
eval ns env (Abs e) = VAbs (bindEval ns env e)
eval ns env (Record xs) = VRecord (fmap (eval ns env) xs)
eval ns env (Pi a b) = VPi (eval ns env a) (bindEval ns env b)
eval ns env (RecordType xs) = VRecordType (fmap (fieldEval ns env) xs)
eval ns env Star = VStar

rigidEval :: Ns -> Env -> Head -> Value
rigidEval ns env (Closed i) = env !! i
rigidEval ns env name = vpar ns name

metaEval :: Ns -> Meta -> Value
metaEval ns meta = case Map.lookup meta (subst ns) of
    Just term -> eval ns [] term
    Nothing -> VFlex meta B0

fieldEval :: Ns -> Env -> (String, Term) -> (String, Value)
fieldEval ns env (name,term) = (name, eval ns env term)

vpar :: Ns -> Head -> Value
vpar ns (Const name) = case Map.lookup name (constants ns) of
    Just (inline, value) | (inline || activated ns) -> value
    _ -> VRigid (Const name) B0
vpar ns head = VRigid head B0

bindEval :: Ns -> Env -> Term -> (Value -> Value)
bindEval ns env term x = eval ns (x:env) term

vsp :: Value -> (Elims Value) -> Value
vsp head B0 = head
vsp head (xs:<App arg) = vsp head xs `vapp` arg
vsp head (xs:<Field i) = vsp head xs `vfield` i
 
vapp :: Value -> Value -> Value
vapp (VAbs f)       v = f v
vapp (VFlex i e)    v = VFlex i (e:<App v)
vapp (VRigid n e)   v = VRigid n (e:<App v)
vapp _              v = error "runtime type error"
  
vfield :: Value -> Int -> Value
vfield (VRecord xs) k = (xs !! k)
vfield (VFlex i e)  k = VFlex i (e:<Field k)
vfield (VRigid n e) k = VRigid n (e:<Field k)
vfield _            _ = error "runtime type error"

vunquoted :: Int -> Value
vunquoted i = VRigid (Unquoted i) B0

vunquoteds :: Int -> [Value]
vunquoteds i = map vunquoted [i-1,i-2..0]

quote :: Int -> Value -> Term
quote i (VFlex j e) = Flex j (fmap (fmap (quote i)) e)
quote i (VRigid h e) = Rigid (varpar i h) (fmap (fmap (quote i)) e)
quote i (VAbs f) = Abs (bindQuote i f)
quote i (VRecord xs) = Record (fmap (quote i) xs)
quote i (VPi a f) = Pi (quote i a) (bindQuote i f)
--quote i (VSigma a f) = Sigma (quote i a) (bindQuote i f)
--quote i (VPair a b) = Pair (quote i a) (quote i b)
quote i VStar = Star
quote i (VRecordType fxs) = RecordType (fmap (fieldQuote i) fxs)
 
bindQuote :: Int -> (Value -> Value) -> Term
bindQuote i f = (quote (i+1) (f (vunquoted i)))

fieldQuote :: Int -> (String, Value) -> (String, Term)
fieldQuote i (name, value) = (name, quote i value)
  
varpar :: Int -> Head -> Head
varpar i (Unquoted k) = Closed (i - k - 1)
varpar i x            = x

tighten' :: Trail Int -> Term -> Term
tighten' xs t =
    let depth = length xs - sum xs
        env = fst (tightener xs)
    in quote depth (eval (Ns Map.empty Map.empty False) env t)

tightener :: Trail Int -> ([Value], Int)
tightener B0 = ([], 0)
tightener (xs:<i) = let (ys,n) = tightener xs
                    in case i of
                        0 -> (vunquoted n:ys, n+1)
                        _ -> (vunquoted n:ys, n)
