module Transform where

-- http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.45.3876

import Control.Monad.ST
import Data.STRef
import Data.Map (Map, lookup, keys, mapWithKey, alter, empty,
                 fromList)
import qualified Data.Set as Set

(!) :: Graph -> Entry -> [Entry]
(!) g e = f (Data.Map.lookup e g)
    where f (Just a) = a
          f Nothing = []

data Entry = TypeOf String
           | DeclOf String
           deriving (Show,Eq,Ord)

type Table a = Map Entry a
type Graph = Table [Entry]

vertices :: Graph -> [Entry]
vertices = keys

type Edge = (Entry, Entry)

edges :: Graph -> [Edge]
edges g = [ (v,w) | v <- vertices g, w <- g ! v ]

mapT :: (Entry -> a -> b) -> Table a -> Table b
mapT = mapWithKey 

outdegree :: Graph -> Table Int
outdegree g = mapT f g
              where f _ ws = length ws

buildG :: [Edge] -> Graph
buildG es = Prelude.foldr f empty es
    where f :: Edge -> Graph -> Graph
          f (v,w) = alter (put w) v
          put w (Just q) = Just (w:q)
          put w Nothing = Just [w]

transposeG :: Graph -> Graph
transposeG g = buildG (reverseE g)

reverseE :: Graph -> [Edge]
reverseE g = [ (w,v) | (v,w) <- edges g ]

indegree :: Graph -> Table Int
indegree g = outdegree (transposeG g)

data Tree a = Node a (Forest a)
    deriving (Show)
type Forest a = [Tree a]

dff :: Graph -> Forest Entry
dff g = dfs g (vertices g)

preorder :: Tree a -> [a]
preorder (Node a ts) = [a] ++ preorderF ts

preorderF :: Forest a -> [a]
preorderF ts = concat (Prelude.map preorder ts)

preOrd :: Graph -> [Entry]
preOrd g = preorderF (dff g)

tabulate :: [Entry] -> Table Int
tabulate vs = fromList (zip vs [1..])

preArr :: Forest Entry -> Table Int
preArr ts = tabulate (preorderF ts)


postorder :: Tree a -> [a]
postorder (Node a ts) = postorderF ts ++ [a]

postorderF :: Forest a -> [a]
postorderF ts = concat (Prelude.map postorder ts)

postOrd :: Graph -> [Entry]
postOrd g = postorderF (dff g)

topSort :: Graph -> [Entry]
topSort g = reverse (postOrd g)

components :: Graph -> Forest Entry
components g = dff (undirected g)

undirected :: Graph -> Graph
undirected g = buildG (edges g ++ reverseE g)

scc :: Graph -> Forest Entry
scc g = dfs (transposeG g) (reverse (postOrd g))

scc' :: Graph -> Forest Entry
scc' g = dfs g (reverse (postOrd (transposeG g)))

generate :: Graph -> Entry -> Tree Entry
generate g v = Node v (Prelude.map (generate g) (g!v))

type Set s = STRef s (Set.Set Entry)

mkEmpty :: ST s (Set s)
mkEmpty = newSTRef (Set.empty)

contains :: Set s -> Entry -> ST s Bool
contains ref e = Set.member e <$> readSTRef ref

include :: Set s -> Entry -> ST s ()
include ref e = modifySTRef' ref (Set.insert e)

prune :: Forest Entry -> Forest Entry
prune ts = runST (mkEmpty >>= \m -> chop m ts)

chop :: Set s -> Forest Entry -> ST s (Forest Entry)
chop m [] = pure []
chop m (Node v ts : us) = do
    visited <- contains m v
    if visited then
       chop m us
    else do
       include m v
       as <- chop m ts
       bs <- chop m us
       pure ((Node v as) : bs)

dfs :: Graph -> [Entry] -> Forest Entry
dfs g vs = prune (map (generate g) vs)
