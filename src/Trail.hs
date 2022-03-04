module Trail where

data Trail a = B0 | Trail a :< a
    deriving (Eq,Show)

instance Functor Trail where
    fmap f B0 = B0
    fmap f (xs:<x) = fmap f xs :< f x

instance Foldable Trail where
    foldr f y B0 = y
    foldr f y (xs:<x) = foldr f (f x y) xs

instance Traversable Trail where
    sequenceA B0 = pure B0 
    sequenceA (xs:<x) = pure (:<) <*> sequenceA xs <*> x

(<.>) :: Trail a -> Trail a -> Trail a
a <.> B0 = a
a <.> (xs:<x) = (a <.> xs) :< x

(!!!) :: Trail a -> Int -> a
(!!!) B0 i = error "overshoot"
(!!!) (xs:<x) 0 = x
(!!!) (xs:<x) n = xs !!! (n-1)
