module Test.MySolutions where

import Prelude

import Data.Array (head, tail, sort, nub, concat)
import Data.Foldable (foldM)
import Data.List (List(..),(:),snoc)
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file

-- The motivation for monads is somewhat un-intuitive.
-- I'd rather explain it along map and join for structural change in a monoid

third :: forall a. Array a -> Maybe a
third xs = do
    butFirst <- tail xs
    butSecond <- tail butFirst
    head butSecond

-- follow the types
-- (Int -> Int -> Array Int) -> Int -> Array Int -> Array Int
possibleSums :: Array Int -> Array Int
possibleSums ary = sort $ nub $ foldM combine 0 ary where 
    combine :: Int -> Int -> Array Int
    combine accu cur = [accu, cur, accu + cur]

-- Alternative approach
possibleSums' :: Array Int -> Array Int
possibleSums' ary = sort $ nub $ concat (map combinations ary) where
    combinations x = concat $ map (\y -> [0, x, y, x+y]) ary -- must remove x from ary
                 
-- 3 ) Maybe ap and apply     
{-
-- the applicative <*> for monads
ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
ap f a = do
  f' <- f
  a' <- a
  pure (f' a')

-- specialization for Maybe
ap :: forall a b. => Maybe (a -> b) -> Maybe a -> Maybe b
ap f a = do
  f' <- f                    -- (1)
  a' <- a
  pure (f' a')

-- case discrimination: Nothing _, _ Nothing, Just Just
ap :: forall a b. => Maybe (a -> b) -> Maybe a -> Maybe b
ap Nothing  a = Nothing      -- because (1) shortcuts
ap (Just f) a = do
  f' <- f
  a' <- a                    -- (2) if a is Nothing same as map _ Nothing
  pure (f' a')               -- same as map f x   

-- in toto: same as below
instance applyMaybe :: Apply Maybe where
  apply (Just fn) x = fn <$> x
  apply Nothing   _ = Nothing

-}

-- 4) Monad laws for the Maybe type
{-
monadic bind for Maybe:
Just x  >>= f = f x
Nothing >>= f = Nothing

-- right  identity
expr >>= \x -> pure x    === expr
two cases
 Just x >>= \x -> Just x  -- pure x is Just x
 (\x -> Just x)(x)        -- definition, beta reduction
 Just x                   -- qed

 Nothing >>= _  === Nothing

-- left identity
pure y >>= \x -> next   === next
 Just y >>= \x -> next   -- definition
 (\x -> next)(y)         -- beta reduction
 next                    -- qed

-- associativity
(f >>= g) >>= h  ===  f >>= (g >>= h)

let f be a Just and g,h return some Just 

(Just a >>= \y -> Just b) >>= \z -> Just c   -- definitions for Just values (left)
((\y -> Just b)(a))       >>= \z -> Just c
(Just b)                  >>= \z -> Just c
(\z -> Just c) (b)
Just c

-- nice motivation for the type signature of "bind"
Just a >>= (\y -> Just b >>= \z -> Just c)   -- definitions for Just values (right)
Just a >>= (\y -> (\z -> Just c) b )
Just a >>= (\y -> Just c )
(\y -> Just c )(a)
Just c

Nothing === Nothing 

-}

-- foldM :: forall f m a b. Foldable f => Monad m => (a -> b -> m a) -> a -> f b -> m a
-- foldM :: forall f m a b. Foldable f => Monad m => ((List a) -> b -> m (List a)) -> (List a) -> List b -> m (List a)

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM predicateM xs = foldM foo Nil xs where
    foo accu cur = do
        bool <- predicateM cur
        if (bool) 
            then pure (snoc accu cur)
            else pure accu
  