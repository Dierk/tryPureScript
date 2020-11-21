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

-- Alternative approach (concatMap aka flatMap)
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

{- Reading notes:

ST = state transition or state thread

In ST new, read, write, modify only 'new' returns an STRef, others return only ST
   thus keeping the ref inside the thread
    new     :: forall a r. a                      -> ST r (STRef r a)
    read    :: forall a r. STRef r a              -> ST r a
    write   :: forall a r. a -> STRef r a         -> ST r a
    modify  :: forall r a. (a -> a) -> STRef r a  -> ST r a

  Also nice: dispatching on the return type!

"for" is in the ST effect while Haskell, Frege have it as alias for flip foldM_  

In run :: forall a. (forall r. ST r a) -> a
r is a higher-rank type. 

Note on generated JS code for "simulate": would be nicer to generate
const (instead of var) and arrow functions.

-}

-- safeDevide to exceptionDivide
-- note: why should assertException be a problem? 
{-
    assertException callback = do            -- might have type issue here
      result <- try callback
      case result of 
        Left   expectedError -> assert true  
        Right  _             -> assert false -- maybe better error indication
-} 
  
-- ST exercise would most likely depend on some "outer structure" template
-- where the student must fill in the blanks. Game of life? Ackermann funktion? Turtle graphics?
-- Alternative: just disallow recursion and folding.  

-- <Address book UI> 

-- putting div in a label ? (formField)

-- (1) which language feature allows the phone "type" introspection?

{-
    Making field validation errors local to a field requires a field abstraction like

    data Field = FirstNameField
               | LastNameField
               | StreetField
               | CityField
               | StateField
               | PhoneField PhoneType

    but this is kind of the wrong abstraction as it is prone to massive duplication.
    We rather need a presentation model made from rich attributes that capture observable props like
    validation, errors, value, mandatory, dirty, placeholder, etc.
    => rich functional UIs
-}

