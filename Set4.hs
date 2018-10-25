{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Set2

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

class Monad m where
  bind :: m a -> (a -> m b) -> m b
  return :: a -> m a

  (>>=) :: m a -> (a -> m b) -> m b
  (>>=) = bind

  fail :: String -> m a
  fail = undefined

instance Monad Maybe where
  return = Just
  bind (Just x) f = f x
  bind Nothing _ = Nothing

instance Monad [] where
  return x = [x]
  bind [] _ = []
  bind (x:xs) f = f x ++ bind xs f

instance Monad Gen where
  return a = Gen (\s -> (a, s))
  bind g f = Gen (genMaker g f)
    where genMaker g f s =
      runGen (f x) s'
      where (x, s') = runGen g s

evalGen :: Gen a -> Seed -> a
evalGen g s = fst $ runGen g s

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (x:xs) =
  x `bind` (\a ->
    sequence xs `bind` (\bs ->
      return $ a:bs))
  
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f a b =
  a `bind` (\x ->
    b `bind` (\y ->
      return $ f x y))

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) f a =
  a `bind` (\x ->
    f x)

join :: Monad m => m (m a) -> m a
join a =
  a `bind` id 

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f a b c =
  a `bind` (\x ->
    b `bind` (\y ->
      c `bind` (\z ->
        return $ f x y z)))

ap :: Monad m => m (a -> b) -> m a -> m b
ap f a =
  a `bind` (\x ->
    f `bind` (\g ->
      return $ g x))
