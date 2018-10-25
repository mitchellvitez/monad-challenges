{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set5 where

import MCPrelude
import Set2
import Set4

makeRandom :: Gen Integer
makeRandom = Gen { runGen = rand }

fiveRands :: Gen [Integer]
fiveRands = do
  a <- makeRandom
  b <- makeRandom
  c <- makeRandom
  d <- makeRandom
  e <- makeRandom
  return [a,b,c,d,e]

randLetter :: Gen Char
randLetter = do
  i <- makeRandom
  return $ toLetter i

randString3 :: Gen String
randString3 = do
  a <- randLetter
  b <- randLetter
  c <- randLetter
  return [a,b,c]

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair a b = do
  x <- a
  y <- b
  return (x, y)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd s = do
  xs <- lookupMay s gd
  t <- tailMay xs
  m <- maximumMay xs
  h <- headMay xs
  x <- divMay (fromIntegral m) (fromIntegral h)
  return x

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries d n1 n2 = do
  x <- lookupMay n1 d
  y <- lookupMay n2 d
  return $ x + y

tailProd :: Num a => [a] -> Maybe a
tailProd xs = do
  ys <- tailMay xs
  return $ product ys

tailSum :: Num a => [a] -> Maybe a
tailSum xs = do
  ys <- tailMay xs
  return $ sum ys

tailMax :: Ord a => [a] -> Maybe a
tailMax xs = do
  ys <- tailMay xs
  maximumMay ys

data Card = Card Int String
instance Show Card where
  show (Card r s) = show r ++ s

allPairs :: [a] -> [b] -> [(a,b)]
allPairs xs ys = do
  a <- xs
  b <- ys
  return (a, b)

allCards :: [Int] -> [String] -> [Card]
allCards rs ss = do
  r <- rs
  s <- ss
  return $ Card r s

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f xs ys zs = do
  x <- xs
  y <- ys
  z <- zs
  return $ f x y z
