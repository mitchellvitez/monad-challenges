{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show (Just x) = "Just " ++ show x
  show Nothing = "Nothing"

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay x ((a, b):rest) =
  if x == a then Just b
  else lookupMay x rest

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay a b = if b == 0 then Nothing else Just (a / b)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay xs = headMay $ qsort (>) xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay xs = headMay $ qsort (<) xs

qsort :: (a -> a -> Bool) -> [a] -> [a]
qsort _ [] = []
qsort c (x:xs) =
  h c (flip c) ++ x : h c c
  where h c c' = qsort c $ filter (c' x) xs

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd s =
  case lookupMay s gd of
    Nothing -> Nothing
    Just xs -> case tailMay xs of
      Nothing -> Nothing
      Just t -> case maximumMay t of
        Nothing -> Nothing
        Just m -> case headMay xs of
          Nothing -> Nothing
          Just h -> case divMay (fromIntegral m) (fromIntegral h) of
            Nothing -> Nothing
            Just result -> Just result

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f Nothing = Nothing
chain f (Just x) = f x

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd s =
  lookupMay s gd `link` (\xs -> 
    tailMay xs `link` (\t ->
      maximumMay xs `link` (\m ->
        headMay xs `link` (\h ->
          divMay (fromIntegral m) (fromIntegral h)))))

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries d n1 n2 = 
  lookupMay n1 d `link` (\x ->
    lookupMay n2 d `link` (\y ->
      mkMaybe (x + y)))

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 d n1 n2 =
  yLink (+) (lookupMay n1 d) (lookupMay n2 d)

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f a b =
  a `link` (\x ->
    b `link` (\y ->
      mkMaybe (f x y)))
 
mkMaybe :: a -> Maybe a
mkMaybe x = Just x

tailProd :: Num a => [a] -> Maybe a
tailProd [] = Nothing
tailProd [_] = Just 1
tailProd (_:xs) = Just $ product xs

tailProd2 :: Num a => [a] -> Maybe a
tailProd2 xs = transMaybe product (tailMay xs)

tailSum :: Num a => [a] -> Maybe a
tailSum [] = Nothing
tailSum [_] = Just 0
tailSum (_:xs) = Just $ sum xs

tailSum2 xs = transMaybe sum (tailMay xs)

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f Nothing = Nothing
transMaybe f (Just x) = Just $ f x

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax xs = transMaybe maximumMay (tailMay xs)

tailMin xs = transMaybe minimumMay (tailMay xs)

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing = Nothing
combine (Just Nothing) = Nothing
combine (Just (Just x)) = Just x
