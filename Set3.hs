{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TupleSections  #-}

module Set3 where

import MCPrelude

data Card = Card Int String
instance Show Card where
  show (Card r s) = show r ++ s

allPairs :: [a] -> [b] -> [(a,b)]
allPairs [] _ = []
allPairs _ [] = []
allPairs (x:xs) ys = map (x,) ys ++ allPairs xs ys

allPairs2 = allCombs (,)

allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards _ [] = []
allCards (x:xs) ys = map (Card x) ys ++ allCards xs ys

allCards2 = allCombs Card

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ _ [] = []
allCombs _ [] _ = []
allCombs f (x:xs) ys = map (f x) ys ++ allCombs f xs ys

allCombs2 :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs2 _ _ [] = []
allCombs2 _ [] _ = []
allCombs2 f xs ys =
  combStep (map f xs) ys

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 _ _ _ [] = []
allCombs3 _ _ [] _ = []
allCombs3 _ [] _ _ = []
allCombs3 f (x:xs) ys zs =
  concat (map (\g -> map g zs) (map (f x) ys)) ++ allCombs3 f xs ys zs

allCombs3_2 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3_2 _ _ _ [] = []
allCombs3_2 _ _ [] _ = []
allCombs3_2 _ [] _ _ = []
allCombs3_2 f xs ys zs =
  combStep (combStep (map f xs) ys) zs

allCombs3_2' f xs ys zs =
  map f xs `combStep` ys `combStep` zs

allCombs4 f xs ys zs os =
  combStep (combStep (combStep (map f xs) ys) zs) os

allCombs4' f xs ys zs os =
  map f xs `combStep` ys `combStep` zs `combStep` os

combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep (f:fs) xs =
  map f xs ++ combStep fs xs
