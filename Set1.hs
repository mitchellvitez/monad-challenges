{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands = 
  let s = mkSeed 1
      (a, s1) = rand s
      (b, s2) = rand s1
      (c, s3) = rand s2
      (d, s4) = rand s3
      (e, _) = rand s4
  in [a,b,c,d,e]

randLetter :: Gen Char
randLetter s = (toLetter i, s') 
  where (i, s') = rand s

randString3 :: String
randString3 = 
  let s = mkSeed 1
      (a, s1) = randLetter s
      (b, s2) = randLetter s1
      (c, s3) = randLetter s2
  in [a,b,c]

randEven :: Gen Integer
randEven s = (2 * i, s')
  where (i, s') = rand s

randOdd :: Gen Integer
randOdd s = (i + 1, s')
  where (i, s') = randEven s

randTen :: Gen Integer
randTen s = (10 * i, s')
  where (i, s') = rand s

randEven_generalA = generalA (*2) rand
randOdd_generalA = generalA (+1) randEven_generalA
randTen_generalA = generalA (*10) rand

-- generalA :: (a -> b) -> Seed -> (a, Seed) -> Seed -> (b, Seed)
generalA :: (a -> b) -> Gen a -> Gen b
generalA transform g s = (transform result, s')
  where (result, s') = g s

randPair :: Gen (Char, Integer)
randPair s =
  let (c, s1) = randLetter s
      (i, s2) = rand s1
  in ((c, i), s2)

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair g1 g2 s =
  let (x, s1) = g1 s
      (y, s2) = g2 s1
  in ((x, y), s2)

generalB :: Gen a -> Gen b -> (a -> b -> c) -> Gen c
generalB g1 g2 combine s = 
  let (x, s1) = g1 s
      (y, s2) = g2 s1
  in (combine x y, s2)

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 g1 g2 = generalB g1 g2 (,)

repRandom :: [Gen a] -> Gen [a]
repRandom [] seed = ([], seed)
repRandom (g:gs) seed =
  let (result, newSeed) = g seed 
  in (result : fst (repRandom gs newSeed), newSeed)

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo g1 f seed =
  let (result, newSeed) = g1 seed
  in f result newSeed

mkGen :: a -> Gen a
mkGen x s = (x, s)

-- from Set 4
generalB2 :: Gen a -> Gen b -> (a -> b -> c) -> Gen c
generalB2 g1 g2 f = 
  g1 `genTwo` (\x ->
    g2 `genTwo` (\y ->
      mkGen (f x y)))

repRandom2 :: [Gen a] -> Gen [a]
repRandom2 [] = mkGen []
repRandom2 (g:gs) =
  g `genTwo` (\x ->
    repRandom2 gs)

