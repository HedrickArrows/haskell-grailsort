{- MIT License
-
- Copyright (c) 2013 Andrey Astrelin
- Copyright (c) 2020 The Holy Grail Sort Project
- 
- Permission is hereby granted, free of charge, to any person obtaining
- a copy of this software and associated documentation files (the
- "Software"), to deal in the Software without restriction, including
- without limitation the rights to use, copy, modify, merge, publish,
- distribute, sublicense, and/or sell copies of the Software, and to
- permit persons to whom the Software is furnished to do so, subject to
- the following conditions:
- 
- The above copyright notice and this permission notice shall be included
- in all copies or substantial portions of the Software.
- 
- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

{- 
The Holy Grail Sort Project

Project Manager:      Summer Dragonfly
Project Contributors: 666666t
                      Amari Calipso
                      Anonymous0726
                      aphitorite
                      Control
                      dani_dlg
                      DeveloperSort
                      EilrahcF
                      Enver
                      Gaming32
                      lovebuny
                      Morwenn
                      MP
                      phoenixbound
                      Spex_guy
                      _fluffyy
                               
                  
Special thanks to "The Studio" Discord community! 
-}

{-
Tester for GrailSort's implementation in Haskell by Mihnik a.k.a Hedrick Arrows
It is quite a mess, I know, but that's because a lot of stuff was tested directly here

Current status: Done
    - More tests to be implemented in the future
-}

{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where
import GrailSort ( grailSortInPlace, grailSortStaticOOP, grailSortDynamicOOP )
import GrailSort.Naive ( grailSortInPlace, grailSortStaticOOP, grailSortDynamicOOP )
import System.Random.Shuffle ( shuffleM )
import Control.Monad ( forM )
import Control.Exception ( SomeException, catch, evaluate )
import Data.Foldable ( Foldable(foldl') )
import Data.Sort (sort)

allSorted :: Ord a => [a] -> Bool
allSorted xs = and $ zipWith (<=) xs (tail xs)

main :: IO ()
main = do
    let max = 100
    putStrLn "Shuffled lists (no duplicates)"
    generalLoop max System.Random.Shuffle.shuffleM GrailSort.Naive.grailSortInPlace "In-Place (Naive)"
    generalLoop max System.Random.Shuffle.shuffleM GrailSort.Naive.grailSortStaticOOP "Out-Of-Place, Static Buffer (Naive)"
    generalLoop max System.Random.Shuffle.shuffleM GrailSort.Naive.grailSortDynamicOOP "Out-Of-Place, Dynamic Buffer (Naive)"

    generalLoop max System.Random.Shuffle.shuffleM GrailSort.grailSortInPlace "In-Place"
    generalLoop max System.Random.Shuffle.shuffleM GrailSort.grailSortStaticOOP  "Out-Of-Place, Static Buffer"
    generalLoop max System.Random.Shuffle.shuffleM GrailSort.grailSortDynamicOOP "Out-Of-Place, Dynamic Buffer"

    putStrLn "\nReversed lists (no duplicates)"
    generalLoop max (return . reverse) GrailSort.Naive.grailSortInPlace "In-Place (Naive)"
    generalLoop max (return . reverse) GrailSort.Naive.grailSortStaticOOP  "Out-Of-Place, Static Buffer (Naive)"
    generalLoop max (return . reverse) GrailSort.Naive.grailSortDynamicOOP "Out-Of-Place, Dynamic Buffer (Naive)"

    generalLoop max (return . reverse) GrailSort.grailSortInPlace "In-Place"
    generalLoop max (return . reverse) GrailSort.grailSortStaticOOP  "Out-Of-Place, Static Buffer"
    generalLoop max (return . reverse) GrailSort.grailSortDynamicOOP "Out-Of-Place, Dynamic Buffer"

    putStrLn "\nGrailSort Adversary (no duplicates)"
    generalLoop max grailSortAdversary GrailSort.Naive.grailSortInPlace "In-Place (Naive)"
    generalLoop max grailSortAdversary GrailSort.Naive.grailSortStaticOOP  "Out-Of-Place, Static Buffer (Naive)"
    generalLoop max grailSortAdversary GrailSort.Naive.grailSortDynamicOOP "Out-Of-Place, Dynamic Buffer (Naive)"

    generalLoop max grailSortAdversary GrailSort.grailSortInPlace "In-Place"
    generalLoop max grailSortAdversary GrailSort.grailSortStaticOOP  "Out-Of-Place, Static Buffer"
    generalLoop max grailSortAdversary GrailSort.grailSortDynamicOOP "Out-Of-Place, Dynamic Buffer"

    putStrLn  "All tasks done"


generalLoop :: (Enum a, Show a1, Foldable t1, Num t2, Ord a1, Num a) => a -> ([a] -> IO (t1 a3)) -> (t1 a3 -> t2 -> Int -> [a1]) -> [Char] -> IO ()
generalLoop high input sort name = do
    res <- forM [lower..high] $ \item -> do
        list <- input [1..item]
        testFunc sort list
    printResults name (foldl' (\(a,b,c) (d,e,f) -> (a+d, b+e, c+f)) (0,0,0) res)
    where lower = 16

printResults :: [Char] -> (Int, Int, Int) -> IO ()
printResults name (_,0,0) = putStrLn (name ++ " ran succesfully.")
printResults name (_, f, e) =
    let
        fails = if f == 0 then "" else show f ++" failures "
        errors = if e == 0 then "" else show e ++" errors"
    in putStrLn (name ++ " encountered problems: " ++ fails ++ errors)


testFunc :: (Num a2, Num b, Num c, Show a1, Foldable t1, Num t2, Ord a1) => (t1 a3 -> t2 -> Int -> [a1]) -> t1 a3 -> IO (a2, b, c)
testFunc foo testList = do
    let val = length testList
    let eq = foo testList 0 val
    res <- (evaluate eq >> return True) `catch` handler
    if res
      then
        if allSorted eq
        then do
            --putStrLn $ "List " ++ show val ++ " sorted"
            return (1,0,0)
        else do
            --let spot = testSpot (head eq) (tail eq) 0
            --putStrLn $ "List " ++ show val ++ " not sorted (starting on pos " ++ show spot  ++ ") " -- ++ show eq
            return (0,1,0)
          --print eq
    else do
      --putStrLn $ "Value: " ++ show val
      return (0,0,1)
    where
      handler :: SomeException -> IO Bool
      handler ex = do
        --putStrLn "Exception caught "
        --print ex
        return False



testSpot :: (Ord a) => a -> [a] -> Int -> Int
testSpot _ [] _ = -1
testSpot h t i =
    let ht = head t
        tt = tail t
    in if h <= ht
        then testSpot ht tt (i + 1)
        else i


------- grailsort adversary code

blockSwap :: [a] -> Int -> Int -> Int -> [a]
blockSwap list a b blockLen =
    let
      x = min a b
      y = max a b
      diff = y - x
      rest = blockLen - diff
    in
        if x + blockLen > y
        then take x list ++ take blockLen (drop y list) ++ take diff (drop rest $ cycle (take diff $ drop x list)) ++ drop (y + blockLen) list
        else take x list ++ take blockLen (drop y list) ++ take (diff - blockLen) (drop (x + blockLen) list) ++ take blockLen (drop x list) ++ drop (y + blockLen) list

rotateGriesMills :: [a] -> Int -> Int -> Int -> [a]
rotateGriesMills list start leftLen rightLen
  | leftLen > 0 && rightLen > 0 =
    if leftLen <= rightLen
    then let list' = blockSwap list start (start + leftLen) leftLen in
        rotateGriesMills list' (start + leftLen) leftLen (rightLen - leftLen)
    else let list' = blockSwap list (start + leftLen - rightLen) (start + leftLen) rightLen in
        rotateGriesMills list' start (leftLen - rightLen) rightLen
  | otherwise = list

mockShift :: [a] -> Int -> Int -> [a]
mockShift list pos to
  | to - pos > 0 =
    take pos list ++ mockMove (drop pos list) (to - pos)
  | otherwise =
    take to list ++ mockPull (drop to list) (pos - to)
  where
    mockMove list@(lh:ls:lt) i
      | i == 0 = list
      | otherwise = ls:mockMove (lh:lt) (i-1)
    mockPull list@(lh:lt) i
      | i == 0 = list
      | otherwise = let (sh:st) = mockPull lt (i-1) in (sh:lh:st)

push :: [a] -> Int -> Int -> Int -> [a]
push list a b bLen = let
  len = b-a
  b1 = b - len `rem` bLen
  m = a + until (\x -> x * 2 >= len) (*2) bLen 
  len1 = b1 - a
  m' = a+b1-m
  list' = rotateGriesMills list (m'-(bLen-2)) (b1-(bLen-1)) b1
  list'' = mockShift list' a m'
  list''' = rotateGriesMills list'' a m' b1
  m'' = a + b1 - m'
  list'''' = push list''' a m'' bLen
  in if len1 < 2*bLen
    then list
    else if b1 - m <bLen
      then push list a m bLen
      else push list'''' m'' b bLen

--adding the line below results in the linter being angry
--grailSortAdversary :: (MonadRandom-0.6.2:Control.Monad.Random.Class.MonadRandom m,  Ord a) => [a] -> m [a]
grailSortAdversary list
  | length list < 16 = return (reverse list)
  | otherwise = let
    len = length list
    bLen = until (\x -> x*x >= len) (*2) 1
    numKeys = (len - 1) `quot` 2 + 1
    keys = bLen + numKeys
    in do
      list' <- System.Random.Shuffle.shuffleM list
      let list'' = reverse (Data.Sort.sort (take keys list')) ++ Data.Sort.sort (drop keys list')
      return $ push list'' keys len bLen