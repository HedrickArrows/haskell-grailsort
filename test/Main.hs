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
                      thatsOven
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
module Main (main) where
import GrailSort ( grailSortInPlace, grailSortStaticOOP, grailSortDynamicOOP )
import GrailSort.Naive ( grailSortInPlace, grailSortStaticOOP, grailSortDynamicOOP )
import System.Random.Shuffle ( shuffleM )
import Control.Monad ( forM )
import Control.Exception ( SomeException, catch, evaluate )


--import Data.Maybe (fromJust, isJust)
--import Control.Monad.ST (runST)
--import Control.Monad.Primitive (PrimMonad (PrimState))
--import Control.Monad.Extra ( (&&^), ifM, loopM )
--import qualified Data.Vector as V
--import qualified Data.Vector.Mutable as VM
--
--import Data.Foldable ( Foldable(foldl') )

allSorted :: Ord a => [a] -> Bool
allSorted xs = and $ zipWith (<=) xs (tail xs)

main :: IO ()
main = do
  
    putStrLn "Shuffled lists (no duplicates)"
    putStrLn "Naive Implementation - In-Place"
    shuffleLoop GrailSort.Naive.grailSortInPlace 100
---
    putStrLn "Naive Implementation - Out-Of-Place (Static)"
    shuffleLoop GrailSort.Naive.grailSortStaticOOP 100
---
    putStrLn "Naive Implementation - Out-Of-Place (Dynamic)"
    shuffleLoop GrailSort.Naive.grailSortDynamicOOP 100
--
    putStrLn "Mutable Vector Implementation - In-Place"
    shuffleLoop GrailSort.grailSortInPlace 100
--
    putStrLn "Mutable Vector Implementation - Out-Of-Place (Static)"
    shuffleLoop GrailSort.grailSortStaticOOP 100
--
    putStrLn "Mutable Vector Implementation - Out-Of-Place (Dynamic)"
    shuffleLoop GrailSort.grailSortDynamicOOP 100
--
    putStrLn "-----------------------------------"
---
    putStrLn "Reversed lists (no duplicates)"
    putStrLn "Naive Implementation - In-Place"
    reverseLoop GrailSort.Naive.grailSortInPlace 100
---
    putStrLn "Naive Implementation - Out-Of-Place (Static)"
    reverseLoop GrailSort.Naive.grailSortStaticOOP 100
---
    putStrLn "Naive Implementation - Out-Of-Place (Dynamic)"
    reverseLoop GrailSort.Naive.grailSortDynamicOOP 100
--
    putStrLn "Mutable Vector Implementation - In-Place"
    reverseLoop GrailSort.grailSortInPlace 100
--
    putStrLn "Mutable Vector Implementation - Out-Of-Place (Static)"
    reverseLoop GrailSort.grailSortStaticOOP 100
---
    putStrLn "Mutable Vector Implementation - Out-Of-Place (Dynamic)"
    reverseLoop GrailSort.grailSortDynamicOOP 100
--
    putStrLn  "All tasks done"
    

shuffleLoop :: (Enum a, Num t2, Ord a1, Show a1, Show a, Num a) => ([a] -> t2 -> Int -> [a1]) -> a -> IO ()
shuffleLoop foo amount = do
    res <- forM [lower..amount] $ \item -> do
        testList <- System.Random.Shuffle.shuffleM [1..item]
        testFunc foo testList
    putStrLn $ (\(s,f,e) ->  "(" ++ show (amount - lower + 1) ++" tests) Successes: " ++ show s ++ " | Failures: " ++ show f ++ " | Errors: " ++ show e) $ foldr (\(a,b,c) (d,e,f) -> (a+d, b+e, c+f)) (0,0,0) res

    where
      lower = 16

reverseLoop :: (Enum a, Num t2, Ord a1, Show a1, Show a, Num a) => ([a] -> t2 -> Int -> [a1]) -> a -> IO ()
reverseLoop foo amount = do
    res <- forM [lower..amount] $ \item ->
        testFunc foo $ reverse [1..item]
    putStrLn $ (\(s,f,e) -> "(" ++ show (amount - lower + 1) ++" tests) Successes: " ++ show s ++ " | Failures: " ++ show f ++ " | Errors: " ++ show e) $ foldr (\(a,b,c) (d,e,f) -> (a+d, b+e, c+f)) (0,0,0) res

    where
      lower = 16

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




