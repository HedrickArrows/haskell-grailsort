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

    putStrLn "Reversed lists (no duplicates)"
    generalLoop max (return . reverse) GrailSort.Naive.grailSortInPlace "In-Place (Naive)"
    generalLoop max (return . reverse) GrailSort.Naive.grailSortStaticOOP  "Out-Of-Place, Static Buffer (Naive)"
    generalLoop max (return . reverse) GrailSort.Naive.grailSortDynamicOOP "Out-Of-Place, Dynamic Buffer (Naive)"

    generalLoop max (return . reverse) GrailSort.grailSortInPlace "In-Place"
    generalLoop max (return . reverse) GrailSort.grailSortStaticOOP  "Out-Of-Place, Static Buffer"
    generalLoop max (return . reverse) GrailSort.grailSortDynamicOOP "Out-Of-Place, Dynamic Buffer"

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
    in putStrLn (name ++ "encountered problems: " ++ fails ++ errors)


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




