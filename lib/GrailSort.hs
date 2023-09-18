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
Hedrick Arrows's Haskell implementation of Grailsort, a.k.a Mihnik tries his best

Rewritten in a "dying language" for fun, based on thatsOven's Python version and Morwenn's C++ rewrite of GrailSort
(just in case, I know this language isn't dying, why else would I be using it)

Current status: WIP 
  - Rewriting from bottoms up
    -- trying to use minimal syntactic sugar where it is possible
-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GrailSort (grailSortInPlace, grailSortStaticOOP, grailSortDynamicOOP) where

--imports
--imports
import Control.Monad ( when, (>=>) )
import Data.Maybe (isNothing, fromJust, isJust)
import Control.Monad.ST (runST)
import Control.Monad.Primitive (PrimMonad (PrimState))
import Control.Monad.Extra ( (&&^), ifM, loopM )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Applicative (Applicative(liftA2))
import Data.STRef ( newSTRef, readSTRef, writeSTRef, modifySTRef' )
import qualified Data.STRef as GHC.STRef
import qualified Control.Monad.ST as GHC.ST

--area of functions one would consider private in other languages

data Subarray = LEFT | RIGHT deriving (Eq, Show)--, Data, Typeable)
--toNumber :: Num p => Subarray -> p
--toNumber enum = case enum of --probably not necessary 
--  LEFT -> 0
--  RIGHT -> 1

-- a remaining function from different implementations
-- might be completely removed
--compareVal :: (Ord a, Num p) => a -> a -> p 
--compareVal a b
--  | a > b     = -1
--  | a < b     = 1
--  | otherwise = 0

----support function because I may need it
----and I actually use it to simplify some things for myself
----less-or-equal is here only in case of base value other than 2, might not need to be included here
--logInt :: Int -> Int -> Int
--logInt base value
--  | value <= 1 = 0
--  | otherwise  = logInt base (value `quot` base) + 1
--

--test loop
--while condition body = loopM condition (body >> pure (Left ())) (pure (Right ()))

----support function comparing two values in one vector to avoid further code repetition
----rewriting LiftA2 into <$> <*> syntax because it seems neater
compareV :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> (a -> a-> b) -> m b
compareV array fst snd op = op <$> VM.read array fst <*> VM.read array snd

--support function to quickly compare two values in STRefs
compareST :: Ord a => GHC.STRef.STRef s a -> GHC.STRef.STRef s a -> (a -> a -> b) -> GHC.ST.ST s b
compareST fst snd op = op <$> readSTRef fst <*> readSTRef snd
--
--
--blockSwap :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailBlockSwap :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailBlockSwap array a b blockLen =
  loopM ( \i -> if i < blockLen
      then
        VM.swap array (a + i) (b + 1) >>
        pure (Left (i + 1))
      else pure $ Right ()
    ) 0
--  forM_ [0..(len-1)] $ \i ->
--     VM.swap array (a + i) (b + i)
--
----how did I fix it
--rotate :: PrimMonad f => VM.MVector (PrimState f) a -> Int -> Int -> Int -> f ()
--grailRotate :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailRotate :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailRotate array start leftLen rightLen = loopM (
  \(start', leftL, rightL) -> if leftL > 0 && rightL > 0
    then (if leftL <= rightL
        then do
          grailBlockSwap array start' (start' + leftL) leftL
          pure $ Left (start' + leftL, leftL, rightL - leftL)
        else do
          grailBlockSwap array (start' + leftL - rightL) (start + leftL) rightL
          pure $ Left (start', leftL - rightL, rightL) )
    else pure $ Right ()
  ) (start, leftLen, rightLen)
--when (leftLen > 0 && rightLen > 0) (piece array start leftLen rightLen)
--  where
--    piece array start leftLen rightLen
--      | leftLen <= rightLen = do
--        blockSwap array start (start + leftLen) leftLen
--        rotate array (start + leftLen) leftLen (rightLen - leftLen)
--      | otherwise = do
--        blockSwap array (start + leftLen - rightLen) (start + leftLen) rightLen
--        rotate array start (leftLen - rightLen) rightLen
--
----also known as optimized gnomesort since it uses swaps instead of insertion
--insertSort :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m ()
grailInsertSort :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m ()
grailInsertSort array start l = let
  begin = start + 1
  end = start + l
  in
    loopM ( \j -> ifM ( pure $ j < end)
      (do
          loopM (\i -> ifM (pure (start<i) &&^ compareV array i (i-1) (<)) (do
            VM.swap array i (i-1)
            pure $ Left (i-1))
            (pure $ Right ())) j
          pure $ Left (j + 1)  )
      (pure $ Right () ) ) begin
--  forM_ [start,(start + 1)..(start + l - 1)] $ \item ->
--    loopM (\i -> ifM (pure (start<i) &&^ compareVecVals array i (i-1) (<)) (do
--      VM.swap array i (i-1)
--      pure $ Left (i-1)) (pure $ Right ())) item
--    --inner array item start
--  --where
--  --  inner v i lo = do
--  --    when (i > lo) do
--  --      current <- VM.read v i
--  --      previous <- VM.read v (i - 1)
--  --      when (current < previous) (VM.swap v i (i - 1))
--  --      inner v (i - 1) lo
--
----I have not much of a clue if this should be created but slice doesn't sound like a good solution
----on a second note though, maybe it is, I need to test it out, but it sure is annoying
--arrayCopy :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> VM.MVector (PrimState m) a -> Int -> Int -> m ()
--arrayCopy fromVector fromIndex toVector toIndex len = do
--  forM_ [0..(len-1)] $ \i -> do
--    val <- VM.read fromVector (fromIndex + i)
--    VM.write toVector (toIndex + i) val
--
--grailBinarySearchLeft :: (Ord a, PrimMonad m) => VM.MVector (PrimState m) a -> Int -> Int -> m a -> m Int
grailBinarySearchLeft :: (Ord a, PrimMonad m) => VM.MVector (PrimState m) a -> Int -> Int ->  a -> m Int
grailBinarySearchLeft array start length target =
  loopM ( \(left, right) -> ifM (pure (left < right))
    (let
      middle = left + ((right - left) `quot` 2)
      in
        ifM (liftA2 (<) (VM.read array (start + middle)) $ pure target)
        (pure $ Left (middle + 1, right))
        (pure $ Left (left, middle)))
    (pure $ Right left)
  ) (0, length)


grailBinarySearchRight :: (Ord a, PrimMonad m) => VM.MVector (PrimState m) a -> Int -> Int -> a -> m Int
grailBinarySearchRight array start length target =
    loopM ( \(left, right) -> ifM (pure (left < right))
      (let
        middle = left + ((right - left) `quot` 2)
        in
          ifM ((> target) <$> VM.read array (start + middle) )
          (pure $ Left (left, middle))
          (pure $ Left (middle + 1, right)))
      (pure $ Right left)
    ) (0, length)
----prototype of a shortcut to reduce code redundancy
----use (<) as `op` argument for binary search left, and (<=) for binary search right
----trust me, it works, especially after I had to fix it because it forced `Num a` and I had to figure out why
--binSearch :: PrimMonad m => VM.MVector (PrimState m) t -> Int -> Int -> t -> (t -> t -> Bool) -> m Int
--binSearch array start len tgt op = while array start tgt 0 len op
--  where
--    while array start tgt left right op
--      | left < right = do
--        let mid = left + ((right - left) `quot` 2)
--        elem <- VM.read array (start + mid)
--        if elem `op` tgt
--        then while array start tgt (mid + 1) right op
--        else while array start tgt left mid op
--      | otherwise =
--        if tgt `op` tgt
--        then pure right
--        else pure left
--
----for posteriority 
--binSearchLeft :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> a -> m Int
--binSearchLeft array start len tgt = binSearch array start len tgt (<)
--
--binSearchRight :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> a -> m Int
--binSearchRight array start len tgt = binSearch array start len tgt (<=)
--

--grailCollectKeys :: (Ord a, PrimMonad m) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m Int
grailCollectKeys :: (Ord a, PrimMonad m) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m Int
grailCollectKeys array start length idealKeys =
  loopM (\(keysFound, firstKey, currKey) -> if currKey < length && keysFound < idealKeys
      then
         VM.read array (start + currKey) >>= (grailBinarySearchLeft array (start + firstKey) keysFound
                                               >=>
                                                 (\ insertPos
                                                    -> compareV
                                                         array (start + currKey) (start + firstKey + insertPos) (/=)
                                                         >>=
                                                           \ result
                                                             -> if insertPos == keysFound || result then
                                                                    let newFirstKey = currKey - keysFound
                                                                    in
                                                                      do grailRotate
                                                                           array (start + firstKey) keysFound
                                                                           (currKey - (firstKey + keysFound))
                                                                         grailRotate
                                                                           array (start + newFirstKey + insertPos) (keysFound - insertPos) 1
                                                                         pure $ Left (keysFound + 1, firstKey, currKey + 1)
                                                                else
                                                                    pure $ Left (keysFound, firstKey, currKey + 1)))
      else do
        grailRotate array start firstKey keysFound
        pure $ Right keysFound
        ) (1, 0, 1)
--collectKeys :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m Int
--collectKeys array start len ideal = while array start len ideal 1 1 0
--  where
--    while array start len ideal found current first
--      | current < len && found < ideal = do
--        sPlusCurr <- VM.read array (start + current)
--        insertPos <- binSearch array (start + first) found sPlusCurr (<) --searchLeft
--        sPlusFirstPlusIns <- VM.read array (start + first + insertPos)
--        if (found == insertPos) || (sPlusCurr /= sPlusFirstPlusIns)
--        then do
--          rotate array (start + first) found  (current - (first + found))
--          let firstKey = current - found
--          rotate array (start + firstKey + insertPos) (found - insertPos) 1
--          while array start len ideal (found + 1) (current + 1) firstKey
--        else
--          while array start len ideal found (current + 1) first
--      | otherwise = do
--        rotate array start first found
--        pure found
--
--grailPairwiseSwaps :: VM.MVector (PrimState m) a -> Int -> Int -> m ()
grailPairwiseSwaps :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m ()
grailPairwiseSwaps array start length = do
  index <- loopM ( \index-> ifM (pure (index < length))
    (let
      left = start + index - 1
      right = start + index
    in do
      ifM (compareV array left right (>))
        (do
          VM.swap array (left - 2) right
          VM.swap array (right-2) left)
        (do
          VM.swap array (left - 2) left
          VM.swap array (right - 2) right)
      pure $ Left (index + 2))
    (pure (Right index)) ) 1
  let left = start + index - 1
  when (left < start + length) (VM.swap array (left - 2) left)
--pairwiseSwaps :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m ()
--pairwiseSwaps array start len = while array start len 1
--  where
--    left = start -1
--    right = start
--    while array start len index
--      | index < len = do
--        aLeft <- VM.read array  (left + index)
--        aRight <- VM.read array (right + index)
--        if aLeft > aRight
--        then do
--          VM.swap array (left + index - 2) (right + index)
--          VM.swap array (right  - 2) (left + index)
--        else do
--          VM.swap array (left + index - 2)  (left + index)
--          VM.swap array (right + index - 2) (right + index)
--        while array start len (index + 2)
--      | otherwise = do
--        let l = start + index - 1
--        when (l < start + len) (VM.swap array (l - 2) l)
--
--grailPairwiseWrites :: VM.MVector (PrimState m) a -> Int -> Int -> m ()
grailPairwiseWrites :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m ()
grailPairwiseWrites array start length = do
  index <- loopM ( \index-> ifM (pure (index < length))
    (let
      left = start + index - 1
      right = start + index
    in do
      leftVal <- VM.read array left
      rightVal <- VM.read array right
      ifM (compareV array left right (>))
        (do
          VM.write array (left - 2) rightVal
          VM.write array (right-2) leftVal)
        (do
          VM.write array (left - 2) leftVal
          VM.write array (right - 2) rightVal)
      pure $ Left (index + 2))
    (pure (Right index)) ) 1
  let left = start + index - 1
  leftVal <- VM.read array left
  when (left < start + length) (VM.write array (left - 2) leftVal)
--pairwiseWrites :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m ()
--pairwiseWrites array start len = while array start len 1
--  where
--    left = start - 1
--    right = start
--    while array start len index
--      | index < len = do
--        aLeft  <- VM.read array (left + index)
--        aRight <- VM.read array (right + index)
--        if aLeft > aRight
--        then do
--          VM.write array (left + index - 2) aRight
--          VM.write array (right + index - 2) aLeft
--        else do
--          VM.write array (left + index- 2) aLeft
--          VM.write array (right + index - 2) aRight
--        while array start len (index + 2)
--      | otherwise = do
--        let left = start + index
--        let sPlusL = start + len
--        aLeft <- VM.read array left
--        when (left < sPlusL) (VM.write array (left - 2) aLeft)
--
--grailMergeForwards :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> m ()
grailMergeForwards array start leftLen rightLen bufferOffset =
    let left   = start
        middle = start + leftLen
        right  = middle
        end    = middle + rightLen
        buffer = start - bufferOffset
    in
      loopM ( \(left', right', buffer') -> if right' < end
        then do
          result <- compareV array left' right' (>)
          if left' == middle || result
            then do
              VM.swap array buffer' right'
              pure $ Left (left', right' + 1, buffer' + 1)
            else do
              VM.swap array buffer' left'
              pure $ Left (left' + 1, right', buffer' + 1)
          --pure $ Left ()
        else do
          when (buffer' /= left') (grailBlockSwap array buffer' left' (middle - left'))
          pure $ Right ())  (left, right, buffer)
--mergeForwards :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> m ()
--mergeForwards array start leftLen rightLen bufferOffset = while array l mid r e buff
--  where
--    l = start
--    mid = start + leftLen
--    r = mid
--    e = mid + rightLen
--    buff = start - bufferOffset
--    while array left middle right end buffer
--      | right < end = do
--        aLeft <- VM.read array left
--        aRight <- VM.read array right
--        if left == middle || aLeft > aRight
--        then do
--          VM.swap array buffer right
--          while array left middle (right + 1) end (buffer + 1)
--        else do
--          VM.swap array buffer left
--          while array (left + 1) middle right end (buffer + 1)
--      | otherwise = when (buffer /= left) (blockSwap array buffer left (middle - left))
--
grailMergeBackwards :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> m ()
grailMergeBackwards array start leftLen rightLen bufferOffset =
  let
    end    = start - 1
    left   = end + leftLen
    middle = left
    right  = middle + rightLen
    buffer = right + bufferOffset
  in do
    loopM ( \(left', right', buffer') -> if left' > end
      then compareV array left' right' (>) >>= \result ->
        if right' == middle || result
        then do
          VM.swap array buffer' left'
          pure $ Left (left' - 1, right', buffer' - 1)
        else do
          VM.swap array buffer' right'
          pure $ Left (left', right' - 1, buffer' - 1)
      else do
        when (right' /= buffer') (loopM (\(right'', buffer'') -> if right'' > middle
          then do
            VM.swap array buffer'' right''
            pure $ Left (right'' - 1, buffer'' - 1)
          else pure $ Right ()
          ) (right', buffer'))
        pure $ Right ()
      ) (left, right, buffer)

--mergeBackwards :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> m ()
--mergeBackwards array start leftLen rightLen bufferOffset = while array e l mid r buff
--  where
--    e = start - 1
--    l = e + leftLen
--    mid = l
--    r = mid + rightLen
--    buff = r + bufferOffset
--    while array end left middle right buffer
--      | left > end = do
--        aLeft <- VM.read array left
--        aRight <- VM.read array right
--        if right == middle || aLeft > aRight
--        then do
--          VM.swap array buffer left
--          while array end (left - 1) middle right (buffer - 1)
--        else do
--          VM.swap array buffer right
--          while array end left middle (right - 1) (buffer - 1)
--      | otherwise = when (right /= buffer) (inner array right middle buffer)
--    inner array right middle buffer =
--      when (right > middle) do
--        VM.swap array buffer right
--        inner array (right - 1) middle (buffer - 1)
--
grailMergeOutOfPlace array start leftLen rightLen bufferOffset =
    let left   = start
        middle = start + leftLen
        right  = middle
        end    = middle + rightLen
        buffer = start - bufferOffset
    in
      loopM (\(left', right', buffer') -> if right' < end
        then compareV array left' right' (>) >>= \result ->
          if left' == middle || result
          then
            --VM.read array right' >>= \val -> VM.write array buffer' val >>
            pure $ Left (left', right' + 1, buffer' + 1)
          else
            --VM.read array left  >>= \val -> VM.write array buffer val >>
            pure $ Left (left' + 1, right', buffer' + 1)
        else pure $ Right ()
      ) (left, right, buffer)
--mergeOutOfPlace array start lLen rLen buffOff = pure ()
----  let
----    l = start
----    mid = start + lLen
----    r = mid
----    end = mid + rLen
----    buf = start - buffOff
----  in do
----    loop (\(buffer, left, right) -> if right < end
----      then 
----      else )
--
--grailBuildInPlace :: (PrimMonad m, Ord a) =>  VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> m ()
--grailBuildInPlace :: VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> m ()

--buildInPlace :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> m ()
--buildInPlace array start len currentLen bufferLen = while array start len currentLen bufferLen
--  where
--    while array start len mergeLen bufferLen
--      | mergeLen < bufferLen = do
--        let fullMerge = mergeLen * 2
--        let mergeEnd = start + len - fullMerge
--        let bufferOffset = mergeLen
--        mergeIndex <- inner array start mergeLen bufferOffset mergeEnd
--        let leftOver = len - (mergeIndex - start)
--        if leftOver > mergeLen
--        then mergeForwards array mergeIndex mergeLen (leftOver - mergeLen) bufferOffset
--        else rotate array (mergeIndex - mergeLen) mergeLen leftOver
--        while array (start - mergeLen) len fullMerge bufferLen
--      | otherwise = do
--        let fullMerge = 2 * bufferLen
--        let lastBlock = len `rem` fullMerge
--        let lastOffset = start + len - lastBlock
--        if lastBlock <= bufferLen
--        then rotate array lastOffset lastBlock bufferLen
--        else mergeBackwards array lastOffset bufferLen (lastBlock - bufferLen) bufferLen
--        forM_ [(lastOffset - fullMerge),(lastOffset - 2 * fullMerge)..start] $ \mergeIndex -> do
--          mergeBackwards array mergeIndex bufferLen bufferLen bufferLen
--    inner array mergeIndex mergeLen bufferOffset mergeEnd
--      | mergeIndex <= mergeEnd = do
--        mergeForwards array mergeIndex mergeLen mergeLen bufferOffset
--        inner array (mergeIndex + mergeLen * 2) mergeLen bufferOffset mergeEnd
--      | otherwise = pure mergeIndex
--
--buildOutOfPlace array start len bufferLen extBuffer extLen = pure ()
----NEEDS TO BE DONE
----NEEDS TO BE DONE
----NEEDS TO BE DONE
--
grailBuildBlocks array start length bufferLen extBuffer extBufferLen = pure ()
--buildBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Maybe (VM.MVector (PrimState m) a) -> Int -> m ()
--buildBlocks array start len bufferLen extBuffer extBufferLen
--  | isNothing extBuffer = do
--    pairwiseSwaps array start len
--    buildInPlace array (start - 2) len 2 bufferLen
--  | bufferLen < extBufferLen =
--    buildOutOfPlace array start len bufferLen (fromJust extBuffer) bufferLen
--  | otherwise = do
--      let extLen = setExtLen 1 extBufferLen
--      buildOutOfPlace array start len bufferLen (fromJust extBuffer) extLen
--  where
--    setExtLen val treshold
--      | val * 2 > treshold = val
--      | otherwise = setExtLen (val * 2) treshold
--
--blockSelectSort :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> m Int
grailBlockSelectSort :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> m Int
grailBlockSelectSort array firstKey start medianKey blockCount blockLen =
  loopM (\(firstBlock, medianKey') -> if firstBlock < blockCount
    then loopM (\(selectBlock, currBlock) -> if currBlock < blockCount
        then --do
          compareV array (start + currBlock * blockLen) (start + selectBlock * blockLen) compare >>= \comp ->
          compareV array (firstKey + currBlock) (firstKey + selectBlock) (<) >>= \cmp2 ->
        --liftA2 compare (VM.read array (start + currBlock * blockLen)) (VM.read array (start + selectBlock * blockLen)) 
          if (comp == LT) || ((comp == EQ) && cmp2)
          then pure (Left (currBlock, currBlock+1))
          else  pure (Left (selectBlock, currBlock+1))
        else pure (Right selectBlock)
      ) (firstBlock, firstBlock + 1) >>= \selectBlock' ->
        if selectBlock' /= firstBlock
        then
          grailBlockSwap array (start + firstBlock * blockLen) (start + selectBlock' * blockLen) blockLen >>
          VM.swap array (firstKey + firstBlock) (firstKey + selectBlock') >>
          if medianKey' == firstBlock
          then pure (Left (firstBlock+1, selectBlock'))
          else if medianKey' == selectBlock'
            then pure (Left (firstBlock+1, firstBlock))
            else pure (Left (firstBlock+1, medianKey'))
        else pure (Left (firstBlock+1, medianKey'))
    else pure (Right medianKey')
    ) (0, medianKey)
--  loopM (\(medKey, firstBlock) ->
--      ifM (pure $ firstBlock < blockCount)
--      (do
--        selectBlock <- loopM (\(selBlock, currBlock) -> ifM (pure (currBlock < blockCount))
--          (do
--            cmp1 <- compareVecVals array (start + (currBlock * blockLen)) (start + (selBlock * blockLen)) (>)
--            cmp2 <- compareVecVals array (start + (currBlock * blockLen)) (start + (selBlock * blockLen)) (==)
--            cmp3 <- compareVecVals array (firstKey + currBlock) (firstKey + selBlock) (<)
--
--            if cmp1 || (cmp2 && cmp3)
--            then pure $ Left (currBlock, currBlock + 1)
--            else pure $ Left (selBlock, currBlock + 1)
--          )
--          (pure $ Right selBlock) ) (firstBlock, firstBlock + 1)
--        if selectBlock /= firstBlock
--        then do
--          blockSwap array (start + firstBlock * blockLen) (start + selectBlock * blockLen) blockLen
--          VM.swap array (firstKey + firstBlock) (firstKey + selectBlock)
--          if medKey == firstBlock
--          then pure $ Left (selectBlock, firstBlock + 1)
--          else if medKey == selectBlock
--            then pure $ Left (firstBlock, firstBlock + 1)
--            else pure $ Left (medKey, firstBlock + 1)
--        else pure $ Left (medKey, firstBlock + 1) )
--      (pure $ Right medKey) ) (medianKey, 0)
----DONE???
--
grailInPlaceBufferReset array start length bufferOffset = pure ()
--inPlaceBufferReset :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
--inPlaceBufferReset array start len bufferOffset =
--  loopM (\(buffer, index) -> ifM (pure (buffer > start)) (do
--    VM.swap array buffer index
--    pure $ Left (buffer - 1, index - 1)
--  )( pure $ Right ())) (start + len - 1, start + len - 1 - bufferOffset)
--
--outOfPlaceBufferReset array start len bufferOffset = pure ()
----NEEDS TO BE DONE
----NEEDS TO BE DONE
----NEEDS TO BE DONE
--
--inPlaceBufferRewind :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
--inPlaceBufferRewind array start leftBlock buffer = when (leftBlock >= start) do
--  VM.swap array buffer leftBlock
--  inPlaceBufferRewind array start (leftBlock - 1) (buffer - 1)
--
--outOfPlaceBufferRewind array start leftBlock buffer = pure ()
----NEEDS TO BE DONE
----NEEDS TO BE DONE
----NEEDS TO BE DONE
--
grailGetSubarray :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m Subarray
grailGetSubarray array currentKey medianKey = ifM (compareV array currentKey medianKey (<)) (pure LEFT) (pure RIGHT)
--getSubarray :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m Subarray
--getSubarray array currentKey medianKey = ifM (compareVecVals array currentKey medianKey (<)) (pure LEFT) (pure RIGHT)
--

grailCountLastMergeBlocks array offset blockCount blockLen =
  let
    blocksToMerge = 0
    lastRightFrag = offset + blockCount * blockLen
    prevLeftBlock = lastRightFrag - blockLen
  in
    loopM (\(bTM, pLB) -> ifM (pure (bTM < blockCount) &&^ compareV array lastRightFrag pLB (<))
      (pure (Left (bTM + 1, pLB - blockLen)))
      (pure (Right bTM))
    ) (blocksToMerge, prevLeftBlock)
--countLastMergeBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m Int
--countLastMergeBlocks array offset blockCount blockLen =
--  loopM (\(blocksToMerge, prevLeftBlock) -> ifM (pure (blocksToMerge < blockCount) &&^ compareVecVals array lastRightFrag prevLeftBlock (<))
--    (pure $ Left (blocksToMerge + 1, prevLeftBlock - blockCount) )
--    (pure $ Right blocksToMerge) ) (0, lastRightFrag - blockLen)
--  where
--    lastRightFrag = offset + blockCount * blockLen
--
grailSmartMerge array start leftLen leftOrigin rightLen bufferOffset =
  let
      left = start
      middle = start + leftLen
      right = middle
      end = middle + rightLen
      buffer = start - bufferOffset
      op = if leftOrigin == LEFT then (<=) else (<)
  in
    loopM ( \(left', right', buffer') ->
      if left < middle && right < end
      then ifM (compareV array left' right' op)
        (VM.swap array left' buffer' >>
          pure (Left (left' + 1, right', buffer' +1)))
        (VM.swap array right' buffer' >>
          pure (Left (left', right' + 1, buffer' +1)))
      else pure (Right (left', right', buffer'))
    ) (left, right, buffer) >>= \(left', right', buffer') ->
    pure (end - right', RIGHT)


--smartMerge :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Subarray -> Int -> Int ->Subarray -> m (Int, Subarray)
--smartMerge array start leftLen leftOrigin rightLen bufferOffset currBlockOrigin
--   | leftOrigin == LEFT = while array start leftOrigin bufferOffset currBlockOrigin (<=)
--   | otherwise          = while array start leftOrigin bufferOffset currBlockOrigin (<)
--  where
--    middle = start + leftLen
--    end    = middle + rightLen
--    while array start leftOrigin bufferOffset currBlockOrigin op = do
--      (left, right, _) <- loopM (\(l, r, buff) -> ifM (pure (l < middle && r < end))
--        (ifM (compareVecVals array l r op)
--        (do
--          VM.swap array buff l
--          pure $ Left (l+1,r,buff+1))
--        (do
--          VM.swap array buff r
--          pure $ Left (l,r+1,buff+1))) (pure $ Right (l,r,buff)) ) (start, middle, start - bufferOffset)
--
--      if left < middle
--      then do
--        inPlaceBufferRewind array left (middle - 1) (end - 1)
--        pure (middle - left, currBlockOrigin )
--      else if leftOrigin == LEFT
--        then pure (end - right, RIGHT)
--        else pure (end - right, LEFT)
--
grailSmartLazyMerge array start leftLen leftOrigin rightLen currBlockLen currBlockOrigin =
  let
    middle = start + leftLen
  in
  (if leftOrigin == LEFT
  then ifM (compareV array (middle - 1) middle (>))
      (loopM ( \(start', leftLen', rightLen', middle') -> if leftLen /= 0
          then VM.read array start' >>= (grailBinarySearchLeft array middle' rightLen' Control.Monad.>=>
                                                               (\ mergeLen
                                                                  -> (if mergeLen /= 0 then
                                                                          grailRotate array start' leftLen' mergeLen
                                                                            >>
                                                                              pure (start' + mergeLen, rightLen' - mergeLen, middle' + mergeLen)
                                                                      else
                                                                          pure (start', rightLen', middle'))
                                                                       >>=
                                                                         \ (start'', rightLen'', middle'')
                                                                           -> if rightLen'' == 0 then
                                                                                  pure (Right (leftLen', True))
                                                                              else
                                                                                  loopM
                                                                                    (\ (st, ll)
                                                                                       -> ifM
                                                                                            (pure (ll /= 0) &&^ compareV array st middle'' (<=))
                                                                                            (pure (Left (st + 1, ll - 1))) (pure (Right (st, ll))))
                                                                                    (start'' + 1, leftLen' - 1)
                                                                                    >>=
                                                                                      \ (start''', leftLen'')
                                                                                        -> pure (Left (start''', leftLen'', rightLen'', middle''))))
          else pure (Right (rightLen', False))
      ) (start, leftLen, rightLen, middle) )
      (pure (rightLen, False))
  else ifM (compareV array (middle - 1) middle (>=))
  (loopM ( \(start', leftLen', rightLen', middle') -> if leftLen /= 0
    then VM.read array start' >>= (grailBinarySearchRight array middle' rightLen'
                                                         >=>
                                                           (\ mergeLen
                                                              -> (if mergeLen /= 0 then
                                                                      grailRotate array start' leftLen' mergeLen
                                                                        >>
                                                                          pure (start' + mergeLen, rightLen' - mergeLen, middle' + mergeLen)
                                                                  else
                                                                      pure (start', rightLen', middle'))
                                                                   >>=
                                                                     \ (start'', rightLen'', middle'')
                                                                       -> if rightLen'' == 0 then
                                                                              pure (Right (leftLen', True))
                                                                          else
                                                                              loopM
                                                                                (\ (st, ll)
                                                                                   -> ifM
                                                                                        (pure (ll /= 0) &&^ compareV array st middle'' (<))
                                                                                        (pure (Left (st + 1, ll - 1))) (pure (Right (st, ll))))
                                                                                (start'' + 1, leftLen' - 1)
                                                                                >>=
                                                                                  \ (start''', leftLen'')
                                                                                    -> pure (Left (start''', leftLen'', rightLen'', middle''))))
    else pure (Right (rightLen', False))
  ) (start, leftLen, rightLen, middle) )
      (pure (rightLen, False))
  ) >>= \(tgtLen, isRLzero) ->
  if isRLzero
  then pure (tgtLen, currBlockOrigin)
  else if leftOrigin == LEFT
      then pure (tgtLen, RIGHT)
      else pure (tgtLen, LEFT)
--smartLazyMerge :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a  -> Int -> Int -> Subarray ->Int -> Int -> Subarray -> m (Int, Subarray)
--smartLazyMerge array start leftLen leftOrigin rightLen currBlockLen currBlockOrigin
--  | leftOrigin == LEFT = ifM (compareVecVals array (middle - 1) middle (>) ) (inner array start leftLen leftOrigin rightLen currBlockOrigin (<) (<=)) (pure $ finish rightLen )
--  | otherwise          = ifM (compareVecVals array (middle - 1) middle (>=)) (inner array start leftLen leftOrigin rightLen currBlockOrigin (<=) (<)) (pure $ finish rightLen )
--  where
--    middle = start + leftLen
--    finish rightL = if leftOrigin == LEFT
--      then (rightL, RIGHT)
--      else (rightL, LEFT)
--    inner array start leftLen leftOrigin rightLen currBlockOrigin searchOp condOp = loopM (\(st, rLen, mid, lLen) -> ifM (pure (lLen == 0))
--        (pure $ Right $ finish rightLen)
--        (do
--          tgt <- VM.read array st
--          mergeLen <- binSearch array mid rLen tgt searchOp
--
--          (startN, rLenN, midN) <- (if mergeLen == 0
--              then pure (st, rLen, mid)
--              else do
--                rotate array st lLen mergeLen
--                pure (st + mergeLen, rLen - mergeLen, mid + mergeLen))
--
--          if rLenN == 0
--          then pure $ Right (lLen, currBlockOrigin)
--          else do
--            res <- loopM (\(s, lL) -> ifM ( compareVecVals array s mid condOp &&^ pure (lL /= 0))
--              (pure $ Left (s + 1, lL -1)) (pure $ Right (s, rLenN, midN, lL) ) ) (startN + 1, lLen - 1)
--            pure $ Left res
--        )) (start, rightLen, middle, leftLen)
--
--smartMergeOutOfPlace array start leftLen leftOrigin rightLen bufferOffset currBlockOrigin = pure ()
----NEEDS TO BE DONE
----NEEDS TO BE DONE
----NEEDS TO BE DONE
--

grailMergeBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> m ()
grailMergeBlocks array firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen =
  let
    nextBlock = start + blockLen
    currBlockLen = blockLen
  in
    grailGetSubarray array firstKey medianKey >>= \currBlockOrigin ->
    loopM ( \(keyIndex, nextBlock', cBLen,cBOrigin) -> if keyIndex < blockCount
      then let
        currBlock = nextBlock - cBLen
        in
        grailGetSubarray array (firstKey + keyIndex) medianKey >>= \nextBlockOrigin ->
        (if nextBlockOrigin == cBOrigin
          then let
            buffer = currBlock - blockLen
          in
            grailBlockSwap array buffer currBlock cBLen >>
            pure (blockLen, cBOrigin)
          else
            grailSmartMerge array currBlock cBLen cBOrigin blockLen blockLen
        ) >>= \(currBlockLen', currBlockOrigin') ->
        pure (Left (keyIndex + 1, nextBlock + blockLen, currBlockLen', currBlockOrigin'))
      else pure (Right (nextBlock', cBLen, cBOrigin))
    ) (1, nextBlock, currBlockLen, currBlockOrigin) >>=
      \(nextBlock', currBlockLen', currBlockOrigin') -> let
      currBlock = nextBlock' - currBlockLen'
      buffer = currBlock - blockLen
    in if lastLen /= 0
      then (if currBlockOrigin' == RIGHT
          then pure (nextBlock', blockLen * lastMergeBlocks)
          else pure (currBlock, currBlockLen' + blockLen * lastMergeBlocks)
        ) >>= \(currBlock', currBlockLen'') ->
        grailMergeForwards array currBlock' currBlockLen'' lastLen blockLen
      else grailBlockSwap array buffer currBlock currBlockLen
--mergeBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> m (Int, Subarray)
--mergeBlocks array firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen = do
--  let currBlockLen = blockLen
--  currBlockOrigin <- getSubarray array firstKey medianKey
--
--  (_, nextBlock, cBlockLen, cBlockOrigin) <- loopM (\(keyIndex, nB, cBL, cBO ) ->
--    ifM (pure $ keyIndex < blockCount) (do
--      let currBlock = nB - currBlockLen
--      nextBlockOrigin <- getSubarray array (firstKey + keyIndex) medianKey
--
--      if nextBlockOrigin == currBlockOrigin
--      then do
--        let buffer = currBlock - blockLen
--        blockSwap array buffer currBlock currBlockLen
--        pure $ Left (keyIndex + 1, nB + blockLen, blockLen, cBO)
--      else do
--        (cBLen, cBOrigin) <- smartMerge array currBlock currBlockLen currBlockOrigin blockLen blockLen currBlockOrigin
--        pure $ Left (keyIndex + 1, nB + blockLen, cBLen, cBOrigin)
--       )
--      (pure $ Right (keyIndex, nB, cBL,cBO))) (1, start + blockLen, currBlockLen, currBlockOrigin)
--
--  let currBlock = nextBlock  - currBlockLen
--  let buffer = currBlock - blockLen
--
--  if lastLen /= 0
--    then if cBlockOrigin == RIGHT
--      then do
--        let cBL = blockLen * lastMergeBlocks
--        mergeForwards array nextBlock cBlockLen lastLen blockLen
--        pure (cBL, LEFT)
--      else do
--        mergeForwards array currBlock cBlockLen lastLen blockLen
--        pure (cBlockLen, cBlockOrigin)
--    else do
--      blockSwap array buffer currBlock currBlockLen
--      pure (cBlockLen, cBlockOrigin)
--

grailLazyMergeBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> m ()
grailLazyMergeBlocks array firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen =
  let
    nextBlock = start + blockLen
    currBlockLen = blockLen
  in
    grailGetSubarray array firstKey medianKey >>= \currBlockOrigin ->
    loopM (\(keyIndex, nextBlock, currBlockLen, currBlockOrigin) -> if keyIndex < blockCount
        then let
          currBlock = nextBlock - currBlockLen
          in
          grailGetSubarray array (firstKey + keyIndex) medianKey >>= \nextBlockOrigin ->
          (if nextBlockOrigin == currBlockOrigin
                then pure (blockLen, currBlockOrigin)
                else grailSmartLazyMerge array currBlock currBlockLen currBlockOrigin blockLen currBlockLen currBlockOrigin
          ) >>= \(currBlockLen', currBlockOrigin') ->
          pure (Left (keyIndex + 1, nextBlock + blockLen, currBlockLen', currBlockOrigin'))
        else pure (Right (nextBlock, currBlockLen, currBlockOrigin))) (1, nextBlock, currBlockLen, currBlockOrigin)
        >>= \(nextBlock, currBlockLen', currBlockOrigin') ->
          let currBlock = nextBlock - currBlockLen in
          when (lastLen /= 0) ( let
            (currBlock', currBlockLen'', currBlockOrigin'') =
              if currBlockOrigin' == RIGHT
              then (nextBlock, blockLen * lastMergeBlocks, LEFT)
              else (currBlock, currBlockLen' * blockLen * lastMergeBlocks, currBlockOrigin')
            in
              grailLazyMerge array currBlock' currBlockLen'' lastLen)
--lazyMergeBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> m (Int, Subarray)
--lazyMergeBlocks array firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen = do --pure (1, LEFT)
--  let currBlockLen = blockLen
--  currBlockOrigin <- getSubarray array firstKey medianKey
--
--  (_, nextBlock, cBlockLen, cBlockOrigin) <- loopM (\(keyIndex, nB, cBL, cBO ) ->
--    ifM (pure $ keyIndex < blockCount) (do
--      let currBlock = nB - currBlockLen
--      nextBlockOrigin <- getSubarray array (firstKey + keyIndex) medianKey
--
--      if nextBlockOrigin == cBO
--      then
--        pure $ Left (keyIndex + 1, nB + blockLen, blockLen, cBO)
--      else do
--        (cBLen, cBOrigin) <- smartLazyMerge array currBlock currBlockLen currBlockOrigin blockLen blockLen currBlockOrigin
--        pure $ Left (keyIndex + 1, nB + blockLen, cBLen, cBOrigin)
--       )
--      (pure $ Right (keyIndex, nB, cBL,cBO))) (1, start + blockLen, currBlockLen, currBlockOrigin)
--
--  let currBlock = nextBlock - cBlockLen
--
--  if lastLen == 0
--    then pure (cBlockLen, cBlockOrigin)
--    else do
--      if cBlockOrigin == RIGHT
--      then lazyMerge array nextBlock (blockLen * lastMergeBlocks) lastLen
--      else lazyMerge array currBlock (blockLen * lastMergeBlocks) lastLen
--      pure (blockLen * lastMergeBlocks, LEFT)
--
--mergeBlocksOutOfPlace array firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen = do pure (1, LEFT)
----NEEDS TO BE DONE
----NEEDS TO BE DONE
----NEEDS TO BE DONE
--

grailCombineInPlace :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool -> m ()
grailCombineInPlace array firstKey start length subarrayLen blockLen mergeCount lastSubarrays buffer =
  let
    fullMerge = 2 * subarrayLen :: Int
    blockCount = fullMerge `quot` blockLen :: Int
  in
    loopM (\mergeIndex -> if mergeIndex < mergeCount
      then let
        offset = start + (mergeIndex * fullMerge)
        medianKey = subarrayLen `quot` blockLen
        in
        grailInsertSort array firstKey blockCount >>
        grailBlockSelectSort array firstKey offset medianKey blockCount blockLen >>= \medianKey' ->
        (if buffer
        then grailMergeBlocks array firstKey (firstKey + medianKey') offset blockCount blockLen 0 0
        else grailLazyMergeBlocks array firstKey (firstKey + medianKey') offset blockCount blockLen 0 0
        ) >>
        pure (Left (mergeIndex + 1))
      else pure (Right ())
      ) 0 >>
    when (lastSubarrays /= 0) (let
      offset = start + mergeCount * fullMerge
      blockCount = lastSubarrays `quot` blockLen
      medianKey = subarrayLen `quot` blockLen
      lastFragment = lastSubarrays - blockCount * blockLen
      in
        grailInsertSort array firstKey (blockCount + 1) >>
        grailBlockSelectSort array firstKey offset medianKey blockCount blockLen >>= \medianKey' ->
        (if lastFragment /= 0
          then grailCountLastMergeBlocks array offset blockCount blockLen
          else pure 0) >>= \lastMergeBlocks ->
        let smartMerges = blockCount - lastMergeBlocks in
          if smartMerges == 0
          then let leftLen = lastMergeBlocks * blockLen in
            if buffer
            then grailMergeForwards array offset leftLen lastFragment blockLen
            else grailLazyMerge array offset leftLen lastFragment
          else if buffer
            then grailMergeBlocks array firstKey (firstKey + medianKey') offset smartMerges blockLen lastMergeBlocks lastFragment
            else grailLazyMergeBlocks array firstKey (firstKey + medianKey') offset smartMerges blockLen lastMergeBlocks lastFragment
    ) >>
    when buffer (grailInPlaceBufferReset array start length blockLen)
--combineInPlace :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool -> Int -> Subarray -> m (Int, Subarray)
--combineInPlace array firstKey start len subarrayLen blockLen mergeCount lastSubarrays buffer currBlockLen currBlockOrigin =
--  while array firstKey start len subarrayLen blockLen mergeCount lastSubarrays buffer currBlockLen currBlockOrigin 0
--    where
--      fullMerge = 2 * subarrayLen
--      blockCount = fullMerge `quot` blockLen
--      while array firstKey start len subarrayLen blockLen mergeCount lastSubarrays buffer currBlockLen currBlockOrigin mergeIndex
--        | mergeIndex < mergeCount = do
--          let offset = start + (mergeIndex * fullMerge)
--          insertSort array firstKey blockCount
--          --while array firstKey start len subarrayLen blockLen mergeCount lastSubarrays buffer currBlockLen currBlockOrigin (mergeIndex + 1)
--          medianKey <- blockSelectSort array firstKey offset (subarrayLen `quot` blockLen) blockCount blockLen
--          (cuBlockLen, cuBlockOrigin) <- if buffer
--            then mergeBlocks array firstKey (firstKey + medianKey) offset blockCount blockLen 0 0
--            else lazyMergeBlocks array firstKey (firstKey + medianKey) offset blockCount blockLen 0 0
--          while array firstKey start len subarrayLen blockLen mergeCount lastSubarrays buffer cuBlockLen cuBlockOrigin (mergeIndex + 1)
--        | otherwise = if lastSubarrays == 0
--          then do
--            when buffer (inPlaceBufferReset array start len blockLen)
--            pure (currBlockLen, currBlockOrigin)
--          else do
--            let offset = start + (mergeCount * fullMerge)
--            let blockCount = lastSubarrays `quot` blockLen
--            insertSort array firstKey (blockCount + 1)
--            medianKey <- blockSelectSort array firstKey offset (subarrayLen `quot` blockLen) blockCount blockLen
--            let lastFragment = lastSubarrays - (blockCount * blockLen)
--            lastMergeBlocks <- if lastFragment == 0 then pure 0
--                  else countLastMergeBlocks array offset blockCount blockLen --firstKey offset medianKey blockCount blockLen )--assignLMB lastFragment array offset blockCount blockLen
--            let smartMerges = blockCount - lastMergeBlocks
--            if smartMerges == 0
--            then do
--              let leftLen = lastMergeBlocks * blockLen
--              if buffer
--              then mergeForwards array offset leftLen lastFragment blockLen
--              else lazyMerge array offset leftLen lastFragment
--              pure (currBlockLen, currBlockOrigin)
--            else if buffer
--              then mergeBlocks array firstKey (firstKey + medianKey) offset smartMerges blockLen lastMergeBlocks lastFragment
--              else lazyMergeBlocks array firstKey (firstKey + medianKey) offset smartMerges blockLen lastMergeBlocks lastFragment
--
grailCombineOutOfPlace array firstKey start length subarrayLen blockLen mergeCount lastSubarrays extBuffer extBufferLen =
  pure ()
--combineOutOfPlace :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> VM.MVector (PrimState m) a -> Int -> Subarray -> m (Int, Subarray)
--combineOutOfPlace array firstKey start len subarrayLen blockLen mergeCount lastSubarrays extBuffer currBlockLen currBlockOrigin = pure ()
--  arrayCopy array (start - blockLen) extBuffer 0 blockLen
--  let fullMerge = 2* subarrayLen
--  let blockCount = fullMerge `quot` blockLen
--  forM_ [0..(mergeCount - 1)] $ \mergeIndex -> do
--    let offset = start + mergeIndex * fullMerge
--    insertSort array firstKey blockCount
--
--    medianKey <- blockSelectSort array firstKey offset (subarrayLen `quot` blockLen) blockCount blockLen
--    mergeBlocksOutOfPlace array firstKey (firstKey + medianKey) offset blockCount blockLen 0 0
--  if lastSubarrays == 0
--  then pure (currBlockLen, currBlockOrigin)
--  else do
--    let offset = start + mergeCount * fullMerge
--    let blockCount = lastSubarrays `quot` blockLen
--    insertSort array firstKey (blockCount + 1)
--
--    medianKey <- blockSelectSort array firstKey offset (subarrayLen `quot` blockLen) blockCount blockLen
--    let lastFragment = lastSubarrays - blockCount * blockLen
--    lastMergeBlocks <- ( if lastFragment /= 0
--        then countLastMergeBlocks array offset blockCount blockLen
--        else pure 0  ) -- lastFragment array offset blockCount blockLen
--    let smartMerges = blockCount - lastMergeBlocks
--    if smartMerges == 0
--    then do
--      let leftLen = lastMergeBlocks * blockLen
--      mergeOutOfPlace array offset leftLen lastFragment blockLen
--      outOfPlaceBufferReset array start len blockLen
--      arrayCopy extBuffer 0 array (start - blockLen) blockLen
--      pure (currBlockLen, currBlockOrigin)
--    else do
--      val <- mergeBlocksOutOfPlace array firstKey (firstKey + medianKey) offset smartMerges blockLen lastMergeBlocks lastFragment
--      outOfPlaceBufferReset array start len blockLen
--      arrayCopy extBuffer 0 array (start - blockLen) blockLen
--      pure val
--

grailCombineBlocks array firstKey start length subarrayLen blockLen buffer extBuffer extBufferLen =
  let
    fullMerge = 2 * subarrayLen
    mergeCount = length `quot` fullMerge
    lastSubarrays = length - fullMerge * mergeCount
    (length', lastSubarrays') = if lastSubarrays <= subarrayLen
        then (length - lastSubarrays, 0)
        else (length, lastSubarrays)
  in
    if buffer && blockLen <= extBufferLen
    then grailCombineOutOfPlace array firstKey start length' subarrayLen blockLen mergeCount lastSubarrays' (fromJust extBuffer) extBufferLen
    else grailCombineInPlace array firstKey start length' subarrayLen blockLen mergeCount lastSubarrays' buffer

--combineBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Bool -> Int -> Subarray -> Maybe (VM.MVector (PrimState m) a)-> Int-> m (Int, Subarray)
--combineBlocks array firstKey start len subarrayLen blockLen buffer currBlockLen currBlockOrigin extBuffer extBufferLen = do
--    let fullMerge = 2 * subarrayLen
--    let mergeCount = len `quot` fullMerge
--    let lastSubarrays = len - (fullMerge * mergeCount)
--
--    if lastSubarrays <= subarrayLen
--    then do inner array mergeCount firstKey start (len - lastSubarrays) 0 subarrayLen blockLen buffer currBlockLen currBlockOrigin extBuffer extBufferLen
--    else do inner array mergeCount firstKey start len lastSubarrays subarrayLen blockLen buffer currBlockLen currBlockOrigin extBuffer extBufferLen
--    where
--      inner array mergeCount firstKey start len lastSubv subarrayLen blockLen buffer currBlockLen currBlockOrigin extBuffer extBufferLen
--        | buffer && blockLen < extBufferLen = combineOutOfPlace array firstKey start len subarrayLen blockLen mergeCount lastSubv (fromJust extBuffer) currBlockLen currBlockOrigin
--        | otherwise = combineInPlace array firstKey start len subarrayLen blockLen mergeCount lastSubv buffer currBlockLen currBlockOrigin

--
----shaved off plenty of lines of code with inclusion of the Extra module, and it starded from here
-----NEEDS ATTENTION URGENTLY
-----MIGHT BE THE SOURCE OF PROBLEM
--lazyMerge :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
--grailLazyMerge :: VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailLazyMerge :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailLazyMerge array start leftLen rightLen
  | leftLen < rightLen = let
    middle = start + leftLen
    in
      loopM ( \(start', leftLen', rightLen', middle') -> if leftLen /= 0
        then
        VM.read array start >>= (grailBinarySearchLeft array middle' rightLen'
                                                            >=>
                                                              (\ mergeLen
                                                                 -> (if mergeLen /= 0 then
                                                                         grailRotate array start' leftLen' mergeLen
                                                                           >>
                                                                             pure (start' + mergeLen, rightLen' - mergeLen, middle' + mergeLen)
                                                                     else
                                                                         pure (start', rightLen', middle'))
                                                                      >>=
                                                                        \ (start'', rightLen'', middle'')
                                                                          -> if rightLen'' == 0 then
                                                                                 pure $ Right ()
                                                                             else
                                                                                 loopM
                                                                                   (\ (st, ll)
                                                                                      -> ifM
                                                                                           (pure (ll /= 0) &&^ compareV array st middle'' (<=))
                                                                                           (pure $ Left (st + 1, ll - 1)) (pure $ Right (st, ll)))
                                                                                   (start'' + 1, leftLen' - 1)
                                                                                   >>=
                                                                                     \ (start''', leftLen'')
                                                                                       -> pure $ Left (start''', leftLen'', rightLen'', middle'')))
          --pure $ Left (start'', leftLen', rightLen'', middle'')
        else pure $ Right ()
      ) (start, leftLen, rightLen, middle)
  | otherwise = let
    end = start + leftLen + rightLen - 1
    in
      loopM ( \(leftLen', rightLen', end') -> if rightLen /= 0
        then
        VM.read array end >>= (grailBinarySearchRight array start leftLen'
                                                 >=>
                                                   (\ mergeLen
                                                      -> (if mergeLen /= leftLen' then
                                                              grailRotate array (start + mergeLen) (leftLen' - mergeLen) rightLen
                                                                >> pure (end' - (leftLen' - mergeLen), mergeLen)
                                                          else
                                                              pure (end', leftLen'))
                                                           >>=
                                                             \ (end'', leftLen'')
                                                               -> if leftLen'' == 0 then
                                                                      pure $ Right ()
                                                                  else
                                                                      let middle = start + leftLen''
                                                                      in
                                                                        loopM
                                                                          (\ (rl, en)
                                                                             -> ifM
                                                                                  (pure (rl /= 0) &&^ compareV array (middle - 1) en (<=))
                                                                                  (pure $ Left (rl - 1, en - 1)) (pure $ Right (rl, en)))
                                                                          (rightLen' - 1, end'' - 1)
                                                                          >>=
                                                                            \ (rightLen'', end''')
                                                                              -> pure $ Left (leftLen'', rightLen'', end''')))
          --pure $ Left (start', leftLen', rightLen', end'')
        else pure $ Right ()
      ) (leftLen, rightLen, end)
--  | leftLen < rightLen = whileLeft array start leftLen rightLen (start + leftLen)
--  | otherwise          = whileRight array start leftLen rightLen (start + leftLen + rightLen - 1)
--    where
--      whileLeft array start leftLen rightLen middle = when (leftLen /= 0) do
--        aStart <- VM.read array start
--        mergeLen <- binSearch array middle rightLen aStart (<)
--
--        (starter, rLen, mid) <- ( if mergeLen /= 0
--          then do
--            rotate array start leftLen mergeLen
--            pure (start + mergeLen, rightLen - mergeLen, middle + mergeLen)
--          else pure (start, rightLen, middle) )
--
--        when (rLen /= 0) do
--          (sta, lLen) <- loopM (\(st, lL) -> ifM (pure (lL /=0) &&^ compareVecVals array st mid (<=))
--              (pure $ Left (st + 1, lL - 1)) (pure $ Right (st, lL)) )(starter + 1, leftLen - 1)
--          whileLeft array sta lLen rLen mid
--      whileRight array start leftLen rightLen end = when (rightLen /= 0) do
--        aEnd <- VM.read array end
--        mergeLen <- binSearch array start leftLen aEnd (<=)
--        (ending, lLen) <- if mergeLen /= leftLen
--          then do
--            rotate array (start + mergeLen) (leftLen - mergeLen) rightLen
--            pure (end - (leftLen - mergeLen), leftLen - mergeLen)
--          else pure(end, leftLen)
--        when (lLen /= 0) do
--          let mid = start + lLen - 1
--          (rLen, endo) <- loopM (\(rL, e) -> ifM (pure (rL /= 0) &&^ compareVecVals array mid e (<=))
--              (pure $ Left (rL - 1, e - 1)) (pure $ Right (rL, e)) ) (rightLen - 1, ending - 1)
--          whileRight array start lLen rLen endo
--
--lazyStableSort :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m ()
--I was way too loud about linter not accepting array type and proposing some weird stuff
--which was oddly enough fixed bu brackets instead of dollar signs
--which, in turn, was discovered after overly complicating the code and letting linter fix it all
grailLazyStableSort :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m ()
grailLazyStableSort array start length =
    loopM (\index -> if index < length
      then let
        left = start + index - 1
        right = start + index
        in
          compareV array left right (>) >>= \res ->
          when res (VM.swap array left right) >>
          pure (Left (index + 2))
      else
        pure $ Right ()) 1 >>
    loopM (\mergeLen -> if mergeLen < length
      then let
        fullMerge = 2 * mergeLen
        mergeEnd = length - fullMerge
        in
        loopM (\mergeIndex -> if mergeIndex <= mergeEnd
            then
              grailLazyMerge array (start + mergeIndex) mergeLen mergeLen >>
              pure (Left (mergeIndex + fullMerge))
            else pure $ Right mergeIndex
          ) 0 >>= \mergeIndex ->
          let leftOver = length - mergeIndex in
          when (leftOver > mergeLen)
            (grailLazyMerge array (start + mergeIndex) mergeLen (leftOver - mergeLen)) >>
            pure (Left fullMerge)
      else
        pure $ Right ()) 2
--  forM_ [1,3..(len - 1)] $ \index -> do
--    let left = start + index - 1
--    let right = start + index
--    aLeft <- VM.read array left
--    aRight <- VM.read array right
--    when (aLeft > aRight) (VM.swap array left right)
--  ------forget whiles, I just went full forM_
--  forM_ (map (2^) [1..(logInt 2 len)]) $ \mergeLen -> do
--    let fullMerge = mergeLen * 2
--    let mergeEnd = len - fullMerge
--    mergeIndex <- inner array 0 mergeLen mergeEnd
--    let leftOver = len - mergeIndex
--    when (leftOver > mergeLen) (lazyMerge array (start + mergeIndex) mergeLen (leftOver - mergeLen))
--  where
--    inner array index mergeLen mergeEnd
--      | index <= mergeEnd = do
--        lazyMerge array index mergeLen mergeLen
--        inner array (index + 2*mergeLen) mergeLen mergeEnd
--      | otherwise = pure index
--
----this thing was rewritten more times than it should be said
--commonSort :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Maybe (VM.MVector (PrimState m) a) -> Int -> m ()
grailCommonSort :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Maybe (VM.MVector (PrimState m) a) -> Int -> m ()
grailCommonSort array start length extBuffer extBufferLen
  | length < 16  = grailInsertSort array start length
  | otherwise = --do return ()
    let
      blockLen = until (\x -> x*x >= length) (*2) 1
      keyLen = ((length - 1) `quot` blockLen) + 1
      idealKeys = keyLen + blockLen
    in
      grailCollectKeys array start length idealKeys >>= \keysFound -> if keysFound < idealKeys
      then if keysFound < 4
        then grailLazyStableSort array start length
        else let
        keyLen = until (<= keysFound) (`quot` 2) blockLen
        blockLen = 0
        idealBuffer = False
        in
          grailCommonSort' array start length extBuffer extBufferLen blockLen keyLen idealBuffer
      else let
        idealBuffer = True
        in
          grailCommonSort' array start length extBuffer extBufferLen blockLen keyLen idealBuffer

grailCommonSort' array start length extBuffer extBufferLen blockLen keyLen idealBuffer =
  let
  bufferEnd = blockLen + keyLen
  subarrayLen = if idealBuffer
      then blockLen
      else keyLen
  (extBuffer', extBufferLen') = if idealBuffer && isJust extBuffer
    then (extBuffer, extBufferLen)
    else (Nothing, 0)
  in
  grailBuildBlocks array (start + bufferEnd) (length - bufferEnd) subarrayLen extBuffer' extBufferLen' >>
  --loop goes here
  loopM ( \subarrayLen' -> if length - bufferEnd > 2 * subarrayLen'
    then let
      subarrayLen'' = subarrayLen' * 2
      currentBlockLen = blockLen
      scrollingBuffer = idealBuffer
      (currentBlockLen', scrollingBuffer') = if idealBuffer
          then (currentBlockLen, scrollingBuffer)
          else let keyBuffer = keyLen `quot` 2
              in if keyBuffer >= ((2 * subarrayLen'') `quot` keyBuffer)
                  then (keyBuffer, True)
                  else ((2 * subarrayLen'') `quot` keyLen, scrollingBuffer)
        in
          grailCombineBlocks array start (start + bufferEnd) (length - bufferEnd) subarrayLen'' currentBlockLen' scrollingBuffer' extBuffer extBufferLen >>
          pure (Left subarrayLen'')
        else
          pure $ Right ()
    ) subarrayLen >>
    grailInsertSort array start bufferEnd >>
    grailLazyMerge array start bufferEnd (length - bufferEnd)
  --grailInsertSort array start bufferEnd >>
  --grailLazyMerge array start bufferEnd (length - bufferEnd)
--    keysFound <- collectKeys array start len idealKeys
--
--    if keysFound < 4
--    then lazyStableSort array start len
--    else do
--
--      let (keyLength, blockLength, idealBuffer) =  if keysFound < idealKeys
--          then (until (<= keysFound) (`quot` 2) blockLen, 0, False)
--          else (keyLen, blockLen, True)
--
--      let bufferEnd = blockLength + keyLength
--      let subarrayLen = if idealBuffer then blockLength else keyLength
--
--      let (extBuffer, extBufferLen) = ( if idealBuffer && isJust extBuff
--          then (extBuff, extBuffLen)
--          else (Nothing , 0) )
--
--      buildBlocks array (start + bufferEnd) (len - bufferEnd) subarrayLen extBuffer extBufferLen
--
--      loopM (\(subvecLen, cBO) -> ifM (pure $ len - bufferEnd > subvecLen) (do
--        let (currentBlockLen, scrollingBuffer) = if not idealBuffer
--            then do
--              let keyBuffer = keyLength `quot` 2
--              if keyBuffer >= (2 * subvecLen) `quot` keyBuffer
--              then (keyBuffer, True)
--              else (2 * subvecLen `quot` keyLength, idealBuffer)
--            else (blockLen, idealBuffer)
--        res <- combineBlocks array start (start + bufferEnd) (len - bufferEnd) subvecLen currentBlockLen scrollingBuffer currentBlockLen cBO extBuffer extBufferLen
--        pure $ Left (2 * subvecLen, snd res))
--        (pure $ Right () ) ) (2 * subarrayLen, LEFT)
--
--      insertSort array start bufferEnd
--      lazyMerge array start bufferEnd (len - bufferEnd)



--general function, not exposed by default
--also the biggest pain in the bum during initial implementation, the reason of which was found later
--moved down here because it's only intermediary, and it stays here to simplify the code
grailSort :: Ord a => [a] -> Int -> Int -> Int -> [a]
grailSort list start len bufferLen
  | null list = []
  | otherwise = runST do --ugly as hell
      array <- V.thaw . V.fromList $ list
      if bufferLen == 0
      then grailCommonSort array start len Nothing 0
      else do --the buffer thing finally works!
        extBuffer <- V.thaw . V.replicate bufferLen $ head list
        grailCommonSort array start len (Just extBuffer) bufferLen
      V.toList <$> V.freeze array



-- area of functions one would consider public in other languages

-- no external buffer used
grailSortInPlace :: Ord a => [a] -> Int -> Int -> [a]
grailSortInPlace list start len = grailSort list start len 0

-- fixed-length external buffer used
grailSortStaticOOP :: Ord a => [a] -> Int -> Int -> [a]
grailSortStaticOOP list start len = grailSort list start len 512

-- external buffer with length of smallest power of 2 larger than square root of input's length used 
grailSortDynamicOOP :: Ord a => [a] -> Int -> Int -> [a]
grailSortDynamicOOP list start len = grailSort list start len $ until (\x -> x*x > length list) (*2) 1
