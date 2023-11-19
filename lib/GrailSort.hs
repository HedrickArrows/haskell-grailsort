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
  - Planning to rewrite in a manner providing proper code execution
    - It is pain to code
-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GrailSort (grailSortInPlace, grailSortStaticOOP, grailSortDynamicOOP) where

import Control.Monad ( when )
import Data.Maybe (fromJust, isJust)
import Control.Monad.ST (runST)
import Control.Monad.Primitive (PrimMonad (PrimState))
import Control.Monad.Extra ( (&&^), ifM, loopM )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Monoid (Endo(appEndo))

--area of functions one would consider private in other languages

data Subarray = LEFT | RIGHT deriving (Eq, Show)

----support function because I may need it
----and I actually use it to simplify some things for myself
----less-or-equal is here only in case of base value other than 2, might not need to be included here
--logInt :: Int -> Int -> Int
--logInt base value
--  | value <= 1 = 0
--  | otherwise  = logInt base (value `quot` base) + 1
--

----support function comparing two values in one vector to avoid further code repetition
----rewriting LiftA2 into <$> <*> syntax because it seems neater
compareV :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> (a -> a-> b) -> m b
compareV array fst snd op = op <$> VM.read array fst <*> VM.read array snd


grailBlockSwap :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailBlockSwap array a b blockLen = do
  loopM (\i -> if i < blockLen
      then do
            VM.swap array (a + i) (b + i)
            pure (Left (i + 1))
      else pure $ Right ()
   ) 0
--
----how did I fix it
grailRotate :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailRotate array start leftLen rightLen =
  loopM (\(start', leftL, rightL) -> if leftL > 0 && rightL > 0
    then (if leftL <= rightL
        then do
          grailBlockSwap array start' (start' + leftL) leftL
          pure $ Left (start' + leftL, leftL, rightL - leftL)
        else do
          grailBlockSwap array (start' + leftL - rightL) (start + leftL) rightL
          pure $ Left (start', leftL - rightL, rightL) )
    else pure $ Right ()
    ) (start, leftLen, rightLen)

----also known as optimized gnomesort since it uses swaps instead of insertion
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




grailBinarySearchLeft :: (Ord a, PrimMonad m) => VM.MVector (PrimState m) a -> Int -> Int -> a -> m Int
grailBinarySearchLeft array start length target =
    loopM ( \(left, right) -> ifM (pure (left < right))
      (let
        middle = left + ((right - left) `quot` 2)
        in
          ifM ((< target) <$> VM.read array (start + middle) )
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



grailCollectKeys :: (Ord a, PrimMonad m) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m Int
grailCollectKeys array start length idealKeys =
  loopM (\ (keysFound, firstKey, currKey) -> if currKey < length && keysFound < idealKeys
    then let
      newFirstKey = currKey - keysFound
      currKey' = currKey + 1
      in do
        pos <- VM.read array (start + currKey)
        insertPos <- grailBinarySearchLeft array (start + firstKey) keysFound pos
        result <- compareV array (start + currKey) (start + firstKey + insertPos) (/=)
        if insertPos == keysFound || result
        then do
          grailRotate array (start + firstKey) keysFound (currKey - (firstKey + keysFound))
          grailRotate array (start + newFirstKey + insertPos) (keysFound - insertPos) 1
          pure $ Left (keysFound + 1, newFirstKey, currKey')
        else pure $ Left (keysFound, firstKey, currKey' )
    else do
      grailRotate array start firstKey keysFound
      pure $ Right keysFound
  ) (1, 0, 1)

grailPairwiseSwaps :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m ()
grailPairwiseSwaps array start length =
  loopM (\index ->
    let
      left = start + index - 1
      right = start + index
    in if index < length
    then do
      ifM (compareV array left right (>))
        (do
          VM.swap array (left - 2)  right
          VM.swap array (right - 2) left
          pure $ Left (index + 2))
        (do
          VM.swap array (left - 2)  left
          VM.swap array (right - 2) right
          pure $ Left (index + 2))
    else do
      when (left < start + length) (VM.swap array (left - 2) left)
      pure $ Right ()
  ) 1



grailPairwiseWrites :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m ()
grailPairwiseWrites array start length =
  loopM (\index ->
    let
      left = start + index - 1
      right = start + index
    in do
      lVal <- VM.read array left
      rVal <- VM.read array right
      if index < length
      then do
        ifM (compareV array left right (>))
          (do
            VM.write array (left - 2)  rVal
            VM.write array (right - 2) lVal)
          (do
            VM.write array (left - 2)  lVal
            VM.write array (right - 2) rVal)
        pure $ Left (index + 2)
      else do
        when (left < start + length) (VM.write array (left - 2) lVal)
        pure $ Right ()
  ) 1


grailMergeForwards :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> m ()
grailMergeForwards array start leftLen rightLen bufferOffset = let
  left   = start
  middle = start + leftLen
  right  = middle
  end    = middle + rightLen
  buffer = start - bufferOffset
  in loopM (\(left, right, buffer) ->
      if right < end
      then do
        cmp <- compareV array left right (>)
        if left == middle || cmp
        then do
          VM.swap array buffer right
          pure $ Left (left, right + 1, buffer + 1)
        else do
          VM.swap array buffer left
          pure $ Left (left + 1, right, buffer + 1)
      else do
        when (buffer /= left) (grailBlockSwap array buffer left (middle - left))
        pure $ Right ()
    ) (left, right, buffer)


grailMergeBackwards :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> m ()
grailMergeBackwards array start leftLen rightLen bufferOffset = let
      end    = start - 1
      left   = end + leftLen
      middle = left
      right  = middle + rightLen
      buffer = right + bufferOffset
  in loopM ( \(left, right, buffer) ->
    if left > end
    then do
      cmp <- compareV array left right (>)
      if right == middle || cmp
      then do
        VM.swap array buffer left
        pure $ Left (left - 1, right, buffer - 1)
      else do
        VM.swap array buffer right
        pure $ Left (left, right - 1, buffer - 1)
    else if right /= buffer
      then do
          loopM (\index ->
            if right - index > middle
            then do
              VM.swap array (buffer - index) (right - index)
              pure $ Left (index + 1)
            else pure $ Right ()
            ) 0
          pure $ Right ()
      else pure $ Right ()
    ) (left, right, buffer)



grailMergeOutOfPlace array start leftLen rightLen bufferOffset = pure ()
-- TO BE DONE LATER


grailBuildInPlace :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> m ()
grailBuildInPlace array start length currentLen bufferLen =
    {- start' <- -} loopM (\(mergeLen, start) -> if mergeLen < bufferLen
      then let
        fullMerge = mergeLen * 2
        mergeEnd = start + length - fullMerge
        bufferOffset = mergeLen
        in do
          mergeIndex <- loopM (\index -> if index > mergeEnd
            then pure $ Right index
            else do
              grailMergeForwards array index mergeLen mergeLen bufferOffset
              pure $ Left (index + fullMerge) ) start
          let leftOver = length - (mergeIndex - start)
          if leftOver > mergeLen
            then grailMergeForwards array mergeIndex mergeLen (leftOver - mergeLen) bufferOffset
            else grailRotate array (mergeIndex - mergeLen) mergeLen leftOver
          pure $ Left (fullMerge, start - mergeLen)
        else let
            fullMerge = 2 * bufferLen
            lastBlock = length `rem` fullMerge
            lastOffset = start + length - lastBlock
            mergeIndex = lastOffset - fullMerge
          in if lastBlock <= bufferLen
            then do
              grailRotate array lastOffset lastBlock bufferLen
              buildInPlace' array start bufferLen fullMerge mergeIndex
              pure $ Right ()
            else do
              grailMergeBackwards array lastOffset bufferLen (lastBlock - bufferLen) bufferLen
              buildInPlace' array start bufferLen fullMerge mergeIndex
              pure $ Right ()
      ) (currentLen, start)
    --pure ()
    where
      buildInPlace' array start bufferLen fullMerge =
        loopM (\mergeIndex -> if mergeIndex >= start
            then do
              grailMergeBackwards array mergeIndex bufferLen bufferLen bufferLen
              pure $ Left (mergeIndex - fullMerge)
            else pure $ Right ())


    -- THE FOLLOWING PART IS FOR FURTHER INSPECTION
    --let
    --  fullMerge = 2 * bufferLen
    --  lastBlock = length `rem` fullMerge
    --  lastOffset = start' + length - lastBlock
    --  mergeIndex = lastOffset - fullMerge
--
    --if lastBlock <= bufferLen
    --then grailRotate array lastOffset lastBlock bufferLen
    --else grailMergeBackwards array lastOffset bufferLen (lastBlock - bufferLen) bufferLen
    --loopM ( \index ->
    --  if index < start'
    --  then do
    --    pure $ Right ()
    --  else do
    --    grailMergeBackwards array index bufferLen bufferLen bufferLen
    --    pure $ Left (index - fullMerge)
    --  ) mergeIndex



grailBuildOutOfPlace array start length bufferLen extLen extBuffer extBufferLen = pure ()
--- OOP METHOD, TO BE DONE LATER

grailBuildBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Maybe (VM.MVector (PrimState m) a) -> Int -> m ()
grailBuildBlocks array start length bufferLen extBuffer extBufferLen
  | isJust extBuffer =
    grailBuildOutOfPlace array start length bufferLen extLen (fromJust extBuffer) extBufferLen
  | otherwise = do
    grailPairwiseSwaps array start length
    grailBuildInPlace array (start - 2) length 2 bufferLen
  where
    extLen
      | bufferLen < extBufferLen = bufferLen
      | otherwise = until (\x -> x*2 > extBufferLen) (*2) 1





grailBlockSelectSort :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> m Int
grailBlockSelectSort  array firstKey start medianKey blockCount blockLen =
    loopM (\(firstBlock, medianKey)  -> if firstBlock < blockCount
        then do
          selectBlock <- loopM (\(selectBlock, currBlock) -> if currBlock < blockCount
            then do
              cmp <- compareV array (start + (currBlock * blockLen)) (start + (selectBlock * blockLen)) compare
              lt <- compareV array (firstKey + currBlock) (firstKey + selectBlock) (<)
              if (cmp == LT) || (cmp == EQ && lt)
              then pure $ Left (currBlock, currBlock + 1)
              else pure $ Left (selectBlock, currBlock + 1)
            else pure $ Right selectBlock
            ) (firstBlock, firstBlock + 1)
          if selectBlock /= firstBlock
          then do
            grailBlockSwap array (start + (firstBlock * blockLen)) (start + (selectBlock * blockLen)) blockLen
            VM.swap array (firstKey + firstBlock) (firstKey + selectBlock)
            if | medianKey == firstBlock  -> pure $ Left (firstBlock + 1, selectBlock)
               | medianKey == selectBlock -> pure $ Left (firstBlock + 1, firstBlock)
               | otherwise                -> pure $ Left (firstBlock + 1, medianKey)
          else pure $ Left (firstBlock + 1, medianKey)
        else pure $ Right medianKey
      ) (0, medianKey)


grailInPlaceBufferReset :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailInPlaceBufferReset array start length bufferOffset =
  let
      buffer = start + length - 1
      index = buffer - bufferOffset
  in
  loopM (\(buffer, index) ->
      if buffer >= start
      then do
        VM.swap array index buffer
        pure $ Left (buffer - 1, index - 1)
      else pure $ Right ()
  ) (buffer, index)


grailOutOfPlaceBufferReset array start length bufferOffset = pure ()


grailInPlaceBufferRewind array start leftBlock buffer =
  loopM (\(leftBlock, buffer) -> if leftBlock >= start
    then do
      VM.swap array buffer leftBlock
      pure $ Left (leftBlock - 1, buffer - 1)
    else pure $ Right ()
  ) (leftBlock, buffer)


grailOutOfPlaceBufferRewind array start leftBlock buffer = pure ()


grailGetSubarray :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m Subarray
grailGetSubarray array currentKey medianKey =
  ifM (compareV array currentKey medianKey (<))
      (pure LEFT)
      (pure RIGHT)


grailCountLastMergeBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m Int
grailCountLastMergeBlocks array offset blockCount blockLen =
  let
      blocksToMerge = 0
      lastRightFrag = offset + (blockCount * blockLen)
      prevLeftBlock = lastRightFrag - blockLen
  in
    loopM (\(blocksToMerge, prevLeftBlock) -> 
      ifM
        (pure (blocksToMerge < blockCount) &&^ compareV array lastRightFrag prevLeftBlock (<))
        (pure $ Left (blocksToMerge + 1, prevLeftBlock - blockLen))
        (pure $ Right blocksToMerge)
      ) (blocksToMerge, prevLeftBlock)


grailSmartMerge :: (Ord a, PrimMonad m) => VM.MVector (PrimState m) a -> Int -> Int -> Subarray -> Int -> Int -> Int -> Subarray -> m (Int, Subarray)
grailSmartMerge array start leftLen leftOrigin rightLen bufferOffset currBlockLen currBlockOrigin =
  let
    left = start
    middle = start + leftLen
    right = middle
    end = middle + rightLen
    buffer = start - bufferOffset
    op
      | leftOrigin == LEFT = (<=)
      | otherwise = (<)
  in loopM (\(left, right, buffer) ->
      if left < middle && right < end
      then ifM (compareV array left right op) (do
        VM.swap array buffer left
        pure $ Left (left + 1, right, buffer + 1)) (do
        VM.swap array buffer right
        pure $ Left (left, right + 1, buffer + 1))
      else if middle < left
        then do
          grailInPlaceBufferRewind array left (middle - 1) (end - 1)
          pure $ Right (middle - left, currBlockOrigin)
        else if leftOrigin == LEFT
          then pure $ Right (end - right, RIGHT)
          else pure $ Right (end - right, LEFT)
      ) (left, right, buffer)


grailSmartLazyMerge :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Subarray -> Int -> Int -> Subarray -> m (Int, Subarray)
grailSmartLazyMerge array start leftLen leftOrigin rightLen currBlockLen currBlockOrigin =
  let
    middle = start + leftLen
  in do
    cmp <- compareV array (middle - 1) middle compare
    if | leftOrigin == LEFT && cmp == GT -> loopM (\(start, leftLen, middle, rightLen) -> 
          if leftLen /= 0
          then do
            aStart <- VM.read array start
            mergeLen <- grailBinarySearchLeft array middle rightLen aStart
            when (mergeLen /= 0) (grailRotate array start leftLen mergeLen)
            let
              (start', rightLen', middle')
                | mergeLen /= 0 = (start + mergeLen, rightLen - mergeLen, middle + mergeLen)
                | otherwise     = (start, rightLen, middle)
            if rightLen' == 0 
            then pure $ Right (leftLen, currBlockOrigin)
            else do 
              (start'', leftLen') <- loopM ( \(start,leftLen) ->
                  ifM (pure (leftLen /= 0) &&^ compareV array start middle' (<=))
                  (pure $ Left (start + 1, leftLen - 1)) (pure $ Right (start, leftLen)))  (start' + 1, leftLen - 1)
              pure $ Left (start'', leftLen', middle', rightLen')
          else pure $ Right $ grailSmartLazyMerge' leftOrigin rightLen
        ) (start, leftLen, middle, rightLen)
       | leftOrigin /= LEFT && cmp /= LT -> loopM (\(start, leftLen, middle, rightLen) -> 
          if leftLen /= 0
          then do
            aStart <- VM.read array start
            mergeLen <- grailBinarySearchRight array middle rightLen aStart
            when (mergeLen /= 0) (grailRotate array start leftLen mergeLen)
            let
              (start', rightLen', middle')
                | mergeLen /= 0 = (start + mergeLen, rightLen - mergeLen, middle + mergeLen)
                | otherwise     = (start, rightLen, middle)
            if rightLen' == 0 
            then pure $ Right (leftLen, currBlockOrigin)
            else do 
              (start'', leftLen') <- loopM ( \(start,leftLen) ->
                ifM (pure (leftLen /= 0) &&^ compareV array start middle' (<))
                (pure $ Left (start + 1, leftLen - 1)) (pure $ Right (start, leftLen)))  (start' + 1, leftLen - 1)
              pure $ Left (start'', leftLen', middle', rightLen')
          else pure $ Right $ grailSmartLazyMerge' leftOrigin rightLen
        ) (start, leftLen, middle, rightLen)
       | otherwise -> pure $ grailSmartLazyMerge' leftOrigin rightLen

grailSmartLazyMerge' :: Subarray -> Int -> (Int, Subarray)
grailSmartLazyMerge' leftOrigin rightLen = (currBlockLen, currBlockOrigin)
    where
        currBlockLen = rightLen
        currBlockOrigin
            | leftOrigin == LEFT = RIGHT
            | otherwise          = LEFT

grailSmartMergeOutOfPlace array start leftLen leftOrigin rightLen bufferOffset = pure ()


grailMergeBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> m ()
grailMergeBlocks array firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen =
  let
    nextBlock = start + blockLen
    currBlockLen = blockLen
  in do
    currBlockOrigin <- grailGetSubarray array firstKey medianKey
    loopM (\(nextBlock, currBlockLen, currBlockOrigin, keyIndex) ->
        if keyIndex < blockCount
        then let
          currBlock = nextBlock - currBlockLen
          buffer = currBlock - blockLen
        in do
          nextBlockOrigin <- grailGetSubarray array (firstKey + keyIndex) medianKey
          (currBlockLen', currBlockOrigin') <- if nextBlockOrigin == currBlockOrigin
            then do
              grailBlockSwap array buffer currBlock currBlockLen
              pure (blockLen, currBlockOrigin)
            else grailSmartMerge array currBlock currBlockLen currBlockOrigin blockLen blockLen currBlockLen currBlockOrigin
          pure $ Left (nextBlock+blockLen, currBlockLen', currBlockOrigin', keyIndex+1)
        else let
            currBlock = nextBlock - currBlockLen
            buffer = currBlock - blockLen
            (currBlock', currBlockLen')
                | currBlockOrigin == RIGHT = (nextBlock, blockLen * lastMergeBlocks)
                | otherwise = (currBlock, currBlockLen + blockLen * lastMergeBlocks)
          in do 
            if lastLen /= 0
            then do
              when (currBlockOrigin == RIGHT) (grailBlockSwap array buffer currBlock currBlockLen) 
              grailMergeForwards array currBlock' currBlockLen' lastLen blockLen
            else grailBlockSwap array buffer currBlock currBlockLen
            pure $ Right ()   
      ) (nextBlock, currBlockLen, currBlockOrigin, 1)


grailLazyMergeBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> m ()
grailLazyMergeBlocks array firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen =
  do
      currBlockOrigin <- grailGetSubarray array firstKey medianKey
      loopM (\(nextBlock, currBlockLen, currBlockOrigin, keyIndex) ->
        if keyIndex < blockCount
        then let
          currBlock = nextBlock - currBlockLen
        in do
          nextBlockOrigin <- grailGetSubarray array (firstKey + keyIndex) medianKey
          if nextBlockOrigin == currBlockOrigin
          then pure $ Left (nextBlock + blockLen, currBlockLen, currBlockOrigin, keyIndex + 1)
          else do
            (currBlockLen', currBlockOrigin') <- grailSmartLazyMerge array currBlock currBlockLen currBlockOrigin blockLen currBlockLen currBlockOrigin
            pure $ Left (nextBlock + blockLen, currBlockLen', currBlockOrigin', keyIndex + 1)
        else let
              currBlock = nextBlock - currBlockLen
              (currBlock', currBlockLen')
                      | currBlockOrigin == RIGHT = (nextBlock, blockLen * lastMergeBlocks)
                      | otherwise = (currBlock, currBlockLen + blockLen * lastMergeBlocks)
          in do
            when (lastLen /= 0) (grailLazyMerge array currBlock' currBlockLen' lastLen)
            pure $ Right ()
        ) (start + blockLen, blockLen, currBlockOrigin, 1)


grailMergeBlocksOutOfPlace array firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen = pure ()


grailCombineInPlace :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool -> m ()
grailCombineInPlace array firstKey start length subarrayLen blockLen mergeCount lastSubarrays buffer =
  let
    fullMerge = 2 * subarrayLen
    blockCount = fullMerge `quot` blockLen
  in
    loopM (\mergeIndex ->
      if mergeIndex < mergeCount
      then let
        offset = start + (mergeIndex * fullMerge)
        medianKey = subarrayLen `quot` blockLen
        in do
          grailInsertSort array firstKey blockCount
          medianKey' <- grailBlockSelectSort array firstKey offset medianKey blockCount blockLen

          if buffer
          then
            grailMergeBlocks array firstKey (firstKey + medianKey') offset blockCount blockLen 0 0
          else
            grailLazyMergeBlocks array firstKey (firstKey + medianKey') offset blockCount blockLen 0 0
          pure $ Left (mergeIndex + 1)
      else let
          offset = start + mergeCount * fullMerge
          blockCount = lastSubarrays `quot` blockLen
          medianKey = subarrayLen `quot` blockLen
          lastFragment = lastSubarrays - (blockCount * blockLen)
        in do
          grailInsertSort array firstKey (blockCount +1)
          medianKey' <- grailBlockSelectSort array firstKey offset medianKey blockCount blockLen
          
          lastMergeBlocks <- if  lastFragment /= 0 
            then grailCountLastMergeBlocks array offset blockCount blockLen
            else pure 0

          let
            smartMerges = blockCount - lastMergeBlocks
            leftLen = lastMergeBlocks * blockLen

          if | smartMerges == 0 && buffer ->
               grailMergeForwards array offset leftLen lastFragment blockLen
             | smartMerges == 0 && not buffer  ->
              grailLazyMerge array offset leftLen lastFragment
             | buffer    ->
              grailMergeBlocks array firstKey (firstKey + medianKey') offset smartMerges blockLen lastMergeBlocks lastFragment
             | otherwise -> 
              grailLazyMergeBlocks array firstKey (firstKey + medianKey') offset smartMerges blockLen lastMergeBlocks lastFragment

          when buffer (grailInPlaceBufferReset array start length blockLen)
          pure $ Right ()
      ) 0



grailCombineOutOfPlace array firstKey start length subarrayLen blockLen mergeCount lastSubarrays extBuffer extBufferLen = pure ()--(array, extBuffer) 

grailCombineBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Bool -> Maybe (VM.MVector (PrimState m) a) -> Int -> m ()
grailCombineBlocks array firstKey start length subarrayLen blockLen buffer extBuffer extBufferLen
  | buffer && blockLen <= extBufferLen = grailCombineOutOfPlace array firstKey start length' subarrayLen blockLen mergeCount lastSubarrays' (fromJust extBuffer) extBufferLen
  | otherwise = grailCombineInPlace array firstKey start length' subarrayLen blockLen mergeCount lastSubarrays' buffer
  where
    fullMerge = 2 * subarrayLen
    mergeCount = length `quot` fullMerge
    lastSubarrays = length - fullMerge * mergeCount
    (length', lastSubarrays')
      | lastSubarrays <= subarrayLen = (length - lastSubarrays, 0)
      | otherwise = (length, lastSubarrays)

grailLazyMerge :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailLazyMerge array start leftLen rightLen
  | leftLen < rightLen =
      loopM (\(leftLen, rightLen,  start, middle) ->
        if leftLen /= 0
        then do
          aStart <- VM.read array start
          mergeLen <- grailBinarySearchLeft array middle rightLen aStart
          (start', rightLen', middle') <- if mergeLen /= 0
              then do
                grailRotate array start leftLen mergeLen
                pure (start + mergeLen, rightLen - mergeLen, middle + mergeLen)
              else pure (start, rightLen, middle)
          if rightLen' == 0
          then pure $ Right ()
          else do
              (start'', leftLen') <- loopM ( \(start, leftLen) ->
                  ifM (pure (leftLen /= 0) &&^ compareV array start middle' (<=))
                    (pure $ Left (start +1, leftLen -1))
                    (pure $ Right (start, leftLen))
                  ) (start' +1, leftLen -1)
              pure $ Left (leftLen', rightLen', start'', middle')
          else pure $ Right ()
      ) (leftLen, rightLen, start, start + leftLen)

  | otherwise =
      loopM (\(leftLen, rightLen, end) ->
        if rightLen /= 0
        then do
          aEnd <- VM.read array end
          mergeLen <- grailBinarySearchRight array start leftLen aEnd
          (end', leftLen') <- if mergeLen /= leftLen
            then do
              grailRotate array (start + mergeLen) (leftLen - mergeLen) rightLen
              pure (end - (leftLen - mergeLen), mergeLen)
            else pure (end, leftLen)
          let middle = start + leftLen' in if leftLen' == 0
            then pure $ Right ()
            else do
              (rightLen', end'') <- loopM ( \(rightLen, end) ->
                ifM (pure (rightLen /= 0) &&^ compareV array (middle - 1) end (<=))
                  (pure $ Left (rightLen -1, end -1))
                  (pure $ Right (rightLen, end))
                ) (rightLen -1, end' -1)
              pure $ Left (leftLen', rightLen', end'')
        else pure $ Right ()
      ) (leftLen, rightLen, start + leftLen + rightLen - 1)

grailLazyStableSort :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m ()
grailLazyStableSort array start length = do
  loopM (\index -> if index < length
    then do
      VM.swap array (start + index - 1) (start + index)
      pure $ Left (index + 2)
    else pure $ Right ()
    ) 1

  loopM (\mergeLen -> if mergeLen < length
    then let
      fullMerge = 2* mergeLen
      mergeEnd = length - fullMerge
      in do
        mergeIndex <- loopM (\mergeIndex ->
            if mergeIndex <= mergeEnd
            then do
              grailLazyMerge array (start + mergeIndex) mergeLen mergeLen
              pure $ Left (mergeIndex + fullMerge)
            else pure $ Right mergeIndex
          ) 0
        let
          leftOver = length - mergeIndex
        when (leftOver > mergeLen) (grailLazyMerge array (start + mergeIndex) mergeLen (leftOver - mergeLen))
        pure $ Left (mergeLen * 2)
      else pure $ Right ()
    ) 2


grailCommonSort :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Maybe (VM.MVector (PrimState m) a) -> Int -> m ()
grailCommonSort array start length extBuffer extBufferLen
  | length < 16 = grailInsertSort array start length
  | otherwise = let
    blockLen = until (\x -> x*x >= length) (*2) 1
    keyLen = ((length - 1) `quot` blockLen) + 1
    idealKeys = keyLen + blockLen
    blockLen' = 0
    in do
      keysFound <- grailCollectKeys array start length idealKeys
      if keysFound < idealKeys
      then if keysFound < 4
        then grailLazyStableSort array start length
        else let keyLen' = until (<= keysFound) (`quot` 2) blockLen
          in grailCommonSort' array start length extBuffer extBufferLen blockLen' keyLen' False
      else   grailCommonSort' array start length extBuffer extBufferLen blockLen  keyLen  True

grailCommonSort' :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Maybe (VM.MVector (PrimState m) a) -> Int -> Int -> Int -> Bool -> m ()
grailCommonSort' array start length extBuffer extBufferLen blockLen keyLen idealBuffer =
  let
    bufferEnd = blockLen + keyLen
    subarrayLen
      | idealBuffer = blockLen
      | otherwise   = keyLen
    (extBuffer', extBufferLen')
      | idealBuffer && isJust extBuffer = (extBuffer, extBufferLen)
      | otherwise = (Nothing, 0)
  in do 
    grailBuildBlocks array (start + bufferEnd) (length - bufferEnd) subarrayLen extBuffer' extBufferLen'
    grailCommonSort'' array start length extBuffer' extBufferLen' blockLen keyLen idealBuffer bufferEnd subarrayLen


grailCommonSort'' :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Maybe (VM.MVector (PrimState m) a) -> Int -> Int -> Int -> Bool -> Int -> Int -> m ()
grailCommonSort'' array start length extBuffer extBufferLen blockLen keyLen idealBuffer bufferEnd =
  loopM (\subarrayLen -> if length - bufferEnd > 2 * subarrayLen
      then let
        subarrayLen' = subarrayLen * 2
        currentBlockLen = blockLen
        scrollingBuffer = idealBuffer
        keyBuffer = keyLen `quot` 2
        (currentBlockLen', scrollingBuffer')
            | idealBuffer                                        = (currentBlockLen, scrollingBuffer)
            | keyBuffer >= ((2 * subarrayLen') `quot` keyBuffer) = (keyBuffer, True)
            | otherwise                                          = ((2 * subarrayLen') `quot` keyLen, scrollingBuffer)
        in do
          grailCombineBlocks array  start (start + bufferEnd) (length - bufferEnd) subarrayLen' currentBlockLen' scrollingBuffer' extBuffer extBufferLen
          pure $ Left subarrayLen'
      else do
        grailInsertSort array start bufferEnd
        grailLazyMerge array start bufferEnd (length - bufferEnd)
        pure $ Right ())




-- area of functions one would consider public in other languages
-- intermediary function existed to pass the input forward, was later integrated 
-- directly into public functions

-- no external buffer used
grailSortInPlace :: Ord a => [a] -> Int -> Int -> [a]
grailSortInPlace list start len
  | null list = []
  | otherwise = runST $ do
    array <- V.thaw . V.fromList $ list
    grailCommonSort array start len Nothing 0
    V.toList <$> V.freeze array

-- fixed-length external buffer used
grailSortStaticOOP :: Ord a => [a] -> Int -> Int -> [a]
grailSortStaticOOP list start len
  | null list = []
  | otherwise = runST $ do
    array <- V.thaw . V.fromList $ list
    extBuffer <- V.thaw . V.replicate 512 $ head list
    grailCommonSort array start len (Just extBuffer) 512
    V.toList <$> V.freeze array

-- external buffer with length of smallest power of 2 larger than square root of input's length used 
grailSortDynamicOOP :: Ord a => [a] -> Int -> Int -> [a]
grailSortDynamicOOP list start len
  | null list = []
  | otherwise = runST $ do
    array <- V.thaw . V.fromList $ list
    extBuffer <- V.thaw . V.replicate bufferLen $ head list
    grailCommonSort array start len (Just extBuffer) bufferLen
    V.toList <$> V.freeze array
    where
      bufferLen = until (\x -> x^2 >= len) (*2) 1

