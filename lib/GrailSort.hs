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

Current status: Done
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

--area of functions one would consider private in other languages

data Subvector = LEFT | RIGHT deriving (Eq, Show)

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
compareV vector fst snd op = op <$> VM.read vector fst <*> VM.read vector snd

vectorCopy :: (PrimMonad m) =>VM.MVector (PrimState m) a -> Int -> VM.MVector (PrimState m) a -> Int -> Int -> m ()
vectorCopy fromVector fromIndex toVector toIndex length =
  loopM (\i -> if i < length
    then do
      v <- VM.read fromVector (fromIndex + i)
      VM.write toVector (toIndex + i) v
      pure $ Left (i+1)
    else pure $ Right ()
    ) 0


grailBlockSwap :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailBlockSwap vector a b blockLen =
  loopM (\i -> if i < blockLen
      then do
            VM.swap vector (a + i) (b + i)
            pure (Left (i + 1))
      else pure $ Right ()
   ) 0
--
----how did I fix it
grailRotate :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailRotate vector start leftLen rightLen =
  loopM (\(start, leftLen, rightLen) -> if leftLen > 0 && rightLen > 0
    then
      if leftLen <= rightLen
      then do
        grailBlockSwap vector start (start + leftLen) leftLen
        pure $ Left (start + leftLen, leftLen, rightLen - leftLen)
      else do
        grailBlockSwap vector (start + leftLen - rightLen) (start + leftLen) rightLen
        pure $ Left (start, leftLen - rightLen, rightLen)
    else pure $ Right ()
    ) (start, leftLen, rightLen)

----also known as optimized gnomesort since it uses swaps instead of insertion
grailInsertSort :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m ()
grailInsertSort vector start l = let
  begin = start + 1
  end = start + l
  in
    loopM ( \j -> if j < end
      then do
          loopM (\i -> ifM (pure (start<i) &&^ compareV vector i (i-1) (<)) (do
            VM.swap vector i (i-1)
            pure $ Left (i-1))
            (pure $ Right ())) j
          pure $ Left (j + 1)
      else pure $ Right () ) begin




grailBinarySearchLeft :: (Ord a, PrimMonad m) => VM.MVector (PrimState m) a -> Int -> Int -> a -> m Int
grailBinarySearchLeft vector start length target =
    loopM ( \(left, right) -> ifM (pure (left < right))
      (let
        middle = left + ((right - left) `quot` 2)
        in
          ifM ((< target) <$> VM.read vector (start + middle) )
          (pure $ Left (middle + 1, right))
          (pure $ Left (left, middle)))
      (pure $ Right left)
    ) (0, length)



grailBinarySearchRight :: (Ord a, PrimMonad m) => VM.MVector (PrimState m) a -> Int -> Int -> a -> m Int
grailBinarySearchRight vector start length target =
    loopM ( \(left, right) -> ifM (pure (left < right))
      (let
        middle = left + ((right - left) `quot` 2)
        in
          ifM ((> target) <$> VM.read vector (start + middle) )
          (pure $ Left (left, middle))
          (pure $ Left (middle + 1, right)))
      (pure $ Right left)
    ) (0, length)



grailCollectKeys :: (Ord a, PrimMonad m) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m Int
grailCollectKeys vector start length idealKeys =
  loopM (\ (keysFound, firstKey, currKey) -> if currKey < length && keysFound < idealKeys
    then let
      newFirstKey = currKey - keysFound
      currKey' = currKey + 1
      in do
        pos <- VM.read vector (start + currKey)
        insertPos <- grailBinarySearchLeft vector (start + firstKey) keysFound pos
        result <- compareV vector (start + currKey) (start + firstKey + insertPos) (/=)
        if insertPos == keysFound || result
        then do
          grailRotate vector (start + firstKey) keysFound (currKey - (firstKey + keysFound))
          grailRotate vector (start + newFirstKey + insertPos) (keysFound - insertPos) 1
          pure $ Left (keysFound + 1, newFirstKey, currKey')
        else pure $ Left (keysFound, firstKey, currKey' )
    else do
      grailRotate vector start firstKey keysFound
      pure $ Right keysFound
  ) (1, 0, 1)

grailPairwiseSwaps :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m ()
grailPairwiseSwaps vector start length =
  loopM (\index ->
    let
      left = start + index - 1
      right = start + index
    in if index < length
    then ifM (compareV vector left right (>))
        (do
          VM.swap vector (left - 2)  right
          VM.swap vector (right - 2) left
          pure $ Left (index + 2))
        (do
          VM.swap vector (left - 2)  left
          VM.swap vector (right - 2) right
          pure $ Left (index + 2))
    else do
      when (left < start + length) (VM.swap vector (left - 2) left)
      pure $ Right ()
  ) 1



grailPairwiseWrites :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m ()
grailPairwiseWrites vector start length =
  loopM (\index -> if index < length 
    then let 
      left = start + index - 1
      right = start + index
    in do 
      lVal <- VM.read vector left 
      rVal <- VM.read vector right 

      if lVal > rVal 
      then do 
        VM.write vector (left - 2) rVal
        VM.write vector (right - 2) lVal
      else do 
        VM.write vector (left - 2) lVal
        VM.write vector (right - 2) rVal
      pure $ Left (index + 2)

    else let 
      left = start + index - 1 
    in do 
      when (left < start + length) do 
        v <- VM.read vector left 
        VM.write vector (left - 2) v 
      pure $ Right ()
  ) 1


grailMergeForwards :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> m ()
grailMergeForwards vector start leftLen rightLen bufferOffset = let
  left   = start
  middle = start + leftLen
  right  = middle
  end    = middle + rightLen
  buffer = start - bufferOffset
  in loopM (\(left, right, buffer) ->
      if right < end
      then do
        cmp <- compareV vector left right (>)
        if left == middle || cmp
        then do
          VM.swap vector buffer right
          pure $ Left (left, right + 1, buffer + 1)
        else do
          VM.swap vector buffer left
          pure $ Left (left + 1, right, buffer + 1)
      else do
        when (buffer /= left) (grailBlockSwap vector buffer left (middle - left))
        pure $ Right ()
    ) (left, right, buffer)


grailMergeBackwards :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> m ()
grailMergeBackwards vector start leftLen rightLen bufferOffset = let
      end    = start - 1
      left   = end + leftLen
      middle = left
      right  = middle + rightLen
      buffer = right + bufferOffset
  in loopM ( \(left, right, buffer) ->
    if left > end
    then do
      cmp <- compareV vector left right (>)
      if right == middle || cmp
      then do
        VM.swap vector buffer left
        pure $ Left (left - 1, right, buffer - 1)
      else do
        VM.swap vector buffer right
        pure $ Left (left, right - 1, buffer - 1)
    else if right /= buffer
      then do
          loopM (\index ->
            if right - index > middle
            then do
              VM.swap vector (buffer - index) (right - index)
              pure $ Left (index + 1)
            else pure $ Right ()
            ) 0
          pure $ Right ()
      else pure $ Right ()
    ) (left, right, buffer)



grailMergeOutOfPlace :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> m ()
grailMergeOutOfPlace vector start leftLen rightLen bufferOffset = let
    left = start
    middle = start + leftLen
    right = middle
    end = middle + rightLen
    buffer = start - bufferOffset
  in loopM (\(left,right,buffer) ->
    if right < end
    then do
      res <- compareV vector left right (>)
      if left == middle || res
      then do
        v <- VM.read vector right
        VM.write vector buffer v
        pure $ Left (left, right + 1, buffer + 1)
      else do
        v <- VM.read vector left
        VM.write vector buffer v
        pure $ Left (left + 1, right, buffer + 1)
    else do
      when (buffer /= left) (loopM (\(left,buffer)->
          if left < middle 
          then do 
            v <- VM.read vector left 
            VM.write vector buffer v 
            pure $ Left (left + 1, buffer + 1)
          else pure $ Right () ) (left,buffer ))
      pure $ Right ()
    ) (left,right,buffer)




grailBuildInPlace :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> m ()
grailBuildInPlace vector start length currentLen bufferLen =
    {- start' <- -} loopM (\(mergeLen, start) -> if mergeLen < bufferLen
      then let
        fullMerge = mergeLen * 2
        mergeEnd = start + length - fullMerge
        bufferOffset = mergeLen
        in do
          mergeIndex <- loopM (\index -> if index > mergeEnd
            then pure $ Right index
            else do
              grailMergeForwards vector index mergeLen mergeLen bufferOffset
              pure $ Left (index + fullMerge) ) start
          let leftOver = length - (mergeIndex - start)
          if leftOver > mergeLen
            then grailMergeForwards vector mergeIndex mergeLen (leftOver - mergeLen) bufferOffset
            else grailRotate vector (mergeIndex - mergeLen) mergeLen leftOver
          pure $ Left (fullMerge, start - mergeLen)
        else let
            fullMerge = 2 * bufferLen
            lastBlock = length `rem` fullMerge
            lastOffset = start + length - lastBlock
            mergeIndex = lastOffset - fullMerge
          in if lastBlock <= bufferLen
            then do
              grailRotate vector lastOffset lastBlock bufferLen
              buildInPlace' vector start bufferLen fullMerge mergeIndex
              pure $ Right ()
            else do
              grailMergeBackwards vector lastOffset bufferLen (lastBlock - bufferLen) bufferLen
              buildInPlace' vector start bufferLen fullMerge mergeIndex
              pure $ Right ()
      ) (currentLen, start)
    --pure ()
    where
      buildInPlace' vector start bufferLen fullMerge =
        loopM (\mergeIndex -> if mergeIndex >= start
            then do
              grailMergeBackwards vector mergeIndex bufferLen bufferLen bufferLen
              pure $ Left (mergeIndex - fullMerge)
            else pure $ Right ())



grailBuildOutOfPlace :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> VM.MVector (PrimState m) a -> Int -> m ()
grailBuildOutOfPlace vector start length bufferLen extLen extBuffer extBufferLen = 
  do 
    vectorCopy vector (start - extLen) extBuffer 0 extLen 
    grailPairwiseWrites vector start length 

    loopM (\(start,mergeLen) -> if mergeLen < extLen
      then let 
        fullMerge = 2 * mergeLen
        mergeEnd = start + length - fullMerge
        bufferOffset = mergeLen
        in do 
        mergeIndex <- loopM (\mergeIndex -> if mergeIndex <= mergeEnd
          then do 
            grailMergeOutOfPlace vector mergeIndex mergeLen mergeLen bufferOffset
            pure $ Left (mergeIndex + fullMerge)
          else pure $ Right mergeIndex) start
        
        let 
          leftOver = length - (mergeIndex - start)
        
        if leftOver > mergeLen 
        then grailMergeOutOfPlace vector mergeIndex mergeLen (leftOver - mergeLen) bufferOffset
        else vectorCopy vector mergeIndex vector (mergeIndex - mergeLen) leftOver

        pure $ Left (start - mergeLen, fullMerge)
      else do 
        vectorCopy extBuffer 0 vector (start + length) extLen 
        grailBuildInPlace vector start length mergeLen bufferLen
        pure $ Right ()
      ) (start - 2, 2)

grailBuildBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Maybe (VM.MVector (PrimState m) a) -> Int -> m ()
grailBuildBlocks vector start length bufferLen extBuffer extBufferLen
  | isJust extBuffer =
    grailBuildOutOfPlace vector start length bufferLen extLen (fromJust extBuffer) extBufferLen
  | otherwise = do
    grailPairwiseSwaps vector start length
    grailBuildInPlace vector (start - 2) length 2 bufferLen
  where
    extLen
      | bufferLen < extBufferLen = bufferLen
      | otherwise = until (\x -> x*2 > extBufferLen) (*2) 1



grailBlockSelectSort :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> m Int
grailBlockSelectSort  vector firstKey start medianKey blockCount blockLen =
    loopM (\(firstBlock, medianKey)  -> if firstBlock < blockCount
        then do
          selectBlock <- loopM (\(selectBlock, currBlock) -> if currBlock < blockCount
            then do
              cmp <- compareV vector (start + (currBlock * blockLen)) (start + (selectBlock * blockLen)) compare
              lt <- compareV vector (firstKey + currBlock) (firstKey + selectBlock) (<)
              if (cmp == LT) || (cmp == EQ && lt)
              then pure $ Left (currBlock, currBlock + 1)
              else pure $ Left (selectBlock, currBlock + 1)
            else pure $ Right selectBlock
            ) (firstBlock, firstBlock + 1)
          if selectBlock /= firstBlock
          then do
            grailBlockSwap vector (start + (firstBlock * blockLen)) (start + (selectBlock * blockLen)) blockLen
            VM.swap vector (firstKey + firstBlock) (firstKey + selectBlock)
            if | medianKey == firstBlock  -> pure $ Left (firstBlock + 1, selectBlock)
               | medianKey == selectBlock -> pure $ Left (firstBlock + 1, firstBlock)
               | otherwise                -> pure $ Left (firstBlock + 1, medianKey)
          else pure $ Left (firstBlock + 1, medianKey)
        else pure $ Right medianKey
      ) (0, medianKey)



grailInPlaceBufferReset :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailInPlaceBufferReset vector start length bufferOffset =
  let
      buffer = start + length - 1
      index = buffer - bufferOffset
  in
  loopM (\(buffer, index) ->
      if buffer >= start
      then do
        VM.swap vector index buffer
        pure $ Left (buffer - 1, index - 1)
      else pure $ Right ()
  ) (buffer, index)


grailOutOfPlaceBufferReset :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailOutOfPlaceBufferReset vector start length bufferOffset = let 
    buffer = start + length - 1
    index  = buffer - bufferOffset 
  in
    loopM (\(buffer,index) -> if buffer >= start 
      then do 
        v <- VM.read vector index 
        VM.write vector buffer v 
        pure $ Left (buffer - 1, index - 1)
      else pure $ Right ()
      ) (buffer, index)


grailInPlaceBufferRewind :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailInPlaceBufferRewind vector start leftBlock buffer =
  loopM (\(leftBlock, buffer) -> if leftBlock >= start
    then do
      VM.swap vector buffer leftBlock
      pure $ Left (leftBlock - 1, buffer - 1)
    else pure $ Right ()
  ) (leftBlock, buffer)


grailOutOfPlaceBufferRewind :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailOutOfPlaceBufferRewind vector start leftBlock buffer = 
  loopM (\(leftBlock,buffer) -> if leftBlock >= start 
    then do 
      v <- VM.read vector leftBlock 
      VM.write vector buffer v 
      pure $ Left (leftBlock - 1, buffer - 1)
    else pure $ Right ()
  ) (leftBlock, buffer)


grailGetSubvector :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m Subvector
grailGetSubvector vector currentKey medianKey =
  ifM (compareV vector currentKey medianKey (<))
      (pure LEFT)
      (pure RIGHT)


grailCountLastMergeBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m Int
grailCountLastMergeBlocks vector offset blockCount blockLen =
  let
      blocksToMerge = 0
      lastRightFrag = offset + (blockCount * blockLen)
      prevLeftBlock = lastRightFrag - blockLen
  in
    loopM (\(blocksToMerge, prevLeftBlock) ->
      ifM
        (pure (blocksToMerge < blockCount) &&^ compareV vector lastRightFrag prevLeftBlock (<))
        (pure $ Left (blocksToMerge + 1, prevLeftBlock - blockLen))
        (pure $ Right blocksToMerge)
      ) (blocksToMerge, prevLeftBlock)


grailSmartMerge :: (Ord a, PrimMonad m) => VM.MVector (PrimState m) a -> Int -> Int -> Subvector -> Int -> Int -> Int -> Subvector -> m (Int, Subvector)
grailSmartMerge vector start leftLen leftOrigin rightLen bufferOffset currBlockLen currBlockOrigin =
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
      then ifM (compareV vector left right op) (do
        VM.swap vector buffer left
        pure $ Left (left + 1, right, buffer + 1)) (do
        VM.swap vector buffer right
        pure $ Left (left, right + 1, buffer + 1))
      else if left < middle
        then do
          grailInPlaceBufferRewind vector left (middle - 1) (end - 1)
          pure $ Right (middle - left, currBlockOrigin)
        else if leftOrigin == LEFT
          then pure $ Right (end - right, RIGHT)
          else pure $ Right (end - right, LEFT)
      ) (left, right, buffer)


grailSmartLazyMerge :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Subvector -> Int -> Int -> Subvector -> m (Int, Subvector)
grailSmartLazyMerge vector start leftLen leftOrigin rightLen currBlockLen currBlockOrigin =
  let
    middle = start + leftLen
  in do
    cmp <- compareV vector (middle - 1) middle compare
    if | leftOrigin == LEFT && cmp == GT -> loopM (\(start, leftLen, middle, rightLen) ->
          if leftLen /= 0
          then do
            aStart <- VM.read vector start
            mergeLen <- grailBinarySearchLeft vector middle rightLen aStart
            when (mergeLen /= 0) (grailRotate vector start leftLen mergeLen)
            let
              (start', rightLen', middle')
                | mergeLen /= 0 = (start + mergeLen, rightLen - mergeLen, middle + mergeLen)
                | otherwise     = (start, rightLen, middle)
            if rightLen' == 0
            then pure $ Right (leftLen, currBlockOrigin)
            else do
              (start'', leftLen') <- loopM ( \(start,leftLen) ->
                  ifM (pure (leftLen /= 0) &&^ compareV vector start middle' (<=))
                  (pure $ Left (start + 1, leftLen - 1)) (pure $ Right (start, leftLen)))  (start' + 1, leftLen - 1)
              pure $ Left (start'', leftLen', middle', rightLen')
          else pure $ Right $ grailSmartLazyMerge' leftOrigin rightLen
        ) (start, leftLen, middle, rightLen)
       | leftOrigin /= LEFT && cmp /= LT -> loopM (\(start, leftLen, middle, rightLen) ->
          if leftLen /= 0
          then do
            aStart <- VM.read vector start
            mergeLen <- grailBinarySearchRight vector middle rightLen aStart
            when (mergeLen /= 0) (grailRotate vector start leftLen mergeLen)
            let
              (start', rightLen', middle')
                | mergeLen /= 0 = (start + mergeLen, rightLen - mergeLen, middle + mergeLen)
                | otherwise     = (start, rightLen, middle)
            if rightLen' == 0
            then pure $ Right (leftLen, currBlockOrigin)
            else do
              (start'', leftLen') <- loopM ( \(start,leftLen) ->
                ifM (pure (leftLen /= 0) &&^ compareV vector start middle' (<))
                (pure $ Left (start + 1, leftLen - 1)) (pure $ Right (start, leftLen)))  (start' + 1, leftLen - 1)
              pure $ Left (start'', leftLen', middle', rightLen')
          else pure $ Right $ grailSmartLazyMerge' leftOrigin rightLen
        ) (start, leftLen, middle, rightLen)
       | otherwise -> pure $ grailSmartLazyMerge' leftOrigin rightLen

grailSmartLazyMerge' :: Subvector -> Int -> (Int, Subvector)
grailSmartLazyMerge' leftOrigin rightLen = (currBlockLen, currBlockOrigin)
    where
        currBlockLen = rightLen
        currBlockOrigin
            | leftOrigin == LEFT = RIGHT
            | otherwise          = LEFT

grailSmartMergeOutOfPlace :: (Ord a, PrimMonad m) => VM.MVector (PrimState m) a -> Int -> Int -> Subvector -> Int -> Int -> m (Int, Subvector)
grailSmartMergeOutOfPlace vector start leftLen leftOrigin rightLen bufferOffset = 
  let 
    left   = start
    middle = start + leftLen
    right  = middle
    end    = middle + rightLen
    buffer = start - bufferOffset
    op 
      | leftOrigin == LEFT = (<=)
      | otherwise          = (<)

  in loopM (\(left, right, buffer) -> if left < middle && right < end 
      then ifM (compareV vector left right op) 
        (do 
          v <- VM.read vector left
          VM.write vector buffer v
          pure $ Left (left + 1, right, buffer + 1))
        (do 
          v <- VM.read vector right
          VM.write vector buffer v
          pure $ Left (left, right + 1, buffer + 1))
      else let 
        currBlockLen 
          | left < middle = middle - left 
          | otherwise     = end - right 
        in if left < middle 
          then do 
            grailOutOfPlaceBufferRewind vector left (middle - 1) (end - 1)
            pure $ Right (currBlockLen, leftOrigin)
          else if leftOrigin == LEFT 
            then pure $ Right (currBlockLen, RIGHT)
            else pure $ Right (currBlockLen, LEFT)
      ) (left, right, buffer)


grailMergeBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> m ()
grailMergeBlocks vector firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen =
  let
    nextBlock = start + blockLen
    currBlockLen = blockLen
  in do
    currBlockOrigin <- grailGetSubvector vector firstKey medianKey
    loopM (\(nextBlock, currBlockLen, currBlockOrigin, keyIndex) ->
        if keyIndex < blockCount
        then let
          currBlock = nextBlock - currBlockLen
          buffer = currBlock - blockLen
        in do
          nextBlockOrigin <- grailGetSubvector vector (firstKey + keyIndex) medianKey
          when (nextBlockOrigin == currBlockOrigin) (grailBlockSwap vector buffer currBlock currBlockLen)
          (currBlockLen', currBlockOrigin') <- if nextBlockOrigin == currBlockOrigin
            then pure (blockLen, currBlockOrigin)
            else grailSmartMerge vector currBlock currBlockLen currBlockOrigin blockLen blockLen currBlockLen currBlockOrigin
          pure $ Left (nextBlock + blockLen, currBlockLen', currBlockOrigin', keyIndex + 1)
        else let
            currBlock = nextBlock - currBlockLen
            buffer = currBlock - blockLen
            (currBlock', currBlockLen')
                | currBlockOrigin == RIGHT = (nextBlock, blockLen * lastMergeBlocks)
                | otherwise = (currBlock, currBlockLen + blockLen * lastMergeBlocks)
          in do
            if lastLen /= 0
            then do
              when (currBlockOrigin == RIGHT) (grailBlockSwap vector buffer currBlock currBlockLen)
              grailMergeForwards vector currBlock' currBlockLen' lastLen blockLen
            else grailBlockSwap vector buffer currBlock currBlockLen
            pure $ Right ()
      ) (nextBlock, currBlockLen, currBlockOrigin, 1)


grailLazyMergeBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> m ()
grailLazyMergeBlocks vector firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen =
  let
    nextBlock = start + blockLen
    currBlockLen = blockLen
  in do
    currBlockOrigin <- grailGetSubvector vector firstKey medianKey
    loopM (\(nextBlock, currBlockLen, currBlockOrigin, keyIndex) ->
      if keyIndex < blockCount
      then let
          currBlock = nextBlock - currBlockLen
        in do
          nextBlockOrigin <- grailGetSubvector vector (firstKey + keyIndex) medianKey

          (currBlockLen', currBlockOrigin') <- if nextBlockOrigin == currBlockOrigin
            then pure (blockLen, currBlockOrigin)
            else grailSmartLazyMerge vector currBlock currBlockLen currBlockOrigin blockLen currBlockLen currBlockOrigin

          pure $ Left (nextBlock + blockLen, currBlockLen', currBlockOrigin', keyIndex + 1)

      else let
          currBlock = nextBlock - currBlockLen
          (currBlock', currBlockLen')
                  | currBlockOrigin == RIGHT = (nextBlock, blockLen * lastMergeBlocks)
                  | otherwise = (currBlock, currBlockLen + blockLen * lastMergeBlocks)
        in do
          when (lastLen /= 0) (grailLazyMerge vector currBlock' currBlockLen' lastLen)
          pure $ Right ()
      ) (nextBlock,currBlockLen,currBlockOrigin,1)


grailMergeBlocksOutOfPlace :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> m ()
grailMergeBlocksOutOfPlace vector firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen = let 
    nextBlock = start + blockLen
    currBlockLen = blockLen
  in do 
    currBlockOrigin <- grailGetSubvector vector firstKey medianKey

    loopM (\(nextBlock, currBlockLen, currBlockOrigin, keyIndex) ->
      if keyIndex < blockCount
      then let
          currBlock = nextBlock - currBlockLen
          in do 
            nextBlockOrigin <- grailGetSubvector vector (firstKey + keyIndex) medianKey
            when (nextBlockOrigin == currBlockOrigin) (let buffer = currBlock - blockLen 
              in vectorCopy vector currBlock vector buffer currBlockLen)
            (currBlockLen',currBlockOrigin') <- if nextBlockOrigin == currBlockOrigin
              then pure (blockLen, currBlockOrigin)
              else grailSmartMergeOutOfPlace vector currBlock currBlockLen currBlockOrigin blockLen blockLen 
            
            pure $ Left (nextBlock + blockLen, currBlockLen', currBlockOrigin', keyIndex + 1)
      else let 
        currBlock = nextBlock - currBlockLen
        buffer    = currBlock - blockLen
        (currBlock', currBlockLen')
          | currBlockOrigin == RIGHT = (nextBlock, blockLen * lastMergeBlocks)
          | otherwise = (currBlock, currBlockLen + blockLen * lastMergeBlocks)
        in if lastLen /= 0
          then do 
              when (currBlockOrigin == RIGHT) (vectorCopy vector currBlock vector buffer currBlockLen)
              grailMergeOutOfPlace vector currBlock' currBlockLen' lastLen blockLen
              pure $ Right ()
          else do 
            vectorCopy vector currBlock vector buffer currBlockLen
            pure $ Right ()
      ) (nextBlock, currBlockLen,currBlockOrigin, 1)


grailCombineInPlace :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool -> m ()
grailCombineInPlace vector firstKey start length subvectorLen blockLen mergeCount lastSubvectors buffer =
  let
    fullMerge = 2 * subvectorLen
    blockCount = fullMerge `quot` blockLen
  in loopM (\mergeIndex ->
      if mergeIndex < mergeCount
      then let
        offset = start + (mergeIndex * fullMerge)
        in do
          grailInsertSort vector firstKey blockCount

          medianKey <- grailBlockSelectSort vector firstKey offset (subvectorLen `quot` blockLen) blockCount blockLen

          if buffer
          then grailMergeBlocks vector firstKey (firstKey + medianKey) offset blockCount blockLen 0 0
          else grailLazyMergeBlocks vector firstKey (firstKey + medianKey) offset blockCount blockLen 0 0

          pure $ Left (mergeIndex + 1)
      else let
          offset = start + (mergeCount * fullMerge)
          blockCount = lastSubvectors `quot` blockLen
          lastFragment = lastSubvectors - (blockCount * blockLen)
        in do
          when (lastSubvectors /= 0) do
            grailInsertSort vector firstKey (blockCount +1)
            medianKey <- grailBlockSelectSort vector firstKey offset (subvectorLen `quot` blockLen) blockCount blockLen

            lastMergeBlocks <- if  lastFragment /= 0
              then grailCountLastMergeBlocks vector offset blockCount blockLen
              else pure 0

            let
              smartMerges = blockCount - lastMergeBlocks
              leftLen = lastMergeBlocks * blockLen
            if | smartMerges == 0 && buffer ->
                 grailMergeForwards vector offset leftLen lastFragment blockLen
               | smartMerges == 0 ->
                 grailLazyMerge vector offset leftLen lastFragment
               | buffer    ->
                 grailMergeBlocks vector firstKey (firstKey + medianKey) offset smartMerges blockLen lastMergeBlocks lastFragment
               | otherwise ->
                 grailLazyMergeBlocks vector firstKey (firstKey + medianKey) offset smartMerges blockLen lastMergeBlocks lastFragment

          when buffer (grailInPlaceBufferReset vector start length blockLen)
          pure $ Right ()
      ) 0



grailCombineOutOfPlace :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> VM.MVector (PrimState m) a -> Int -> m ()
grailCombineOutOfPlace vector firstKey start length subvectorLen blockLen mergeCount lastSubvectors extBuffer extBufferLen =
  let
    fullMerge = 2 * subvectorLen
    blockCount = fullMerge `quot` blockLen
  in do
    vectorCopy vector (start - blockLen) extBuffer 0 blockLen

    loopM (\mergeIndex -> if mergeIndex < mergeCount
      then let
          offset = start + mergeIndex * fullMerge
        in do
          grailInsertSort vector firstKey blockCount
          medianKey <- grailBlockSelectSort vector firstKey offset (subvectorLen `quot` blockLen) blockCount blockLen
          grailMergeBlocksOutOfPlace vector firstKey (firstKey + medianKey) offset blockCount blockLen 0 0
          pure $ Left (mergeIndex + 1)
      else pure $ Right ()) 0

    when (lastSubvectors /= 0) let
      offset = start + mergeCount * fullMerge
      blockCount = lastSubvectors `quot` blockLen
      lastFragment = lastSubvectors - (blockCount * blockLen)
      in do
        grailInsertSort vector firstKey (blockCount + 1)
        medianKey <- grailBlockSelectSort vector firstKey offset (subvectorLen `quot` blockLen) blockCount blockLen

        lastMergeBlocks <- if lastFragment /= 0
            then grailCountLastMergeBlocks vector offset blockCount blockLen
            else pure 0

        let
          smartMerges = blockCount - lastMergeBlocks
          leftLen = lastMergeBlocks * blockLen

        if smartMerges == 0
        then grailMergeOutOfPlace vector offset leftLen lastFragment blockLen
        else grailMergeBlocksOutOfPlace vector firstKey (firstKey + medianKey) offset smartMerges blockLen lastMergeBlocks lastFragment

    grailOutOfPlaceBufferReset vector start length blockLen
    vectorCopy extBuffer 0 vector (start - blockLen) blockLen

grailCombineBlocks :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> Int -> Int -> Bool -> Maybe (VM.MVector (PrimState m) a) -> Int -> m ()
grailCombineBlocks vector firstKey start length subvectorLen blockLen buffer extBuffer extBufferLen
  | buffer && blockLen <= extBufferLen = grailCombineOutOfPlace vector firstKey start length' subvectorLen blockLen mergeCount lastSubvectors' (fromJust extBuffer) extBufferLen
  | otherwise = grailCombineInPlace vector firstKey start length' subvectorLen blockLen mergeCount lastSubvectors' buffer
  where
    fullMerge = 2 * subvectorLen
    mergeCount = length `quot` fullMerge
    lastSubvectors = length - (fullMerge * mergeCount)
    (length', lastSubvectors')
        | lastSubvectors <= subvectorLen = (length - lastSubvectors, 0)
        | otherwise = (length, lastSubvectors)

grailLazyMerge :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Int -> m ()
grailLazyMerge vector start leftLen rightLen
  | leftLen < rightLen = let
    middle = start + leftLen
    in
      loopM (\(start, leftLen, rightLen, middle) -> if leftLen /= 0
        then do
          aStart <- VM.read vector start
          mergeLen <- grailBinarySearchLeft vector middle rightLen aStart

          when (mergeLen /= 0) (grailRotate vector start leftLen mergeLen)
          let (start', rightLen', middle')
                | mergeLen /= 0 = (start + mergeLen, rightLen - mergeLen, middle + mergeLen)
                | otherwise     = (start, rightLen, middle)

          if rightLen' == 0
          then pure $ Right ()
          else do
            aMid <- VM.read vector middle'
            (start'', leftLen') <- loopM (\(st,ll) -> do
                aStart <- VM.read vector st
                if ll /= 0 && aStart <= aMid
                then pure $ Left (st + 1, ll - 1)
                else pure $ Right (st,ll)
                ) (start' +1, leftLen -1)
            pure $ Left (start'', leftLen', rightLen', middle')
        else pure $ Right ()
      ) (start, leftLen, rightLen, middle)
  | otherwise = let
    end = start + leftLen + rightLen - 1
    in
      loopM (\(leftLen, rightLen, end) ->
        if rightLen == 0
        then pure $ Right ()
        else do
          aEnd <- VM.read vector end
          mergeLen <- grailBinarySearchRight vector start leftLen aEnd
          when (mergeLen /= leftLen) (grailRotate vector (start + mergeLen) (leftLen - mergeLen) rightLen)
          let (end', leftLen')
                | mergeLen /= leftLen = (end - (leftLen - mergeLen), mergeLen )
                | otherwise           = (end, leftLen)

          if leftLen' == 0
          then pure $ Right ()
          else let middle = start + leftLen' in do
            aMidM <- VM.read vector (middle - 1)
            (end'', rightLen') <- loopM (\(en,rl) -> do
              aEnd <- VM.read vector en
              if rl/= 0 && aMidM <= aEnd
              then pure $ Left (en -1, rl - 1)
              else pure $ Right (en,rl)
              ) (end' - 1, rightLen - 1)
            pure $ Left (leftLen', rightLen', end'')
      ) (leftLen, rightLen, end)


grailLazyStableSort :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> m ()
grailLazyStableSort vector start length = do
  loopM (\index -> if index < length
    then do
      VM.swap vector (start + index - 1) (start + index)
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
              grailLazyMerge vector (start + mergeIndex) mergeLen mergeLen
              pure $ Left (mergeIndex + fullMerge)
            else pure $ Right mergeIndex
          ) 0
        let
          leftOver = length - mergeIndex
        when (leftOver > mergeLen) (grailLazyMerge vector (start + mergeIndex) mergeLen (leftOver - mergeLen))
        pure $ Left (mergeLen * 2)
      else pure $ Right ()
    ) 2


grailCommonSort :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Maybe (VM.MVector (PrimState m) a) -> Int -> m ()
grailCommonSort vector start length extBuffer extBufferLen
  | length < 16 = grailInsertSort vector start length
  | otherwise = let
    blockLen = until (\x -> x*x >= length) (*2) 1
    keyLen = ((length - 1) `quot` blockLen) + 1
    idealKeys = keyLen + blockLen
    blockLen' = 0
    in do
      keysFound <- grailCollectKeys vector start length idealKeys
      if keysFound < idealKeys
      then if keysFound < 4
        then grailLazyStableSort vector start length
        else let keyLen' = until (<= keysFound) (`quot` 2) blockLen
          in grailCommonSort' vector start length extBuffer extBufferLen blockLen' keyLen' False
      else   grailCommonSort' vector start length extBuffer extBufferLen blockLen  keyLen  True

grailCommonSort' :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Maybe (VM.MVector (PrimState m) a) -> Int -> Int -> Int -> Bool -> m ()
grailCommonSort' vector start length extBuffer extBufferLen blockLen keyLen idealBuffer =
  let
    bufferEnd = blockLen + keyLen
    subvectorLen
      | idealBuffer = blockLen
      | otherwise   = keyLen
    (extBuffer', extBufferLen')
      | idealBuffer && isJust extBuffer = (extBuffer, extBufferLen)
      | otherwise = (Nothing, 0)
  in do
    grailBuildBlocks vector (start + bufferEnd) (length - bufferEnd) subvectorLen extBuffer' extBufferLen'
    grailCommonSort'' vector start length extBuffer' extBufferLen' blockLen keyLen idealBuffer bufferEnd subvectorLen


grailCommonSort'' :: (PrimMonad m, Ord a) => VM.MVector (PrimState m) a -> Int -> Int -> Maybe (VM.MVector (PrimState m) a) -> Int -> Int -> Int -> Bool -> Int -> Int -> m ()
grailCommonSort'' vector start length extBuffer extBufferLen blockLen keyLen idealBuffer bufferEnd  =
  loopM (\subvectorLen -> if length - bufferEnd > 2 * subvectorLen
      then let
            subvectorLen' = subvectorLen * 2
            currentBlockLen = blockLen
            scrollingBuffer = idealBuffer
            keyBuffer = keyLen `quot` 2
            (currentBlockLen', scrollingBuffer')
                | idealBuffer                                        = (currentBlockLen, scrollingBuffer)
                | keyBuffer >= ((2 * subvectorLen') `quot` keyBuffer) = (keyBuffer, True)
                | otherwise                                          = ((2 * subvectorLen') `quot` keyLen, scrollingBuffer)
        in do
          grailCombineBlocks vector start (start + bufferEnd) (length - bufferEnd) subvectorLen' currentBlockLen' scrollingBuffer' extBuffer extBufferLen
          pure $ Left subvectorLen'
      else do
        grailInsertSort vector start bufferEnd
        grailLazyMerge vector start bufferEnd (length - bufferEnd)
        pure $ Right ())


-- area of functions one would consider public in other languages
-- intermediary function existed to pass the input forward, was later integrated 
-- directly into public functions

-- no external buffer used
grailSortInPlace :: Ord a => [a] -> Int -> Int -> [a]
grailSortInPlace list start len
  | null list = []
  | otherwise = runST $ do
    vector <- V.thaw . V.fromList $ list
    grailCommonSort vector start len Nothing 0
    V.toList <$> V.freeze vector

-- fixed-length external buffer used
grailSortStaticOOP :: Ord a => [a] -> Int -> Int -> [a]
grailSortStaticOOP list start len
  | null list = []
  | otherwise = runST $ do
    vector <- V.thaw . V.fromList $ list
    extBuffer <- V.thaw . V.replicate 512 $ head list
    grailCommonSort vector start len (Just extBuffer) 512
    V.toList <$> V.freeze vector

-- external buffer with length of smallest power of 2 larger than square root of input's length used 
grailSortDynamicOOP :: Ord a => [a] -> Int -> Int -> [a]
grailSortDynamicOOP list start len
  | null list = []
  | otherwise = runST $ do
    vector <- V.thaw . V.fromList $ list
    extBuffer <- V.thaw . V.replicate bufferLen $ head list
    VM.clear extBuffer
    grailCommonSort vector start len (Just extBuffer) bufferLen
    V.toList <$> V.freeze vector
    where
      bufferLen = until (\x -> x^2 >= len) (*2) 1

