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
Mihnik's Haskell "naive" implementation of Grailsort (purely functional, no in-place guarantee)

Rewritten without any magic of complicated modules, based on thatsOven's Python version
NOTE: despite them using name 'array', they are lists

------ IMPORTANT NOTE: Due to the purely functional approach, every operation on the list creates an accordingly modified copy.
------ This can take a lot of memory should the input list be large. Keep that in mind.

Current status: Done
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GrailSort.Naive  (grailSortInPlace, grailSortStaticOOP, grailSortDynamicOOP) where
import Data.Maybe (isJust, fromJust)

--support function
arrayCopy :: [a] -> Int -> [a] -> Int -> Int -> [a]
arrayCopy fromArray fromIndex toArray toIndex length =
    let
        slice = drop fromIndex . take (fromIndex + length) $ fromArray
    in
        take toIndex toArray ++ slice ++ drop (toIndex + length) toArray

--support data structure
data Subarray = LEFT | RIGHT deriving (Eq, Show)



arrayWrite :: Ord a => [a] -> Int -> Int -> [a]
arrayWrite array' target source = take target array' ++ [array' !! source] ++ drop (target + 1) array'


grailSwap :: [a] -> Int -> Int -> [a]
--grailSwap array a b = (take a array) ++ [array !! b] ++ (take (b - a - 1) (drop (a + 1) array)) ++ [array !! a] ++ (drop (b + 1) array)
grailSwap array x y =
      let
        a = min x y
        b = max x y
        a' = array !! a
        b' = array !! b
      in
        take a array ++ [b'] ++ drop (a + 1) (take b array) ++ [a'] ++ drop (b + 1) array



grailBlockSwap :: [a] -> Int -> Int -> Int -> [a]
grailBlockSwap array x y blockLen =
    let
      a = min x y
      b = max x y
      --diff = (a + blockLen - b)
      --beginning = take a array
      --aBlock = drop a (take (a + blockLen) array)
      --aBlock1 = drop a (take (a+diff) array)
      --aBlock2 = drop (a + diff) (take (a + blockLen - diff) array)
      --bBlock = drop b (take (b + blockLen) array)
      --between = drop (a + blockLen) (take b array)
      --finish = drop (b + blockLen) array
    in
        --if diff <= 0
        --then beginning ++ bBlock ++ between ++ aBlock ++ finish
        --else beginning ++ bBlock  ++ aBlock2 ++ aBlock1 ++ finish
        --beginning ++ bBlock ++ between ++ aBlock ++ finish
       --take a array ++ drop b (take (b + blockLen) array) ++ drop (a + blockLen) (take b array) ++ drop a (take (a + blockLen) array) ++ drop (b + blockLen) array
       grailBlockSwap' array a b blockLen


grailBlockSwap' :: [a] -> Int -> Int -> Int -> [a]
grailBlockSwap' array a b blockLen
    | blockLen == 0 = array
    | otherwise = grailBlockSwap' (grailSwap array a b) (a+1) (b+1) (blockLen -1)



grailRotate :: [a] -> Int -> Int -> Int -> [a]
grailRotate array start leftLen rightLen =
    let 
        beginning = take start array
        leftBlock = drop start (take (start + leftLen) array)
        rightBlock = drop (start+ leftLen) (take (start + rightLen + leftLen) array)
        finish = drop (start + leftLen + rightLen) array
    in
        beginning ++ rightBlock ++ leftBlock ++ finish
    --take start array ++ drop (start+ leftLen) (take (start + rightLen + leftLen) array) ++ drop start (take (start + leftLen) array) ++ drop (start + leftLen + rightLen) array
-- | leftLen <= 0 || rightLen <= 0 = array
-- | leftLen <= rightLen = grailRotate (grailBlockSwap array start (start + leftLen) leftLen) (start + leftLen) leftLen (rightLen - leftLen)
-- | otherwise = grailRotate (grailBlockSwap array (start + leftLen - rightLen) (start + leftLen) rightLen) start (leftLen - rightLen) rightLen



grailInsertSort :: (Ord a) => [a] -> Int -> Int -> [a]
grailInsertSort array start length = grailInsertSort' array start length 1

grailInsertSort' :: Ord a => [a] -> Int -> Int -> Int -> [a]
grailInsertSort' array start length item
    | item == length = array
    | otherwise = grailInsertSort' (grailInsert array start item (start + item - 1) (start + item)) start length (item + 1)

grailInsert :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> [a]
grailInsert array start item left right
    | left < start = array
    | array !! left > array !! right = grailInsert (grailSwap array left right) start item (left - 1) (right - 1)
    | otherwise = array



grailBinarySearchLeft :: (Ord a) => [a] -> Int -> Int -> a -> Int
grailBinarySearchLeft array start length target =
    let
    left  = 0
    right = length
    in
    grailBinarySearchLeft' array start left right target

grailBinarySearchLeft' :: (Ord a) => [a] -> Int -> Int -> Int -> a -> Int
grailBinarySearchLeft' array start left right target
    | left < right =
    let
        middle = left + ((right - left) `quot` 2)
    in
        if array !! (start + middle) < target
        then grailBinarySearchLeft' array start (middle + 1) right target
        else grailBinarySearchLeft' array start left middle target
    | otherwise = left



grailBinarySearchRight :: (Ord a) => [a] -> Int -> Int -> a -> Int
grailBinarySearchRight array start length target =
    let
    left  = 0
    right = length
    in
    grailBinarySearchRight' array start left right target

grailBinarySearchRight' :: (Ord a) => [a] -> Int -> Int -> Int -> a -> Int
grailBinarySearchRight' array start left right target
    | left < right =
    let
        middle = left + ((right - left) `quot` 2)
    in
        if array !! (start + middle) > target
        then grailBinarySearchRight' array start left middle target
        else grailBinarySearchRight' array start (middle + 1) right target
    | otherwise = right



grailCollectKeys :: (Ord a) => [a] -> Int -> Int -> Int -> (Int, [a])
grailCollectKeys array start length idealKeys = grailCollectKeys' array start length idealKeys 1 0 1

grailCollectKeys' :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, [a])
grailCollectKeys' array start length idealKeys keysFound firstKey currKey
    | currKey < length && keysFound < idealKeys =
    let insertPos = grailBinarySearchLeft array (start + firstKey) keysFound (array !! (start + currKey))
        newFirstKey = currKey - keysFound
        array1 = grailRotate array (start + firstKey) keysFound (currKey - (firstKey + keysFound))
        array2 = grailRotate array1 (start + newFirstKey + insertPos) (keysFound - insertPos) 1
    in if insertPos == keysFound || (array !! (start + currKey)) /= (array !! (start + firstKey + insertPos))
        then grailCollectKeys' array2 start length idealKeys (keysFound + 1) newFirstKey (currKey + 1)
        else grailCollectKeys' array start length idealKeys keysFound firstKey (currKey + 1)
    | otherwise = (keysFound, grailRotate array start firstKey keysFound)




grailPairwiseSwaps :: Ord a => [a] -> Int -> Int -> [a]
grailPairwiseSwaps array start length = grailPairwiseSwaps' array start length 1

grailPairwiseSwaps' :: Ord a => [a] -> Int -> Int -> Int -> [a]
grailPairwiseSwaps' array start length index
    | index < length = if (array !! left) > (array !! right)
        then let
            array1 = grailSwap array (left - 2) right
            array2 = grailSwap array1 (right - 2) left
            in grailPairwiseSwaps' array2 start length (index + 2)
        else let
            array1 = grailSwap array (left - 2) left
            array2 = grailSwap array1 (right - 2) right
            in grailPairwiseSwaps' array2  start length (index + 2)
    | otherwise = if left < start + length
        then grailSwap array (left - 2) left
        else array
    where
        left = start + index - 1
        right = start + index



grailPairwiseWrites :: Ord a => [a] -> Int -> Int -> [a]
grailPairwiseWrites array start length = grailPairwiseWrites' array start length 1

grailPairwiseWrites' :: Ord a => [a] -> Int -> Int -> Int -> [a]
grailPairwiseWrites' array start length index
    | index < length = if (array !! left) > (array !! right)
        then let
            array1 = arrayWrite array (left - 2) right
            array2 = arrayWrite array1 (right - 2) left
            in grailPairwiseWrites' array2 start length (index + 2)
        else let
            array1 = arrayWrite array (left - 2) left
            array2 = arrayWrite array1 (right - 2) right
            in grailPairwiseWrites' array2  start length (index + 2)
    | otherwise = if left < start + length
        then arrayWrite array (left - 2) left
        else array
    where
        left = start + index - 1
        right = start + index



grailMergeForwards :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> [a]
grailMergeForwards array start leftLen rightLen bufferOffset =
    let left   = start
        middle = start + leftLen
        right  = middle
        end    = middle + rightLen
        buffer = start - bufferOffset
    in grailMergeForwards' array left middle right end buffer

grailMergeForwards' :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> Int -> [a]
grailMergeForwards' array left middle right end buffer
    | right < end =
        if left == middle || array !! left > array !! right
        then grailMergeForwards' (grailSwap array buffer right) left middle (right+1) end (buffer+1)
        else grailMergeForwards' (grailSwap array buffer left) (left+1) middle right end (buffer+1)
    | buffer /= left = grailBlockSwap array buffer left (middle-left)
    | otherwise = array



grailMergeBackwards :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> [a]
grailMergeBackwards array start leftLen rightLen bufferOffset =
    let end    = start - 1
        left   = end + leftLen
        middle = left
        right  = middle + rightLen
        buffer = right + bufferOffset
    in grailMergeBackwards' array start leftLen rightLen bufferOffset end left middle right buffer

grailMergeBackwards' :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [a]
grailMergeBackwards' array start leftLen rightLen bufferOffset end left middle right buffer
    | left > end =
        if right == middle || array !! left > array !! right
        then grailMergeBackwards' (grailSwap array buffer left) start leftLen rightLen bufferOffset end (left - 1) middle right (buffer - 1)
        else grailMergeBackwards' (grailSwap array buffer right) start leftLen rightLen bufferOffset end left middle (right - 1) (buffer - 1)
    | right /= buffer = grailMergeBackwards'' array middle right buffer
    | otherwise = array

grailMergeBackwards'' :: (Ord a) => [a] -> Int -> Int -> Int -> [a]
grailMergeBackwards'' array middle right buffer
    | right > middle = grailMergeBackwards'' (grailSwap array buffer right) middle (right - 1) (buffer - 1)
    | otherwise = array



grailMergeOutOfPlace :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> [a]
grailMergeOutOfPlace array start leftLen rightLen bufferOffset =
    let left   = start
        middle = start + leftLen
        right  = middle
        end    = middle + rightLen
        buffer = start - bufferOffset
    in grailMergeOutOfPlace' array left middle right end buffer

grailMergeOutOfPlace' :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> Int -> [a]
grailMergeOutOfPlace' array left middle right end buffer
    | right < end = let
            left' = array !! left
            right' = array !! right
            arrayL = arrayWrite array buffer left
            arrayR = arrayWrite array buffer right
        in if left == middle || left' > right'
            then grailMergeOutOfPlace' arrayR left middle (right + 1) end (buffer + 1)
            else grailMergeOutOfPlace' arrayL (left + 1) middle right end (buffer + 1)
    | otherwise = if buffer /= left
        then grailMergeOutOfPlace'' array left middle end buffer
        else array

grailMergeOutOfPlace'' :: Ord a => [a] -> Int -> Int -> Int -> Int -> [a]
grailMergeOutOfPlace'' array left middle end buffer
    | left < middle = grailMergeOutOfPlace'' (arrayWrite array buffer left) (left + 1) middle end (buffer + 1)
    | otherwise = array



grailBuildInPlace :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> [a]
grailBuildInPlace array start length currentLen bufferLen = --array
    let mergeLen = currentLen
    in grailBuildInPlace' array start length currentLen bufferLen mergeLen

grailBuildInPlace' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> [a]
grailBuildInPlace' array start length currentLen bufferLen mergeLen
    | mergeLen < bufferLen =
        let
            fullMerge = 2 * mergeLen
            mergeEnd = start + length - fullMerge
            bufferOffset = mergeLen
            (mergeIndex, array') = until (\(index,_) -> index > mergeEnd)
                (\(index, arr) -> (index + fullMerge, grailMergeForwards arr index mergeLen mergeLen bufferOffset)
                ) (start, array)
            leftOver = length - (mergeIndex - start)

            array'' = if leftOver > mergeLen
                then grailMergeForwards array' mergeIndex mergeLen (leftOver - mergeLen) bufferOffset
                else grailRotate array' (mergeIndex - mergeLen) mergeLen leftOver
        in
            grailBuildInPlace' array'' (start - mergeLen) length currentLen bufferLen fullMerge
    | otherwise =
        let
        fullMerge = 2 * bufferLen
        lastBlock = length `rem` fullMerge
        lastOffset = start + length - lastBlock
        array' 
            | lastBlock <= bufferLen = grailRotate array lastOffset lastBlock bufferLen
            | otherwise  = grailMergeBackwards array lastOffset bufferLen (lastBlock - bufferLen) bufferLen
        mergeIndex = lastOffset - fullMerge
        in
        snd $ until (\(index,_) -> index < start) (\(index, array'') ->
                (index - fullMerge, grailMergeBackwards array'' index bufferLen bufferLen bufferLen))
                (mergeIndex, array')




grailBuildOutOfPlace :: Ord a => [a] -> Int -> Int -> Int -> Int -> [a] -> Int -> ([a], Maybe [a])
grailBuildOutOfPlace array start length bufferLen extLen extBuffer extBufferLen = --([], [])
    let
        extBuffer' = arrayCopy array (start - extLen) extBuffer 0 extLen
        array' = grailPairwiseWrites array start length
        start' = start - 2
        mergeLen = 2
    in
        grailBuildOutOfPlace' array' start' length bufferLen extLen extBuffer' extBufferLen mergeLen

grailBuildOutOfPlace' :: Ord a => [a] -> Int -> Int -> Int -> Int -> [a] -> Int -> Int -> ([a], Maybe [a])
grailBuildOutOfPlace' array start length bufferLen extLen extBuffer extBufferLen mergeLen
    | mergeLen < extLen =
        let
            fullMerge = 2 * mergeLen
            mergeEnd = start + length - fullMerge
            bufferOffset = mergeLen
            (mergeIndex, array') = until (\(index, _) -> index > mergeEnd) (\(index, arr) ->
                (index + fullMerge, grailMergeOutOfPlace arr index mergeLen mergeLen bufferOffset))
                (start, array)
            leftOver = length - (mergeIndex - start)
            array'' = if leftOver > mergeLen
                then grailMergeOutOfPlace array' mergeIndex mergeLen (leftOver - mergeLen) bufferOffset
                else arrayCopy array' mergeIndex array' (mergeIndex - mergeLen) leftOver
        in
            grailBuildOutOfPlace' array'' (start - mergeLen) length bufferLen extLen extBuffer extBufferLen (mergeLen * 2)
    | otherwise =
        (grailBuildInPlace (arrayCopy extBuffer 0 array (start + length) extLen) start length mergeLen bufferLen, Just extBuffer)



grailBuildBlocks :: Ord a => [a] -> Int -> Int -> Int -> Maybe [a] -> Int -> ([a], Maybe [a])
grailBuildBlocks array start length bufferLen extBuffer extBufferLen -- = case extBuffer of
    | isJust extBuffer = 
            grailBuildOutOfPlace array start length bufferLen extLen (fromJust extBuffer) extBufferLen
    | otherwise = 
            (grailBuildInPlace array' (start - 2) length 2 bufferLen, Nothing)
    where 
        extLen
          | bufferLen < extBufferLen = bufferLen
          | otherwise = until (\x -> x*2 > extBufferLen) (*2) 1
        array' = grailPairwiseSwaps array start length




--thanks to Taihennami for finding where the sort goes wrong
grailBlockSelectSort :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> ([a], Int)
grailBlockSelectSort array firstKey start medianKey blockCount blockLen =
    let
        firstBlock = 0
    in
        grailBlockSelectSort'  array firstKey start medianKey blockCount blockLen firstBlock



grailBlockSelectSort' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> ([a], Int)
grailBlockSelectSort'  array firstKey start medianKey blockCount blockLen firstBlock
    | firstBlock < blockCount = let
        selectBlock = grailBlockSelectSort'' array firstKey start blockCount firstBlock firstBlock blockLen (firstBlock + 1)
        array'  = grailBlockSwap array (start + firstBlock * blockLen) (start + selectBlock * blockLen) blockLen
        array'' = grailSwap array' (firstKey + firstBlock) (firstKey + selectBlock)
        in
            if selectBlock /= firstBlock
            then if medianKey == firstBlock
                then grailBlockSelectSort' array'' firstKey start selectBlock blockCount blockLen (firstBlock + 1)
                else if medianKey == selectBlock
                    then grailBlockSelectSort' array'' firstKey start firstBlock blockCount blockLen (firstBlock + 1)
                    else grailBlockSelectSort' array'' firstKey start medianKey blockCount blockLen (firstBlock + 1)
            else grailBlockSelectSort'  array firstKey start medianKey blockCount blockLen (firstBlock + 1)
    | otherwise = (array, medianKey)

grailBlockSelectSort'' :: Ord a => [a] -> Int -> Int -> Int -> t -> Int -> Int -> Int -> Int
grailBlockSelectSort'' array firstKey start blockCount firstBlock selectBlock blockLen currBlock
    | currBlock < blockCount =
        let
            comp =  compare (array!!(start + currBlock * blockLen)) (array!!(start + selectBlock * blockLen))
            selectBlock' = if comp == LT || (comp == EQ && array!!(firstKey + currBlock) < array!!(firstKey + selectBlock))
                then currBlock
                else selectBlock
        in
            grailBlockSelectSort'' array firstKey start blockCount firstBlock selectBlock' blockLen (currBlock + 1)
    | otherwise = selectBlock




grailInPlaceBufferReset :: [a] -> Int -> Int -> Int -> [a]
grailInPlaceBufferReset array start length bufferOffset =
    let
        buffer = start + length - 1
        index = buffer - bufferOffset
    in
        grailInPlaceBufferReset' array start length bufferOffset buffer index

grailInPlaceBufferReset' :: [a] -> Int -> Int -> Int -> Int -> Int -> [a]
grailInPlaceBufferReset' array start length bufferOffset buffer index
    | buffer >= start =
            grailInPlaceBufferReset' (grailSwap array index buffer) start length bufferOffset (buffer-1) (index-1)
    | otherwise = array



grailOutOfPlaceBufferReset :: [a] -> Int -> Int -> Int -> [a]
grailOutOfPlaceBufferReset array start length bufferOffset =
    let
        buffer = start + length - 1
        index = buffer - bufferOffset
    in
        grailOutOfPlaceBufferReset' array start length bufferOffset buffer index

grailOutOfPlaceBufferReset' :: [a] -> Int -> Int -> Int -> Int -> Int -> [a]
grailOutOfPlaceBufferReset' array start length bufferOffset buffer index
    | buffer >= start =
        let
            a = array !! index
            array' = take buffer array ++ [a] ++ drop (buffer + 1) array
        in
            grailOutOfPlaceBufferReset' array' start length bufferOffset (buffer-1) (index-1)
    | otherwise = array



grailInPlaceBufferRewind :: [a] -> Int -> Int -> Int -> [a]
grailInPlaceBufferRewind array start leftBlock buffer
    | leftBlock >= start =
        let
            array' = grailSwap array buffer leftBlock
        in
            grailInPlaceBufferRewind array' start (leftBlock-1) (buffer - 1)
    | otherwise = array




grailOutOfPlaceBufferRewind :: [a] -> Int -> Int -> Int -> [a]
grailOutOfPlaceBufferRewind array start leftBlock buffer
    | leftBlock >= start =
        let
            a = array !! leftBlock
            array' = take buffer array ++ [a] ++ drop (buffer + 1) array
        in
            grailOutOfPlaceBufferRewind array' start (leftBlock-1) (buffer - 1)
    | otherwise = array



grailGetSubarray :: Ord a => [a] -> Int -> Int -> Subarray
grailGetSubarray array currentKey medianKey =
    let
        a = array !! currentKey
        b = array !! medianKey
    in
        if a < b
        then LEFT
        else RIGHT



grailCountLastMergeBlocks :: Ord a => [a] -> Int -> Int -> Int -> Int
grailCountLastMergeBlocks array offset blockCount blockLen =
    let
        blocksToMerge = 0
        lastRightFrag = offset + (blockCount * blockLen)
        prevLeftBlock = lastRightFrag - blockLen
    in grailCountLastMergeBlocks' array offset blockCount blockLen blocksToMerge lastRightFrag prevLeftBlock

grailCountLastMergeBlocks' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int
grailCountLastMergeBlocks' array offset blockCount blockLen blocksToMerge lastRightFrag prevLeftBlock
    | blocksToMerge < blockCount && lRFval < pLBval =
        grailCountLastMergeBlocks' array offset blockCount blockLen (blocksToMerge+1) lastRightFrag (prevLeftBlock - blockLen)
    | otherwise = blocksToMerge
    where
        lRFval = array !! lastRightFrag
        pLBval = array !! prevLeftBlock



grailSmartMerge :: Ord a => [a] -> Int -> Int -> Subarray -> Int -> Int -> Int -> Subarray -> ([a], Int, Subarray)
grailSmartMerge array start leftLen leftOrigin rightLen bufferOffset currBlockLen currBlockOrigin =
    let
        left = start
        middle = start + leftLen
        right = middle
        end = middle + rightLen
        buffer = start - bufferOffset
    in
        if leftOrigin == LEFT
        then grailSmartMerge' array start leftLen leftOrigin rightLen bufferOffset currBlockLen currBlockOrigin left middle right end buffer (<=)
        else grailSmartMerge' array start leftLen leftOrigin rightLen bufferOffset currBlockLen currBlockOrigin left middle right end buffer (<)

grailSmartMerge' :: [a] -> Int -> Int -> Subarray -> Int -> Int -> Int -> Subarray -> Int -> Int -> Int -> Int -> Int -> (a -> a -> Bool) -> ([a], Int, Subarray)
grailSmartMerge' array start leftLen leftOrigin rightLen bufferOffset currBlockLen currBlockOrigin left middle right end buffer op
    | left < middle && right < end = let
        (array', left', right')
            | (array !! left) `op` (array !! right) = (grailSwap array buffer left,  left + 1, right)
            | otherwise                             = (grailSwap array buffer right, left, right + 1)
        buffer' = buffer + 1
        in grailSmartMerge' array' start leftLen leftOrigin rightLen bufferOffset currBlockLen currBlockOrigin left' middle right' end buffer' op
    | otherwise =
        if left < middle
        then let
            currBlockLen' = middle - left
            array' = grailInPlaceBufferRewind array left (middle - 1) (end - 1)
            in (array', currBlockLen', currBlockOrigin)
        else let currBlockLen' = end - right
            in if leftOrigin == LEFT
                then (array, currBlockLen', RIGHT)
                else (array, currBlockLen', LEFT)



grailSmartLazyMerge :: Ord a => [a] -> Int -> Int -> Subarray -> Int -> Int -> Subarray -> ([a], Int, Subarray)
grailSmartLazyMerge array start leftLen leftOrigin rightLen currBlockLen currBlockOrigin
    | leftOrigin == LEFT && array!!(middle-1) >  array!!middle = grailSmartLazyMergeLeft array start leftLen leftOrigin rightLen middle currBlockLen currBlockOrigin
    | leftOrigin /= LEFT && array!!(middle-1) >= array!!middle = grailSmartLazyMergeRight array start leftLen leftOrigin rightLen middle currBlockLen currBlockOrigin
    | otherwise = grailSmartLazyMerge' array leftOrigin rightLen
    where
        middle = start + leftLen

grailSmartLazyMergeLeft :: Ord a => [a] -> Int -> Int -> Subarray -> Int -> Int -> Int -> Subarray -> ([a], Int, Subarray)
grailSmartLazyMergeLeft array start leftLen leftOrigin rightLen middle currBlockLen currBlockOrigin
    | leftLen /= 0 =
        let
            mergeLen = grailBinarySearchLeft array middle rightLen (array!!start)
            (array', start', rightLen', middle') = if mergeLen /= 0
                then (grailRotate array start leftLen mergeLen, start + mergeLen, rightLen - mergeLen, middle + mergeLen)
                else (array, start, rightLen, middle)
        in
            if rightLen == 0
            then (array', leftLen, currBlockOrigin)
            else let (start'', leftLen') = until (\(s, lL)-> not (lL /= 0 && array!!s <= array!!middle))
                        (\(s, lL) -> (s+1, lL -1)) (start'+1, leftLen-1)
                in
                    grailSmartLazyMergeLeft array' start'' leftLen' leftOrigin rightLen' middle' currBlockLen currBlockOrigin
    | otherwise = grailSmartLazyMerge' array leftOrigin rightLen

grailSmartLazyMergeRight :: Ord a => [a] -> Int -> Int -> Subarray -> Int -> Int -> Int -> Subarray -> ([a], Int, Subarray)
grailSmartLazyMergeRight array start leftLen leftOrigin rightLen middle currBlockLen currBlockOrigin
    | leftLen /= 0 =
        let
            mergeLen = grailBinarySearchRight array middle rightLen (array!!start)
            (array', start', rightLen', middle') = if mergeLen /= 0
                then (grailRotate array start leftLen mergeLen, start + mergeLen, rightLen - mergeLen, middle + mergeLen)
                else (array, start, rightLen, middle)
        in
            if rightLen == 0
            then (array', leftLen, currBlockOrigin)
            else let (start'', leftLen') = until (\(s, lL)-> not (lL /= 0 && array!!s < array!!lL))
                        (\(s, lL) -> (s+1, lL -1)) (start'+1, leftLen-1)
                in
                    grailSmartLazyMergeRight array' start'' leftLen' leftOrigin rightLen' middle' currBlockLen currBlockOrigin
    | otherwise = grailSmartLazyMerge' array leftOrigin rightLen

grailSmartLazyMerge' :: [a] -> Subarray -> Int -> ([a], Int, Subarray)
grailSmartLazyMerge' array leftOrigin rightLen = (array, currBlockLen, currBlockOrigin)
    where
        currBlockLen = rightLen
        currBlockOrigin
            | leftOrigin == LEFT = RIGHT
            | otherwise          = LEFT

grailSmartMergeOutOfPlace :: Ord a => [a] -> Int -> Int -> Subarray -> Int -> Int -> ([a], Int, Subarray)
grailSmartMergeOutOfPlace array start leftLen leftOrigin rightLen bufferOffset = let
    left = start
    middle = start + leftLen
    right = middle
    end = middle + rightLen
    buffer = start - bufferOffset
    in if leftOrigin == LEFT
    then grailSmartMergeOutOfPlace' array start leftLen leftOrigin rightLen bufferOffset left middle right end buffer (<=)
    else grailSmartMergeOutOfPlace' array start leftLen leftOrigin rightLen bufferOffset left middle right end buffer (<)

grailSmartMergeOutOfPlace' :: [t1] -> Int -> Int -> Subarray -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (t1 -> t1 -> Bool) -> ([t1], Int, Subarray)
grailSmartMergeOutOfPlace' array start leftLen leftOrigin rightLen bufferOffset left middle right end buffer op
    | left < middle && right < end = let
        buffer' = buffer + 1
        (array', left', right')
            | (array !! left) `op` (array !! right) = (grailSwap array buffer left, left + 1, right)
            | otherwise                             = (grailSwap array buffer right, left, right + 1)
        in
            grailSmartMergeOutOfPlace' array' start leftLen leftOrigin rightLen bufferOffset left' middle right' end buffer' op
    | otherwise = if left < middle
        then
            let array' = grailOutOfPlaceBufferRewind array left (middle - 1) (end - 1)
            in (array', middle - left, leftOrigin)
        else let cBLen = end - right
            in if leftOrigin == LEFT
                then (array, cBLen, RIGHT)
                else (array, cBLen, LEFT)



grailMergeBlocks :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [a]
grailMergeBlocks array firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen =
    let
        nextBlock = start + blockLen
        currBlockLen = blockLen
        currBlockOrigin = grailGetSubarray array firstKey medianKey
    in
        grailMergeBlocks' array firstKey medianKey blockCount blockLen lastMergeBlocks lastLen nextBlock currBlockLen currBlockOrigin 1

grailMergeBlocks' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Subarray -> Int -> [a]
grailMergeBlocks' array firstKey medianKey blockCount blockLen lastMergeBlocks lastLen nextBlock currBlockLen currBlockOrigin keyIndex
    | keyIndex < blockCount =
        let
            currBlock = nextBlock - currBlockLen
            nextBlockOrigin = grailGetSubarray array (firstKey + keyIndex) medianKey
            buffer = currBlock - blockLen
            (array', currBlockLen', currBlockOrigin')
                | nextBlockOrigin == currBlockOrigin =  (grailBlockSwap array buffer currBlock currBlockLen, blockLen, currBlockOrigin)
                | otherwise = grailSmartMerge array currBlock currBlockLen currBlockOrigin blockLen blockLen currBlockLen currBlockOrigin
        in
            grailMergeBlocks' array' firstKey medianKey blockCount blockLen lastMergeBlocks lastLen (nextBlock+blockLen) currBlockLen' currBlockOrigin' (keyIndex+1)
    | otherwise =
        let
            currBlock = nextBlock - currBlockLen
            buffer = currBlock - blockLen
            array' = grailBlockSwap array buffer currBlock currBlockLen
            (array'', currBlock', currBlockLen')
                | currBlockOrigin == RIGHT = (array', nextBlock, blockLen * lastMergeBlocks)
                | otherwise = (array, currBlock, currBlockLen + blockLen * lastMergeBlocks)
        in
            if lastLen /= 0
            then grailMergeForwards array'' currBlock' currBlockLen' lastLen blockLen
            else grailBlockSwap array buffer currBlock currBlockLen



grailLazyMergeBlocks :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [a]
grailLazyMergeBlocks array firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen =
    let
        nextBlock = start + blockLen
        currBlockLen = blockLen
        currBlockOrigin = grailGetSubarray array firstKey medianKey
    in 
        grailLazyMergeBlocks' array firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen nextBlock currBlockLen currBlockOrigin 1

grailLazyMergeBlocks' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Subarray -> Int -> [a]
grailLazyMergeBlocks' array firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen nextBlock currBlockLen currBlockOrigin keyIndex
    | keyIndex < blockCount =
        let
            currBlock = nextBlock - currBlockLen
            nextBlockOrigin = grailGetSubarray array (firstKey + keyIndex) medianKey
            (array', currBlockLen', currBlockOrigin')
                | nextBlockOrigin == currBlockOrigin = (array, blockLen, currBlockOrigin)
                | otherwise = grailSmartLazyMerge array currBlock currBlockLen currBlockOrigin blockLen currBlockLen currBlockOrigin
        in
            grailLazyMergeBlocks' array' firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen (nextBlock + blockLen) currBlockLen' currBlockOrigin' (keyIndex + 1)
    | otherwise =
        let
            currBlock = nextBlock - currBlockLen
            (currBlock', currBlockLen')
                    | currBlockOrigin == RIGHT = (nextBlock, blockLen * lastMergeBlocks)
                    | otherwise = (currBlock, currBlockLen + blockLen * lastMergeBlocks)
        in
            if lastLen /= 0
            then grailLazyMerge array currBlock' currBlockLen' lastLen 
            else array 



grailMergeBlocksOutOfPlace :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [a]
grailMergeBlocksOutOfPlace array firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen = let
    nextBlock = start + blockLen
    currBlockLen = blockLen
    currBlockOrigin = grailGetSubarray array firstKey medianKey
    in
        grailMergeBlocksOutOfPlace' array firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen nextBlock currBlockLen currBlockOrigin 1

grailMergeBlocksOutOfPlace' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Subarray -> Int -> [a]
grailMergeBlocksOutOfPlace' array firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen nextBlock currBlockLen currBlockOrigin keyIndex
    | keyIndex < blockCount = let
        currBlock = nextBlock - currBlockLen
        nextBlockOrigin = grailGetSubarray array (firstKey + keyIndex) medianKey
        in if nextBlockOrigin == currBlockOrigin
            then let
                buffer = currBlock - blockLen
                array' = arrayCopy array currBlock array buffer currBlockLen
                currBlockLen' = blockLen
                in
                grailMergeBlocksOutOfPlace' array' firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen (nextBlock + blockLen) currBlockLen' currBlockOrigin (keyIndex + 1)
            else let
                (array', cBLen, cBOrigin) = grailSmartMergeOutOfPlace array currBlock currBlockLen currBlockOrigin blockLen blockLen
                in
                grailMergeBlocksOutOfPlace' array' firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen (nextBlock + blockLen) cBLen cBOrigin (keyIndex + 1)
    | otherwise = let
        currBlock = nextBlock - currBlockLen
        buffer = currBlock - blockLen
        in if lastLen /= 0
            then if currBlockOrigin == RIGHT
                then let
                    array' = arrayCopy array currBlock array buffer currBlockLen
                    currBlock' = nextBlock
                    cBLen = blockLen * lastMergeBlocks
                    cBOrigin = LEFT
                    in
                    grailMergeOutOfPlace array' currBlock' cBLen lastLen blockLen --(array'', cBLen, cBOrigin)
                else let
                    cBLen = currBlockLen + blockLen * lastMergeBlocks
                    in
                    grailMergeOutOfPlace array currBlock cBLen lastLen blockLen --(array', cBLen, currBlockOrigin)
            else arrayCopy array currBlock array buffer currBlockLen--(array', currBlockLen, currBlockOrigin)



grailCombineInPlace :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool -> [a]
grailCombineInPlace array firstKey start length subarrayLen blockLen mergeCount lastSubarrays buffer =
    let
        fullMerge = 2 * subarrayLen
        blockCount = fullMerge `quot` blockLen
    in
        grailCombineInPlace' array firstKey start length subarrayLen blockLen mergeCount lastSubarrays buffer fullMerge blockCount 0

grailCombineInPlace' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool -> Int -> Int -> Int -> [a]
grailCombineInPlace' array firstKey start length subarrayLen blockLen mergeCount lastSubarrays buffer fullMerge blockCount mergeIndex
    | mergeIndex < mergeCount = let
        offset = start + (mergeIndex * fullMerge)
        array' = grailInsertSort array firstKey blockCount
        medianKey = subarrayLen `quot` blockLen
        (array'', medianKey') = grailBlockSelectSort array' firstKey offset medianKey blockCount blockLen

        array'''
            | buffer = grailMergeBlocks array'' firstKey (firstKey + medianKey') offset blockCount blockLen 0 0
            | otherwise = grailLazyMergeBlocks array'' firstKey (firstKey + medianKey') offset blockCount blockLen 0 0
        in
            grailCombineInPlace' array''' firstKey start length subarrayLen blockLen mergeCount lastSubarrays buffer fullMerge blockCount (mergeIndex + 1)
    | otherwise = grailCombineInPlace'' array firstKey start length subarrayLen blockLen mergeCount lastSubarrays buffer fullMerge

grailCombineInPlace'' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool -> Int -> [a]
grailCombineInPlace'' array firstKey start length subarrayLen blockLen mergeCount lastSubarrays buffer fullMerge =
    let
        offset = start + mergeCount * fullMerge
        blockCount = lastSubarrays `quot` blockLen

        array' = grailInsertSort array firstKey (blockCount +1)
        medianKey = subarrayLen `quot` blockLen

        (array'', medianKey') = grailBlockSelectSort array' firstKey offset medianKey blockCount blockLen

        lastFragment = lastSubarrays - blockCount * blockLen
        lastMergeBlocks 
            | lastFragment /= 0 = grailCountLastMergeBlocks array'' offset blockCount blockLen
            | otherwise = 0
        smartMerges = blockCount - lastMergeBlocks
        leftLen = lastMergeBlocks * blockLen
        array'''
          | smartMerges == 0 && buffer = grailMergeForwards array'' offset leftLen lastFragment blockLen
          | smartMerges == 0 && not buffer = grailLazyMerge array'' offset leftLen lastFragment
          | buffer    = grailMergeBlocks array'' firstKey (firstKey + medianKey') offset smartMerges blockLen lastMergeBlocks lastFragment
          | otherwise = grailLazyMergeBlocks array'' firstKey (firstKey + medianKey') offset smartMerges blockLen lastMergeBlocks lastFragment
    in
        if lastSubarrays /= 0
        then if buffer
            then grailInPlaceBufferReset array''' start length blockLen
            else array'''
        else if buffer
            then grailInPlaceBufferReset array start length blockLen
            else array



grailCombineOutOfPlace :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [a] -> t -> ([a], Maybe [a])
grailCombineOutOfPlace array firstKey start length subarrayLen blockLen mergeCount lastSubarrays extBuffer extBufferLen = --(array, extBuffer)
    let
        extBuffer' = arrayCopy array (start - blockLen) extBuffer 0 blockLen
        fullMerge = 2 * subarrayLen
        blockCount = fullMerge `quot` blockLen
    in grailCombineOutOfPlace' array firstKey start length subarrayLen blockLen mergeCount lastSubarrays extBuffer' extBufferLen fullMerge blockCount 0

grailCombineOutOfPlace' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [a] -> t -> Int -> Int -> Int -> ([a], Maybe [a])
grailCombineOutOfPlace' array firstKey start length subarrayLen blockLen mergeCount lastSubarrays extBuffer extBufferLen fullMerge blockCount mergeIndex
    | mergeIndex < mergeCount = let
            offset = start + mergeIndex * fullMerge
            array' = grailInsertSort array firstKey blockCount
            medianKey = subarrayLen `quot` blockLen
            (array'', medianKey') = grailBlockSelectSort array' firstKey offset medianKey blockCount blockLen
            array''' = grailMergeBlocksOutOfPlace array'' firstKey (firstKey + medianKey') offset blockCount blockLen 0 0
        in
            grailCombineOutOfPlace' array''' firstKey start length subarrayLen blockLen mergeCount lastSubarrays extBuffer extBufferLen fullMerge blockCount (mergeIndex + 1)
    | lastSubarrays /= 0 = let
            offset = start + mergeCount * fullMerge
            blockCount' = lastSubarrays `quot` blockLen
            array' = grailInsertSort array firstKey (blockCount' + 1)

            medianKey = subarrayLen `quot` blockLen
            (array'', medianKey') = grailBlockSelectSort array' firstKey offset medianKey blockCount' blockLen
            lastFragment = lastSubarrays - blockCount' * blockLen

            lastMergeBlocks
                | lastFragment /= 0 = grailCountLastMergeBlocks array'' offset blockCount' blockLen
                | otherwise = 0
            smartMerges = blockCount' - lastMergeBlocks

            array'''
                | smartMerges == 0 = grailMergeOutOfPlace array'' offset (lastMergeBlocks * blockLen) lastFragment blockLen
                | otherwise = grailMergeBlocksOutOfPlace array'' firstKey (firstKey + medianKey') offset smartMerges blockLen lastMergeBlocks lastFragment
            in grailCombineOutOfPlace'' array''' start length blockLen extBuffer

    | otherwise = grailCombineOutOfPlace'' array  start length  blockLen extBuffer

grailCombineOutOfPlace'' :: [a] -> Int -> Int -> Int -> [a] -> ([a], Maybe [a])
grailCombineOutOfPlace'' array start length blockLen extBuffer =
    let
        array'  = grailOutOfPlaceBufferReset array start length blockLen
        array'' = arrayCopy extBuffer 0 array' (start - blockLen) blockLen
    in
        (array'', Just extBuffer)



grailCombineBlocks :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Bool -> Maybe [a] -> Int -> ([a], Maybe [a])
grailCombineBlocks array firstKey start length subarrayLen blockLen buffer extBuffer extBufferLen 
    | buffer && blockLen <= extBufferLen = grailCombineOutOfPlace array firstKey start length' subarrayLen blockLen mergeCount lastSubarrays' (fromJust extBuffer) extBufferLen
    | otherwise = (grailCombineInPlace array firstKey start length' subarrayLen blockLen mergeCount lastSubarrays' buffer, Nothing)
    where
        fullMerge = 2 * subarrayLen
        mergeCount = length `quot` fullMerge
        lastSubarrays = length - fullMerge * mergeCount
        (length', lastSubarrays') 
            | lastSubarrays <= subarrayLen = (length - lastSubarrays, 0)
            | otherwise = (length, lastSubarrays)




grailLazyMerge :: Ord a => [a] -> Int -> Int -> Int -> [a]
grailLazyMerge array start leftLen rightLen =
    if leftLen < rightLen
        then let
            middle = start + leftLen
            in grailLazyMergeLeft array start leftLen rightLen middle
        else let
            end = start + leftLen + rightLen - 1
            in grailLazyMergeRight array start leftLen rightLen end

grailLazyMergeLeft :: Ord a => [a] -> Int -> Int -> Int -> Int -> [a]
grailLazyMergeLeft array start leftLen rightLen middle
    | leftLen /= 0 = let
        mergeLen = grailBinarySearchLeft array middle rightLen (array!!start)
        test = mergeLen /= 0
        array' 
            | test = grailRotate array start leftLen mergeLen
            | otherwise = array
        (start', rightLen', middle') 
            | test = (start + mergeLen, rightLen - mergeLen, middle + mergeLen)
            | otherwise = (start, rightLen, middle)
        (start'', leftLen') = until (\(st,ll) -> not (ll /= 0 && (array'!!st) <= (array'!!middle'))) 
            (\(st,ll) -> (st+1,ll-1)) (start' +1, leftLen -1)
        in
            if rightLen' == 0
            then array'
            else grailLazyMergeLeft array' start'' leftLen' rightLen' middle'
    | otherwise = array

grailLazyMergeRight :: Ord a => [a] -> Int -> Int -> Int -> Int -> [a]
grailLazyMergeRight array start leftLen rightLen end
    | rightLen /= 0 = let
        mergeLen = grailBinarySearchRight array start leftLen (array!!end)
        test = mergeLen /= leftLen
        array' 
            | test = grailRotate array (start + mergeLen) (leftLen - mergeLen) rightLen
            | otherwise = array
        (end', leftLen') 
            | test = (end - (leftLen - mergeLen), mergeLen)
            | otherwise = (end, leftLen)
        middle = start + leftLen'
        (rightLen', end'') = until (\(rl,en) -> not (rl /= 0 && (array'!!(middle-1)) <= (array'!!en))) (\(rl,en) -> (rl-1,en-1)) (rightLen -1, end' -1)
        in
            if leftLen' == 0
            then array'
            else grailLazyMergeRight array' start leftLen' rightLen' end''
    | otherwise = array



grailLazyStableSort :: Ord a => [a] -> Int -> Int -> [a]
grailLazyStableSort array start length =
    let
        array' = grailLazyStableSort' array start length 1
        mergeLen = 2
    in
        grailLazyStableSort'' array' start length mergeLen

grailLazyStableSort' :: Ord a => [a] -> Int -> Int -> Int -> [a]
grailLazyStableSort' array start length index
    | index < length =
        let
            left = start + index -1
            right = start + index
        in
            if (array !! left) > (array !! right)
            then grailLazyStableSort' (grailSwap array left right) start length (index + 2)
            else grailLazyStableSort' array start length (index + 2)
    | otherwise = array

grailLazyStableSort'' :: Ord a => [a] -> Int -> Int -> Int -> [a]
grailLazyStableSort'' array start length mergeLen
    | mergeLen < length =
        let
            fullMerge = 2 * mergeLen
            mergeEnd = length - fullMerge
            (mergeIndex, array') = until (\(index, _) -> index > mergeEnd) (\(index, arr) ->
                (index + fullMerge, grailLazyMerge arr (start + index) mergeLen mergeLen)) (0, array)
            leftOver = length - mergeIndex
            array'' = grailLazyMerge array' (start + mergeIndex) mergeLen (leftOver - mergeLen)
        in
            if leftOver > mergeLen
            then grailLazyStableSort'' array'' start length (mergeLen * 2)
            else grailLazyStableSort'' array' start length (mergeLen * 2)
    | otherwise = array



grailCommonSort :: Ord a => [a] -> Int -> Int -> Maybe [a] -> Int -> [a]
grailCommonSort array start length extBuffer extBufferLen
    | length < 16 = grailInsertSort array start length
    | keysFound < idealKeys = if keysFound < 4
        then grailLazyStableSort array start length
        else grailCommonSort' array' start length extBuffer extBufferLen blockLen' keyLen' False
    | otherwise = grailCommonSort' array' start length extBuffer extBufferLen blockLen  keyLen  True 
        where
            blockLen = until (\x -> x*x >= length) (*2) 1
            keyLen = ((length - 1) `quot` blockLen) + 1
            idealKeys = keyLen + blockLen
            (keysFound, array') = grailCollectKeys array start length idealKeys
            keyLen' = until (<= keysFound) (`quot` 2) blockLen
            blockLen' = 0

grailCommonSort' :: Ord a => [a] -> Int -> Int -> Maybe [a] -> Int -> Int -> Int -> Bool -> [a]
grailCommonSort' array start length extBuffer extBufferLen blockLen keyLen idealBuffer =
    let
        bufferEnd = blockLen + keyLen
        subarrayLen 
            | idealBuffer = blockLen
            | otherwise   = keyLen
        (extBuffer', extBufferLen') = if idealBuffer && isJust extBuffer
            then (extBuffer, extBufferLen)
            else (Nothing, 0)
        (array', extBuffer'') = grailBuildBlocks array (start + bufferEnd) (length - bufferEnd) subarrayLen extBuffer' extBufferLen'
    in
        grailCommonSort'' array' start length extBuffer'' extBufferLen' blockLen keyLen idealBuffer bufferEnd subarrayLen

grailCommonSort'' :: Ord a => [a] -> Int -> Int -> Maybe [a] -> Int -> Int -> Int -> Bool -> Int -> Int -> [a]
grailCommonSort'' array start length extBuffer extBufferLen blockLen keyLen idealBuffer bufferEnd subarrayLen
    | length - bufferEnd > 2 * subarrayLen =
        let
            subarrayLen' = subarrayLen * 2
            currentBlockLen = blockLen
            scrollingBuffer = idealBuffer
            keyBuffer = keyLen `quot` 2
            (currentBlockLen', scrollingBuffer') 
                | idealBuffer                                        = (currentBlockLen, scrollingBuffer)
                | keyBuffer >= ((2 * subarrayLen') `quot` keyBuffer) = (keyBuffer, True)
                | otherwise                                          = ((2 * subarrayLen') `quot` keyLen, scrollingBuffer)
            (array', extBuffer') =  grailCombineBlocks array start (start + bufferEnd) (length - bufferEnd) subarrayLen' currentBlockLen' scrollingBuffer' extBuffer extBufferLen
        in
            grailCommonSort'' array' start length extBuffer' extBufferLen blockLen keyLen idealBuffer bufferEnd subarrayLen'
    | otherwise = grailLazyMerge (grailInsertSort array start bufferEnd) start bufferEnd (length - bufferEnd)



-- area of functions one would consider public in other languages

-- no external buffer used
grailSortInPlace :: Ord a => [a] -> Int -> Int -> [a]
grailSortInPlace array start length 
    | null array = []
    | otherwise = 
    let extBuffer = Nothing
        extBufferLen = 0
    in grailCommonSort array start length extBuffer extBufferLen

-- fixed-length external buffer used
grailSortStaticOOP :: Ord a => [a] -> Int -> Int -> [a]
grailSortStaticOOP array start length 
  | null array = []
  | otherwise = 
    let var = head array
        extBufferLen = 512
        extBuffer = replicate extBufferLen var
    in grailCommonSort array start length (Just extBuffer) extBufferLen
    

-- external buffer with length of smallest power of 2 larger than square root of input's length used 
grailSortDynamicOOP :: Ord a => [a] -> Int -> Int -> [a]
grailSortDynamicOOP array start length 
  | null array = []
  | otherwise = 
    let var = head array
        extBufferLen = until (\x -> x * x >= length) (*2) 1
        extBuffer = replicate extBufferLen var
    in grailCommonSort array start length (Just extBuffer) extBufferLen
