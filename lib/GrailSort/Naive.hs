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
Mihnik's Haskell "naive" implementation of Grailsort (no in-place guarantee)

Rewritten without any magic of complicated modules, based on Amari Calipso's Python version

------ IMPORTANT NOTE: Due to the purely functional approach, every operation on the list creates an accordingly modified copy.
------ This can take a lot of memory should the input list be large. Keep that in mind.

Current status: Done
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module GrailSort.Naive  (grailSortInPlace, grailSortStaticOOP, grailSortDynamicOOP) where
import Data.Maybe (isJust, fromJust)

--support function
listCopy :: [a] -> Int -> [a] -> Int -> Int -> [a]
listCopy fromList fromIndex toList toIndex length =
    let
        slice = drop fromIndex . take (fromIndex + length) $ fromList
    in
        take toIndex toList ++ slice ++ drop (toIndex + length) toList

--support data structure
data Sublist = LEFT | RIGHT deriving (Eq, Show)



listWrite :: Ord a => [a] -> Int -> Int -> [a]
listWrite list' target source = take target list' ++ [list' !! source] ++ drop (target + 1) list'


grailSwap :: [a] -> Int -> Int -> [a]
grailSwap list a b
    | a == b = list
    | a > b = grailSwap list b a
    | otherwise = lessCutsSwap'' list a b --take a list ++ [b'] ++ take (b - a - 1) (drop (a + 1) list) ++ [a'] ++ drop (b + 1) list
    --where
    --    a' = list !! a
    --    b' = list !! b

lessCutsSwap'' :: [a] -> Int -> Int -> [a]
lessCutsSwap'' list a b =
    let
        diff = b - a - 1
        start = take a list
        (mh:mt) = drop a list
        mid' = take diff mt
        (fh:ft) = drop diff mt
    in
        start ++ fh:mid' ++ mh:ft



grailBlockSwap :: [a] -> Int -> Int -> Int -> [a]
grailBlockSwap list a b blockLen =
    let
      x = min a b
      y = max a b
      diff = y - x
      rest = blockLen - diff
    in
        if x + blockLen > y
        then take x list ++ take blockLen (drop y list) ++ take diff (drop rest $ cycle (take diff $ drop x list)) ++ drop (y + blockLen) list
        else take x list ++ take blockLen (drop y list) ++ take (diff - blockLen) (drop (x + blockLen) list) ++ take blockLen (drop x list) ++ drop (y + blockLen) list



grailRotate :: [a] -> Int -> Int -> Int -> [a]
grailRotate list start leftLen rightLen =
    let
        beginning = take start list
        leftBlock = take leftLen (drop start list)
        rightBlock = take rightLen (drop (start + leftLen) list)
        finish = drop (start + leftLen + rightLen) list
    in
        beginning ++ rightBlock ++ leftBlock ++ finish



grailInsertSort :: (Ord a) => [a] -> Int -> Int -> [a]
grailInsertSort = gnomesortSetup --grailInsertSort list start length = grailInsertSort' list start length 1

grailInsertSort' :: Ord a => [a] -> Int -> Int -> Int -> [a]
grailInsertSort' list start length item
    | item == length = list
    | otherwise = grailInsertSort' (grailInsert list start item (start + item - 1) (start + item)) start length (item + 1)

grailInsert :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> [a]
grailInsert list start item left right
    | left < start = list
    | list !! left > list !! right = grailInsert (grailSwap list left right) start item (left - 1) (right - 1)
    | otherwise = list

reverseAppend :: [a] -> [a] -> [a]
reverseAppend [] x = x
reverseAppend (xh:xt) t = reverseAppend xt (xh:t)

gnomesortSetup :: Ord a => [a] -> Int -> Int -> [a]
gnomesortSetup [] _ _ = []
gnomesortSetup list start length = take start list ++ reverseAppend (gnomesort (take length (drop start list))) (drop (start + length) list)

--source: https://rosettacode.org/wiki/Sorting_algorithms/Gnome_sort#Haskell
gnomesort :: Ord a => [a] -> [a]
gnomesort [] = []
gnomesort (x:xs) = gs [x] xs
 where
    gs vv@(v:vs) (w:ws)
        | v<=w = gs (w:vv) ws
        | otherwise = gs vs (w:v:ws)
    gs [] (y:ys) = gs [y] ys
    gs xs [] = xs --reverse xs



grailBinarySearchLeft :: (Ord a) => [a] -> Int -> Int -> a -> Int
grailBinarySearchLeft list start length target =
    let
    left  = 0
    right = length
    in
    grailBinarySearchLeft' list start left right target

grailBinarySearchLeft' :: (Ord a) => [a] -> Int -> Int -> Int -> a -> Int
grailBinarySearchLeft' list start left right target
    | left < right =
    let
        middle = left + ((right - left) `quot` 2)
    in
        if list !! (start + middle) < target
        then grailBinarySearchLeft' list start (middle + 1) right target
        else grailBinarySearchLeft' list start left middle target
    | otherwise = left



grailBinarySearchRight :: (Ord a) => [a] -> Int -> Int -> a -> Int
grailBinarySearchRight list start length target =
    let
    left  = 0
    right = length
    in
    grailBinarySearchRight' list start left right target

grailBinarySearchRight' :: (Ord a) => [a] -> Int -> Int -> Int -> a -> Int
grailBinarySearchRight' list start left right target
    | left < right =
    let
        middle = left + ((right - left) `quot` 2)
    in
        if list !! (start + middle) > target
        then grailBinarySearchRight' list start left middle target
        else grailBinarySearchRight' list start (middle + 1) right target
    | otherwise = right



grailCollectKeys :: (Ord a) => [a] -> Int -> Int -> Int -> (Int, [a])
grailCollectKeys list start length idealKeys = grailCollectKeys' list start length idealKeys 1 0 1

grailCollectKeys' :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, [a])
grailCollectKeys' list start length idealKeys keysFound firstKey currKey
    | currKey < length && keysFound < idealKeys =
    let insertPos = grailBinarySearchLeft list (start + firstKey) keysFound (list !! (start + currKey))
        newFirstKey = currKey - keysFound
        list1 = grailRotate list (start + firstKey) keysFound (currKey - (firstKey + keysFound))
        list2 = grailRotate list1 (start + newFirstKey + insertPos) (keysFound - insertPos) 1
    in if insertPos == keysFound || (list !! (start + currKey)) /= (list !! (start + firstKey + insertPos))
        then grailCollectKeys' list2 start length idealKeys (keysFound + 1) newFirstKey (currKey + 1)
        else grailCollectKeys' list start length idealKeys keysFound firstKey (currKey + 1)
    | otherwise = (keysFound, grailRotate list start firstKey keysFound)



grailPairwiseSwaps :: Ord a => [a] -> Int -> Int -> [a]
grailPairwiseSwaps = testPairwiseSwapsSetup --grailPairwiseSwaps list start length = grailPairwiseSwaps' list start length 1

grailPairwiseSwaps' :: Ord a => [a] -> Int -> Int -> Int -> [a]
grailPairwiseSwaps' list start length index
    | index < length = if (list !! left) > (list !! right)
        then let
            list1 = grailSwap list (left - 2) right
            list2 = grailSwap list1 (right - 2) left
            in grailPairwiseSwaps' list2 start length (index + 2)
        else let
            list1 = grailSwap list (left - 2) left
            list2 = grailSwap list1 (right - 2) right
            in grailPairwiseSwaps' list2  start length (index + 2)
    | otherwise = if left < start + length
        then grailSwap list (left - 2) left
        else list
    where
        left = start + index - 1
        right = start + index

testPairwiseSwapsSetup :: Ord a => [a] ->Int -> Int -> [a]
testPairwiseSwapsSetup list start length = take (start - 2) list ++ testPairwiseSwaps (drop (start - 2) list) length

testPairwiseSwaps :: Ord a => [a] -> Int -> [a]
testPairwiseSwaps list length
    | length > 1 = let (a:b:c:d:tt) = list in
        if c > d
        then d:c:testPairwiseSwaps (b:a:tt) (length-2)
        else c:d:testPairwiseSwaps (a:b:tt) (length-2)
    | otherwise = if length == 1
        then let (a:b:c:t) = list in c:b:a:t
        else list



grailPairwiseWrites :: Ord a => [a] -> Int -> Int -> [a]
grailPairwiseWrites = testPairwisewritesSetup --grailPairwiseWrites list start length = grailPairwiseWrites' list start length 1

grailPairwiseWrites' :: Ord a => [a] -> Int -> Int -> Int -> [a]
grailPairwiseWrites' list start length index
    | index < length = if (list !! left) > (list !! right)
        then let
            list1 = listWrite list (left - 2) right
            list2 = listWrite list1 (right - 2) left
            in grailPairwiseWrites' list2 start length (index + 2)
        else let
            list1 = listWrite list (left - 2) left
            list2 = listWrite list1 (right - 2) right
            in grailPairwiseWrites' list2  start length (index + 2)
    | otherwise = if left < start + length
        then listWrite list (left - 2) left
        else list
    where
        left = start + index - 1
        right = start + index

testPairwisewritesSetup :: Ord a => [a] ->Int -> Int -> [a]
testPairwisewritesSetup list start length = take (start - 2) list ++ testPairwiseWrites (drop (start - 2) list) length

testPairwiseWrites :: Ord a => [a] -> Int -> [a]
testPairwiseWrites list length
    | length > 1 = let (_:_:c:d:tt) = list in
        if c > d
        then d:c:testPairwiseWrites (c:d:tt) (length-2)
        else c:d:testPairwiseWrites (c:d:tt) (length-2)
    | otherwise = if length == 1
        then let (_:b:c:t) = list in c:b:c:t
        else list



grailMergeForwards :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> [a]
grailMergeForwards = testMergeForwardsSetup --list start leftLen rightLen bufferOffset =
    --let left   = start
    --    middle = start + leftLen
    --    right  = middle
    --    end    = middle + rightLen
    --    buffer = start - bufferOffset
    --in grailMergeForwards' list left middle right end buffer

testMergeForwardsSetup :: Ord a => [a] -> Int -> Int -> Int -> Int -> [a]
testMergeForwardsSetup list start leftLen rightLen bufferOffset =
    let startCut = start - bufferOffset
        left   = bufferOffset
        middle = left + leftLen
        right  = middle
        end    = middle + rightLen
        buffer = 0
    in take startCut list ++ grailMergeForwards' (drop startCut list) left middle right end buffer

grailMergeForwards' :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> Int -> [a]
grailMergeForwards' list left middle right end buffer
    | right < end =
        if left == middle || list !! left > list !! right
        then grailMergeForwards' (grailSwap list buffer right) left middle (right+1) end (buffer+1)
        else grailMergeForwards' (grailSwap list buffer left) (left+1) middle right end (buffer+1)
    | buffer /= left = grailBlockSwap list buffer left (middle-left)
    | otherwise = list



grailMergeBackwards :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> [a]
grailMergeBackwards list start leftLen rightLen bufferOffset =
    let end    = start - 1
        left   = end + leftLen
        middle = left
        right  = middle + rightLen
        buffer = right + bufferOffset
    in grailMergeBackwards' list end left middle right buffer

grailMergeBackwards' :: (Ord a) => [a] -> Int -> Int ->  Int -> Int -> Int -> [a]
grailMergeBackwards' list end left middle right buffer
    | left > end =
        if right == middle || list !! left > list !! right
        then grailMergeBackwards' (grailSwap list buffer left) end (left - 1) middle right (buffer - 1)
        else grailMergeBackwards' (grailSwap list buffer right) end left middle (right - 1) (buffer - 1)
    | right /= buffer = grailMergeBackwards'' list middle right buffer
    | otherwise = list

grailMergeBackwards'' :: (Ord a) => [a] -> Int -> Int -> Int -> [a]
grailMergeBackwards'' list middle right buffer
    | right > middle = grailMergeBackwards'' (grailSwap list buffer right) middle (right - 1) (buffer - 1)
    | otherwise = list



grailMergeOutOfPlace :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> [a]
grailMergeOutOfPlace list start leftLen rightLen bufferOffset =
    let left   = start
        middle = start + leftLen
        right  = middle
        end    = middle + rightLen
        buffer = start - bufferOffset
    in grailMergeOutOfPlace' list left middle right end buffer

grailMergeOutOfPlace' :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> Int -> [a]
grailMergeOutOfPlace' list left middle right end buffer
    | right < end = let
            left' = list !! left
            right' = list !! right
            listL = listWrite list buffer left
            listR = listWrite list buffer right
        in if left == middle || left' > right'
            then grailMergeOutOfPlace' listR left middle (right + 1) end (buffer + 1)
            else grailMergeOutOfPlace' listL (left + 1) middle right end (buffer + 1)
    | otherwise = if buffer /= left
        then grailMergeOutOfPlace'' list left middle end buffer
        else list

grailMergeOutOfPlace'' :: Ord a => [a] -> Int -> Int -> Int -> Int -> [a]
grailMergeOutOfPlace'' list left middle end buffer
    | left < middle = grailMergeOutOfPlace'' (listWrite list buffer left) (left + 1) middle end (buffer + 1)
    | otherwise = list



grailBuildInPlace :: (Ord a) => [a] -> Int -> Int -> Int -> Int -> [a]
grailBuildInPlace list start length currentLen bufferLen = --list
    let mergeLen = currentLen
    in grailBuildInPlace' list start length currentLen bufferLen mergeLen

grailBuildInPlace' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> [a]
grailBuildInPlace' list start length currentLen bufferLen mergeLen
    | mergeLen < bufferLen =
        let
            fullMerge = 2 * mergeLen
            mergeEnd = start + length - fullMerge
            bufferOffset = mergeLen
            (mergeIndex, list') = until (\(index,_) -> index > mergeEnd)
                (\(index, arr) -> (index + fullMerge, grailMergeForwards arr index mergeLen mergeLen bufferOffset)
                ) (start, list)
            leftOver = length - (mergeIndex - start)

            list'' = if leftOver > mergeLen
                then grailMergeForwards list' mergeIndex mergeLen (leftOver - mergeLen) bufferOffset
                else grailRotate list' (mergeIndex - mergeLen) mergeLen leftOver
        in
            grailBuildInPlace' list'' (start - mergeLen) length currentLen bufferLen fullMerge
    | otherwise =
        let
        fullMerge = 2 * bufferLen
        lastBlock = length `rem` fullMerge
        lastOffset = start + length - lastBlock
        list'
            | lastBlock <= bufferLen = grailRotate list lastOffset lastBlock bufferLen
            | otherwise  = grailMergeBackwards list lastOffset bufferLen (lastBlock - bufferLen) bufferLen
        mergeIndex = lastOffset - fullMerge
        in
        snd $ until (\(index,_) -> index < start) (\(index, list'') ->
                (index - fullMerge, grailMergeBackwards list'' index bufferLen bufferLen bufferLen))
                (mergeIndex, list')



grailBuildOutOfPlace :: Ord a => [a] -> Int -> Int -> Int -> Int -> [a] -> Int -> ([a], Maybe [a])
grailBuildOutOfPlace list start length bufferLen extLen extBuffer extBufferLen = --([], [])
    let
        extBuffer' = listCopy list (start - extLen) extBuffer 0 extLen
        list' = grailPairwiseWrites list start length
        start' = start - 2
        mergeLen = 2
    in
        grailBuildOutOfPlace' list' start' length bufferLen extLen extBuffer' extBufferLen mergeLen

grailBuildOutOfPlace' :: Ord a => [a] -> Int -> Int -> Int -> Int -> [a] -> Int -> Int -> ([a], Maybe [a])
grailBuildOutOfPlace' list start length bufferLen extLen extBuffer extBufferLen mergeLen
    | mergeLen < extLen =
        let
            fullMerge = 2 * mergeLen
            mergeEnd = start + length - fullMerge
            bufferOffset = mergeLen
            (mergeIndex, list') = until (\(index, _) -> index > mergeEnd) (\(index, arr) ->
                (index + fullMerge, grailMergeOutOfPlace arr index mergeLen mergeLen bufferOffset))
                (start, list)
            leftOver = length - (mergeIndex - start)
            list'' = if leftOver > mergeLen
                then grailMergeOutOfPlace list' mergeIndex mergeLen (leftOver - mergeLen) bufferOffset
                else listCopy list' mergeIndex list' (mergeIndex - mergeLen) leftOver
        in
            grailBuildOutOfPlace' list'' (start - mergeLen) length bufferLen extLen extBuffer extBufferLen (mergeLen * 2)
    | otherwise =
        (grailBuildInPlace (listCopy extBuffer 0 list (start + length) extLen) start length mergeLen bufferLen, Just extBuffer)



grailBuildBlocks :: Ord a => [a] -> Int -> Int -> Int -> Maybe [a] -> Int -> ([a], Maybe [a])
grailBuildBlocks list start length bufferLen extBuffer extBufferLen
    | isJust extBuffer =
            grailBuildOutOfPlace list start length bufferLen extLen (fromJust extBuffer) extBufferLen
    | otherwise =
            (grailBuildInPlace list' (start - 2) length 2 bufferLen, Nothing)
    where
        extLen
          | bufferLen < extBufferLen = bufferLen
          | otherwise = until (\x -> x*2 > extBufferLen) (*2) 1
        list' = grailPairwiseSwaps list start length



--thanks to Taihennami for finding where the sort goes wrong
grailBlockSelectSort :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> ([a], Int)
grailBlockSelectSort list firstKey start medianKey blockCount blockLen =
    let
        firstBlock = 0
    in
        grailBlockSelectSort'  list firstKey start medianKey blockCount blockLen firstBlock

grailBlockSelectSort' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> ([a], Int)
grailBlockSelectSort'  list firstKey start medianKey blockCount blockLen firstBlock
    | firstBlock < blockCount = let
        selectBlock = grailBlockSelectSort'' list firstKey start blockCount firstBlock firstBlock blockLen (firstBlock + 1)
        list'  = grailBlockSwap list (start + firstBlock * blockLen) (start + selectBlock * blockLen) blockLen
        list'' = grailSwap list' (firstKey + firstBlock) (firstKey + selectBlock)
        in
            if selectBlock /= firstBlock
            then if medianKey == firstBlock
                then grailBlockSelectSort' list'' firstKey start selectBlock blockCount blockLen (firstBlock + 1)
                else if medianKey == selectBlock
                    then grailBlockSelectSort' list'' firstKey start firstBlock blockCount blockLen (firstBlock + 1)
                    else grailBlockSelectSort' list'' firstKey start medianKey blockCount blockLen (firstBlock + 1)
            else grailBlockSelectSort'  list firstKey start medianKey blockCount blockLen (firstBlock + 1)
    | otherwise = (list, medianKey)

grailBlockSelectSort'' :: Ord a => [a] -> Int -> Int -> Int -> t -> Int -> Int -> Int -> Int
grailBlockSelectSort'' list firstKey start blockCount firstBlock selectBlock blockLen currBlock
    | currBlock < blockCount =
        let
            comp =  compare (list!!(start + currBlock * blockLen)) (list!!(start + selectBlock * blockLen))
            selectBlock' = if comp == LT || (comp == EQ && list!!(firstKey + currBlock) < list!!(firstKey + selectBlock))
                then currBlock
                else selectBlock
        in
            grailBlockSelectSort'' list firstKey start blockCount firstBlock selectBlock' blockLen (currBlock + 1)
    | otherwise = selectBlock



grailInPlaceBufferReset :: [a] -> Int -> Int -> Int -> [a]
grailInPlaceBufferReset list start length bufferOffset =
    let
        buffer = start + length - 1
        index = buffer - bufferOffset
    in
        grailInPlaceBufferReset' list start length bufferOffset buffer index

grailInPlaceBufferReset' :: [a] -> Int -> Int -> Int -> Int -> Int -> [a]
grailInPlaceBufferReset' list start length bufferOffset buffer index
    | buffer >= start =
            grailInPlaceBufferReset' (grailSwap list index buffer) start length bufferOffset (buffer-1) (index-1)
    | otherwise = list



grailOutOfPlaceBufferReset :: [a] -> Int -> Int -> Int -> [a]
grailOutOfPlaceBufferReset list start length bufferOffset =
    let
        buffer = start + length - 1
        index = buffer - bufferOffset
    in
        grailOutOfPlaceBufferReset' list start length bufferOffset buffer index

grailOutOfPlaceBufferReset' :: [a] -> Int -> Int -> Int -> Int -> Int -> [a]
grailOutOfPlaceBufferReset' list start length bufferOffset buffer index
    | buffer >= start =
        let
            a = list !! index
            list' = take buffer list ++ [a] ++ drop (buffer + 1) list
        in
            grailOutOfPlaceBufferReset' list' start length bufferOffset (buffer-1) (index-1)
    | otherwise = list



grailInPlaceBufferRewind :: [a] -> Int -> Int -> Int -> [a]
grailInPlaceBufferRewind list start leftBlock buffer
    | leftBlock >= start =
        let
            list' = grailSwap list buffer leftBlock
        in
            grailInPlaceBufferRewind list' start (leftBlock-1) (buffer - 1)
    | otherwise = list



grailOutOfPlaceBufferRewind :: [a] -> Int -> Int -> Int -> [a]
grailOutOfPlaceBufferRewind list start leftBlock buffer
    | leftBlock >= start =
        let
            a = list !! leftBlock
            list' = take buffer list ++ [a] ++ drop (buffer + 1) list
        in
            grailOutOfPlaceBufferRewind list' start (leftBlock-1) (buffer - 1)
    | otherwise = list



grailGetSublist :: Ord a => [a] -> Int -> Int -> Sublist
grailGetSublist list currentKey medianKey =
    let
        a = list !! currentKey
        b = list !! medianKey
    in
        if a < b
        then LEFT
        else RIGHT



grailCountLastMergeBlocks :: Ord a => [a] -> Int -> Int -> Int -> Int
grailCountLastMergeBlocks list offset blockCount blockLen =
    let
        blocksToMerge = 0
        lastRightFrag = offset + (blockCount * blockLen)
        prevLeftBlock = lastRightFrag - blockLen
    in grailCountLastMergeBlocks' list offset blockCount blockLen blocksToMerge lastRightFrag prevLeftBlock

grailCountLastMergeBlocks' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int
grailCountLastMergeBlocks' list offset blockCount blockLen blocksToMerge lastRightFrag prevLeftBlock
    | blocksToMerge < blockCount && lRFval < pLBval =
        grailCountLastMergeBlocks' list offset blockCount blockLen (blocksToMerge+1) lastRightFrag (prevLeftBlock - blockLen)
    | otherwise = blocksToMerge
    where
        lRFval = list !! lastRightFrag
        pLBval = list !! prevLeftBlock



grailSmartMerge :: Ord a => [a] -> Int -> Int -> Sublist -> Int -> Int -> Int -> Sublist -> ([a], Int, Sublist)
grailSmartMerge list start leftLen leftOrigin rightLen bufferOffset currBlockLen currBlockOrigin =
    let
        left = start
        middle = start + leftLen
        right = middle
        end = middle + rightLen
        buffer = start - bufferOffset
    in
        if leftOrigin == LEFT
        then grailSmartMerge' list start leftLen leftOrigin rightLen bufferOffset currBlockLen currBlockOrigin left middle right end buffer (<=)
        else grailSmartMerge' list start leftLen leftOrigin rightLen bufferOffset currBlockLen currBlockOrigin left middle right end buffer (<)

grailSmartMerge' :: [a] -> Int -> Int -> Sublist -> Int -> Int -> Int -> Sublist -> Int -> Int -> Int -> Int -> Int -> (a -> a -> Bool) -> ([a], Int, Sublist)
grailSmartMerge' list start leftLen leftOrigin rightLen bufferOffset currBlockLen currBlockOrigin left middle right end buffer op
    | left < middle && right < end = let
        (list', left', right')
            | (list !! left) `op` (list !! right) = (grailSwap list buffer left,  left + 1, right)
            | otherwise                             = (grailSwap list buffer right, left, right + 1)
        buffer' = buffer + 1
        in grailSmartMerge' list' start leftLen leftOrigin rightLen bufferOffset currBlockLen currBlockOrigin left' middle right' end buffer' op
    | otherwise =
        if left < middle
        then let
            currBlockLen' = middle - left
            list' = grailInPlaceBufferRewind list left (middle - 1) (end - 1)
            in (list', currBlockLen', currBlockOrigin)
        else let currBlockLen' = end - right
            in if leftOrigin == LEFT
                then (list, currBlockLen', RIGHT)
                else (list, currBlockLen', LEFT)



grailSmartLazyMerge :: Ord a => [a] -> Int -> Int -> Sublist -> Int -> Int -> Sublist -> ([a], Int, Sublist)
grailSmartLazyMerge list start leftLen leftOrigin rightLen currBlockLen currBlockOrigin
    | leftOrigin == LEFT && list!!(middle-1) >  list!!middle = grailSmartLazyMergeLeft list start leftLen leftOrigin rightLen middle currBlockLen currBlockOrigin
    | leftOrigin /= LEFT && list!!(middle-1) >= list!!middle = grailSmartLazyMergeRight list start leftLen leftOrigin rightLen middle currBlockLen currBlockOrigin
    | otherwise = grailSmartLazyMerge' list leftOrigin rightLen
    where
        middle = start + leftLen

grailSmartLazyMergeLeft :: Ord a => [a] -> Int -> Int -> Sublist -> Int -> Int -> Int -> Sublist -> ([a], Int, Sublist)
grailSmartLazyMergeLeft list start leftLen leftOrigin rightLen middle currBlockLen currBlockOrigin
    | leftLen /= 0 =
        let
            mergeLen = grailBinarySearchLeft list middle rightLen (list!!start)
            (list', start', rightLen', middle') = if mergeLen /= 0
                then (grailRotate list start leftLen mergeLen, start + mergeLen, rightLen - mergeLen, middle + mergeLen)
                else (list, start, rightLen, middle)
        in
            if rightLen == 0
            then (list', leftLen, currBlockOrigin)
            else let
                midVal = list!!middle
                (start'', leftLen') = until (\(s, lL)-> not (lL /= 0 && list!!s <= midVal))
                        (\(s, lL) -> (s+1, lL -1)) (start'+1, leftLen-1)
                in
                    grailSmartLazyMergeLeft list' start'' leftLen' leftOrigin rightLen' middle' currBlockLen currBlockOrigin
    | otherwise = grailSmartLazyMerge' list leftOrigin rightLen

grailSmartLazyMergeRight :: Ord a => [a] -> Int -> Int -> Sublist -> Int -> Int -> Int -> Sublist -> ([a], Int, Sublist)
grailSmartLazyMergeRight list start leftLen leftOrigin rightLen middle currBlockLen currBlockOrigin
    | leftLen /= 0 =
        let
            mergeLen = grailBinarySearchRight list middle rightLen (list!!start)
            (list', start', rightLen', middle') = if mergeLen /= 0
                then (grailRotate list start leftLen mergeLen, start + mergeLen, rightLen - mergeLen, middle + mergeLen)
                else (list, start, rightLen, middle)
        in
            if rightLen == 0
            then (list', leftLen, currBlockOrigin)
            else let (start'', leftLen') = until (\(s, lL)-> not (lL /= 0 && list!!s < list!!lL))
                        (\(s, lL) -> (s+1, lL -1)) (start'+1, leftLen-1)
                in
                    grailSmartLazyMergeRight list' start'' leftLen' leftOrigin rightLen' middle' currBlockLen currBlockOrigin
    | otherwise = grailSmartLazyMerge' list leftOrigin rightLen

grailSmartLazyMerge' :: [a] -> Sublist -> Int -> ([a], Int, Sublist)
grailSmartLazyMerge' list leftOrigin rightLen = (list, currBlockLen, currBlockOrigin)
    where
        currBlockLen = rightLen
        currBlockOrigin
            | leftOrigin == LEFT = RIGHT
            | otherwise          = LEFT

grailSmartMergeOutOfPlace :: Ord a => [a] -> Int -> Int -> Sublist -> Int -> Int -> ([a], Int, Sublist)
grailSmartMergeOutOfPlace list start leftLen leftOrigin rightLen bufferOffset = let
    left = start
    middle = start + leftLen
    right = middle
    end = middle + rightLen
    buffer = start - bufferOffset
    in if leftOrigin == LEFT
    then grailSmartMergeOutOfPlace' list start leftLen leftOrigin rightLen bufferOffset left middle right end buffer (<=)
    else grailSmartMergeOutOfPlace' list start leftLen leftOrigin rightLen bufferOffset left middle right end buffer (<)

grailSmartMergeOutOfPlace' :: [a] -> Int -> Int -> Sublist -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (a -> a -> Bool) -> ([a], Int, Sublist)
grailSmartMergeOutOfPlace' list start leftLen leftOrigin rightLen bufferOffset left middle right end buffer op
    | left < middle && right < end = let
        buffer' = buffer + 1
        (list', left', right')
            | (list !! left) `op` (list !! right) = (grailSwap list buffer left, left + 1, right)
            | otherwise                             = (grailSwap list buffer right, left, right + 1)
        in
            grailSmartMergeOutOfPlace' list' start leftLen leftOrigin rightLen bufferOffset left' middle right' end buffer' op
    | otherwise = if left < middle
        then
            let list' = grailOutOfPlaceBufferRewind list left (middle - 1) (end - 1)
            in (list', middle - left, leftOrigin)
        else let cBLen = end - right
            in if leftOrigin == LEFT
                then (list, cBLen, RIGHT)
                else (list, cBLen, LEFT)



grailMergeBlocks :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [a]
grailMergeBlocks list firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen =
    let
        nextBlock = start + blockLen
        currBlockLen = blockLen
        currBlockOrigin = grailGetSublist list firstKey medianKey
    in
        grailMergeBlocks' list firstKey medianKey blockCount blockLen lastMergeBlocks lastLen nextBlock currBlockLen currBlockOrigin 1

grailMergeBlocks' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Sublist -> Int -> [a]
grailMergeBlocks' list firstKey medianKey blockCount blockLen lastMergeBlocks lastLen nextBlock currBlockLen currBlockOrigin keyIndex
    | keyIndex < blockCount =
        let
            currBlock = nextBlock - currBlockLen
            nextBlockOrigin = grailGetSublist list (firstKey + keyIndex) medianKey
            buffer = currBlock - blockLen
            (list', currBlockLen', currBlockOrigin')
                | nextBlockOrigin == currBlockOrigin =  (grailBlockSwap list buffer currBlock currBlockLen, blockLen, currBlockOrigin)
                | otherwise = grailSmartMerge list currBlock currBlockLen currBlockOrigin blockLen blockLen currBlockLen currBlockOrigin
        in
            grailMergeBlocks' list' firstKey medianKey blockCount blockLen lastMergeBlocks lastLen (nextBlock+blockLen) currBlockLen' currBlockOrigin' (keyIndex+1)
    | otherwise =
        let
            currBlock = nextBlock - currBlockLen
            buffer = currBlock - blockLen
            list' = grailBlockSwap list buffer currBlock currBlockLen
            (list'', currBlock', currBlockLen')
                | currBlockOrigin == RIGHT = (list', nextBlock, blockLen * lastMergeBlocks)
                | otherwise = (list, currBlock, currBlockLen + blockLen * lastMergeBlocks)
        in
            if lastLen /= 0
            then grailMergeForwards list'' currBlock' currBlockLen' lastLen blockLen
            else grailBlockSwap list buffer currBlock currBlockLen



grailLazyMergeBlocks :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [a]
grailLazyMergeBlocks list firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen =
    let
        nextBlock = start + blockLen
        currBlockLen = blockLen
        currBlockOrigin = grailGetSublist list firstKey medianKey
    in
        grailLazyMergeBlocks' list firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen nextBlock currBlockLen currBlockOrigin 1

grailLazyMergeBlocks' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Sublist -> Int -> [a]
grailLazyMergeBlocks' list firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen nextBlock currBlockLen currBlockOrigin keyIndex
    | keyIndex < blockCount =
        let
            currBlock = nextBlock - currBlockLen
            nextBlockOrigin = grailGetSublist list (firstKey + keyIndex) medianKey
            (list', currBlockLen', currBlockOrigin')
                | nextBlockOrigin == currBlockOrigin = (list, blockLen, currBlockOrigin)
                | otherwise = grailSmartLazyMerge list currBlock currBlockLen currBlockOrigin blockLen currBlockLen currBlockOrigin
        in
            grailLazyMergeBlocks' list' firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen (nextBlock + blockLen) currBlockLen' currBlockOrigin' (keyIndex + 1)
    | otherwise =
        let
            currBlock = nextBlock - currBlockLen
            (currBlock', currBlockLen')
                    | currBlockOrigin == RIGHT = (nextBlock, blockLen * lastMergeBlocks)
                    | otherwise = (currBlock, currBlockLen + blockLen * lastMergeBlocks)
        in
            if lastLen /= 0
            then grailLazyMerge list currBlock' currBlockLen' lastLen
            else list



grailMergeBlocksOutOfPlace :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [a]
grailMergeBlocksOutOfPlace list firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen = let
    nextBlock = start + blockLen
    currBlockLen = blockLen
    currBlockOrigin = grailGetSublist list firstKey medianKey
    in
        grailMergeBlocksOutOfPlace' list firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen nextBlock currBlockLen currBlockOrigin 1

grailMergeBlocksOutOfPlace' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Sublist -> Int -> [a]
grailMergeBlocksOutOfPlace' list firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen nextBlock currBlockLen currBlockOrigin keyIndex
    | keyIndex < blockCount = let
        currBlock = nextBlock - currBlockLen
        nextBlockOrigin = grailGetSublist list (firstKey + keyIndex) medianKey
        in if nextBlockOrigin == currBlockOrigin
            then let
                buffer = currBlock - blockLen
                list' = listCopy list currBlock list buffer currBlockLen
                currBlockLen' = blockLen
                in
                grailMergeBlocksOutOfPlace' list' firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen (nextBlock + blockLen) currBlockLen' currBlockOrigin (keyIndex + 1)
            else let
                (list', cBLen, cBOrigin) = grailSmartMergeOutOfPlace list currBlock currBlockLen currBlockOrigin blockLen blockLen
                in
                grailMergeBlocksOutOfPlace' list' firstKey medianKey start blockCount blockLen lastMergeBlocks lastLen (nextBlock + blockLen) cBLen cBOrigin (keyIndex + 1)
    | otherwise = let
        currBlock = nextBlock - currBlockLen
        buffer = currBlock - blockLen
        in if lastLen /= 0
            then if currBlockOrigin == RIGHT
                then let
                    list' = listCopy list currBlock list buffer currBlockLen
                    currBlock' = nextBlock
                    cBLen = blockLen * lastMergeBlocks
                    --cBOrigin = LEFT
                    in
                    grailMergeOutOfPlace list' currBlock' cBLen lastLen blockLen --(list'', cBLen, cBOrigin)
                else let
                    cBLen = currBlockLen + blockLen * lastMergeBlocks
                    in
                    grailMergeOutOfPlace list currBlock cBLen lastLen blockLen --(list', cBLen, currBlockOrigin)
            else listCopy list currBlock list buffer currBlockLen--(list', currBlockLen, currBlockOrigin)



grailCombineInPlace :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool -> [a]
grailCombineInPlace list firstKey start length sublistLen blockLen mergeCount lastSublists buffer =
    let
        fullMerge = 2 * sublistLen
        blockCount = fullMerge `quot` blockLen
    in
        grailCombineInPlace' list firstKey start length sublistLen blockLen mergeCount lastSublists buffer fullMerge blockCount 0

grailCombineInPlace' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool -> Int -> Int -> Int -> [a]
grailCombineInPlace' list firstKey start length sublistLen blockLen mergeCount lastSublists buffer fullMerge blockCount mergeIndex
    | mergeIndex < mergeCount = let
        offset = start + (mergeIndex * fullMerge)
        list' = grailInsertSort list firstKey blockCount
        medianKey = sublistLen `quot` blockLen
        (list'', medianKey') = grailBlockSelectSort list' firstKey offset medianKey blockCount blockLen

        list'''
            | buffer    = grailMergeBlocks list'' firstKey (firstKey + medianKey') offset blockCount blockLen 0 0
            | otherwise = grailLazyMergeBlocks list'' firstKey (firstKey + medianKey') offset blockCount blockLen 0 0
        in
            grailCombineInPlace' list''' firstKey start length sublistLen blockLen mergeCount lastSublists buffer fullMerge blockCount (mergeIndex + 1)
    | otherwise = grailCombineInPlace'' list firstKey start length sublistLen blockLen mergeCount lastSublists buffer fullMerge

grailCombineInPlace'' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool -> Int -> [a]
grailCombineInPlace'' list firstKey start length sublistLen blockLen mergeCount lastSublists buffer fullMerge =
    let
        offset = start + mergeCount * fullMerge
        blockCount = lastSublists `quot` blockLen

        list' = grailInsertSort list firstKey (blockCount +1)
        medianKey = sublistLen `quot` blockLen

        (list'', medianKey') = grailBlockSelectSort list' firstKey offset medianKey blockCount blockLen

        lastFragment = lastSublists - blockCount * blockLen
        lastMergeBlocks
            | lastFragment /= 0 = grailCountLastMergeBlocks list'' offset blockCount blockLen
            | otherwise = 0
        smartMerges = blockCount - lastMergeBlocks
        leftLen = lastMergeBlocks * blockLen
        list'''
          | smartMerges == 0 && buffer = grailMergeForwards list'' offset leftLen lastFragment blockLen
          | smartMerges == 0           = grailLazyMerge list'' offset leftLen lastFragment
          | buffer    = grailMergeBlocks list'' firstKey (firstKey + medianKey') offset smartMerges blockLen lastMergeBlocks lastFragment
          | otherwise = grailLazyMergeBlocks list'' firstKey (firstKey + medianKey') offset smartMerges blockLen lastMergeBlocks lastFragment
    in
        if lastSublists /= 0
        then if buffer
            then grailInPlaceBufferReset list''' start length blockLen
            else list'''
        else if buffer
            then grailInPlaceBufferReset list start length blockLen
            else list



grailCombineOutOfPlace :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [a] -> t -> ([a], Maybe [a])
grailCombineOutOfPlace list firstKey start length sublistLen blockLen mergeCount lastSublists extBuffer extBufferLen = --(list, extBuffer)
    let
        extBuffer' = listCopy list (start - blockLen) extBuffer 0 blockLen
        fullMerge = 2 * sublistLen
        blockCount = fullMerge `quot` blockLen
    in grailCombineOutOfPlace' list firstKey start length sublistLen blockLen mergeCount lastSublists extBuffer' extBufferLen fullMerge blockCount 0

grailCombineOutOfPlace' :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [a] -> t -> Int -> Int -> Int -> ([a], Maybe [a])
grailCombineOutOfPlace' list firstKey start length sublistLen blockLen mergeCount lastSublists extBuffer extBufferLen fullMerge blockCount mergeIndex
    | mergeIndex < mergeCount = let
            offset = start + mergeIndex * fullMerge
            list' = grailInsertSort list firstKey blockCount
            medianKey = sublistLen `quot` blockLen
            (list'', medianKey') = grailBlockSelectSort list' firstKey offset medianKey blockCount blockLen
            list''' = grailMergeBlocksOutOfPlace list'' firstKey (firstKey + medianKey') offset blockCount blockLen 0 0
        in
            grailCombineOutOfPlace' list''' firstKey start length sublistLen blockLen mergeCount lastSublists extBuffer extBufferLen fullMerge blockCount (mergeIndex + 1)
    | lastSublists /= 0 = let
            offset = start + mergeCount * fullMerge
            blockCount' = lastSublists `quot` blockLen
            list' = grailInsertSort list firstKey (blockCount' + 1)

            medianKey = sublistLen `quot` blockLen
            (list'', medianKey') = grailBlockSelectSort list' firstKey offset medianKey blockCount' blockLen
            lastFragment = lastSublists - blockCount' * blockLen

            lastMergeBlocks
                | lastFragment /= 0 = grailCountLastMergeBlocks list'' offset blockCount' blockLen
                | otherwise = 0
            smartMerges = blockCount' - lastMergeBlocks

            list'''
                | smartMerges == 0 = grailMergeOutOfPlace list'' offset (lastMergeBlocks * blockLen) lastFragment blockLen
                | otherwise = grailMergeBlocksOutOfPlace list'' firstKey (firstKey + medianKey') offset smartMerges blockLen lastMergeBlocks lastFragment
            in grailCombineOutOfPlace'' list''' start length blockLen extBuffer

    | otherwise = grailCombineOutOfPlace'' list  start length  blockLen extBuffer

grailCombineOutOfPlace'' :: [a] -> Int -> Int -> Int -> [a] -> ([a], Maybe [a])
grailCombineOutOfPlace'' list start length blockLen extBuffer =
    let
        list'  = grailOutOfPlaceBufferReset list start length blockLen
        list'' = listCopy extBuffer 0 list' (start - blockLen) blockLen
    in
        (list'', Just extBuffer)



grailCombineBlocks :: Ord a => [a] -> Int -> Int -> Int -> Int -> Int -> Bool -> Maybe [a] -> Int -> ([a], Maybe [a])
grailCombineBlocks list firstKey start length sublistLen blockLen buffer extBuffer extBufferLen
    | buffer && blockLen <= extBufferLen = grailCombineOutOfPlace list firstKey start length' sublistLen blockLen mergeCount lastSublists' (fromJust extBuffer) extBufferLen
    | otherwise = (grailCombineInPlace list firstKey start length' sublistLen blockLen mergeCount lastSublists' buffer, Nothing)
    where
        fullMerge = 2 * sublistLen
        mergeCount = length `quot` fullMerge
        lastSublists = length - fullMerge * mergeCount
        (length', lastSublists')
            | lastSublists <= sublistLen = (length - lastSublists, 0)
            | otherwise = (length, lastSublists)



grailLazyMerge :: Ord a => [a] -> Int -> Int -> Int -> [a]
grailLazyMerge list start leftLen rightLen
    | leftLen < rightLen = let
            middle = start + leftLen
            in grailLazyMergeLeft list start leftLen rightLen middle
    | otherwise = let
            end = start + leftLen + rightLen - 1
            in grailLazyMergeRight list start leftLen rightLen end

grailLazyMergeLeft :: Ord a => [a] -> Int -> Int -> Int -> Int -> [a]
grailLazyMergeLeft list start leftLen rightLen middle
    | leftLen /= 0 = let
        mergeLen = grailBinarySearchLeft list middle rightLen (list!!start)
        list'
            | mergeLen /= 0 = grailRotate list start leftLen mergeLen
            | otherwise     = list
        (start', rightLen', middle')
            | mergeLen /= 0 = (start + mergeLen, rightLen - mergeLen, middle + mergeLen)
            | otherwise     = (start, rightLen, middle)
        (start'', leftLen') = until (\(st,ll) -> not (ll /= 0 && (list'!!st) <= (list'!!middle')))
            (\(st,ll) -> (st+1,ll-1)) (start' +1, leftLen -1)
        in
            if rightLen' == 0
            then list'
            else grailLazyMergeLeft list' start'' leftLen' rightLen' middle'
    | otherwise = list

grailLazyMergeRight :: Ord a => [a] -> Int -> Int -> Int -> Int -> [a]
grailLazyMergeRight list start leftLen rightLen end
    | rightLen /= 0 = let
        mergeLen = grailBinarySearchRight list start leftLen (list!!end)
        list'
            | mergeLen /= leftLen = grailRotate list (start + mergeLen) (leftLen - mergeLen) rightLen
            | otherwise           = list
        (end', leftLen')
            | mergeLen /= leftLen = (end - (leftLen - mergeLen), mergeLen)
            | otherwise           = (end, leftLen)
        middle = start + leftLen'
        (rightLen', end'') = until (\(rl,en) -> not (rl /= 0 && (list'!!(middle-1)) <= (list'!!en))) (\(rl,en) -> (rl-1,en-1)) (rightLen -1, end' -1)
        in
            if leftLen' == 0
            then list'
            else grailLazyMergeRight list' start leftLen' rightLen' end''
    | otherwise = list



grailLazyStableSort :: Ord a => [a] -> Int -> Int -> [a]
grailLazyStableSort list start length =
    let
        list' = take start list ++ grailLazyStableSort' (drop start list) length --grailLazyStableSort' list start length 1
        mergeLen = 2
    in
        grailLazyStableSort'' list' start length mergeLen

grailLazyStableSort' :: Ord a => [a] -> Int -> [a] --Int -> Int -> [a]
grailLazyStableSort' list@(a:b:t) length --list start length index
    | length > 1 =
      --  let
      --      left = start + index -1
      --      right = start + index
      --  in
            if a > b--(list !! left) > (list !! right)
            then b:a:grailLazyStableSort' t (length - 2) --grailLazyStableSort' (grailSwap list left right) start length (index + 2)
            else a:b:grailLazyStableSort' t (length - 2) --grailLazyStableSort' list start length (index + 2)
    | otherwise = list

grailLazyStableSort'' :: Ord a => [a] -> Int -> Int -> Int -> [a]
grailLazyStableSort'' list start length mergeLen
    | mergeLen < length =
        let
            fullMerge = 2 * mergeLen
            mergeEnd = length - fullMerge
            (mergeIndex, list') = until (\(index, _) -> index > mergeEnd) (\(index, arr) ->
                (index + fullMerge, grailLazyMerge arr (start + index) mergeLen mergeLen)) (0, list)
            leftOver = length - mergeIndex
            list'' = grailLazyMerge list' (start + mergeIndex) mergeLen (leftOver - mergeLen)
        in
            if leftOver > mergeLen
            then grailLazyStableSort'' list'' start length (mergeLen * 2)
            else grailLazyStableSort'' list' start length (mergeLen * 2)
    | otherwise = list



grailCommonSort :: Ord a => [a] -> Int -> Int -> Maybe [a] -> Int -> [a]
grailCommonSort list start length extBuffer extBufferLen
    | length < 16 = grailInsertSort list start length
    | keysFound < idealKeys = if keysFound < 4
        then grailLazyStableSort list start length
        else grailCommonSort' list' start length extBuffer extBufferLen blockLen' keyLen' False
    | otherwise = grailCommonSort' list' start length extBuffer extBufferLen blockLen  keyLen  True
        where
            blockLen = until (\x -> x*x >= length) (*2) 1
            keyLen = ((length - 1) `quot` blockLen) + 1
            idealKeys = keyLen + blockLen
            (keysFound, list') = grailCollectKeys list start length idealKeys
            keyLen' = until (<= keysFound) (`quot` 2) blockLen
            blockLen' = 0

grailCommonSort' :: Ord a => [a] -> Int -> Int -> Maybe [a] -> Int -> Int -> Int -> Bool -> [a]
grailCommonSort' list start length extBuffer extBufferLen blockLen keyLen idealBuffer =
    let
        bufferEnd = blockLen + keyLen
        sublistLen
            | idealBuffer = blockLen
            | otherwise   = keyLen
        (extBuffer', extBufferLen') = if idealBuffer && isJust extBuffer
            then (extBuffer, extBufferLen)
            else (Nothing, 0)
        (list', extBuffer'') = grailBuildBlocks list (start + bufferEnd) (length - bufferEnd) sublistLen extBuffer' extBufferLen'
    in
        grailCommonSort'' list' start length extBuffer'' extBufferLen' blockLen keyLen idealBuffer bufferEnd sublistLen

grailCommonSort'' :: Ord a => [a] -> Int -> Int -> Maybe [a] -> Int -> Int -> Int -> Bool -> Int -> Int -> [a]
grailCommonSort'' list start length extBuffer extBufferLen blockLen keyLen idealBuffer bufferEnd sublistLen
    | length - bufferEnd > 2 * sublistLen =
        let
            sublistLen' = sublistLen * 2
            currentBlockLen = blockLen
            scrollingBuffer = idealBuffer
            keyBuffer = keyLen `quot` 2
            (currentBlockLen', scrollingBuffer')
                | idealBuffer                                       = (currentBlockLen, scrollingBuffer)
                | keyBuffer >= ((2 * sublistLen') `quot` keyBuffer) = (keyBuffer, True)
                | otherwise                                         = ((2 * sublistLen') `quot` keyLen, scrollingBuffer)
            (list', extBuffer') =  grailCombineBlocks list start (start + bufferEnd) (length - bufferEnd) sublistLen' currentBlockLen' scrollingBuffer' extBuffer extBufferLen
        in
            grailCommonSort'' list' start length extBuffer' extBufferLen blockLen keyLen idealBuffer bufferEnd sublistLen'
    | otherwise = grailLazyMerge (grailInsertSort list start bufferEnd) start bufferEnd (length - bufferEnd)



-- area of functions one would consider public in other languages

-- no external buffer used
grailSortInPlace :: Ord a => [a] -> Int -> Int -> [a]
grailSortInPlace list start length
    | null list = []
    | otherwise =
    let extBuffer = Nothing
        extBufferLen = 0
    in grailCommonSort list start length extBuffer extBufferLen

-- fixed-length external buffer used
grailSortStaticOOP :: Ord a => [a] -> Int -> Int -> [a]
grailSortStaticOOP list start length
  | null list = []
  | otherwise =
    let var = head list
        extBufferLen = 512
        extBuffer = replicate extBufferLen var
    in grailCommonSort list start length (Just extBuffer) extBufferLen

-- external buffer with length of smallest power of 2 larger than square root of input's length used 
grailSortDynamicOOP :: Ord a => [a] -> Int -> Int -> [a]
grailSortDynamicOOP list start length
  | null list = []
  | otherwise =
    let var = head list
        extBufferLen = until (\x -> x * x >= length) (*2) 1
        extBuffer = replicate extBufferLen var
    in grailCommonSort list start length (Just extBuffer) extBufferLen
