{--In this kata, your task is to create all permutations of a non-empty input string and remove duplicates, if present.

Create as many "shufflings" as you can!

Examples:

With input 'a':
Your function should return: ['a']

With input 'ab':
Your function should return ['ab', 'ba']

With input 'abc':
Your function should return ['abc','acb','bac','bca','cab','cba']

With input 'aabb':
Your function should return ['aabb', 'abab', 'abba', 'baab', 'baba', 'bbaa']

Note: The order of the permutations doesn't matter.

Good luck!
--}

-- | https://www.codewars.com/kata/5254ca2719453dcc0b00027d/train/haskell
module SoMeanyPermutations where

-- | Generate all unique permutations of a string
-- Uses frequency counting to avoid duplicate generation and tail recursion for performance
permutations :: String -> [String]
permutations "" = [""]
permutations s = generateTail (countFreq s) (length s) ""

-- | Count character frequencies in a string
countFreq :: String -> [(Char, Int)]
countFreq = foldr addCount []
  where
    addCount :: Char -> [(Char, Int)] -> [(Char, Int)]
    addCount c [] = [(c, 1)]
    addCount c ((ch, cnt):rest)
        | c == ch    = (ch, cnt + 1) : rest
        | otherwise  = (ch, cnt) : addCount c rest

-- | Tail-recursive permutation generator using frequency map
generateTail :: [(Char, Int)] -> Int -> String -> [String]
generateTail freqMap targetLen current
    | targetLen == 0 = [current]
    | otherwise = concat $ map tryChar freqMap
  where
    tryChar :: (Char, Int) -> [String]
    tryChar (c, cnt)
        | cnt <= 0    = []
        | otherwise   = generateTail (decrementCount freqMap c) (targetLen - 1) $! (current ++ [c])

-- | Decrement the count of a specific character in the frequency map
-- Remove entries that reach zero count
decrementCount :: [(Char, Int)] -> Char -> [(Char, Int)]
decrementCount freqMap targetChar = go freqMap
  where
    go [] = []
    go ((ch, cnt):rest)
        | ch == targetChar && cnt > 1 = (ch, cnt - 1) : rest
        | ch == targetChar && cnt == 1 = rest
        | otherwise                   = (ch, cnt) : go rest
