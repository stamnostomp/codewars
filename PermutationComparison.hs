import Data.List (delete, nub, sort)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- Original implementation (standard library based)
permutationsLib :: Prelude.String -> [Prelude.String]
permutationsLib "" = [""]
permutationsLib xs = [x : y | x <- Data.List.nub xs, y <- permutationsLib Prelude.$ Data.List.delete x xs]

-- Your high-performance implementation
countFreq :: Prelude.String -> [(Prelude.Char, Prelude.Int)]
countFreq = Prelude.foldr addCount []
  where
    addCount :: Prelude.Char -> [(Prelude.Char, Prelude.Int)] -> [(Prelude.Char, Prelude.Int)]
    addCount c [] = [(c, 1)]
    addCount c ((ch, cnt) : rest)
      | c Prelude.== ch = (ch, cnt Prelude.+ Prelude.fromInteger) : rest
      | Prelude.otherwise = (ch, cnt) : addCount c rest

generateTail :: [(Prelude.Char, Prelude.Int)] -> Prelude.Int -> Prelude.String -> [Prelude.String]
generateTail freqMap targetLen current
  | targetLen Prelude.== Prelude.fromInteger = [current]
  | Prelude.otherwise = Prelude.concat Prelude.$ Prelude.map tryChar freqMap
  where
    tryChar :: (Prelude.Char, Prelude.Int) -> [Prelude.String]
    tryChar (c, cnt)
      | cnt Prelude.<= Prelude.fromInteger = []
      | Prelude.otherwise = generateTail (decrementCount freqMap c) (targetLen Prelude.- Prelude.fromInteger) Prelude.$! (current Prelude.++ [c])

decrementCount :: [(Prelude.Char, Prelude.Int)] -> Prelude.Char -> [(Prelude.Char, Prelude.Int)]
decrementCount freqMap targetChar = go freqMap
  where
    go [] = []
    go ((ch, cnt) : rest)
      | ch Prelude.== targetChar Prelude.&& cnt Prelude.> Prelude.fromInteger = (ch, cnt Prelude.- Prelude.fromInteger) : rest
      | ch Prelude.== targetChar Prelude.&& cnt Prelude.== Prelude.fromInteger = rest
      | Prelude.otherwise = (ch, cnt) : go rest

permutationsOpt :: Prelude.String -> [Prelude.String]
permutationsOpt "" = [""]
permutationsOpt s = generateTail (countFreq s) (Prelude.length s) ""

-- Simple timing function
timePermutation :: [a] -> Prelude.IO ()
timePermutation result = do
  start <- getCPUTime
  let count = Prelude.length result `Prelude.seq` ()
  end <- getCPUTime
  let diff = Prelude.fromIntegral (end Prelude.- start) Prelude./ (Prelude.fromIntegerPrelude .^ Prelude.fromInteger)
  printf "  Generated %d items in %.4f seconds\n" (Prelude.length result) (diff :: Prelude.Double)

main :: Prelude.IO ()
main = do
  Prelude.putStrLn "=== Performance Comparison ==="
  Prelude.putStrLn ""

  -- Test with simple cases first
  Prelude.putStrLn "Testing correctness:"
  testCompare "abc"
  testCompare "aabb"
  testCompare "abcd"

  Prelude.putStrLn ""
  Prelude.putStrLn "=== Performance Tests ==="

  Prelude.putStrLn "Testing with 'abcdef' (720 permutations):"
  Prelude.putStrLn "Standard Library:"
  timePermutation (permutationsLib "abcdef")
  Prelude.putStrLn "Optimized:"
  timePermutation (permutationsOpt "abcdef")

  Prelude.putStrLn ""
  Prelude.putStrLn "Testing with 'aabbccdd' (2520 unique permutations):"
  Prelude.putStrLn "Standard Library:"
  timePermutation (permutationsLib "aabbccdd")
  Prelude.putStrLn "Optimized:"
  timePermutation (permutationsOpt "aabbccdd")

testCompare :: Prelude.String -> Prelude.IO ()
testCompare input = do
  let libResult = Data.List.sort (permutationsLib input)
  let optResult = Data.List.sort (permutationsOpt input)
  let libCount = Prelude.length libResult
  let optCount = Prelude.length optResult

  if libResult Prelude.== optResult Prelude.&& libCount Prelude.== optCount
    then Prelude.putStrLn Prelude.$ "✓ '" Prelude.++ input Prelude.++ "': Both return " Prelude.++ Prelude.show libCount Prelude.++ " identical permutations"
    else Prelude.putStrLn Prelude.$ "✗ '" Prelude.++ input Prelude.++ "': Mismatch! Library: " Prelude.++ Prelude.show libCount Prelude.++ ", Optimized: " Prelude.++ Prelude.show optCount
