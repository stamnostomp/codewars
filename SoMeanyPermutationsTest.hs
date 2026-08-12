import SoMeanyPermutations

main :: IO ()
main = do
  putStrLn "Testing permutations implementation:"
  putStrLn ""

  -- Test cases from the problem description
  test "a" ["a"]
  test "ab" ["ab", "ba"]
  test "abc" ["abc","acb","bac","bca","cab","cba"]
  test "aabb" ["aabb", "abab", "abba", "baab", "baba", "bbaa"]

  -- Additional test cases
  test "" [""]  -- edge case
  test "aa" ["aa"]  -- all same characters
  test "aba" ["aab","aba","baa"]  -- with duplicates

  putStrLn ""
  putStrLn "All tests completed!"

test :: String -> [String] -> IO ()
test input expected = do
  let result = permutations input
  let resultSorted = sort result
  let expectedSorted = sort expected
  if resultSorted == expectedSorted
    then putStrLn $ "✓ permutations \"" ++ input ++ "\" = " ++ show resultSorted
    else putStrLn $ "✗ FAILED: permutations \"" ++ input ++ "\" = " ++ show resultSorted ++ " (expected " ++ show expectedSorted ++ ")"

-- Simple sort function for comparison (avoiding Data.List import)
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y < x] ++ [x] ++ sort [y | y <- xs, y >= x]