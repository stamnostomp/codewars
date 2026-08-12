findNextSquare :: Int -> Int
findNextSquare n = (nextRoot ^ 2)
  where
    nextRoot = ceiling (sqrt (fromIntegral n))
