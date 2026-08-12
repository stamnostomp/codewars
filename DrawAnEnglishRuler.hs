{-
Draw an English Ruler (ASCII)

A standard English ruler is marked in inches. For each whole inch, the ruler shows a major tick with a numeric label. Each time the interval size is halved, the tick length decreases by 1. The length of the major tick t defines the scale of the ruler.

Write a function that generates the ASCII drawing of a ruler.

    t - major tick length
    n - number of inches on the ruler

-}

-- | https://www.codewars.com/kata/696fe8b06b4e2e6ddb50caa8
module DrawAnEnglishRuler where
