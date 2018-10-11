import qualified Data.List as List

main =
  let toY = y 2.0 4.5
      xRange = createRange (-15) 15 0.001
   in putStrLn . show $
      roots xRange $ indexOfNonZero $ diff $ sign $ map toY xRange

y :: Float -> Float -> Float -> Float
y m c x = m * x + c

createRange :: Float -> Float -> Float -> [Float]
createRange start end step = [start,(start + step) .. end]

sign :: [Float] -> [Int]
sign = foldl signEach []
  where
    signEach xs x = (signItem x) : xs
    signItem x
      | x < 0 = -1
      | x > 0 = 1
      | otherwise = 0

diff :: [Int] -> [Int]
diff [] = []
diff xs = zipWith (-) (tail xs) xs

indexOfNonZero :: [Int] -> [Int]
indexOfNonZero = List.findIndices (/= 0)

roots :: [Float] -> [Int] -> (Float, Float)
roots xs [] = error "Line does not intesect x axis in given range"
roots xs (i:_) = (xs `at` i, xs `at` (i + 1))
  where
    at = (!!)
