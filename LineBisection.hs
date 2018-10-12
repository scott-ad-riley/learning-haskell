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
sign = map signItem
  where
    signItem x = toNum $ compare x 0
    toNum LT = -1
    toNum GT = 1
    toNum EQ = 0

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
