import Data.List

main =
  putStrLn . show $
  roots xrange $ intersectionIndex $ diffArray $ signArray $ map toY xrange
  where
    toY = y 2.0 4.5
    xrange = createRange (-15) 0.01 (15)

y :: Float -> Float -> Float -> Float
y m c x = m * x + c

createRange :: Float -> Float -> Float -> [Float]
createRange start step end = [start,(start + step) .. end]

signArray :: [Float] -> [Int]
signArray xs = signReduce xs []

signReduce :: [Float] -> [Int] -> [Int]
signReduce [] acc = acc
signReduce (x:xs) acc = signReduce xs ((sign x) : acc)

sign :: Float -> Int
sign x
  | x < 0 = (-1)
  | x > 0 = 1
  | otherwise = 0

diffArray :: [Int] -> [Int]
diffArray [] = []
diffArray (x:xs) = diffReduce xs x []

diffReduce :: [Int] -> Int -> [Int] -> [Int]
diffReduce [] _ acc = acc
diffReduce (curr:xs) prev acc = diffReduce xs curr ((prev - curr) : acc)

intersectionIndex :: [Int] -> [Int]
intersectionIndex xs = findIndices notZero xs

notZero :: Int -> Bool
notZero x = not (x == 0)

roots :: [Float] -> [Int] -> (Float, Float)
roots xs [] = error "Could not find the point of intersection"
roots xs (z:zs) = (xs !! z, xs !! (z + 1))
