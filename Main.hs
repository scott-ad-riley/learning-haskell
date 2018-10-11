main = putStrLn . show $ diffArray $ signArray $ map (line 2.0 4.5) xrange

xrange :: [Float]
xrange = createRange (-15) 0.01 (15)

line :: Float -> Float -> (Float -> Float)
line m c = y m c

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
diffReduce (x:xs) prev acc = diffReduce xs (x) ((prev - x) : acc)
