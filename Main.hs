main = putStrLn . show $ signArray $ map (line 2.0 4.5) xrange

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
