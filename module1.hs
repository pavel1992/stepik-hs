main = putStrLn "Hello, world!"

max5 = max 5

lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

sign x = if x > 0 then 1 else if x == 0 then 0 else (-1)

x |-| y = abs (x - y)

doubleFact n = if n == 0 || n == 1 then n else n * doubleFact (n - 2)

fibonacci n
  | n == 0 = 0
  | n == 1 = 1
  | n == (-1) = 1
  | n > 1 = fibonacci (n - 1) + fibonacci (n - 2)
  | n < (-1) = fibonacci (n + 2) - fibonacci (n + 1)
  | otherwise = undefined

helper first second n
  | n == 0 = 0
  | n == 1 = second
  | n == (-1) = second
  | n > 1 = helper second (first + second) (n - 1)
  | n < (-1) = helper second (first - second) (n + 1)
  | otherwise = undefined 
fibonacci1 :: Integer -> Integer
fibonacci1 n = helper 0 1 n

seqA :: Integer -> Integer
seqA n 
  | n >= 0 = let
    helper first second third 0 = first
    helper first second third 1 = second
    helper first second third 2 = third
    helper first second third n = helper second third (third + second - 2 * first) (n - 1)
  in helper 1 2 3 n


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x
  | div x 10 == 0 = (abs x, 1)
  | otherwise = helper (0, 1) (abs x)
    where
    helper (summ, count) x
      | div x 10 == 0 = (summ + abs x, count)
      | otherwise = helper (summ + mod x 10, count + 1) (div x 10)


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = helper 0 a f a b
  where
    sumRaw f a b curr summ
      | a == b = summ
      | curr > a && curr > b = summ
      | curr < a && curr < b = summ
      | otherwise = sumRaw f a b (getNextStep a b curr) (summ + f curr)
    getNextStep a b currStep
      | a > b = currStep - (a - b) / 1000
      | otherwise = currStep + (b - a) / 1000
    helper sum curr f a b = ((f a + f b) / 2 + (sumRaw f a b a 0)) * ((b - a) / 1000)
