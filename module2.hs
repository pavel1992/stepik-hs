      
on3 op f x y z = op (f x) (f y) (f z)

class Printable a where
    toString:: a -> [Char]

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
    toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

ip a b c d = show a ++ show b ++ show c ++ show d

class (Enum a, Bounded a, Eq a) => SafeEnum a where
    ssucc :: a -> a
    ssucc a
      |  a == maxBound = minBound
      | otherwise = succ a

    spred :: a -> a
    spred a
      | a == minBound = maxBound
      | otherwise = pred a

avg :: Int -> Int -> Int -> Double
avg x y z = (fromIntegral x + fromIntegral y + fromIntegral z) / 3