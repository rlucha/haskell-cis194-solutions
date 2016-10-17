toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise =  extractTens n : toDigitsRev (div n 10)

extractTens :: Integer -> Integer
extractTens n = rem n 10

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (ns) = reverse(zipWith (*) (cycle [2,1]) (reverse ns))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = extractDigits x + sumDigits xs

extractDigits :: Integer -> Integer
extractDigits x
    | x < 9 = x
    | otherwise = (div x 10) + (rem x 10)

validate :: Integer -> Bool
validate x
    | isValid = True
    | otherwise = False
    where isValid = (sumDigits . doubleEveryOther . toDigits) x `rem` 10 == 0
