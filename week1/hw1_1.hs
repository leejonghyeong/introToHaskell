    -- Validate Credit Card Number
    
    reverse' :: [a]->[a]
    reverse' [] = []
    reverse' (x:xs) = reverse' xs ++ [x]

    toDigits, toDigitsRev :: Integer -> [Integer]
    toDigitsRev n
        | n <= 0 = []
        | otherwise = n `rem` 10 : toDigitsRev (n `div` 10)

    toDigits = reverse' . toDigitsRev

    -- [8,7,6,5] == [16,7,12,5]
    -- [1,2,3] == [1,4,3]
    doubleEveryOther :: [Integer] -> [Integer]
    doubleEveryOther [] = []
    doubleEveryOther (x:xs) 
        | odd. length$ xs =  x*2 : doubleEveryOther xs
        | otherwise = x : doubleEveryOther xs

    --below function has less complexity
    doubleEveryOther' :: [Integer] -> [Integer]
    doubleEveryOther' xs = [if i `mod` 2 == len `mod` 2 then x*2 else x | i<-[0..len-1], let x = (xs !! i)]
        where len = length xs

    sumDigits :: [Integer] -> Integer
    sumDigits [] = 0
    sumDigits (x: xs) = sum x + sumDigits xs
        where sum x = x `div` 10 + x `rem` 10

    validate :: Integer -> Bool
    validate = (\x-> x `rem` 10 == 0). sumDigits. doubleEveryOther'. toDigits
    testNum = 4012888888881881