{-
	sumThree a b c
	is a functon to add 3 integers
-}
sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

-- identic is the identity function for Bool values
identic :: Bool -> Bool
identic x = x

sumFour a b c d = a + b + c + d

-- myMax x y returns the largest number between x and y
myMax x y = if x <= y then y else x

mySum x = if x <= 0
            then 0
            else x + mySum (x - 1)

fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

{-
	(myGcd) is a function that returns the greates common divisor of a and b
-}
myGcd :: Integer -> Integer -> Integer
myGcd a b
 | a < 0 || b < 0 = myGcd (abs a) (abs b)
 | b == 0 = a
 | otherwise = myGcd b (a `mod` b)


doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

checkOdd xl = [x | x <- xl, x `mod` 2 == 1]
