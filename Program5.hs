-- (HW1)

-- 1
improve3::Double->Double->Double
improve3 0 _ = 1.0
improve3 n a = ((2 * n) + (a / (n)^2))/3

-- 2
good::Double->Double->Double->Bool
good x a eps 
             | eps < 0 = False
             | abs(x^3 - a) <= eps = True
             | otherwise = False

-- 3
newtonIter::Double->Double->Double->Double
newtonIter x a eps
             | good x a eps  = x
             | otherwise = newtonIter ( improve3 x a) a eps

sqrt3Newton::Double->Double->Double
sqrt3Newton a eps =
   let improve3 n a 
             | n == 0 = 1.0
             | otherwise = ((2 * n) + (a / (n)^2))/3
       good x a eps 
             | eps < 0 = False
             | abs(x^3 - a) <= eps = True
             | otherwise = False
       newtonIter x a eps
             | good x a eps  = x
             | otherwise = newtonIter ( improve3 x a) a eps
    in newtonIter 1 a eps

sqrt3Newton1 :: Double->Double->Double
sqrt3Newton1 a eps = newtonIter 1 a eps

sqrtNewton a eps =
   let improve g = 0.5*(g+a/g)
       goodEnough g = abs (g^2-a) < eps
       sqrtIter g
             | goodEnough g = g
             | otherwise = sqrtIter (improve g)
    in sqrtIter 1.0

--(HW2) It is well-known that if p(x) is a polynomial of degree n

--(HW3) Consider the function flx defined by

flx p xs = fxAcc p xs [] where
   fxAcc _ [] acc = reverse acc
   fxAcc p (x:xs) acc
     | p x = fxAcc p xs (x:acc)
     | otherwise = fxAcc p xs acc

--1. What does (flx p xs) compute?
--2. Is the function flx tail-recursive? Motivate your answer.


-- (HW4) Consider the tail-recursive function drop1 defined by

drop1::[Integer]->[Integer]
drop1 [] = []
drop1 (x:xs) = dropAux xs where
  dropAux [] = []
  dropAux (y:ys)
    | y <= x = dropAux ys
    | otherwise = x:(y:ys)

--1. What does (drop1 [a1, a2, . . . , an]) compute when [a1, a2, . . . , an] is a non-empty list of integers?

--2. Use recursion to define the function

-- (HW5) Write in Haskell a tail-recursive definition of the function
fy::Integer->Integer
fy n 
             | n == 1 = 1
             | n == 2 = 2
             | n == 3 = 1
             | otherwise = fy (n-1) + 2 * fy (n-2) - fy (n-3)

-- (HW) Define by tail recursion the function
xs = [['r'], ['g'], ['b']]

tell [] = "\n The list is empty"
tell (x:xs) = x ++ " " ++ tell xs

-- (HW 10) Define the higher-order function