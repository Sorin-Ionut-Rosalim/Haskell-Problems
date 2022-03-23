-- Cube root newton's method:
-- We want to find the cube root or a

{-
   Function for improving our guess
   n - our guess
   a - the number for which we want to extract the cube root  
-}
improve3::Double->Double->Double
improve3 0 _ = 1.0
improve3 n a = ((2 * n) + (a / (n)^2))/3

{-
   Function for checking our guess
   x - our guess
   a - the number for which we want to extract the cube root  
   eps - EPSILON / the approximation error
-}
good::Double->Double->Double->Bool
good x a eps 
             | eps < 0 = False
             | abs(x^3 - a) <= eps = True
             | otherwise = False

{-
   The function iterates until it finds a good guess
   x - our guess
   a - the number for which we want to extract the cube root  
   eps - EPSILON / the approximation error
-}
newtonIter::Double->Double->Double->Double
newtonIter x a eps
             | good x a eps  = x
             | otherwise = newtonIter ( improve3 x a) a eps

{-
   sqrt3Newton1 uses the above functions
   a - the number for which we want to extract the cube root  
   eps - EPSILON / the approximation error
-}
sqrt3Newton1 :: Double->Double->Double
sqrt3Newton1 a eps = newtonIter 1 a eps

{-
   sqrt3Newton uses the inside functions
   a - the number for which we want to extract the cube root  
   eps - EPSILON / the approximation error
-}
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



