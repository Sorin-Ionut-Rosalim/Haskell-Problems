-- Cube root newton's method:
-- We want to find the cube root or a

{-
   sqrtNewton uses the inside functions
   a - the number for which we want to extract the cube root  
   eps - EPSILON / the approximation error
-}
sqrtNewton a eps =
   let improve g = 0.5*(g+a/g)
       goodEnough g = abs (g^2-a) < eps
       sqrtIter g
          | goodEnough g = g
          | otherwise = sqrtIter (improve g)
    in sqrtIter 1.0
