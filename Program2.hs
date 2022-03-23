-- Exercise 1
-- (myAnd x y) should return the logical AND

myAnd :: Bool -> Bool -> Bool
myAnd True y = y
myAnd False _ = False

myOr :: Bool -> Bool -> Bool
myOr True _ = True
myOr False y = y

-- Extra exercise
{-
   butLast xs returns the element 
   before the last one in list xs   
-}

butLast [x,_] = x
butLast (_:xs) = butLast xs


-- Exercise 2
-- 2. Consider the following datatype of binary trees, whose nodes contain integer values:

data Tree = Leaf | Nod Integer Tree Tree 
     deriving (Show, Eq)

--Define the following functions:
-- (a)
inOrder :: Tree -> [Integer]
inOrder Leaf = []
inOrder (Nod n lt rt) = lst1 ++ [n] ++ lst2
    where lst1 = inOrder lt
          lst2 = inOrder rt

-- (b)
isSorted :: [Integer] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:(y:xs)) = (x<=y) && isSorted (y:xs)

-- (c)
isBst :: Tree -> Bool
isBst xs = isSorted (inOrder xs)

-- (d)
search :: Tree -> Integer -> Bool
search Leaf _ = False
search (Nod cv lt rt) x
    | x == cv = True
    | x < cv = search lt x
    | x > cv = search rt x

-- (e)
insBST :: Tree -> Integer -> Tree
insBST Leaf x = Nod x Leaf Leaf
insBST (Nod cv lt rt) x
    | x > cv  = Nod cv lt (insBST rt x)
    | cv == x = Nod cv lt rt
    | x < cv  = Nod cv (insBST lt x) rt

-- ex 3
{-
   3. An environment is a list of type [(String,Float)] which does not contain two pairs
   (s1,v1) and (s2,v2) with s1==s2. For example, [("x",3.14),("y",-5.6),("z",29.5)]
   is an environment. Environments associate values to variables represented by strings.
-}
-- Consider the following datatype for arithmetic expressions:

data Exp = Const Float | Var String | Sum Exp Exp | Prod Exp Exp
    deriving (Show, Eq)

-- (a)
val :: String -> [(String,Float)] -> Float
val toFind [] = 0.0
val toFind ((key,value):xs)
    | toFind == key = value
    | toFind /= key = val toFind xs

-- (b)
eval :: Exp -> [(String,Float)] -> Float
eval (Const constantValue) _       = constantValue
eval (Var mappedValue) env         = val mappedValue env
eval (Sum leftSide rightSide) env  = eval leftSide env + eval rightSide env
eval (Prod leftSide rightSide) env = eval leftSide env * eval rightSide env


-- Exercise 4
-- 4. Consider geometric shapes described by values of the algebraic datatype
data Shape = Circle Float
    | Rect Float Float
    | RtTriangle Float Float
    | Square Float
    deriving (Eq,Show)

-- Define the following functions:
-- (a)
area :: Shape -> Float
area (Circle r) = 3.14 * r * r
area (Rect x y) = x * y
area (RtTriangle x y) = x * y / 2
area (Square x) = x * x

-- (b)
perimeter :: Shape -> Float
perimeter (Circle r) = 3.14 * r * 2
perimeter (Rect x y) = (x * y) * 2
perimeter (RtTriangle x y) = p
     where p = sqrt (x^2 + y^2) + x + y
perimeter (Square x) = x * 4



-- ex 5
-- 5. Consider natural numbers represented by values of the algebraic datatype

data Nat = Zero | Succ Nat
     deriving (Eq,Show)

-- Define the following functions:
-- (a)
add :: Nat -> Nat -> Nat
add Zero y = y
add y Zero = y
add (Succ x) (Succ y) = Succ( Succ( add x y))

-- (b)
mult :: Nat -> Nat -> Nat
mult Zero x = Zero
mult x Zero = Zero
mult (Succ Zero) y = y
mult y (Succ Zero) = y
mult (Succ x) y = mult x (add y y)

data Kid = Girl String | Boy String 
    deriving ( Show, Eq)

