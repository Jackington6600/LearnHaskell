-- Chapter 2)

n = a `div` length xs
 where
  a = 10
  xs = [1,2,3,4,5]

last [] = error "last"
last xs = head(reverse xs)

last2 [] = error "last2"
last2 xs = xs !! ((length xs) - 1)

init [] = error "init"
init xs = reverse(tail(reverse xs))

init2 [] = error "init2"
init2 xs = take ((length xs) - 1) xs

-- Chapter 3)

{-

['a','b','c'] :: [Char]
('a','b','c') :: (Char, Char, Char)
[(False,'0'), (True,'1')] :: [(Bool, Char)]
([False, True], ['0','1']) :: ([Bool], [Char])
[tail, init, reverse] :: [[a] -> [a]]

-}

bools = [True, False]
nums = [[1]] :: [[Int]]
add x y z = x + y + z :: Int
copy a = (a,a)
apply f a = f (a)

{-

second :: [a] -> a
swap :: (t1, t) -> (t, t1)
pair :: t -> t1 -> (t, t1)
double :: Num a => a -> a
palindrome :: Eq a => [a] -> Bool
twice :: (t -> t) -> t -> t

-}

{-

Functions cannot be checked to be equal because you'd have to check every input and output to see if they match.
Functions are considered equal if given the same input they give the same output.

-}

-- Chapter 4)

halve [] = ([], [])
halve xs = (take ((length xs) `div` 2) xs, reverse (take ((length xs) `div` 2) (reverse xs)))

third xs = head(tail(tail xs))

third2 xs = xs !! 2

third3 [] = error "third3"
third3 (_:_:c:_) = c
third3 xs = error "third3"

safetail xs = if null xs then [] else tail xs

safetail2 xs | null xs = []
 | otherwise = tail xs

safetail3 [] = []
safetail3 xs = tail xs

{-

True || True = True
True || False = True
False || True = True
False || False = False

-}

and a b = if a then if b then True else False else False

-- TODO Q6-8

-- Chapter 5)

squares = sum [n^2 | n <- [1..100]]

grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

square m = [(x,y) | x <- [0..m], y <- [0..n], x /=y]

replicate n some = [ [some] | x <- [1..n]]

pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]
