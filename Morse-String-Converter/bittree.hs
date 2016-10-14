-- We consider strings of containg the characters 0 and 1 only.
-- This is like the file prefixfreebittree.hs (read the comments there),
-- but allows prefixes to occur.


-- We need to modify our type of trees. The new "EndOr" allows for prefixes.
data Tree = Empty | EndOr Tree | Branch Tree Tree deriving Show

-- This is like in the other file, but accounting for the new case:
has :: Tree -> String -> Bool
Empty        `has` xs        = False
(EndOr t)    `has` []        = True
(EndOr t)    `has` xs        = t `has` xs
(Branch l r) `has` []        = False
(Branch l r) `has` ('0':xs)  = l `has` xs
(Branch l r) `has` ('1':xs)  = r `has` xs

-- Ditto.
tree :: [String] -> Tree
tree [] = Empty
tree [""] = EndOr Empty
tree xss | "" `elem` xss = EndOr t
         | otherwise     = t
  where
    yss = [ys | ('0':ys) <- xss]
    zss = [zs | ('1':zs) <- xss] 
    t = Branch (tree yss) (tree zss)

-- Ditto.
strings :: Tree -> [String]
strings Empty = []
strings (EndOr t) = [] : strings t
strings (Branch l r) = ['0':xs | xs <- strings l] ++ ['1':xs | xs <- strings r]

-- Ditto.
subtree :: Tree -> String -> Tree
subtree Empty        xs = Empty
subtree (EndOr t)    "" = t
subtree (EndOr t)    xs = subtree t xs
subtree (Branch l r) "" = Branch l r
subtree (Branch l r) ('0':xs) = subtree l xs
subtree (Branch l r) ('1':xs) = subtree r xs

-- Try some experiments based on those of prefixtreebittree.hs.
