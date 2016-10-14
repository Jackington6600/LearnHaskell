-- We consider strings of containg the characters 0 and 1 only.

-- A tree that represents a collection of binary strings:
data Tree = Empty            -- reject 
          | End              -- accept and end
          | Branch Tree Tree -- accept and proceed 
          deriving Show

data T = AcceptEmpty
       | AcceptZero T
       | AcceptOne T
       | AcceptZeroOrOne T T

-- Convert this collection into a list:
strings :: Tree -> [String]
strings Empty        = []
strings End          = [""]
strings (Branch l r) = ['0':xs | xs <- strings l] 
                    ++ ['1':xs | xs <- strings r]

-- Check whether a tree has a given string:
has :: Tree -> String -> Bool
Empty        `has` xs = False
End          `has` [] = True
End          `has` _  = False
(Branch l r) `has` [] = False
(Branch l r) `has` ('0':xs)  = l `has` xs
(Branch l r) `has` ('1':xs)  = r `has` xs

-- The opposite of the function strings. 
tree :: [String] -> Tree
tree []   = Empty
tree [""] = End
tree xss  = Branch (tree yss) (tree zss)
  where
    yss = [ys | ('0':ys) <- xss]
    zss = [zs | ('1':zs) <- xss] 
-- This works only for prefix-free lists of strings, where a list of
-- strings is called prefix-free if every string in the list, no other
-- string in the list is a prefix of it.

-- Use a string to follow a path in a tree, getting a subtree.
subtree :: Tree -> String -> Tree
subtree Empty        xs       = Empty
subtree End          xs       = End
subtree (Branch l r) ""       = Branch l r
subtree (Branch l r) ('0':xs) = subtree l xs
subtree (Branch l r) ('1':xs) = subtree r xs

-- A string is valid for a given tree if we can decompose it in a list
-- of contiguous substrings, each of which is a path in the tree:
valid :: Tree -> String -> Bool
valid t xs = f t xs
 where
   f Empty        xs       = False
   f End          ""       = True
   f End          xs       = f t xs
   f (Branch l r) ""       = False
   f (Branch l r) ('0':xs) = f l xs
   f (Branch l r) ('1':xs) = f r xs

-- We now do this decomposition, in two ways.
-- First we need to helper functions.

-- This takes an initial substring that is in the tree, if it exists:
bighead :: Tree -> String -> String
bighead Empty        xs       = undefined
bighead End          xs       = ""
bighead (Branch l r) ""       = undefined
bighead (Branch l r) ('0':xs) = '0' : bighead l xs
bighead (Branch l r) ('1':xs) = '1' : bighead r xs

-- This removes such an initial substring:
bigtail :: Tree -> String -> String
bigtail Empty        xs       = undefined
bigtail End          xs       = xs
bigtail (Branch l r) ""       = undefined
bigtail (Branch l r) ('0':xs) = bigtail l xs
bigtail (Branch l r) ('1':xs) = bigtail r xs

-- We perform the above decomposition by repeatedly taking bighead and
-- bigtail:
decompose :: Tree -> String -> [String]
decompose t xs = f xs
 where
  f [] = []
  f xs = bighead t xs : f(bigtail t xs)
-- Theorem. (concat(decompose t xs) == xs && and [t `has` ys | ys <- decompose t xs])

-- Another helper function computed the bighead and the bigtail
-- simultaneously, to be more efficient, avoiding recomputations:
ht :: Tree -> String -> (String,String)
ht Empty        xs       = undefined
ht End          xs       = ("",xs)
ht (Branch l r) ""       = undefined
ht (Branch l r) ('0':xs) = case ht l xs of (ys,zs) -> ('0' : ys, zs)
ht (Branch l r) ('1':xs) = case ht r xs of (ys,zs) -> ('1' : ys, zs)
-- Theorem. ht xs = (bighead xs, bigtail xs).

-- This works like decompose, but using the more efficient, combined
-- big head and tail functions given by ht:
decompose' :: Tree -> String -> [String]
decompose' t [] = []
decompose' t xs = case ht t xs of (ys,zs) -> ys : decompose' t zs

{-

  Experiments:

  *Main> tree ["00", "010", "011", "10", "11", "111"]
  Branch (Branch End (Branch End End)) (Branch End (Branch Empty End))

  *Main> strings(tree ["00", "010", "011", "10", "11", "111"])
  ["00","010","011","10","111"]

  However, if we add prefixes, they disappear (and hence the prefix prefixfree occurring 
  in the name of this file:

  *Main> strings(tree ["0", "00", "010", "01", "011", "10", "11", "111"])
  ["00","010","011","10","111"]

  We have another file bittree.hs which allows prefixes. But
  warning, there are situation in which we do want prefixes (see
  dicttree.hs) and situations in which we don't want them (your
  exercise on Huffman compression).

  *Main> (tree ["00", "010", "011", "10", "111"]) `has` "0"
  False
  *Main> (tree ["00", "010", "011", "10",  "111"]) `has` "00"
  True
  *Main> (tree ["00", "010", "011", "10",  "111"]) `has` "010"
  True
  *Main> (tree ["00", "010", "011", "10",  "111"]) `has` "110"
  False

  *Main> decompose (tree ["00", "010", "011", "10", "111"]) "0011110011010"
  ["00","111","10","011","010"]

  *Main> decompose (tree ["00", "010", "011", "10", "111"]) "001111001101"
  ["00","111","10","011","01*** Exception: Prelude.undefined

  In practice, it is preferable and safer to use something like the Maybe type, 
  rather than undefined. 

-}

