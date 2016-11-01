{-# LANGUAGE Safe #-} 
module Assessed1Part2 where

-- We begin with a sample solution of part 1, produced by Cory.

import Data.Maybe
import Data.Char

data Tree c = Leaf c Int | Branch (Tree c) (Tree c) Int
    deriving (Show, Eq, Ord, Read)

data Bit = Z | I
    deriving (Eq, Ord)

instance Show Bit where
    show Z = "0"
    show I = "1"
    showList [] = id
    showList (x:xs) = \out -> (show x) ++ showList xs out

readBit :: Char -> Bit
readBit '0' = Z
readBit '1' = I

readBits :: [Char] -> [Bit]
readBits = map readBit

decode :: Eq c => (Tree c, [Bit]) -> [c]
decode (tree, bits) = decodeAux tree tree bits

decodeAux :: Eq c => Tree c -> Tree c -> [Bit] -> [c]
decodeAux fullTree (Leaf c _) [] = [c]
decodeAux fullTree (Leaf c _) bs = c:(decodeAux fullTree fullTree bs) 
decodeAux fullTree (Branch left right _) (Z:bs) = decodeAux fullTree left bs
decodeAux fullTree (Branch left right _) (I:bs) = decodeAux fullTree right bs

{- The input String has the following format:

   * An integer n coded as a sequence of digits.

   * This is followed by exact n characters, have a tree write with
     show, that can be read with read.

   * A sequence of 0's and 1's (characters) representing a sequence of bits.

   The output should be some text.

-}

decompress :: String -> String
decompress str = decode (t,bits)
    where
        (n',str') = span isDigit str
        n         = read n'
        t'        = take n str'
        t         = read t'
        bits      = readBits $ drop n str'

{- Decompression for a smarter compression algorithm: For a short
string or a random string, the Huffman code of the string is longer
than the string. In this case, we produce the original string with a '*'
at the front, indicating that no compression was performed. 

However, we need to simulate this using `charlength`, since we're
outputting a bitsequence as characters.  charlength is the bit-length
of a single character. We could change this to simulate a different
character encoding. -}

charlength :: Int
charlength = 8

-- gives the length in "bits" of a string
memSize :: String -> Int
memSize s = 8 * (length s)

-- Smarter decompression, as discussed above. The input is either *
-- followed by a string, or as in the original decompression function:
decompress' :: String -> String
decompress' ('*':s)   = s
decompress' s = decompress s

-- Generate the frequency table
-- An element of the type Freq is a symbol together with its frequency.
type Freq c = (c,Int)

leaf :: Freq c -> Tree c
leaf (c,i) = Leaf c i

freq :: Tree c -> Int
freq (Leaf _ i) = i
freq (Branch _ _ i) = i

-- Generates a frequency table. 
tabulate :: Eq c => [c] -> [Freq c]
tabulate string = quicksort (foldr update [] string)

-- Removes the existing entry for c (if it exists), updates it, and
-- then reinserts it if no entry exists, we start over at 0, and then
-- "update"
update :: Eq c => c -> [Freq c] -> [Freq c]
update c keys = newFreq : rest
    where
        (old,rest) = (is c) `outOf` keys
        key = fromMaybe (c,0) old
        newFreq = mapSnd (+1) key

is :: Eq c => c -> Freq c -> Bool
is c (d,_) = c == d

outOf :: (a -> Bool) -> [a] -> (Maybe a,[a])
outOf p []     = (Nothing,[])
outOf p (x:xs) = if (p x) then (Just x,xs) else (mapSnd (x:) $ outOf p xs)

mapSnd :: (a -> b) -> (c,a) -> (c,b)
mapSnd f (c,a) = (c, f a)

{- End of part 1. Your tasks for part 2 begin here. -}

-- Produce a Huffman tree from a list of Huffman trees.
-- https://www.siggraph.org/education/materials/HyperGraph/video/mpeg/mpegfaq/huffman_tutorial.html
makeTree :: [Tree c] -> Tree c
makeTree (x:[])      = x
makeTree (x:y:trees) = makeTree ( insertTree (Branch x y (freq x + freq y)) trees )

testmakeTree = makeTree [Leaf 'a' 2, Leaf 'b' 4, Leaf 'c' 5]

-- You may wish to use a helper function such as this:
insertTree x []        = [x]
insertTree x (t:trees) | (freq x) <= (freq t) = (x:t:trees)
                       | otherwise            = [t] ++ (insertTree x trees)

-- Generate a tree from list of Freqs (using makeTree above):
generateTree :: [Freq c] -> Tree c
generateTree freqs = makeTree (map (leaf) freqs)

-- Encoding table.
-- A key is a key-value pair (an entry in a map/table).
type Key c = (c,[Bit])

-- The whole coding table
type CodingTable c = [Key c]

-- Given a tree, generates a coding table
makeTable :: Eq c => Tree c -> CodingTable c
makeTable tree = makeTable' tree []

makeTable' (Leaf c _) accum            = [(c, accum)]
makeTable' (Branch left right _) accum = makeTable' left (accum ++ [Z]) ++ makeTable' right (accum ++ [I])

-- Takes a string of symbols to a bit string, based on a given coding table
encodeUsingTable :: Eq c => CodingTable c -> [c] -> [Bit]
encodeUsingTable codeTable []         = []
encodeUsingTable codeTable (s:string) = findinTable s codeTable ++ encodeUsingTable codeTable string

findinTable s []                    = error "Character not found"
findinTable s ((x, bits):codeTable) | s == x    = bits
                                    | otherwise = findinTable s codeTable

-- Encodes directly from the tree (more efficient?).
encodeUsing :: Eq c => Tree c -> [c] -> [Bit]
encodeUsing tree []         = []
encodeUsing tree (s:string) = (findinTree s tree []) ++ (encodeUsing tree string)

findinTree s (Leaf c _) accum            | s == c = accum
findinTree s (Leaf c _) accum            = []
findinTree s (Branch left right _) accum = findinTree s left (accum ++ [Z]) ++ findinTree s right (accum ++ [I])


-- From a string of symbols, generate the coding tree and the encoding
encode :: Eq c => [c] -> (Tree c, [Bit])
encode string = (generateTree (tabulate string), encodeUsing (generateTree (tabulate string)) string)

-- Encoding trees

-- Compressing a string. This should be the inverse of decompress.
-- That is, this should output a string of the form
--
-- n ++ t ++ c
--
-- Where,
--    * n is a read from an integer
--    * t is read from a tree, and contains exactly n characters.
--    * c is string of bits.
compress :: String -> String
compress string = len ++ treeshow ++ bit
    where (tree, bits) = encode string
          len          = show (length (show tree))
          treeshow     = show tree
          bit          = (showList bits) ""

-- Smarter compression: if the encoded string is larger than the input string,
-- instead output the input string with a '*' in front.
compress' :: String -> String
compress' string = co
    where (tree, bits)   = encode string
          len            = show (length (show tree))
          treeshow       = show tree
          bit            = (showList bits) ""
          sizeCompressed = (memSize len) + (memSize treeshow) + (length bit)
          size           = memSize string
          co             = if size > sizeCompressed then
                            len ++ treeshow ++ bit
                         else
                            "*" ++ string



-- Basic quicksort algorithm to put the list of pairs in descending order.
quicksort []         = []
quicksort ((x,y):xs) = quicksort [(a,b) | (a,b) <- xs, b <= y] ++ [(x,y)] ++ quicksort [(a,b) | (a,b) <- xs, b > y]
