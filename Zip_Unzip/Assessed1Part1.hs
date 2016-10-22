{-# LANGUAGE Safe #-} -- For automatic marking to work.
module Assessed1Part1 where


{- Huffman Codes -}

data Tree c = Leaf c Int | Branch (Tree c) (Tree c) Int
    deriving (Show, Eq, Ord, Read)

data Bit = Z | I
    deriving (Eq, Ord)

instance Show Bit where
    show Z = "0"
    show I = "1"
    showList [] = id
    showList (x:xs) = \out -> (show x) ++ showList xs out

{--- Decoding ---}
-- Notice that this should work for types more general than Char (our c).
-- Question:
-- decode :: Eq c => (Tree c, [Bit]) -> [c]
decode (tree, bits) = decodeAux (tree, bits) []

-- You may or may not wish to use a helper function as follows for
-- decode (this function will not be marked, and you can leave it
-- undefined if you don't use it):
-- decodeAux :: Eq c => Tree c -> Tree c -> [Bit] -> [c]
decodeAux (tree, (0:bits)) accum = 


{-- decompression --}

{- The input String has the following format:

   * An integer n coded as a sequence of digits.
   
   * This is followed by exact n characters, have a tree write with
     show, that can be read with read.

   * A sequence of 0's and 1's (characters) representing a sequence of bits.

   The output should be some text.

-}

-- decompress :: String -> String
decompress string = decompressHelper string (decompressIntToTraverse string ([],0)) [] [] 0

decompressIntToTraverse (i:string) (intToTraverse, int2)
            | i == 'B'  = read (intToTraverse) + int2
            | otherwise = decompressIntToTraverse string ((intToTraverse ++ [i]), (int2 + 1))

decompressHelper [] x tree bits counter = decode (read (tree), bits)
decompressHelper (i:string) x tree bits counter = if counter < x then
                                                        decompressHelper string x (tree ++ [i]) bits (counter + 1)
                                                    else
                                                        decompressHelper string x tree (bits ++ [i]) (counter + 1)

{--- Decompression for a smarter compression algorithm: For a short
string or a random string, the Huffman code of the string is longer
than the string. In this case, we produce the original string with a '*'
at the front, indicating that no compression was performed. 

However, we need to simulate this using `charlength`, since we're
outputting a bitsequence as characters.  charlength is the bit-length
of a single character. We could change this to simulate a different
character encoding.  ---}

charlength :: Int
charlength = 8

--gives the length in "bits" of a string
memSize :: String -> Int
memSize s = 8 * (length s)

-- Smarter decompression, as discussed above. The input is either *
-- followed by a string, or as in the original decompression function:
decompress' :: String -> String
decompress' = undefined


{--- Generate the frequency table ---}
--An element of the type Freq is a symbol together with its frequency.
type Freq c = (c,Int)

leaf :: Freq c -> Tree c
leaf (c,i) = Leaf c i

freq :: Tree c -> Int
freq (Leaf _ i) = i
freq (Branch _ _ i) = i


--Generates a frequency table. 
tabulate :: Eq c => [c] -> [Freq c]
tabulate = undefined

-- Get the frequency of ASCII characters in a file.
frequency xs = frequency' xs []

frequency&sort xs = quciksort (frequency' xs [])

-- frequency helper function to do the legwork.
frequency' [] accum = accum
frequency' (x:xs) accum = frequency' xs (insert [] x accum)

-- insert the next given character into the list of pairs, incrementing if it is already
-- present and appending to the end of the list ( initial frequency 1) if it is not already present.
insert pre x [] = pre ++ [(x,1)]
insert pre x ((a,n):as) = if x == a then pre ++ (a,n+1):as else insert (pre ++ [(a,n)]) x as

-- Basic quicksort algorithm to put the list of pairs in descending order.
quicksort [] = []
quicksort ((x,y):xs) = quicksort [(a,b) | (a,b) <- xs, b > y] ++ [(x,y)] ++ quicksort [(a,b) | (a,b) <- xs, b <= y]