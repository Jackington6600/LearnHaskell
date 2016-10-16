import MorseLib

-- Encode a sentance to morse code:
encode s = codeText (words s)

codeText [] = []
codeText (c:cs) = (codeWord c) ++ mediumGap ++ (codeText cs)

codeWord [] = []
codeWord (c:cs) = (codeSymbol c) ++ shortGap ++ (codeWord cs)

-- Decode morse code to a sentance:
decode m = decode' m [] []

decode' [] [] accum                                                          = accum
decode' (Silence:Silence:Silence:Silence:Silence:Silence:Silence:[]) w accum = decode' [] [] (accum ++ (decodeWord (w ++ [Silence,Silence,Silence])))
decode' (Silence:Silence:Silence:Silence:Silence:Silence:Silence:m) w accum  = decode' m [] (accum ++ (decodeWord (w ++ [Silence,Silence,Silence]) ++ [' ']))
decode' (c:m) w accum                                                        = decode' m (w ++ [c]) accum

decodeWord w = decodeWord' w [] []

decodeWord' [] [] accum                             = accum
decodeWord' (Silence:Silence:Silence:[]) char accum = decodeWord' [] [] (accum ++ (decodeChar (char ++ [Silence])))
decodeWord' (Silence:Silence:Silence:w) char accum  = decodeWord' w [] (accum ++ (decodeChar (char ++ [Silence])))
decodeWord' (c:w) char accum                        = decodeWord' w (char ++ [c]) accum

decodeChar char = [b | (a,b) <- table, char == a]