import MorseLib

encode s = codeText (words s)

codeText [] = []
codeText (c:cs) = (codeWord c) ++ mediumGap ++ (codeText cs)

codeWord [] = []
codeWord (c:cs) = (codeSymbol c) ++ shortGap ++ (codeWord cs)

decode m = 