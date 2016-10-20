import Frequency

-- Tree that will encode and decode the text to reduce the file sizes
-- Uses frequency

data Tree = Empty
          | EndOr Tree
          | Branch Tree Tree