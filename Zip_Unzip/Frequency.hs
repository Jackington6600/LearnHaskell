-- Get the frequency of ASCII characters in a file.

frequency xs = frequency' xs []

-- frequency helper function to do the legwork.
frequency' [] accum = quicksort accum
frequency' (x:xs) accum = frequency' xs (insert [] x accum)

-- insert the next given character into the list of pairs, incrementing if it is already
-- present and appending to the end of the list ( initial frequency 1) if it is not already present.
insert pre x [] = pre ++ [(x,1)]
insert pre x ((a,n):as) = if x == a then pre ++ (a,n+1):as else insert (pre ++ [(a,n)]) x as

-- Basic quicksort algorithm to put the list of pairs in descending order.
quicksort [] = []
quicksort ((x,y):xs) = quicksort [(a,b) | (a,b) <- xs, b > y] ++ [(x,y)] ++ quicksort [(a,b) | (a,b) <- xs, b <= y]