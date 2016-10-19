-- Get the frequency of ASCII characters in a file.
-- TODO: Solve insert, currently destroys the accumulator
-- 'where' may not work

frequency xs = frequency' xs []

frequency' [] accum = accum
frequency' (x:xs) accum = frequency' xs (insert [] x accum)

insert pre x [] = pre ++ [(x,1)]
insert pre x ((a,n):as) = if x == a then pre ++ (a,n+1):as else insert (pre ++ [(a,n)]) x as