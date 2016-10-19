-- Get the frequency of ASCII characters in a file.
-- TODO: Solve insert, currently destroys the accumulator
-- 'where' may not work

frequency xs = frequency' xs []

frequency' [] accum = accum
frequency' (x:xs) accum = frequency' xs (insert x accum)

insert x ((a,n):accum) where x == a = (a,n+1):accum