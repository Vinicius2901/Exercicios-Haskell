aplica2 f x = f(f x)
--aplica2 f x = \x -> (x+1)(\x -> (x+1) 10)

inc x = x + 1

--aplica inc 10 = inc (inc 10)
--                inc (10 + 1)
--                inc    11
--                inc 11 = 12

\x -> x+1
