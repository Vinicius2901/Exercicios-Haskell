aplica2 f x = f(f x)
--aplica2 f x = \x -> (x+1)(\x -> (x+1) 10)

inc x = x + 1

--aplica inc 10 = inc (inc 10)
--                inc (10 + 1)
--                inc    11
--                inc 11 = 12

\x -> x+1

maiorc x y = if x>y then x else y

maiorn (x,y) = if x>y then x else y

curry' = \f x y -> f (x,y)

zero = \s z -> z
um = \s z -> s z
dois = \s z -> s (s z)
tres = \s z -> s (s (s z))
quatro = \s z -> s (s (s (s z)))

suc = \w y x -> y (w y x)
add = \x y w u -> x w (y w u)

if' = \c x y -> c x y
v = \a b -> a
f = \a b -> b
