quad x = x^2

arearet b h = b * h

fco x = if x > 1 && x < 2 then 0 else 1

fatorial 0 = 1
fatorial n = n * fatorial (n-1)

fat x | x <=1 = 1
      | x > 1 = x * fat (x-1) --mesma coisa que a de cima, com uns frufru diferente.

conjA = [2*x | x <- [1..10]]

conjB = [3*x | x <- [1..20], mod x 2 == 0]

conjC = [3*x | x <- [1..20]]

soma (x,y) = x + y


--menor valor de uma lista

menor [x] = x
menor (x:xs) = if x < menor xs then x else menor xs

removerTab [] = []
removerTab (x:xs) = if x == '\t' then ' ' : removerTab xs else x : removerTab xs

simetrico [] = []
simetrico (x:xs) = if fst x == snd x then True : simetrico xs else False : simetrico xs
