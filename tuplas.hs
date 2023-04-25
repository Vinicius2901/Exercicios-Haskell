soma_e_sub (a,b) = (a+b, a-b)
soma (a,b) = a+b

primeiro (a,b) = a
segundo (a,b) = b

primeiro2 (a,_) = a
segundo2 (_,b) = b

cltuplas = [(x,y) | x <- [1,2,3], y <- ['a','b']] {- Faz todas as combinações entre a primeira e segunda lista -}

clt2 = [(x,y,z) | x <- [1..10], y <- [1..10], z <- [1..10]]

clt3 = [(x,y,z) | x <- [1..100], y <- [1..100], z <- [1..100], {- condição dos valores que eu quero das listas -} x^2 + y^2 + z^2 == 9]

{- Exercício 3 -}

media [] = []
media (x:xs) | x > 10 || x < 0 = media xs
             | x >= 7 = ("Aprovado", x) : media xs
             | otherwise = ("Reprovado", x) : media xs
