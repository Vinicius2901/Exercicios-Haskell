--Exercício 1

conjunto = [x^3 | x <- [0..8]]

--Exercício 2

duplicalist [] = []
duplicalist (x:xs) = x : x : duplicalist xs

--Exercício 3

inteiros [] = []
inteiros (x:xs) | mod x 2 == 0 = inteiros xs
                | otherwise = x : inteiros xs