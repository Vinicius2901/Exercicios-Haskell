--Exercício 1

conjunto = [x^3 | x <- [0..8]]

--Exercício 2

duplicalist [] = []
duplicalist (x:xs) = x : x : duplicalist xs

--Exercício 3

impares [] = []
impares (x:xs) | mod x 2 == 0 = impares xs
                | otherwise = x : impares xs 
