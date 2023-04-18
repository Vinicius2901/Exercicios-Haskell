--Exercício 1

conjunto = [x^3 | x <- [0..8]]

--Exercício 2

duplicalist [] = []
duplicalist (x:xs) = x : x : duplicalist xs

--Exercício 3

impares [] = []
impares (x:xs) | mod x 2 == 0 = impares xs
               | otherwise = x : impares xs 

--Exercício 4

nprimeiros 0 _ = []
nprimeiros n [] = []
nprimeiros n (x:xs) = x : nprimeiros (n-1) xs

--Explicação:
--nprimeiros 2 [1,2,3] = 1 : nprimeiros 1 [2,3]
--nprimeiros 1 [2,3] = 2 : nprimeiros 0 [3]
--nprimeiros 0 [3] = []
--1:2:[] = [1,2]

--Exercício 5

nultimos n [] = []
nultimos n (x:xs) = reverse (nprimeiros n (reverse (x:xs)))

--Exercício 6

inverso [] = []
inverso (x:xs) = 
