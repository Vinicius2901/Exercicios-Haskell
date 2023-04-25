--Exercício 1

pertence _ [] = False
pertence n (x:xs) = if  n==x then True else pertence n xs

--Exercício 2

intersecao _ [] = []
intersecao [] _ = []
intersecao (x:xs) (a:b) | pertence x (a:b) = x : intersecao xs (a:b)
                        | otherwise = intersecao xs (a:b)

--Exercício 3

inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

--Exercício 4

nprimeiros 0 _ = []
nprimeiros n [] = []
nprimeiros n (x:xs) = x : nprimeiros (n-1) xs

nultimos n [] = []
nultimos n (x:xs) = inverso (nprimeiros n (inverso (x:xs)))

--Exercício 5

soma2 [] [] = []
soma2 [] _ = []
soma2 _ [] = []
soma2 (x:xs) (y:ys) = (x + y) : soma2 xs ys

--Exercício 6

pot2' 0 = []
pot2' n = 2^n : pot2' (n-1)

pot n = inverso (pot2' n)

--Exercício 7

intercalacao [] [] = []
intercalacao [] (x:xs) = (x:xs)
intercalacao (x:xs) [] = (x:xs)
intercalacao (x:xs) (y:ys) | x < y = x : intercalacao xs (y:ys)
                           | x == y = x : y : intercalacao xs ys
                           | y < x = y : intercalacao (x:xs) ys

--Exercício 8

menor [x] = x
menor (x:xs) | x < menor xs = x
             | otherwise = menor xs

--Exercício 9

removerElem _ [] = []
removerElem n (x:xs) | n /= x = x : removerElem n xs
                     | otherwise = removerElem n xs

--Exercício 10

ordenar [] = []
ordenar (x:xs) = menor (x:xs) : removerElem (menor (x:xs)) (ordenar xs)
