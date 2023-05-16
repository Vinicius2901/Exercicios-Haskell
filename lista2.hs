import Data.Char

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
ordenar ls = min : ordenar (removerElem min ls)
        where min = menor ls --declaração de variável

--Exercício 11

insereOrd n [] = [n]
insereOrd n (x:xs) | n < x = n : x : xs
                   | pertence n (x:xs) = (x:xs)
                   | otherwise = x : insereOrd n xs

--Exercício 12

enesimo 0 (x:xs) = []
enesimo n [] = error "não existe elemento na lista"
enesimo n (x:xs) | n == 1 = x
                 | otherwise = enesimo (n-1) xs

--Exercício 13

repetir 0 y = []
repetir x y = y : repetir (x-1) y

--Exercício 14

removeTab [] = []
removeTab (x:xs) | '\t' == x = ' ' : removeTab xs
                 | otherwise = x : removeTab xs

--Exercício 15

minusculas [] = []
minusculas (x:xs) = toLower x : minusculas xs

--Exercício 16

inversoDupla [] = []
inversoDupla ((x,y):xs) = (y,x) : inversoDupla xs

--Exercício 17

simetrico [] = []
simetrico ((x,y):xs) | x == y = True : simetrico xs
                     | otherwise = False : simetrico xs

--Exercício 18

numString' 0 = []
numString' n = chr ((rem n 10) +48) : numString' (div n 10)

numString n = inverso (numString' n)

--Exercício 19

stringNum' [] = []
stringNum' (x:xs) = ord x -48 : stringNum' xs

stringNum'' [] = 0
stringNum'' (x:xs) = x*10^(length(x:xs)-1) + stringNum'' xs

stringNum ls = stringNum'' (stringNum' ls)

--Exercicio 20

decBin' 0 = [0]
decBin' 1 = [1]
decBin' n | mod n 2 == 1 = 1 : decBin' (div n 2)
          | mod n 2 == 0 = 0 : decBin' (div n 2)

decBin'' n = inverso (decBin' n)

decBin n = numString (stringNum'' (decBin'' n))

--Exercício 21

binDec' (x:xs) = stringNum' (x:xs)

binDec'' [] = 0
binDec'' (x:xs) = x*2^(length(x:xs)-1) + binDec'' xs

binDec ls = binDec'' (binDec' ls)

--Exercício 22

trocoCafe' 0 = []
trocoCafe' x | x >= 50 = (50,div x 50) : trocoCafe' (mod x 50)
             | x >= 20 = (20,div x 20) : trocoCafe' (mod x 20)
             | x >= 10 = (10,div x 10) : trocoCafe' (mod x 10)
             | otherwise = (5,div x 5) : trocoCafe' (mod x 5)

trocoCafe x y | (y-x) < 5 = []
              | otherwise = trocoCafe' (y-x)
