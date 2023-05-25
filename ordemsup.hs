import Data.Char

transforma _ [] = []
transforma f (x:xs) = f x : transforma f xs

all2min x = toLower x

all2mai x = toUpper x

num2string x | x == 0 = "zero"
             | x == 1 = "um"
             | x == 2 = "dois"
             | x == 3 = "tres"
             | x == 4 = "quatro"
             | x == 5 = "cinco"
             | x == 6 = "seis"
             | x == 7 = "sete"
             | x == 8 = "oito"
             | x == 9 = "nove"
             | otherwise = "Num sei"

aplicarsobre _ [] = []
aplicarsobre f (x:xs) = f x : aplicarsobre f xs

quadrado x = x^2

posicao ls = zip [1..length(ls)] ls

divisao dupla = (snd dupla) `div` (fst dupla)

reduzird f aux [] = aux
reduzird f aux (x:xs) = f x (reduzird f aux xs)
