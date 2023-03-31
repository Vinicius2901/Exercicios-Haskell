--Exercício 1

funcao x = x^2 + 2*x + 3

--Exercício 2/7

modulo x y z = sqrt (x^2 + y^2 + z^2)

--Exercício 3

cem x | x > 100 = True
      | otherwise = False

--Exercício 4

soma x y z w = x+y+z+w

--Exercício 5

vetores x y z x' y' z' = (x*x' + y*y' + z*z')

--Exercício 6

porcentagem x y = (x / y) * 100

--Exercício 8

multiplo 7 = 1
multiplo n | mod n 7 == 0 = 1 + multiplo (n - 7)
            | otherwise = multiplo (n - 1)
