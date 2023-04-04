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

--Exercício 9

multiplo 0 = 0
multiplo n | mod n 7 == 0 = 1 + multiplo (n - 7)
           | otherwise = multiplo (n - 1)

--Exercício 10

pot x 0 = 1
pot x y = x * pot x (y-1)

--Exercício 11

quad 0 = 0
quad x = x^2 + quad (x - 1)

--Exercício 12

multi3 0 = 0
multi3 x | mod x 3 == 0 = x + multi3(x - 3)
         | otherwise = multi3 (x - 1)

--Exercício 13

raizin x = floor (sqrt x)
