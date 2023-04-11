--Exercício 1

ehTriangulo a b c | b >= c && a > (b - c) && a < (b + c) = True
                  | c >= b && a > (c - b) && a < (b + c) = True
                  | a >= c && b > (a - c) && b < (a + c) = True
                  | c >= a && b > (c - a) && b < (a + c) = True
                  | a >= b && c > (a - b) && c < (a + b) = True
                  | b >= a && c > (b - a) && c < (a + b) = True
                  | otherwise = False

--Exercício 2

tipoTriangulo a b c | a == b && b == c = "equilatero"
                    | a /= b && a /= c && b /= c = "escaleno"
                    | otherwise = "isosceles"

--Exercício 3

triangulo a b c | ehTriangulo a b c == False = "nao eh triangulo"
                | otherwise = tipoTriangulo a b c

--Exercício 4

somapares 0 = 0
somapares n | mod n 2 == 0 = n + somapares (n - 2)
            | otherwise = somapares (n - 1)

--Exercício 5

potencia 0 m = m
potencia n m = 2^n * m + potencia (n - 1) m 

--Exercício 6

primo n = primo' n (n - 1)

primo' x 1 = True
primo' x y | mod x y == 0 = False
           | otherwise = primo' x (y - 1)

--Exercício 7



seriePI :: Int -> Double
seriePI n = seriePI' 1 0
 where
    seriePI' :: Double -> Double -> Double
    seriePI' termo soma | termo > fromIntegral n = soma
                        | otherwise = seriePI' (termo + 4) (soma + ((4 / termo) - (4 / (termo + 2))))

