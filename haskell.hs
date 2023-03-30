ehTriangulo a b c | b >= c && a > (b - c) && a < (b + c) = "True"
                  | c >= b && a > (c - b) && a < (b + c) = "True"
                  | a >= c && b > (a - c) && b < (a + c) = "True"
                  | c >= a && b > (c - a) && b < (a + c) = "True"
                  | a >= b && c > (a - b) && c < (a + b) = "True"
                  | b >= a && c > (b - a) && c < (a + b) = "True"
                  | otherwise = "False"

tipoTriangulo a b c | a == b && b == c = "equilatero"
                    | a /= b && a /= c && b /= c = "escaleno"
                    | otherwise = "isosceles"

triangulo a b c | ehTriangulo a b c == "False" = "nao eh triangulo"
                | otherwise = tipoTriangulo a b c

somapares 0 = 0
somapares n | mod n 2 == 0 = n + somapares (n - 2)
            | otherwise = somapares (n - 1)

potencia 0 m = m
potencia n m = 2^n * m + potencia (n - 1) m

primo n = primo' n (n - 1)

primo' n 1 = "True"
primo' n x | mod n x == 0 = "False"
           | otherwise = primo' n (x - 1)
