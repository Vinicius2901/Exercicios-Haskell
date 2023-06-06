data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving Show

{- $ tranfere o parênteses para a direita: (((a)b)c) -> (a(b(c))) -}
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
            {- _ _ são qualquer ponto do plano cartesiano -}
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
                {- pares ordenados: (x1,y1) (x2,y2) -}
