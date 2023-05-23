mapear _ [] = []
mapear f (x:xs) = f x : mapear f xs

multi2 x = 2*x
menos1 x = x-1

filtrar _ [] = []
filtrar p (x:xs) | p x = x : filtrar p xs
                 | otherwise = filtrar p xs

reduzir f [x] = x
reduzir f (x:xs) = f x (reduzir f xs)

--adiciona o valor à esquerda
reduzird f aux [] = aux
reduzird f aux (x:xs) = f x (reduzird f aux xs)

--adiciona o valor à esquerda
reduzire f aux [] = aux
reduzire f aux (a:b) = reduzire f (f aux a) b
