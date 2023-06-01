fsl f p ls = [f x | x <- ls, p x]
                  --definição de x está em x <- ls

mapear _ [] = []
mapear f (x:xs) = f x : mapear f xs

filtrar _ [] = []
filtrar p (x:xs) | p x = x : filtrar p xs
                 | otherwise = filtrar p xs

funcao2 f p ls = mapear f (filtrar p ls)
