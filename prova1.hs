acker 0 n = n + 1
acker m 0 = acker (m-1) 1
acker m n = acker (m-1) (acker m (n-1))

pares [] = []
pares (x:xs) | rem x 2 == 0 = x:pares xs
             | otherwise = pares xs

ocorrencias' _ [] = []
ocorrencias' n (x:xs) | n == x = x : ocorrencias' n xs
                      | otherwise = ocorrencias' n xs

ocorrencias n ls = length (ocorrencias' n ls)

todos [True] = True
todos (x:xs) | x == False = False
             | otherwise = todos xs

inverteDuplas [] = []
inverteDuplas ((x,y):xs) = (y,x):inverteDuplas xs
