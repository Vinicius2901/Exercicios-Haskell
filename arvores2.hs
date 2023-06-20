data Arvore a = Nulo | No a (Arvore a) (Arvore a) deriving Show

arvoreB = No (1,'A') (No (2,'B') Nulo Nulo)
                 (No (3,'C') (No (4,'D') Nulo Nulo)
                         (No (5,'E') Nulo Nulo))

--acessa folhas e coloca os valores em uma lista

folhas Nulo = []
folhas (No n Nulo Nulo) = [n]
folhas (No _ esq dir) = folhas esq ++ folhas dir

--acessa apenas os nós internos de uma arvore

nos_internos Nulo = []
nos_internos (No _ Nulo Nulo) = []
nos_internos (No n esq dir) = n : nos_internos esq ++ nos_internos dir
