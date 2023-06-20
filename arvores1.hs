data ArvoreBin_Int = Nil_Int | Nodo_Int Int ArvoreBin_Int ArvoreBin_Int deriving Show

arvoreB = Nodo_Int 4 (Nodo_Int 2 Nil_Int Nil_Int)
                     (Nodo_Int 10 (Nodo_Int 5 Nil_Int Nil_Int)
                                  (Nodo_Int 15 Nil_Int Nil_Int))

--acessa folhas e coloca os valores em uma lista

folhas_int Nil_Int = []
folhas_int (Nodo_Int n Nil_Int Nil_Int) = [n]
folhas_int (Nodo_Int _ esq dir) = folhas_int esq ++ folhas_int dir

--acessa todos os nós e coloca em uma lista

todos Nil_Int = []
todos (Nodo_Int n esq dir) = n : todos esq ++ todos dir

--acessa apenas os nós internos de uma arvore

nos_internos Nil_Int = []
nos_internos (Nodo_Int _ Nil_Int Nil_Int) = []
nos_internos (Nodo_Int n esq dir) = n : nos_internos esq ++ nos_internos dir
