--module trabalhofinal where

import System.IO
import Data.Char
import Data.List

type Doc = String
type Linha = String
type Palavra = String

data Tree = Node [Int] Palavra Tree Tree | Leaf deriving Show

--construirIndice :: Doc -> [([Int], Palavra)]

pulalinha x | isSpace x = '\n'
            | otherwise = x

--Numero de linhas e a respectiva linha.
numLinhas :: [Linha] -> [(Int, Linha)]
numLinhas ls = numLinhas' ls 1

numLinhas' [] _ = []
numLinhas' (x:xs) n = (n, x) : numLinhas' xs (n+1)

-- tirar pontuações
palavras' [] = []
palavras' (x:xs) | isAlpha x || isSpace x = x : palavras' xs
                 | otherwise = palavras' xs

-- tirar palavras < 3
palavras'' [] _ _ = []
palavras'' (x:xs) c ultimaPalavra | isSpace x && c < 3 = palavras'' xs 0 []
                                  | isSpace x = ultimaPalavra ++ [x] ++ palavras'' xs 0 []
                                  | otherwise = palavras'' xs (c+1) (ultimaPalavra ++ [x])

numeraPalavras [] = []
numeraPalavras ((nlin,lin):proxlin) = (map ((, ) nlin) (words lin)) ++ numeraPalavras proxlin

{-
agrupar [] = []
agrupar ((x,y):xs) = ([a | (a,b) <- (x,y):xs, b == y], y) : agrupar [(a,b) | (a,b) <- (x,y):xs, b/= y]-} 
insereOrd e [] = [e]
insereOrd e l@(x:xs)  |e == x = l
                      |e < x = (e:l)
                      |otherwise = (x:insereOrd e xs)

--inserir lista em arvore

ins Leaf y x = Node [y] x  Leaf Leaf
ins (Node i s esq dir) y x |x == s = Node (insereOrd y i) s esq dir
                           |x < s = Node i s (ins esq y x) dir
                           |otherwise = Node i s esq (ins dir y x)

mIndexTree [] tree = tree
mIndexTree ((y,x):xs) tree = mIndexTree xs (ins tree y x)

printar Leaf = return ()
printar (Node l p esq dir) = do printar esq
                                putStrLn (p ++ " - " ++ show l)
                                printar dir
main :: IO ()
main = do
    conteudo <- readFile "Alice.txt"
    let semPontuacao = palavras' conteudo
        maior3 = palavras'' semPontuacao 0 []
        nlinhas = lines maior3
        numeroLinhas = numLinhas nlinhas
        numeraPal = numeraPalavras numeroLinhas
        palDeArvore = mIndexTree numeraPal Leaf 
    --print numeraPal
    printar palDeArvore
