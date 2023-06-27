module trabalhofinal where

import System.IO
import Data.Char
import Data.List

type Doc = String
type Linha = String
type Palavra = String

data Tree 	= Node Palavra [Int] Tree Tree | Leaf deriving Show

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
numeraPalavras ((nlin,lin):proxlin) = (map ((nlin,)) (words lin)) ++ numeraPalavras proxlin

{-
agrupar [] = []
agrupar ((x,y):xs) = ([a | (a,b) <- (x,y):xs, b == y], y) : agrupar [(a,b) | (a,b) <- (x,y):xs, b/= y]-} 

insereOrd n [] = [n]
insereOrd n (x:xs) | n < x = n : x : xs
                   | pertence n (x:xs) = (x:xs)
                   | otherwise = x : insereOrd n xs

--inserir lista em arvore
ins x n Leaf = Nodo x [n] Leaf Leaf
ins w n esq dir = case compare x w
                    | LT -> Nodo w ls (ins x n esq)
                    | EQ -> Nodo w (ins) esq dir
                    | GT -> Node w ls esq (ins x n dir)



main :: IO ()
main = do
    conteudo <- readFile "Alice.txt"
    let semPontuacao = palavras' conteudo
        maior3 = palavras'' semPontuacao 0 []
        nlinhas = lines maior3
        numeroLinhas = numLinhas nlinhas
        numeraPal = numeraPalavras numeroLinhas
        sortedPal = sortOn (map toLower . snd) numeraPal
        palAgrupado = agrupar sortedPal
        palEliminado = eliminarRep palAgrupado
    
    print palEliminado
