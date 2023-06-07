import System.IO
import Data.Char

type Doc = String
type Linha = String
type Palavra = String

--construirIndice :: Doc -> [([Int], Palavra)]

pulalinha x | isSpace x = '\n'
            | otherwise = x

numLinhas :: [Linha] -> [(Int, Linha)]
numLinhas ls = numLinhas' ls 1

numLinhas' [] _ = []
numLinhas' (x:xs) n = (n, x) : numLinhas' xs (n+1)

-- tirar pontuações
palavras' [] = []
palavras' (x:xs) | isAlpha x || isSpace x = x : palavras' xs
                 | otherwise = palavras' xs

-- tirar palavras <3
palavras'' [] _ _ = []
palavras'' (x:xs) c ultimaPalavra | isSpace x && c < 3 = palavras'' xs 0 []
                                  | isSpace x = ultimaPalavra ++ [x] ++ palavras'' xs 0 []
                                  | otherwise = palavras'' xs (c+1) (ultimaPalavra ++ [x])

numeraPalavras [] = []
numeraPalavras ((nlin,lin):proxlin) = (map ((nlin,)) (words lin)) ++ numeraPalavras proxlin

main :: IO ()
main = do
    conteudo <- readFile "Alice.txt"
    let semPontuacao = palavras' conteudo
        maior3 = palavras'' semPontuacao 0 []
        nlinhas = lines maior3
        numeroLinhas = numLinhas nlinhas
        numeraPal = numeraPalavras numeroLinhas
    print numeraPal
