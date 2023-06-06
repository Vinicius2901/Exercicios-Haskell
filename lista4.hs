import System.IO
import Data.Char

type Doc = String
type Linha = String
type Palavra = String

--construirIndice :: Doc -> [([Int], Palavra)]

pulalinha x | isSpace x = '\n'
            | otherwise = x

main :: IO ()
main = do
    conteudo <- readFile "alice.txt"
    let nlinhas = lines conteudo
    let numLinhas = length nlinhas
    print numLinhas
