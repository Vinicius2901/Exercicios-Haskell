import System.IO

main :: IO ()
main = do
        conteudo <- readFIle "alice.txt"
        putStrLn conteudo
