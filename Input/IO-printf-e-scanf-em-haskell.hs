main :: IO ()
main = do
        putStrLn "Digite um valor:"
        x <- getLine
        putStrLn "Digite um segundo valor:"
        y <- getLine
        putStr "A soma dos dois valores é: "
        putStrLn (show (read x + read y))
