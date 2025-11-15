module Inventario where


import Tipos
import Utils
import System.IO


menu :: IO ()
menu = do
putStrLn "1 - Adicionar Item"
putStrLn "2 - Remover Item"
putStrLn "3 - Listar Itens"
putStrLn "4 - Sair"
putStr "Escolha: "
op <- getLine
case op of
"1" -> adicionarItem >> menu
"2" -> removerItem >> menu
"3" -> listarItens >> menu
"4" -> putStrLn "Saindo..."
_ -> putStrLn "Opção inválida" >> menu


adicionarItem :: IO ()
adicionarItem = do
putStrLn "Nome do item:"
nome <- getLine
putStrLn "Quantidade:"
qtd <- readLn
registrarLog ("Adicionado: " ++ nome)
appendFile "Inventario.dat" (nome ++ ":" ++ show qtd ++ "\n")


removerItem :: IO ()
removerItem = do
putStrLn "Nome a remover:"
nome <- getLine
itens <- lerInventario
let novos = filter ((/= nome) . fst) itens
registrarLog ("Removido: " ++ nome)
salvarInventario novos


listarItens :: IO ()
listarItens = do
itens <- lerInventario
mapM_ (\(n,q) -> putStrLn (n ++ " - " ++ show q)) itens


lerInventario :: IO [(String, Int)]
lerInventario = do
conteudo <- readFile "Inventario.dat"
return (map parseItem (lines conteudo))


salvarInventario :: [(String, Int)] -> IO ()
salvarInventario itens = writeFile "Inventario.dat"
(unlines [n ++ ":" ++ show q | (n,q) <- itens])


parseItem :: String -> (String, Int)
parseItem linha = (nome, read qtd)
where (nome, _ : qtd) = break (== ':') linha