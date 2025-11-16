module Inventario where

import System.IO
import Control.Exception (try, IOException)
import Tipos
import Utils

--------------------------------------------------------------
-- 1. LÓGICA PURA
--------------------------------------------------------------

adicionarItemInv :: Inventario -> Nome -> Quantidade -> Inventario
adicionarItemInv inv nome qtd =
    case lookup nome inv of
        Nothing -> (nome, qtd) : inv
        Just q0 -> map (\(n,q) -> if n == nome then (n, q0 + qtd) else (n,q)) inv

removerItemInv :: Inventario -> Nome -> Quantidade -> Either String Inventario
removerItemInv inv nome qtd =
    case lookup nome inv of
        Nothing -> Left "Item não encontrado."
        Just qAtual ->
            if qtd > qAtual
                then Left "Quantidade insuficiente."
                else Right (map (\(n,q) ->
                        if n == nome then (n, qAtual - qtd) else (n,q)
                    ) inv)

atualizarItemInv :: Inventario -> Nome -> Quantidade -> Either String Inventario
atualizarItemInv inv nome qtd =
    if qtd < 0 then Left "Quantidade inválida."
    else case lookup nome inv of
        Nothing -> Left "Item não existe."
        Just _  -> Right (map (\(n,q) ->
                    if n == nome then (n, qtd) else (n,q)
                ) inv)

--------------------------------------------------------------
-- 2. PERSISTÊNCIA
--------------------------------------------------------------

salvarInventario :: Inventario -> IO ()
salvarInventario inv =
    writeFile "Inventario.dat" $
        unlines [n ++ ":" ++ show q | (n,q) <- inv]

carregarInventario :: IO Inventario
carregarInventario = do
    txt <- readFileIfExists "Inventario.dat"
    case txt of
        Nothing -> do
            let base = inventarioBase
            salvarInventario base
            return base
        Just raw ->
            return
              [ (n, read q)
              | linha <- lines raw
              , let (n,rest) = break (== ':') linha
              , rest /= ""
              , let q = drop 1 rest
              ]

--------------------------------------------------------------
-- 3. INVENTÁRIO INICIAL OBRIGATÓRIO (>= 10 itens)
--------------------------------------------------------------

inventarioBase :: Inventario
inventarioBase =
    [ ("Arroz",10), ("Feijao",10), ("Macarrao",10), ("Sal",10), ("Acucar",10)
    , ("Alcool",10), ("Agua",10), ("Oleo",10), ("Cafe",10), ("Pao",10)
    ]
