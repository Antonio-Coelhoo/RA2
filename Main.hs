module Main where

import Tipos
import Inventario
import Relatorios
import Utils
import System.IO

--------------------------------------------------------------
-- MAIN
--------------------------------------------------------------

main :: IO ()
main = do
    inv <- carregarInventario
    logs <- carregarLogs
    menu inv logs

carregarLogs :: IO [LogEntry]
carregarLogs = do
    txt <- readFileIfExists "Auditoria.log"
    case txt of
        Nothing -> return []
        Just raw -> return (map read (lines raw))

--------------------------------------------------------------
-- MENU
--------------------------------------------------------------

menu :: Inventario -> [LogEntry] -> IO ()
menu inv logs = do
    putStrLn "\n=== Sistema de Inventário ==="
    putStrLn "1 - Adicionar Item"
    putStrLn "2 - Remover Item"
    putStrLn "3 - Atualizar Item"
    putStrLn "4 - Listar"
    putStrLn "5 - Report"
    putStrLn "6 - Sair"
    putStr "Escolha: "
    op <- getLine

    case op of
        "1" -> fluxoAdicionar inv logs
        "2" -> fluxoRemover inv logs
        "3" -> fluxoAtualizar inv logs
        "4" -> listar inv >> menu inv logs
        "5" -> fluxoReport inv logs
        "6" -> putStrLn "Encerrando..."
        _   -> menu inv logs

--------------------------------------------------------------
-- LISTAR
--------------------------------------------------------------

listar :: Inventario -> IO ()
listar inv =
    mapM_ (\(n,q) -> putStrLn (n ++ " - " ++ show q)) inv

--------------------------------------------------------------
-- ADICIONAR
--------------------------------------------------------------

fluxoAdicionar :: Inventario -> [LogEntry] -> IO ()
fluxoAdicionar inv logs = do
    putStrLn "Nome:"
    nome <- getLine
    putStrLn "Quantidade:"
    qtd <- readLn

    let novoInv = adicionarItemInv inv nome qtd

    ts <- timestampNow
    let logEntry = LogEntry ts Adicionar nome Sucesso
    registrarLog logEntry

    salvarInventario novoInv
    menu novoInv (logEntry:logs)

--------------------------------------------------------------
-- REMOVER
--------------------------------------------------------------

fluxoRemover :: Inventario -> [LogEntry] -> IO ()
fluxoRemover inv logs = do
    putStrLn "Nome:"
    nome <- getLine
    putStrLn "Quantidade:"
    qtd <- readLn

    ts <- timestampNow

    case removerItemInv inv nome qtd of
        Left erro -> do
            let e = LogEntry ts Remover nome (Falha erro)
            registrarLog e
            putStrLn erro
            menu inv (e:logs)

        Right novoInv -> do
            let e = LogEntry ts Remover nome Sucesso
            registrarLog e
            salvarInventario novoInv
            menu novoInv (e:logs)

--------------------------------------------------------------
-- ATUALIZAR
--------------------------------------------------------------

fluxoAtualizar :: Inventario -> [LogEntry] -> IO ()
fluxoAtualizar inv logs = do
    putStrLn "Nome:"
    nome <- getLine
    putStrLn "Nova quantidade:"
    qtd <- readLn

    ts <- timestampNow

    case atualizarItemInv inv nome qtd of
        Left erro -> do
            let e = LogEntry ts Atualizar nome (Falha erro)
            registrarLog e
            putStrLn erro
            menu inv (e:logs)

        Right novoInv -> do
            let e = LogEntry ts Atualizar nome Sucesso
            registrarLog e
            salvarInventario novoInv
            menu novoInv (e:logs)

--------------------------------------------------------------
-- RELATÓRIO
--------------------------------------------------------------

fluxoReport :: Inventario -> [LogEntry] -> IO ()
fluxoReport inv logs = do
    putStrLn "\n=== RELATÓRIO ==="

    putStrLn "\nErros registrados:"
    mapM_ print (logsDeErro logs)

    putStrLn "\nItem mais movimentado:"
    print (itemMaisMovimentado logs)

    putStrLn "\nDigite um item para ver o histórico:"
    nome <- getLine
    print (historicoPorItem nome logs)

    ts <- timestampNow
    let e = LogEntry ts Report "Report" Sucesso
    registrarLog e

    menu inv (e:logs)
