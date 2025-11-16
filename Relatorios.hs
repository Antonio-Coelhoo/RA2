module Relatorios where

import Tipos
import Data.List (group, sort, maximumBy)
import Data.Ord (comparing)

--------------------------------------------------------------
-- HISTÓRICO POR ITEM
--------------------------------------------------------------

historicoPorItem :: Nome -> [LogEntry] -> [LogEntry]
historicoPorItem nome logs =
    filter (\l -> detalhes l == nome) logs

--------------------------------------------------------------
-- LOGS DE ERRO
--------------------------------------------------------------

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro =
    filter (\l -> case status l of
        Falha _ -> True
        _       -> False)

--------------------------------------------------------------
-- ITEM MAIS MOVIMENTADO
--------------------------------------------------------------

extrairItem :: LogEntry -> Nome
extrairItem = detalhes

itemMaisMovimentado :: [LogEntry] -> Nome
itemMaisMovimentado [] = "Nenhum item registrado"
itemMaisMovimentado logs =
    let itens = sort (map extrairItem logs)
        grupos = group itens
        contagens = [(head g, length g) | g <- grupos]
    in fst (maximumBy (comparing snd) contagens)
