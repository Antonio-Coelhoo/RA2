module Tipos where

type Nome = String
type Quantidade = Int
type Inventario = [(Nome, Quantidade)]

data Acao = Adicionar | Remover | Atualizar | Report
    deriving (Show, Read, Eq)

data StatusLog = Sucesso | Falha String
    deriving (Show, Read, Eq)

data LogEntry = LogEntry
    { timestamp :: String
    , acao      :: Acao
    , detalhes  :: String      -- GUARDA SOMENTE O ITEM
    , status    :: StatusLog
    }
    deriving (Show, Read, Eq)
