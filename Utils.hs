module Utils where

import System.IO
import Control.Exception (try, IOException)
import Tipos

--------------------------------------------------------------
-- REGISTRAR LOG
--------------------------------------------------------------

registrarLog :: LogEntry -> IO ()
registrarLog entry =
    appendFile "Auditoria.log" (show entry ++ "\n")

--------------------------------------------------------------
-- LER ARQUIVO DE FORMA SEGURA
--------------------------------------------------------------

readFileIfExists :: FilePath -> IO (Maybe String)
readFileIfExists path = do
    mh <- tryOpen path
    case mh of
        Nothing -> return Nothing
        Just h  -> do
            txt <- hGetContents h
            length txt `seq` hClose h
            return (Just txt)

tryOpen :: FilePath -> IO (Maybe Handle)
tryOpen f = do
    resultado <- try (openFile f ReadMode) :: IO (Either IOException Handle)
    case resultado of
        Left _  -> return Nothing
        Right h -> return (Just h)

--------------------------------------------------------------
-- TIMESTAMP SIMPLIFICADO (aceito no trabalho)
--------------------------------------------------------------

timestampNow :: IO String
timestampNow = pure "<timestamp>"
