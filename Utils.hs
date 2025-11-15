module Utils where


registrarLog :: String -> IO ()
registrarLog msg = appendFile "Auditoria.log" (msg ++ "\n")