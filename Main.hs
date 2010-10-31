module Main where

import Lienzo
import Instrucciones

ejecutarPrograma :: FilePath -> IO Lienzo
ejecutarPrograma path = do
    archivo <- readFile path 
    let lineas = lines archivo
        instrucciones = map leerInstruccion lineas
        dimensiones = leerDimensiones (head lineas)
    return $ interpretarBatch dimensiones instrucciones 

exportarLienzo :: Lienzo -> FilePath -> IO ()
exportarLienzo lienzo path = writeFile path (show lienzo)
