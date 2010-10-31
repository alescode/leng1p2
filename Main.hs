module Main where

import Lienzo
import Instrucciones

ejecutarPrograma :: FilePath -> IO Lienzo
ejecutarPrograma path = do
    archivo <- readFile path 
    return $ interpretarBatch (leerDimensiones $ head $ lines archivo)
           (map leerInstruccion (tail $ lines archivo))
    -- return $ interpretarBatch dimensiones instrucciones 
    --  where lineas = lines archivo
    --        instrucciones = map leerInstruccion lineas
    --        dimensiones = leerDimensiones (head lineas)

exportarLienzo :: Lienzo -> FilePath -> IO ()
exportarLienzo lienzo path = writeFile path (show lienzo)

main = do
    lienzo <- ejecutarPrograma "p.in"
    print lienzo
    return ()

