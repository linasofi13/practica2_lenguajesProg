import Data.List
import qualified Data.Char 
import Control.Monad.Cont
import Control.Monad
import Text.Printf

solveRPN :: String -> Float  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs  
            foldingFunction xs "sum" = [sum xs]  
            foldingFunction xs numberString = read numberString:xs    

count :: Char -> String -> Int -- numero de ocurrencias de un caracter
count x [] = 0
count x (c:cs) | x == c = 1 + count x cs
               | otherwise = count x cs

isNumber :: String -> Bool
isNumber s = case (reads s) :: [(Int, String)] of
    [(_, "")] -> True
    _         -> False

intsCount :: [String] -> Int 
intsCount = length . filter isNumber

--notacionPolaca x = printf "%d! = %d \n" x $ solveRPN x -- %d -> decimal | %f -> float | %s -> string

main :: IO ()
main = do
    putStrLn "Bienvenido a la Calculadora de Notacion Polaca Inversa de Haskell"
    putStrLn "-----Ingrese una expresion posfija para continuar-----"
    putStrLn "--Un ejemplo de una expresion puede ser: 2 3 + "
    putStrLn "-----Por favor ingrese numeros de tipo Int(Enteros) -----"
    putStrLn "-----Recuerde que debe ingresar una expresion valida-----"
    putStrLn "-----Presione 0 para Salir-----"
    line <- getLine
    if line == "0"
       then return ()
        else do -- verificacion de la expresion
        let lengthLine = length line
        if lengthLine < 2
            then main
            else do
                let listLine = words line
                let cantidadInts = intsCount listLine
                let cantidadMas = count '+' line
                let cantidadMenos = count '-' line
                let cantidadMulti = count '*' line
                let cantidadDiv = count '/' line
                let cantidadOperadores = cantidadMas + cantidadMenos + cantidadMulti + cantidadDiv
                if cantidadOperadores >= cantidadInts
                    then main
                    else do
                        let cantidadEspacios = count ' ' line
                        let cantidadOperadoresInts = cantidadInts + cantidadOperadores
                        if cantidadEspacios /= (cantidadOperadoresInts-1)
                            then main
                            else do
                                let primerIndex = head line
                                let slicing = tail line
                                let segundoIndex = head slicing
                                if primerIndex == '-' && segundoIndex == ' '
                                    then main
                                    else do
                                        printf " |  -  Resultado: %f  -   |" $ solveRPN line
                                        putStrLn " "
                                        main 

                                
