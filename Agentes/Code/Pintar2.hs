module Pintar2
(
    pintaTablero2
)

where

    import System.IO.Unsafe
    import Ambiente
    import Utiles (esta,estaB,estaC,elimina,compara,randomList,direcciones,sumaTupla,cambiar,validoCorral,validoNino,validoRobot,validoObstaculoColoca,validoSuciedad,newPos,validoMovObs,ninosJuntos,direccionComp,arrayMovObs,boolMovObs,ninoR,minBFS,minBool)

    signo :: Ambiente -> (Int,Int) -> IO()
    signo amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom = ran, dimension = (n,m)} (x,y) | (estaB rob (x,y) && (estaB nin (x,y)) && (esta suc (x,y))) = putStr "RNS" | (estaB rob (x,y) && (estaB nin (x,y)) && (esta cor (x,y))) = putStr "RCN" | (estaB rob (x,y) && (esta suc (x,y))) = putStr "RS " | (estaB rob (x,y) && (esta cor (x,y))) = putStr "RC " | (estaB rob (x,y) && (estaB nin (x,y))) = putStr "RN " | (estaB rob (x,y)) = putStr " R " | ((estaB nin (x,y)) && (esta cor (x,y))) = putStr "CN " | (esta cor (x,y)) = putStr " C " | ((estaB nin (x,y)) && (esta suc (x,y))) = putStr "NS " | (estaB nin (x,y)) = putStr " N " | (esta suc (x,y)) = putStr " S " | (esta obs (x,y)) = putStr " O " | otherwise = putStr "   "

    pintaf0:: Int -> Int -> Bool-> IO()
    pintaf0 n cn v | n == 1 =
        if not(v)
            then do
                putStrLn "___ "
            else do
                putStr "___ "
                putStr "               "
                putStr " "
                pintaf0 cn cn False
        | otherwise = do
            putStr "___ "
            pintaf0 (n-1) cn v

    pintaf1:: Ambiente -> Ambiente -> Int -> Int -> Int -> Bool -> IO()
    pintaf1 amb1@Ambiente { dimension = (n,m)} amb2 f c cc v| c==1 =do
        if not(v)
            then do
                signo amb2 (((n+1)-f),m)
                putStrLn "|" 
            else do
                signo amb1 (((n+1)-f),m)
                putStr "|"
                putStr "               "
                putStr "|"
                pintaf1 amb1 amb2 f cc cc False 
        | otherwise = do
            if not(v)
                then do
                    signo amb2 (((n+1)-f), ((m+1)-c))
                    putStr "|"
                    pintaf1 amb1 amb2 f (c-1) cc v
                else do
                    signo amb1 (((n+1)-f), ((m+1)-c))
                    putStr "|"
                    pintaf1 amb1 amb2 f (c-1) cc v

    pintaf2:: Int -> Int -> Bool -> IO()
    pintaf2 n cn v | n==1 =
        if not(v)
            then do
                putStrLn "___|" 
            else do
                putStr "___|"
                putStr "               "
                putStr "|" 
                pintaf2 cn cn False
        | otherwise = do
            putStr "___|"
            pintaf2 (n-1) cn v

    pintaF:: Ambiente -> Ambiente -> Int -> Int -> IO()
    pintaF amb1 amb2 f c = do
        putStr "|"
        pintaf1 amb1 amb2 f c c True
        putStr "|"
        pintaf2 c c True

--PINTA TABLERO RECIBE: Ambiente(amb), Int (cantidad de filas), Int (cantidad de columnas), Bool (True)------------------------- 

    pintaTablero2::Ambiente -> Ambiente -> Int -> Int -> Bool -> IO()
    pintaTablero2 amb1 amb2 f c b | b = do
        putStr " "
        pintaf0 c c True
        pintaTablero2 amb1 amb2 f c False | f == 1 = do
            pintaF amb1 amb2 f c | otherwise = do
                pintaF amb1 amb2 f c
                pintaTablero2 amb1 amb2 (f-1) c False