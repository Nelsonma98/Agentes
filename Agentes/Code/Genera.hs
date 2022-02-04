module Genera
(
    generaCorral,
    generaNino,
    generaRobot,
    generaObstaculo,
    generaSuciedadInicial,
    generaSucNin
)
where
    import System.Random
    import Ambiente
    import Utiles (esta,estaB,estaC,elimina,compara,randomList,direcciones,sumaTupla,cambiar,validoCorral,validoNino,validoRobot,validoObstaculoColoca,validoSuciedad,newPos,validoMovObs,ninosJuntos,direccionComp,arrayMovObs,boolMovObs,ninoR,minBFS,minBool)


--GENERAN EL CORRAL EN LAS PRIMERAS FILAS (La 1ra posicion que se pasa es (1,1))------

    generaCorral:: Ambiente -> (Int,Int) -> Int -> Ambiente
    generaCorral amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=ran, dimension = (n,m)} (x,y) can | can==1 =
        Ambiente { ninos = nin, corral= (x,y): cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=ran, dimension = (n,m)}| otherwise =
            if validoCorral amb (sumaTupla (x,y) (direcciones 1))
                then generaCorral (Ambiente { ninos = nin, corral= (x,y): cor, obstaculo = obs, robot = rob, suciedad = suc, lrandom=ran,dimension = (n,m)}) (sumaTupla (x,y) (direcciones 1)) (can-1)
                else generaCorral (Ambiente { ninos = nin, corral= (x,y): cor, obstaculo = obs, robot = rob, suciedad = suc, lrandom=ran,dimension = (n,m)}) (x+1,1) (can-1)

------------------------------------------------------------------------ 
--GENERA LOS NINOS EN EL AMBIENTE --------------------------------------

    generaNino:: Ambiente -> Int -> Ambiente
    generaNino amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=(x:y:xs), dimension = (n,m)} cant | cant==1 =
        if validoNino amb (((mod x n)+1),((mod y m)+1))
            then (if (esta cor (((mod x n)+1),((mod y m)+1))) then Ambiente { ninos = (((mod x n)+1),((mod y m)+1),False):nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=xs, dimension = (n,m)} else Ambiente { ninos = (((mod x n)+1),((mod y m)+1),True):nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=xs, dimension = (n,m)})
            else generaNino (Ambiente { ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=xs, dimension = (n,m)}) cant 
        | otherwise =
            if validoNino amb (((mod x n)+1),((mod y m)+1))
                then (if (esta cor (((mod x n)+1),((mod y m)+1))) then generaNino (Ambiente { ninos = (((mod x n)+1),((mod y m)+1),False):nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=xs, dimension = (n,m)}) (cant - 1) else generaNino (Ambiente { ninos = (((mod x n)+1),((mod y m)+1),True):nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=xs, dimension = (n,m)}) (cant - 1))
                else generaNino (Ambiente { ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=xs, dimension = (n,m)}) cant

------------------------------------------------------------------------

--GENERA LOS ROBOTS EN EL AMBIENTE --------------------------------------

    generaRobot::  Ambiente -> Int -> Ambiente
    generaRobot amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=(x:y:xs), dimension = (n,m)} cant | cant==1 =
        if validoRobot amb (((mod x n)+1),((mod y m)+1))
            then Ambiente { ninos = nin, corral = cor, obstaculo = obs, robot = (((mod x n)+1),((mod y m)+1),True):rob, suciedad = suc,lrandom= xs, dimension = (n,m)}
            else generaRobot (Ambiente { ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=xs, dimension = (n,m)}) cant 
        | otherwise =
            if validoRobot amb (((mod x n)+1),((mod y m)+1))
                then generaRobot (Ambiente { ninos = nin, corral = cor, obstaculo = obs, robot = (((mod x n)+1),((mod y m)+1),True):rob, suciedad = suc,lrandom=xs, dimension = (n,m)}) (cant-1)
                else generaRobot (Ambiente { ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=xs, dimension = (n,m)}) cant

------------------------------------------------------------------------

--GENERA LOS OBSTACULOS EN EL AMBIENTE--------------------------------------

    generaObstaculo::  Ambiente -> Int -> Ambiente
    generaObstaculo amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=(x:y:xs), dimension = (n,m)} cant | cant==1 =
        if validoObstaculoColoca amb (((mod x n)+1),((mod y m)+1))
            then Ambiente { ninos = nin, corral = cor, obstaculo = (((mod x n)+1),((mod y m)+1)):obs, robot = rob, suciedad = suc,lrandom=xs, dimension = (n,m)}
            else generaObstaculo (Ambiente { ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=xs, dimension = (n,m)}) cant 
        | otherwise =
            if validoObstaculoColoca amb (((mod x n)+1),((mod y m)+1))
                then generaObstaculo (Ambiente { ninos = nin, corral = cor, obstaculo = (((mod x n)+1),((mod y m)+1)):obs, robot = rob, suciedad = suc,lrandom=xs, dimension = (n,m)}) (cant-1)
                else generaObstaculo (Ambiente { ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=xs, dimension = (n,m)}) cant

------------------------------------------------------------------------

--GENERA LA SUCIEDAD INICIAL---------------------------------------------------
    generaSuciedadInicial::  Ambiente -> Int -> Ambiente
    generaSuciedadInicial amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=(x:y:xs), dimension = (n,m)} cant | cant==1 =
        if validoSuciedad amb (((mod x n)+1),((mod y m)+1))
            then Ambiente { ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = (((mod x n)+1),((mod y m)+1)):suc,lrandom=xs, dimension = (n,m)}
            else generaSuciedadInicial (Ambiente { ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=xs, dimension = (n,m)}) cant 
        | otherwise =
            if validoSuciedad amb (((mod x n)+1),((mod y m)+1))
                then generaSuciedadInicial (Ambiente { ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = (((mod x n)+1),((mod y m)+1)):suc,lrandom=xs, dimension = (n,m)}) (cant-1)
                else generaSuciedadInicial (Ambiente { ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=xs, dimension = (n,m)}) cant

-----------------------------------------------------------------------------

--GENERA LA SUCIEDAD DE NINOS------------------------------------------------

    generaSucNin:: Ambiente -> (Int,Int) -> Int -> Ambiente
    generaSucNin amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=(r:xs), dimension = (n,m)} (x,y) k | k==1 =
        let
            (sx,sy) = sumaTupla (x,y) (direccionComp (mod r 8))
        in if (validoSuciedad amb (sx,sy)) then Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = (sx,sy):suc,lrandom=xs, dimension = (n,m)} else Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=xs, dimension = (n,m)} 
        | otherwise =
            let
                (sx,sy) = sumaTupla (x,y) (direccionComp (mod r 8))
            in if (validoSuciedad amb (sx,sy)) then generaSucNin (Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = (sx,sy):suc,lrandom=xs, dimension = (n,m)}) (x,y) (k-1) else generaSucNin (Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=xs, dimension = (n,m)}) (x,y) (k-1)
-----------------------------------------------------------------------------