module Main(
)

where

    --import Pintar(pintaTablero)
    import Pintar2(pintaTablero2)
    import Utiles(esta,estaB,estaC,elimina,compara,randomList,direcciones,sumaTupla,cambiar,validoCorral,validoNino,validoRobot,validoObstaculoColoca,validoSuciedad,newPos,validoMovObs,ninosJuntos,direccionComp,arrayMovObs,boolMovObs,ninoR,minBFS,minBool)
    import Genera(generaCorral,generaNino,generaRobot,generaObstaculo,generaSuciedadInicial,)
    import Mover(movNin)
    import Robot2(movRob2)
    import Robot1(movRob1)
    import System.Random
    import System.IO.Unsafe
    import Ambiente

    -- main: Int(cantidad de filas), Int(cantidad de columnas), Int(tiempo de variacion de ambiente), Int(cantidad de Robots)
    main:: Int -> Int -> Int ->Int -> IO()
    main f c tmp cr =
        let
            (_,gen)= random (mkStdGen ((mod (f*2311) c)+1))::(Int,StdGen)
            listRandom = randomList 0 1000 gen
            amb = Ambiente {ninos = [], corral = [], obstaculo = [], robot = [], suciedad = [],lrandom = listRandom, dimension = (f,c)}
            (cnin,gen1)=randomR (1,(div (f*c) 4))gen::(Int,StdGen)
            (csuc,gen2)=randomR (1,(min f c))gen1::(Int,StdGen)
            (cobs,gen3)=randomR (1,(min f c))gen2::(Int,StdGen)
            ambF = generaSuciedadInicial (generaObstaculo (generaRobot (generaNino (generaCorral amb (1,1) cnin) cnin) cr) cobs) csuc
        in
            pint ambF ambF tmp tmp cnin cr cobs
        
    
    pint::Ambiente -> Ambiente -> Int -> Int -> Int -> Int -> Int -> IO()
    pint amb1@Ambiente {ninos = nin1, corral = cor1, obstaculo = obs1, robot = rob1, suciedad = suc1,lrandom=ran1, dimension = (n1,m1)} amb2@Ambiente{ninos = nin2, corral = cor2, obstaculo = obs2, robot = rob2, suciedad = suc2,lrandom=ran2, dimension = (n2,m2)} tmp tmpC cnin crob cobs = if (tmp==0) 
        then do 
            putStrLn "***Fin del Tiempo***"
            pint (generaObstaculo (generaRobot (generaNino (Ambiente {ninos = [], corral = cor1, obstaculo = [], robot = [], suciedad = suc1,lrandom = ran1, dimension = (n1,m1)}) cnin) crob) cobs) (generaObstaculo (generaRobot (generaNino (Ambiente {ninos = [], corral = cor2, obstaculo = [], robot = [], suciedad = suc2,lrandom = ran2, dimension = (n2,m2)}) cnin) crob) cobs) tmpC tmpC cnin crob cobs
        else do
            pintaTablero2 amb1 amb2 n1 m1 True
            if ((length suc1) > (div (3*((n1*m1) - ((length nin1)+(length cor1)+(length obs1)+(length rob1)))) 5)) && ((length suc2) > (div (3*((n2*m2) - ((length nin2)+(length cor2)+(length obs2)+(length rob2)))) 5)) then putStr "Los tableros  superaron el 60% de suciedad" else if ((length suc1) > (div (3*((n1*m1) - ((length nin1)+(length cor1)+(length obs1)+(length rob1)))) 5)) then putStr "El primer tablero superó el 60% de suciedad" else if ((length suc2) > (div (3*((n2*m2) - ((length nin2)+(length cor2)+(length obs2)+(length rob2)))) 5)) then putStr "El segundo tablero superó el 60% de suciedad" else putStr "===Fin del Turno==="
            l <- getLine
            pint (movNin (movRob1 amb1 crob cnin) (cnin - 1)) (movNin (movRob2 amb2 crob cnin) (cnin - 1)) (tmp - 1) tmpC cnin crob cobs