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
    pint amb1@Ambiente { corral=cor1, suciedad = suc1,lrandom=ran1, dimension = (n,m)} amb2@Ambiente{corral=cor2, suciedad = suc2,lrandom=ran2} tmp tmpC cnin crob cobs = if tmp==0
        then do 
            putStrLn "***Fin del Tiempo***"
            pint (generaObstaculo (generaRobot (generaNino (Ambiente {ninos = [], corral = cor1, obstaculo = [], robot = [], suciedad = suc1,lrandom = ran1, dimension = (n,m)}) cnin) crob) cobs) (generaObstaculo (generaRobot (generaNino (Ambiente {ninos = [], corral = cor2, obstaculo = [], robot = [], suciedad = suc2,lrandom = ran2, dimension = (n,m)}) cnin) crob) cobs) tmpC tmpC cnin crob cobs
        else do
            pintaTablero2 amb1 amb2 n m True
            putStr "===Fin del Turno==="
            l <- getLine
            pint (movNin (movRob1 amb1 crob cnin) (cnin - 1)) (movNin (movRob2 amb2 crob cnin) (cnin - 1)) (tmp - 1) tmpC cnin crob cobs