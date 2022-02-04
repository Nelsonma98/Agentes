module Mover
(
    movObs,
    movNin
)
where
    import System.Random
    import Ambiente
    import Utiles (esta,estaB,estaC,elimina,compara,randomList,direcciones,sumaTupla,cambiar,validoCorral,validoNino,validoRobot,validoObstaculoColoca,validoSuciedad,newPos,validoMovObs,ninosJuntos,direccionComp,arrayMovObs,boolMovObs,ninoR,minBFS,minBool)
    import Genera (generaSucNin)
    import System.IO.Unsafe

    movObs:: Ambiente -> (Int,Int) -> Int -> ([(Int,Int)],Bool)
    movObs amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=ran, dimension = (n,m)} pi d | (esta obs (newPos pi d)) && (validoMovObs (movObs amb (newPos pi d) d)) =
        (cambiar (cambiar obs (newPos pi d) (newPos (newPos pi d) d)) pi (newPos pi d),True) 
        | validoObstaculoColoca amb (newPos pi d) && not(esta suc (newPos pi d)) =
            ((cambiar obs pi (newPos pi d)),True)| otherwise = (obs,False)

    ninLib:: Ambiente -> (Int,Int,Bool) -> Ambiente
    ninLib amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob,lrandom = (r:rnd), suciedad = suc, dimension = (n,m)} (x1,y1,b) =
        let
            (x2,y2)= sumaTupla (x1,y1) (direcciones ((mod r 4)))
            ambC = Ambiente {ninos = (cambiar nin (x1,y1,b) (x2,y2,b)), corral = cor, obstaculo = obs, robot = rob,lrandom = rnd, suciedad = suc, dimension = (n,m)}
            ambSC = Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob,lrandom = rnd, suciedad = suc, dimension = (n,m)}
        in
            if ((validoNino amb (x2,y2)) && not(esta cor (x2,y2)) && not(esta obs (x2,y2)))
                then generaSucNin ambC (x2,y2) (ninosJuntos ambC (x2,y2) 0 0)
                else 
                    if ((esta obs (x2,y2)) && (boolMovObs(movObs amb (x2,y2) (mod r 4))) ) 
                        then (generaSucNin (Ambiente {ninos = (cambiar nin (x1,y1,b) (x2,y2,b)), corral = cor, obstaculo = (arrayMovObs(movObs amb (x2,y2) (mod r 4))), robot = rob,lrandom = rnd, suciedad = suc, dimension = (n,m)}) (x2,y2) (ninosJuntos (Ambiente {ninos = (cambiar nin (x1,y1,b) (x2,y2,b)), corral = cor, obstaculo = (arrayMovObs(movObs amb (x2,y2) (mod r 4))), robot = rob,lrandom = rnd, suciedad = suc, dimension = (n,m)}) (x2,y2) 0 0) ) 
                        else generaSucNin ambSC (x1,y1) (ninosJuntos ambSC (x1,y1) 0 0)

--LOS NINOS SE MUEVEN Y GENERAN LA SUCIEDAD (Siempre que se puedan mover)=============================
    movNin:: Ambiente -> Int -> Ambiente
    movNin amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob,lrandom = (r:rnd), suciedad = suc, dimension = (n,m)} cntN | cntN == 0 =
        let
            (x1,y1,b)= nin !! cntN
        in
            if b then ninLib amb (x1,y1,b) else amb 
        | otherwise =
            let
                (x1,y1,b)= nin !! cntN
            in
                if b then movNin (ninLib amb (x1,y1,b)) (cntN-1) else movNin amb (cntN-1)