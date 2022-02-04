module Robot1
(
    movRob1
)
where
    import System.Random
    import Ambiente
    import Utiles (esta,estaB,estaC,elimina,compara,randomList,direcciones,sumaTupla,cambiar,validoCorral,validoNino,validoRobot,validoObstaculoColoca,validoSuciedad,newPos,validoMovObs,ninosJuntos,direccionComp,arrayMovObs,boolMovObs,ninoR,minBFS,sig,minBool,corralLibre,dejaNin,bfsC,soltarN)
    import Genera (generaSucNin)
    import System.IO.Unsafe

    bfsNS:: Ambiente -> (Int,Int) -> [(Int,Int)] -> ((Int,Int),Bool,Int)
    bfsNS amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=ran, dimension = (n,m)} (x,y) vis | (esta vis (x,y)) || (estaB rob (x,y)) || (esta obs (x,y)) || ((esta cor (x,y)) && (estaB nin (x,y))) || (x < 1) || (x > n) || (y < 1) || (y > m) = ((0,0),False,-1) | (estaB nin (x,y)) || (esta suc (x,y)) = ((x,y),True,0) | otherwise = 
        let
            (t,b,p) = minBFS ((bfsNS amb (x-1,y) ((x,y):vis)):(bfsNS amb (x,y+1) ((x,y):vis)):(bfsNS amb (x+1,y) ((x,y):vis)):(bfsNS amb (x,y-1) ((x,y):vis)) : []) (x,y)
        in if b then (t,b,p+1) else (t,b,p)

    sinNin::Ambiente -> (Int,Int,Bool) -> Int -> Ambiente
    sinNin amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob,lrandom = rnd, suciedad = suc, dimension = (n,m)} (xr,yr,br) cntN =
        let
            ((cxN,cyN),b1) = ninoR amb (xr,yr)

            ((bxNS,byNS),b2,_) = sig ((bfsNS amb (xr-1,yr) ((xr,yr):[])):(bfsNS amb (xr,yr+1) ((xr,yr):[])):(bfsNS amb (xr+1,yr) ((xr,yr):[])):(bfsNS amb (xr,yr-1) ((xr,yr):[])) : [])
        in
            if b1
                then
                    Ambiente {ninos = cambiar nin (cxN,cyN,True) (xr,yr,False), corral =cor, obstaculo = obs, robot = cambiar rob (xr,yr,br) (xr,yr,False), lrandom = rnd, suciedad = suc, dimension = (n,m) }
                else if b2
                    then 
                        Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = cambiar rob (xr,yr,br) (bxNS,byNS,True),lrandom = rnd, suciedad = elimina suc (bxNS,byNS), dimension = (n,m)}
                    else amb

    conNin:: Ambiente -> (Int,Int,Bool) -> Int -> Ambiente
    conNin amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob,lrandom = rnd, suciedad = suc, dimension = (n,m)} (xr,yr,br) cntN =
        let
            ((lxr,lyr),b4) = dejaNin amb (xr,yr) cntN
        in
            if (soltarN amb (xr,yr) cntN)
                then
                    Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = cambiar rob (xr,yr,False) (xr,yr,True),lrandom = rnd, suciedad = suc, dimension = (n,m)}
                else if b4
                        then 
                            let
                                ((newx,newy),newb) = dejaNin amb (lxr,lyr) cntN
                            in
                                if (soltarN amb (lxr,lyr) cntN)
                                    then
                                        Ambiente {ninos = cambiar nin (xr,yr,False) (lxr,lyr,False), corral = cor, obstaculo = obs, robot = cambiar rob (xr,yr,False) (lxr,lyr,True),lrandom = rnd, suciedad = suc, dimension = (n,m)}
                                    else if newb
                                        then
                                            Ambiente {ninos = cambiar nin (xr,yr,False) (newx,newy,False), corral = cor, obstaculo = obs, robot = cambiar rob (xr,yr,False) (newx,newy,False),lrandom = rnd, suciedad = suc, dimension = (n,m)}
                                        else
                                            Ambiente {ninos = cambiar nin (xr,yr,False) (lxr,lyr,False), corral = cor, obstaculo = obs, robot = cambiar rob (xr,yr,False) (lxr,lyr,False),lrandom = rnd, suciedad = suc, dimension = (n,m)}
                        else amb

    movRob1:: Ambiente -> Int -> Int -> Ambiente
    movRob1 amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob,lrandom = rnd, suciedad = suc, dimension = (n,m)} cntR cntN | cntR ==1 =
        let
            (xr,yr,br) = rob !! (cntR-1)
        in
            if br then sinNin amb (xr,yr,br) cntN else conNin amb (xr,yr,br) cntN | otherwise =
                let
                    (xr,yr,br) = rob !! (cntR-1)
                in
                    if br then movRob1 (sinNin amb (xr,yr,br) cntN) (cntR-1) cntN else movRob1 (conNin amb (xr,yr,br) cntN) (cntR-1) cntN