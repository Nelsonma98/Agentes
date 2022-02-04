module Utiles
(
    esta,
    estaB,
    estaC,
    elimina,
    compara,
    randomList,
    direcciones,
    direccionComp,
    sumaTupla,
    cambiar,
    validoCorral,
    validoNino,
    validoRobot,
    validoObstaculoColoca,
    validoSuciedad,
    newPos,
    validoMovObs,
    ninosJuntos,
    arrayMovObs,
    boolMovObs,
    minBFS,
    sig,
    minBool,
    ninoR,
    corralLibre,
    dejaNin,
    bfsC,
    soltarN
)

where

    import System.Random
    import Ambiente

    esta:: (Eq a) => [a] -> a -> Bool
    esta [] _ = False
    esta (x:xs) y = if x==y then True else esta xs y

--estaB: IGNORA EL BOOL DE LAS TUPLAS DEL ARRAY--------------------------------------------------------------------

    estaB:: [(Int,Int,Bool)]->(Int,Int)->Bool
    estaB [] _ = False
    estaB (x:xs) y = if compara x y then True else estaB xs y

--estaC: DEVUELVE EL BOOL DE LA TUPLA DEL ARRAY QUE COINCIDA CON LA TUPLA QUE SE PASA-------------------------------

    estaC:: [(Int,Int,Bool)] -> (Int,Int) -> Bool
    estaC [] _ = False
    estaC ((a1,b1,c1):xs) (a2,b2) | (a1==a2 && b1==b2) = c1 | otherwise = estaC xs (a2,b2)

    elimina:: (Eq a) => [a] -> a -> [a]
    elimina [] _ = []
    elimina [x] n = if n==x then [] else [x]
    elimina (x:xs) n = if n == x then xs else x:elimina xs n

--compara: IGNORA EL BOOL DE LAS TUPLAS DEL ARRAY--------------------------------------------------------------------

    compara:: (Int,Int,Bool)-> (Int,Int) ->Bool
    compara t1 t2 =
        let
            (a1,b1,c1)=t1
            (a2,b2)=t2
        in
            if ((a1==a2) && (b1==b2)) then True else False

    randomList ::Int -> Int-> StdGen -> [Int]
    randomList i f = randomRs (i, f)

    direcciones:: Int -> (Int,Int)
    direcciones n = [(-1,0),(0,1),(1,0),(0,-1)] !! n

    direccionComp :: Int -> (Int,Int)
    direccionComp n = [(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1),(-1,-1)] !! n

    sumaTupla:: (Int, Int)->(Int,Int)->(Int,Int)
    sumaTupla (a1,b1) (a2,b2) = (a1+a2, b1+b2)

    cambiar:: (Eq a) => [a] -> a -> a ->[a]
    cambiar [] _ _ = []
    cambiar (x:xs) a b = if x==a then b:xs else x:(cambiar xs a b)

    newPos:: (Int,Int) -> Int -> (Int,Int)
    newPos (x,y) d =
        let
            (a,b)= direcciones d
        in
            ((x+a),(y+b))

-- ninosJuntos: Ambiente, (int,int), int = 0 , int = 0 -------------------------
    ninosJuntos:: Ambiente -> (Int,Int) -> Int -> Int -> Int
    ninosJuntos amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=ran, dimension = (n,m)} (x,y) k acum | k==7 =
        let
            (nx,ny)= sumaTupla (x,y) (direccionComp k)
        in if (estaB nin (nx,ny)) && (estaC nin (nx,ny)) then (ensucia (acum + 1)) else ensucia acum 
        | otherwise =
            let
                (nx,ny)= sumaTupla (x,y) (direccionComp k)
            in if (estaB nin (nx,ny)) && (estaC nin (nx,ny)) then ninosJuntos amb (x,y) (k+1) (acum +1) else ninosJuntos amb (x,y) (k+1) acum

    ensucia:: Int -> Int
    ensucia a | a==0 = 1 | a==1 = 3 | otherwise = 6

    arrayMovObs:: ([(Int,Int)],Bool) -> [(Int,Int)]
    arrayMovObs (a,b) = a

    boolMovObs:: ([(Int,Int)],Bool) -> Bool
    boolMovObs (a,b) = b

    minBool:: ((Int,Int),Bool,Int) -> ((Int,Int),Bool,Int) -> ((Int,Int),Bool,Int)
    minBool (a1,b1,c1) (a2,b2,c2) | b1 && b2 = if c1 < c2 then (a1,b1,c1) else (a2,b2,c2) | b1 = (a1,b1,c1) | b2 = (a2,b2,c2) | otherwise = ((0,0),False,-1)

    minBFS:: [((Int,Int),Bool,Int)] -> (Int,Int) -> ((Int,Int),Bool,Int)
    minBFS [] _ = ((0,0),False,-1)
    minBFS ((a,b,c):xs) pos = if b then minBool (pos,b,c) (minBFS xs pos) else minBFS xs pos

    sig:: [((Int,Int),Bool,Int)] -> ((Int,Int),Bool,Int)
    sig [] = ((0,0),False,-1)
    sig ((a,b,c):xs) = if b then minBool (a,b,c) (sig xs) else sig xs


--Revisa si un robot tiene un nino a su lado
    ninoR::Ambiente -> (Int,Int) -> ((Int,Int),Bool)
    ninoR amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=ran, dimension = (n,m)} (x,y) = if (estaB nin ((x - 1),y)) && not(esta cor ((x - 1),y)) && not(estaB nin (x,y)) && not(estaB rob ((x - 1),y))  then (((x - 1),y),True) else
        if (estaB nin ((x + 1),y)) && not(esta cor ((x + 1),y)) && not(estaB nin (x,y)) && not(estaB rob ((x + 1),y)) then (((x + 1),y),True) else
            if (estaB nin (x,(y - 1))) && not(esta cor (x,(y - 1))) && not(estaB nin (x,y)) && not(estaB rob ((x),y-1)) then ((x,(y - 1)),True) else
                if (estaB nin (x,(y + 1))) && not(esta cor (x,(y + 1))) && not(estaB nin (x,y)) && not(estaB rob ((x),y+1)) then ((x,(y + 1)),True) else ((0,0),False)

    corralLibre:: Ambiente -> (Int,Int) -> Bool
    corralLibre amb@Ambiente{ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=ran, dimension = (n,m)} (x,y) = (not(estaB nin (x,y)) && not(estaB rob (x,y)))

--Cuando se lleva un nino al corral este metodo entrega el paso que deberia dar si hay camino y si hay camino
    dejaNin:: Ambiente -> (Int,Int) -> Int -> ((Int,Int),Bool)
    dejaNin _ _ 0 = ((0,0),False)
    dejaNin amb@Ambiente{ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=ran, dimension = (n,m)} (x,y) c =
        let
            (nx,ny,nz) = sig ( (bfsC amb ((x - 1),y) (cor !! (c - 1)) ((x,y):(x,y):[])) : (bfsC amb (x,(y + 1)) (cor !! (c - 1)) ((x,y):(x,y):[]) ) : (bfsC amb ((x + 1),y) (cor !! (c - 1)) ((x,y):(x,y):[])) : (bfsC amb (x,(y - 1)) (cor !! (c - 1)) ((x,y):(x,y):[])) : [] )
        in
            if (corralLibre amb (cor !! (c - 1))) && ny then (nx ,True) else dejaNin amb (x,y) (c - 1)

    bfsC:: Ambiente -> (Int,Int) -> (Int,Int) -> [(Int,Int)] -> ((Int,Int),Bool,Int)
    bfsC amb@Ambiente{ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=ran, dimension = (n,m)} (x,y) (c1,c2) vis = if (esta vis (x,y)) || (estaB rob (x,y)) || (esta obs (x,y)) || (estaB nin (x,y)) || (x < 1) || (x > n) || (y < 1) || (y > m) then ((0,0),False,-1) else if (x==c1) && (y==c2) then ((x,y),True,0) else
        let
            (t,b,p) = minBFS ( (bfsC amb ((x - 1),y) (c1,c2) ((x,y):vis)) : (bfsC amb (x,(y + 1)) (c1,c2) ((x,y):vis) ) : (bfsC amb ((x + 1),y) (c1,c2) ((x,y):vis)) : (bfsC amb (x,(y - 1)) (c1,c2) ((x,y):vis)) : [] ) (x,y)
        in if b then (t,b,(p + 1)) else (t,b,p)

--Si un robot ya puede soltar al nino----------------------------------------------
    soltarN:: Ambiente -> (Int,Int) -> Int -> Bool
    soltarN amb@Ambiente{corral = cor} pos cntC | cntC == 1 =
        if (cor !! (cntC - 1)) == pos then True else False | otherwise =
            if corralLibre amb (cor !! (cntC-1)) then False else
                if (cor !! (cntC-1)) == pos then True else soltarN amb pos (cntC-1)












--POSICIONES VALIDAS =================================================================

    validoCorral:: Ambiente -> (Int,Int) -> Bool
    validoCorral amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=ran, dimension = (n,m)} (x,y) = ( (0 < x) && (x < (n+1)) && (0 < y) && (y < (m+1)) && not(esta cor (x,y)))

    validoNino:: Ambiente -> (Int,Int) -> Bool
    validoNino amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=ran, dimension = (n,m)} (x,y) = ((0 < x) && (x < (n+1)) && (0 < y) && (y < (m+1)) && not(estaB nin (x,y)) && not(estaB rob (x,y)))

    validoRobot:: Ambiente -> (Int,Int) -> Bool
    validoRobot amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=ran, dimension = (n,m)} (x,y) = ((0<x) && (x<(n+1)) && (0<y) && (y<(m+1)) && not(estaB nin (x,y)) && not(estaB rob (x,y)) && not(esta obs (x,y)))

    validoObstaculoColoca:: Ambiente -> (Int,Int) -> Bool
    validoObstaculoColoca amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=ran, dimension = (n,m)} (x,y) = ((0<x) && (x<(n+1)) && (0<y) && (y<(m+1)) && not(estaB nin (x,y)) && not(estaB rob (x,y)) && not(esta obs (x,y)) && not(esta cor (x,y)))

    validoSuciedad:: Ambiente -> (Int,Int) -> Bool
    validoSuciedad amb@Ambiente {ninos = nin, corral = cor, obstaculo = obs, robot = rob, suciedad = suc,lrandom=ran, dimension = (n,m)} (x,y) = ((0<x) && (x<(n+1)) && (0<y) && (y<(m+1)) && not(estaB nin (x,y)) && not(estaB rob (x,y)) && not(esta obs (x,y)) && not(esta cor (x,y)))

    validoMovObs:: ([(Int,Int)],Bool) -> Bool
    validoMovObs (a,b) = b