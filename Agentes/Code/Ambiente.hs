module Ambiente(
    Ambiente(..),
)
where

    data Ambiente = Ambiente
      {
        corral:: [(Int,Int)],
        ninos :: [(Int,Int,Bool)],
        obstaculo :: [(Int,Int)],
        robot :: [(Int,Int,Bool)],
        suciedad :: [(Int,Int)],
        lrandom :: [Int],
        dimension :: (Int, Int)
      }deriving (Show)