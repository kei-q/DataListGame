{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Gen(Seq, generateOEIS, generate) where

import Data.List
import System.Random
import Control.Monad
import Math.OEIS

type Seq = [Integer]

------------------------------------------------------------
-- genOEIS
generateOEIS :: IO (IO Seq)
generateOEIS = do
  seqs <- readFile "oeis.data"
  let seqs' = (read seqs) :: [Seq]
  let choice = do
        n <- randomRIO (0,99)
        return $ seqs' !! n
  return choice

{- 手抜き実装
generateOEIS :: Rnd Seq
generateOEIS = do
  n <- rInt (0,9)
  let lst = case getSequenceByID ("A00000" ++ show n) of
              Just lst -> lst
              Nothing  -> [1..]
  return lst
-}

{-
  Random
-}
newtype Rnd a = Rnd { runRnd :: IO a } deriving (Monad, Functor)

rInt :: (Int,Int) -> Rnd Int
rInt range = Rnd $ do
  r <- newStdGen
  setStdGen r
  randomRIO range

rInteger :: (Integer,Integer) -> Rnd Integer
rInteger range = Rnd $ do
  r <- newStdGen
  setStdGen r
  randomRIO range

rElem :: [a] -> Rnd a
rElem lst = do
  n <- rInt (0, length lst - 1)
  return $ lst !! n

{-
  generate
-}
type Gen' = Rnd (Seq -> Seq)

genList :: [Gen']
genList = [
 gTake ,gDrop
 ,gIntersperse
 ,gScanl1
 ,gCycle
 ,gZipWith
 ,gMap]

generate :: Int -> IO (IO Seq)
generate n = do
  let choice = do
        initList <- runRnd gList
        runRnd $ foldM (\lst _ -> ap (join $ rElem genList) (return lst)) initList [1..n]
  return choice
  
gMap :: Gen'
gMap = fmap map gUnary

gTake :: Gen'
gTake = fmap take gInt

gDrop :: Gen'
gDrop = fmap drop gInt

gIntersperse :: Gen'
gIntersperse = fmap intersperse gInteger

gScanl1 :: Gen'
gScanl1 = fmap scanl1 gBinary

gCycle :: Gen'
gCycle = return cycle

gZipWith :: Gen'
gZipWith = (fmap $ uncurry zipWith) aux
  where
    aux = do
      f <- gBinary
      l <- gList
      return (f,l)

{-----------------------------------------------------------
  primitive
-}

gInt :: Rnd Int
gInt = rInt (1,9)

gInteger :: Rnd Integer
gInteger = rInteger (1,9)

gList :: Rnd Seq
gList = do
  n <- rInt (0, length gListFunc - 1)
  return $ (gListFunc !! n) 1
    where
      gListFunc = [enumFrom , enumFromThen (-1), enumFromThen 1]

gUnary :: Rnd (Integer -> Integer)
gUnary = do
  f <- gBinary
  return $ f 1

gBinary :: Rnd (Integer -> Integer -> Integer)
gBinary = do
  let fs = [(+), (-), (*)] -- ,div]
  c <- rInt (0, length fs - 1)
  return $ fs !! c

gPred :: Seq -> Rnd (Integer -> Bool)
gPred lst = do
  let fs = [(<), (>), (<=), (>=), (==), (/=)]
  c <- rInt (0, length fs - 1)
  select <- rInt (0, min (length lst -1) 20)
  return $ (fs !! c) (lst !! select)

