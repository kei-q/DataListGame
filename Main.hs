module Main (module Data.List) where

import Control.Concurrent (forkIO)
import Data.List
import Data.Char (digitToInt)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import System.Random (randomRIO)
import Control.Monad (join)

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

import Gen
import Physics

scrW, scrH ::Int
(scrW,scrH) = (640,480) -- px

fontName :: String
fontName = "./VL-Gothic-Regular.ttf"

fontSize :: Int
fontSize = 30 -- px

data GameResource
    = GR { grSpace :: Space
         , grFont :: TTF.Font
         , grScr :: SDL.Surface
         , grGen :: IO Seq
         , grStep:: IO ()
         }

data GameState
    = GS { gsCurrentSeq :: Seq
         , gsTrash :: [Obj]
         }

-- flymakeのためのダミー
main :: IO ()
main = start 1 >> return ()

start :: Int -> IO (Seq -> IO ())
start level = do
  let generaterList = generateOEIS : map generate [1..]
  slot <- newIORef []
  forkIO $ initSDL $ \scr -> do
    font <- TTF.openFont fontName fontSize
    generater <- generaterList !! level
    (space,step) <- initPhysics
    loop slot (GR space font scr generater step) (GS [1..] [])
  let updater = writeIORef slot
  return updater
  
loop :: IORef Seq -> GameResource -> GameState -> IO ()
loop r (GR space font scr gen step) initGS = loop' initGS
  where
    drawFunc = drawText font scr
    loop' gs = do
      let curr = take 16 (gsCurrentSeq gs)
      
      input <- readIORef r
      gs' <- if take 16 input == take 16 curr
        then do
          seq <- gen
          objs' <- sequence $ discardSeq curr space
          return $ GS seq (objs' ++ gsTrash gs)
        else return gs

      -- drawing
      clearScreen scr
      drawFunc (showSeq curr) (20,30) (0x68, 0xbe, 0x8d)
      drawObjs drawFunc $ gsTrash gs
      updateScreen scr

      step
      SDL.delay 16
      keyProc (loop' gs')

keyProc :: IO () -> IO ()
keyProc cont = do
  e <- SDL.pollEvent
  case e of
    SDL.KeyDown (SDL.Keysym SDL.SDLK_q _ _) -> return ()
    SDL.Quit                                -> return ()
    _ -> cont

drawObjs _ [] = return ()
drawObjs f (o:os) = do
  o' <- o
  draw f o'
  drawObjs f os

draw f (Trash c (x,y) _) = do
  f (show c) (floor x, floor y) (0xff,0xff,0xff)
  return ()
draw _ _ = error "error: draw"

------------------------------------------------------------
-- list util
showSeq :: Seq -> [Char]
showSeq seq = intercalate " " $ map show seq

-- TODO 分解
discardSeq seq space
    = let seq' = showSeq seq
          poss = [(fromIntegral x,30) | x <- [20,(20+fontSize`div`2)..]]
      in [do 
           angle <- randomRIO (-100,100)
           buildTrash space (digitToInt c) angle (fromIntegral fontSize / 4) pos
          | (c,pos) <- zip seq' poss, c /= ' ', c /= '-']


------------------------------------------------------------
-- sdlutil
initSDL :: (SDL.Surface -> IO ()) -> IO ()
initSDL act = SDL.withInit [SDL.InitVideo] $ do
  TTF.init
  scr <- SDL.setVideoMode scrW scrH 32 []
  act scr
  TTF.quit

-- no signature
drawText font scr s (x,y) (r,g,b) = do
  fontSur <- TTF.renderTextSolid font s (SDL.Color r g b)
  let fs' = fontSize `div` 2
  SDL.blitSurface fontSur Nothing scr (Just $ SDL.Rect (x-fs') (y-fs') 0 0)

clearScreen :: SDL.Surface -> IO Bool
clearScreen scr = do
  black <- SDL.mapRGB (SDL.surfaceGetPixelFormat scr) 0x00 0x00 0x00
  SDL.fillRect scr Nothing black

updateScreen :: SDL.Surface -> IO ()
updateScreen scr = SDL.updateRect scr $ SDL.Rect 0 0 0 0