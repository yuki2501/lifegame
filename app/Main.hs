module Main where
import Gloss.Render
import Data.DIM2CA
import qualified Data.Array.Repa as Rp
import Data.Array.Repa.Algorithms.Randomish (randomishIntArray)
import System.Random
import Control.Monad.ST
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GHC.Float.RealFracMethods
import Debug.Trace
width = 400
height = 200
fieldWidth = 200
fieldHeight = 200
cellSize = 5 
data GameState = GameState{ca :: Dim2CA,seed :: Int,isPosed :: Bool,mode :: Mode,clipBoard :: Board,command :: String,fps :: Int}
data Mode = Normal | Insert deriving Eq
data Board = Empty | Field [Bool]
main :: IO ()
main =  do
  seed <- randomIO :: IO Int
  let field = initfield seed
  let rule = Rule Moore [3] [2,3]
  let lifegame = Dim2CA field rule True 1 
  let state = GameState lifegame seed False Normal Empty [] 60
  play (InWindow "The Game of Life" (width * cellSize,height * cellSize) (120,100)) white (fps state) state renderField eventHandler tickHandler

renderField :: GameState -> Picture
renderField state  = (translate (fromIntegral((fieldWidth - width)*cellSize)/2) (fromIntegral((height - fieldHeight)*cellSize)/2) $ (renderArray black (fromIntegral cellSize) $ field (ca  state))) <> (if (mode state == Insert) then scale 0.5 0.5 $ translate 300 (-400) (text "-- INSERT --") else mempty) <> (translate 300 200  (text (show(generation $ ca state)))) <> (translate 300 330 $ text "Generation")<> (scale 0.5 0.5 $ translate 180 (-410) (text $ reverse (command state)))


eventHandler :: Event ->  GameState -> GameState 
eventHandler (EventKey (MouseButton LeftButton) Down _ (mx, my)) state = if mode state == Insert then state{ca = cellChangedField  (ca state) $ clickedIndex2ArrayIndex mx my}
                                                                                                   else state
eventHandler (EventKey (Char c) Down _ _ ) state 
  | c == 'r' = if null $ command state then state{ca = (ca state){field = initfield (seed state + 1),generation = 1}} else  state{command = 'r':(command state)}
  | c == 'p' = if null $ command state then state{isPosed = not(isPosed state)} else state{command = 'p':(command state)}
  | c == 'n' = if null $ command state then state{ca = fieldContentUpdate (ca state)} else state {command = 'n':(command state)}
  | c == 'i' = if mode state == Normal && (null $  command state) then state{mode = Insert} else if not (null $  command state) then state{command = 'i':(command state) }else state{mode = Normal}
  | c == 'd' = if mode state == Insert && (null $  command state) then state{ca = ((ca state){generation = 1,field = runST $ Rp.computeUnboxedP $ Rp.fromFunction (Rp.Z Rp.:.fieldHeight Rp.:.fieldWidth)  (const False)})} else if not (null $  command state) then state{command = 'd':(command state) } else state
  | c == ':' = state{command = ':':(command state)}
  | otherwise = if not (null $ command state) then (state{command = c:(command state)}) else state
eventHandler (EventKey (SpecialKey s) Down _ _) state
  | s == KeyBackspace = if not (null $ command state) then (state{command = tail (command state)}) else state
  | s == KeyDelete = if not (null $ command state) then (state{command = tail (command state)}) else state
eventHandler _ ca = ca

tickHandler :: Float -> GameState -> GameState
tickHandler _ state = if not(isPosed state) then state{ca = fieldContentUpdate (ca state )} else state

initfield :: Int -> Field
initfield i = runST $ Rp.computeUnboxedP $ Rp.map ( == 0) (randomishIntArray (Rp.Z Rp.:.fieldHeight Rp.:.fieldWidth) 0 1 i)

char2Bool :: Char -> Bool
char2Bool c =  c == '#'

clickedIndex2ArrayIndex :: Float -> Float -> Rp.DIM2
clickedIndex2ArrayIndex mx my = tuple2Index (safeAccess  (((abs(mx + int2Float((width*cellSize)`div`2))/(int2Float cellSize)))) fieldWidth,safeAccess (((abs(my - int2Float((height*cellSize)`div`2))/(int2Float cellSize)))) fieldHeight)
  where
    safeAccess :: Float -> Int -> Int
    safeAccess i j= if abs (roundFloatInt i) > j then 0 else (roundFloatInt i)


block = Rp.fromListUnboxed (Rp.Z Rp.:.(2::Int) Rp.:.(2::Int)) (map char2Bool "####")
beehive = Rp.fromListUnboxed (Rp.Z Rp.:.(3::Int) Rp.:.(4::Int)) (map char2Bool " ## #  # ## ")
loaf = Rp.fromListUnboxed (Rp.Z Rp.:.(4::Int) Rp.:.(4::Int)) (map char2Bool " ## #  # # #  # ")
boat = Rp.fromListUnboxed (Rp.Z Rp.:.(3::Int) Rp.:.(3::Int)) (map char2Bool "## # # # ")
blinker =Rp.fromListUnboxed (Rp.Z Rp.:.(1::Int) Rp.:.(3::Int)) (map char2Bool "###")
toad = Rp.fromListUnboxed (Rp.Z Rp.:.(2::Int) Rp.:.(4::Int))" ###### "
beacon = Rp.fromListUnboxed (Rp.Z Rp.:.(4::Int) Rp.:.(4::Int))( map char2Bool "##  ##    ##  ##")

