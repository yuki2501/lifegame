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
fieldWidth = 400
fieldHeight = 400
cellSize = 1 
data GameState = GameState{ca :: Dim2CA,seed :: Int,isPosed :: Bool}
main :: IO ()
main =  do
  seed <- randomIO :: IO Int
  let field = initfield seed
  let rule = Rule Moore [3] [2,3]
  let lifegame = Dim2CA field rule True 1
  let state = GameState lifegame seed False
  play (InWindow "The Game of Life" (fieldWidth * cellSize,fieldHeight * cellSize) (120,100)) black 30 state renderField eventHandler tickHandler

renderField :: GameState -> Picture
renderField  = renderArray white (fromIntegral cellSize) . field . ca


eventHandler :: Event ->  GameState -> GameState 
eventHandler (EventKey (MouseButton LeftButton) Down _ (mx, my)) state = state{ca = cellChangedField  (ca state) $ tuple2Index (safeAccess ((toIndex mx (fieldWidth - 1))+(fieldWidth -1)`div`2) fieldWidth,safeAccess ((fieldHeight -1) `div` 2 - (toIndex my (fieldHeight -1))) fieldHeight)}
  where
    toIndex :: Float -> Int ->  Int
    toIndex x i   = if even i then ((roundFloatInt x) `div`  cellSize) else   (roundFloatInt x `div` cellSize)  + 1
    safeAccess :: Int -> Int -> Int
    safeAccess i j= if abs i > j then 0 else i
eventHandler (EventKey (Char c) Down _ _ ) state 
  | c == 'r' = state{ca = (ca state){field = initfield (seed state + 1)}}
  | c == 'p' = state{isPosed = not(isPosed state)}
  | c == 'n' = state{ca = fieldContentUpdate (ca state)}
eventHandler _ ca = ca
    

tickHandler :: Float -> GameState -> GameState
tickHandler _ state = if not(isPosed state) then state{ca = fieldContentUpdate (ca state )} else state

initfield :: Int -> Field
initfield i = runST $ do
  randomFieldContent <- Rp.computeUnboxedP $ Rp.map (\i -> (i == 0)) (randomishIntArray (Rp.Z Rp.:.fieldWidth Rp.:.fieldHeight) 0 1 i)
  pure  randomFieldContent


