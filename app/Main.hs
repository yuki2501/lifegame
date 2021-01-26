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
data GameState = GameState{ca :: Dim2CA,seed :: Int,isPosed :: Bool,mode :: Mode}
data Mode = Normal | Insert deriving Eq
main :: IO ()
main =  do
  seed <- randomIO :: IO Int
  let field = initfield seed
  let rule = Rule Moore [3] [2,3]
  let lifegame = Dim2CA field rule True 1
  let state = GameState lifegame seed False Normal
  play (InWindow "The Game of Life" (width * cellSize,height * cellSize) (120,100)) white 60 state renderField eventHandler tickHandler

renderField :: GameState -> Picture
renderField state  = (translate (fromIntegral((fieldWidth - width)*cellSize)/2) (fromIntegral((height - fieldHeight)*cellSize)/2) $ renderArray black (fromIntegral cellSize) $ field (ca  state))<> (if (mode state == Insert) then scale 0.5 0.5 $ translate 180 (-400) (text "-- INSERT --") else mempty) <> (translate 180 200  (text (show(generation $ ca state)))) <> (translate 180 330 $ text "Generation") 


eventHandler :: Event ->  GameState -> GameState 
eventHandler (EventKey (MouseButton LeftButton) Down _ (mx, my)) state = if (mode state == Insert) then state{ca = cellChangedField  (ca state) $ tuple2Index (safeAccess ((toIndex ((mx+fromIntegral(((width - fieldWidth)*cellSize))/2)) (fieldWidth- 1))+(fieldWidth -1)`div`2) fieldWidth,safeAccess ((height -1) `div` 2 - (toIndex (my-fromIntegral((height - fieldHeight)*cellSize)) (fieldHeight -1))) fieldHeight)}
                                                                                                   else state
  where
    toIndex :: Float -> Int ->  Int
    toIndex x i   = if even i then ((roundFloatInt x) `div`  cellSize) else   (roundFloatInt x `div` cellSize)  + 1
    safeAccess :: Int -> Int -> Int
    safeAccess i j= if abs i > j then 0 else i
eventHandler (EventKey (Char c) Down _ _ ) state 
  | c == 'r' = state{ca = (ca state){field = initfield (seed state + 1),generation = 1}}
  | c == 'p' = state{isPosed = not(isPosed state)}
  | c == 'n' = state{ca = fieldContentUpdate (ca state)}
  | c == 'i' = if (mode state == Normal)then state{mode = Insert} else state{mode = Normal}
  | c == 'd' = state{ca = ((ca state){generation = 1,field = runST $ Rp.computeUnboxedP $ Rp.fromFunction (Rp.Z Rp.:.fieldHeight Rp.:.fieldWidth)  (\_ -> False)})}
eventHandler _ ca = ca

tickHandler :: Float -> GameState -> GameState
tickHandler _ state = if not(isPosed state) then state{ca = fieldContentUpdate (ca state )} else state

initfield :: Int -> Field
initfield i = runST $ do
  randomFieldContent <- Rp.computeUnboxedP $ Rp.map (\i -> (i == 0)) (randomishIntArray (Rp.Z Rp.:.fieldHeight Rp.:.fieldWidth) 0 1 i)
  pure  randomFieldContent


