{-# LANGUAGE OverloadedStrings #-}

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
import Parser
import Debug.Trace
import qualified Data.Text as T
width = 400
height = 200
fieldWidth = 200
fieldHeight = 200
cellSize = 5 
data GameState = GameState{ca :: Dim2CA,seed :: Int,isPosed :: Bool,mode :: Mode,clipBoard :: Field,command :: String,fps :: Int} 
data Mode = Normal | Insert deriving Eq
main :: IO ()
main =  do
  seed <- randomIO :: IO Int
  let field = initfield seed
  let rule = Rule Moore [3] [2,3]
  let lifegame = Dim2CA field rule True 1  
  let state = GameState lifegame seed False Normal (Rp.computeUnboxedS $ Rp.fromFunction (Rp.Z Rp.:.1 Rp.:.1) (const 1)) [] 60
  play (InWindow "The Game of Life" (width * cellSize,height * cellSize) (120,100)) white (fps state) state renderField eventHandler tickHandler

renderField :: GameState -> Picture
renderField state  = (translate (fromIntegral((fieldWidth - width)*cellSize)/2) (fromIntegral((height - fieldHeight)*cellSize)/2) $ (renderArray black (fromIntegral cellSize) $ field (ca  state))) <> (if (mode state == Insert) then scale 0.5 0.5 $ translate 300 (-400) (text "-- INSERT --") else mempty) <> (translate 300 200  (text (show(generation $ ca state)))) <> (translate 300 330 $ text "Generation")<> (scale 0.5 0.5 $ translate 180 (-410) (text $ reverse (command state))) <> (translate 300 90 $ text ("Rule:" ++ ruleInt2Strng (activeCellNeighbor $ rule $ca state) ++ "/" ++ruleInt2Strng (birthCellNeighbor $ rule $ ca state) ))
                                                                                                                                                                                                                                   where
                                                                                                                                                                                                                                     ruleInt2Strng :: [Int] -> String
                                                                                                                                                                                                                                     ruleInt2Strng [] = []
                                                                                                                                                                                                                                     ruleInt2Strng (x:xs) = show x ++ ruleInt2Strng xs 

eventHandler :: Event ->  GameState -> GameState 
eventHandler (EventKey (MouseButton LeftButton) Down _ (mx, my)) state = if mode state == Insert then state{ca = cellChangedField  (ca state) $ clickedIndex2ArrayIndex mx my}
                                                                                                   else state{ca = fieldContentUpdate (ca state)}
eventHandler (EventKey (MouseButton RightButton) Down _ (mx,my)) state = if mode state == Insert then pasteClipBoard state (clickedIndex2ArrayIndex mx my) else state{ca = fieldContentUpdate (ca state)}
eventHandler (EventKey (Char c) Down _ _ ) state 
  | c == 'r' = if null $ command state then state{ca = (ca state){field = initfield (seed state + 100),generation = 1}} else  state{command = 'r':(command state)}
  | c == 'p' = if null $ command state then state{isPosed = not(isPosed state)} else state{command = 'p':(command state)}
  | c == 'n' = if null $ command state then state{ca = fieldContentUpdate (ca state)} else state {command = 'n':(command state)}
  | c == 'i' = if mode state == Normal && (null $  command state) then state{mode = Insert} else if not (null $  command state) then state{command = 'i':(command state) }else state{mode = Normal}
  | c == 'd' = if mode state == Insert && (null $  command state) then state{ca = ((ca state){generation = 1,field = runST $ Rp.computeUnboxedP $ Rp.fromFunction (Rp.Z Rp.:.fieldHeight Rp.:.fieldWidth)  (const 0)})} else if not (null $  command state) then state{command = 'd':command state } else state
  | c == ':' = if command state == reverse "error" then state{command = ':':[]} else state{command = ':':(command state)}
  | otherwise = if not (null $ command state) then (state{command = c:(command state)}) else state
eventHandler (EventKey (SpecialKey s) Down _ _) state
  | s == KeyBackspace = if not (null $ command state) then (state{command = tail (command state)}) else state
  | s == KeyDelete = if not (null $ command state) then (state{command = tail (command state)}) else state
  | s == KeySpace = if not (null $ command state) then (state {command = ' ':command state}) else state
  | s == KeyEnter = if command state == reverse "error" then state{command = []} else if not (null $ command state) then inputedcommandsInterpret state ( (result(T.pack $ reverse $ command state))) else  state
eventHandler _ ca = ca

tickHandler :: Float -> GameState -> GameState
tickHandler _ state = if not(isPosed state) then state{ca = fieldContentUpdate (ca state )} else state

initfield :: Int -> Field
initfield i =  (randomishIntArray (Rp.Z Rp.:.fieldHeight Rp.:.fieldWidth) 0 1 i)

char2Int :: Char -> Int
char2Int c = if c == '#' then 1 else 0

clickedIndex2ArrayIndex :: Float -> Float -> Rp.DIM2
clickedIndex2ArrayIndex mx my = tuple2Index (safeAccess  (((abs(mx + int2Float((width*cellSize)`div`2))/(int2Float cellSize)))) fieldWidth,safeAccess (((abs(my - int2Float((height*cellSize)`div`2))/(int2Float cellSize)))) fieldHeight)
  where
    safeAccess :: Float -> Int -> Int
    safeAccess i j= if abs (roundFloatInt i) > j then 0 else (roundFloatInt i)
changerule :: GameState -> Int -> Int -> GameState
changerule state i i' = let activeCellNeighbor' = int2list i
                            birthCellNeighbor' = int2list i'
                         in state{ca = (ca state){rule = (rule$ca state){activeCellNeighbor = activeCellNeighbor',birthCellNeighbor = birthCellNeighbor'}},command = ""}
                         where
                           int2list 0 = []
                           int2list i = let (d,m) = divMod i 10
                                       in reverse(m:int2list d)

block = Rp.fromListUnboxed (Rp.Z Rp.:.(2::Int) Rp.:.(2::Int)) (map char2Int "####")
beehive = Rp.fromListUnboxed (Rp.Z Rp.:.(3::Int) Rp.:.(4::Int)) (map char2Int " ## #  # ## ")
loaf = Rp.fromListUnboxed (Rp.Z Rp.:.(4::Int) Rp.:.(4::Int)) (map char2Int " ## #  # # #  # ")
boat = Rp.fromListUnboxed (Rp.Z Rp.:.(3::Int) Rp.:.(3::Int)) (map char2Int "## # # # ")
blinker =Rp.fromListUnboxed (Rp.Z Rp.:.(1::Int) Rp.:.(3::Int)) (map char2Int "###")
toad = Rp.fromListUnboxed (Rp.Z Rp.:.(2::Int) Rp.:.(4::Int))(map char2Int " ###### ")
beacon = Rp.fromListUnboxed (Rp.Z Rp.:.(4::Int) Rp.:.(4::Int))( map char2Int "##  ##    ##  ##")
templeteList = ["block","beehive","loaf","boat","blinker","toad","beacon"]
interpretCommand ::    Command  ->  InputedCommands 
interpretCommand  f = case f of
                       (GameCommand command r) -> interpret command  (interpretCommand r )
                       (CommandParameter int r) ->Parameter int (interpretCommand  r)
                       (CommandNil) -> Nil 
                       
interpret :: T.Text -> InputedCommands -> InputedCommands
interpret t inputedcommands
  | t == "changefps" = Changefps inputedcommands
  | t == "changerule" = ChangeRule inputedcommands
  | t `elem` templeteList = Set t inputedcommands
  | otherwise = inputedcommands
                                                        
data InputedCommands  = Changefps   (InputedCommands )|ChangeRule  (InputedCommands ) |Set T.Text (InputedCommands )|Parameter Int (InputedCommands ) |Nil deriving (Show,Eq,Read)

result ::   T.Text -> InputedCommands 
result  text = interpretCommand (  (parseCommand text))

inputedcommandsInterpret ::  GameState -> InputedCommands  -> GameState
inputedcommandsInterpret state f = case f of
                                     (Changefps (Parameter i (Nil))) -> state{fps = i,command = ""}
                                     (ChangeRule (Parameter i (Parameter i' (Nil)))) -> changerule state i i'
                                     (Set "block" (Nil)) -> state{clipBoard = block,command = ""}
                                     (Set "beehive" (Nil)) -> state{clipBoard = beehive,command = ""}
                                     (Set "loaf" (Nil)) -> state{clipBoard = loaf,command = ""}
                                     (Set "boat" (Nil)) -> state{clipBoard = boat,command = ""}
                                     (Set "blinker" (Nil)) -> state{clipBoard = blinker, command = ""}
                                     (Set "toad" (Nil)) -> state{clipBoard = toad, command = ""}
                                     (Set "beacon" (Nil)) -> state{clipBoard = beacon,command = ""}
                                     _ -> state{command = reverse "error"}


pasteClipBoard :: GameState -> Rp.DIM2 -> GameState
pasteClipBoard state (Rp.Z Rp.:.x Rp.:.y) = let newField = runST $ Rp.computeUnboxedP $ Rp.traverse (field $ ca state) id (cellupdate (ca state))
                                                  where
                                                    cellupdate ::Dim2CA -> (Rp.DIM2 -> Int) -> Rp.DIM2 -> Int
                                                    cellupdate ca _ (Rp.Z Rp.:.i Rp.:.j) = if (x - 1 <= i && i <= x + ((\(Rp.Z Rp.:.i Rp.:._) -> i) $ Rp.extent (clipBoard state)) -2 && y - 1 <= j && j <=  ((\(Rp.Z Rp.:._ Rp.:.j) -> j) $ Rp.extent (clipBoard state)) + y - 2) then trace((show (i - (y - 1)))++" "++(show (j - (x - 1)))) $(clipBoard state) Rp.! (Rp.Z Rp.:.(i - (x - 1)) Rp.:.(j - (y - 1))) else ((field (ca)) Rp.! (Rp.Z Rp.:.i Rp.:.j))
                                             in state{ ca = (ca state){field = newField} }
