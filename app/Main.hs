{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
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
import Control.Monad.Skeleton
import qualified Data.Text as T
width = fieldWidth * 2
height = fieldHeight
fieldWidth = 200
fieldHeight = 200
cellSize = 5 
data GameState = GameState{ca :: Dim2CA,seed :: Int,isPosed :: Bool,mode :: Mode,clipBoard :: Field,command :: String,fps :: Int,clickedIndex :: (Int,Int)} 
data Mode = Normal | Insert deriving Eq
main :: IO ()
main =  do
  seed <- randomIO :: IO Int
  let field = initfield seed
  let rule = Rule Moore [3] [2,3]
  let lifegame = Dim2CA field rule True 1  
  clipboard <- Rp.computeUnboxedP $ Rp.fromFunction (Rp.Z Rp.:.(1::Int) Rp.:.(1::Int)) (\ _ -> (1::Int))
  let state = GameState lifegame seed False Normal (clipboard) [] 60 (0,0)
  play (InWindow "The Game of Life" (width * cellSize ,height * cellSize) (120,100)) white (fps state) state renderField eventHandler tickHandler

renderField :: GameState -> Picture
renderField state  = (translate (fromIntegral((fieldWidth - width)*cellSize)/2) (fromIntegral((height - fieldHeight)*cellSize)/2) $ renderArray black (fromIntegral cellSize) $ field (ca  state)) 
  <> (if (mode state == Insert) then translate 300 (-500) $ scale 0.5 0.5   (text "-- INSERT --") else mempty) 
  <> (translate 300 200  (text (show(generation $ ca state)))) 
  <> (translate 300 330 $ text "Generation")
  <> (scale 0.5 0.5 $ translate 180 (-410) (text $ reverse (command state))) <> (translate 300 90 $ text ("Rule:" ++ ruleInt2Strng (activeCellNeighbor $ rule $ca state) ++ "/" ++ruleInt2Strng (birthCellNeighbor $ rule $ ca state) ))
  <> (translate 300  (-100) $scale 0.5 0.5 $ text ("Active Cell:" ++ show(runST $ Rp.sumAllP (field $ ca state))))
  <> (translate 300 (-150) $ scale 0.5 0.5 $ text ("Clicked Cell" ++ show(clickedIndex state)))
                                                                                                                                                                                                                                   where
                                                                                                                                                                                                                                     ruleInt2Strng :: [Int] -> String
                                                                                                                                                                                                                                     ruleInt2Strng [] = []
                                                                                                                                                                                                                                     ruleInt2Strng (x:xs) = show x ++ ruleInt2Strng xs 

eventHandler :: Event ->  GameState -> GameState 
eventHandler (EventKey (MouseButton LeftButton) Down _ (mx, my)) state = if mode state == Insert then state{ca = cellChangedField  (ca state) $ clickedIndex2ArrayIndex mx my}
                                                                                                   else state{ca = fieldContentUpdate (ca state)}
eventHandler (EventKey (MouseButton RightButton) Down _ (mx,my)) state = if mode state == Insert then pasteClipBoard state (clickedIndex2ArrayIndex mx my)  else state{clickedIndex = ((\(Rp.Z Rp.:.i Rp.:.j) -> (i,j)) $ clickedIndex2ArrayIndex  mx my)}
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
  | s == KeyEnter =  if not (null $ command state) then runM state ( bone (result(T.pack $ reverse $ command state))) else  state{command = "error"}
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
pulsar = Rp.fromListUnboxed (Rp.Z Rp.:.(13::Int) Rp.:.(13::Int)) (map char2Int "  ###   ###               #    # #    ##    # #    ##    # #    #  ###   ###                 ###   ###  #    # #    ##    # #    ##    # #    #               ###   ###  ")
penta = Rp.fromListUnboxed (Rp.Z Rp.:.(8::Int) Rp.:.(3::Int)) (map char2Int "#### ############## ####")
glider = Rp.fromListUnboxed (Rp.Z Rp.:.(3::Int) Rp.:.(3::Int)) (map char2Int " #   ####")
lightstarship = Rp.fromListUnboxed (Rp.Z Rp.:.(4::Int)Rp.:.(5::Int)) (map char2Int " #  ##    #   ##### ")
middlestarship = Rp.fromListUnboxed (Rp.Z Rp.:.(5::Int) Rp.:.(6::Int)) (map char2Int "   #   #   ##     #    ###### ")
heavystarship = Rp.fromListUnboxed (Rp.Z Rp.:.(5::Int) Rp.:.(7::Int)) (map char2Int "   ##   #    ##      #     ####### ")
glidergun = Rp.fromListUnboxed (Rp.Z Rp.:.(9::Int) Rp.:.(36::Int)) (map char2Int "                        #                                 # #                       ##      ##            ##           #   #    ##            ####        #     #   ##              ##        #   # ##    # #                     #     #       #                      #   #                                ##                      ")
diehard = Rp.fromListUnboxed (Rp.Z Rp.:.(3::Int) Rp.:.(8::Int)) (map char2Int "      # ##       #   ###")
acom = Rp.fromListUnboxed (Rp.Z Rp.:.(3::Int) Rp.:.(7::Int)) (map char2Int " #        #   ##  ###")
templeteList = ["block","beehive","loaf","boat","blinker","toad","beacon","pulsar","penta","glider","lightstarship","middlestarship","heavystarship","glidergun","diehard","acom"]
interpretCommand  f = case f of
                       (GameCommand command r) -> interpret command  r
                       (CommandNil) -> Nil'
                       _ -> Error'                      
interpret t inputedcommands
  | t == "changerule"  =  case inputedcommands of
                            (CommandParameter i (CommandParameter i' (CommandNil))) -> (ChangeRule' i i')
                            (CommandParameter i (CommandParameter i' (GameCommand t r))) -> ((ChangeRule' i i'))   
                            _ ->(Error')
  | t == "cut" = case inputedcommands of
                   (CommandParameter i (CommandParameter i' (CommandParameter i'' (CommandParameter i''' (CommandNil))))) -> if (i <= i'') && (i' <= i''') then (Cut' i i' i'' i''') else Error'
                   _ -> Error'
  | t `elem` templeteList = case inputedcommands of
                              CommandNil -> (Set' t)
                              _ -> Error'
  | otherwise = Error'
data InputedCommands' r  where
  ChangeRule'::   Int -> Int -> InputedCommands' GameState 
  Cut'       ::  Int -> Int -> Int -> Int -> InputedCommands' GameState  
  Set'       ::  T.Text -> InputedCommands' GameState 
  Nil'       :: InputedCommands' GameState 
  Error' :: InputedCommands' GameState 

type M = Skeleton InputedCommands' 


runM :: GameState ->  M a -> a
runM state m = case debone m of
                 ChangeRule' i i' :>>= k -> runM ( changerule state  i  i')$ k $ (changerule state  i i')
                 Cut' i i' i'' i''' :>>= k -> runM (cut state i i' i'' i''') $ k $ (cut state i i' i'' i''')
                 Set' t :>>= k -> runM (interpretSetCommand state t) $ k $ (interpretSetCommand state t)
                 Nil' :>>= k -> runM state $ k (state{command = ""})
                 Error' :>>= k -> runM (state{command = reverse "error"}) $ k $ (state{command = reverse "error"})
                 Return a -> a
                 

result ::   T.Text -> InputedCommands' GameState 
result  text = interpretCommand (  (parseCommand text))


interpretSetCommand :: GameState -> T.Text -> GameState
interpretSetCommand state t
  | t == "block" = state{clipBoard = block, command = ""}
  | t == "beehive" = state{clipBoard = beehive,command = ""}
  | t == "loaf" = state{clipBoard = loaf,command = ""}
  | t == "boat" = state{clipBoard = boat, command = ""}
  | t == "blinker" = state{clipBoard = blinker,command = ""}
  | t == "toad" = state{clipBoard = toad, command = ""}
  | t == "beacon"  = state{clipBoard = beacon,command = ""}
  | t == "pulsar" = state{clipBoard = pulsar,command = ""}
  | t == "penta" = state{clipBoard = penta,command = ""}
  | t == "glider" = state{clipBoard = glider , command = ""}
  | t == "lightstarship" = state{clipBoard = lightstarship,command = ""}
  | t == "middlestarship" = state{clipBoard = middlestarship,command = ""}
  | t == "heavystarship" = state{clipBoard = heavystarship,command = ""}
  | t == "glidergun" = state{clipBoard = glidergun,command = ""}
  | t == "diehard" = state{clipBoard = diehard,command = ""}
  | t == "acom" = state{clipBoard = acom, command = ""}
  | otherwise     = state{command = reverse "error"}

pasteClipBoard :: GameState -> Rp.DIM2 -> GameState
pasteClipBoard state (Rp.Z Rp.:.x Rp.:.y) = let newField = runST $ Rp.computeUnboxedP $! Rp.traverse (field $ ca state) id (cellupdate (ca state))
                                                  where
                                                    cellupdate ::Dim2CA -> (Rp.DIM2 -> Int) -> Rp.DIM2 -> Int
                                                    cellupdate ca _ (Rp.Z Rp.:.i Rp.:.j) = if (x - 1 <= i && i <= x + ((\(Rp.Z Rp.:.i Rp.:._) -> i) $ Rp.extent (clipBoard state)) -2 && y - 1 <= j && j <=  ((\(Rp.Z Rp.:._ Rp.:.j) -> j) $ Rp.extent (clipBoard state)) + y - 2) then (clipBoard state) Rp.! (Rp.Z Rp.:.(i - (x - 1)) Rp.:.(j - (y - 1))) else ((field (ca)) Rp.! (Rp.Z Rp.:.i Rp.:.j))
                                             in  state{ ca = (ca state){field = newField} }

cut :: GameState -> Int -> Int -> Int -> Int -> GameState
cut state x y x' y' = let mkCell :: Rp.DIM2 -> Int
                          mkCell (Rp.Z Rp.:.i Rp.:.j)= (field $ ca state) Rp.! (Rp.Z Rp.:.(i+y-1) Rp.:.(j + x -1 ))
                          clipBoard' = runST $ Rp.computeUnboxedP $! Rp.fromFunction (Rp.Z Rp.:.(y' - y + 1) Rp.:.(x' - x + 1)) (mkCell)
                      in state{clipBoard = clipBoard',command = ""} 
