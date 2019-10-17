module SudokusolverVectorUnboxed
    ( sudokuSolver
    ) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

rowAt i = (\x -> [x .. x+8]) (9 * quot i 9)
colAt i = map ((rem i 9+) . (*9)) [0..8]
boxAt i = do
  let boxColumn = 3 * quot (rem i 9) 3
  let boxRow = quot i 27
  map (27*boxRow+boxColumn+) $ concat $ flip map [0,1,2] $ \i -> map (9*i+) [0,1,2]

checkList :: [Int] -> Int -> Bool
checkList list x = (length . filter (==x)) list == 1

sudokuSolver = do
  let
    inputData = [8,0,0,0,0,0,0,0,0,0,0,3,6,0,0,0,0,0,0,7,0,0,9,0,2,0,0,0,5,0,0,0,7,0,0,0,0,0,0,0,4,5,7,0,0,0,0,0,1,0,0,0,3,0,0,0,1,0,0,0,0,6,8,0,0,8,5,0,0,0,1,0,0,9,0,0,0,0,4,0,0]
    initialSudoku = V.fromList inputData :: V.Vector Int
  print $ runST $ do
    mutableSudoku <- V.thaw initialSudoku
    solve mutableSudoku initialSudoku True 0
    V.freeze mutableSudoku

solve :: MV.STVector s Int -> V.Vector Int -> Bool -> Int -> ST s ()
solve sudoku initial direction 81 = return ()
solve sudoku initial direction (-1) = return ()
solve sudoku initial direction i = do
  let initialValue = initial V.! i
  x <- MV.read sudoku i
  if initialValue == 0 then
    if x < 9 then do
      MV.write sudoku i (x+1)
      row <- mapM (MV.read sudoku) (rowAt i)
      col <- mapM (MV.read sudoku) (colAt i)
      box <- mapM (MV.read sudoku) (boxAt i)
      if checkList row (x+1) && checkList col (x+1) && checkList box (x+1) then
        solve sudoku initial True (i+1)
      else
        solve sudoku initial True i
    else do
      MV.write sudoku i 0
      solve sudoku initial False (i-1)
  else
    if direction then
      solve sudoku initial direction (i+1)
    else
      solve sudoku initial direction (i-1)
