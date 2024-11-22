module Sudoku where

import Test.QuickCheck
import Data.List
import Data.Char

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku 
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sudoku = 
  let 
    r = rows sudoku
  in validNrOfRows r && validNrOfRows r
  

validNrOfRows :: [Row] -> Bool
validNrOfRows rows = length rows == 9

validNumberInCell ::[Row] -> Bool
validNumberInCell rows = and [and $ map validNumber row | row <- rows]  
  where
    validNumber :: Cell -> Bool
    validNumber cell = 
      case cell of 
        Just n -> n > 0 && n < 10
        Nothing -> True
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               


   

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled sudoku = 
  let 
    r = rows sudoku
  in and [ and $ map isNumber row | row <- r] 
    where
      isNumber :: Cell -> Bool 
      isNumber cell = 
        case cell of 
          Just n -> True
          Nothing -> False




------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku []) = return () 
printSudoku sudoku = do
  let (row:rest) = rows sudoku
      newSudoku = Sudoku rest
  printOut row
  printSudoku newSudoku


printOut :: Row -> IO ()
printOut [] = putStr "\n"
printOut (cell:cells) = do
  case cell of 
    Just n -> putStr (show n)
    Nothing -> putStr (".")
  printOut cells
-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do 
  texts <- readFile file
  let fixedTexts = lines texts
      rows = [rowMaker text | text <- fixedTexts]
  if isSudoku (Sudoku rows) 
    then return (Sudoku rows)  
    else error "Not a Sudoku!"


rowMaker :: [Char] -> Row
rowMaker list = foldr (\char acc -> case char of 
    '.'       -> Nothing : acc
    otherwise -> Just (digitToInt char) : acc) [] list
  
------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency[
  (1,do n <- rMaybeInt
        return n),
  (9,return Nothing)]
  where
    rMaybeInt :: Gen (Cell)
    rMaybeInt = do
      n <- choose(1,9)
      return $ Just n

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do 
    rows <- vectorOf 9 (vectorOf 9 cell)
    return (Sudoku rows)
 

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sud = isSudoku sud
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock block = 
  let onlyNumbers = onlyNumbersInBlock block
      dupDelete = nub onlyNumbers
  in onlyNumbers == dupDelete
    where
        onlyNumbersInBlock :: Block -> Block 
        onlyNumbersInBlock  = foldr (\cell acc -> case cell of
                        Just n   -> Just n : acc
                        otherwise -> acc) []


-- * D2

blocks :: Sudoku -> [Block]
blocks = undefined

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths = undefined

-- * D3

isOkay :: Sudoku -> Bool
isOkay = undefined


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
