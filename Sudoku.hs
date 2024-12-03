module Sudoku where

import Test.QuickCheck
import Data.List
import Data.Char
import Data.Maybe

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
      [ [j 3,j 7,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
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

example2 = Sudoku [[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
        [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
        [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
        [Nothing,Nothing,Just 9,Nothing,Just 3,Nothing,Nothing,Nothing,Just 4],
        [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just 9],
        [Nothing,Nothing,Nothing,Just 7,Nothing,Nothing,Nothing,Nothing,Just 1],
        [Nothing,Just 1,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
        [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
        [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]]

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
  in validNrOfRows r && validNumberInCell r
  

validNrOfRows :: [Row] -> Bool
validNrOfRows rows = length rows == 9 && all (== 9)(map length rows)

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
blocks sudoku = columnMaker sudoku ++ threeTimesThreeBlock sudoku ++ rows sudoku 
  
 
columnMaker :: Sudoku -> [Block]
columnMaker sudoku = 
  let rs = rows sudoku
  in transpose rs
      

threeTimesThreeBlock :: Sudoku -> [Block]
threeTimesThreeBlock sudoku = 
  let rs = rows sudoku
  in helperTwo $ helper rs

helper :: [Row] -> [Block]
helper []   = []
helper rs
  | all null rs = [] 
  | otherwise =
      let block = [take 3 r | r <- rs]
          newRs = [drop 3 r | r <- rs]
      in block ++ helper newRs

helperTwo :: [Block] -> [Block]
helperTwo [] = []
helperTwo list = 
  let block   = [concat (take 3 list)]
      newList  =  drop 3 list
  in  block ++ helperTwo newList



prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sudoku = length (blocks sudoku) == 27 && and [ helperPropLength b | b <- blocks sudoku]

helperPropLength :: Row -> Bool
helperPropLength row = length row == 9

-- * D3

isOkay :: Sudoku -> Bool
isOkay sudoku = and [isOkayBlock block | block <- blocks sudoku]



---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks sud = 
  let rs = rows sud 
      zippedRs = zipSudoku rs
  in concat [checkBlank tuple | tuple <- zippedRs]


zipSudoku :: [Row] -> [([Cell],Int)]
zipSudoku rs = rs `zip` [0..8]

checkBlank :: ([Cell],Int) -> [Pos]
checkBlank (l,row) = [(row,col) | (Nothing,col) <- l `zip` [0..8]]



prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = length (blanks allBlankSudoku) == 9*9


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = loopAndChange i y xs

loopAndChange :: Int -> a -> [a] -> [a]
loopAndChange i y [] = []
loopAndChange i y (x:xs) 
  | i > 0            = x : loopAndChange (i-1) y xs
  | i == 0           = y : xs 



prop_bangBangEquals_correct ::  Eq a => [a] -> (Int,a) -> Bool
prop_bangBangEquals_correct xs (i,y)
  | null xs            = xs !!= (abs i,y) == xs 
  | abs i >= length xs = xs !!= (abs i,y) == xs 
  | otherwise          = (length (xs !!= (abs i,y)) == length xs) && ((xs !!= (abs i,y)) /= xs)


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update sudoku (r,col) cell = 
  let zippedRows = zipSudoku $ rows sudoku
  in Sudoku [changeRow col r (l,i) cell | (l,i) <- zippedRows]


changeRow :: Int -> Int -> ([Cell],Int) -> Maybe Int -> Row
changeRow col r (l,i) cell
  | r == i    = l !!= (col,cell)
  | otherwise = l


prop_update_updated :: Sudoku -> Pos -> Cell -> Bool 
prop_update_updated sudoku (r,col) cell
  | abs r >= 9   = True 
  | abs col >= 9 = True
  |otherwise =
      let result = update sudoku (abs r,abs col) cell
          l = (rows result) !! (abs r) 
          c = l !! (abs col)
      in c == cell 



------------------------------------------------------------------------------

-- * F1
solve :: Sudoku -> Maybe Sudoku
solve sudoku = 
  let listOfAnswers = solve' sudoku (blanks sudoku) 
  in  case listOfAnswers of
    [] -> Nothing
    _ -> Just (listOfAnswers !! 0)
  

solve' :: Sudoku -> [Pos] -> [Sudoku]
solve' sudoku [] = [sudoku]
solve' sudoku listOfBlanks
  | isSudoku sudoku == False = []
  | isOkay sudoku == False   = []
  | otherwise =  
     let [p] = take 1 listOfBlanks
         newListofBlanks = drop 1 listOfBlanks
     in concat [solve' (update sudoku p (Just n)) newListofBlanks | n <- [1..9]]
    

-- * F2
readAndSolve :: FilePath -> IO ()
readAndSolve filepath = do 
  sud <- readSudoku filepath
  let answer = solve sud
  case answer of 
    Nothing -> putStrLn "no answer"
    Just sud -> printSudoku sud



-- * F3
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf solution original 
 | isFilled solution == False = False
 | isOkay solution == False = False
 | otherwise = 
    let bs = blanks original
        result = helperThree solution bs
    in  blocks result == blocks original
    
  
helperThree :: Sudoku -> [Pos] -> Sudoku
helperThree sudoku [] = sudoku
helperThree sudoku (p:ps) = helperThree (update sudoku p Nothing) ps

-- * F4
prop_SolveSound :: Sudoku -> Property
prop_SolveSound original = 
  case (solve original) of 
    Nothing -> property True
    Just solution -> property (isSolutionOf solution original)
