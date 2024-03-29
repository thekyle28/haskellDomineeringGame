{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
module GameExercise where

{-

Before running your solution in ghci or compiling it by ghc on lab machines
make sure you run

    module load ghc/7.6.3

(This is to make sure your version of GHC supports Safe language extension.)

-}

import Control.Monad (liftM, ap)
import Control.Applicative (Applicative(..))
import System.Random

-- We have two players PV (the vertical player) and PH (the horizontal player).
-- More information is in the pdf handout on Canvas.

data Player = PV | PH deriving (Read, Show, Eq, Ord)
type Coordinates = (Int,Int)
type BoardSize = (Int,Int) -- Represents the size of the board, where the first number is the number of rows and the second number is the number of columns
type Outcome = Int 

-- You need to define a type of boards:

data Board = Board {
                 boardSize      :: BoardSize
               , occupied       :: [Coordinates] 
               , nextPlayer     :: Player
               , outcome        :: Outcome
              }

-- You also need to provide a type of moves:

data Move = Move { 
                   coordinate  :: (Int,Int)
                 , player      :: Player   
                 } --we store only one point assuming the player can only play a right horizontal move or a downward vertical move.

-- You will work with the definition of Tree as in our tic-tac-toe
-- lectures and handouts:

data Tree = Fork {root :: Board, children :: [(Move,Tree)]}

-- In order to test your program, we will need to create boards,
-- without knowing how you choose to represent boards. You have to
-- implement the following function toBoard for that purpose.
--
-- Input: a tuple (xBound, yBound, coordinates, player) where
--
--     (1) xBound and yBound are non-negative Int's
--     (2) coordinates is a list of pairs (x,y) with 0 <= x < xBound
--                                               and 0 <= y < yBound
--         which are to be occupied in the board.
--     (3) player is the Player that plays next in that board.
--
-- Output: a Board according to your choice of representation.

toBoard :: (Int, Int, [(Int,Int)], Player) -> Board
toBoard (x, y, occupied, player) = ( Board (x,y) occupied player 0 )

{-allPossibleHMoves :: BoardSize -> [Move] -- return all of the possible horizonal moves available on an empty board with size BoardSize. 
allPossibleHMoves (xs,ys) = [Move (x,y) (x',y') | x <-[0..(xs-1)], x' <-[0..(xs-1)], y <-[0..(ys-1)], y'<-[0..(ys-1)], --defines the range of values x, x', y and y' can have.
                            (x'-x) == 1, not (x == x'), y==y']

allPossibleVMoves :: BoardSize -> [Move] -- return all of the possible vertical moves available on an empty board with size BoardSize. 
allPossibleVMoves (xs,ys) = [Move (x,y) (x',y') | x <-[0..(xs-1)], x' <-[0..(xs-1)], y <-[0..(ys-1)], y'<-[0..(ys-1)],
                            (y'-y) == 1, not (y == y'), x==x'] 

allCoords :: BoardSize -> [Coordinates] -- returns all the coordinates on an empty board.
allCoords (xs,ys) = [ (x,y) | x <-[0..(xs-1)], y <-[0..(ys-1)] ]
-}

possibleHMoves :: BoardSize -> [Coordinates] -> [Move] -- return the possible horizonal moves on a board, given the boardSize and the list of occupied spaces.
possibleHMoves (xs,ys) occupied = if ((xs>1) && (ys>1)) then [Move (x,y) PH |  x <-[0..(xs-2)], y <-[0..(ys-1)], --ensure all of the coordinates are within the range of the board
                                                    not ((x,y) `elem` occupied), not ((x+1,y) `elem` occupied)] -- neither of the coordinates can be in the occupied moves list.
                                                   else [] 

possibleVMoves :: BoardSize -> [Coordinates] -> [Move] -- return the possible vertical moves on a board
possibleVMoves (xs,ys) occupied = if ((xs>1) && (ys>1)) then [Move (x,y) PV |  x <-[0..(xs-1)], y <-[0..(ys-2)], --ensure all of the coordinates are within the range of the board
                                                    not ((x,y) `elem` occupied), not ((x,y+1) `elem` occupied)] -- neither of the coordinates can be in the occupied moves list. --[Move (x,y) (x',y') | x <-points, column <-points , (column-row) == columns, not (row == column)]
                                                   else []                                                   

available :: BoardSize -> [Coordinates] -> [Coordinates] -- returns all the coordinates available on a board with occupied spaces, given the boardSize and the occupied spaces.
available (xs,ys) occupied = [ (x,y) | x <-[0..(xs-1)], y <-[0..(ys-1)], not ((x,y) `elem` occupied) ]


exampleBoard = (3,2, [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1)],PH)
-- We also need to perform the opposite conversion, from your
-- representation of boards, to our naive description of boards, so
-- that we can "read" your boards whenever we need to, for testing
-- purposes:

fromBoard :: Board -> (Int, Int, [(Int, Int)], Player)
fromBoard ( Board (x,y) occupied player _ ) = (x, y, occupied, player)

-- Similarly, given a Board, we want to create a Move given
-- (x,y,player) where (x,y) is a position in the Board:

toMove :: Board -> (Int, Int, Player) -> Move
toMove _ (x,y,player) = Move (x,y) player

-- And we want to do the opposite too:

fromMove :: Move -> (Int, Int, Player)
fromMove (Move (x,y) player) = (x,y, player)

fromMove' :: Board -> Move -> (Int, Int, Player)
fromMove' b = fromMove

-- The first exercise is to play an allowed move in a board, and get
-- the resulting board. Simply throw an error if the move is not
-- allowed in that board. We will only test your function with allowed
-- moves:

play :: Move -> Board -> Board
play (Move (x,y) PH) board@(Board size occupied player outcome) = let occupied' = insert (x+1,y) (insert (x,y) occupied) in 
                                                            ( Board size occupied' PV (getOutcome(board{occupied = occupied'})) ) -- update occupied so that both new occupied coordinates are included
play (Move (x,y) PV) board@(Board size occupied player outcome) = let occupied' = insert (x,y+1) (insert (x,y) occupied) in 
                                                            (Board size occupied' PH (getOutcome(board{occupied = occupied'})) )-- update occupied so that both new occupied coordinates are included

--yet to be tested so could be a cause of the problems.
getOutcome :: Board -> Outcome
getOutcome board@(Board size occupied player outcome) | player == PH = let moves = allowedMoves board in 
                                                        if null(moves) then minBound 
                                                        else length moves - length (possibleVMoves size occupied) 
                                                      | player == PV = let moves = allowedMoves board in
                                                        if null(moves) then maxBound
                                                        else length moves - length (possibleHMoves size occupied) 
                                                         



insert :: Ord x => x -> [x] -> [x]
insert x [] = [x]
insert x (vs@(y : ys)) 
    | x == y       = vs
    | x <  y       = x : vs
    | otherwise    = y : insert x ys

delete :: Ord x => x -> [x] -> [x]
delete x [] = []
delete x (vs@(y : ys))
    | x == y    = ys 
    | x <  y    = vs
    | otherwise = y : delete x ys 

-- Ah. But what are the allowed moves in a given board? You tell me:

allowedMoves :: Board -> [Move]
allowedMoves (Board size occupied player _) | player == PH = (possibleHMoves size occupied) 
                                            | otherwise    = (possibleVMoves size occupied)

-- Now build the tree of a game. You are allowed to base your
-- implementation on any of the given treeOf implementations for the
-- several tic-tac-toe programs in Canvas (discussed in the lectures):

treeOf :: Board -> Tree
treeOf board = Fork board [(m, treeOf(play m board)) | m <- allowedMoves board]


-- Now we want to have the computer playing first, lazily against an
-- opponent. The opponent supplies the list of moves. But the computer
-- is not allowed to cheat. It has to play its first move without
-- looking at any of the moves of the opponent:

computerFirst :: Tree -> [Move] -> [Move]
computerFirst tree@(Fork board subtree) moves 
 | null(allowedMoves board) = []
 | otherwise                = if null(optimalMoves tree) then [] else
                              let move = fst $ head (optimalMoves tree) in
                              let board' = play move board      in
                              move:computerSecond (treeOf board') moves

computerSecond :: Tree -> [Move] -> [Move]
computerSecond _ [] = []
computerSecond tree@(Fork board subtree) (opponentMoves) 
 | null(allowedMoves board) = []
 | otherwise                = let board' = play (head opponentMoves) board in
                              let tree' = treeOf board' in
                              if null(optimalMoves tree') then [] else
                              let move = fst $ head (optimalMoves tree') in
                              let board'' = play move board' in
                              move:(computerSecond (treeOf board'') (tail opponentMoves) )

-- And now you want the computer to play second. It will have to first
-- check the head move of the opponent, provided the list of moves is
-- non-empty, and base its first move (the head of the output list) on
-- that:                              

optimalMoves :: Tree -> [(Move,Tree)] -- returns the optimal moves that player can make.
optimalMoves (Fork board []) = []
optimalMoves (tree@(Fork _ forest)) =
  [(m,subtree) | (m,subtree) <- forest, 
                 optimalOutcome subtree == optimalOutcome tree]

optimalOutcome :: Tree -> Outcome
optimalOutcome (Fork board []) = getOutcome board 
optimalOutcome (Fork board forest) 
   | nextPlayer board == PH = supremum optimalOutcomes
   | otherwise              = infimum  optimalOutcomes
 where 
   optimalOutcomes = [optimalOutcome tree | (_,tree) <- forest]

supremum :: [Outcome] -> Outcome -- the function where we wish to maximise the outcome
supremum []        = minBound
supremum (x:xs)    | x == maxBound = maxBound
                   | x == minBound = supremum xs
                   | otherwise = supremum' x xs

supremum' :: Outcome -> [Outcome] -> Outcome --supreme' does the pruning, by using the first Outcome as the current maximum number found
supremum' currentMax [] = currentMax
supremum' currentMax (x:xs) | x == maxBound  = maxBound
                            | x > currentMax = supremum' x xs
                            | otherwise      = supremum' currentMax xs

infimum :: [Outcome] -> Outcome -- the function where we wish to minimise the outcome
infimum []         = maxBound
infimum (x:xs)     | x == minBound = minBound
                   | x == maxBound = infimum xs
                   | otherwise = infimum' x xs

infimum' :: Outcome -> [Outcome] -> Outcome -- infimum' does the pruning, by using the first Outcome as the current minimum number found
infimum' currentMinimum [] = currentMinimum
infimum' currentMinimum (x:xs)  | x == minBound  = minBound
                                | x < currentMinimum = infimum' x xs
                                | otherwise          = infimum' currentMinimum xs





-- This should be done so that the following example works:

iplay :: ([Move]->[Move]) -> ([Move]->[Move]) -> [Move]
iplay f g = intercalate ys xs
  where
    ys = f xs
    xs = g ys

intercalate :: [a] -> [a] -> [a]
intercalate []     ys = ys 
intercalate (x:xs) ys = x : intercalate ys xs

-- What the following example should do is produce the list of moves
-- that results from having the computer playing against itself:

example :: Tree -> [Move]
example tree = iplay (computerFirst tree) (computerSecond tree)

emptyBoard size = treeOf (Board size [] PH 0)
-- We now move to random playing. The randomness monad we used for
-- quick sort in the lecture is not sufficiently lazy for our
-- purposes. We work with a lazy Random monad based on
--
--   https://hackage.haskell.org/package/MonadRandomLazy-0.1/docs/Control-Monad-LazyRandom.html
--
-- instead, define below.  


-- We use the standard random generator as our type of seeds for
-- random things:

type Seed = StdGen

-- We get seeds for random-thing generation from Int's:

mkSeed :: Int -> Seed
mkSeed = mkStdGen

-- See https://en.wikipedia.org/wiki/Random_seed
-- We define the monad as follows:

newtype LRand a = LRand (Seed -> a)

instance Functor LRand where
 fmap f (LRand h) = LRand (f.h)

instance Applicative LRand where
 pure  = return
 (<*>) = ap

instance Monad LRand where
 return x = LRand (\seed -> x)  -- The seed is ignored.

 LRand m >>= k =                -- The seed is not only used, but also transformed and propagated.
   LRand (\s ->
     let (s1,s2)  = split s     -- The split function is predefined in the random libraries. Hoogle it.
         LRand m' = k (m s1)
      in m' s2
   )

-- The following are to "get out" this monad:

evalRand :: LRand a -> Seed -> a
evalRand (LRand f) s = f s

-- What this says is that if you have a random element of type a (that
-- is, something of type LRand a), one way to get something of type a
-- is to provide a seed.

-- This is like the above, but also produces a new seed, if we need it:

runRand :: LRand a -> Seed -> (a, Seed)
runRand (LRand f) s = (f s1, s2)
 where (s1, s2) = split s

-- And finally we need to be able to generate random elements:

getRandom :: Random a => LRand a
getRandom = LRand $ fst . random

-- But this needs a to be in the Random type class. Most types are
-- automatically there, and it is unlikely you will need to worry
-- about this in this exercise, unless you do very sophisticated
-- things.

-- We also may need to get random elements within a range:

getRandomR :: Random a => (a,a) -> LRand a
getRandomR range = LRand $ fst . randomR range

-- This is the end of our definition of our lazy randomness monad.

randomFirst :: Tree -> [Move] -> LRand [Move]
randomFirst tree@(Fork board subtree) moves 
 | null(allowedMoves board) = do return []
 | otherwise                = let allowedMoves' = allowedMoves board in
                               do
                               pick <- getRandomR (0, ((length allowedMoves')-1)) 
                               let move = ( allowedMoves' !! pick) 
                               let board' = (play move board) 
                               rest<- randomSecond (treeOf board') moves
                               return (move: rest) 

randomSecond :: Tree -> [Move] -> LRand [Move]
randomSecond _ [] = do return []
randomSecond tree@(Fork board subtree) (opponentMoves) 
 | null(allowedMoves board) = return []
 | otherwise                = let board' = play (head opponentMoves) board in
                              let allowedMoves' = allowedMoves board' in
                              if null(allowedMoves') then return [] 
                                                     else do
                                                          pick <- ( getRandomR (0, ((length allowedMoves')-1)) )
                                                          let move = ( allowedMoves' !! pick)  
                                                          let board'' = (play move board' )
                                                          rest<-(randomSecond (treeOf board'') (tail opponentMoves))
                                                          return (move: rest)



{-
randomFirst :: Tree -> [Move] -> LRand [Move]
randomFirst = (Fork board@(Board size occupied player outcome) ((move, subtree):subtrees) ) opponentMoves
  | null(allowedMoves board) = LRand []
  | otherwise = let allowedMoves' = allowedMoves board in
                let moveMade = allowedMoves' !! getRandomR (0, ((length allowedMoves') -1)) in
                moveMade:randomFirst' (Fork (play moveMade (play moveMade board)) opponentMoves)

randomFirst' :: Tree -> [Move] -> LRand [Move]
randomFirst' = undefined

randomSecond :: Tree -> [Move] -> LRand [Move]
randomSecond = undefined
-}

--play optimally but instead use some heuristic to speed up computations and save space, 
--but still trying to play "well" so that bigger boards can be played.

computerFirstHeuristic :: Board -> [Move] -> [Move]
computerFirstHeuristic board moves | null(allowedMoves board) = []
                                   | otherwise                = let tree = treeOf board in
                                                                if null(optimalMoves' tree) then [] else
                                                                let move = fst $ head (optimalMoves' tree) in
                                                                let board' = play move board      in
                                                                move:computerSecondHeuristic board' moves

computerSecondHeuristic :: Board -> [Move] -> [Move]
computerSecondHeuristic _ [] =  []
computerSecondHeuristic board (opponentMoves) 
 | null(allowedMoves board) = []
 | otherwise                = let board' = play (head opponentMoves) board in
                              let tree = treeOf board' in
                              if null(optimalMoves' tree) then [] else
                              let move = fst $ head (optimalMoves' tree) in
                              let board'' = play move board' in
                              move:(computerSecondHeuristic board'' (tail opponentMoves) )

--estimate size of tree and calculate whether the tree is small enough to calculate the outcome

optimalMoves' :: Tree -> [(Move,Tree)] -- returns the optimal moves that player can make.
optimalMoves' (Fork board []) = []
optimalMoves' (tree@(Fork board forest)) = let limit = 30 in
                                           let n     = length (allowedMoves board) in
  [(m,subtree) | (m,subtree) <- forest, 
                 optimalOutcome' subtree limit (n + length (allowedMoves (root subtree))) == optimalOutcome' tree limit n]

optimalOutcome' :: Tree -> Int -> Int -> Outcome
optimalOutcome' (Fork board []) _ _ = getOutcome board 
optimalOutcome' (Fork board forest) limit n = if n>limit then getOutcome board else
                                               if (n<limit) && (nextPlayer board == PH) then supremum optimalOutcomes else infimum  optimalOutcomes
                                                where 
                                                  optimalOutcomes = [optimalOutcome' tree limit (length (allowedMoves (root tree)) + n) | (_,tree) <- forest]
                                               

treeOf' :: Board -> Int -> Tree
treeOf' board n | n == 0    = (Fork board [])
                | otherwise = (Fork board [(m, treeOf' (play m board) (n-1)) | m <- allowedMoves board])

--computerFirstHeuristic :: Tree [Move] -> [Move]
--computerFirstHeuristic tree@(Fork board subtree) moves 
-- | null(allowedMoves board) = []
-- | otherwise                = let move = fst $ head (optimalMoves tree) in
--                              let board' = play move board      in
--                              move:computerSecondHeuristic (treeOf board') moves

--computerSecondHeuristic :: Tree -> [Move] -> [Move]
--computerSecondHeuristic _ [] = []
--computerSecondHeuristic tree@(Fork board subtree) (opponentMoves) 
-- | null(allowedMoves board) = []
-- | otherwise                = let board' = play (head opponentMoves) board      in
--                              let tree' = treeOf board' in
--                              let move = fst $ head (optimalMoves tree') in
--                              let board'' = play move board' in
--                              move:(computerSecondHeuristic (treeOf board'') (tail opponentMoves) )

instance Show Move where
    show  (Move (x,y) PH) = show (x,y) ++ " to " ++ show (x+1,y)
    show  (Move (x,y) PV) = show (x,y) ++ " to " ++ show (x+1,y)
