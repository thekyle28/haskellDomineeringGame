Strategy game exercise


--This will be your last exercise for this module. It has two parts:

(1) Unassessed, but mandatory part, based on tic-tac-toe as taught in
    the lectures.

    This handout contains the unassessed part. The deadline is
    Sunday 22 November, and you have to report your work in Canvas.
    Sample solutions are available in Canvas.

    This is crucial preparation for the assessed part (2). 
    
    The objective is to force you to go through the material of 
    the lectures in more detail, and to make you go a little bit 
    beyond the lectures. 
     
    *** In (1), you are allowed to freely discuss everything with 
    your colleagues. ***

(2) Assessed part, based on a different, but still relatively simple,
    strategy game, for which the same techniques apply, of course with
    suitable modifications and extensions.

    This will be available in a handout given on Monday 23 November.

    *** In (2), you are supposed to produce your own work. ***

    The deadline will be a suitable date in the last week of term.
    You will have interim deadlines. 

Approach
--------

Try to cheat as little as possible from the given sample solutions. If
you do have to cheat now and then, make sure, in order not to feel
guilty, you understand, either by yourself (preferably), or by
conversations with colleagues and demonstrators.

We now repeat and extend the handout tictactoe.lhs with added exercises
-----------------------------------------------------------------------

Playing TicTacToe optimally using game trees
--------------------------------------------

> import System.IO


The board and moves are

             0 | 1 | 2
            ---+---+---
             3 | 4 | 5
            ---+---+---
             6 | 7 | 8

> type Move = Int -- We will use only 0..8.

> possibleMoves :: [Move]
> possibleMoves = [0..8]

A list of moves wins if it contains a line, a column or a diagonal.

> wins :: [Move] -> Bool
> wins = someContained [[0,1,2],[3,4,5],[6,7,8], -- lines
>                       [0,3,6],[1,4,7],[2,5,8], -- columns
>                       [0,4,8],[2,4,6]]         -- diagonals

The players are

> data Player = X | O deriving (Eq,Show)

The outcome of the game can be 

    1  = X wins
    0  = draw
   -1  = O wins

Player X tries to maximize the outcome, and 
player O tries to minimize it.

> type Outcome = Int -- We will use only -1, 0, 1.

A board can be represented by a pair (xs,os) where xs is the list of X
moves and os is the list of O moves. 

   * When the length of the list xs++os is even, it is X's turn to
     play.

   * However, in order to make things more efficient, we will store
     whose turn it is.

   * The list of available moves consists of all moves in possibleMoves
     which are not in xs++os.

   * But, again for the sake of efficiency, we will keep the list of
     allowed moves in the board representation.

   * We will also store the outcome of the game for efficiency, even
     though it can again be computed from the other data.

> data Board = Board {
>                 nextPlayer     :: Player
>               , xmoves         :: [Move] 
>               , omoves         :: [Move] 
>               , allowedMoves   :: [Move]
>               , outcome        :: Outcome
>              }

> initialBoard :: Board
> initialBoard = Board X [] [] possibleMoves 0

Exercise. You have a board 

             X | X | 2
            ---+---+---
             3 | O | 5
            ---+---+---
             6 | 7 | 8

We assume that X started until this board was reached. Complete the
definition of this board:

> boardExample :: Board
> boardExample = Board O [0,1] [4] [2,3,5,6,7,8] 0

To check this, you can run

*Main> putStr(show boardExample)

To play a move, we don't check whether it is valid. We instead assume
that whenever the funcion play is called, it is called with an allowed
move. This is a potential source of bugs, and so should be used
carefully.

> play :: Move -> Board -> Board
> play m (Board X xs os as 0) = let xs' = insert m xs in
>   if wins xs' 
>      then Board O xs' os []            1
>      else Board O xs' os (delete m as) 0
> play m (Board O xs os as 0) = let os' = insert m os in
>   if wins os'
>      then Board X xs os' []          (-1)
>      else Board X xs os' (delete m as) 0
> play m board = error "Bug when using function play."

Exercise. Given a list of moves and a board, play the moves, getting a
final board. But maybe some moves are invalid. In that case, answer
Nothing.

> playMoves :: [Move] -> Board -> Maybe Board
> playMoves [] board 				= Just board
> playMoves _ board@(Board _ _ _ [] _ ) 	= Just board
> playMoves (mv:mvs) board@(Board X xs os as 0) = if not (elem mv as) then Nothing
>						  else playMoves mvs (play mv board)
> playMoves (mv:mvs) board@(Board O xs os as 0) = if not (elem mv as) then Nothing
>						  else playMoves mvs (play mv board)

Test your function with correct and incorrect lists of moves, say with
the initial board or with the boardExample (both defined above).

For example, you should get

*Main> playMoves [1,2,3,4,5] initialBoard 
Just O plays next
The current outcome of the game is 0

0 | X | O
--+---+--
X | O | X
--+---+--
6 | 7 | 8

*Main> playMoves [1,2,3,4,5,6] initialBoard 
Just X plays next
The current outcome of the game is -1

0 | X | O
--+---+--
X | O | X
--+---+--
O | 7 | 8

*Main> playMoves [1,2,3,4,5,6,7] initialBoard 
Nothing

> winning :: Board -> Bool
> winning board = outcome board /= 0

> data Tree = Fork Board [(Move,Tree)]

We refer to an element of the type [(Move,Tree)] as a forest.  A leaf
is a fork with an empty forest. It occurs when we have a winning
board.

Given a board, we build the game tree that starts with that board:

> treeOf :: Board -> Tree
> treeOf board = Fork board forest
>   where
>     forest :: [(Move,Tree)]
>     forest 
>       | winning board = [] 
>       | otherwise     = [(m, treeOf(play m board)) | m <- allowedMoves board]

> tictactoe :: Tree
> tictactoe = treeOf initialBoard

The number of plays is the number of paths:

> noplays :: Tree -> Int 
> noplays (Fork _ []) = 1
> noplays (Fork _ forest) = sum [noplays tree | (_,tree) <- forest]

*Main> noplays tictactoe 
255168

This means that there are 255168 valid plays (which is less than
9!=362880) . This is 2^6 * 3^2 * 443, where 443 is a prime
number. However, some of these 443 plays end up in the same board
position.

Assuming that both players play rationally, the following gives the
optimal outcome of a game (this is the minimax algorithm without
alpha-beta pruning -- another function optimalOutcome' which uses a
form of alpha-beta prunning is defined below.).

Exercise. Consider the tree of boardExample defined above.  How many
valid plays starting from it are there? Compute it with what we have
above, and give me the number.

Answer. 457 according to the function noplays.


We only very briefly discussed this in the 16 November 2015 lecture:

> optimalOutcome :: Tree -> Outcome
> optimalOutcome (Fork board []) = outcome board 
> optimalOutcome (Fork board forest) 
>    | nextPlayer board == X = maximum optimalOutcomes
>    | otherwise             = minimum optimalOutcomes
>  where 
>    optimalOutcomes = [optimalOutcome tree | (_,tree) <- forest]

*Main> optimalOutcome tictactoe 
0

This means that if the two players play rationally, the outcome of the
game is necessarily a draw.

Exercise. What if the initial board is taken to be boardExample
instead? Compute the answer using the above functions.

Answer. optimalOutcome (treeOf boardExample) 
0

An optimal move is one that produces the optimal outcome, assuming
that both players play rationally.

> optimalMoves :: Tree -> [(Move,Tree)]
> optimalMoves (Fork board []) = []
> optimalMoves (tree@(Fork _ forest)) =
>   [(m,subtree) | (m,subtree) <- forest, 
>                  optimalOutcome subtree == optimalOutcome tree]

Exercise (much harder and optional). Write another version that avoids
recomputations. I won't give a sample solution. I will instead take a
different approach to achieve this. This is reported in
tictactoe2.lhs. But you should have a go.

We now play interactively by navigating the tree. We show the board as
follows:

> instance Show Board where
>   show (Board pl xs os as oc) =
>        show pl ++ " plays next\n"
>     ++ "The current outcome of the game is " ++ show oc ++ "\n\n"
>     ++ f 0 ++ " | " ++ f 1 ++ " | " ++ f 2 ++ "\n"
>     ++ "--+---+--\n"
>     ++ f 3 ++ " | " ++ f 4 ++ " | " ++ f 5 ++ "\n"
>     ++ "--+---+--\n"
>     ++ f 6 ++ " | " ++ f 7 ++ " | " ++ f 8 ++ "\n"
>     where
>       f m | m `elem` xs = show X
>           | m `elem` os = show O
>           | otherwise   = show m

We call this function when it is the user's turn to play:

> usersTurn :: Tree -> IO()
> usersTurn (Fork board []) = 
>   putStrLn("Game over with outcome " ++ show(outcome board))
> usersTurn (tree@(Fork board forest)) = do
>   putStrLn(show board)
>   putStrLn("The optimal outcome is " ++ show(optimalOutcome tree))
>   putStr("Please play: ")
>   hFlush stdout
>   s <- getLine
>   let m = read s :: Move
>   case lookup m forest of
>     Nothing -> do
>       putStrLn "Invalid move. Try again."
>       usersTurn (Fork board forest)
>     Just tree -> do
>       if winning board
>          then putStrLn (show board ++ "\nYou win.")
>          else computersTurn tree

We call this function when it is the computer's turn to play:

> computersTurn :: Tree -> IO()
> computersTurn (Fork board []) = 
>   putStrLn("Game over with outcome " ++ show(outcome board))
> computersTurn (tree@(Fork board forest)) = do
>   putStrLn(show board)
>   putStrLn("I am thinking...")
>   putStrLn("The optimal outcome is " ++ show(optimalOutcome tree))
>   let myMoves = optimalMoves tree
>   putStrLn("My optimal moves are " ++ show [ m | (m,_) <- myMoves])
>   if null myMoves
>      then putStrLn "Draw."
>      else do
>        let (m,subtree) = head myMoves -- Choose the first good move, say.
>        putStrLn("I play " ++ show m)
>        let Fork board' forest' = subtree
>        if winning board' 
>           then putStrLn(show board' ++ "\nI win.")
>           else usersTurn subtree

Now choose who starts:

> main :: IO()
> main = computersTurn tictactoe

Try this, by running main. And also try instead to make the user to
have the first turn, and see what happens: 

> main2 :: IO() 
> main2 = usersTurn tictactoe

Exercise. Now suppose we want to have the computer playing against
itself. Write main3 for this purpose.

> main3 :: IO()
> main3 = computerAgainstItself tictactoe

> computerAgainstItself :: Tree -> IO()
> computerAgainstItself = undefined

This is what I get in my solution:

*Main> main3
X plays next
The current outcome of the game is 0

0 | 1 | 2
--+---+--
3 | 4 | 5
--+---+--
6 | 7 | 8

I am thinking...
The optimal outcome is 0
My optimal moves are [0,1,2,3,4,5,6,7,8]
I play 0
O plays next
The current outcome of the game is 0

X | 1 | 2
--+---+--
3 | 4 | 5
--+---+--
6 | 7 | 8

I am thinking...
The optimal outcome is 0
My optimal moves are [4]
I play 4
X plays next
The current outcome of the game is 0

X | 1 | 2
--+---+--
3 | O | 5
--+---+--
6 | 7 | 8

I am thinking...
The optimal outcome is 0
My optimal moves are [1,2,3,5,6,7,8]
I play 1
O plays next
The current outcome of the game is 0

X | X | 2
--+---+--
3 | O | 5
--+---+--
6 | 7 | 8

I am thinking...
The optimal outcome is 0
My optimal moves are [2]
I play 2
X plays next
The current outcome of the game is 0

X | X | O
--+---+--
3 | O | 5
--+---+--
6 | 7 | 8

I am thinking...
The optimal outcome is 0
My optimal moves are [6]
I play 6
O plays next
The current outcome of the game is 0

X | X | O
--+---+--
3 | O | 5
--+---+--
X | 7 | 8

I am thinking...
The optimal outcome is 0
My optimal moves are [3]
I play 3
X plays next
The current outcome of the game is 0

X | X | O
--+---+--
O | O | 5
--+---+--
X | 7 | 8

I am thinking...
The optimal outcome is 0
My optimal moves are [5]
I play 5
O plays next
The current outcome of the game is 0

X | X | O
--+---+--
O | O | X
--+---+--
X | 7 | 8

I am thinking...
The optimal outcome is 0
My optimal moves are [7,8]
I play 7
X plays next
The current outcome of the game is 0

X | X | O
--+---+--
O | O | X
--+---+--
X | O | 8

I am thinking...
The optimal outcome is 0
My optimal moves are [8]
I play 8
Game over with outcome 0
O plays next
The current outcome of the game is 0

X | X | O
--+---+--
O | O | X
--+---+--
X | O | X

You don't need to get literally the same answer to be correct.

This is a bit slow, right? At least at the beginning. Try to
understand what I did in tictactoe.lhs to make it faster (a version of
alpha-beta pruning).

Exercise. Now suppose we instead what to have two users playing
against each other.

> main4 :: IO()
> main4 = userAgainstUser tictactoe

> userAgainstUser :: Tree -> IO()
> userAgainstUser = undefined

I am not very clever with my solution, but it works. I get, for
instance:

*Main> main4
X plays next
The current outcome of the game is 0

0 | 1 | 2
--+---+--
3 | 4 | 5
--+---+--
6 | 7 | 8

Please play: 0
O plays next
The current outcome of the game is 0

X | 1 | 2
--+---+--
3 | 4 | 5
--+---+--
6 | 7 | 8

Please play: 2
X plays next
The current outcome of the game is 0

X | 1 | O
--+---+--
3 | 4 | 5
--+---+--
6 | 7 | 8

Please play: 4
O plays next
The current outcome of the game is 0

X | 1 | O
--+---+--
3 | X | 5
--+---+--
6 | 7 | 8

Please play: 8
X plays next
The current outcome of the game is 0

X | 1 | O
--+---+--
3 | X | 5
--+---+--
6 | 7 | O

Please play: 3
O plays next
The current outcome of the game is 0

X | 1 | O
--+---+--
X | X | 5
--+---+--
6 | 7 | O

Please play: 5
Game over
X plays next
The current outcome of the game is -1

X | 1 | O
--+---+--
X | X | O
--+---+--
6 | 7 | O

*Main> 

Anything like that, perhaps with improvements, is allowed as a valid
solution.

Appendix. Our list library, for the sake of self-containedness,
follows. We work with ordered lists without repetitions, for
the sake of efficiency.

> contained :: Ord x => [x] -> [x] -> Bool
> contained [] ys = True
> contained xs [] = False
> contained (us@(x : xs)) (y : ys) 
>     | x == y    = contained xs ys
>     | x >= y    = contained us ys
>     | otherwise = False

> someContained :: Ord x => [[x]] -> [x] -> Bool
> someContained [] ys = False
> someContained xss [] = False
> someContained (xs : xss) ys = contained xs ys || someContained xss ys

> insert :: Ord x => x -> [x] -> [x]
> insert x [] = [x]
> insert x (vs@(y : ys)) 
>     | x == y       = vs
>     | x <  y       = x : vs
>     | otherwise    = y : insert x ys

> delete :: Ord x => x -> [x] -> [x]
> delete x [] = []
> delete x (vs@(y : ys))
>     | x == y    = ys 
>     | x <  y    = vs
>     | otherwise = y : delete x ys 
