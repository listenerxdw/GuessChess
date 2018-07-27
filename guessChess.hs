-- Name       : Dawei Xu
-- ID         : 
-- Description: This program is for Declarative Programming Project 2.

-- |This is my version of ChessGuess. The methodology of my version is quite
-- simple, which is first finding out the number of chesses in the target,
-- and the number of black chesses and white chesses through guessing two
-- different color at the first two guesses. Then starting from guessing each
-- chess which is taken from the GameState. If the feedback shows the colours
-- decrease and the corrects increase, it means last chess taken from the
-- initial chess set is the right one. And if the kind increase, it means the
-- opposite color is the right guess. In this way, we can finally guess out
-- all the correct chesses.

module Project2 (initialGuess, nextGuess, GameState) where

-- Here I define the type of GameState as a 2-pair which include a String list
-- and a 2-pair Int. The String list is used to store all the possible chesses
-- while the Int pair is used to store the number of guesses and the actual
-- number of chesses, so that I can distinguish the first 2 guesses and the
-- others.

type GameState=([String],(Int,Int))


-- Initialize the GameState. Here I put all the chess sets in it except the
-- "BP", because the first guess will be "BP".

initialGS :: GameState
initialGS = (["WP","WN","BN","BB","WB","BR","WR","BQ","WQ","BK","WK"],(0,0))

------------------------Main Function------------------------------

-- The first guess will be all "BP".

initialGuess :: Int -> ([String],GameState)
initialGuess i = ( take i (repeat "BP") , initialGS)

-- This function will take the previous guess ,previous GameState and the
-- feedback then return the next guess and next GameState based on the previous
-- information.

nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (preString , preState) (corrects, kinds, colours)
            =(nextString, nextState)
            where 
            nextString= newGuess corrects kinds colours preString preState
            nextState = setState preState corrects colours


------------------------Auxiliary Function---------------------------
-- This function returns the length of the target, which is the number of
-- chesses through first two guesses information. Moreover, the second round
-- size will still be the max size.

getNum :: ([String],(Int,Int)) -> Int -> Int -> [String] -> Int
getNum gamestate corrects colours preString
    | fst(snd (gamestate))==0 = length(preString)
    | fst(snd (gamestate))==1 = (snd(snd(gamestate))+colours+corrects)
    | otherwise = snd(snd(gamestate)) 

-- This function is used to return the correct chess set.

getCorrect :: Int -> [String] -> [String]
getCorrect corrects preString = take corrects preString


-- This function is to put those chess set into correct chesses list which
-- have same kind but opposite color. We can just put these chess with different
-- color into the correct list.

addChess :: Int -> Int-> [String] -> [String] -> [String]
addChess corrects kinds preString (x:xs) 
        = (getCorrect corrects preString )++(take kinds (repeat x))



-- This function is to for creating new Strings. Through the feedback, it will
-- select the next chess set from initialGS and create a new guess.

newGuess :: Int -> Int -> Int -> [String] -> ([String],(Int,Int)) -> [String]
newGuess corrects kinds colours preString  gamestate
        = colourString ++ (take (len -length(colourString)) (repeat x))
        where 
            colourString = addChess corrects kinds preString (fst(gamestate))
            len          = getNum gamestate corrects colours preString
            x:xy         = getNext(fst(gamestate))               

--This function is to reset the GameState.

setState :: ([String],(Int,Int)) -> Int -> Int -> ([String],(Int,Int))
setState gamestate corrects colours 
        | round == 0 = (state,(1,colours))
        | round == 1 = (state,(2,(corrects +colours+len)))
        | otherwise  = (state,(2,len))
        where 
        state = getNext(getNext(fst(gamestate)) )
        round    = fst(snd(gamestate))
        len      = snd(snd(gamestate))



-- This function is to to return the next chess set in initialGS.

getNext :: [String] -> [String]
getNext (x:xs) = xs






