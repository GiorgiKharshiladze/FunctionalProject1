module Project1 where

import System.Random
import System.IO
import Test.QuickCheck


data Suit = Club | Diamond | Heart | Spade
     deriving (Eq, Show, Enum)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King| Ace
      deriving (Eq, Ord, Show, Enum)

type Card = (Suit, Value)
type Deck = [Card]

showCard :: Card -> String
showCard (s, v) = show (v) ++ " " ++ "of" ++ " " ++ show (s)

helloWorld :: IO ()
helloWorld = putStrLn "Hello World!"

helloName :: IO ()
helloName = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn ("Hello " ++ name ++ "!")

eightBall :: IO ()
eightBall = do
    putStrLn "You may ask your question...."
    answer <- getLine
    let result = ["Yes", "I think so", "That is plausible", "You should pray", "No", "I don't think so", "Don't ask me", "Think about it"]
    i <- randomRIO(0, length result-1)
    putStrLn(result!!i)


autoWar :: IO ()
autoWar = do
    let wholeDeck = [(s,v) |s <- [Club .. Diamond], v <- [Two .. Ace]]
    let list1 = []
    let list2 = []
    i <- randomRIO(0, length wholeDeck-1)
    -- wholeDeck!!i : list1.1
    putStrLn (showCard(wholeDeck!!i))
