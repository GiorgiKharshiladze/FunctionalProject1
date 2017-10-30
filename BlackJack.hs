module Project1 where
import System.Random
import System.IO
import Data.List

data Suit = Club | Diamond | Heart | Spade
     deriving (Eq, Show, Enum)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King| Ace
      deriving (Eq, Show, Enum)

type Card = (Suit, Value)
type Deck = [Card]

showCard :: Card -> String
showCard (s,v) = show (v) ++ " " ++ "of" ++ " " ++ show (s)

cardVal :: Card -> [Integer]
cardVal (_, Two) = [2]
cardVal (_, Three) = [3]
cardVal (_, Four) = [4]
cardVal (_, Five) = [5]
cardVal (_, Six) = [6]
cardVal (_, Seven) = [7]
cardVal (_, Eight) = [8]
cardVal (_, Nine) = [9]
cardVal (_, Ten) = [10]
cardVal (_, Jack) = [10]
cardVal (_, Queen) = [10]
cardVal (_, King) = [10]
cardVal (_, Ace) = [1,11]

cardShuffle :: Deck -> Deck -> IO Deck
cardShuffle [] shuffled_deck = return shuffled_deck
cardShuffle original_deck shuffled_deck = do
    i <- randomRIO(0, length original_deck - 1)
    let chosen_card = original_deck!!i
    let left_deck = take i original_deck
    let right_deck = drop (i+1) original_deck
    cardShuffle (right_deck++left_deck) (chosen_card:shuffled_deck)

listSum :: [Integer] -> [[Integer]] -> [Integer]
listSum x [] = [l | l <- x, l <= 21]
listSum x y = do
        let new_list = [a+b | a <- x, b <- y!!0]
        let updated_list = drop 1 y 
        listSum new_list updated_list 

handValue :: Deck -> Integer
handValue deck 
    |(listSum [0] (map cardVal deck)) == [] = 22
    |otherwise = last (sort (listSum [0] (map cardVal deck))) 

blackJack :: IO ()
blackJack = do
    let wholeDeck = [(s,v) | s <- [Club .. Spade], v <- [Two .. Ace]]
    shuffled <- cardShuffle wholeDeck [] 
    let dealer_hand = [shuffled!!0, shuffled!!1]
    let player_hand = [shuffled!!2, shuffled!!3]
    putStrLn "One of the dealer's cards is a :"
    putStrLn (showCard(dealer_hand!!0))
    putStrLn "Your cards are: "
    putStrLn (showCard(player_hand!!0))
    putStrLn (showCard(player_hand!!1))
    initialBlackJack player_hand dealer_hand shuffled 

initialBlackJack :: Deck -> Deck -> Deck -> IO ()
initialBlackJack player_hand dealer_hand shuffled = do
    putStrLn "Your hand value is: "
    putStrLn (show(handValue player_hand))
    if (handValue player_hand == 21) && (handValue dealer_hand < 21) then do
        putStrLn "Your first hand was a BlackJack, you win!"
    else if (handValue player_hand == 21) && (handValue dealer_hand == 21) then do
        putStrLn "Both you and the dealer had a BlackJack, it's a tie!"
    else do
        putStrLn "Do you want to hit or stand? Press 'h' for hit and 's' for stand"
        ans <- getLine
        if (ans == "h") then do 
            hitCard player_hand dealer_hand shuffled 
        else do
            putStrLn "Standing..Waiting for the dealer."
            standCard player_hand dealer_hand shuffled


standCard :: Deck -> Deck -> Deck -> IO ()
standCard player_hand dealer_hand shuffled = do
    if (handValue dealer_hand < 17) then do 
        let temp = [shuffled!!0]
        let new_hand = dealer_hand ++ temp 
        let new_shuffled = drop 1 shuffled 
        putStrLn "The dealer drew a: "
        putStrLn (showCard(shuffled!!0))
        putStrLn "Dealer hand value now : "
        putStrLn (show(handValue new_hand))
        standCard player_hand new_hand new_shuffled 
    else if (handValue dealer_hand > 17) && (handValue dealer_hand < 21) then do
        putStrLn "Final value of dealer's hand: "
        putStrLn (show(handValue dealer_hand))
        putStrLn "Final value of your hand: "
        putStrLn (show(handValue player_hand))
        if (handValue dealer_hand > handValue player_hand) then do 
            putStrLn "Dealer has won this round! Try again!"
        else do 
            putStrLn "You have won this round!"
    else if (handValue dealer_hand == 21) then do 
        if (handValue player_hand == 21) then do 
            putStrLn "Value of the dealer's hand is:"
            putStrLn (show(handValue dealer_hand))
            putStrLn "It's a tie!"
        else do 
            putStrLn (show(handValue dealer_hand))
            putStrLn "Dealer has a BlackJack! You've lost!"
    else do
        putStrLn "The dealer busted! You've won the round!"

hitCard :: Deck -> Deck -> Deck -> IO ()
hitCard player_hand dealer_hand shuffled = do
    let temp = [shuffled!!0]
    let new_hand = player_hand ++ temp
    let new_shuffled = drop 1 shuffled
    putStrLn "You drew a: "
    putStrLn (showCard(last new_hand))
    putStrLn "Your hand value is now: "
    putStrLn (show(handValue new_hand))
    if (handValue new_hand > 21) then do 
        putStrLn "It's a bust, you lost!"
    else if (handValue new_hand < 21) then do
        putStrLn "Do you want to hit or stand? Press 'h' for hit and 's' for stand."
        ans <- getLine
        if (ans == "h") then do
            hitCard new_hand dealer_hand new_shuffled 
        else do 
            standCard new_hand dealer_hand new_shuffled 
    else do 
        putStrLn "BlackJack! Standing and waiting for the dealer....."
        standCard new_hand dealer_hand new_shuffled 













