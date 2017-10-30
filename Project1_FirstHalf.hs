module Project1 where
import System.Random
import System.IO
import Data.List

data Suit = Club | Diamond | Heart | Spade
     deriving (Eq, Show, Enum)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King| Ace
      deriving (Eq, Ord, Show, Enum)

type Card = (Suit, Value)
type Deck = [Card]



showCard :: Card -> String
showCard (s,v) = show (v) ++ " " ++ "of" ++ " " ++ show (s)

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

cardShuffle :: Deck -> Deck -> IO Deck
cardShuffle [] shuffled_deck = return shuffled_deck
cardShuffle original_deck shuffled_deck = do
    i <- randomRIO(0, length original_deck - 1)
    let chosen_card = original_deck!!i
        left_deck = take i original_deck
        right_deck = drop (i+1) original_deck
    cardShuffle (right_deck++left_deck) (chosen_card:shuffled_deck)

playWarInteractive :: Deck -> Deck -> IO ()
playWarInteractive first_hand [] = putStrLn "Sorry, you lose!!"
playWarInteractive [] second_hand = putStrLn "Congratulations, you have won!!"
playWarInteractive first_hand second_hand = do
    if (snd(first_hand!!0) > snd(second_hand!!0)) then do
        let temp = [second_hand!!0, first_hand!!0]
        let first_new = drop 1 first_hand
        let second_new = drop 1 second_hand
        strin <- getLine
        putStrLn "Computer has a:" 
        putStrLn (showCard(first_hand!!0))
        putStrLn "You have a: " 
        putStrLn (showCard(second_hand!!0))
        putStrLn "Computer won this round!"
        playWarInteractive (first_new ++ temp) second_new
    else 
        if (snd(first_hand!!0) < snd(second_hand!!0)) then do
            let temp = [first_hand!!0, second_hand!!0]
            let first_new = drop 1 first_hand
            let second_new = drop 1 second_hand
            strin <- getLine
            putStrLn "Computer has a: "
            putStrLn (showCard(first_hand!!0))
            putStrLn "You have a: " 
            putStrLn (showCard(second_hand!!0))
            putStrLn "You won this round!"
            playWarInteractive first_new (second_new++temp)
    else do
        string <- getLine
        putStrLn "It's a draw!"
        putStrLn "Computer has a :"
        putStrLn (showCard(first_hand!!0))
        putStrLn "You have a :"
        putStrLn (showCard(second_hand!!0))
        if ((length first_hand) >4) && ((length second_hand) > 4) then do
            let  warLoot = [first_hand!!0, first_hand!!1, first_hand!!2, first_hand!!3, first_hand!!4, second_hand!!0, second_hand!!1, second_hand!!2, second_hand!!3, second_hand!!4]
            if (snd(first_hand!!4)) < snd(second_hand!!4) then do
            let first_new = drop 5 first_hand 
            let second_new = drop 5 second_hand
            strin <- getLine
            putStrLn "Computer's fourth card is a :"
            putStrLn (showCard (first_hand!!4))
            putStrLn "Your fourth card is a :" 
            putStrLn (showCard (second_hand!!4))
            putStrLn "Computer has won the war!"
            playWarInteractive (first_new++warLoot) second_new
            else do 
                let first_new = drop 5 first_hand
                let second_new = drop 5 second_hand
                strin <- getLine
                putStrLn "Computer's fourth card is a "
                putStrLn (showCard (first_hand!!4))
                putStrLn "Your fourth card is a " 
                putStrLn (showCard (second_hand!!4))
                putStrLn "You have won the war!"
                playWarInteractive first_new (second_new++warLoot)
        else 
            if ((length first_hand) > 4) && ((length second_hand) < 4) then do 
                if (snd(first_hand!!4)) < (snd(last second_hand)) then do
                    let warLoot = [first_hand!!0, first_hand!!1, first_hand!!2, first_hand!!3, first_hand!!4]
                    let first_new = drop 5 first_hand
                    let second_new = second_hand++warLoot
                    strin <- getLine
                    putStrLn "Computer's fourth card is a " 
                    putStrLn (showCard(first_hand!!4))
                    putStrLn "Your last card is a " 
                    putStrLn (showCard(last second_hand))
                    putStrLn "You have won the war!"
                    playWarInteractive first_new second_new       
                else do
                    let first_new = first_hand ++ second_hand
                    let second_new = []
                    strin <- getLine
                    putStrLn "Computer's fourth card is a "
                    putStrLn (showCard(first_hand!!4))
                    putStrLn "Your last card is a " 
                    putStrLn (showCard(last second_hand))
                    putStrLn "Computer has won the war!"
                    playWarInteractive first_new second_new
            else 
                if ((length first_hand) < 4) && ((length second_hand) > 4) then do 
                    if (snd(last first_hand)) > (snd(second_hand!!4)) then do 
                        let warLoot = [second_hand!!0, second_hand!!1, second_hand!!2, second_hand!!3, second_hand!!4]
                        let first_new = first_hand ++ warLoot
                        let second_new = drop 5 second_hand
                        strin <- getLine
                        putStrLn "Compter's last card is a "
                        putStrLn (showCard(last first_hand))
                        putStrLn "Your fourth card is a " 
                        putStrLn (showCard(second_hand!!4))
                        putStrLn "Computer has a won the war!"
                        playWarInteractive first_new second_new
                    else do
                        let first_new = []
                        let second_new = second_hand ++ first_hand
                        strin <- getLine
                        putStrLn "Computer's last card is a " 
                        putStrLn (showCard(last first_hand))
                        putStrLn "Your fourth card is a " 
                        putStrLn (showCard(second_hand!!4))
                        putStrLn "You have won the war!"
                        playWarInteractive first_new second_new
            else do
                if (snd(last first_hand)) > (snd (last second_hand)) then do
                    let first_new = first_hand ++ second_hand
                    let second_new = []
                    string <- getLine
                    putStrLn "Computer's last card is a " 
                    putStrLn (showCard(last first_hand))
                    putStrLn "Your last card is a " 
                    putStrLn (showCard(last second_hand))
                    putStrLn "Computer has won the war!"
                    playWarInteractive first_new second_new
                else do
                    let first_new = []
                    let second_new = first_hand ++ second_hand
                    string <- getLine
                    putStrLn "Computer's last card is a " 
                    putStrLn (showCard(last first_hand))
                    putStrLn "Your last card is a "
                    putStrLn (showCard(last second_hand))
                    putStrLn "You have won the war!"
                    playWarInteractive first_new second_new

playWarAuto :: Deck -> Deck -> IO ()
playWarAuto first_hand [] = putStrLn "Sorry, you lose!!"
playWarAuto [] second_hand = putStrLn "Congratulations, you have won!!"
playWarAuto first_hand second_hand = do
    if (snd(first_hand!!0) > snd(second_hand!!0)) then do
        let temp = [second_hand!!0, first_hand!!0]
        let first_new = drop 1 first_hand
        let second_new = drop 1 second_hand
        putStrLn "Computer has a:" 
        putStrLn (showCard(first_hand!!0))
        putStrLn "You have a: " 
        putStrLn (showCard(second_hand!!0))
        putStrLn "Computer won this round!"
        playWarAuto (first_new ++ temp) second_new
    else 
        if (snd(first_hand!!0) < snd(second_hand!!0)) then do
            let temp = [first_hand!!0, second_hand!!0]
            let first_new = drop 1 first_hand
            let second_new = drop 1 second_hand
            putStrLn "Computer has a: "
            putStrLn (showCard(first_hand!!0))
            putStrLn "You have a: " 
            putStrLn (showCard(second_hand!!0))
            putStrLn "You won this round!"
            playWarAuto first_new (second_new++temp)
    else do
        putStrLn "It's a draw!"
        putStrLn "Computer has a :"
        putStrLn (showCard(first_hand!!0))
        putStrLn "You have a :"
        putStrLn (showCard(second_hand!!0))
        if ((length first_hand) >4) && ((length second_hand) > 4) then do
            putStrLn "Both have more than 4 cards"
            let  warLoot = [first_hand!!0, first_hand!!1, first_hand!!2, first_hand!!3, first_hand!!4, second_hand!!0, second_hand!!1, second_hand!!2, second_hand!!3, second_hand!!4]
            if (snd(first_hand!!4)) < snd(second_hand!!4) then do
            let first_new = drop 5 first_hand 
            let second_new = drop 5 second_hand
            putStrLn "Computer's fourth card is a :"
            putStrLn (showCard (first_hand!!4))
            putStrLn "Your fourth card is a :" 
            putStrLn (showCard (second_hand!!4))
            putStrLn "Computer has won the war!"
            playWarAuto (first_new++warLoot) second_new
            else do 
                let first_new = drop 5 first_hand
                let second_new = drop 5 second_hand
                putStrLn "Computer's fourth card is a "
                putStrLn (showCard (first_hand!!4))
                putStrLn "Your fourth card is a " 
                putStrLn (showCard (second_hand!!4))
                putStrLn "You have won the war!"
                playWarAuto first_new (second_new++warLoot)
        else 
            if ((length first_hand) > 4) && ((length second_hand) < 4) then do 
                putStrLn "Computer has more than four cards, you have less than four cards"
                if (snd(first_hand!!4)) < (snd(last second_hand)) then do
                    let warLoot = [first_hand!!0, first_hand!!1, first_hand!!2, first_hand!!3, first_hand!!4]
                    let first_new = drop 5 first_hand
                    let second_new = second_hand++warLoot
                    putStrLn "Computer's fourth card is a " 
                    putStrLn (showCard(first_hand!!4))
                    putStrLn "Your last card is a " 
                    putStrLn (showCard(last second_hand))
                    putStrLn "You have won the war!"
                    playWarAuto first_new second_new       
                else do
                    let first_new = first_hand ++ second_hand
                    let second_new = []
                    putStrLn "Computer's fourth card is a "
                    putStrLn (showCard(first_hand!!4))
                    putStrLn "Your last card is a " 
                    putStrLn (showCard(last second_hand))
                    putStrLn "Computer has won the war!"
                    playWarAuto first_new second_new
            else 
                if ((length first_hand) < 4) && ((length second_hand) > 4) then do
                    putStrLn "Computer has less than four cards, you have more than four cards" 
                    if (snd(last first_hand)) > (snd(second_hand!!4)) then do 
                        let warLoot = [second_hand!!0, second_hand!!1, second_hand!!2, second_hand!!3, second_hand!!4]
                        let first_new = first_hand ++ warLoot
                        let second_new = drop 5 second_hand
                        putStrLn "Compter's last card is a "
                        putStrLn (showCard(last first_hand))
                        putStrLn "Your fourth card is a " 
                        putStrLn (showCard(second_hand!!4))
                        putStrLn "Computer has a won the war!"
                        playWarAuto first_new second_new
                    else do
                        let first_new = []
                        let second_new = second_hand ++ first_hand
                        putStrLn "Computer's last card is a " 
                        putStrLn (showCard(last first_hand))
                        putStrLn "Your fourth card is a " 
                        putStrLn (showCard(second_hand!!4))
                        putStrLn "You have won the war!"
                        playWarAuto first_new second_new
            else do
                putStrLn "Both players have less than four cards"
                if (snd(last first_hand)) > (snd (last second_hand)) then do
                    let first_new = first_hand ++ second_hand
                    let second_new = []
                    putStrLn "Computer's last card is a " 
                    putStrLn (showCard(last first_hand))
                    putStrLn "Your last card is a " 
                    putStrLn (showCard(last second_hand))
                    putStrLn "Computer has won the war!"
                    playWarAuto first_new second_new
                else do
                    let first_new = []
                    let second_new = first_hand ++ second_hand
                    putStrLn "Computer's last card is a " 
                    putStrLn (showCard(last first_hand))
                    putStrLn "Your last card is a "
                    putStrLn (showCard(last second_hand))
                    putStrLn "You have won the war!"
                    playWarAuto first_new second_new

interactiveWar :: IO ()
interactiveWar= do
    let wholeDeck = [(s,v) |s <- [Club .. Spade], v <- [Two .. Ace]]
    shuffled <- cardShuffle wholeDeck []
    let first_shuffle = take 26 shuffled
    shuffled_deck_one <- cardShuffle first_shuffle []
    let second_shuffle = drop 26 shuffled
    shuffled_deck_two <- cardShuffle second_shuffle []
    putStrLn "Time to go to war! Press enter to draw new cards."
    playWarInteractive shuffled_deck_one shuffled_deck_two

autoWar :: IO ()
autoWar = do
    let wholeDeck = [(s,v) |s <- [Club .. Spade], v <- [Two .. Ace]]
    shuffled <- cardShuffle wholeDeck []
    let first_shuffle = take 26 shuffled
    shuffled_deck_one <- cardShuffle first_shuffle []
    let second_shuffle = drop 26 shuffled
    shuffled_deck_two <- cardShuffle second_shuffle []
    putStrLn "Cards will be automatically drawn!"
    playWarAuto shuffled_deck_one shuffled_deck_two












    



  
       

  



    

  






    





