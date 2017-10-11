(*
    In the card game poker, a hand consists of five cards and are ranked, 
    from lowest to highest, in the following way:

    High Card: Highest value card.
    One Pair: Two cards of the same value.
    Two Pairs: Two different pairs.
    Three of a Kind: Three cards of the same value.
    Straight: All cards are consecutive values.
    Flush: All cards of the same suit.
    Full House: Three of a kind and a pair.
    Four of a Kind: Four cards of the same value.
    Straight Flush: All cards are consecutive values of same suit.
    Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
    The cards are valued in the order:
    2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

    If two players have the same ranked hands then the rank made up of the highest value wins; for example,
    a pair of eights beats a pair of fives (see example 1 below). 
    But if two ranks tie, for example, both players have a pair of queens, 
    then highest cards in each hand are compared (see example 4 below); 
    if the highest cards tie then the next highest cards are compared, and so on.

    The file, poker.txt, contains one-thousand random hands dealt to two players. 
    Each line of the file contains ten cards (separated by a single space): 
    the first five are Player 1's cards and the last five are Player 2's cards. 
    You can assume that all hands are valid (no invalid characters or repeated cards),
    each player's hand is in no specific order, and in each hand there is a clear winner.

    How many hands does Player 1 win?
*)

#load "054Card.fsx"
open Card.Poker
open System.IO
open System

let splitStr (separator: char []) (str: string) = 
    str.Split(separator, StringSplitOptions.RemoveEmptyEntries)

let countUniqueCardsSuits cards = 
    Array.length (Array.countBy snd cards)

let groupAndSortCardsByValue (Hand cards) =     
    let sortByGroupLengthAndCardValue =         
        Array.sortByDescending (fun card -> (snd >> Seq.length) card, fst card)

    (Array.groupBy fst >> sortByGroupLengthAndCardValue >> Array.map snd) cards

let cardsGroupsCount cardsGroups =     
    Array.map Seq.length cardsGroups
let toCard (cardString : string) : Card = 
    (getCardValue cardString.[0], getSuit cardString.[1])

let uniqueCardValuesHand (Hand sortedCards) = 
    let suitsCount =  countUniqueCardsSuits sortedCards
    let highCard = Array.maxBy fst sortedCards |> fst
    let lowCard = Array.minBy fst sortedCards |> fst

    match suitsCount, (highCard - lowCard), highCard with
    | 1, 4, 14 -> RoyalFlush
    | 1, 4, _ -> StraightFlush
    | 1, _, _ -> Flush
    | _, 4, _ -> Straight
    | _ -> HighCard

let getPokerHand hand =     
    let cardsGroupedByValue = groupAndSortCardsByValue hand    

    match cardsGroupsCount cardsGroupedByValue with
    | [|2;1;1;1;|] -> Pair, cardsGroupedByValue
    | [|2;2;1|] -> TwoPairs, cardsGroupedByValue
    | [|3;1;1;|] -> ThreeOfAKind, cardsGroupedByValue
    | [|3;2|] -> FullHouse, cardsGroupedByValue
    | [|4;1;|] -> FourOfAKind, cardsGroupedByValue
    | _ -> uniqueCardValuesHand hand, cardsGroupedByValue

let twoPlayersHandsInput = 
    File.ReadAllLines "054PokerHands/054PokerHands.txt"        
    |> Array.map ((splitStr [|' '|]) >> (Array.splitAt 5))

let twoPlayersHands : (Hand * Hand) [] = 
    twoPlayersHandsInput
    |> Array.map (fun (player1Hand, player2Hand) -> 
        Hand (Array.map toCard player1Hand), Hand (Array.map toCard player2Hand)
    )

twoPlayersHands
|> Array.filter (fun (player1Hand, player2Hand) -> 
    getPokerHand player1Hand > getPokerHand player2Hand
)
|> Seq.length
