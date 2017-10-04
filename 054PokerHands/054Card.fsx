namespace Card
module Poker = 
    type Suit = 
    |Hearts
    |Diamonds
    |Clubs
    |Spades

    type CardValue =  int

    type Card = CardValue * Suit

    type Hand = Card []

    type PokerHand = 
    |HighCard
    |Pair
    |TwoPairs
    |ThreeOfAKind
    |Straight 
    |Flush 
    |FullHouse
    |FourOfAKind
    |StraightFlush
    |RoyalFlush

    let getSuit (suitStr:string) = 
        match suitStr.ToLower() with
        | "h" -> Hearts
        | "d" -> Diamonds
        | "c" -> Clubs
        | "s" -> Spades
        | _ -> failwith "Invalid card suit"

    let getCardValue (cardValue:string) : CardValue = 
        match cardValue.ToLower() with
        | "2" ->  2
        | "3" ->  3
        | "4" ->  4
        | "5" ->  5
        | "6" ->  6
        | "7" ->  7
        | "8" ->  8
        | "9" ->  9
        | "t" ->  10
        | "j" ->  11
        | "q" ->  12
        | "k" ->  13
        | "a" ->  14
        | _ -> failwith "Invalid card value"
