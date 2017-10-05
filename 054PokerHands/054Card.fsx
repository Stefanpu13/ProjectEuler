namespace Card
module Poker = 
    type Suit = 
    |Hearts
    |Diamonds
    |Clubs
    |Spades

    type CardValue =  int

    type Card = CardValue * Suit

    type Hand = Hand of Card []

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

    let getSuit suitStr = 
        match suitStr with
        | 'H' -> Hearts
        | 'D'-> Diamonds
        | 'C' -> Clubs
        | 'S' -> Spades
        | _ -> failwith "Invalid card suit"

    let getCardValue cardValue : CardValue = 
        match cardValue with
        | '2' ->  2
        | '3' ->  3
        | '4' ->  4
        | '5' ->  5
        | '6' ->  6
        | '7' ->  7
        | '8' ->  8
        | '9' ->  9
        | 'T' ->  10
        | 'J' ->  11
        | 'Q' ->  12
        | 'K' ->  13
        | 'A' ->  14
        | _ -> failwith "Invalid card value"
