namespace Logic

#load "054Card.fsx"
open Card.Poker

module ClassicPoker =         
    let countUniqueCardsSuits cards = 
        Array.length (Array.countBy snd cards)

    let groupAndSortCardsByValue (cards: Hand) = 
        // Will sort by group length and, if same group lengths, 
        // will sort by card value
        let sortByGroupCountAndCardValue =         
            Array.sortByDescending (fun card -> (snd >> Seq.length) card, fst card) 

        (Array.groupBy fst >> sortByGroupCountAndCardValue) cards

    let cardsGroupsCount cardsGroups = 
        (Array.map (snd >> Seq.length)) cardsGroups

    let uniqueCardValuesHand sortedCards = 
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