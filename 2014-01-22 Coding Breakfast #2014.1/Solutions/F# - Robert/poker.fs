open System

type Suit =
    | Hearts
    | Diamonds
    | Clubs
    | Spades
    static member FromInt32 i =
        match i with
        | 0 -> Hearts
        | 1 -> Diamonds
        | 2 -> Clubs
        | 3 -> Spades
        | _ -> failwith "invalid suite number"
    member x.ToShortString() =
        match x with
        | Hearts -> "H"
        | Diamonds -> "D"
        | Clubs -> "C"
        | Spades -> "S"
    member x.ToSymbol() =
        match x with
        | Hearts -> "♥"
        | Diamonds -> "♦"
        | Clubs -> "♣"
        | Spades -> "♠"

type Number =
    | Ace
    | King
    | Queen
    | Jack
    | Value of int
    static member FromInt32 i =
        match i with
        | 1 -> Ace
        | 13 -> King
        | 12 -> Queen
        | 11 -> Jack
        | i when 2 <= i && i <= 10 -> Value i
        | _ -> failwith "invalid card number"
    member x.ToInt32 aceHigh =
        match x with
        | Ace -> if aceHigh then 14 else 1
        | King -> 13
        | Queen -> 12
        | Jack -> 11
        | Value i -> i
    override x.ToString() =
        match x with
        | Ace -> "Ace"
        | King -> "King"
        | Queen -> "Queen"
        | Jack -> "Jack"
        | Value i -> sprintf "%i" i
    member x.ToShortString() =
        match x with
        | Ace -> "A"
        | King -> "K"
        | Queen -> "Q"
        | Jack -> "J"
        | Value i -> sprintf "%i" i

type Card =
    { Suit: Suit
      Number: Number }
    override x.ToString() =
        sprintf "%s %s" (x.Number.ToString()) (x.Suit.ToString())
    member x.ToShortString() =
        sprintf "%s%s" (x.Suit.ToShortString()) (x.Number.ToShortString())

let cardListToString (cards: seq<Card>) =
    let cardStrings =
        cards |> Seq.map (fun x -> x.ToShortString())
    String.Join(", ", cardStrings)

type Hand =
    | StraightFlush of list<Card> 
    | FourOfAKind of list<Card> * Card
    | FullHouse of list<Card> * list<Card>
    | Fush of list<Card>
    | Straight of list<Card> 
    | ThreeOfAKind of list<Card> * list<Card>
    | TwoPair of list<Card> * list<Card> * list<Card>
    | Pair of list<Card> * list<Card>
    | Highcard of list<Card> 
override x.ToString() =
    match x with
    | StraightFlush cards -> sprintf "Straight flush %s" (cardListToString cards)
    | FourOfAKind (cards, remaining) -> sprintf "Four of a kind %s, high card %s" (cardListToString cards) (remaining.ToShortString())
    | FullHouse (threeOfAKind, pair) -> sprintf "Full house %s, %s" (cardListToString cards) (cardListToString pair)
    | Fush cards -> sprintf "Fush %s" (cardListToString cards)
    | Straight cards -> sprintf "Straight %s" (cardListToString cards)
    | ThreeOfAKind (cards, remaining) -> sprintf "Three of a kind %s, high cards %s" (cardListToString cards) (cardListToString remaining)
    | TwoPair (pair1, pair2, remaining) -> sprintf "Two pair %s, %s high cards %s" (cardListToString pair1) (cardListToString pair2) (cardListToString remaining)
    | Pair (pair, remaining) -> sprintf "Pair %s, %s high cards %s" (cardListToString pair) (cardListToString remaining)
    | Highcard cards ->  sprintf "High card %s" (cardListToString cards)


let getRemaining foundCards allCards =
    allCards
    |> Seq.filter (fun x -> not (Seq.exists (fun y -> x = y) foundCards))  
    |> List.ofSeq

let findGroup groupBy strict length (cards: seq<Card>) =
    cards
    |> Seq.groupBy groupBy
    |> Seq.tryFind (fun (suit, cards) -> if strict then Seq.length cards = length else Seq.length cards >= length)
    |> Option.map (fun (suit, foundCards) -> suit, foundCards |> List.ofSeq, getRemaining foundCards cards)


let (|IsFlush|_|) strict length (cards: seq<Card>) =
    findGroup (fun x -> x.Suit) strict length cards

let (|IsOfAKind|_|) strict length (cards: seq<Card>) =
    findGroup (fun x -> x.Number) strict length cards

let (|IsStraight|_|) length (cards: seq<Card>) =
    let innerTest cardValue = 
        let sortedCards = 
            cards
            |> Seq.map (fun x -> cardValue x, x)  
            // sort backwards to ensure we get the highest run
            |> Seq.sortBy (fun (x, _) -> -x)
            |> List.ofSeq
        let rec testForSequence cards acc accLength =
            match cards with
            | (v1, c1) :: (v2, c2) :: rest -> 
                let nextAccLenth = accLength + 1
                printfn "%i %i %i %i" v1 v2 nextAccLenth length
                // because we're peaking at the next one, we need a minus one on the length if we've found the full list
                if v1 = v2 + 1 && nextAccLenth = length - 1 then 
                    (v1, c1) :: (v2, c2) :: acc
                elif v1 = v2 + 1 then 
                    testForSequence ((v2, c2) :: rest) ((v1, c1) :: acc) nextAccLenth
                elif v1 = v2 then 
                    // if equal ignore, could still be a run, but depends on next card
                    testForSequence ((v2, c2) :: rest) acc nextAccLenth
                else 
                    testForSequence ((v2, c2) :: rest) [] 0
            | head :: [] -> head :: acc
            | [] -> acc
        testForSequence sortedCards [] 0 |> List.map snd 
    let aceHighRun = innerTest (fun x -> x.Number.ToInt32 true)
    let highLength = List.length aceHighRun
    printfn "highLength %i" highLength
    if highLength = length then
        let remaining = getRemaining aceHighRun cards
        Some(aceHighRun, remaining)
    else
        let aceLowRun = innerTest (fun x -> x.Number.ToInt32 false)
        let lowLength = List.length aceLowRun
        if lowLength = length then
            let remaining = getRemaining aceLowRun cards
            Some(aceLowRun, remaining)
        else
            None

let (|IsStraightFlush|_|) length hand =
    match hand with
    | IsFlush false length (suite, flushCards, remaining) -> 
        match flushCards with 
        | IsRun length (runCards, _) -> Some (suite, runCards, remaining)
        | _ -> None
    | _ -> None

let (|IsDoubleOfAKind|_|) length1 length2 hand =
    match hand with
    | IsOfAKind true length1 (number, threeOfAKind, remaining) -> 
        printfn "found three %s %s" (cardListToString threeOfAKind) (cardListToString remaining) 
        match remaining with 
        | IsOfAKind false length2 (number, pair, remaining) -> 
            match pair with
            | c1 :: c2 :: rest ->
                Some (threeOfAKind, [c1; c2], rest @ remaining)
            | _ -> failwith "should never be less than two elements in this list"
        | _ -> None
    | _ -> None

let pickHighest (cards: seq<Card>) length =
    cards 
    |> Seq.sortBy (fun x -> - (x.ToInt32 true)
    |> Seq.take length
    |> Seq.toList

let nameHand hand =
    match hand with
    | IsStraightFlush 5 (suit, runCards, remaining) -> printfn "Running flush %A %s" suit (cardListToString runCards) 
    | IsOfAKind true 4 (number, kindCards, remaining) -> printfn "Four of a kind %s" (cardListToString kindCards) 
    | IsDoubleOfAKind 3 2 (threeOfAKind, pair, remaining) -> printfn "Full house %s, %s" (cardListToString threeOfAKind) (cardListToString pair)
    | IsFlush false 5 (suite, flushCards, remaining) -> printfn "Five flush %s" (cardListToString flushCards)
    | IsStraight 5 (runCards, remaining) -> printfn "Five run %s" (cardListToString runCards)
    | IsOfAKind true 3 (number, kindCards, remaining) -> printfn "Three of a kind %s" (cardListToString kindCards) 
    | IsDoubleOfAKind 2 2 (pair1, pair2, remaining) -> printfn "Two pair %s, %s" (cardListToString pair1) (cardListToString pair2)
    | IsOfAKind true 2 (number, kindCards, remaining) -> printfn "Pair of %s" (cardListToString kindCards) 
    | _ -> printfn "Highcard %s" (pickHighest 5)

// tests

let deck =
    [| for x in 0 .. 3 do
        for y in 1 .. 13 do
            yield  {Suit = Suit.FromInt32 x; Number = Number.FromInt32 y } |]

let deckSet = deck |> Set.ofSeq

// generate test data
//for i in 0 .. deck.Length - 1 do
//    printfn "let %s = deck.[%i]" (deck.[i].ToShortString()) i

let HA = deck.[0]
let H2 = deck.[1]
let H3 = deck.[2]
let H4 = deck.[3]
let H5 = deck.[4]
let H6 = deck.[5]
let H7 = deck.[6]
let H8 = deck.[7]
let H9 = deck.[8]
let H10 = deck.[9]
let HJ = deck.[10]
let HQ = deck.[11]
let HK = deck.[12]
let DA = deck.[13]
let D2 = deck.[14]
let D3 = deck.[15]
let D4 = deck.[16]
let D5 = deck.[17]
let D6 = deck.[18]
let D7 = deck.[19]
let D8 = deck.[20]
let D9 = deck.[21]
let D10 = deck.[22]
let DJ = deck.[23]
let DQ = deck.[24]
let DK = deck.[25]
let CA = deck.[26]
let C2 = deck.[27]
let C3 = deck.[28]
let C4 = deck.[29]
let C5 = deck.[30]
let C6 = deck.[31]
let C7 = deck.[32]
let C8 = deck.[33]
let C9 = deck.[34]
let C10 = deck.[35]
let CJ = deck.[36]
let CQ = deck.[37]
let CK = deck.[38]
let SA = deck.[39]
let S2 = deck.[40]
let S3 = deck.[41]
let S4 = deck.[42]
let S5 = deck.[43]
let S6 = deck.[44]
let S7 = deck.[45]
let S8 = deck.[46]
let S9 = deck.[47]
let S10 = deck.[48]
let SJ = deck.[49]
let SQ = deck.[50]
let SK = deck.[51]

// 5 running in flush
nameHand [DA; D2; D3; D4; D5; S3; C4]
nameHand [D10; DJ; DQ; DK; DA; SK; D2]
nameHand [D2; D3; D4; D5; D6; SK; SQ]

// four of a kind
nameHand [DA; HA; CA; SA; D6; SK; SQ]

// full house
nameHand [DA; HA; CA; SK; DK; SK; SQ]
nameHand [DA; HA; CA; SK; DK; SQ; SQ]
nameHand [DA; HA; CA; SK; DK; SJ; S2]

// flush
nameHand [DA; D3; D5; D7; D9; DJ; DQ]

// five run
nameHand [DA; H2; S3; C4; D5; H6; S7]

// three of a kind
nameHand [DA; HA; SA; C4; D6; H8; S10]

// two pair
nameHand [DA; HA; SK; CK; D6; H8; S10]

// pair
nameHand [DA; HA; SK; C4; D6; H8; S10]

// TODO high
