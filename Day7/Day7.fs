open Util

let cardFrequencies (cards: string) = Seq.countBy id cards |> Map.ofSeq

let handType (cards: string) =
    let freqs = cardFrequencies cards |> Map.values |> Seq.sortDescending
    Seq.map string freqs |> String.concat ""

let handType2 (cards: string) =
    let freqMap = cardFrequencies cards
    let jokers = Map.tryFind 'J' freqMap |> Option.defaultValue 0
    let freqMap = Map.remove 'J' freqMap
    let freqList = Map.values freqMap |> Seq.toArray |> Array.sortDescending
    let freqList = if jokers = 5 then [| 0 |] else freqList
    freqList[0] <- freqList[0] + jokers
    Array.map string freqList |> String.concat ""

let cardOrder = "23456789TJQKA"
let cardOrder2 = "J" + String.filter ((<>) 'J') cardOrder

let cardValues order =
    order |> Seq.indexed |> Seq.map (fun (a, b) -> (b, a)) |> Map.ofSeq

let handValue order hand =
    let values = cardValues order
    Seq.map (fun card -> Map.find card values) hand
    |> Seq.fold (fun acc c -> acc * String.length order + c) 0

let part1 () =
    let hands =
        inputFile ()
        |> readFile
        |> parseWords
        |> ArrayOps.pairs
        |> Array.map (fun (cards, value) ->
            (handType cards, handValue cardOrder cards, int value, cards))
        |> Array.sort
    Array.mapi (fun i (_, _, value, _) -> (i + 1) * value) hands |> Array.sum

let part2 () =
    let hands =
        inputFile ()
        |> readFile
        |> parseWords
        |> ArrayOps.pairs
        |> Array.map (fun (cards, value) ->
            (handType2 cards, handValue cardOrder2 cards, int value))
        |> Array.sort
    Array.mapi (fun i (_, _, value) -> (i + 1) * value) hands |> Array.sum

show [ part1; part2 ]
