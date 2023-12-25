open Util

let part1 () =
    let lines = inputFile () |> readLines
    let score (line: string) =
        let _, winning, drawn = line.Split(':', '|') |> ArrayOps.asTriple
        let winning = parseNumbers winning |> HashSet
        let drawn = parseNumbers drawn
        let matches = Array.filter winning.Contains drawn
        let len = matches.Length
        if len = 0 then 0 else 1 <<< (len - 1)
    lines |> Array.map score |> Array.sum

let part2 () =
    let lines = inputFile () |> readLines
    let numMatches (line: string) =
        let  _, winning, drawn = line.Split(':', '|') |> ArrayOps.asTriple
        let winning = parseNumbers winning |> HashSet
        let drawn = parseNumbers drawn
        Array.filter winning.Contains drawn |> Array.length
    let matches = Array.map numMatches lines
    let cardCounts = Array.create matches.Length 1
    for i in 0 .. matches.Length - 1 do
        for j in 1 .. matches[i] do
            cardCounts[i + j] <- cardCounts[i + j] + cardCounts[i]
    Array.sum cardCounts

show [ part1; part2 ]
