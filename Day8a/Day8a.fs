open Util

#nowarn "25" // suppress incomplete pattern matches warnings

// This is a more general solution for day 8 that also works if the cycle
// length isn't a multiple of the initial segment. It uses the sieve method
// for the Chinese remainder theorem, because it also works for cases where
// the moduli aren't pairwise coprime.

let cycle s =
    seq {
        while true do
            yield! s
    }

let getInput () =
    let [| directions; connections |] = inputFile () |> readRecords
    let connections =
        connections
        |> parseWords
        |> Array.chunkBySize 3
        |> Array.map (fun [| orig; l; r |] -> (orig, (l, r)))
        |> Map.ofArray
    (directions, connections)

let findPath directions connections is_endpos start =
    let traverse pos dir =
        let l, r = Map.find pos connections
        if dir = 'L' then l else r
    cycle directions
    |> Seq.scan traverse start
    |> Seq.findIndex is_endpos
    |> int64

let part1 () =
    let directions, connections = getInput ()
    findPath directions connections ((=) "ZZZ") "AAA"

let combinePaths (start1, len1) (start2, len2) =
    seq {
        for p in Seq.initInfinite (fun i -> start1 + len1 * int64 (i + 1)) do
            if p % len2 = start2 then
                yield start1 * start2, p
    }
    |> Seq.head

let combinedPathLengths cycles =
    Seq.sortByDescending snd cycles |> Seq.reduce combinePaths

let part2 () =
    let directions, connections = getInput ()
    let places = connections.Keys |> Array.ofSeq
    let mutable positions =
        Array.filter (fun (place: string) -> place[2] = 'A') places
    let find_cycle (start: string) =
        seq {
            let mutable seen = MutableMap() // much faster than Map
            let mutable pos = start
            for index, (dir, offset) in
                Seq.mapi (fun i dir -> (dir, i)) directions
                |> cycle
                |> Seq.indexed do
                if seen.ContainsKey((pos, offset)) then
                    if pos[2] = 'Z' then
                        let oldIndex = seen[(pos, offset)]
                        let cycleLength = index - oldIndex
                        let cycleStart = oldIndex % cycleLength
                        yield (int64 cycleStart, int64 cycleLength)
                else
                    seen[(pos, offset)] <- index
                let l, r = connections[pos]
                pos <- if dir = 'L' then l else r
        }
        |> Seq.head
    let cycles = Array.map find_cycle positions
    combinedPathLengths cycles |> snd

show [ part1; part2 ]
