open Util

let cycle s =
    seq {
        while true do
            yield! s
    }

let getInput () =
    let directions, connections = inputFile () |> readRecords |> ArrayOps.asPair
    let connections =
        connections
        |> parseWords
        |> ArrayOps.triples
        |> Array.map (fun (orig, l, r) -> (orig, (l, r)))
        |> Map.ofArray
    (directions, connections)

let findPath directions connections is_endpos start =
    let traverse pos dir =
        let l, r = Map.find pos connections
        if dir = 'L' then l else r
    cycle directions |> Seq.scan traverse start |> Seq.findIndex is_endpos |> int64

let part1 () =
    let directions, connections = getInput ()
    findPath directions connections ((=) "ZZZ") "AAA"

let part2 () =
    let directions, connections = getInput ()
    let places = connections.Keys |> Array.ofSeq
    let mutable positions =
        Array.filter (fun (place: string) -> place[2] = 'A') places
    let pathLengths =
        Array.map
            (findPath directions connections (fun pos -> pos[2] = 'Z'))
            positions
    Array.reduce lcm64 pathLengths

show [ part1; part2 ]
