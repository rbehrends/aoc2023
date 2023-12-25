open Util

type RangeMap(outStart: int64, inStart: int64, length: int64) =
    let inEnd = inStart + length - 1L
    member this.contains(value: int64) = value >= inStart && value <= inEnd
    member this.apply(value: int64) =
        if this.contains value then
            outStart + (value - inStart)
        else
            value
    member this.apply((rangeStart, rangeEnd): int64 * int64) =
        (this.apply rangeStart, this.apply rangeEnd)
    member this.intersect((rangeStart, rangeEnd): int64 * int64) =
        let resultStart = max rangeStart inStart
        let resultEnd = min rangeEnd inEnd
        if resultStart <= resultEnd then
            Some(resultStart, resultEnd)
        else
            None
    member this.difference((rangeStart, rangeEnd): int64 * int64) =
        seq {
            if rangeEnd < inStart then
                yield (rangeStart, rangeEnd)
            else if rangeStart > inEnd then
                yield (rangeStart, rangeEnd)
            else
                if rangeStart < inStart then
                    yield (rangeStart, inStart - 1L)
                if rangeEnd > inEnd then
                    yield (inEnd + 1L, rangeEnd)
        }
        |> Array.ofSeq

let part1 () =
    let records = inputFile () |> readRecords
    let mutable seeds = parseNumbers (Seq.head records)
    for mapSpecs in Seq.tail records do
        let maps =
            parseNumbers mapSpecs
            |> ArrayOps.triples
            |> Array.map (fun (outStart, inStart, length) ->
                RangeMap(outStart, inStart, length))
        seeds <-
            Array.map
                (fun seed ->
                    match
                        Array.tryFind
                            (fun (map: RangeMap) -> map.contains seed)
                            maps
                    with
                    | Some map -> map.apply seed
                    | None -> seed)
                seeds
    Array.min seeds

let part2 () =
    let records = inputFile () |> readRecords
    let seedRanges =
        parseNumbers (Seq.head records)
        |> ArrayOps.pairs
        |> Array.map (fun (start, length) -> (start, start + length - 1L))
    let maps =
        Array.tail records
        |> Array.map (
            parseNumbers
            >> ArrayOps.triples
            >> Array.map (fun (outStart, inStart, length) ->
                RangeMap(outStart, inStart, length))
        )
    seq {
        for seedRange in seedRanges do
            let mutable inProgressRanges = Array.singleton seedRange
            let mutable finalRanges = [||]
            for mapSet in maps do
                for map in mapSet do
                    finalRanges <-
                        Array.append
                            finalRanges
                            (Array.choose map.intersect inProgressRanges
                             |> Array.map map.apply)
                    inProgressRanges <-
                        inProgressRanges
                        |> Array.map map.difference
                        |> Array.concat
                inProgressRanges <- Array.append inProgressRanges finalRanges
                finalRanges <- [||]
            yield Array.map fst inProgressRanges |> Array.min
    }
    |> Seq.min

show [ part1; part2 ]
