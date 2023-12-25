open Util

let Good = '.'
let Damaged = '#'

type Springs(row: string, groups: int array) =
    member this.skipGood(start: int) =
        seq {
            for i in start .. row.Length - 1 do
                if row[i] <> Good then
                    yield i
            yield row.Length
        }
        |> Seq.head

    member this.anyDamagedLeft(start: int) =
        seq { start .. row.Length - 1 }
        |> Seq.exists (fun i -> row[i] = Damaged)

    member this.possibleSegments(start: int, len: int) =
        seq {
            for i in start .. row.Length - len do
                if
                    seq { i .. i + len - 1 }
                    |> Seq.forall (fun j -> row[j] <> Good)
                    && (i + len = row.Length || row[i + len] <> Damaged)
                then
                    yield Some(i + len + 1)
                if row[i] = Damaged then
                    yield None
        }
        |> Seq.takeWhile Option.isSome
        |> Seq.map Option.get

    member this.count() =
        let rec recursiveCount (start: int, group: int) =
            if group = groups.Length then
                if this.anyDamagedLeft start then 0L else 1L
            else
                let start = this.skipGood start
                this.possibleSegments (start, groups[group])
                |> Seq.map (fun start' -> recursiveCount (start', group + 1))
                |> Seq.sum
        recursiveCount (0, 0)

    member this.countCached() =
        let cache = Array2D.create (row.Length + 2) (groups.Length + 1) -1L

        let rec recursiveCount (start: int, group: int) =
            let cachedResult = cache[start, group]
            if cachedResult >= 0L then
                cachedResult
            else
                let cachedResult =
                    if group = groups.Length then
                        if this.anyDamagedLeft start then 0L else 1L
                    else
                        let start = this.skipGood start
                        this.possibleSegments (start, groups[group])
                        |> Seq.map (fun start' ->
                            recursiveCount (start', group + 1))
                        |> Seq.sum
                cache[start, group] <- cachedResult
                cachedResult
        recursiveCount (0, 0)

let part1 () =
    let lines = inputFile () |> readLines
    let parseLine (line: string) =
        let row, groups = line.Split(' ') |> ArrayOps.asPair
        let groups = parseNumbers groups |> Array.map int
        Springs(row, groups)
    let springs = Array.map parseLine lines
    Array.map (fun (s: Springs) -> s.count ()) springs |> Array.sum

let part2 () =
    let lines = inputFile () |> readLines
    let parseLine (line: string) =
        let row, groups = line.Split(' ') |> ArrayOps.asPair
        let row = Array.replicate 5 row |> String.concat "?"
        let groups =
            parseNumbers groups
            |> Array.map int
            |> Array.replicate 5
            |> Array.concat
        Springs(row, groups)
    let springs = Array.map parseLine lines
    Array.map (fun (s: Springs) -> s.countCached ()) springs |> Array.sum

show [ part1; part2 ]
