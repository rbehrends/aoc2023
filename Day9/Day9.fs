open Util

let part1 () =
    let inputs = inputFile () |> readLines |> Array.map parseSignedNumbers
    let rec extrapolate line =
        if Array.forall ((=) 0L) line then
            Array.append line [|0L|]
        else
            let diffs = Array.pairwise line |> Array.map (fun (a, b) -> b - a)
            extrapolate diffs |> Array.scan (+) line[0]
    Array.map extrapolate inputs |> Array.map Array.last |> Array.sum

let part2 () =
    let inputs = inputFile () |> readLines |> Array.map parseSignedNumbers
    let rec extrapolate line =
        if Array.forall ((=) 0L) line then
            Array.append [|0L|] line
        else
            let diffs = Array.pairwise line |> Array.map (fun (a, b) -> b - a)
            Array.scanBack (fun a b -> b - a) (extrapolate diffs) (Array.last line)
    Array.map extrapolate inputs |> Array.map Array.head |> Array.sum

show [part1; part2]
