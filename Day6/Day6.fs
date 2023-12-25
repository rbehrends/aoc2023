open System.Text.RegularExpressions
open Util

let countBruteForce (time, distance) =
    seq {
        for i in 1L .. time - 1L do
            yield i * (time - i)
    }
    |> Seq.filter ((<) distance)
    |> Seq.length

let countAnalytically (time, distance) =
    // solve: x * (time - x) > distance <=> x * x - time * x + distance < 0
    let p = -double time
    let q = double distance
    // Normally, we'd use floor x2 - ceil x1 + 1 here, but because of
    // floating point rounding either bound can be off by one for perfect
    // squares, so we start with conservative approximations and then adjust
    // them if needed.
    let x1 = -p / 2.0 - sqrt (p * p / 4.0 - q) |> floor |> int64
    let x2 = -p / 2.0 + sqrt (p * p / 4.0 - q) |> ceil |> int64
    let x1 = if x1 * (time - x1) <= distance then x1 + 1L else x1
    let x2 = if x2 * (time - x2) <= distance then x2 - 1L else x2
    x2 - x1 + 1L

let part1 () =
    let pairs =
        inputFile ()
        |> readLines
        |> Array.map parseNumbers
        |> ArrayOps.asPair
        |> fun (times, distances) -> Array.zip times distances
    Array.map countAnalytically pairs |> Array.reduce (*)

let part2 () =
    inputFile ()
    |> readLines
    |> Array.map (fun s -> Regex.Replace(s, @"[^0-9]", ""))
    |> Array.map int64
    |> ArrayOps.asPair
    |> countAnalytically

show [ part1; part2 ]
