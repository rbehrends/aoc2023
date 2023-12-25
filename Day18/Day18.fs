open System
open Util

let move (row, col) (dir, steps) =
    match dir with
    | 'R' -> (row, col + steps)
    | 'L' -> (row, col - steps)
    | 'U' -> (row - steps, col)
    | 'D' -> (row + steps, col)
    | _ -> failwith "invalid direction"

let shoelace points =
    let points = Array.append points points[0..0]
    let area =
        Seq.pairwise points
        |> Seq.map (fun ((row, col), (row', col')) -> row * col' - col * row')
        |> Seq.sum
    let path =
        Seq.pairwise points
        |> Seq.map (fun ((row, col), (row', col')) ->
            abs (row - row') + abs (col - col'))
        |> Seq.sum
    (abs area + path) / 2L + 1L

let positions input = Array.scan move (0L, 0L) input

let part1 () =
    let input =
        inputFile ()
        |> readLines
        |> Array.map (
            parseWords
            >> ArrayOps.asTriple
            >> fun (dir, steps, _) -> (dir[0], int64 steps)
        )
    positions input |> shoelace

let part2 () =
    let input =
        inputFile ()
        |> readLines
        |> Array.map (
            parseWords
            >> ArrayOps.asTriple
            >> fun (_, _, hex) ->
                ("RDLU"[int hex[5..5]], Convert.ToInt64(hex[0..4], 16))
        )
    positions input |> shoelace

show [ part1; part2 ]
