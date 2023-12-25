open Util
open Grid

let solve factor =
    let grid = inputFile () |> readLines |> Grid.fromStrings ((=) '#')
    let coords =
        grid.each
        |> Seq.choose (fun (row, col, galaxy) ->
            if galaxy then Some(row, col) else None)
        |> Array.ofSeq
    let emptyRows =
        [| 0 .. grid.rows - 1 |]
        |> Array.map (fun row ->
            grid.eachInRow row |> Seq.forall (fun (_, g) -> not g))
    let emptyCols =
        [| 0 .. grid.columns - 1 |]
        |> Array.map (fun column ->
            grid.eachInColumn column |> Seq.forall (fun (_, g) -> not g))
    let pathLength (row, col) (row', col') =
        let steps from until (expanded: bool array) =
            let dir = if from > until then -1 else 1
            seq { from + dir .. dir .. until }
            |> Seq.map (fun pos -> if expanded[pos] then factor else 1L)
            |> Seq.sum
        steps row row' emptyRows + steps col col' emptyCols
    seq {
        for i in 1 .. coords.Length - 1 do
            for j in 0 .. i - 1 do
                yield pathLength coords[i] coords[j]
    }
    |> Seq.sum

let part1 () = solve 2L
let part2 () = solve 1_000_000L

show [ part1; part2 ]
