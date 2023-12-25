open Util
open Grid

type Direction =
    | North
    | South
    | East
    | West

let grid = inputFile () |> readLines |> Grid.fromStrings id

let step row col dir =
    match dir, grid[row, col] with
    | North, '\\' -> [ (row, col - 1), West ]
    | South, '\\' -> [ (row, col + 1), East ]
    | East, '\\' -> [ (row + 1, col), South ]
    | West, '\\' -> [ (row - 1, col), North ]
    | North, '/' -> [ (row, col + 1), East ]
    | South, '/' -> [ (row, col - 1), West ]
    | East, '/' -> [ (row - 1, col), North ]
    | West, '/' -> [ (row + 1, col), South ]
    | North, '-'
    | South, '-' -> [ (row, col - 1), West; (row, col + 1), East ]
    | East, '|'
    | West, '|' -> [ (row - 1, col), North; (row + 1, col), South ]
    | North, _ -> [ (row - 1, col), North ]
    | South, _ -> [ (row + 1, col), South ]
    | East, _ -> [ (row, col + 1), East ]
    | West, _ -> [ (row, col - 1), West ]

let scanBeam pos dir =
    let mutable worklist = [ pos, dir ]
    let seen = MutableSet<(int * int) * Direction>()
    while worklist <> [] do
        worklist <-
            List.map
                (fun (pos, dir) ->
                    let row, col = pos
                    if grid.inside row col && seen.Add(pos, dir) then
                        step row col dir
                    else
                        [])
                worklist
            |> List.concat
    Seq.map fst seen |> MutableSet |> _.Count

let part1 () = scanBeam (0, 0) East

let part2 () =
    seq {
        for row in 0..grid.rows - 1 do
            yield scanBeam (row, 0) East
            yield scanBeam (row, (grid.columns - 1)) West
        for col in 0..grid.columns do
            yield scanBeam (0, col) South
            yield scanBeam ((grid.rows - 1), col) North
    }
    |> Seq.max

show [ part1; part2 ]
