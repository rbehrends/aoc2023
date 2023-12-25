open Util
open Grid

let slideNorth (grid: Grid<char>) =
    for col in 0 .. grid.columns - 1 do
        let mutable limit = 0
        for row in 0 .. grid.rows - 1 do
            match grid[row, col] with
            | '#' -> limit <- row + 1
            | 'O' ->
                grid[row, col] <- '.'
                grid[limit, col] <- 'O'
                limit <- limit + 1
            | _ -> ()

let calculateLoad (grid: Grid<char>) =
    grid.each
    |> Seq.map (fun (row, _, rock) ->
        if rock = 'O' then (grid.rows - row) else 0)
    |> Seq.sum

let part1 () =
    let grid = inputFile () |> readLines |> Grid.fromStrings id
    slideNorth grid
    calculateLoad grid

let part2 () =
    let numSpins = 1000000000
    let spin (grid: Grid<char>) =
        let mutable grid = grid.clone
        for _ in 1..4 do
            slideNorth grid
            grid <- grid.rotate90
        grid
    let mutable grid = inputFile () |> readLines |> Grid.fromStrings id
    let cycleStart, cycleLength =
        seq {
            let seen = MutableMap<Grid<char>, int>()
            let mutable grid = grid
            for iteration in Seq.initInfinite id do
                if seen.ContainsKey grid then
                    let cycleStart = seen[grid]
                    yield (cycleStart, iteration - cycleStart)
                seen[grid] <- iteration
                grid <- spin grid
        }
        |> Seq.head
    for i in 1..cycleStart do
        grid <- spin grid
    let numSpins = numSpins - cycleStart
    for i in 1 .. numSpins - (numSpins / cycleLength) * cycleLength do
        grid <- spin grid
    calculateLoad grid

show [ part1; part2 ]
