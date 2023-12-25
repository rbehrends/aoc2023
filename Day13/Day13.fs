open Util
open Grid

let grids =
    inputFile ()
    |> readRecords
    |> Array.map (fun lines -> Grid.fromStrings ((=) '#') (lines.Split("\n")))

let findRowReflection smudge (grid: Grid<bool>) =
    let countReflectionDifferences row =
        seq {
            for d in 1 .. min row (grid.rows - row) do
                for col in 0 .. grid.columns - 1 do
                    if grid[row - d, col] <> grid[row + d - 1, col] then
                        yield ()
        }
        |> Seq.length
    seq {
        for row in 1 .. grid.rows - 1 do
            if countReflectionDifferences row = smudge then
                yield row
        yield 0
    }
    |> Seq.head

let findReflection smudge (grid: Grid<bool>) =
    findRowReflection smudge grid * 100 + findRowReflection smudge grid.transpose

let part1 () =
    Array.map (findReflection 0) grids |> Array.sum

let part2 () =
    Array.map (findReflection 1) grids |> Array.sum

show [ part1; part2 ]
