open System.Collections.Generic
open Util
open Grid

let part1 () =
    let grid = inputFile () |> readLines |> Grid.fromStrings id
    let start =
        Seq.filter (fun (_, _, tile) -> tile = 'S') grid.each
        |> Seq.exactlyOne
        |> (fun (row, col, _) -> (row, col))
    grid[fst start, snd start] <- '.'
    let mutable places = MutableSet([ start ])
    for _ in 1..64 do
        places <-
            seq {
                for row, col in places do
                    for row', col' in grid.directAdjacentPos row col do
                        if grid[row', col'] = '.' then
                            yield (row', col')
            }
            |> MutableSet
    int64 places.Count

let part2 () =
    let steps = 26501365L
    // We exploit the fact that 26501365 ≡ 65 (mod 131) where 131 is the
    // number of rows and lines in the grid and 131 = 2 * 65 + 1, the start
    // symbol is in the center, and the center row and column as well as the
    // border are free. This leads to a repeating pattern contained in an
    // approximately diamond shape.
    let grid = inputFile () |> readLines |> Grid.fromStrings id
    assert (grid.rows = grid.columns)
    let baseSize = grid.rows
    let start =
        Seq.filter (fun (_, _, tile) -> tile = 'S') grid.each
        |> Seq.exactlyOne
        |> (fun (row, col, _) -> (row, col))
    grid[fst start, snd start] <- '.'
    let rep = 2
    // Find plot counts for x = 131 * i + 65 for i in 0..2
    let grid = grid.repeat (2 * rep + 1) (2 * rep + 1)
    let start = struct (fst start + baseSize * rep, snd start + baseSize * rep)
    let mutable places = MutableSet([ start ])
    let plotCounts = List<int64>()
    for counter in 1 .. baseSize * rep + baseSize / 2 do
        places <-
            seq {
                for row, col in places do
                    for row', col' in grid.directAdjacentPos row col do
                        if grid[row', col'] = '.' then
                            yield struct (row', col')
            }
            |> MutableSet
        if counter % baseSize = baseSize / 2 then
            plotCounts.Add places.Count
    let y0, y1, y2 = Array.ofSeq plotCounts |> ArrayOps.asTriple
    // We can use a quadratic equation to describe size progression. We
    // determine the coefficients via a system of linear equations.
    // Solve:
    //   a * 0^2 + b * 0 + c = y0
    //   a * 1^2 + b * 1 + c = y1
    //   a * 2^2 + b * 2 + c = y2
    let a = (y0 - 2L * y1 + y2) / 2L
    let b = (-3L * y0 + 4L * y1 - y2) / 2L
    let c = y0
    let baseSize = int64 baseSize
    let x = (steps - baseSize / 2L) / baseSize
    assert (x * baseSize + baseSize / 2L = steps)
    a * x * x + b * x + c

show [ part1; part2 ]
