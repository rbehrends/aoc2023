open Util
open Grid

[<System.Flags; AutoOpen>]
type Direction =
    | Disconnected = 0
    | North = 1
    | South = 2
    | West = 4
    | East = 8

type AreaType =
    | Empty
    | Pipe
    | Outside

let connections =
    function
    | '.' -> Disconnected
    | '|' -> North ||| South
    | '-' -> West ||| East
    | 'L' -> North ||| East
    | 'J' -> North ||| West
    | '7' -> South ||| West
    | 'F' -> South ||| East
    | 'S' -> North ||| South ||| East ||| West
    | ch -> failwith $"invalid pipe component: '{ch}'"

let buildGrid () =
    let lines = inputFile () |> readLines
    let grid =
        Grid.init lines.Length lines[0].Length (fun row col ->
            connections (lines[row][col]))
    for row, col in grid.eachPos do
        if grid[row - 1, col] &&& South = Disconnected then
            grid[row, col] <- grid[row, col] &&& ~~~North
        if grid[row + 1, col] &&& North = Disconnected then
            grid[row, col] <- grid[row, col] &&& ~~~South
        if grid[row, col - 1] &&& East = Disconnected then
            grid[row, col] <- grid[row, col] &&& ~~~West
        if grid[row, col + 1] &&& West = Disconnected then
            grid[row, col] <- grid[row, col] &&& ~~~East
    let start =
        grid.eachPos
        |> Seq.choose (fun (row, col) ->
            if lines[row][col] = 'S' then Some(row, col) else None)
        |> Seq.exactlyOne
    (start, grid)

let pipeLinks start (grid: Grid<Direction>) =
    let nextLink (row, col) =
        seq {
            let conn = grid[row, col]
            if conn &&& North = North then
                yield (row - 1, col)
            if conn &&& South = South then
                yield (row + 1, col)
            if conn &&& West = West then
                yield (row, col - 1)
            if conn &&& East = East then
                yield (row, col + 1)
        }
    seq {
        let mutable prev = start
        let mutable pos = nextLink prev |> Seq.head
        yield start
        while pos <> start do
            yield pos
            let pos' = nextLink pos |> Seq.filter ((<>) prev) |> Seq.exactlyOne
            prev <- pos
            pos <- pos'
    }

let part1 () =
    let start, grid = buildGrid ()
    Seq.length (pipeLinks start grid) / 2

let part2 () =
    let start, grid = buildGrid ()
    let pipe = Seq.append (pipeLinks start grid) (Seq.singleton start)
    // Expand the grid so that flood fills go in between directly adjacent pipe
    // segments. This also ensures that all squares on the outside border are
    // empty.
    let outGrid =
        Grid<AreaType>.make (2 * grid.rows + 1) (2 * grid.columns + 1) Empty
    for (row, col), (row', col') in pipe |> Seq.pairwise do
        outGrid[2 * row + 1, 2 * col + 1] <- Pipe
        outGrid[row + row' + 1, col + col' + 1] <- Pipe
        outGrid[2 * row' + 1, 2 * col' + 1] <- Pipe
    let rec floodFill row col =
        if outGrid[row, col] = Empty then
            outGrid[row, col] <- Outside
            for row', col' in outGrid.directAdjacentPos row col do
                floodFill row' col'
    // Now flood fill everything starting from the border. Because the border
    // consists of empty squares, it suffices to start from just one square.
    floodFill 0 0
    // And collapse the grid again to its original size.
    let outGrid =
        Grid<AreaType>.init grid.rows grid.columns (fun row col ->
            outGrid[2 * row + 1, 2 * col + 1])
    // The remaining empty squares are the ones inside the pipe.
    outGrid.each |> Seq.filter (fun (_, _, at) -> at = Empty) |> Seq.length

show [ part1; part2 ]
