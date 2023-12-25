open Util
open Grid

let segment row firstCol lastCol =
    seq {
        for col in firstCol..lastCol do
            yield (row, col)
    }

let part1 () =
    let lines = inputFile () |> readLines
    let grid =
        Grid.init lines.Length lines[0].Length (fun row col -> lines[row][col])
    let symbolAdj = Grid(lines.Length, lines[0].Length, false)
    for row, col, ch in grid.each do
        if (ch < '0' || ch > '9') && ch <> '.' then
            for r, c, _ in grid.adjacent row col do
                symbolAdj[r, c] <- true
    let partNumbers =
        seq {
            for row in 0 .. lines.Length - 1 do
                for first, last, num in parseNumbersWithPositions lines[row] do
                    let num = int num
                    if
                        Seq.exists
                            (fun (r, c) -> symbolAdj[r, c])
                            (segment row first last)
                    then
                        yield num
        }
    Seq.sum partNumbers

let part2 () =
    let lines = inputFile () |> readLines
    let grid = VirtualGrid(lines.Length, lines[0].Length)

    let gearNumbers = MutableMap<int * int, int list>()
    for row in 0 .. lines.Length - 1 do
        for first, last, num in parseNumbersWithPositions lines[row] do
            let num = int num
            let adjacentGears (r, c) =
                seq {
                    for r', c' in grid.adjacentPos r c do
                        if lines[r'][c'] = '*' then
                            yield (r', c')
                }
                |> Set.ofSeq
            let gears =
                Seq.fold
                    (fun acc pos -> Set.union acc (adjacentGears pos))
                    Set.empty
                    (segment row first last)
            for gear in gears do
                if gearNumbers.ContainsKey gear then
                    gearNumbers[gear] <- num :: gearNumbers[gear]
                else
                    gearNumbers[gear] <- [ num ]

    Seq.choose
        (function
        | [ a; b ] -> Some(a * b)
        | _ -> None)
        gearNumbers.Values
    |> Seq.sum

show [ part1; part2 ]
