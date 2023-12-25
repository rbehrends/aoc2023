open Util
open Grid

let part1 () =
    let grid = inputFile () |> readLines |> Grid.fromStrings id
    let endRow = grid.rows - 1
    let endCol = grid.columns - 2
    assert (grid[endRow, endCol] = '.')
    let rec dfs seen row col distance =
        if Set.contains (row, col) seen then
            0
        else if row = endRow && col = endCol then
            distance
        else
            let seen = Set.add (row, col) seen
            grid.directAdjacent row col
            |> Seq.map (fun (row', col', tile) ->
                let recurse () = dfs seen row' col' (distance + 1)
                match tile with
                | '.' -> recurse ()
                | '>' -> if col' > col then recurse () else 0
                | '<' -> if col' < col then recurse () else 0
                | 'v' -> if row' > row then recurse () else 0
                | '^' -> if row' < row then recurse () else 0
                | _ -> 0)
            |> Seq.max
    dfs Set.empty 0 1 0

let part2 () =
    let grid = inputFile () |> readLines |> Grid.fromStrings id
    let endRow = grid.rows - 1
    let endCol = grid.columns - 2
    let graph =
        MutableMap<struct (int * int), MutableMap<struct (int * int), int>>()
    let connect v1 v2 =
        if not (graph.ContainsKey v1) then
            graph[v1] <- MutableMap()
        if not (graph.ContainsKey v2) then
            graph[v2] <- MutableMap()
        graph[v1][v2] <- 1
        graph[v2][v1] <- 1
    // build a condensed graph
    for row, col, tile in grid.each do
        if tile <> '#' then
            for row', col', tile' in grid.directAdjacent row col do
                if tile' <> '#' then
                    connect (row, col) (row', col')
    let mutable changed = true
    while changed do
        changed <- false
        for v in graph.Keys |> Array.ofSeq do
            if graph[v].Count = 2 then
                let (va, da), (vb, db) =
                    graph[v] |> mapToSeq |> Array.ofSeq |> ArrayOps.asPair
                graph.Remove(v) |> ignore
                graph[va].Remove(v) |> ignore
                graph[vb].Remove(v) |> ignore
                graph[va][vb] <- da + db
                graph[vb][va] <- da + db
                changed <- true
    let seen = Grid<bool>.make grid.rows grid.columns false
    let rec dfs row col distance =
        if seen[row, col] then
            0
        else if row = endRow && col = endCol then
            distance
        else
            seen[row, col] <- true
            let d' =
                seq {
                    for edge in graph[(row, col)] do
                        let struct (row', col') = edge.Key
                        let d = edge.Value
                        yield dfs row' col' (distance + d)
                }
                |> Seq.max
            seen[row, col] <- false
            d'
    dfs 0 1 0


show [ part1; part2 ]
