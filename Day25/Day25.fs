open Util

let makeEdges (vertices: string array) =
    let v1 = vertices[0]
    Array.map (fun v2 -> struct (v1, v2)) vertices[1..]

let part1 () =
    let input = inputFile () |> readLines |> Array.map parseWords
    let edges = Array.map makeEdges input |> Array.concat
    let rng = System.Random()
    let karger (edges: struct (string * string) array) =
        // Karger's algorithm
        let vertexNumbering =
            Array.map (fun struct (v1, v2) -> [| v1; v2 |]) edges
            |> Array.concat
            |> Set.ofArray
            |> Array.ofSeq
            |> Array.indexed
            |> Array.map (fun (i, x) -> (x, i))
            |> makeMutableMap
        let mutable edges =
            Array.map
                (fun struct (v1, v2) ->
                    struct (vertexNumbering[v1], vertexNumbering[v2]))
                edges
        let mutable numVertices = vertexNumbering.Count
        let sizes = MutableList<int>()
        for i in 1..numVertices do
            sizes.Add 1
        while numVertices > 2 do
            let e = rng.Next(Array.length edges)
            let struct (va, vb) = edges[e]
            let vnew = sizes.Count
            sizes.Add (sizes[va] + sizes[vb])
            edges <-
                Array.map
                    (fun struct (v1, v2) ->
                        if v1 = va || v1 = vb then
                            struct (vnew, v2)
                        else
                            struct (v1, v2))
                    edges
                |> Array.map (fun struct (v1, v2) ->
                    if v2 = va || v2 = vb then
                        struct (v1, vnew)
                    else
                        struct (v1, v2))
                |> Array.filter (fun struct (v1', v2') -> v1' <> v2')
            sizes[va] <- 0
            sizes[vb] <- 0
            numVertices <- numVertices - 1
        let a, b =
            Seq.filter ((<>) 0) sizes
            |> Array.ofSeq
            |> ArrayOps.asPair
        (Array.length edges, a, b)

    seq {
        while true do
            let n, a, b = karger edges
            if n = 3 && a <> 1 && b <> 1 then
                yield a * b
    }
    |> Seq.head

show [ part1 ]
