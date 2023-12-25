open System.Collections.Generic
open Util

[<Struct>]
type Brick = { x0: int; y0: int; z0: int; x1: int; y1: int; z1: int }

let parseBrick line =
    parseNumbers line
    |> Array.map int
    |> Array.chunkBySize 3
    |> Array.map ArrayOps.asTriple
    |> ArrayOps.asPair
    |> fun ((x0, y0, z0), (x1, y1, z1)) ->
        { x0 = x0; y0 = y0; z0 = z0; x1 = x1; y1 = y1; z1 = z1 }

let overlapRange a b a' b' =
    let inRange lo hi el = el >= lo && el <= hi
    inRange a b a' || inRange a b b' || inRange a' b' a || inRange a' b' b

let overlap b1 b2 =
    overlapRange b1.x0 b1.x1 b2.x0 b2.x1
    && overlapRange b1.y0 b1.y1 b2.y0 b2.y1

let findStructure bricks =
    let bricks = Array.sortBy (_.z0) bricks
    let stack = MutableList<Brick>()
    let minZ = Seq.map (_.z0) bricks |> Seq.min
    for brick in bricks do
        let overlapping =
            Seq.filter (fun b -> overlap brick b && brick.z0 > b.z1) stack
            |> Seq.map (_.z1)
        let maxZ =
            if Seq.isEmpty overlapping then minZ - 1 else Seq.max overlapping
        let brick' =
            { brick with z0 = maxZ + 1; z1 = brick.z1 - (brick.z0 - maxZ) + 1 }
        stack.Add brick'
    let stack = Array.ofSeq stack
    let topMap = Array.groupBy (_.z1) stack |> makeMutableMap
    let supports =
        Seq.map (fun brick -> (brick, MutableList())) stack |> makeMutableMap
    let supportedBy = MutableMap<Brick, List<Brick>>()
    for brick in stack do
        let below =
            topMap.GetValueOrDefault((brick.z0 - 1), [||])
            |> Array.filter (overlap brick)
        for brick' in below do
            supports[brick'].Add brick
        supportedBy[brick] <- MutableList(below)
    stack, supports, supportedBy

let part1 () =
    let bricks = inputFile () |> readLines |> Array.map parseBrick
    let stack, _, supportedBy = findStructure bricks
    let essential =
        Seq.map (fun brick -> supportedBy[brick]) stack
        |> Seq.filter (fun below -> below.Count = 1)
        |> Seq.concat
        |> Seq.distinct
    stack.Length - Seq.length essential

let part2 () =
    let bricks = inputFile () |> readLines |> Array.map parseBrick
    let stack, supports, supportedBy = findStructure bricks
    let disintegrate brick =
        let indegree =
            supportedBy
            |> Seq.map (fun item -> (item.Key, item.Value.Count))
            |> makeMutableMap
        let queue = Queue([ brick ])
        let mutable count = 0
        while queue.Count > 0 do
            let brick = queue.Dequeue()
            for brick' in supports[brick] do
                indegree[brick'] <- indegree[brick'] - 1
                if indegree[brick'] = 0 then
                    count <- count + 1
                    queue.Enqueue brick'
        count
    Array.map disintegrate stack |> Array.sum

show [ part1; part2 ]
