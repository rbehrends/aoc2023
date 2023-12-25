open System.Collections.Generic
open Util

type Part = MutableMap<char, int>
type Box = Map<char, int * int> // dimension name -> (lo, hi)
type Transition =
    {
        category: char
        threshold: int
        dir: char
        action: string
    }
type Workflow =
    {
        name: string
        transitions: Transition array
        fallback: string
    }

let parseWorkflow line =
    let words = parseWordsAndSeps line
    let statename = words[0]
    let fallback = words[words.Length - 2]
    let transitions =
        words[2 .. words.Length - 3]
        |> Array.chunkBySize 6
        |> Array.map (fun chunk ->
            {
                category = chunk[0][0] // 'x', 'm', 'a', or 's'
                dir = chunk[1][0]      // '<' or '>'
                threshold = int chunk[2]
                // skip chunk[3] == ":"
                action = chunk[4]
                // skip chunk[5] == ","
            })
    let result =
        {
            name = statename
            transitions = transitions
            fallback = fallback
        }
    KeyValuePair(result.name, result)

let parseRatings line =
    parseWords line
    |> ArrayOps.pairs
    |> Array.map (fun (category, value) ->
        KeyValuePair(category[0], int value))
    |> MutableMap

let getInput () =
    let workflowDesc, inputDesc =
        inputFile () |> readRecords |> ArrayOps.asPair
    let workflows =
        workflowDesc.Split('\n') |> Array.map parseWorkflow |> MutableMap
    let parts = inputDesc.Split('\n') |> Array.map parseRatings
    (workflows, parts)

let workflows, parts = getInput ()

let testPart (part: Part) =
    let mutable workflowName = "in"
    while workflowName <> "R" && workflowName <> "A" do
        let workflow = workflows[workflowName]
        workflowName <- seq {
            for transition in workflow.transitions do
                if transition.dir = '<' then
                    if part[transition.category] < transition.threshold then
                        yield transition.action
                else
                    if part[transition.category] > transition.threshold then
                        yield transition.action
        }
        |> Seq.tryHead
        |> function
            | None -> workflow.fallback
            | Some name -> name
    workflowName = "A"

let part1 () =
    let parts = Array.filter testPart parts
    Array.map (fun (part: Part) -> part.Values |> Seq.sum) parts
    |> Array.sum
    |> int64

let splitBox (box: Box) (dim: char) (threshold: int) =
    let lo, hi = Map.find dim box
    let seg1 = Map.add dim (lo, threshold) box
    let seg2 = Map.add dim (threshold + 1, hi) box
    (seg1, seg2)

let boxVolume (box: Box) =
    box.Values
    |> Seq.map (fun (lo, hi) -> if lo > hi then 0L else int64 (hi - lo + 1))
    |> Seq.reduce (*)

let rec processWorkflow (workflowName: string) (box: Box) =
    if workflowName = "R" then
        0L
    else if workflowName = "A" then
        boxVolume box
    else
        let workflow: Workflow = workflows[workflowName]
        let mutable total = 0L
        let mutable box = box
        for transition in workflow.transitions do
            if transition.dir = '<' then
                let lo, hi =
                    splitBox
                        box
                        transition.category
                        (transition.threshold - 1)
                total <- total + processWorkflow transition.action lo
                box <- hi
            else
                let lo, hi =
                    splitBox
                        box
                        transition.category
                        transition.threshold
                total <- total + processWorkflow transition.action hi
                box <- lo
        total + processWorkflow workflow.fallback box

let part2 () =
    let initial = Seq.map (fun ch -> (ch, (1, 4000))) "xmas" |> Map.ofSeq
    processWorkflow "in" initial

show [ part1; part2 ]
