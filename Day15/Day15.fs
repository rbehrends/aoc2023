open Util

let HASH s =
    Seq.fold (fun acc ch -> (acc + int ch) * 17 % 256) 0 s

let part1 () =
    let fields = inputFile () |> readFile |> _.Split(',')
    Array.map HASH fields |> Array.sum

let part2 () =
    let fields = inputFile () |> readFile |> _.Split(',')
    let boxes: (string * int) array array = Array.create 256 [||]
    for field in fields do
        if field.EndsWith("-") then
            let label = field.TrimEnd('-')
            let h = HASH label
            boxes[h] <- Array.filter (fun (lbl, _) -> lbl <> label) boxes[h]
        else
            let name, lens = field.Split("=") |> ArrayOps.asPair
            let lens = int lens[0] - int '0'
            let h = HASH name
            if Array.exists (fun (lbl, _) -> lbl = name) boxes[h] then
                boxes[h] <-
                    Array.map
                        (fun (lbl, oldLens) ->
                            if lbl = name then (lbl, lens) else (lbl, oldLens))
                        boxes[h]
            else
                boxes[h] <- Array.append boxes[h] [| (name, lens) |]
    let focusingPower boxIndex box =
        Array.mapi (fun i (_, lens) -> (i + 1) * (boxIndex + 1) * lens) box
        |> Array.sum
    Array.mapi focusingPower boxes |> Array.sum

show [ part1; part2 ]
