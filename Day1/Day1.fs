open Util

let extractValues (line: string) =
    let digits =
        Seq.toArray line
        |> Array.filter (fun ch -> ch >= '0' && ch <= '9')
        |> Array.map (fun ch -> int (ch - '0'))
    digits[0] * 10 + Array.last digits

let namedDigits = [|
       ("one", 1)
       ("two", 2)
       ("three", 3)
       ("four", 4)
       ("five", 5)
       ("six", 6)
       ("seven", 7)
       ("eight", 8)
       ("nine", 9)
       yield! seq { for i in 0..9 do yield (string i, i) }
    |]

let extractNamedValues (line: string) =
    let digits =
        seq {
            for i in 0 .. line.Length - 1 do
                for digitName, digitValue in namedDigits do
                    if line[i .. i + digitName.Length - 1] = digitName then
                        yield digitValue
        } |> Seq.toArray
    digits[0] * 10 + Array.last digits

let part1 () =
    let lines = inputFile () |> readLines
    lines |> Array.map extractValues |> Array.sum

let part2 () =
    let lines = inputFile () |> readLines
    lines |> Array.map extractNamedValues |> Array.sum

show [ part1; part2 ]
