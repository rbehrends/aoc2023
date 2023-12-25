open Util
open FParsec

#nowarn "25" // suppress incomplete pattern matches warnings

type Cubeset = { red: int; green: int; blue: int }
type Game = { id: int; cubesets: Cubeset array }

let cubesetMax a b =
    {
        red = max a.red b.red
        green = max a.green b.green
        blue = max a.blue b.blue
    }
let cubeset0 = { red = 0; green = 0; blue = 0 }

let parseCubes = choice [
    pint32 .>>? pstring " red" |>> (fun r -> { cubeset0 with red = r })
    pint32 .>>? pstring " green" |>> (fun g -> { cubeset0 with green = g })
    pint32 .>>? pstring " blue" |>> (fun b -> { cubeset0 with blue = b })
]
let parseCubeset = sepBy parseCubes (pstring ", ") |>> (List.reduce cubesetMax)
let parseCubesets = sepBy parseCubeset (pstring "; ") |>> List.toArray
let parseGameSpec =
    pstring "Game " >>. pint32 .>>. (pstring ": " >>. parseCubesets)
    |>> fun (a, b) -> { id = a; cubesets = b }
let parseGame str =
    match run parseGameSpec str with Success (result, _, _) -> result

let part1 () =
    let games = inputFile () |> readLines |> Array.map parseGame
    let checkGame game =
        Array.forall
            (fun set -> set.red <= 12 && set.green <= 13 && set.blue <= 14)
            game.cubesets
    Array.filter checkGame games
    |> Array.map (fun game -> game.id)
    |> Array.sum

let part2 () =
    let games = inputFile () |> readLines |> Array.map parseGame
    let findMin game =
        Array.reduce cubesetMax game.cubesets
    games
    |> Array.map findMin
    |> Array.map (fun set -> set.red * set.green * set.blue)
    |> Array.sum

show [ part1; part2 ]
