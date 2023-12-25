open Util

#nowarn "25" // suppress incomplete pattern matches warnings

type Cubeset = { red: int; green: int; blue: int }
type Game = { id: int; cubesets: Cubeset array }

let parseCubeset (str: string) =
    let words = parseWords str
    let mutable result = { red = 0; green = 0; blue = 0 }
    for [| countStr; color |] in Array.chunkBySize 2 words do
        let count = int countStr
        result <-
            match color with
            | "green" -> { result with green = count }
            | "red" -> { result with red = count }
            | "blue" -> { result with blue = count }
    result

let parseGame (str: string) =
    let [| gamespec; rest |] = str.Split(':')
    let gameId = (parseNumbers gamespec)[0]
    let cubesetspecs = rest.Split(';')
    {
        id = int gameId
        cubesets = Array.map parseCubeset cubesetspecs
    }

let part1 () =
    let games = inputFile () |> readLines |> Array.map parseGame
    let checkGame game =
        Array.forall
            (fun set -> set.red <= 12 && set.green <= 13 && set.blue <= 14)
            game.cubesets
    Array.filter checkGame games
    |> Array.map (_.id)
    |> Array.sum

let part2 () =
    let games = inputFile () |> readLines |> Array.map parseGame
    let findBounds game =
        Array.reduce
            (fun a b ->
                {
                    red = max a.red b.red
                    green = max a.green b.green
                    blue = max a.blue b.blue
                })
            game.cubesets
    games
    |> Array.map findBounds
    |> Array.map (fun set -> set.red * set.green * set.blue)
    |> Array.sum

show [ part1; part2 ]
