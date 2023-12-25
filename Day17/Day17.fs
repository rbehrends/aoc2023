open Util
open Grid

type Direction =
    | North
    | South
    | West
    | East

let opposite =
    function
    | North -> South
    | South -> North
    | West -> East
    | East -> West

let move (row, col) =
    function
    | North -> (row - 1, col)
    | South -> (row + 1, col)
    | West -> (row, col - 1)
    | East -> (row, col + 1)

type State =
    {
        pos: int * int
        dir: Direction
        straightMoves: int
        heatloss: int
    }

let grid =
    inputFile () |> readLines |> Grid.fromStrings (fun x -> int x - int '0')

let searchFor cond next =
    let pq = PriorityQueue<State, int>()
    let initial =
        {
            pos = (0, 0)
            dir = East
            straightMoves = 0
            heatloss = 0
        }
    pq.Enqueue(initial, initial.heatloss)
    pq.Enqueue({ initial with dir = South }, initial.heatloss)
    let seen = MutableSet()
    seq {
        while true do
            let state = pq.Dequeue()
            let row, col = state.pos
            if seen.Add struct (row, col, state.dir, state.straightMoves) then
                yield state
                for nextState in next state grid do
                    pq.Enqueue(nextState, nextState.heatloss)
    }
    |> Seq.find cond
    |> _.heatloss

let next lbMoves ubMoves (state: State) (grid: Grid<int>) =
    seq {
        for dir in [| North; South; West; East |] do
            let nextPos = move state.pos dir
            let nextState =
                {
                    pos = nextPos
                    dir = dir
                    straightMoves = state.straightMoves + 1
                    heatloss = state.heatloss + grid[fst nextPos, snd nextPos]
                }
            if dir = state.dir then
                if grid.contains nextPos && state.straightMoves < ubMoves then
                    yield nextState
            else if dir <> opposite state.dir then
                if grid.contains nextPos && state.straightMoves >= lbMoves then
                    yield { nextState with straightMoves = 1 }
    }

let part1 () =
    let endPos = (grid.rows - 1, grid.columns - 1)
    searchFor (fun state -> state.pos = endPos) (next 0 3)

let part2 () =
    let endPos = (grid.rows - 1, grid.columns - 1)
    searchFor
        (fun state -> state.pos = endPos && state.straightMoves >= 4)
        (next 4 10)

show [ part1; part2 ]
