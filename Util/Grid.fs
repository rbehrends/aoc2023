namespace Grid

type VirtualGrid(rows: int, cols: int) =

    member this.rows: int = rows
    member this.columns: int = cols

    member this.eachPos: seq<int * int> =
        seq {
            for row in 0 .. rows - 1 do
                for col in 0 .. cols - 1 do
                    yield row, col
        }

    member this.adjacentPos (row: int) (col: int) : seq<int * int> =
        seq {
            for r in -1 .. +1 do
                let row' = row + r
                if row' >= 0 && row' < rows then
                    for c in -1 .. +1 do
                        let col' = col + c
                        if col' >= 0 && col' < cols then
                            yield row', col'
        }

    member this.directAdjacentPos (row: int) (col: int) : seq<int * int> =
        seq {
            if row > 0 then
                yield row - 1, col
            if row < rows - 1 then
                yield row + 1, col
            if col > 0 then
                yield row, col - 1
            if col < cols - 1 then
                yield row, col + 1
        }

type Grid<'a when 'a : comparison>(rows: int, cols: int, default_value: 'a) =
    inherit VirtualGrid(rows, cols)

    let grid = Array2D.create rows cols default_value

    member private this.grid = grid

    override this.GetHashCode (): int =
        hash grid

    override this.Equals (other: obj): bool =
        let other = other :?> Grid<'a>
        compare grid other.grid = 0

    member this.Rows: int = rows
    member this.Columns: int = cols

    static member init rows cols initializer =
        let grid = Grid(rows, cols, Unchecked.defaultof<'a>)
        for row in 0..rows-1 do
            for col in 0..cols-1 do
                grid[row, col] <- initializer row col
        grid

    static member make rows cols default_value =
        Grid(rows, cols, default_value)

    static member fromStrings (f: char -> 'a) (s: string array) =
        Grid.init s.Length s[0].Length (fun row col -> f (s[row][col]))

    member this.clone: Grid<'a> =
        let result = Grid(rows, cols, default_value)
        for row in 0 .. rows - 1 do
            for col in 0 .. cols - 1 do
                result[row, col] <- grid[row, col]
        result

    member this.repeat vertRepeat horizRepeat : Grid<'a> =
        Grid.init (rows * vertRepeat) (cols * horizRepeat) (fun row col ->
            grid[row % rows, col % cols])

    member this.subgrid (row, col) (row', col') =
        Grid.init (row' - row + 1) (col' - col + 1) (fun r c ->
            grid[row + r, col + c])

    member this.transpose: Grid<'a> =
        let result = Grid(cols, rows, default_value)
        for row in 0 .. rows - 1 do
            for col in 0 .. cols - 1 do
                result[col, row] <- grid[row, col]
        result

    member this.rotate90: Grid<'a> =
        Grid<'a>.init this.columns this.rows (fun row col -> grid[cols - col - 1, row])

    member this.inside row col =
        row >= 0 && row < rows && col >= 0 && col < cols

    member this.contains (row, col) = this.inside row col

    member this.Item
        with get (row: int, col: int): 'a =
            if row >= 0 && row < rows && col >= 0 && col < cols then
                grid[row, col]
            else
                default_value
        and set (row: int, col: int) (new_value: 'a) =
            if row >= 0 && row < rows && col >= 0 && col < cols then
                grid[row, col] <- new_value

    member this.each: seq<int * int * 'a> =
        seq {
            for row, col in this.eachPos do
                yield row, col, grid[row, col]
        }

    member this.eachInRow (row: int) : seq<int * 'a> =
        seq {
            for col in 0..this.columns - 1 do
                yield col, grid[row, col]
        }

    member this.eachInColumn (col: int) : seq<int * 'a> =
        seq {
            for row in 0..this.rows - 1 do
                yield col, grid[row, col]
        }

    member this.adjacent(row: int) (col: int) : seq<int * int * 'a> =
        seq {
            for r, c in this.adjacentPos row col do
                yield r, c, grid[r, c]
        }

    member this.adjacentValues (row: int) (col: int) : seq<'a> =
        seq {
            for r, c in this.adjacentPos row col do
                yield grid[r, c]
        }

    member this.directAdjacent (row: int) (col: int) : seq<int * int * 'a> =
        seq {
            for r, c in this.directAdjacentPos row col do
                yield r, c, grid[r, c]
        }

    member this.directAdjacentValues (row: int) (col: int) : seq<'a> =
        seq {
            for r, c in this.directAdjacentPos row col do
                yield grid[r, c]
        }

    member this.show(f: 'a -> string): string =
        let grid' = Array2D.create rows cols null
        for row, col, value in this.each do
            grid'[row, col] <- f(value)
        let make_row row =
            seq { 0..cols-1 } |> Seq.map (fun col -> grid'[row, col]) |> String.concat ""
        seq { 0..rows-1 } |> Seq.map make_row |> String.concat "\n"

