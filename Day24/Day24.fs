open Util
open Rationals

type Vector(x: Rational, y: Rational, z: Rational) =
    override Vector.ToString() : string = string (x, y, z)
    member this.x = x
    member this.y = y
    member this.z = z
    member this.reduce = (x.CanonicalForm, y.CanonicalForm, z.CanonicalForm)
    static member (+)(this: Vector, other: Vector) =
        Vector(this.x + other.x, this.y + other.y, this.z + other.z)
    static member (*)(this: Vector, c: Rational) =
        Vector(this.x * c, this.y * c, this.z * c)
    static member (-)(this: Vector, other: Vector) =
        Vector(this.x - other.x, this.y - other.y, this.z - other.z)
    static member (~-)(this: Vector) = Vector(-this.x, -this.y, -this.z)

let intersect (x1, y1) (x2, y2) (x3, y3) (x4, y4) =
    let den: Rational = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    if den.IsZero then
        None
    else
        let tnum = (x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)
        let unum = (x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2)
        let t = tnum / den
        let u = unum / den
        if t >= Rational.Zero && u >= Rational.Zero then
            let px = x1 + t * (x2 - x1)
            let py = y1 + t * (y2 - y1)
            Some(px.CanonicalForm, py.CanonicalForm)
        else
            None

let parseXY line =
    let nums = parseSignedNumbers line |> Array.map Rational
    ((nums[0], nums[1]), (nums[0] + nums[3], nums[1] + nums[4]))

let part1 () =
    let lines = inputFile () |> readLines |> Array.map parseXY
    let lo = Rational 200000000000000L
    let hi = Rational 400000000000000L
    seq {
        for i, (p1, p2) in Seq.indexed lines do
            for p3, p4 in lines[i + 1 ..] do
                match intersect p1 p2 p3 p4 with
                | None -> ()
                | Some(x, y) ->
                    if x >= lo && x <= hi && y >= lo && y <= hi then
                        yield (x, y)
    }
    |> Seq.length
    |> string

let parseVectors line =
    let nums = parseSignedNumbers line |> Array.map Rational
    (Vector(nums[0], nums[1], nums[2]), Vector(nums[3], nums[4], nums[5]))

let gauss (matrix: Rational array array) =
    let rows = matrix.Length
    let cols = matrix[0].Length
    for i in 0 .. rows - 1 do
        let mutable j = i + 1
        while (matrix[i][i]).IsZero do
            if not (matrix[j][i]).IsZero then
                for k in i .. cols - 1 do
                    matrix[i][k] <- matrix[i][k] + matrix[j][k]
            else
                j <- j + 1
        let t = matrix[i][i]
        for k in i .. cols - 1 do
            matrix[i][k] <- matrix[i][k] / t
        for j in i + 1 .. rows - 1 do
            let t = matrix[j][i]
            for k in i .. cols - 1 do
                matrix[j][k] <- matrix[j][k] - t * matrix[i][k]
    for i in rows - 1 .. -1 .. 0 do
        for j in 0 .. i - 1 do
            let t = matrix[j][i]
            for k in i .. cols - 1 do
                matrix[j][k] <- matrix[j][k] - t * matrix[i][k]
    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            matrix[i][j] <- (matrix[i][j]).CanonicalForm

let part2 () =
    // p[i] + t[i] * v[i] = p + t[i] * v
    // -t[i] = (p[i].x - p.x) / (v[i].x - v.x)
    //       = (p[i].y - p.y) / (v[i].y - v.y)
    //       = (p[i].z - p.z) / (v[i].z - v.z)
    //
    // We can now eliminate t[i], but the resulting equations will still be
    // bilinear. Use different i/j to make them linear.
    //
    // (p[i].x - p.x) * (v[i].y - v.y) = (p[i].y - p.y) * (v[i].x - v.x)
    // p.x * v.y - p.y * v.x
    //       = v[i].x * p.y - v[i].y * p.x + p[i].y * v.x - p[i].x * v.y +
    //         p[i].x * v[i].y - p[i].y * v[i].x
    //       = v[j].x * p.y - v[j].y * p.x + p[j].y * v.x - p[j].x * v.y +
    //         p[j].x * v[j].y - p[j].y * v[j].x
    //
    // Repeat for four different (i, j) to get a system of linear equations to
    // find p.x, p.y, v.x, and v.y, then repeat it with x/z or y/z coordinates to
    // find p.z and v.z.
    //
    // Note: it is not actually guaranteed that the resulting system of linear
    // equations is actually linearly independent.
    let lines =
        inputFile ()
        |> readLines
        |> Array.map parseVectors
    let buildMatrix c1 c2 =
        Array.pairwise lines[0..4]
        |> Array.map (fun (linei: Vector * Vector, linej: Vector * Vector) ->
            let pi, vi = linei
            let pj, vj = linej
            [|
                c2 vj - c2 vi
                c1 vi - c1 vj
                c2 pi - c2 pj
                c1 pj - c1 pi
                (c1 pj * c2 vj - c2 pj * c1 vj)
                - (c1 pi * c2 vi - c2 pi * c1 vi)
            |])
    let matxy = buildMatrix (_.x) (_.y)
    let matxz = buildMatrix (_.x) (_.z)
    gauss matxy
    gauss matxz
    string (matxy[0][4] + matxy[1][4] + matxz[1][4])

show [ part1; part2 ]
