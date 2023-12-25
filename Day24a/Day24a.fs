open System
open Util
open Rationals

// An alternative algorithmic solution that doesn't either reduce the
// problem to a math problem or uses a constraint solver.

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

let parseVectors line =
    let nums = parseSignedNumbers line |> Array.map Rational
    (Vector(nums[0], nums[1], nums[2]), Vector(nums[3], nums[4], nums[5]))

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

let part2 () =
    // Observe that when we're looking for the solutions to
    //
    // p[i] + t[i] * v[i] = p + t[i] * v
    //
    // we can transform the equations to
    //
    // p[i] + t[i] * (v[i] - v) = p
    //
    // Thus, we need to find a v such that all lines (p[i], v[i] - v)
    // intersect. This allows us to construct a search algorithm where
    // we check for values of v with increasing vector norms until we find
    // a solution.
    //
    // To further speed this up, we initially only check for intersections
    // with x and y coordinates and then verify that the z coordinate also is
    // correct.
    //
    // Note: there are some corner cases that we ignore for the sake of
    // simplicity, because they significantly increase the complexity of
    // the algorithm without stopping us from finding a solution for our
    // input.
    let lines = inputFile () |> readLines |> Array.map parseVectors
    seq {
        let remainingLines = lines[2..]
        let check (vx: int) (vy: int) =
            let v = Vector(Rational vx, Rational vy, Rational 0)
            let xy (vec: Vector) = (vec.x, vec.y)
            let p0, v0 = lines[0]
            let p0' = p0 + v0 - v
            let p1, v1 = lines[1]
            let p1' = p1 + v1 - v
            let p0, p0' = xy p0, xy p0'
            let p1, p1' = xy p1, xy p1'
            match intersect p0 p0' p1 p1' with
            | Some(ix, iy) ->
                // Check that all lines intersect at this point, first just
                // for x/y coordinates.
                if
                    Seq.forall
                        (fun (pi, vi) ->
                            let p' = pi + vi - v
                            let p, p' = xy pi, xy p'
                            match intersect p0 p0' p p' with
                            | Some(ix', iy') -> ix = ix' && iy = iy'
                            | None -> false)
                        remainingLines
                then
                    // Now determine vz and verify that all lines also
                    // intersect in the z dimension.
                    let p0, v0 = lines[0]
                    let p1, v1 = lines[1]
                    let timeToImpact (pi: Vector) (vi: Vector) =
                        if ix <> pi.x then (ix - pi.x) / (vi.x - v.x)
                        else if iy <> pi.y then (iy - pi.y) / (vi.y - v.y)
                        else Rational.Zero
                    let t0 = timeToImpact p0 v0
                    let t1 = timeToImpact p1 v1
                    // If t0 = t1, either any vz works for this pair (if the
                    // numerator is zero) or none (if the numerator isn't
                    // zero). In practice, this doesn't seem to happen, so we
                    // ignore this case for the sake of simplicity.
                    let vz = (p0.z + t0 * v0.z - p1.z - t1 * v1.z) / (t0 - t1)
                    if
                        Seq.forall
                            (fun (pi: Vector, vi: Vector) ->
                                let ti = timeToImpact pi vi
                                p0.z + t0 * (v0.z - vz) = pi.z
                                                          + ti * (vi.z - vz))
                            remainingLines
                    then
                        let v = Vector(vx, vy, vz)
                        Some (p0 + (v0 - v) * t0).reduce
                    else
                        None
                else
                    None
            | None -> None

        for d in 1 .. Int32.MaxValue do
            for d' in -d .. d do
                yield check d d'
                yield check -d d'
                if d' <> d && d' <> -d then
                    yield check d' d
                    yield check d' -d
    }
    |> Seq.choose id
    |> Seq.head
    |> fun (x, y, z) -> x + y + z |> string


show [ part1; part2 ]
