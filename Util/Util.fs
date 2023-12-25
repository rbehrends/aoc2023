namespace Util

open System
open System.IO
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open System.Text.RegularExpressions

type PriorityQueue<'el, 'prio> =
    System.Collections.Generic.PriorityQueue<'el, 'prio>

type Queue<'t> =
    System.Collections.Generic.Queue<'t>

// We are using these instead of the ones in FSharp.Collections for speed
type MutableSet<'t> = System.Collections.Generic.HashSet<'t>
type MutableMap<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>
type MutableList<'t> = System.Collections.Generic.List<'t>
type HashSet<'t> = System.Collections.Generic.HashSet<'t>
type Dictionary<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

[<AutoOpen>]
type Util =
    static member numberRegex = Regex(@"\d+")
    static member alphanumRegex = Regex(@"[0-9a-zA-Z]+")
    static member alphanumOrNotRegex = Regex(@"[0-9a-zA-Z]+|[^0-9a-zA-Z]+")
    static member signedNumberRegex = Regex(@"-?\d+")

    static member parseNumbers str =
        numberRegex.Matches(str)
        |> Seq.map (fun m -> Int64.Parse(m.ToString()))
        |> Seq.toArray

    static member parseNumbersWithPositions str =
        numberRegex.Matches(str)
        |> Seq.map (fun m ->
            (m.Index, m.Index + m.Length - 1, Int64.Parse(m.ToString())))
        |> Seq.toArray

    static member parseSignedNumbers str =
        signedNumberRegex.Matches(str)
        |> Seq.map (fun m -> Int64.Parse(m.ToString()))
        |> Seq.toArray

    static member parseWords str =
        alphanumRegex.Matches(str)
        |> Seq.map (fun m -> m.ToString())
        |> Seq.toArray

    static member parseWordsAndSeps str =
        alphanumOrNotRegex.Matches(str)
        |> Seq.map (fun m -> m.ToString())
        |> Seq.toArray

    static member currentDir
        ([<CallerFilePath; Optional; DefaultParameterValue("")>] path: string)
        =
        DirectoryInfo(path).Parent.FullName

    static member parentDir
        ([<CallerFilePath; Optional; DefaultParameterValue("")>] path: string)
        =
        DirectoryInfo(path).Parent.Parent.FullName

    static member day
        ([<CallerFilePath; Optional; DefaultParameterValue("")>] path: string)
        =
        let day = DirectoryInfo(path).Parent.Name
        let number = numberRegex.Match day
        number.ToString()

    static member inputFile
        ([<CallerFilePath; Optional; DefaultParameterValue("")>] path: string)
        =
        let args = Environment.GetCommandLineArgs()
        if args.Length > 1 then
            args[1]
        else
            $"{parentDir (path)}/input/input{day (path)}.txt"

    static member testFile
        ([<CallerFilePath; Optional; DefaultParameterValue("")>] path: string)
        =
        $"{parentDir (path)}/input/test{day (path)}.txt"

[<AutoOpen>]
module Util =
    let readFile path = File.ReadAllText path
    let readLines path = File.ReadAllLines path
    let readRecords path = readFile path |> _.Split("\n\n")
    let print (str: 'a) = Console.Write str
    let println (str: 'a) = Console.WriteLine str
    let makeMutableMap (pairs: ('a *'b) seq) =
        Seq.map (fun (a, b) ->
            System.Collections.Generic.KeyValuePair(a, b)) pairs |> MutableMap
    let mapToSeq (map: MutableMap<'a, 'b>): ('a * 'b) seq =
        seq {
            for entry in map do
                yield entry.Key, entry.Value
        }
    let show (parts: (unit -> 'a) list) =
        let mutable counter = 1
        for part in parts do
            let timer = System.Diagnostics.Stopwatch()
            timer.Start()
            let result = part ()
            timer.Stop()
            Console.WriteLine(
                $"part {counter}: {result} ({double timer.ElapsedMilliseconds / 1000.0}s)"
            )
            counter <- counter + 1
    let observe value =
        println value
        value
    let observeWith fn value =
        println (fn value)
        value
    let rec gcd a b =
        if b = 0 then a
        else gcd b (a % b)
    let lcm a b = a / (gcd a b) * b
    let rec gcd64 a b =
        if b = 0L then a
        else gcd64 b (a % b)
    let lcm64 a b = a / (gcd64 a b) * b

#nowarn "25"

module ArrayOps =
    let asPair [|a; b|] = (a, b)
    let asTriple [|a; b; c|] = (a, b, c)
    let pairs arr = Array.chunkBySize 2 arr |> Array.map (fun [|a;b|] -> a, b)
    let triples arr = Array.chunkBySize 3 arr |> Array.map (fun [|a;b;c|] -> a, b, c)
