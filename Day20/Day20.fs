open System.Collections.Generic
open Util

type Pulse =
    | Low
    | High

[<AbstractClass>]
type Gate() =
    let _outputs = List<Gate>()
    let _inputs = MutableMap<Gate, Pulse>()
    member this.outputs = _outputs
    member this.inputs = _inputs
    abstract member receivePulse: Gate -> Pulse -> (Gate * Pulse) array
    member this.sendPulse(pulse: Pulse) =
        Seq.map (fun gate -> (gate, pulse)) _outputs |> Array.ofSeq
    member this.connect(other: Gate) =
        this.outputs.Add(other)
        other.inputs[this] <- Low

type FlipFlop() =
    inherit Gate()
    let mutable on = false
    override this.receivePulse (_: Gate) (pulse: Pulse) =
        if pulse = High then
            [||]
        else
            on <- not on
            let outPulse = if on then High else Low
            this.sendPulse outPulse

type Conjunction() =
    inherit Gate()
    override this.receivePulse (from: Gate) (pulse: Pulse) =
        this.inputs[from] <- pulse
        if Seq.forall ((=) High) this.inputs.Values then
            this.sendPulse Low
        else
            this.sendPulse High

type Untyped() =
    inherit Gate()
    override this.receivePulse (_: Gate) (_: Pulse) = [||]

type Broadcaster() =
    inherit Gate()
    override this.receivePulse (_: Gate) (pulse: Pulse) = this.sendPulse pulse

let parseGate line : string * Gate * string array =
    let gateNames = parseWords line
    if line.StartsWith("&") then
        (gateNames[0], Conjunction(), gateNames[1..])
    else if line.StartsWith("%") then
        (gateNames[0], FlipFlop(), gateNames[1..])
    else
        (gateNames[0], Broadcaster(), gateNames[1..])

let pushButton broadcaster =
    let pulseQueue = Queue<Gate * Gate * Pulse>()
    pulseQueue.Enqueue(broadcaster, broadcaster, Low)
    let mutable lowPulses = 1
    let mutable highPulses = 0
    while pulseQueue.Count <> 0 do
        let fromGate, toGate, pulse = pulseQueue.Dequeue()
        for nextGate, nextPulse in toGate.receivePulse fromGate pulse do
            match nextPulse with
            | Low -> lowPulses <- lowPulses + 1
            | High -> highPulses <- highPulses + 1
            pulseQueue.Enqueue(toGate, nextGate, nextPulse)
    (lowPulses, highPulses)

let pushButtonChecked broadcaster checkTransition =
    let pulseQueue = Queue<Gate * Gate * Pulse>()
    pulseQueue.Enqueue(broadcaster, broadcaster, Low)
    while pulseQueue.Count <> 0 do
        let fromGate, toGate, pulse = pulseQueue.Dequeue()
        checkTransition toGate pulse
        for nextGate, nextPulse in toGate.receivePulse fromGate pulse do
            pulseQueue.Enqueue(toGate, nextGate, nextPulse)

let buildNetwork () =
    let input = inputFile () |> readLines |> Array.map parseGate
    let gates = MutableMap<string, Gate>()
    for name, gate, _ in input do
        gates[name] <- gate
    for _, gate, outputs in input do
        for output in outputs do
            if not (gates.ContainsKey output) then
                gates[output] <- Untyped()
            gate.connect gates[output]
    gates

let part1 () =
    let gates = buildNetwork ()
    let broadcaster = gates["broadcaster"]
    seq { 1..1000 }
    |> Seq.map (fun _ -> pushButton broadcaster)
    |> Seq.fold (fun (lo, hi) (lo', hi') -> (lo + lo', hi + hi')) (0, 0)
    |> fun (lo, hi) -> int64 lo * int64 hi

let part2 () =
    let gates = buildNetwork ()
    let broadcaster = gates["broadcaster"]
    let rx = gates["rx"]
    let rxInput =
        Seq.filter (fun (gate: Gate) -> gate.outputs.Contains rx) gates.Values
        |> Seq.exactlyOne
    let preRxInputs =
        Seq.filter
            (fun (gate: Gate) -> gate.outputs.Contains rxInput)
            gates.Values
        |> MutableSet
    let cycleLengths = MutableMap<Gate, int64>()
    let mutable buttonPresses = 0
    while cycleLengths.Count <> preRxInputs.Count do
        buttonPresses <- buttonPresses + 1
        pushButtonChecked broadcaster (fun gate pulse ->
            if
                pulse = Low
                && preRxInputs.Contains gate
                && not (cycleLengths.ContainsKey gate)
            then
                cycleLengths[gate] <- buttonPresses)
    Seq.reduce lcm64 cycleLengths.Values

show [ part1; part2 ]
