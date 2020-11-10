open System.IO

type Dir =
    | Up
    | Down
    | Left
    | Right

// 2 line input:
let wires = File.ReadAllLines("day3-input.txt")

let wire1 = Array.toList (wires.[0].Split ",")
let wire2 = Array.toList (wires.[1].Split ",")

let dirFromStr s =
    match s with
    | 'l'
    | 'L' -> Some Left
    | 'r'
    | 'R' -> Some Right
    | 'u'
    | 'U' -> Some Up
    | 'd'
    | 'D' -> Some Down
    | _ -> None

let addPoints (dx, dy) (x, y) = x + dx, y + dy

let step dir (x, y) =
    let t =
        match dir with
        | Up -> 0, 1
        | Down -> 0, -1
        | Left -> -1, 0
        | Right -> 1, 0

    addPoints t (x, y)

let moveAndKeepTrack (x, y, cmd: string) =
    let mutable s = Set.empty
    match dirFromStr cmd.[0] with
    | Some dir ->
        let move = step dir
        let steps = int cmd.[1..]
        let mutable coord = (x, y)
        for i = 1 to steps do
            coord <- move coord
            s <- Set.add coord s
        s, coord
    | None -> s, (x, y)

let wireCoords wire =
    let rec loop s (x, y) lines =
        match lines with
        | h :: t ->
            let newSet, newPosition = moveAndKeepTrack (x, y, h)
            loop (Set.union s newSet) newPosition t
        | [] -> s

    loop Set.empty (0, 0) wire

let manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let coords1 = wireCoords wire1
let coords2 = wireCoords wire2

let intersections = Set.intersect coords1 coords2
let distance = manhattanDistance (0, 0)

let findMinimum (values: Set<int * int>) distance =
    let rec loop ((x, y), min) s =
        match s with
        | h :: t ->
            let d = distance h
            if d < min then loop (h, d) t else loop ((x, y), min) t
        | [] -> ((x, y), min)

    loop ((0, 0), System.Int32.MaxValue) (Set.toList values)

// part 1:
findMinimum intersections distance

open System.Collections.Generic
// part 2:
let stepsTo (wire: string list) =
    let rec loop (acc: Dictionary<(int * int), int>) currentPosition totalSteps (commands: string list) =
        match commands with
        | nextCommand :: otherCommands ->
            match dirFromStr nextCommand.[0] with
            | Some dir ->
                let steps = int nextCommand.[1..]
                let mutable pos = currentPosition
                for i = 1 to steps do
                    pos <- step dir pos
                    incr totalSteps
                    if not (acc.ContainsKey pos) then acc.Add(pos, !totalSteps)
                loop acc pos totalSteps otherCommands
            | None -> acc
        | [] -> acc

    let d = new Dictionary<(int * int), int>()
    loop d (0, 0) (ref 0) wire

let dist1 = stepsTo wire1
let dist2 = stepsTo wire2

let mutable result = Map.empty
let minimum = System.Int32.MaxValue

for item in dist1 do
    if dist2.ContainsKey item.Key then
        let value = dist2.Item item.Key
        result <- result.Add(item.Key, item.Value + value)
