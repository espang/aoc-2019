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

let moveAndKeepTrack (x, y, cmd: string) =
    let mutable s = Set.empty
    match dirFromStr cmd.[0] with
    | Some dir ->
        let t =
            match dir with
            | Up -> 0, 1
            | Down -> 0, -1
            | Left -> -1, 0
            | Right -> 1, 0

        let move = addPoints t
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
