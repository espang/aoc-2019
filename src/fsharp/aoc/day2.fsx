open System.IO

type Operation =
    | Addition
    | Multiplication
    | Halt
    | Unknown of int

let parseOperation n =
    match n with
    | 1 -> Addition
    | 2 -> Multiplication
    | 99 -> Halt
    | _ -> Unknown n

// parseOperation(2)
let memory =
    Array.map int (File.ReadAllText("day2-input.txt").Split ",")

let step (memory: int [], index: int) =
    let operation = parseOperation memory.[index]
    match operation with
    | Addition ->
        memory.[memory.[index + 3]] <- memory.[memory.[index + 1]]
                                       + memory.[memory.[index + 2]]
        index + 4, operation
    | Multiplication ->
        memory.[memory.[index + 3]] <- memory.[memory.[index + 1]]
                                       * memory.[memory.[index + 2]]
        index + 4, operation
    | Halt -> index, operation
    | Unknown n ->
        printfn "unknown command '%d'" n
        index, operation

let run (mem: int []) =
    let rec loop index =
        let nextIndex, operation = step (mem, index)
        match operation with
        | Halt -> ()
        | Unknown cmd -> ()
        | _ -> loop nextIndex

    loop 0

let copyMem (mem: int []) =
    Array.init mem.Length (fun i -> memory.[i])

// part1:
// copy the input into a mutable array:
let mem = copyMem memory

mem.[1] <- 12
mem.[2] <- 2

run (mem)
printfn "result of part1: %d" mem.[0]


// part2:
//
let runWith noun verb mem =
    let copiedMem = copyMem mem
    copiedMem.[1] <- noun
    copiedMem.[2] <- verb
    run copiedMem
    copiedMem.[0]

for noun in 0 .. 99 do
    for verb in 0 .. 99 do
        let result = runWith noun verb memory
        if result = 19690720
        then printfn "noun: %d, verb: %d. Result: %d" noun verb (noun * 100 + verb)
