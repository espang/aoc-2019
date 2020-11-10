// 10007 cards 0..10006 in this order!


let techniqueOne cards = List.rev cards

let techniqueTwo n (cards: int list) =
    let numberOfCards = if n > 0 then n else cards.Length + n
    (List.skip numberOfCards cards)
    @ (List.take numberOfCards cards)

let techniqueThree n (cards: int list) =
    let arr: int array = Array.zeroCreate cards.Length
    let nextIndex i = (i + n) % arr.Length

    let rec loop (acc: int []) index stack =
        match stack with
        | next :: rest ->
            acc.[index] <- next
            loop acc (nextIndex index) rest
        | [] -> acc

    loop arr 0 cards |> Array.toList

let find v cards =
    let rec loop pos cards =
        match cards with
        | head :: tail -> if head = v then pos else loop (pos + 1) tail
        | [] -> pos

    loop 0 cards

let shuffle (command: string) cards =
    if command.StartsWith "deal with" then
        let words = command.Trim().Split(" ")
        techniqueThree (int words.[3]) cards
    elif command.StartsWith "cut" then
        let words = command.Trim().Split(" ")
        techniqueTwo (int words.[1]) cards
    else
        assert (command = "deal into new stack")
        techniqueOne cards

let applyCommands commands cards =
    let rec loop acc cmds =
        match cmds with
        | head :: tail -> loop (shuffle head acc) tail
        | [] -> acc

    loop cards commands

open System.IO

let commands =
    File.ReadAllLines("day22-input.txt")
    |> Array.toList

// part 1:
let shuffledDeck = applyCommands commands [ 0 .. 10006 ]
find 2019 shuffledDeck
