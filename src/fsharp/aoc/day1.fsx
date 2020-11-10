open System
open System.IO

let lines = File.ReadAllLines("day1-input.txt")

let numbers = Seq.map int lines

let mass x = x / 3 - 2
// part1
Seq.sumBy mass numbers

//part2
let cumulativeMass x =
    let rec loop acc n =
        let m = mass n
        if m <= 0 then acc else loop (acc + m) m

    loop 0 x

Seq.sumBy cumulativeMass numbers
