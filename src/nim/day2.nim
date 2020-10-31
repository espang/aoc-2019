import sequtils
import strutils

# optcode one two three
# 1 -> program[three] = program[one] + program[two]
# 2 -> program[three] = program[one] * program[two]
# 99 -> end
proc step(program: var seq[int], index: int): (int, bool) =
  let optcode = program[index]
  case optcode
  of 1:
    let
      i1 = program[index+1]
      i2 = program[index+2]
      i3 = program[index+3]
    program[i3] = program[i1] + program[i2]
    (index+4, true)
  of 2:
    let
      i1 = program[index+1]
      i2 = program[index+2]
      i3 = program[index+3]
    program[i3] = program[i1] * program[i2]
    (index+4, true)
  of 99:
    (index, false)
  else:
    echo "unexpected optcode:", optcode, "at index:", index
    echo "program:", program
    (index, false)

proc loop(program: var seq[int]) =
  var pos: int
  var not_done = true
  while not_done:
    (pos, not_done) = step(program, pos)

# part 1:
let raw_program_part1 = readFile("day2-1-input.txt")
var program_part1 = raw_program_part1.strip().split(",").map(parseInt)
# restore the gravity assit program to the 1202 program alarm
program_part1[1] = 12
program_part1[2] = 2
loop(program_part1)

echo "result part1: ", program_part1[0]

# part 2:
proc loop_with(program: seq[int], noun, verb: int): int =
  var p = program
  p[1] = noun
  p[2] = verb
  loop(p)
  p[0]

proc find_noun_verb(program: seq[int], wanted: int): (int, int) =
  for noun in 0..99:
    for verb in 0..99:
      let got = loop_with(program, noun, verb)
      if got == wanted:
        return (noun, verb)
  (-1, -1)

program_part1 = raw_program_part1.strip().split(",").map(parseInt)
let (noun, verb) = find_noun_verb(program_part1, 19690720)
echo "result part2: ", 100*noun+verb
