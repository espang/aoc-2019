import sequtils
import strutils

type
  Instruction = tuple[code: int, immediate: array[3, bool]]

proc input_to_program(filename: string): seq[int] =
  let raw = readFile(filename)
  raw.strip().split(",").map(parseInt)

proc parse_instruction(i: int): Instruction =
  var tmp: int
  tmp = i div 100
  var s: array[3, bool]
  s[0] = tmp mod 10 == 1
  tmp = tmp div 10
  s[1] = tmp mod 10 == 1
  tmp = tmp div 10
  s[2] = tmp mod 10 == 1
  (i mod 100, s)

proc step(program: var seq[int], input, index: int): int =
  let instruction = parse_instruction(program[index])
  
  case instruction.code
  of 1:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
      i3 = if instruction.immediate[2]: index+3 else: program[index+3]
    program[i3] = program[i1] + program[i2]
    index+4
  of 2:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
      i3 = if instruction.immediate[2]: index+3 else: program[index+3]
    program[i3] = program[i1] * program[i2]
    index+4
  of 3:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
    program[i1] = input
    index+2
  of 4:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
    echo "output: ", program[i1]
    index+2
  of 5:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
    if not (program[i1] == 0):
      program[i2]
    else:
      index+3 
  of 6:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
    if program[i1] == 0:
      program[i2]
    else:
      index+3
  of 7:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
      i3 = if instruction.immediate[2]: index+3 else: program[index+3]
    program[i3] = if program[i1] < program[i2]: 1 else: 0
    index+4
  of 8:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
      i3 = if instruction.immediate[2]: index+3 else: program[index+3]
    program[i3] = if program[i1] == program[i2]: 1 else: 0
    index+4
  of 99:
    -1
  else:
    echo "unexpected instruction: ", program[index]
    -1

proc loop(program: seq[int], input: int = 1) =
  var p = program
  var pos: int
  while pos >= 0:
    pos = step(p, input, pos)
  echo "loop done, program[0]: ", p[0]


# part1:
loop(input_to_program("day5-input.txt"))

# part2:
echo "---"
loop(input_to_program("day5-input.txt"), 5)
