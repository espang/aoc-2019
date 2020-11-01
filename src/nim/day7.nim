import algorithm
import sequtils
import strutils

type
  Instruction = tuple[code: int, immediate: array[3, bool]]
  Action {.pure.} = enum
    input, output, other, done

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

proc step(program: var seq[int], input, index: int): (int, Action) =
  let instruction = parse_instruction(program[index])
  
  case instruction.code
  of 1:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
      i3 = if instruction.immediate[2]: index+3 else: program[index+3]
    program[i3] = program[i1] + program[i2]
    (index+4, Action.other)
  of 2:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
      i3 = if instruction.immediate[2]: index+3 else: program[index+3]
    program[i3] = program[i1] * program[i2]
    (index+4, Action.other)
  of 3:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
    program[i1] = input
    (index+2, Action.input)
  of 4:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
    (program[i1], Action.output)
  of 5:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
    if not (program[i1] == 0):
      (program[i2], Action.other)
    else:
      (index+3, Action.other) 
  of 6:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
    if program[i1] == 0:
      (program[i2], Action.other)
    else:
      (index+3, Action.other)
  of 7:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
      i3 = if instruction.immediate[2]: index+3 else: program[index+3]
    program[i3] = if program[i1] < program[i2]: 1 else: 0
    (index+4, Action.other)
  of 8:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
      i3 = if instruction.immediate[2]: index+3 else: program[index+3]
    program[i3] = if program[i1] == program[i2]: 1 else: 0
    (index+4, Action.other)
  of 99:
    (0, Action.done)
  else:
    echo "unexpected instruction: ", program[index]
    (0, Action.done)

proc loop(program: seq[int], input1, input2: int): int =
  var p = program
  var pos: int
  var action = Action.other
  var i = input1
  while not (action == Action.output or action == Action.done):
    let (np, na) = step(p, i, pos)
    if na == Action.output:
      return np
    if na == Action.input:
      i = input2
    pos = np
    action = na

proc run(program: seq[int], phase_setting: seq[int]): int =
  let output_a = loop(program, phase_setting[0], 0)
  let output_b = loop(program, phase_setting[1], output_a)
  let output_c = loop(program, phase_setting[2], output_b)
  let output_d = loop(program, phase_setting[3], output_c)
  loop(program, phase_setting[4], output_d)
  
let program = input_to_program("day7-input.txt")

var phase_setting = @[0, 1, 2, 3, 4]
var maximum = run(program, phase_setting)
var best = phase_setting.mapIt($it).join("")
while phase_setting.nextPermutation():
  let r = run(program, phase_setting)
  if r > maximum:
    maximum = r
    best = phase_setting.mapIt($it).join("")
echo best
echo maximum
