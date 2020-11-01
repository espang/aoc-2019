import algorithm
import lists
import sequtils
import strutils

type
  Instruction = tuple[code: int, immediate: array[3, bool]]
  Action {.pure.} = enum
    input, output, other, done
  Amplifier = ref object
    program: seq[int]
    inputs: DoublyLinkedList[int]
    outputs: DoublyLinkedList[int]
    last_output: int
    index: int
    done: bool

proc init(a: var Amplifier, program: seq[int]) =
  var p = program
  a.program = p

proc add_input(a: var Amplifier, input: int) =
  a.inputs.append(newDoublyLinkedNode[int](input))

proc peek_input(a: var Amplifier): int =
  a.inputs.head.value

proc try_peek_input(a: var Amplifier): int =
  if not isNil(a.inputs.head):
    a.peek_input()
  else: 0
  
proc pop_input(a: var Amplifier): int =
  let head = a.inputs.head
  a.inputs.remove(head)
  head.value

proc add_output(a: var Amplifier, output: int) =
  a.outputs.append(newDoublyLinkedNode[int](output))

proc next_output(a: var Amplifier): int =
  let head = a.outputs.head
  a.outputs.remove(head)
  a.last_output = head.value
  head.value

proc step(program: var seq[int], input, index: int): (int, int, Action)
proc run(a: var Amplifier) =
  if a.done: return
  var next_input = a.try_peek_input()  
  while true:
    let (next_index, output, action) = step(a.program, next_input, a.index)
    case action
    of Action.input:
      discard a.pop_input()
      next_input = a.try_peek_input()
      a.index = next_index
    of Action.output:
      a.add_output(output)
      a.index = next_index
      return
    of Action.done:
      echo "done!!!!"
      a.done = true
      return
    of Action.other:
      a.index = next_index
    
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

proc step(program: var seq[int], input, index: int): (int, int, Action) =
  let instruction = parse_instruction(program[index])
  
  case instruction.code
  of 1:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
      i3 = if instruction.immediate[2]: index+3 else: program[index+3]
    program[i3] = program[i1] + program[i2]
    (index+4, 0, Action.other)
  of 2:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
      i3 = if instruction.immediate[2]: index+3 else: program[index+3]
    program[i3] = program[i1] * program[i2]
    (index+4, 0, Action.other)
  of 3:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
    program[i1] = input
    (index+2, 0, Action.input)
  of 4:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
    (index+2, program[i1], Action.output)
  of 5:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
    if not (program[i1] == 0):
      (program[i2], 0, Action.other)
    else:
      (index+3, 0, Action.other) 
  of 6:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
    if program[i1] == 0:
      (program[i2], 0, Action.other)
    else:
      (index+3, 0, Action.other)
  of 7:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
      i3 = if instruction.immediate[2]: index+3 else: program[index+3]
    program[i3] = if program[i1] < program[i2]: 1 else: 0
    (index+4, 0, Action.other)
  of 8:
    let
      i1 = if instruction.immediate[0]: index+1 else: program[index+1]
      i2 = if instruction.immediate[1]: index+2 else: program[index+2]
      i3 = if instruction.immediate[2]: index+3 else: program[index+3]
    program[i3] = if program[i1] == program[i2]: 1 else: 0
    (index+4, 0, Action.other)
  of 99:
    (0, 0, Action.done)
  else:
    echo "unexpected instruction: ", program[index]
    (0, 0, Action.done)

proc run(program: seq[int], phase_setting: seq[int]): int =
  var
    p_a = program
    amp_a = Amplifier(program : p_a)
    p_b = program
    amp_b = Amplifier(program : p_b)
    p_c = program
    amp_c = Amplifier(program : p_c)
    p_d = program
    amp_d = Amplifier(program : p_d)
    p_e = program
    amp_e = Amplifier(program : p_e)

  amp_a.init(program)
  amp_b.init(program)
  amp_c.init(program)
  amp_d.init(program)
  amp_e.init(program)

  amp_a.add_input(phase_setting[0])
  amp_a.add_input(0)
  amp_b.add_input(phase_setting[1])
  amp_c.add_input(phase_setting[2])
  amp_d.add_input(phase_setting[3])
  amp_e.add_input(phase_setting[4])
  
  while not amp_e.done:
    amp_a.run()
    if not amp_a.done:
      amp_b.add_input(amp_a.next_output())
    amp_b.run()
    if not amp_b.done:
      amp_c.add_input(amp_b.next_output())
    amp_c.run()
    if not amp_c.done:
      amp_d.add_input(amp_c.next_output())
    amp_d.run()
    if not amp_d.done:
      amp_e.add_input(amp_d.next_output())
    amp_e.run()
    
    if amp_e.done:
      return amp_e.last_output
    else: amp_a.add_input(amp_e.next_output())
  0
  
let program = input_to_program("day7-input.txt")

when true:
  var phase_setting = @[5, 6, 7, 8, 9]
  var maximum = run(program, phase_setting)
  var best = phase_setting.mapIt($it).join("")
  while phase_setting.nextPermutation():
    let r = run(program, phase_setting)
    echo "result: ", r
    if r > maximum:
      maximum = r
      best = phase_setting.mapIt($it).join("")
  echo best
  echo maximum

