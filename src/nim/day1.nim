import sequtils
import streams
import strutils


proc full_required(mass: int): int =
  mass div 3 - 2

proc read_file(fs: FileStream): seq[int] =
  var modules: seq[int] = @[]
  var line: string
  if not isNil(fs):
    echo "start reading"
    while fs.readLine(line):
      modules.add(parseInt(line))
    fs.close()
  else:
    echo "stream was nil"
  return modules

# Get answer for day 1 part 1:
var strm = newFileStream("day1-1-input.txt")
let modules = read_file(strm)
echo "answer1:", foldr(modules.mapIt(full_required(it)), a + b)


# Get answer for day 1 part 2:
proc rec_fuel_required(mass: int): int =
  let required_fuel = full_required(mass)
  if required_fuel <= 0:
    0
  else:
    required_fuel + rec_fuel_required(required_fuel)

echo "answer2:", foldr(modules.mapIt(rec_fuel_required(it)), a+b)
