import sets
import sequtils
import strutils

type
  Point = tuple[x, y: int]
  Dir {.pure.} = enum
    up, down, left, right

proc do_step(point: Point, dir: Dir): Point =
  case dir
  of Dir.up:
    (point.x, point.y+1)
  of Dir.down:
    (point.x, point.y-1)
  of Dir.left:
    (point.x-1, point.y)
  of Dir.right:
    (point.x+1, point.y)

proc manhattan_distance(p1, p2: Point): int =
  abs(p1.x - p2.x) + abs(p1.y - p2.y)

proc input(): (seq[string], seq[string]) =
  let content = readFile("day3-input.txt")
  let wires = content.strip().split("\n")
  (wires[0].split(","),
   wires[1].split(","))

proc direction_from(command: string): Dir =
  let direction = command[0]
  case direction
  of 'U': Dir.up
  of 'D': Dir.down
  of 'L': Dir.left
  of 'R': Dir.right
  else:
    let msg = "I don't know how to handle that better yet: " & $command
    raise newException(IOError, msg)

proc steps_from(command: string): int =
  parseInt(command[1..^1])

proc parse_command(command: string): (Dir, int) =
  (direction_from(command),
   steps_from(command))

proc move(p: var Point, dir: Dir, steps:int):  HashSet[Point] =
  var s: HashSet[Point]
  s.incl(p)
  for i in 0 .. steps-1:
    p = p.do_step(dir)
    s.incl(p)
  return s

proc points_of(wire: seq[string]): HashSet[Point] =
  var
    p = (0, 0)
    points: HashSet[Point]
  for command in wire:
    let (dir, steps) = parse_command(command)
    let s = move(p, dir, steps)
    points = union(points, s)
  return points

let (wire1, wire2) = input()
var points_wire1 = points_of(wire1)
points_wire1.excl((0, 0))
var points_wire2 = points_of(wire2)
points_wire2.excl((0, 0))
let intersections = toSeq(intersection(points_wire1, points_wire2))
let distances = intersections.mapIt(manhattan_distance((0, 0), it))
let min_index = minIndex(distances)

echo "closest point: ", intersections[min_index]
echo "with distance: ", distances[min_index]
