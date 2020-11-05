import intsets
import strutils
import tables

type
  Board = ref object
    bugs: int
  Point = tuple[x, y:int]

proc add_bug_at(b: var Board, p: Point) =
  let location = p.y * 5 + p.x
  b.bugs = b.bugs or (1 shl location)

proc rem_bug_at(b: var Board, p: Point) =
  let location = p.y * 5 + p.x
  b.bugs = b.bugs xor (1 shl location)

proc is_bug_at(b: Board, p: Point): bool =
  if p.y < 0 or p.y > 4 or p.x < 0 or p.x > 4:
    return false
  let location = p.y * 5 + p.x
  let ret = ((b.bugs shr location) and 1) == 1
  ret

proc adjacent_bugs_at(b: Board, p: Point): int = 
  var adjacent_bugs = 0
  if b.is_bug_at((p.x-1, p.y)): adjacent_bugs += 1
  if b.is_bug_at((p.x+1, p.y)): adjacent_bugs += 1
  if b.is_bug_at((p.x, p.y-1)): adjacent_bugs += 1
  if b.is_bug_at((p.x, p.y+1)): adjacent_bugs += 1
  adjacent_bugs

proc shall_die(b: Board, p: Point): bool =
  let n = b.adjacent_bugs_at(p)
  n != 1

proc shall_infest(b: Board, p: Point): bool = 
  let n = b.adjacent_bugs_at(p)
  n == 1 or n == 2
  

# shows the board mirrored
proc show(b: Board) =
  echo "----- ", b.bugs
  for i in 0..4:
    let row = (0b11111 and (b.bugs shr (i*5)))
    echo row.toBin(5)

proc step(b: Board): Board =
  var b1 = Board(bugs: b.bugs)
  for x in 0..4:
    for y in 0..4:
      let p = (x, y)
      if b.is_bug_at(p):
        if b.shall_die(p):
          b1.rem_bug_at(p)
        else: discard
      else:
        if b.shall_infest(p):
          b1.add_bug_at(p)
  b1

const 
  test_bugs = 0b0000100100110010100110000
  input_bugs = 0b0010100000100100000001001

when false:
  var b = Board(bugs : input_bugs)
  var seen: IntSet
  b.show()
  seen.incl(b.bugs)
  while true:
    b = b.step()
    if seen.contains(b.bugs):
      echo "biodiversity rating is: ", b.bugs
      break
    seen.incl(b.bugs)
  

# 
var grid: Table[int, Board]
grid[0] = Board(bugs: test_bugs)
grid[-1] = Board()
grid[1] = Board()

let iterations = 10
for step in 0..9:
  echo "step ", step 
  var next: Table[int, Board]
  
  if step mod 3 == 0:
    # bugs need 3 minutes to grow to the edge of
    # a grid and then new grids might be infected
    # with bugs. Grow the grid:
    let index = 1 + step div 3
    grid[index] = Board()
    grid[-index] = Board()

  # handle all grids:
  for k,v in grid.pairs:
    # create a new grid for the handled grid for the next step
    var b1 = Board(bugs : v.bugs)
    for x in 0..4:
      for y in 0..4:
        let p = (x, y)
        var adjacent_bugs = grid[k].adjacent_bugs_at(p)
        if x == 0:
          if grid.hasKey(k-1):
            
    next[k] = b1
  grid = next
    

# count bugs
var total: int
for _,v in grid.pairs:
  for x in 0..4:
    for y in 0..4:
      let p = (x,y)
      if v.is_bug_at(p):
        total += 1
echo "number of bugs: ", total
