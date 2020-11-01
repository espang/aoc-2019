import sequtils
import sets
import streams
import strutils
import tables

proc parse_input(s: string): (string, string) =
  let t = s.strip().split(")")
  (t[0], t[1])

proc input_to_planets(filename: string): Table[string, string] =
  var t: Table[string, string]
  var line: string
  var fs = newFileStream(filename)
  if not isNil(fs):
    while fs.readLine(line):
      let (inner, outer) = parse_input(line)
      if t.hasKey(outer): echo "unexpected orbits"
      t[outer] = inner
  # planets
  t

proc orbits_of(planet: string, planets: Table[string, string]): int =
  if planets.hasKey(planet):
    1 + orbits_of(planets[planet], planets)
  else:
    0

proc checksum(planets: Table[string, string]): int =
  var total: int
  for k in planets.keys():
    total += orbits_of(k, planets)
  total

proc distances(planet: string, planets: Table[string, string]): Table[string, int] =
  var t: Table[string, int]
  var p = planet
  var d = 1
  while planets.hasKey(p):
    p = planets[p]
    t[p] = d
    d += 1
  t



let planets = input_to_planets("day6-input.txt")

# part 1
echo "part1: ", checksum(planets)

# part 2
let dist_you = distances("YOU", planets)
let dist_san = distances("SAN", planets)

var minimum = 0
var shared_orbit: string
for k in dist_you.keys():
  if dist_san.hasKey(k):
    if minimum == 0 or minimum > dist_you[k]:
      shared_orbit = k
      minimum = dist_you[k]

echo "part2: ", dist_you[shared_orbit] + dist_san[shared_orbit] - 2
