import strutils

type
  Layer = ref object
    width: int
    height: int
    values: seq[int]
    index: int

proc newLayer(width, height: int): Layer =
  Layer(
    width: width,
    height: height,
    values: newSeq[int](width*height)
  )

proc add_point(l: var Layer, v: int) =
  l.values[l.index] = v
  l.index.inc

proc is_full(l: Layer): bool =
  l.index == l.width * l.height

proc count(l: Layer, search:int): int =
  for _, v in l.values:
    if v == search:
      result.inc

proc show(l: Layer) =
  var i = 0
  for row in 0.. l.height-1:
    var rowS = ""
    for col in 0.. l.width-1:
      if l.values[i] == 0:
        rowS = rowS & " "
      elif l.values[i] == 1:
        rowS = rowS & "#"
      else: 
        rowS = rowS & "T"
      i.inc
    echo rowS

proc decode_image(image: string, width, height: int): seq[Layer] =
  var l = newLayer(width, height)
  for _, c in image:
    if l.is_full():
      result.add(l)
      l = newLayer(width, height)
    l.add_point(parseInt($c))
  result.add(l)
  result

let
  test_input = "123456789012"
  input = readFile("day8-input.txt").strip()

let layers = decode_image(input, 25, 6)

var minimum = -1
var product = 0
for _, layer in layers:
  let m = layer.count(0)
  if minimum < 0 or minimum > m:
    minimum = m
    product = layer.count(1) * layer.count(2)
echo "0s:", minimum
echo "res:", product
layers[0].show()


# 0: black
# 1: white
# 2: transparent
proc color_at(idx: int, layers: seq[Layer]): int =
  for _, l in layers:
    let v = l.values[idx]
    if v != 2:
      return v
  return 2

var finalImage = newLayer(25, 6)
var index = 0
while not finalImage.is_full():
  finalImage.add_point(color_at(index, layers))
  index.inc
finalImage.show()
