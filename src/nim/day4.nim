import tables

const
  low = 172851
  high = 675869

proc int_to_seq(n: int): seq[int] =
  var s: seq[int]
  var n = n
  while n > 0:
    s.add(n mod 10)
    n = n div 10
  s

proc decreasing(s: seq[int]): bool =
  var last = s[0]
  for n in s:
    if n > last:
      return false
    last = n
  true

proc duplicate(s: seq[int], exactly_two: bool = false): bool =
  var t: Table[int, int]
  for n in s:
    if not t.hasKey(n):
      t[n] = 0
    t[n] += 1
  for v in t.values():
    if (exactly_two and v == 2) or (not exactly_two and v >= 2): return true
  false

proc check_number(n: int, exactly_two: bool = false ): bool =
  var s = int_to_seq(n)
  decreasing(s) and duplicate(s, exactly_two)

var total: int
for i in low .. high:
  if check_number(i):
    total += 1

echo total

var total2: int
for i in low .. high:
  if check_number(i, true):
    total2 += 1

echo total2
