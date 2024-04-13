## Provides procedures for deserializing JSON data into Nim data types.

import private/[jsonnode, jsontree, rawops], jsonptr, std/[macros, tables, options, strutils]
from std/parsejson import JsonKindError
export JsonKindError

proc raiseJsonKindError*(kind: JsonNodeKind, kinds: set[JsonNodeKind]) {.noinline.} =
  let msg = format("Incorrect JSON kind. Wanted '$1' but got '$2'.", kinds, kind)
  raise newException(JsonKindError, msg)

template verifyJsonKind*(tree: JsonTree; n: NodePos, kinds: set[JsonNodeKind]) =
  let kind = JsonNodeKind(n.kind)
  if kind notin kinds:
    raiseJsonKindError(kind, kinds)

proc initFromJson*(dst: var string; tree: JsonTree; n: NodePos) =
  verifyJsonKind(tree, n, {JString, JNull})
  if n.kind == opcodeNull:
    dst = ""
  else:
    if n.isShort:
      dst = n.shortStr
    else:
      dst = n.str

proc initFromJson*(dst: var bool; tree: JsonTree; n: NodePos) =
  verifyJsonKind(tree, n, {JBool})
  dst = n.bval

proc initFromJson*(dst: var JsonTree; tree: JsonTree; n: NodePos) =
  rawExtract(dst, tree, n)

proc initFromJson*[T: SomeInteger](dst: var T; tree: JsonTree; n: NodePos) =
  verifyJsonKind(tree, n, {JInt})
  if n.isShort:
    dst = cast[T](n.operand)
  else:
    when T is BiggestUInt:
      dst = parseBiggestUInt n.str
    elif T is BiggestInt:
      dst = parseBiggestInt n.str
    elif T is SomeSignedInt:
      dst = T(parseInt n.str)
    else:
      dst = T(parseUInt n.str)

proc initFromJson*[T: SomeFloat](dst: var T; tree: JsonTree; n: NodePos) =
  verifyJsonKind(tree, n, {JInt, JFloat})
  if n.kind == opcodeFloat:
    if n.isShort:
      dst = T(parseFloat n.shortStr)
    else:
      dst = T(parseFloat n.str)
  else:
    if n.isShort:
      dst = T(cast[int64](n.operand))
    else:
      dst = T(parseBiggestInt n.str)

proc initFromJson*[T: enum](dst: var T; tree: JsonTree; n: NodePos) =
  verifyJsonKind(tree, n, {JString})
  if n.isShort:
    dst = parseEnum[T](n.shortStr)
  else:
    dst = parseEnum[T](n.str)

proc initFromJson*[T](dst: var seq[T]; tree: JsonTree; n: NodePos) =
  verifyJsonKind(tree, n, {JArray})
  dst.setLen len(tree, n)
  var i = 0
  for x in sonsReadonly(tree, n):
    initFromJson(dst[i], tree, x)
    inc i

proc initFromJson*[S, T](dst: var array[S, T]; tree: JsonTree; n: NodePos) =
  verifyJsonKind(tree, n, {JArray})
  var i = int(low(dst))
  for x in sonsReadonly(tree, n):
    initFromJson(dst[S(i)], tree, x)
    inc i

proc initFromJson*[T](dst: var (Table[string, T]|OrderedTable[string, T]); tree: JsonTree; n: NodePos) =
  verifyJsonKind(tree, n, {JObject})
  for x in keys(tree, n):
    if x.isShort:
      initFromJson(mgetOrPut(dst, x.shortStr, default(T)), tree, x.firstSon)
    else:
      initFromJson(mgetOrPut(dst, x.str, default(T)), tree, x.firstSon)

proc initFromJson*[T](dst: var ref T; tree: JsonTree; n: NodePos) =
  verifyJsonKind(tree, n, {JObject, JNull})
  if n.kind == opcodeNull:
    dst = nil
  else:
    dst = new(T)
    initFromJson(dst[], tree, n)

proc initFromJson*[T](dst: var Option[T]; tree: JsonTree; n: NodePos) =
  if not n.isNil and n.kind != opcodeNull:
    when T is ref:
      dst = some(new(T))
    else:
      dst = some(default(T))
    initFromJson(dst.get, tree, n)

proc initFromJson*[T: object|tuple](dst: var T; tree: JsonTree; n: NodePos) =
  verifyJsonKind(tree, n, {JObject})
  for x in keys(tree, n):
    for k, v in dst.fieldPairs:
      if x.str == k:
        initFromJson(v, tree, x.firstSon)
        break # emulate elif

proc fromJson*[T](tree: JsonTree; path: JsonPtr; t: typedesc[T]): T =
  let n = findNode(tree, path.string)
  if n.isNil:
    raisePathError(path.string)
  result = default(T)
  initFromJson(result, tree, n)

iterator items*[T](tree: JsonTree; path: JsonPtr; t: typedesc[T]): T =
  ## Iterator for the items of `x`. `x` has to be a JArray.
  let n = findNode(tree, path.string)
  if n.isNil:
    raisePathError(path.string)
  assert n.kind == opcodeArray
  var item = default(T)
  for x in sonsReadonly(tree, n):
    initFromJson(item, tree, x)
    yield item

iterator pairs*[T](tree: JsonTree; path: JsonPtr; t: typedesc[T]): (lent string, T) =
  ## Iterator for the pairs of `x`. `x` has to be a JObject.
  let n = findNode(tree, path.string)
  if n.isNil:
    raisePathError(path.string)
  assert n.kind == opcodeObject
  var item = default(T)
  var buf = newString(payloadBits div 8)
  for x in keys(tree, n):
    initFromJson(item, tree, x.firstSon)
    if x.isShort:
      for i in 0 ..< buf.len:
        buf[i] = chr(n.operand shr (i * 8) and 0xFF)
      yield (buf, item)
    else:
      yield (x.str, item)
