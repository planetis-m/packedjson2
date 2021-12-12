import
  packedjson2 / bitabs,
  std / [parsejson, streams, strutils]
export JsonParsingError, JsonKindError

type
  JsonNode* = distinct int32

  Node = distinct int32
  NodePos = distinct int
  JsonNodeKind* = enum ## possible JSON node types
    JNull,
    JBool,
    JInt,
    JFloat,
    JString,
    JObject,
    JArray

const
  jsRoot* = JsonNode(0)  ## Each `JsonTree` starts from this index.
  jsNull* = JsonNode(-1) ## Null `JsonNode`

proc `==`*(a, b: JsonNode): bool {.borrow.}
proc `<`*(a, b: JsonNode): bool {.borrow.}
proc `<=`*(a, b: JsonNode): bool {.borrow.}

const
  opcodeBits = 3

  opcodeNull = ord JNull
  opcodeBool = ord JBool
  opcodeFalse = opcodeBool
  opcodeTrue = opcodeBool or 0b0000_1000
  opcodeInt = ord JInt
  opcodeFloat = ord JFloat
  opcodeString = ord JString
  opcodeObject = ord JObject
  opcodeArray = ord JArray
  opcodeKeyValuePair = 7

  opcodeMask = 0b111

template kind(n: Node): int32 = n.int32 and opcodeMask
template operand(n: Node): int32 = n.int32 shr opcodeBits.int32
template toNode(kind, operand: int32): Node = Node(operand shl opcodeBits.int32 or kind)

type
  JsonTree* = object
    nodes: seq[Node]
    atoms: BiTable[string]

proc isAtom(tree: JsonTree; pos: int): bool {.inline.} =
  tree.nodes[pos].kind <= opcodeString

proc nextChild(tree: JsonTree; pos: var int) {.inline.} =
  if tree.nodes[pos].kind > opcodeString:
    assert tree.nodes[pos].operand > 0
    inc pos, tree.nodes[pos].operand
  else:
    inc pos

proc kind*(tree: JsonTree; n: JsonNode): JsonNodeKind {.inline.} =
  JsonNodeKind tree.nodes[n.int].kind

iterator sonsReadonly(tree: JsonTree; n: NodePos): NodePos =
  var pos = n.int
  assert tree.nodes[pos].kind > opcodeString
  let last = pos + tree.nodes[pos].operand
  inc pos
  while pos < last:
    yield NodePos(pos)
    nextChild tree, pos

proc parentImpl(tree: JsonTree; n: NodePos): NodePos =
  # finding the parent of a node is rather easy:
  var pos = n.int - 1
  while pos >= 0 and (isAtom(tree, pos) or (pos + tree.nodes[pos].operand - 1 < n.int)):
    dec pos
  #assert pos >= 0, "node has no parent"
  result = NodePos(pos)

template parent(n: NodePos): NodePos = parentImpl(tree, n)

proc firstSon(n: NodePos): NodePos {.inline.} = NodePos(n.int+1)

template kind(n: NodePos): int32 = tree.nodes[n.int].kind
template litId(n: NodePos): LitId = LitId tree.nodes[n.int].operand

template operand(n: NodePos): int32 = tree.nodes[n.int].operand

proc rawGet(tree: JsonTree, n: JsonNode, name: string): JsonNode =
  assert kind(tree, n) == JObject
  let litId = tree.atoms.getKeyId(name)
  if litId == LitId(0):
    return jsNull
  for ch0 in sonsReadonly(tree, NodePos n):
    assert ch0.kind == opcodeKeyValuePair
    if ch0.firstSon.litId == litId:
      return JsonNode(ch0.int+2) # guaranteed that firstSon isAtom

proc isNil*(n: JsonNode): bool {.inline.} = n < jsRoot

proc raiseKeyError(name: string) {.noinline, noreturn.} =
  raise newException(KeyError, "key not found in object: " & name)

proc get*(tree: JsonTree, n: JsonNode, name: string): JsonNode =
  ## Gets a field from a `JObject`.
  ## If the value at `name` does not exist, raises KeyError.
  result = rawGet(tree, n, name)
  if result.isNil:
    raiseKeyError(name)

proc raiseIndexDefect() {.noinline, noreturn.} =
  raise newException(IndexDefect, "index out of bounds")

proc get*(tree: JsonTree, n: JsonNode, index: int): JsonNode =
  ## Gets the node at `index` in an Array. Result is undefined if `index`
  ## is out of bounds, but as long as array bound checks are enabled it will
  ## result in an exception.
  assert kind(tree, n) == JArray
  var i = index
  for ch0 in sonsReadonly(tree, NodePos n):
    if i == 0: return JsonNode ch0
    dec i
  raiseIndexDefect()

proc contains*(tree: JsonTree, n: JsonNode, key: string): bool =
  ## Checks if `key` exists in `n`.
  let x = rawGet(tree, n, key)
  result = x >= jsRoot

proc hasKey*(tree: JsonTree, n: JsonNode, key: string): bool =
  ## Checks if `key` exists in `n`.
  result = contains(tree, n, key)

proc get*(tree: JsonTree, n: JsonNode, keys: varargs[string]): JsonNode =
  ## Traverses the tree and gets the given value. If any of the
  ## keys do not exist, returns ``JNull``. Also returns ``JNull`` if one of the
  ## intermediate data structures is not an object.
  result = n
  for kk in keys:
    if kind(tree, result) != JObject: return jsNull
    block searchLoop:
      let litId = tree.atoms.getKeyId(kk)
      if litId == LitId(0):
        return jsNull
      for ch0 in sonsReadonly(tree, NodePos result):
        assert ch0.kind == opcodeKeyValuePair
        if ch0.firstSon.litId == litId:
          result = JsonNode(ch0.int+2) # guaranteed that firstSon isAtom
          break searchLoop
      return jsNull

proc get*(tree: JsonTree, n: JsonNode, indexes: varargs[int]): JsonNode =
  ## Traverses the tree and gets the given value. If any of the
  ## indexes do not exist, returns ``JNull``. Also returns ``JNull`` if one of the
  ## intermediate data structures is not an array.
  result = n
  for j in indexes:
    if kind(tree, result) != JArray: return jsNull
    echo NodePos(n).operand
    block searchLoop:
      var i = j
      for ch0 in sonsReadonly(tree, NodePos result):
        if i == 0:
          result = JsonNode ch0
          break searchLoop
        dec i
      return jsNull

proc getStr*(tree: JsonTree, n: JsonNode, default: string = ""): string =
  ## Retrieves the string value of a `JString`.
  ##
  ## Returns `default` if `x` is not a `JString`.
  if n.isNil or kind(tree, n) != JString: result = default
  else: result = tree.atoms[NodePos(n).litId]

proc getInt*(tree: JsonTree, n: JsonNode, default: int = 0): int =
  ## Retrieves the int value of a `JInt`.
  ##
  ## Returns `default` if `x` is not a `JInt`, or if `x` is nil.
  if n.isNil or kind(tree, n) != JInt: result = default
  else: result = parseInt tree.atoms[NodePos(n).litId]

proc getBiggestInt*(tree: JsonTree, n: JsonNode, default: BiggestInt = 0): BiggestInt =
  ## Retrieves the BiggestInt value of a `JInt`.
  ##
  ## Returns `default` if `x` is not a `JInt`, or if `x` is nil.
  if n.isNil or kind(tree, n) != JInt: result = default
  else: result = parseBiggestInt tree.atoms[NodePos(n).litId]

proc getFloat*(tree: JsonTree, n: JsonNode, default: float = 0.0): float =
  ## Retrieves the float value of a `JFloat`.
  ##
  ## Returns `default` if `x` is not a `JFloat` or `JInt`, or if `x` is nil.
  if n.isNil: return default
  case kind(tree, n)
  of JFloat:
    result = parseFloat tree.atoms[NodePos(n).litId]
  of JInt:
    result = float(parseBiggestInt tree.atoms[NodePos(n).litId])
  else:
    result = default

proc getBool*(tree: JsonTree, n: JsonNode, default: bool = false): bool =
  ## Retrieves the bool value of a `JBool`.
  ##
  ## Returns `default` if `n` is not a `JBool`, or if `n` is nil.
  if n.isNil or kind(tree, n) != JBool: result = default
  else: result = NodePos(n).operand == 1

type
  PatchPos = distinct int32

proc prepare(tree: var JsonTree; kind: int32): PatchPos =
  result = PatchPos tree.nodes.len
  tree.nodes.add Node kind

proc patch(tree: var JsonTree; pos: PatchPos) =
  let pos = pos.int
  assert tree.nodes[pos].kind > opcodeString
  let distance = int32(tree.nodes.len - pos)
  tree.nodes[pos] = toNode(tree.nodes[pos].int32, distance)

proc parseJson(tree: var JsonTree; p: var JsonParser) =
  case p.tok
  of tkString:
    tree.nodes.add toNode(opcodeString, int32 getOrIncl(tree.atoms, p.a))
    discard getTok(p)
  of tkInt:
    tree.nodes.add toNode(opcodeInt, int32 getOrIncl(tree.atoms, p.a))
    discard getTok(p)
  of tkFloat:
    tree.nodes.add toNode(opcodeFloat, int32 getOrIncl(tree.atoms, p.a))
    discard getTok(p)
  of tkTrue:
    tree.nodes.add Node opcodeTrue
    discard getTok(p)
  of tkFalse:
    tree.nodes.add Node opcodeFalse
    discard getTok(p)
  of tkNull:
    tree.nodes.add Node opcodeNull
    discard getTok(p)
  of tkCurlyLe, tkBracketLe:
    var insertPos: seq[PatchPos] = @[]
    while true:
      if insertPos.len > 0 and
          kind(NodePos insertPos[^1]) == opcodeObject and p.tok != tkCurlyRi:
        if p.tok != tkString:
          raiseParseErr(p, "string literal as key")
        else:
          let patchPos = tree.prepare(opcodeKeyValuePair)
          tree.nodes.add toNode(opcodeString, int32 getOrIncl(tree.atoms, p.a))
          insertPos.add patchPos
          discard getTok(p)
          eat(p, tkColon)

      template putVal() =
        if insertPos.len > 0:
          if kind(NodePos insertPos[^1]) == opcodeKeyValuePair:
            tree.patch insertPos.pop()

      case p.tok
      of tkString, tkInt, tkFloat, tkTrue, tkFalse, tkNull:
        # this recursion for atoms is fine and could easily be avoided
        # since it deals with atoms only.
        parseJson(tree, p)
        putVal()
        if p.tok == tkComma:
          discard getTok(p)
      of tkCurlyLe:
        insertPos.add tree.prepare(opcodeObject)
        discard getTok(p)
      of tkBracketLe:
        insertPos.add tree.prepare(opcodeArray)
        discard getTok(p)
      of tkCurlyRi:
        if insertPos.len > 0 and kind(NodePos insertPos[^1]) == opcodeObject:
          tree.patch insertPos.pop()
          putVal()
          discard getTok(p)
          if insertPos.len == 0: break
        else:
          raiseParseErr(p, "{")
        if p.tok == tkComma:
          discard getTok(p)
      of tkBracketRi:
        if insertPos.len > 0 and kind(NodePos insertPos[^1]) == opcodeArray:
          tree.patch insertPos.pop()
          putVal()
          discard getTok(p)
          if insertPos.len == 0: break
        else:
          raiseParseErr(p, "{")
        if p.tok == tkComma:
          discard getTok(p)
      else:
        raiseParseErr(p, "{")
  of tkError, tkCurlyRi, tkBracketRi, tkColon, tkComma, tkEof:
    raiseParseErr(p, "{")

proc parseJson*(s: Stream, filename: string = ""): JsonTree =
  ## Parses from a stream `s` into a `JsonNode`. `filename` is only needed
  ## for nice error messages.
  ## If `s` contains extra data, it will raise `JsonParsingError`.
  var p: JsonParser
  open(p, s, filename)
  try:
    discard getTok(p)
    parseJson(result, p)
    eat(p, tkEof)
  finally:
    close(p)

proc parseJson*(buffer: string): JsonTree =
  ## Parses JSON from `buffer`.
  ## If `buffer` contains extra data, it will raise `JsonParsingError`.
  parseJson(newStringStream(buffer), "input")

proc parseFile*(filename: string): JsonTree =
  ## Parses `file` into a `JsonNode`.
  ## If `file` contains extra data, it will raise `JsonParsingError`.
  var stream = newFileStream(filename, fmRead)
  if stream == nil:
    raise newException(IOError, "cannot read from file: " & filename)
  result = parseJson(stream, filename)

when isMainModule:
  block:
    let data = """{"a": [1, false, {"key": [4, 5]}, 4]}"""
    let x = parseJson(data)
    assert x.atoms.len == 5
    assert kind(x, jsRoot) == JObject
    assert get(x, jsRoot, "a") == JsonNode 3
    assert hasKey(x, jsRoot, "a")
    assert x.nodes[1].kind == opcodeKeyValuePair
    assert x.nodes[1].operand == 12
    assert get(x, JsonNode 6, "key") == JsonNode 9
    assert hasKey(x, JsonNode 6, "key")
    assert x.nodes[7].kind == opcodeKeyValuePair
    assert x.nodes[7].operand == 5
    assert kind(x, JsonNode 9) == JArray
    assert get(x, JsonNode 9, 1) == JsonNode 11
    assert kind(x, JsonNode 5) == JBool
    assert getBool(x, JsonNode 5) == false
    assert kind(x, JsonNode 4) == JInt
    assert getInt(x, JsonNode 4) == 1
    assert kind(x, JsonNode 11) == JInt
    assert getInt(x, JsonNode 11) == 5
    assert get(x, jsRoot, "a", "key") == jsNull
    assert get(x, JsonNode 3, 2) == JsonNode 6
  block:
    let data = """{"a": {"key": [4, [1, 2, 3]]}}"""
    let x = parseJson(data)
    assert x.atoms.len == 6
    assert kind(x, jsRoot) == JObject
    assert get(x, jsRoot, "a", "key") == JsonNode 6
    assert get(x, JsonNode 6, 1, 2) == JsonNode 11
