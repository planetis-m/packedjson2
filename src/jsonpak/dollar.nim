from std/json import escapeJsonUnquoted, escapeJson
import private/[bitabs, jsonnode, jsontree], std/importutils

type
  JsonIter = object
    stack: seq[(int32, int32)]
    tos: NodePos
    tosEnd: int
    pos: int

template tosEnd(n: NodePos): int = n.int + n.operand

proc initJsonIter(tree: JsonTree, n: NodePos): JsonIter =
  result = JsonIter(stack: @[], tos: n, tosEnd: n.tosEnd, pos: n.int+1)

proc pushImpl(it: var JsonIter, tree: JsonTree, n: NodePos) =
  it.stack.add (int32 it.tos, int32 it.pos)
  it.tos = n
  it.tosEnd = n.tosEnd
  it.pos = n.int+1

template push(it: JsonIter, n: NodePos) = pushImpl(it, tree, n)

type
  Action = enum
    actionElem, actionKeyVal, actionPop, actionEnd

proc currentAndNext(it: var JsonIter, tree: JsonTree): (NodePos, LitId, Action) =
  if it.pos < it.tosEnd:
    if it.tos.kind == opcodeArray:
      result = (NodePos it.pos, LitId(0), actionElem)
    else:
      let litId = (NodePos it.pos).litId
      result = (firstSon(NodePos it.pos), litId, actionKeyVal)
      inc it.pos
    nextChild tree, it.pos
  elif it.stack.len > 0:
    result = (it.tos, LitId(0), actionPop)
    let tmp = it.stack.pop()
    it.tos = tmp[0].NodePos
    it.pos = tmp[1]
    it.tosEnd = it.tos.tosEnd
  else:
    result = (nilNodeId, LitId(0), actionEnd)

proc toUgly*(result: var string, tree: JsonTree, n: NodePos) =
  privateAccess(JsonTree)
  template key: string = tree.atoms[keyId]
  case n.kind
  of opcodeArray, opcodeObject:
    if n.kind == opcodeArray:
      result.add "["
    else:
      result.add "{"
    var it = initJsonIter(tree, n)
    var pendingComma = false
    while true:
      let (child, keyId, action) = currentAndNext(it, tree)
      case action
      of actionPop:
        if child.kind == opcodeArray:
          result.add "]"
        else:
          result.add "}"
        pendingComma = true
      of actionEnd: break
      of actionElem, actionKeyVal:
        if pendingComma:
          result.add ","
          pendingComma = false
        if action == actionKeyVal:
          key.escapeJson(result)
          result.add ":"
        case child.kind
        of opcodeArray:
          result.add "["
          it.push child
          pendingComma = false
        of opcodeObject:
          result.add "{"
          it.push child
          pendingComma = false
        of opcodeInt, opcodeFloat:
          result.add child.str
          pendingComma = true
        of opcodeString:
          escapeJson(child.str, result)
          pendingComma = true
        of opcodeBool:
          result.add(if child.bval: "true" else: "false")
          pendingComma = true
        of opcodeNull:
          result.add "null"
          pendingComma = true
        else: discard
    if n.kind == opcodeArray:
      result.add "]"
    else:
      result.add "}"
  of opcodeString:
    escapeJson(n.str, result)
  of opcodeInt, opcodeFloat:
    result.add n.str
  of opcodeBool:
    result.add(if n.bval: "true" else: "false")
  of opcodeNull:
    result.add "null"
  else: discard

proc `$`*(tree: JsonTree): string =
  ## Converts `tree` to its JSON Representation on one line.
  privateAccess(JsonTree)
  result = newStringOfCap(tree.nodes.len shl 1)
  toUgly(result, tree, rootNodeId)
