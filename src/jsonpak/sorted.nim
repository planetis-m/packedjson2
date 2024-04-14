import private/[bitabs, jsontree, jsonnode, rawops], std/[algorithm, importutils]

type
  SortedJsonTree* = distinct JsonTree

proc rawSorted(result: var JsonTree; tree: JsonTree, n: NodePos) =
  privateAccess(JsonTree)
  var stack = @[n.PatchPos]
  result.nodes = newSeq[Node](tree.nodes.len)
  result.atoms = BiTable[string]()
  while stack.len > 0:
    let curr = stack.pop().NodePos
    case curr.kind
    of opcodeObject:
      result.nodes.add tree.nodes[curr.int]
      var pairs: seq[(string, PatchPos)] = @[]
      for n in keys(tree, curr):
        pairs.add (n.str, n.PatchPos)
      sort(pairs, proc (a, b: (string, PatchPos)): int = cmp(a[0], b[0]))
      for i in countdown(pairs.high, 0):
        let n = pairs[i][1].NodePos
        stack.add PatchPos(n.firstSon)
        stack.add n.PatchPos
    of opcodeArray:
      result.nodes.add tree.nodes[curr.int]
      var items: seq[PatchPos] = @[]
      for n in sonsReadonly(tree, curr):
        items.add n.PatchPos
      for i in countdown(items.high, 0):
        stack.add items[i]
    of opcodeInt, opcodeFloat, opcodeString:
      result.nodes.add toAtomNode(result, curr.kind, curr.str)
    else:
      result.nodes.add tree.nodes[curr.int]

proc sorted*(tree: JsonTree): SortedJsonTree {.inline.} =
  result = JsonTree().SortedJsonTree
  rawSorted(JsonTree(result), tree, rootNodeId)

proc rawTest(tree, value: JsonTree, n: NodePos): bool =
  privateAccess(JsonTree)
  if n.kind != value.nodes[0].kind: return false
  if n.kind == opcodeNull: return true
  let L = span(tree, n.int)
  if L != value.nodes.len: return false
  for i in 0..<L:
    let n = NodePos(i+n.int) # careful
    case n.kind
    of opcodeInt, opcodeFloat, opcodeString:
      if value.atoms[LitId value.nodes[i].operand] != n.str: return false
    else:
      if value.nodes[i] != tree.nodes[n.int]: return false
  return true

proc `==`*(a, b: SortedJsonTree): bool {.inline.} =
  privateAccess(JsonTree)
  if JsonTree(a).nodes.len != JsonTree(b).nodes.len:
    return false
  rawTest(JsonTree(a), JsonTree(b), rootNodeId)

proc rawDeduplicate(tree: var JsonTree, n: NodePos, parents: var seq[PatchPos]) =
  privateAccess(JsonTree)
  case n.kind
  of opcodeObject:
    parents.add n.PatchPos
    var totaldiff = 0
    var prevKeyId = LitId(0)
    var pos = n.int+1
    var last = len(tree, n)-1
    var count = 0
    while count <= last:
      if prevKeyId == LitId(0) or NodePos(pos).str != tree.atoms[prevKeyId]:
        prevKeyId = NodePos(pos).litId
        rawDeduplicate(tree, NodePos(pos+1), parents)
        inc count
        inc pos
        nextChild tree, pos
      else:
        let oldfull = tree.nodes.len
        let diff = span(tree, pos+1) + 1
        let endpos = pos + diff
        for i in countup(endpos, oldfull-1):
          tree.nodes[i+diff] = tree.nodes[i]
        setLen(tree.nodes, oldfull+diff)
        dec totaldiff, diff
        dec last
    if totaldiff < 0:
      rawUpdateParents(tree, parents, totaldiff)
    discard parents.pop()
  of opcodeArray:
    parents.add n.PatchPos
    var pos = n.int+1
    let last = len(tree, n)-1
    var count = 0
    while count <= last:
      rawDeduplicate(tree, NodePos(pos), parents)
      inc count
      nextChild tree, pos
    discard parents.pop()
  else:
    discard

proc deduplicate*(tree: var SortedJsonTree) =
  var parents: seq[PatchPos] = @[]
  rawDeduplicate(JsonTree(tree), rootNodeId, parents)
