import jsontree, jsonnode, jsonpointer, jsonops, std/[algorithm, sequtils, importutils]

proc test*(tree: JsonTree; path: JsonPtr, value: JsonTree): bool =
  privateAccess(JsonTree)
  let n = findNode(tree, path.string)
  if n.isNil:
    raisePathError(path.string)
  if span(tree, n.int) != value.nodes.len:
    return false
  rawTest(tree, value, n, rootNodeId)

proc replace*(tree: var JsonTree, path: JsonPtr, value: JsonTree) =
  privateAccess(JsonTree)
  # Find the target node
  let res = findNodeMut(tree, path.string)
  if res.node.isNil:
    raisePathError(path.string)
  # Replace the value at the target node
  let diff = span(value, 0) - span(tree, res.node.int)
  rawReplace(tree, value, res.node)
  # Update the operands of the parent nodes
  rawUpdateParents(tree, res.parents, diff)

proc remove*(tree: var JsonTree, path: JsonPtr) =
  privateAccess(JsonTree)
  # Find the target node
  var res = findNodeMut(tree, path.string)
  if res.node.isNil:
    raisePathError(path.string)
  # Remove the target node
  var startPos, diff = 0
  if res.parents.len > 0 and res.parents[^1].NodePos.kind == opcodeObject:
    startPos = res.node.int - 1
    diff = -1 - span(tree, res.node.int)
  else:
    startPos = res.node.int
    diff = -span(tree, res.node.int)
  let endPos = startPos - diff
  tree.nodes.delete(startPos, endPos - 1)
  # Update the operands of the parent nodes
  rawUpdateParents(tree, res.parents, diff)

proc add*(tree: var JsonTree, path: JsonPtr, value: JsonTree) =
  privateAccess(JsonTree)
  # Find the target node
  var res = findNodeMut(tree, path.string)
  var diff = 0
  if res.node.isNil:
    # Add a new node
    let parent = res.parents[^1].NodePos
    let startPos = parent.int + parent.operand
    if parent.kind == opcodeObject:
      # Add a new key-value pair to an object
      diff = 1 + span(value, 0)
      rawAddKeyValuePair(tree, value, NodePos(startPos), res.key)
    else:
      # Add a new element to an array
      diff = span(value, 0)
      rawAdd(tree, value, NodePos(startPos))
  else:
    if res.parents.len > 0 and res.parents[^1].NodePos.kind == opcodeArray:
      # Insert a value before the given index
      diff = span(value, 0)
      rawAdd(tree, value, res.node)
    else:
      # Replace an existing node
      diff = span(value, 0) - span(tree, res.node.int)
      rawReplace(tree, value, res.node)
  rawUpdateParents(tree, res.parents, diff)

proc copy*(tree: var JsonTree, `from`, path: JsonPtr) =
  privateAccess(JsonTree)
  # Find the source node
  let srcNode = findNode(tree, `from`.string)
  if srcNode.isNil:
    raisePathError(`from`.string)
  # Find the target node
  let res = findNodeMut(tree, path.string)
  if res.node == srcNode:
    # Source and destination are the same, no need to copy
    return
  if binarySearch(res.parents, srcNode.PatchPos) >= 0:
    raise newException(PathError, "Invalid operation: `from` is an ancestor of `path`")
  var diff = 0
  if res.node.isNil:
    # Copy to a new node
    let parent = res.parents[^1].NodePos
    let startPos = parent.int + parent.operand
    if parent.kind == opcodeObject:
      # Copy to a new key-value pair in an object
      diff = 1 + span(tree, srcNode.int)
      rawAddKeyValuePair(tree, srcNode, NodePos(startPos), res.key)
    else:
      # Copy to a new element in an array
      diff = span(tree, srcNode.int)
      rawAdd(tree, srcNode, NodePos(startPos))
  else:
    if res.parents.len > 0 and res.parents[^1].NodePos.kind == opcodeArray:
      # Insert the copied value before the given index
      diff = span(tree, srcNode.int)
      rawAdd(tree, srcNode, res.node)
    else:
      # Replace an existing node
      diff = span(tree, srcNode.int) - span(tree, res.node.int)
      rawReplace(tree, srcNode, res.node)
  rawUpdateParents(tree, res.parents, diff)

proc move*(tree: var JsonTree, `from`, path: JsonPtr) =
  privateAccess(JsonTree)
  # Find the source node
  var src = findNodeMut(tree, `from`.string)
  if src.node.isNil:
    raisePathError(`from`.string)
  # Find the target node
  let dest = findNodeMut(tree, path.string)
  if dest.node == src.node:
    # Source and destination are the same, no need to copy
    return
  if binarySearch(dest.parents, src.node.PatchPos) >= 0:
    raise newException(PathError, "Invalid operation: `from` is an ancestor of `path`")
  var startPos, diff = 0
  if dest.node.isNil:
    # Copy to a new node
    let parent = dest.parents[^1].NodePos
    startPos = parent.int + parent.operand
    if parent.kind == opcodeObject:
      # Copy to a new key-value pair in an object
      diff = 1 + span(tree, src.node.int)
      rawAddKeyValuePair(tree, src.node, NodePos(startPos), dest.key)
    else:
      # Copy to a new element in an array
      diff = span(tree, src.node.int)
      rawAdd(tree, src.node, NodePos(startPos))
  else:
    startPos = dest.node.int
    if dest.parents.len > 0 and dest.parents[^1].NodePos.kind == opcodeArray:
      # Insert the copied value before the given index
      diff = span(tree, src.node.int)
      rawAdd(tree, src.node, dest.node)
    else:
      # Replace an existing node
      diff = span(tree, src.node.int) - span(tree, dest.node.int)
      rawReplace(tree, src.node, dest.node)
  rawUpdateParents(tree, dest.parents, diff)
  if binarySearch(src.parents, dest.node.PatchPos) >= 0:
    # In case the source was overwritten by the destination
    return
  for parent in mitems(src.parents):
    if parent >= startPos.PatchPos:
      inc parent, diff
  if src.node >= startPos.NodePos:
    inc src.node, diff
  if src.parents.len > 0 and src.parents[^1].NodePos.kind == opcodeObject:
    startPos = src.node.int - 1
    diff = -1 - span(tree, src.node.int)
  else:
    startPos = src.node.int
    diff = -span(tree, src.node.int)
  let endPos = startPos - diff
  tree.nodes.delete(startPos, endPos - 1)
  # Update the operands of the parent nodes
  rawUpdateParents(tree, src.parents, diff)

when isMainModule:
  import std/assertions, jsonmapper, jsondollar

  proc main =
    var tree = %*{
      "name": "John",
      "age": 30,
      "numbers": [1, 2, 3]
    }

    block: # replace a string value
      var tree = tree
      var newValue = %*"Jane"
      tree.replace(JsonPtr"/name", newValue)
      assert tree == %*{
        "name": "Jane",
        "age": 30,
        "numbers": [1, 2, 3]
      }

    block: # replace an array
      var tree = tree
      var newValue = %*[4, 5]
      tree.replace(JsonPtr"/numbers", newValue)
      assert tree == %*{
        "name": "John",
        "age": 30,
        "numbers": [4, 5]
      }

    block: # replace a non-existing path
      var tree = tree
      var newValue = %*"New York"
      assert:
        try: (tree.replace(JsonPtr"/address", newValue); false)
        except PathError: true

    block: # replace an object
      var tree = tree
      var newValue = %*{"city": "New York"}
      tree.replace(JsonPtr"", newValue)
      assert tree == %*{"city": "New York"}

    tree = %*{
      "a": 1,
      "b": {"c": 2, "d": 3},
      "e": [4, 5, 6],
      "f": nil,
      "g": true
    }

    block:
      var tree = tree
      var newValue = %*7
      tree.replace(JsonPtr"/e/2", newValue)
      assert tree == %*{
        "a": 1,
        "b": {"c": 2, "d": 3},
        "e": [4, 5, 7],
        "f": nil,
        "g": true
      }

    block:
      var tree = tree
      var newValue = %*5
      tree.replace(JsonPtr"/b/c", newValue)
      assert tree == %*{
        "a": 1,
        "b": {"c": 5, "d": 3},
        "e": [4, 5, 6],
        "f": nil,
        "g": true
      }

    block:
      var tree = tree
      var newValue = %*5
      assert:
        try: (tree.replace(JsonPtr"/e/-", newValue); false)
        except PathError: true

    block:
      var tree = tree
      var newValue = %*{"": [10, 11], "g": [12]}
      tree.replace(JsonPtr"/f", newValue)
      assert tree == %*{
        "a": 1,
        "b": {"c": 2, "d": 3},
        "e": [4, 5, 6],
        "f": {"": [10, 11], "g": [12]},
        "g": true
      }

    block:
      var tree = %*{
        "a": 1,
        "b": {"c": 2, "d": 3},
        "e": [4, 5, 6]
      }

      tree.remove(JsonPtr"/b/c")
      assert tree == %*{
        "a": 1,
        "b": {"d": 3},
        "e": [4, 5, 6]
      }

      tree.remove(JsonPtr"/e/1")
      assert tree == %*{
        "a": 1,
        "b": {"d": 3},
        "e": [4, 6]
      }

      tree.remove(JsonPtr"/a")
      assert tree == %*{
        "b": {"d": 3},
        "e": [4, 6]
      }

      assert:
        try: (tree.remove(JsonPtr"/x"); false)
        except PathError: true

      tree.remove(JsonPtr"/e")
      assert tree == %*{
        "b": {"d": 3}
      }

      tree.remove(JsonPtr"")
      assert tree.isEmpty

    block: # add
      var tree = %*{
        "a": 1,
        "b": {"c": 2},
        "d": [3, 4]
      }

      var value1 = %*{"f": 5}
      tree.add(JsonPtr"/b/e", value1)
      assert tree == %*{
        "a": 1,
        "b": {"c": 2, "e": {"f": 5}},
        "d": [3, 4]
      }

      tree = %*{
        "a": {"x": 24, "y": 25},
        "b": {"c": 3, "d": 4},
        "arr": [1, 2, 3, 4],
        "str": "hello"
      }

      block: # test
        assert tree.test(JsonPtr"/a", %*{"x": 24, "y": 25}) == true
        assert tree.test(JsonPtr"/b", %*{"c": 3, "d": 5}) == false
        assert tree.test(JsonPtr"/arr", %*[1, 2, 3, 4]) == true
        assert tree.test(JsonPtr"/str", %*"hello") == true

      block: # replace existing node
        let newValue = %*{"x": 100, "y": 200}
        tree.add(JsonPtr"/a", newValue)
        assert tree.test(JsonPtr"/a", newValue) == true

      block: # add new key-value pair to object
        let newValue = %*{"e": 5}
        tree.add(JsonPtr"/b", newValue)
        assert tree.test(JsonPtr"/b", %*{"e": 5}) == true

      block: # add new element to array
        let newValue = %*5
        tree.add(JsonPtr"/arr/2", newValue)
        assert tree.test(JsonPtr"/arr", %*[1, 2, 5, 3, 4]) == true

      block: # add new element to the end of array
        let newValue = %*5
        tree.add(JsonPtr"/arr/-", newValue)
        assert tree.test(JsonPtr"/arr", %*[1, 2, 5, 3, 4, 5]) == true

      block: # add new node to root
        let newValue = %*{"new": "value"}
        tree.add(JsonPtr"", newValue)
        assert tree.test(JsonPtr"/new", %*"value") == true

      tree = %*{
        "a": {"x": 24, "y": 25},
        "b": {"c": 3, "d": 4, "e": 5},
        "arr": [1, 2, 3, 4],
        "str": "hello"
      }

      block: # copy existing node to a new location
        var tree = tree
        tree.copy(JsonPtr"/a", JsonPtr"/copied")
        assert tree.test(JsonPtr"/copied", %*{"x": 24, "y": 25}) == true

      block: # copy existing node to replace another node
        var tree = tree
        tree.copy(JsonPtr"/a", JsonPtr"/b")
        assert tree.test(JsonPtr"/b", %*{"x": 24, "y": 25}) == true

      block: # copy existing node to a new element in an array
        var tree = tree
        tree.copy(JsonPtr"/a", JsonPtr"/arr/2")
        assert tree.test(JsonPtr"/arr", %*[1, 2, {"x": 24, "y": 25}, 3, 4]) == true

      block: # copy existing node to the end of an array
        var tree = tree
        tree.copy(JsonPtr"/a", JsonPtr"/arr/-")
        assert tree.test(JsonPtr"/arr", %*[1, 2, 3, 4, {"x": 24, "y": 25}]) == true

      block: # copy existing node to the root
        var tree = tree
        tree.copy(JsonPtr"/a", JsonPtr"")
        assert tree.test(JsonPtr"", %*{"x": 24, "y": 25}) == true

      block: # copy a child node to its parent
        var tree = tree
        tree.copy(JsonPtr"/a/x", JsonPtr"/a")
        assert tree.test(JsonPtr"/a", %*24) == true

      block: # copy a node to itself
        var tree = tree
        tree.copy(JsonPtr"/a", JsonPtr"/a")
        assert tree.test(JsonPtr"/a", %*{"x": 24, "y": 25}) == true

      block: # copy array element to a new location
        var tree = tree
        tree.copy(JsonPtr"/arr/0", JsonPtr"/copied_element")
        assert tree.test(JsonPtr"/copied_element", %*1) == true

      block: # copy array element to replace another element
        var tree = tree
        tree.copy(JsonPtr"/arr/0", JsonPtr"/arr/1")
        assert tree.test(JsonPtr"/arr", %*[1, 1, 2, 3, 4]) == true

      block: # copy non-existing node (should raise PathError)
        try:
          tree.copy(JsonPtr"/non_existing", JsonPtr"/copied")
          assert false, "Expected PathError"
        except PathError:
          assert true

      block: # copy parent to child (should raise PathError)
        try:
          tree.copy(JsonPtr"/a", JsonPtr"/a/x")
          assert false, "Expected PathError"
        except PathError:
          assert true

      block: # move existing node to the root
        var tree = tree
        tree.move(JsonPtr"/a", JsonPtr"")
        assert tree.test(JsonPtr"", %*{"x": 24, "y": 25}) == true

      block: # move a child node to its parent
        var tree = tree
        tree.move(JsonPtr"/a/x", JsonPtr"/a")
        assert tree.test(JsonPtr"", %*{"a":24,"b":{"c":3,"d":4,"e":5},"arr":[1,2,3,4],"str":"hello"})

      block: # move array element to a new location
        var tree = tree
        tree.move(JsonPtr"/arr/0", JsonPtr"/copied_element")
        assert tree.test(JsonPtr"",
          %*{"a":{"x":24,"y":25},"b":{"c":3,"d":4,"e":5},"arr":[2,3,4],"str":"hello","copied_element":1})

      block: # move existing node to the end of an array
        var tree = tree
        tree.move(JsonPtr"/a", JsonPtr"/arr/-")
        assert tree.test(JsonPtr"",
          %*{"b":{"c":3,"d":4,"e":5},"arr":[1,2,3,4,{"x":24,"y":25}],"str":"hello"})

      block: # move existing node to replace another node
        var tree = tree
        tree.move(JsonPtr"/b", JsonPtr"/a/x")
        assert tree.test(JsonPtr"",
          %*{"a":{"x":{"c":3,"d":4,"e":5},"y":25},"arr":[1,2,3,4],"str":"hello"})

  static: main()
  main()
