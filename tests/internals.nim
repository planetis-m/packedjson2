# To be included by main!
proc main =
  block:
    let data = """{"a":[1,false,{"key":[4,5]},4]}"""
    let x = parseJson(data)
    assert not x.isEmpty
    assert x.atoms.len == 5
    assert kind(x, JsonPtr"") == JObject
    assert posFromPtr(x, JsonPtr"") == rootNodeId
    assert posFromPtr(x, JsonPtr"/a") == NodePos 3
    assert posFromPtr(x, JsonPtr"/a/4") == nilNodeId
    assert posFromPtr(x, JsonPtr"/a/4/key") == nilNodeId
    assert contains(x, JsonPtr"/a")
    assert x.nodes[1].kind == opcodeKeyValuePair
    assert x.nodes[1].operand == 12
    assert contains(x, JsonPtr"/a/2/key")
    assert not contains(x, JsonPtr"/a/2/a")
    assert x.nodes[7].kind == opcodeKeyValuePair
    assert x.nodes[7].operand == 5
    assert posFromPtr(x, JsonPtr"/a/2/key") == NodePos 9
    assert kind(x, JsonPtr"/a/2/key") == JArray
    assert posFromPtr(x, JsonPtr"/a/1") == NodePos 5
    assert kind(x, JsonPtr"/a/1") == JBool
    assert fromJson(x, JsonPtr"/a/1", bool) == false
    assert posFromPtr(x, JsonPtr"/a/0") == NodePos 4
    assert kind(x, JsonPtr"/a/0") == JInt
    assert fromJson(x, JsonPtr"/a/0", int) == 1
    assert posFromPtr(x, JsonPtr"/a/2/key/1") == NodePos 11
    assert kind(x, JsonPtr"/a/2/key/1") == JInt
    assert fromJson(x, JsonPtr"/a/2/key/1", int) == 5
    #assert posFromPtr(x, JsonPtr"", NodePos 3) == NodePos 3
    #assert posFromPtr(x, JsonPtr"/2", NodePos 3) == NodePos 6
    #assert posFromPtr(x, JsonPtr"/-", NodePos 3, noDash = false) == NodePos 13
    assert $x == data

  block:
    let data = """{"a":{"key":[4,[1,2,3]]}}"""
    let x = parseJson(data)
    assert not x.isEmpty
    assert x.atoms.len == 6
    assert kind(x, JsonPtr"") == JObject
    assert posFromPtr(x, JsonPtr"/a/key") == NodePos 6
    #assert posFromPtr(x, JsonPtr"/1/2", NodePos 6) == NodePos 11
    assert $x == data

static: main()
main()
