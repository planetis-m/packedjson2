# To be included by main!
proc main =
  block:
    let data = """{"a":[1,false,{"key":[4,5]},4]}"""
    let x = parseJson(data)
    assert not x.isEmpty
    assert x.atoms.len == 5
    assert kind(x, jRoot) == JObject
    assert get(x, jRoot, "a") == JsonNode 3
    assert hasKey(x, jRoot, "a")
    assert x.nodes[1].kind == opcodeKeyValuePair
    assert x.nodes[1].operand == 12
    assert get(x, JsonNode 6, "key") == JsonNode 9
    assert hasKey(x, JsonNode 6, "key")
    assert not hasKey(x, JsonNode 6, "a")
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
    assert get(x, jRoot, "a", "key") == jNull
    assert get(x, JsonNode 3, 2) == JsonNode 6
    assert get(x, jRoot, "b", "key") == jNull
    assert get(x, JsonNode 3, 2, 1) == jNull
    assert $x == data

  block:
    let data = """{"a":{"key":[4,[1,2,3]]}}"""
    let x = parseJson(data)
    assert not x.isEmpty
    assert x.atoms.len == 6
    assert kind(x, jRoot) == JObject
    assert get(x, jRoot, "a", "key") == JsonNode 6
    assert get(x, JsonNode 6, 1, 2) == JsonNode 11
    for k, v in pairs(x, jRoot):
      assert k == toStr("a")
      assert kind(x, v) == JObject
    assert $x == data

  block:
    let data = """{"a":0,"key":[4,[1,2,3]],"b":{"a":false}}"""
    var x = parseJson(data)
    assert $x == data
    assert not x.isEmpty
    assert x.atoms.len == 8
    assert hasKey(x, jRoot, "a")
    assert hasKey(x, jRoot, "key")
    delete(x, jRoot, "a")
    assert not hasKey(x, jRoot, "a")
    assert hasKey(x, jRoot, "key")
    assert kind(x, jRoot) == JObject
    assert kind(x, JsonNode 3) == JArray

  block:
    let x = %*{
      "a": [1, 2, 3],
      "b": 4,
      "c": [5, 6],
      "d": {"e": [7, 8], "f": 9}
    }
    assert not x.isEmpty
    assert x.atoms.len == 15
    assert $x == """{"a":[1,2,3],"b":4,"c":[5,6],"d":{"e":[7,8],"f":9}}"""

  block:
    type
      Foo = ref object
        a: array[1, Vec3]
        b: bool
        c: string
        d: Bar
      Bar = enum
        foo, bar, baz
      Vec3 = object
        x, y, z: int
    let x = %*Foo(a: [Vec3(x: 1, y: 2, z: 3)], b: true, c: "hi", d: foo)
    assert not x.isEmpty
    assert x.atoms.len == 12
    assert $x == """{"a":[{"x":1,"y":2,"z":3}],"b":true,"c":"hi","d":"foo"}"""

main()
