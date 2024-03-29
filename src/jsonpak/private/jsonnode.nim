type
  Node* = distinct int32
  JsonNodeKind* = enum ## possible JSON node types
    JNull,
    JBool,
    JInt,
    JFloat,
    JString,
    JObject,
    JArray

const
  opcodeBits = 3

  opcodeNull* = ord JNull
  opcodeBool* = ord JBool
  opcodeFalse* = opcodeBool
  opcodeTrue* = opcodeBool or 0b0000_1000
  opcodeInt* = ord JInt
  opcodeFloat* = ord JFloat
  opcodeString* = ord JString
  opcodeObject* = ord JObject
  opcodeArray* = ord JArray

  opcodeMask = 0b111

template kind*(n: Node): int32 = n.int32 and opcodeMask
template operand*(n: Node): int32 = int32(n.uint32 shr opcodeBits.int32)

template toNode*(kind, operand: int32): Node =
  Node(operand shl opcodeBits.int32 or kind)

proc `==`*(a, b: Node): bool {.borrow.}
