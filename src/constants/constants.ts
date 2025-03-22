export const NodeTypes = {
  Number: 1,
  String: 2,
  NormalExpression: 3,
  SpecialExpression: 4,
  Symbol: 5,
  Modifier: 6,
  ReservedSymbol: 7,
  Binding: 8,
  Spread: 10,
} as const

const NodeTypesSet = new Set(Object.values(NodeTypes))

export type NodeType = typeof NodeTypes[keyof typeof NodeTypes]

export function getNodeTypeName(type: NodeType): keyof typeof NodeTypes {
  return Object.keys(NodeTypes).find(key => NodeTypes[key as keyof typeof NodeTypes] === type) as keyof typeof NodeTypes
}

// TODO, is this needed?
export function isNodeType(type: unknown): type is NodeType {
  return typeof type === 'number' && NodeTypesSet.has(type as NodeType)
}

const functionTypes = [
  'UserDefined',
  'Partial',
  'Comp',
  'Constantly',
  'Juxt',
  'Complement',
  'EveryPred',
  'SomePred',
  'Fnull',
  'Builtin',
  'NativeJsFunction',
] as const

const functionTypeSet = new Set(functionTypes)

export type FunctionType = typeof functionTypes[number]

export function isFunctionType(type: unknown): type is FunctionType {
  return typeof type === 'string' && functionTypeSet.has(type as FunctionType)
}
