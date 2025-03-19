export const astNodeTypeNames = [
  'Number',
  'String',
  'NormalExpression',
  'SpecialExpression',
  'Symbol',
  'Modifier',
  'ReservedSymbol',
  'Binding',
  'Argument',
  'Partial',
  'Comment',
  'Spread',
] as const

const astNodeTypeSet = new Set(astNodeTypeNames)

export type AstNodeType = typeof astNodeTypeNames[number]

export function isAstNodeType(type: unknown): type is AstNodeType {
  return typeof type === 'string' && astNodeTypeSet.has(type as AstNodeType)
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
