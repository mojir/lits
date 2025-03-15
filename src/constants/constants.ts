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
] as const

const astNodeTypeSet = new Set(astNodeTypeNames)

export type AstNodeType = typeof astNodeTypeNames[number]

export function isAstNodeType(type: unknown): type is AstNodeType {
  return typeof type === 'string' && astNodeTypeSet.has(type as AstNodeType)
}

export enum FunctionType {
  UserDefined = 301,
  Partial = 302,
  Comp = 303,
  Constantly = 304,
  Juxt = 305,
  Complement = 306,
  EveryPred = 307,
  SomePred = 308,
  Fnull = 309,
  Builtin = 310,
  NativeJsFunction = 399,
}

export const functionTypeName = new Map([
  [FunctionType.UserDefined, 'UserDefined'],
  [FunctionType.Partial, 'Partial'],
  [FunctionType.Comp, 'Comp'],
  [FunctionType.Constantly, 'Constantly'],
  [FunctionType.Juxt, 'Juxt'],
  [FunctionType.Complement, 'Complement'],
  [FunctionType.EveryPred, 'EveryPred'],
  [FunctionType.SomePred, 'SomePred'],
  [FunctionType.Fnull, 'Fnull'],
  [FunctionType.Builtin, 'Builtin'],
])

export function isFunctionType(type: unknown): type is FunctionType {
  return typeof type === 'number' && functionTypeName.has(type)
}
