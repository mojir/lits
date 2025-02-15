export enum AstNodeType {
  Number = 201,
  String = 202,
  NormalExpression = 203,
  SpecialExpression = 204,
  Name = 205,
  Modifier = 206,
  ReservedName = 207,
  Binding = 208,
  Argument = 209,
  Partial = 210,
  Comment = 211,
}

export const astNodeTypeName = new Map([
  [AstNodeType.Number, 'Number'],
  [AstNodeType.String, 'String'],
  [AstNodeType.NormalExpression, 'NormalExpression'],
  [AstNodeType.SpecialExpression, 'SpecialExpression'],
  [AstNodeType.Name, 'Name'],
  [AstNodeType.Modifier, 'Modifier'],
  [AstNodeType.ReservedName, 'ReservedName'],
  [AstNodeType.Binding, 'Binding'],
  [AstNodeType.Argument, 'Argument'],
  [AstNodeType.Partial, 'Partial'],
])

export function isAstNodeType(type: unknown): type is AstNodeType {
  return typeof type === 'number' && astNodeTypeName.has(type)
}

export const tokenTypes = [
  'LParen',
  'RParen',
  'LBrace',
  'RBrace',
  'LBracket',
  'RBracket',
  'Number',
  'Name',
  'String',
  'ReservedName',
  'Modifier',
  'RegexpShorthand',
  'FnShorthand',
  'CollectionAccessor',
  'Comment',
  'NewLine',
  'Infix',
  'Postfix',
  'InfixOperator',
] as const

export type TokenType = typeof tokenTypes[number]

export function isTokenType(type: string): type is TokenType {
  return typeof type === 'string' && tokenTypes.includes(type as TokenType)
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
  Fnil = 309,
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
  [FunctionType.Fnil, 'Fnil'],
  [FunctionType.Builtin, 'Builtin'],
])

export function isFunctionType(type: unknown): type is FunctionType {
  return typeof type === 'number' && functionTypeName.has(type)
}
