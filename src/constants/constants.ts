export enum AstNodeType {
  Number = 201,
  String = 202,
  NormalExpression = 203,
  SpecialExpression = 204,
  Symbol = 205,
  Modifier = 206,
  ReservedSymbol = 207,
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
  [AstNodeType.Symbol, 'Name'],
  [AstNodeType.Modifier, 'Modifier'],
  [AstNodeType.ReservedSymbol, 'ReservedName'],
  [AstNodeType.Binding, 'Binding'],
  [AstNodeType.Argument, 'Argument'],
  [AstNodeType.Partial, 'Partial'],
])

export function isAstNodeType(type: unknown): type is AstNodeType {
  return typeof type === 'number' && astNodeTypeName.has(type)
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
