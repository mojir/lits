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

export enum TokenType {
  Bracket = 101,
  Number = 102,
  Name = 103,
  String = 104,
  ReservedName = 105,
  Modifier = 106,
  RegexpShorthand = 107,
  FnShorthand = 108,
  CollectionAccessor = 109,
}

export const tokenTypeName = new Map([
  [TokenType.Bracket, 'Bracket'],
  [TokenType.Number, 'Number'],
  [TokenType.Name, 'Name'],
  [TokenType.String, 'String'],
  [TokenType.ReservedName, 'ReservedName'],
  [TokenType.Modifier, 'Modifier'],
  [TokenType.RegexpShorthand, 'RegexpShorthand'],
  [TokenType.FnShorthand, 'FnShorthand'],
  [TokenType.CollectionAccessor, 'CollectionAccessor'],
])

export function isTokenType(type: unknown): type is TokenType {
  return typeof type === 'number' && tokenTypeName.has(type)
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
