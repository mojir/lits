import { normalExpressionKeys, specialExpressionKeys } from '../src/builtin'

export const api = {
  collection: [
    'count',
    'get',
    'get_in',
    'contains?',
    'has?',
    'has_some?',
    'has_every?',
    'assoc',
    'assoc_in',
    'concat',
    'not_empty',
    'every?',
    'not_every?',
    'any?',
    'not_any?',
    'update',
    'update_in',
  ] as const,
  array: [
    'array',
    'range',
    'repeat',
    'flatten',
    'mapcat',
  ] as const,
  sequence: [
    'nth',
    'push',
    'pop',
    'unshift',
    'shift',
    'slice',
    'reductions',
    'reduce',
    'reduce_right',
    'map',
    'filter',
    'position',
    'index_of',
    'some',
    'reverse',
    'first',
    'second',
    'last',
    'rest',
    'nthrest',
    'next',
    'nthnext',
    'take',
    'take_last',
    'take_while',
    'drop',
    'drop_last',
    'drop_while',
    'sort',
    'sort_by',
    'distinct',
    'remove',
    'remove_at',
    'split_at',
    'split_with',
    'frequencies',
    'group_by',
    'partition',
    'partition_all',
    'partition_by',
  ] as const,
  math: [
    '+',
    '-',
    '*',
    '/',
    'mod',
    '%',
    'quot',
    'inc',
    'dec',
    'sqrt',
    'cbrt',
    '**',
    'exp',
    'round',
    'trunc',
    'floor',
    'ceil',
    'min',
    'max',
    'abs',
    'sign',
    'positive_infinity',
    'negative_infinity',
    'max_safe_integer',
    'min_safe_integer',
    'max_value',
    'min_value',
    'epsilon',
    'nan',
    'e',
    'pi',
    'log',
    'log2',
    'log10',
    'sin',
    'cos',
    'tan',
    'asin',
    'acos',
    'atan',
    'sinh',
    'cosh',
    'tanh',
    'asinh',
    'acosh',
    'atanh',
  ] as const,
  functional: [
    'apply',
    'identity',
    'partial',
    'comp',
    'constantly',
    'juxt',
    'complement',
    'every_pred',
    'some_pred',
    'fnil',
  ] as const,
  misc: [
    '!=',
    '==',
    '<',
    '>',
    '<=',
    '>=',
    '!',
    'write!',
    'iso_date>epoch',
    'epoch>iso_date',
    'boolean',
    'compare',
    'uuid!',
    'equal?',
    'json_parse',
    'json_stringify',
  ] as const,
  object: [
    'dissoc',
    'object',
    'keys',
    'vals',
    'entries',
    'find',
    'merge',
    'merge_with',
    'zipmap',
    'select_keys',
  ] as const,
  predicate: [
    'boolean?',
    'nil?',
    'number?',
    'string?',
    'function?',
    'integer?',
    'array?',
    'object?',
    'coll?',
    'seq?',
    'regexp?',
    'zero?',
    'pos?',
    'neg?',
    'even?',
    'odd?',
    'finite?',
    'nan?',
    'negative_infinity?',
    'positive_infinity?',
    'false?',
    'true?',
    'empty?',
    'not_empty?',
  ] as const,
  regularExpression: [
    'regexp',
    'match',
    'replace',
  ] as const,
  specialExpressions: [
    '&&',
    '||',
    'def',
    'defs',
    'let',
    'if_let',
    'when_let',
    'when_first',
    'fn',
    'defn',
    'defns',
    'try',
    'throw',
    'if',
    'if_not',
    'cond',
    'when',
    'when_not',
    'comment',
    'do',
    'recur',
    'loop',
    'time!',
    'doseq',
    'for',
    'declared?',
    '??',
  ] as const,
  string: [
    'subs',
    'string_repeat',
    'str',
    'number',
    'lower_case',
    'upper_case',
    'trim',
    'trim_left',
    'trim_right',
    'pad_left',
    'pad_right',
    'split',
    'template',
    'to_char_code',
    'from_char_code',
    'encode_base64',
    'decode_base64',
    'encode_uri_component',
    'decode_uri_component',
    'join',
    '++',
  ] as const,
  bitwise: [
    '<<',
    '>>',
    '>>>',
    '~',
    '&',
    '&!',
    '|',
    '^',
    'bit_flip',
    'bit_clear',
    'bit_set',
    'bit_test',
  ] as const,
  assert: [
    'assert',
    'assert=',
    'assert!=',
    'assert_equal',
    'assert_not_equal',
    'assert_gt',
    'assert_lt',
    'assert_gte',
    'assert_lte',
    'assert_true',
    'assert_false',
    'assert_truthy',
    'assert_falsy',
    'assert_null',
    'assert_throws',
    'assert_throws_error',
    'assert_not_throws',
  ] as const,
  shorthand: [
    '-short-regexp',
    '-short-fn',
    '-short-string',
    '-short-dot',
    '-short-hash',
  ] as const satisfies `-short-${string}`[],
  datatype: [
    '-type-number',
    '-type-string',
    '-type-object',
    '-type-array',
    '-type-boolean',
    '-type-function',
    '-type-integer',
    '-type-any',
    '-type-nil',
    '-type-collection',
    '-type-sequence',
    '-type-regexp',
    '-type-never',
  ] as const satisfies `-type-${string}`[],
} as const

export type CollectionApiName = typeof api.collection[number]
export type ArrayApiName = typeof api.array[number]
export type SequenceApiName = typeof api.sequence[number]
export type MathApiName = typeof api.math[number]
export type FunctionalApiName = typeof api.functional[number]
export type MiscApiName = typeof api.misc[number]
export type ObjectApiName = typeof api.object[number]
export type PredicateApiName = typeof api.predicate[number]
export type RegularExpressionApiName = typeof api.regularExpression[number]
export type SpecialExpressionsApiName = typeof api.specialExpressions[number]
export type StringApiName = typeof api.string[number]
export type BitwiseApiName = typeof api.bitwise[number]
export type AssertApiName = typeof api.assert[number]

export type FunctionName =
  | CollectionApiName
  | ArrayApiName
  | SequenceApiName
  | MathApiName
  | FunctionalApiName
  | MiscApiName
  | ObjectApiName
  | PredicateApiName
  | RegularExpressionApiName
  | SpecialExpressionsApiName
  | StringApiName
  | BitwiseApiName
  | AssertApiName

export type ShorthandName = typeof api.shorthand[number]

export type DatatypeName = typeof api.datatype[number]

const functionNames = [
  ...api.collection,
  ...api.array,
  ...api.sequence,
  ...api.math,
  ...api.functional,
  ...api.misc,
  ...api.object,
  ...api.predicate,
  ...api.regularExpression,
  ...api.specialExpressions,
  ...api.string,
  ...api.bitwise,
  ...api.assert,
] as const

const apiNames = [
  ...functionNames,
  ...api.shorthand,
  ...api.datatype,
] as const

const functionNamesFromLitsSrc = [...normalExpressionKeys, ...specialExpressionKeys] as const

for (const functionName of functionNamesFromLitsSrc) {
  if (!apiNames.includes(functionName as ApiName))
    throw new Error(`Function name "${functionName}" is not included in the API`)
}

for (const functionName of functionNames) {
  if (!functionNamesFromLitsSrc.includes(functionName as FunctionName))
    throw new Error(`Function name "${functionName}" is not included in the Lits source`)
}

export type ApiName = typeof apiNames[number]

export function isApiName(arg: string): arg is ApiName {
  return apiNames.includes(arg as ApiName)
}

export const categoryRecord = {
  'Special expression': true,
  'Predicate': true,
  'Sequence': true,
  'Collection': true,
  'Array': true,
  'Object': true,
  'String': true,
  'Math': true,
  'Functional': true,
  'Regular expression': true,
  'Bitwise': true,
  'Misc': true,
  'Assert': true,
  'Shorthand': true,
  'Datatype': true,
} as const

export type Category = keyof typeof categoryRecord

export const categories = Object.keys(categoryRecord) as Category[]

const dataTypes = [
  'number',
  'string',
  'object',
  'array',
  'boolean',
  'function',
  'integer',
  'any',
  'null',
  'collection',
  'sequence',
  'regexp',
  'never',
] as const
export type DataType = typeof dataTypes[number]

export function isDataType(arg: string): arg is DataType {
  return dataTypes.includes(arg as DataType)
}
