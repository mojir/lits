import { normalExpressionKeys, specialExpressionKeys } from '../src/builtin'

export const api = {
  collection: [
    'count',
    'get',
    'get-in',
    'contains?',
    'has?',
    'has-some?',
    'has-every?',
    'assoc',
    'assoc-in',
    'concat',
    'not-empty',
    'every?',
    'not-every?',
    'any?',
    'not-any?',
    'update',
    'update-in',
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
    'reduce-right',
    'map',
    'filter',
    'position',
    'index-of',
    'some',
    'reverse',
    'first',
    'second',
    'last',
    'rest',
    'nthrest',
    'next',
    'nthnext',
    'cons',
    'take',
    'take-last',
    'take-while',
    'drop',
    'drop-last',
    'drop-while',
    'sort',
    'sort-by',
    'random-sample!',
    'rand-nth!',
    'shuffle!',
    'distinct',
    'remove',
    'remove-at',
    'split-at',
    'split-with',
    'frequencies',
    'group-by',
    'partition',
    'partition-all',
    'partition-by',
  ] as const,
  math: [
    '+',
    '-',
    '*',
    '/',
    'mod',
    'rem',
    'quot',
    'inc',
    'dec',
    'sqrt',
    'cbrt',
    'pow',
    'exp',
    'round',
    'trunc',
    'floor',
    'ceil',
    'min',
    'max',
    'abs',
    'sign',
    'positive-infinity',
    'negative-infinity',
    'max-safe-integer',
    'min-safe-integer',
    'max-value',
    'min-value',
    'epsilon',
    'nan',
    'e',
    'pi',
    'log',
    'log2',
    'log10',
    'rand!',
    'rand-int!',
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
    'every-pred',
    'some-pred',
    'fnil',
  ] as const,
  misc: [
    'not=',
    '=',
    '<',
    '>',
    '<=',
    '>=',
    'not',
    'write!',
    'inst-ms!',
    'iso-date-time->inst-ms',
    'inst-ms->iso-date-time',
    'boolean',
    'compare',
    'lits-version!',
    'uuid!',
    'equal?',
    'json-parse',
    'json-stringify',
  ] as const,
  object: [
    'dissoc',
    'object',
    'keys',
    'vals',
    'entries',
    'find',
    'merge',
    'merge-with',
    'zipmap',
    'select-keys',
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
    'negative-infinity?',
    'positive-infinity?',
    'false?',
    'true?',
    'empty?',
    'not-empty?',
  ] as const,
  regularExpression: [
    'regexp',
    'match',
    'replace',
  ] as const,
  specialExpressions: [
    'and',
    'or',
    'def',
    'defs',
    'let',
    'if-let',
    'when-let',
    'when-first',
    'fn',
    'defn',
    'defns',
    'try',
    'throw',
    'if',
    'if-not',
    'cond',
    'when',
    'when-not',
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
    'string-repeat',
    'str',
    'number',
    'number-to-string',
    'lower-case',
    'upper-case',
    'trim',
    'trim-left',
    'trim-right',
    'pad-left',
    'pad-right',
    'split',
    'template',
    'to-char-code',
    'from-char-code',
    'encode-base64',
    'decode-base64',
    'encode-uri-component',
    'decode-uri-component',
    'join',
  ] as const,
  bitwise: [
    'bit-shift-left',
    'bit-shift-right',
    'bit-not',
    'bit-and',
    'bit-and-not',
    'bit-or',
    'bit-xor',
    'bit-flip',
    'bit-clear',
    'bit-set',
    'bit-test',
  ] as const,
  assert: [
    'assert',
    'assert=',
    'assert-not=',
    'assert-equal',
    'assert-not-equal',
    'assert>',
    'assert<',
    'assert>=',
    'assert<=',
    'assert-true',
    'assert-false',
    'assert-truthy',
    'assert-falsy',
    'assert-nil',
    'assert-throws',
    'assert-throws-error',
    'assert-not-throws',
  ] as const,
  shorthand: [
    '_short_regexp',
    '_short_fn',
    '_short_string',
    '_short_dot',
    '_short_hash',
  ] as const satisfies `_short_${string}`[],
  datatype: [
    '_type_number',
    '_type_string',
    '_type_object',
    '_type_array',
    '_type_boolean',
    '_type_function',
    '_type_integer',
    '_type_any',
    '_type_nil',
    '_type_collection',
    '_type_sequence',
    '_type_regexp',
    '_type_never',
  ] as const satisfies `_type_${string}`[],
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
  'nil',
  'collection',
  'sequence',
  'regexp',
  'never',
] as const
export type DataType = typeof dataTypes[number]

export function isDataType(arg: string): arg is DataType {
  return dataTypes.includes(arg as DataType)
}
