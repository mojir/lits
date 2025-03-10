import type { Argument } from '.'

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
    'last-index-of',
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
    'take-last',
    'take-while',
    'drop',
    'drop-last',
    'drop-while',
    'sort',
    'sort-by',
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
    'starts-with?',
    'ends-with?',
    'interleave',
    'interpose',
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
    '√',
    '∛',
    '**',
    'round',
    'trunc',
    'floor',
    'ceil',
    'min',
    'max',
    'abs',
    'sign',
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
    'every-pred',
    'some-pred',
    'fnull',
  ] as const,
  misc: [
    '≠',
    '=',
    '<',
    '>',
    '≤',
    '≥',
    '!',
    'write!',
    'iso-date->epoch',
    'epoch->iso-date',
    'boolean',
    'compare',
    'identical?',
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
    'null?',
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
    'replace-all',
  ] as const,
  specialExpressions: [
    '&&',
    '||',
    'def',
    'let',
    'fn',
    'defn',
    'try',
    'throw',
    'if',
    'unless',
    'cond',
    'switch',
    'comment',
    'do',
    'recur',
    'loop',
    'doseq',
    'for',
    'defined?',
    '??',
  ] as const,
  string: [
    'subs',
    'string-repeat',
    'str',
    'number',
    'lower-case',
    'upper-case',
    'trim',
    'trim-left',
    'trim-right',
    'pad-left',
    'pad-right',
    'split',
    'split-lines',
    'template',
    'to-char-code',
    'from-char-code',
    'encode-base64',
    'decode-base64',
    'encode-uri-component',
    'decode-uri-component',
    'join',
    '++',
    'capitalize',
    'blank?',
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
    'bit-flip',
    'bit-clear',
    'bit-set',
    'bit-test',
  ] as const,
  assert: [
    'assert',
    'assert=',
    'assert!=',
    'assert-gt',
    'assert-lt',
    'assert-gte',
    'assert-lte',
    'assert-true',
    'assert-false',
    'assert-truthy',
    'assert-falsy',
    'assert-null',
    'assert-throws',
    'assert-throws-error',
    'assert-not-throws',
  ] as const,
  shorthand: [
    '-short-regexp',
    '-short-fn',
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
    '-type-null',
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

export type NormalExpressionName =
  | CollectionApiName
  | ArrayApiName
  | SequenceApiName
  | MathApiName
  | FunctionalApiName
  | MiscApiName
  | ObjectApiName
  | PredicateApiName
  | RegularExpressionApiName
  | StringApiName
  | BitwiseApiName
  | AssertApiName

export type FunctionName =
  | NormalExpressionName
  | SpecialExpressionsApiName

export type ShorthandName = typeof api.shorthand[number]

export type DatatypeName = typeof api.datatype[number]

const apiFunctionNames = [
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
  ...apiFunctionNames,
  ...api.shorthand,
  ...api.datatype,
] as const

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

export function getOperatorArgs(a: DataType | DataType[], b: DataType | DataType[]): Record<string, Argument> {
  return { a: { type: a }, b: { type: b } }
}
