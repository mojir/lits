import type { Argument } from '.'

function getNumberTheorySequenceNames<T extends string>(name: T): [`TEMP-nth.${T}-seq`, `TEMP-nth.${T}-nth`, `TEMP-nth.${T}-take-while`, `TEMP-nth.${T}?`] {
  return [`TEMP-nth.${name}-seq`, `TEMP-nth.${name}-nth`, `TEMP-nth.${name}-take-while`, `TEMP-nth.${name}?`]
}

function getVectorReductionNames<T extends string>(name: T): [`vec.${T}`, `vec.moving-${string}`, `vec.centered-moving-${string}`, `vec.running-${string}`] {
  const baseName = name.replace(/^TEMP-/, '')
  return [`vec.${name}`, `vec.moving-${baseName}`, `vec.centered-moving-${baseName}`, `vec.running-${baseName}`]
}

export const api = {
  collection: [
    'filter',
    'filteri',
    'map',
    'mapi',
    'reduce',
    'reducei',
    'reduce-right',
    'reducei-right',
    'reductions',
    'reductionsi',
    'count',
    'get',
    'get-in',
    'contains?',
    'assoc',
    'assoc-in',
    '++',
    'not-empty',
    'every?',
    'not-every?',
    'any?',
    'not-any?',
    'update',
    'update-in',
  ] as const,
  array: [
    'range',
    'repeat',
    'flatten',
    'mapcat',
    'moving-fn',
    'running-fn',
  ] as const,
  sequence: [
    'TEMP-nth',
    'push',
    'pop',
    'unshift',
    'shift',
    'slice',
    'splice',
    'position',
    'index-of',
    'last-index-of',
    'some',
    'reverse',
    'first',
    'second',
    'last',
    'rest',
    'next',
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
    'rem',
    'quot',
    'inc',
    'dec',
    'sqrt',
    'cbrt',
    '^',
    'round',
    'trunc',
    'floor',
    'ceil',
    'min',
    'max',
    'abs',
    'sign',
    'ln',
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
    'to-rad',
    'to-deg',
  ] as const,
  functional: [
    '|>',
    'apply',
    'identity',
    'comp',
    'constantly',
    'juxt',
    'complement',
    'every-pred',
    'some-pred',
    'fnull',
  ] as const,
  meta: [
    'doc',
    'arity',
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
    'iso-date->epoch',
    'epoch->iso-date',
    'boolean',
    'compare',
    'identical?',
    'import',
    'json-parse',
    'json-stringify',
  ] as const,
  object: [
    'dissoc',
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
    'negative-infinity?',
    'positive-infinity?',
    'false?',
    'true?',
    'empty?',
    'not-empty?',
    'vector?',
    'grid?',
    'matrix?',
  ] as const,
  regularExpression: [
    'regexp',
    'match',
    'replace',
    'replace-all',
  ] as const,
  string: [
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
    'capitalize',
    'blank?',
  ] as const,
  bitwise: [
    '<<',
    '>>',
    '>>>',
    'bit-not',
    '&',
    'bit-and-not',
    '|',
    'xor',
    'bit-flip',
    'bit-clear',
    'bit-set',
    'bit-test',
  ] as const,
  // TODO, remove some, add some. E.g. type guards, assert-number, assert-string, etc.
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
  grid: [
    'grid.every?',
    'grid.some?',
    'grid.every-row?',
    'grid.some-row?',
    'grid.every-col?',
    'grid.some-col?',
    'grid.row',
    'grid.col',
    'grid.shape',
    'grid.fill',
    'grid.generate',
    'grid.reshape',
    'grid.transpose',
    'grid.flip-h',
    'grid.flip-v',
    'grid.rotate',
    'grid.reverse-rows',
    'grid.reverse-cols',
    'grid.slice',
    'grid.slice-rows',
    'grid.slice-cols',
    'grid.splice-rows',
    'grid.splice-cols',
    'grid.concat-rows',
    'grid.concat-cols',
    'grid.TEMP-map',
    'grid.mapi',
    'grid.TEMP-reduce',
    'grid.reducei',
    'grid.push-rows',
    'grid.unshift-rows',
    'grid.pop-row',
    'grid.shift-row',
    'grid.push-cols',
    'grid.unshift-cols',
    'grid.pop-col',
    'grid.shift-col',
    'grid.from-array',
  ] as const,
  matrix: [
    'mat.mul',
    'mat.det',
    'mat.inv',
    'mat.adj',
    'mat.cofactor',
    'mat.minor',
    'mat.trace',
    'mat.symmetric?',
    'mat.triangular?',
    'mat.upper-triangular?',
    'mat.lower-triangular?',
    'mat.diagonal?',
    'mat.square?',
    'mat.orthogonal?',
    'mat.identity?',
    'mat.invertible?',
    'mat.hilbert',
    'mat.vandermonde',
    'mat.band',
    'mat.banded?',
    'mat.rank',
    'mat.frobenius-norm',
    'mat.one-norm',
    'mat.inf-norm',
    'mat.max-norm',
  ] as const,
  vector: [
    'vec.monotonic?',
    'vec.strictly-monotonic?',
    'vec.increasing?',
    'vec.decreasing?',
    'vec.strictly-increasing?',
    'vec.strictly-decreasing?',
    'vec.median',
    'vec.mode',
    'vec.min-index',
    'vec.max-index',
    'vec.sort-indices',
    'vec.count-values',
    'vec.linspace',
    'vec.ones',
    'vec.zeros',
    'vec.fill',
    'vec.generate',
    'vec.cumsum',
    'vec.cumprod',
    'vec.quartiles',
    'vec.percentile',
    'vec.quantile',
    'vec.histogram',
    'vec.ecdf',
    'vec.outliers?',
    'vec.outliers',
    'vec.bincount',
    'vec.winsorize',
    'vec.mse',
    'vec.mae',
    'vec.rmse',
    'vec.smape',
    ...getVectorReductionNames('mean'),
    ...getVectorReductionNames('median'),
    ...getVectorReductionNames('variance'),
    ...getVectorReductionNames('sample-variance'),
    ...getVectorReductionNames('sum'),
    ...getVectorReductionNames('prod'),
    ...getVectorReductionNames('TEMP-min'),
    ...getVectorReductionNames('TEMP-max'),
    ...getVectorReductionNames('stdev'),
    ...getVectorReductionNames('sample-stdev'),
    ...getVectorReductionNames('iqr'),
    ...getVectorReductionNames('span'),
    ...getVectorReductionNames('geometric-mean'),
    ...getVectorReductionNames('harmonic-mean'),
    ...getVectorReductionNames('skewness'),
    ...getVectorReductionNames('sample-skewness'),
    ...getVectorReductionNames('kurtosis'),
    ...getVectorReductionNames('sample-kurtosis'),
    ...getVectorReductionNames('excess-kurtosis'),
    ...getVectorReductionNames('sample-excess-kurtosis'),
    ...getVectorReductionNames('rms'),
    ...getVectorReductionNames('mad'),
    ...getVectorReductionNames('medad'),
    ...getVectorReductionNames('gini-coefficient'),
    ...getVectorReductionNames('entropy'),
    ...getVectorReductionNames('skewness'),
  ] as const,
  linAlg: [
    'lin.reflect',
    'lin.refract',
    'lin.lerp',
    'lin.rotate2d',
    'lin.rotate3d',
    'lin.dot',
    'lin.cross',
    'lin.normalize-minmax',
    'lin.normalize-zscore',
    'lin.normalize-robust',
    'lin.normalize-l1',
    'lin.normalize-l2',
    'lin.normalize-log',
    'lin.angle',
    'lin.projection',
    'lin.orthogonal?',
    'lin.parallel?',
    'lin.collinear?',
    'lin.cosine-similarity',
    'lin.euclidean-distance',
    'lin.euclidean-norm',
    'lin.manhattan-distance',
    'lin.manhattan-norm',
    'lin.hamming-distance',
    'lin.hamming-norm',
    'lin.chebyshev-distance',
    'lin.chebyshev-norm',
    'lin.minkowski-distance',
    'lin.minkowski-norm',
    'lin.cov',
    'lin.corr',
    'lin.spearman-corr',
    'lin.pearson-corr',
    'lin.kendall-tau',
    'lin.autocorrelation',
    'lin.cross-correlation',
    'lin.rref',
    'lin.solve',
    'lin.to-polar',
    'lin.from-polar',
  ] as const,
  numberTheory: [
    ...getNumberTheorySequenceNames('abundant'),
    ...getNumberTheorySequenceNames('bell'),
    ...getNumberTheorySequenceNames('catalan'),
    ...getNumberTheorySequenceNames('composite'),
    ...getNumberTheorySequenceNames('factorial'),
    ...getNumberTheorySequenceNames('fibonacci'),
    ...getNumberTheorySequenceNames('geometric'),
    ...getNumberTheorySequenceNames('golomb'),
    ...getNumberTheorySequenceNames('happy'),
    ...getNumberTheorySequenceNames('look-and-say'),
    ...getNumberTheorySequenceNames('lucas'),
    ...getNumberTheorySequenceNames('lucky'),
    ...getNumberTheorySequenceNames('mersenne'),
    ...getNumberTheorySequenceNames('padovan'),
    ...getNumberTheorySequenceNames('partition'),
    ...getNumberTheorySequenceNames('pell'),
    ...getNumberTheorySequenceNames('perfect'),
    ...getNumberTheorySequenceNames('perfect-cube'),
    ...getNumberTheorySequenceNames('perfect-power'),
    ...getNumberTheorySequenceNames('perfect-square'),
    ...getNumberTheorySequenceNames('polygonal'),
    ...getNumberTheorySequenceNames('prime'),
    ...getNumberTheorySequenceNames('recaman'),
    ...getNumberTheorySequenceNames('sylvester'),
    ...getNumberTheorySequenceNames('thue-morse'),
    ...getNumberTheorySequenceNames('tribonacci'),
    'TEMP-nth.collatz-seq',
    'TEMP-nth.juggler-seq',
    'TEMP-nth.bernoulli-seq',
    'TEMP-nth.bernoulli-take-while',
    'TEMP-nth.bernoulli-nth',
    'TEMP-nth.combinations',
    'TEMP-nth.count-combinations',
    'TEMP-nth.derangements',
    'TEMP-nth.count-derangements',
    'TEMP-nth.divisors',
    'TEMP-nth.count-divisors',
    'TEMP-nth.proper-divisors',
    'TEMP-nth.count-proper-divisors',
    'TEMP-nth.prime-factors',
    'TEMP-nth.count-prime-factors',
    'TEMP-nth.distinct-prime-factors',
    'TEMP-nth.count-distinct-prime-factors',
    'TEMP-nth.factorial',
    'TEMP-nth.partitions',
    'TEMP-nth.count-partitions',
    'TEMP-nth.permutations',
    'TEMP-nth.count-permutations',
    'TEMP-nth.power-set',
    'TEMP-nth.count-power-set',
    'TEMP-nth.coprime?',
    'TEMP-nth.divisible-by?',
    'TEMP-nth.gcd',
    'TEMP-nth.lcm',
    'TEMP-nth.multinomial',
    'TEMP-nth.amicable?',
    'TEMP-nth.euler-totient',
    'TEMP-nth.mobius',
    'TEMP-nth.mertens',
    'TEMP-nth.sigma',
    'TEMP-nth.carmichael-lambda',
    'TEMP-nth.cartesian-product',
    'TEMP-nth.perfect-power',
    'TEMP-nth.mod-exp',
    'TEMP-nth.mod-inv',
    'TEMP-nth.extended-gcd',
    'TEMP-nth.chinese-remainder',
    'TEMP-nth.stirling-first',
    'TEMP-nth.stirling-second',
  ] as const,
  random: [
    '!:random',
    '!:random-int',
    '!:random-int-inclusive',
    '!:random-float',
    '!:random-boolean',
    '!:random-item',
    '!:random-sample',
    '!:random-sample-unique',
    '!:shuffle',
    '!:random-normal',
    '!:random-exponential',
    '!:random-binomial',
    '!:random-poisson',
    '!:random-gamma',
    '!:random-pareto',
    '!:uuid',
    '!:random-char',
    '!:random-string',
    '!:random-id',
    '!:random-color',
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
    '-type-vector',
    '-type-matrix',
    '-type-grid',
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
export type MetaApiName = typeof api.meta[number]
export type ObjectApiName = typeof api.object[number]
export type PredicateApiName = typeof api.predicate[number]
export type RegularExpressionApiName = typeof api.regularExpression[number]
export type SpecialExpressionsApiName = string
export type StringApiName = typeof api.string[number]
export type BitwiseApiName = typeof api.bitwise[number]
export type AssertApiName = typeof api.assert[number]
export type GridApiName = typeof api.grid[number]
export type MatrixApiName = typeof api.matrix[number]
export type NumberTheoryApiName = typeof api.numberTheory[number]
export type VectorApiName = typeof api.vector[number]
export type LinAlgApiName = typeof api.linAlg[number]
export type RandomApiName = typeof api.random[number]

// Core functions - always available without import
export type CoreNormalExpressionName =
  | CollectionApiName
  | ArrayApiName
  | SequenceApiName
  | MathApiName
  | FunctionalApiName
  | MetaApiName
  | MiscApiName
  | ObjectApiName
  | PredicateApiName
  | RegularExpressionApiName
  | StringApiName
  | BitwiseApiName
  | AssertApiName

// Namespace functions - require import()
export type NamespaceExpressionName =
  | MatrixApiName
  | VectorApiName
  | LinAlgApiName
  | GridApiName
  | NumberTheoryApiName
  | RandomApiName

// All normal expression names
export type NormalExpressionName =
  | CoreNormalExpressionName
  | NamespaceExpressionName

export type FunctionName =
  | NormalExpressionName
  | SpecialExpressionsApiName

export type ShorthandName = typeof api.shorthand[number]

export type DatatypeName = typeof api.datatype[number]

// Core API function names (always available)
const coreApiFunctionNames = [
  ...api.collection,
  ...api.array,
  ...api.sequence,
  ...api.math,
  ...api.functional,
  ...api.meta,
  ...api.misc,
  ...api.object,
  ...api.predicate,
  ...api.regularExpression,
  ...api.string,
  ...api.bitwise,
  ...api.assert,
] as const

// Namespace API function names (require import())
const namespaceApiFunctionNames = [
  ...api.matrix,
  ...api.vector,
  ...api.linAlg,
  ...api.grid,
  ...api.numberTheory,
  ...api.random,
] as const

// All API function names
const apiFunctionNames = [
  ...coreApiFunctionNames,
  ...namespaceApiFunctionNames,
] as const

// Core API names (core functions + shorthand + datatype)
const coreApiNames = [
  ...coreApiFunctionNames,
  ...api.shorthand,
  ...api.datatype,
] as const

const apiNames = [
  ...apiFunctionNames,
  ...api.shorthand,
  ...api.datatype,
] as const

export type CoreApiName = typeof coreApiNames[number]
export type ApiName = typeof apiNames[number]

export function isApiName(arg: string): arg is ApiName {
  return apiNames.includes(arg as ApiName)
}

export function isCoreApiName(arg: string): arg is CoreApiName {
  return coreApiNames.includes(arg as CoreApiName)
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
  'Meta': true,
  'Assert': true,
  'Vector': true,
  'Linear Algebra': true,
  'Matrix': true,
  'Grid': true,
  'Number Theory': true,
  'Random': true,
  'Shorthand': true,
  'Datatype': true,
} as const

export type Category = keyof typeof categoryRecord

export const categories = Object.keys(categoryRecord) as Category[]

// Categories that are namespaces (require import)
export const namespaceCategories: Category[] = ['Vector', 'Linear Algebra', 'Matrix', 'Grid', 'Number Theory', 'Random']

// Core categories (always available)
export const coreCategories = categories.filter(c => !namespaceCategories.includes(c))

// Map from category to namespace import name
export const categoryToNamespace: Partial<Record<Category, string>> = {
  'Vector': 'vec',
  'Linear Algebra': 'lin',
  'Matrix': 'mat',
  'Grid': 'grid',
  'Number Theory': 'TEMP-nth',
  'Random': 'rand',
}

const dataTypes = [
  'number',
  'string',
  'object',
  'array',
  'vector',
  'matrix',
  'grid',
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
