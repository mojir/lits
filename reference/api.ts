export { categories, categoryRecord, coreCategories, isDataType, moduleCategories } from '../src/builtin/interface'
export type { Category, DataType } from '../src/builtin/interface'

function getNumberTheorySequenceNames<T extends string>(name: T): [`number-theory.${T}-seq`, `number-theory.${T}-nth`, `number-theory.${T}-take-while`, `number-theory.${T}?`] {
  return [`number-theory.${name}-seq`, `number-theory.${name}-nth`, `number-theory.${name}-take-while`, `number-theory.${name}?`]
}

function getVectorReductionNames<T extends string>(name: T): [`vector.${T}`, `vector.moving-${string}`, `vector.centered-moving-${string}`, `vector.running-${string}`] {
  const baseName = name.replace(/^/, '')
  return [`vector.${name}`, `vector.moving-${baseName}`, `vector.centered-moving-${baseName}`, `vector.running-${baseName}`]
}

export const api = {
  collection: [
    'filter',
    'map',
    'reduce',
    'count',
    'get',
    'contains?',
    'assoc',
    '++',
  ] as const,
  collectionUtils: [
    'collection.filteri',
    'collection.mapi',
    'collection.reducei',
    'collection.reduce-right',
    'collection.reducei-right',
    'collection.reductions',
    'collection.reductionsi',
    'collection.get-in',
    'collection.assoc-in',
    'collection.update',
    'collection.update-in',
    'collection.not-empty',
    'collection.every?',
    'collection.not-every?',
    'collection.any?',
    'collection.not-any?',
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
    'nth',
    'push',
    'pop',
    'index-of',
    'some',
    'reverse',
    'first',
    'second',
    'last',
    'rest',
    'next',
    'sort',
    'slice',
  ] as const,
  sequenceUtils: [
    'sequence.position',
    'sequence.last-index-of',
    'sequence.shift',
    'sequence.unshift',
    'sequence.splice',
    'sequence.sort-by',
    'sequence.take',
    'sequence.take-last',
    'sequence.take-while',
    'sequence.drop',
    'sequence.drop-last',
    'sequence.drop-while',
    'sequence.distinct',
    'sequence.remove',
    'sequence.remove-at',
    'sequence.split-at',
    'sequence.split-with',
    'sequence.frequencies',
    'sequence.group-by',
    'sequence.partition',
    'sequence.partition-all',
    'sequence.partition-by',
    'sequence.starts-with?',
    'sequence.ends-with?',
    'sequence.interleave',
    'sequence.interpose',
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
    '^',
    'round',
    'trunc',
    'floor',
    'ceil',
    'min',
    'max',
    'abs',
    'sign',
  ] as const,
  mathUtils: [
    'math.ln',
    'math.log2',
    'math.log10',
    'math.sin',
    'math.cos',
    'math.tan',
    'math.asin',
    'math.acos',
    'math.atan',
    'math.sinh',
    'math.cosh',
    'math.tanh',
    'math.asinh',
    'math.acosh',
    'math.atanh',
    'math.to-rad',
    'math.to-deg',
  ] as const,
  functional: [
    '|>',
    'apply',
    'identity',
    'comp',
    'constantly',
  ] as const,
  functionalUtils: [
    'functional.juxt',
    'functional.complement',
    'functional.every-pred',
    'functional.some-pred',
    'functional.fnull',
  ] as const,
  meta: [
    'doc',
    'arity',
  ] as const,
  misc: [
    '≠',
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
    'collection?',
    'sequence?',
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
    'str',
    'number',
    'lower-case',
    'upper-case',
    'trim',
    'split',
    'join',
    'blank?',
  ] as const,
  stringUtils: [
    'string.string-repeat',
    'string.from-char-code',
    'string.to-char-code',
    'string.trim-left',
    'string.trim-right',
    'string.pad-left',
    'string.pad-right',
    'string.split-lines',
    'string.template',
    'string.encode-base64',
    'string.decode-base64',
    'string.encode-uri-component',
    'string.decode-uri-component',
    'string.capitalize',
  ] as const,
  bitwise: [
    '<<',
    '>>',
    '>>>',
    '&',
    '|',
    'xor',
  ] as const,
  bitwiseUtils: [
    'bitwise.bit-not',
    'bitwise.bit-and-not',
    'bitwise.bit-flip',
    'bitwise.bit-clear',
    'bitwise.bit-set',
    'bitwise.bit-test',
  ] as const,
  assert: [
    'assert.assert',
    'assert.assert=',
    'assert.assert!=',
    'assert.assert-gt',
    'assert.assert-lt',
    'assert.assert-gte',
    'assert.assert-lte',
    'assert.assert-true',
    'assert.assert-false',
    'assert.assert-truthy',
    'assert.assert-falsy',
    'assert.assert-null',
    'assert.assert-throws',
    'assert.assert-throws-error',
    'assert.assert-not-throws',
    'assert.assert-array',
    'assert.assert-boolean',
    'assert.assert-collection',
    'assert.assert-function',
    'assert.assert-grid',
    'assert.assert-integer',
    'assert.assert-matrix',
    'assert.assert-number',
    'assert.assert-object',
    'assert.assert-regexp',
    'assert.assert-sequence',
    'assert.assert-string',
    'assert.assert-vector',
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
    'grid.map',
    'grid.mapi',
    'grid.reduce',
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
    'matrix.mul',
    'matrix.det',
    'matrix.inv',
    'matrix.adj',
    'matrix.cofactor',
    'matrix.minor',
    'matrix.trace',
    'matrix.symmetric?',
    'matrix.triangular?',
    'matrix.upper-triangular?',
    'matrix.lower-triangular?',
    'matrix.diagonal?',
    'matrix.square?',
    'matrix.orthogonal?',
    'matrix.identity?',
    'matrix.invertible?',
    'matrix.hilbert',
    'matrix.vandermonde',
    'matrix.band',
    'matrix.banded?',
    'matrix.rank',
    'matrix.frobenius-norm',
    'matrix.one-norm',
    'matrix.inf-norm',
    'matrix.max-norm',
  ] as const,
  vector: [
    'sum',
    'mean',
    'median',
    'prod',
  ] as const,
  vectorUtils: [
    'vector.monotonic?',
    'vector.strictly-monotonic?',
    'vector.increasing?',
    'vector.decreasing?',
    'vector.strictly-increasing?',
    'vector.strictly-decreasing?',
    'vector.median',
    'vector.mode',
    'vector.min-index',
    'vector.max-index',
    'vector.sort-indices',
    'vector.count-values',
    'vector.linspace',
    'vector.ones',
    'vector.zeros',
    'vector.fill',
    'vector.generate',
    'vector.cumsum',
    'vector.cumprod',
    'vector.quartiles',
    'vector.percentile',
    'vector.quantile',
    'vector.histogram',
    'vector.ecdf',
    'vector.outliers?',
    'vector.outliers',
    'vector.bincount',
    'vector.winsorize',
    'vector.mse',
    'vector.mae',
    'vector.rmse',
    'vector.smape',
    ...getVectorReductionNames('mean'),
    ...getVectorReductionNames('median'),
    ...getVectorReductionNames('variance'),
    ...getVectorReductionNames('sample-variance'),
    ...getVectorReductionNames('sum'),
    ...getVectorReductionNames('prod'),
    ...getVectorReductionNames('min'),
    ...getVectorReductionNames('max'),
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
    'linear-algebra.reflect',
    'linear-algebra.refract',
    'linear-algebra.lerp',
    'linear-algebra.rotate2d',
    'linear-algebra.rotate3d',
    'linear-algebra.dot',
    'linear-algebra.cross',
    'linear-algebra.normalize-minmax',
    'linear-algebra.normalize-zscore',
    'linear-algebra.normalize-robust',
    'linear-algebra.normalize-l1',
    'linear-algebra.normalize-l2',
    'linear-algebra.normalize-log',
    'linear-algebra.angle',
    'linear-algebra.projection',
    'linear-algebra.orthogonal?',
    'linear-algebra.parallel?',
    'linear-algebra.collinear?',
    'linear-algebra.cosine-similarity',
    'linear-algebra.euclidean-distance',
    'linear-algebra.euclidean-norm',
    'linear-algebra.manhattan-distance',
    'linear-algebra.manhattan-norm',
    'linear-algebra.hamming-distance',
    'linear-algebra.hamming-norm',
    'linear-algebra.chebyshev-distance',
    'linear-algebra.chebyshev-norm',
    'linear-algebra.minkowski-distance',
    'linear-algebra.minkowski-norm',
    'linear-algebra.cov',
    'linear-algebra.corr',
    'linear-algebra.spearman-corr',
    'linear-algebra.pearson-corr',
    'linear-algebra.kendall-tau',
    'linear-algebra.autocorrelation',
    'linear-algebra.cross-correlation',
    'linear-algebra.rref',
    'linear-algebra.solve',
    'linear-algebra.to-polar',
    'linear-algebra.from-polar',
  ] as const,
  numberTheory: [
    ...getNumberTheorySequenceNames('abundant'),
    ...getNumberTheorySequenceNames('arithmetic'),
    ...getNumberTheorySequenceNames('bell'),
    ...getNumberTheorySequenceNames('catalan'),
    ...getNumberTheorySequenceNames('composite'),
    ...getNumberTheorySequenceNames('deficient'),
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
    'number-theory.collatz-seq',
    'number-theory.juggler-seq',
    'number-theory.bernoulli-seq',
    'number-theory.bernoulli-take-while',
    'number-theory.bernoulli-nth',
    'number-theory.combinations',
    'number-theory.count-combinations',
    'number-theory.derangements',
    'number-theory.count-derangements',
    'number-theory.divisors',
    'number-theory.count-divisors',
    'number-theory.proper-divisors',
    'number-theory.count-proper-divisors',
    'number-theory.prime-factors',
    'number-theory.count-prime-factors',
    'number-theory.distinct-prime-factors',
    'number-theory.count-distinct-prime-factors',
    'number-theory.factorial',
    'number-theory.partitions',
    'number-theory.count-partitions',
    'number-theory.permutations',
    'number-theory.count-permutations',
    'number-theory.power-set',
    'number-theory.count-power-set',
    'number-theory.coprime?',
    'number-theory.divisible-by?',
    'number-theory.gcd',
    'number-theory.lcm',
    'number-theory.multinomial',
    'number-theory.amicable?',
    'number-theory.euler-totient',
    'number-theory.mobius',
    'number-theory.mertens',
    'number-theory.sigma',
    'number-theory.carmichael-lambda',
    'number-theory.cartesian-product',
    'number-theory.perfect-power',
    'number-theory.mod-exp',
    'number-theory.mod-inv',
    'number-theory.extended-gcd',
    'number-theory.chinese-remainder',
    'number-theory.stirling-first',
    'number-theory.stirling-second',
  ] as const,
  random: [
    'random.random!',
    'random.random-int!',
    'random.random-int-inclusive!',
    'random.random-float!',
    'random.random-boolean!',
    'random.random-item!',
    'random.random-sample!',
    'random.random-sample-unique!',
    'random.shuffle!',
    'random.random-normal!',
    'random.random-exponential!',
    'random.random-binomial!',
    'random.random-poisson!',
    'random.random-gamma!',
    'random.random-pareto!',
    'random.uuid!',
    'random.random-char!',
    'random.random-string!',
    'random.random-id!',
    'random.random-color!',
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
export type StringUtilsApiName = typeof api.stringUtils[number]
export type CollectionUtilsApiName = typeof api.collectionUtils[number]
export type SequenceUtilsApiName = typeof api.sequenceUtils[number]
export type BitwiseApiName = typeof api.bitwise[number]
export type BitwiseUtilsApiName = typeof api.bitwiseUtils[number]
export type AssertApiName = typeof api.assert[number]
export type GridApiName = typeof api.grid[number]
export type MatrixApiName = typeof api.matrix[number]
export type NumberTheoryApiName = typeof api.numberTheory[number]
export type VectorApiName = typeof api.vector[number]
export type VectorUtilsApiName = typeof api.vectorUtils[number]
export type LinAlgApiName = typeof api.linAlg[number]
export type RandomApiName = typeof api.random[number]
export type MathUtilsApiName = typeof api.mathUtils[number]
export type FunctionalUtilsApiName = typeof api.functionalUtils[number]

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
  | VectorApiName

// Module functions - require import()
export type ModuleExpressionName =
  | MatrixApiName
  | VectorUtilsApiName
  | LinAlgApiName
  | GridApiName
  | NumberTheoryApiName
  | RandomApiName
  | MathUtilsApiName
  | FunctionalUtilsApiName
  | AssertApiName
  | StringUtilsApiName
  | CollectionUtilsApiName
  | SequenceUtilsApiName
  | BitwiseUtilsApiName

// All normal expression names
export type NormalExpressionName =
  | CoreNormalExpressionName
  | ModuleExpressionName

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
  ...api.vector,
] as const

// Module API function names (require import())
const moduleApiFunctionNames = [
  ...api.matrix,
  ...api.vectorUtils,
  ...api.linAlg,
  ...api.grid,
  ...api.numberTheory,
  ...api.random,
  ...api.mathUtils,
  ...api.functionalUtils,
  ...api.assert,
  ...api.stringUtils,
  ...api.collectionUtils,
  ...api.sequenceUtils,
  ...api.bitwiseUtils,
] as const

// All API function names
const apiFunctionNames = [
  ...coreApiFunctionNames,
  ...moduleApiFunctionNames,
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
