export { categories, categoryRecord, coreCategories, isDataType, moduleCategories } from '../src/builtin/interface'
export type { Category, DataType } from '../src/builtin/interface'

function getNumberTheorySequenceNames<T extends string>(name: T): [`Number-Theory.${T}-seq`, `Number-Theory.${T}-nth`, `Number-Theory.${T}-take-while`, `Number-Theory.${T}?`] {
  return [`Number-Theory.${name}-seq`, `Number-Theory.${name}-nth`, `Number-Theory.${name}-take-while`, `Number-Theory.${name}?`]
}

function getVectorReductionNames<T extends string>(name: T): [`Vector.${T}`, `Vector.moving-${string}`, `Vector.centered-moving-${string}`, `Vector.running-${string}`] {
  const baseName = name.replace(/^/, '')
  return [`Vector.${name}`, `Vector.moving-${baseName}`, `Vector.centered-moving-${baseName}`, `Vector.running-${baseName}`]
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
    'Collection.filteri',
    'Collection.mapi',
    'Collection.reducei',
    'Collection.reduce-right',
    'Collection.reducei-right',
    'Collection.reductions',
    'Collection.reductionsi',
    'Collection.get-in',
    'Collection.assoc-in',
    'Collection.update',
    'Collection.update-in',
    'Collection.not-empty',
    'Collection.every?',
    'Collection.not-every?',
    'Collection.any?',
    'Collection.not-any?',
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
    'Sequence.position',
    'Sequence.last-index-of',
    'Sequence.shift',
    'Sequence.unshift',
    'Sequence.splice',
    'Sequence.sort-by',
    'Sequence.take',
    'Sequence.take-last',
    'Sequence.take-while',
    'Sequence.drop',
    'Sequence.drop-last',
    'Sequence.drop-while',
    'Sequence.distinct',
    'Sequence.remove',
    'Sequence.remove-at',
    'Sequence.split-at',
    'Sequence.split-with',
    'Sequence.frequencies',
    'Sequence.group-by',
    'Sequence.partition',
    'Sequence.partition-all',
    'Sequence.partition-by',
    'Sequence.starts-with?',
    'Sequence.ends-with?',
    'Sequence.interleave',
    'Sequence.interpose',
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
    'Math.ln',
    'Math.log2',
    'Math.log10',
    'Math.sin',
    'Math.cos',
    'Math.tan',
    'Math.asin',
    'Math.acos',
    'Math.atan',
    'Math.sinh',
    'Math.cosh',
    'Math.tanh',
    'Math.asinh',
    'Math.acosh',
    'Math.atanh',
    'Math.to-rad',
    'Math.to-deg',
  ] as const,
  functional: [
    '|>',
    'apply',
    'identity',
    'comp',
    'constantly',
  ] as const,
  functionalUtils: [
    'Functional.juxt',
    'Functional.complement',
    'Functional.every-pred',
    'Functional.some-pred',
    'Functional.fnull',
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
    'String.string-repeat',
    'String.from-char-code',
    'String.to-char-code',
    'String.trim-left',
    'String.trim-right',
    'String.pad-left',
    'String.pad-right',
    'String.split-lines',
    'String.template',
    'String.encode-base64',
    'String.decode-base64',
    'String.encode-uri-component',
    'String.decode-uri-component',
    'String.capitalize',
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
    'Bitwise.bit-not',
    'Bitwise.bit-and-not',
    'Bitwise.bit-flip',
    'Bitwise.bit-clear',
    'Bitwise.bit-set',
    'Bitwise.bit-test',
  ] as const,
  assert: [
    'Assert.assert',
    'Assert.assert=',
    'Assert.assert!=',
    'Assert.assert-gt',
    'Assert.assert-lt',
    'Assert.assert-gte',
    'Assert.assert-lte',
    'Assert.assert-true',
    'Assert.assert-false',
    'Assert.assert-truthy',
    'Assert.assert-falsy',
    'Assert.assert-null',
    'Assert.assert-throws',
    'Assert.assert-throws-error',
    'Assert.assert-not-throws',
    'Assert.assert-array',
    'Assert.assert-boolean',
    'Assert.assert-collection',
    'Assert.assert-function',
    'Assert.assert-grid',
    'Assert.assert-integer',
    'Assert.assert-matrix',
    'Assert.assert-number',
    'Assert.assert-object',
    'Assert.assert-regexp',
    'Assert.assert-sequence',
    'Assert.assert-string',
    'Assert.assert-vector',
  ] as const,
  grid: [
    'Grid.every?',
    'Grid.some?',
    'Grid.every-row?',
    'Grid.some-row?',
    'Grid.every-col?',
    'Grid.some-col?',
    'Grid.row',
    'Grid.col',
    'Grid.shape',
    'Grid.fill',
    'Grid.generate',
    'Grid.reshape',
    'Grid.transpose',
    'Grid.flip-h',
    'Grid.flip-v',
    'Grid.rotate',
    'Grid.reverse-rows',
    'Grid.reverse-cols',
    'Grid.slice',
    'Grid.slice-rows',
    'Grid.slice-cols',
    'Grid.splice-rows',
    'Grid.splice-cols',
    'Grid.concat-rows',
    'Grid.concat-cols',
    'Grid.map',
    'Grid.mapi',
    'Grid.reduce',
    'Grid.reducei',
    'Grid.push-rows',
    'Grid.unshift-rows',
    'Grid.pop-row',
    'Grid.shift-row',
    'Grid.push-cols',
    'Grid.unshift-cols',
    'Grid.pop-col',
    'Grid.shift-col',
    'Grid.from-array',
  ] as const,
  matrix: [
    'Matrix.mul',
    'Matrix.det',
    'Matrix.inv',
    'Matrix.adj',
    'Matrix.cofactor',
    'Matrix.minor',
    'Matrix.trace',
    'Matrix.symmetric?',
    'Matrix.triangular?',
    'Matrix.upper-triangular?',
    'Matrix.lower-triangular?',
    'Matrix.diagonal?',
    'Matrix.square?',
    'Matrix.orthogonal?',
    'Matrix.identity?',
    'Matrix.invertible?',
    'Matrix.hilbert',
    'Matrix.vandermonde',
    'Matrix.band',
    'Matrix.banded?',
    'Matrix.rank',
    'Matrix.frobenius-norm',
    'Matrix.one-norm',
    'Matrix.inf-norm',
    'Matrix.max-norm',
  ] as const,
  vector: [
    'Vector.monotonic?',
    'Vector.strictly-monotonic?',
    'Vector.increasing?',
    'Vector.decreasing?',
    'Vector.strictly-increasing?',
    'Vector.strictly-decreasing?',
    'Vector.median',
    'Vector.mode',
    'Vector.min-index',
    'Vector.max-index',
    'Vector.sort-indices',
    'Vector.count-values',
    'Vector.linspace',
    'Vector.ones',
    'Vector.zeros',
    'Vector.fill',
    'Vector.generate',
    'Vector.cumsum',
    'Vector.cumprod',
    'Vector.quartiles',
    'Vector.percentile',
    'Vector.quantile',
    'Vector.histogram',
    'Vector.ecdf',
    'Vector.outliers?',
    'Vector.outliers',
    'Vector.bincount',
    'Vector.winsorize',
    'Vector.mse',
    'Vector.mae',
    'Vector.rmse',
    'Vector.smape',
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
    'Linear-Algebra.reflect',
    'Linear-Algebra.refract',
    'Linear-Algebra.lerp',
    'Linear-Algebra.rotate2d',
    'Linear-Algebra.rotate3d',
    'Linear-Algebra.dot',
    'Linear-Algebra.cross',
    'Linear-Algebra.normalize-minmax',
    'Linear-Algebra.normalize-zscore',
    'Linear-Algebra.normalize-robust',
    'Linear-Algebra.normalize-l1',
    'Linear-Algebra.normalize-l2',
    'Linear-Algebra.normalize-log',
    'Linear-Algebra.angle',
    'Linear-Algebra.projection',
    'Linear-Algebra.orthogonal?',
    'Linear-Algebra.parallel?',
    'Linear-Algebra.collinear?',
    'Linear-Algebra.cosine-similarity',
    'Linear-Algebra.euclidean-distance',
    'Linear-Algebra.euclidean-norm',
    'Linear-Algebra.manhattan-distance',
    'Linear-Algebra.manhattan-norm',
    'Linear-Algebra.hamming-distance',
    'Linear-Algebra.hamming-norm',
    'Linear-Algebra.chebyshev-distance',
    'Linear-Algebra.chebyshev-norm',
    'Linear-Algebra.minkowski-distance',
    'Linear-Algebra.minkowski-norm',
    'Linear-Algebra.cov',
    'Linear-Algebra.corr',
    'Linear-Algebra.spearman-corr',
    'Linear-Algebra.pearson-corr',
    'Linear-Algebra.kendall-tau',
    'Linear-Algebra.autocorrelation',
    'Linear-Algebra.cross-correlation',
    'Linear-Algebra.rref',
    'Linear-Algebra.solve',
    'Linear-Algebra.to-polar',
    'Linear-Algebra.from-polar',
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
    'Number-Theory.collatz-seq',
    'Number-Theory.juggler-seq',
    'Number-Theory.bernoulli-seq',
    'Number-Theory.bernoulli-take-while',
    'Number-Theory.bernoulli-nth',
    'Number-Theory.combinations',
    'Number-Theory.count-combinations',
    'Number-Theory.derangements',
    'Number-Theory.count-derangements',
    'Number-Theory.divisors',
    'Number-Theory.count-divisors',
    'Number-Theory.proper-divisors',
    'Number-Theory.count-proper-divisors',
    'Number-Theory.prime-factors',
    'Number-Theory.count-prime-factors',
    'Number-Theory.distinct-prime-factors',
    'Number-Theory.count-distinct-prime-factors',
    'Number-Theory.factorial',
    'Number-Theory.partitions',
    'Number-Theory.count-partitions',
    'Number-Theory.permutations',
    'Number-Theory.count-permutations',
    'Number-Theory.power-set',
    'Number-Theory.count-power-set',
    'Number-Theory.coprime?',
    'Number-Theory.divisible-by?',
    'Number-Theory.gcd',
    'Number-Theory.lcm',
    'Number-Theory.multinomial',
    'Number-Theory.amicable?',
    'Number-Theory.euler-totient',
    'Number-Theory.mobius',
    'Number-Theory.mertens',
    'Number-Theory.sigma',
    'Number-Theory.carmichael-lambda',
    'Number-Theory.cartesian-product',
    'Number-Theory.perfect-power',
    'Number-Theory.mod-exp',
    'Number-Theory.mod-inv',
    'Number-Theory.extended-gcd',
    'Number-Theory.chinese-remainder',
    'Number-Theory.stirling-first',
    'Number-Theory.stirling-second',
  ] as const,
  random: [
    'Random.random!',
    'Random.random-int!',
    'Random.random-int-inclusive!',
    'Random.random-float!',
    'Random.random-boolean!',
    'Random.random-item!',
    'Random.random-sample!',
    'Random.random-sample-unique!',
    'Random.shuffle!',
    'Random.random-normal!',
    'Random.random-exponential!',
    'Random.random-binomial!',
    'Random.random-poisson!',
    'Random.random-gamma!',
    'Random.random-pareto!',
    'Random.uuid!',
    'Random.random-char!',
    'Random.random-string!',
    'Random.random-id!',
    'Random.random-color!',
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

// Module functions - require import()
export type ModuleExpressionName =
  | MatrixApiName
  | VectorApiName
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
] as const

// Module API function names (require import())
const moduleApiFunctionNames = [
  ...api.matrix,
  ...api.vector,
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
