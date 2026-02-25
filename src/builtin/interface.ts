import type { ContextStack } from '../evaluator/ContextStack'
import type { EvaluateNode, ExecuteFunction } from '../evaluator/interface'
import type { GetUndefinedSymbols, UndefinedSymbols } from '../getUndefinedSymbols'
import type { Any, Arr } from '../interface'
import type {
  SpecialExpressionNode,
} from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
import type { MaybePromise } from '../utils/maybePromise'
import type { SpecialExpressions } from '.'

export type Arity = { min?: number, max?: number }

// --- Data types used in documentation ---

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

// --- Category type ---

export const categoryRecord = {
  'special-expression': true,
  'predicate': true,
  'sequence': true,
  'collection': true,
  'array': true,
  'object': true,
  'string': true,
  'math': true,
  'functional': true,
  'regular-expression': true,
  'bitwise': true,
  'misc': true,
  'meta': true,
  'assertion': true,
  'vector': true,
  'linear-algebra': true,
  'matrix': true,
  'grid': true,
  'number-theory': true,
  'random': true,
  'convert': true,
  'shorthand': true,
  'datatype': true,
} as const

export type Category = keyof typeof categoryRecord

export const categories = Object.keys(categoryRecord) as Category[]

// Categories that are modules (require import)
export const moduleCategories: Category[] = ['assertion', 'bitwise', 'collection', 'convert', 'functional', 'grid', 'linear-algebra', 'math', 'matrix', 'number-theory', 'random', 'sequence', 'string', 'vector']

// Core categories (always available) — special-expression first, rest alphabetical
export const coreCategories: Category[] = ['special-expression', 'array', 'assertion', 'bitwise', 'collection', 'datatype', 'functional', 'math', 'meta', 'misc', 'object', 'predicate', 'regular-expression', 'sequence', 'shorthand', 'string', 'vector']

// --- FunctionDocs types ---

export interface TypedValue {
  type: DataType[] | DataType
  rest?: true
  array?: true
}

export type Argument = TypedValue & {
  description?: string
}

export interface Variant {
  argumentNames: string[]
}

export interface FunctionDocs {
  category: Category
  description: string
  returns: TypedValue
  args: Record<string, Argument>
  variants: Variant[]
  examples: string[]
  seeAlso?: string[]
  hideOperatorForm?: true
  tags?: string[]
}

export interface CustomDocs {
  category: Category
  description: string
  customVariants: string[]
  details?: [string, string, string | undefined][]
  returns?: TypedValue
  examples: string[]
  seeAlso?: string[]
  tags?: string[]
}

export type SpecialExpressionDocs = FunctionDocs | CustomDocs

export function isFunctionDocs(docs: SpecialExpressionDocs): docs is FunctionDocs {
  return 'args' in docs && 'variants' in docs
}

type NormalExpressionEvaluator<T> = (
  params: Arr,
  sourceCodeInfo: SourceCodeInfo | undefined,
  contextStack: ContextStack,
  { executeFunction }: { executeFunction: ExecuteFunction },
) => MaybePromise<T>

export interface BuiltinNormalExpression<T> {
  evaluate: NormalExpressionEvaluator<T>
  pure?: boolean
  name?: string
  arity: Arity
  docs?: FunctionDocs
}

export type BuiltinNormalExpressions = Record<string, BuiltinNormalExpression<Any>>
type BuiltinAllNormalExpressions = BuiltinNormalExpression<Any>[]

interface EvaluateHelpers {
  evaluateNode: EvaluateNode
  builtin: Builtin
  getUndefinedSymbols: GetUndefinedSymbols
}
export interface BuiltinSpecialExpression<T, N extends SpecialExpressionNode> {
  evaluate: (node: N, contextStack: ContextStack, helpers: EvaluateHelpers) => MaybePromise<T>
  evaluateAsNormalExpression?: NormalExpressionEvaluator<T>
  arity: Arity
  docs?: SpecialExpressionDocs
  getUndefinedSymbols: (
    node: N,
    contextStack: ContextStack,
    params: { getUndefinedSymbols: GetUndefinedSymbols, builtin: Builtin, evaluateNode: EvaluateNode },
  ) => UndefinedSymbols
}

export interface Builtin {
  normalExpressions: BuiltinNormalExpressions
  allNormalExpressions: BuiltinAllNormalExpressions
  specialExpressions: SpecialExpressions
}
