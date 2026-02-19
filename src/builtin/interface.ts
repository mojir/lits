import type { ContextStack } from '../evaluator/ContextStack'
import type { EvaluateNode, ExecuteFunction } from '../evaluator/interface'
import type { GetUndefinedSymbols, UndefinedSymbols } from '../getUndefinedSymbols'
import type { Any, Arr } from '../interface'
import type {
  SpecialExpressionNode,
} from '../parser/types'
import type { SourceCodeInfo } from '../tokenizer/token'
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
  'Number-Theory': true,
  'Random': true,
  'Shorthand': true,
  'Datatype': true,
} as const

export type Category = keyof typeof categoryRecord

export const categories = Object.keys(categoryRecord) as Category[]

// Categories that are namespaces (require import)
export const namespaceCategories: Category[] = ['Vector', 'Linear Algebra', 'Matrix', 'Grid', 'Number-Theory', 'Random', 'Assert']

// Core categories (always available)
export const coreCategories = categories.filter(c => !namespaceCategories.includes(c))

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

export function isCustomDocs(docs: SpecialExpressionDocs): docs is CustomDocs {
  return 'customVariants' in docs
}

export type NormalExpressionEvaluator<T> = (
  params: Arr,
  sourceCodeInfo: SourceCodeInfo | undefined,
  contextStack: ContextStack,
  { executeFunction }: { executeFunction: ExecuteFunction },
) => T

export interface BuiltinNormalExpression<T> {
  evaluate: NormalExpressionEvaluator<T>
  name?: string
  arity: Arity
  docs?: FunctionDocs
}

export type BuiltinNormalExpressions = Record<string, BuiltinNormalExpression<Any>>
export type BuiltinAllNormalExpressions = BuiltinNormalExpression<Any>[]

export interface EvaluateHelpers {
  evaluateNode: EvaluateNode
  builtin: Builtin
  getUndefinedSymbols: GetUndefinedSymbols
}
export interface BuiltinSpecialExpression<T, N extends SpecialExpressionNode> {
  evaluate: (node: N, contextStack: ContextStack, helpers: EvaluateHelpers) => T
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
