import type { Context } from '../../evaluator/interface'
import type { Any } from '../../interface'
import type { AstNode, SpecialExpressionNode, SymbolNode } from '../../parser/types'
import { joinSets } from '../../utils'
import { tryCatch } from '../../utils/maybePromise'
import type { BuiltinSpecialExpression, CustomDocs } from '../interface'
import type { specialExpressionTypes } from '../specialExpressionTypes'

export type WithHandler = [AstNode, AstNode] // [effectExpr, handlerFn]

export type TryNode = SpecialExpressionNode<[typeof specialExpressionTypes['try'], AstNode, SymbolNode | undefined, AstNode | undefined, WithHandler[]]>

const docs: CustomDocs = {
  category: 'special-expression',
  customVariants: [
    'try { try-body } catch { catch-body }',
    'try { try-body } catch(error) { catch-body }',
    'try { try-body } with { case effect then handler ... } end',
    'try { try-body } with { case effect then handler ... } catch { catch-body }',
    'try { try-body } with { case effect then handler ... } catch(error) { catch-body }',
  ],
  details: [
    ['try-body', 'expressions', 'The expressions to try.'],
    ['error', 'symbol', 'The error variable to bind.'],
    ['catch-body', 'expression', 'The expressions to evaluate if the try-body throws an error.'],
    ['effect', 'expression', 'An expression evaluating to an effect value.'],
    ['handler', 'expression', 'A function that handles the effect. Its return value resumes the perform call.'],
  ],
  description: 'Executes `try-body`. If that throws, the `catch-body` gets executed. '
    + 'Effect handlers can be installed via `with` to intercept `perform` calls. See examples for details.',
  examples: [
    `
try
  2 / 4
catch
  "Oops!"
end`,
    `
try
  foo()
catch(error)
  "Error: " ++ error.message
end`,
    `
try
  foo()
catch
  42
end`,
  ],
}

export const trySpecialExpression: BuiltinSpecialExpression<Any, TryNode> = {
  arity: {},
  docs,
  evaluate: (node, contextStack, { evaluateNode }) => {
    const [, tryExpression, errorSymbol, catchExpression] = node[1]
    // Note: withHandlers (node[1][4]) will be used in Phase 2 for effect dispatch.
    // For now, only the try/catch behavior is active.
    if (catchExpression) {
      return tryCatch(
        () => evaluateNode(tryExpression, contextStack),
        (error) => {
          const newContext: Context = errorSymbol
            ? {
                [errorSymbol[1]]: { value: error as Any },
              }
            : {}
          return evaluateNode(catchExpression, contextStack.create(newContext))
        },
      )
    }
    // with-only form (or empty — will be validated in Phase 2)
    return evaluateNode(tryExpression, contextStack)
  },
  getUndefinedSymbols: (node, contextStack, { getUndefinedSymbols, builtin, evaluateNode }) => {
    const [, tryExpression, errorSymbol, catchExpression, withHandlers] = node[1]
    const tryResult = getUndefinedSymbols([tryExpression], contextStack, builtin, evaluateNode)

    let catchResult = new Set<string>()
    if (catchExpression) {
      const newContext: Context = errorSymbol
        ? {
            [errorSymbol[1]]: { value: true },
          }
        : {}
      catchResult = getUndefinedSymbols([catchExpression], contextStack.create(newContext), builtin, evaluateNode)
    }

    // Collect undefined symbols from with-handler expressions
    let withResult = new Set<string>()
    for (const [effectExpr, handlerFn] of withHandlers) {
      const effectResult = getUndefinedSymbols([effectExpr], contextStack, builtin, evaluateNode)
      const handlerResult = getUndefinedSymbols([handlerFn], contextStack, builtin, evaluateNode)
      withResult = joinSets(withResult, effectResult, handlerResult)
    }

    return joinSets(tryResult, catchResult, withResult)
  },
}
