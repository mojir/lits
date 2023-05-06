import { Builtin } from '../builtin/interface'
import { lookUp } from '../evaluator'
import { ContextStack } from '../evaluator/interface'
import { AstNode } from '../parser/interface'
import { asValue } from '../utils/assertion'
import { AnalyzeAst, AnalyzeResult, UndefinedSymbolEntry } from './interface'

export const analyzeAst: AnalyzeAst = (astNode, contextStack, builtin: Builtin) => {
  const astNodes = Array.isArray(astNode) ? astNode : [astNode]

  const analyzeResult: AnalyzeResult = {
    undefinedSymbols: new Set<UndefinedSymbolEntry>(),
  }

  for (const subNode of astNodes) {
    const result = analyzeAstNode(subNode, contextStack, builtin)
    result.undefinedSymbols.forEach(symbol => analyzeResult.undefinedSymbols.add(symbol))
  }

  return analyzeResult
}

function analyzeAstNode(astNode: AstNode, contextStack: ContextStack, builtin: Builtin): AnalyzeResult {
  const emptySet = new Set<UndefinedSymbolEntry>()
  switch (astNode.type) {
    case `Name`: {
      const lookUpResult = lookUp(astNode, contextStack)
      if (!lookUpResult.builtinFunction && !lookUpResult.contextEntry && !lookUpResult.specialExpression) {
        return { undefinedSymbols: new Set([{ symbol: astNode.value, token: astNode.token }]) }
      }
      return { undefinedSymbols: emptySet }
    }
    case `String`:
    case `Number`:
    case `Modifier`:
    case `ReservedName`:
      return { undefinedSymbols: emptySet }
    case `NormalExpression`: {
      const undefinedSymbols = new Set<UndefinedSymbolEntry>()
      const { expression, name, token } = astNode
      if (typeof name === `string`) {
        const lookUpResult = lookUp({ type: `Name`, value: name, token }, contextStack)
        if (
          lookUpResult.builtinFunction === null &&
          lookUpResult.contextEntry === null &&
          lookUpResult.specialExpression === null
        ) {
          undefinedSymbols.add({ symbol: name, token: astNode.token })
        }
      }
      if (expression) {
        switch (expression.type) {
          case `String`:
          case `Number`:
            break
          case `NormalExpression`:
          case `SpecialExpression`: {
            const subResult = analyzeAstNode(expression, contextStack, builtin)
            subResult.undefinedSymbols.forEach(symbol => undefinedSymbols.add(symbol))
            break
          }
        }
      }

      for (const subNode of astNode.params) {
        const subNodeResult = analyzeAst(subNode, contextStack, builtin)
        subNodeResult.undefinedSymbols.forEach(symbol => undefinedSymbols.add(symbol))
      }
      return { undefinedSymbols }
    }
    case `SpecialExpression`: {
      const specialExpression = asValue(builtin.specialExpressions[astNode.name], astNode.token?.debugInfo)
      const result = specialExpression.analyze(astNode, contextStack, {
        analyzeAst,
        builtin,
      })
      return result
    }
  }
}
