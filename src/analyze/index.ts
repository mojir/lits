import type { Builtin } from '../builtin/interface'
import type { ContextStack } from '../evaluator/ContextStack'
import { AstNodeType } from '../constants/constants'
import type { AstNode } from '../parser/interface'
import { asNonUndefined } from '../typeGuards'
import type { AnalyzeAst, AnalyzeResult, UndefinedSymbolEntry } from './interface'

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
  switch (astNode.t) {
    case AstNodeType.Name: {
      const lookUpResult = contextStack.lookUp(astNode)
      if (lookUpResult === null)
        return { undefinedSymbols: new Set([{ symbol: astNode.v, token: astNode.tkn }]) }

      return { undefinedSymbols: emptySet }
    }
    case AstNodeType.String:
    case AstNodeType.Number:
    case AstNodeType.Modifier:
    case AstNodeType.ReservedName:
      return { undefinedSymbols: emptySet }
    case AstNodeType.NormalExpression: {
      const undefinedSymbols = new Set<UndefinedSymbolEntry>()
      const { e: expression, n: name, tkn: token } = astNode
      if (typeof name === 'string') {
        const lookUpResult = contextStack.lookUp({ t: AstNodeType.Name, v: name, tkn: token })
        if (lookUpResult === null)
          undefinedSymbols.add({ symbol: name, token: astNode.tkn })
      }
      if (expression) {
        switch (expression.t) {
          case AstNodeType.String:
          case AstNodeType.Number:
            break
          case AstNodeType.NormalExpression:
          case AstNodeType.SpecialExpression: {
            const subResult = analyzeAstNode(expression, contextStack, builtin)
            subResult.undefinedSymbols.forEach(symbol => undefinedSymbols.add(symbol))
            break
          }
        }
      }

      for (const subNode of astNode.p) {
        const subNodeResult = analyzeAst(subNode, contextStack, builtin)
        subNodeResult.undefinedSymbols.forEach(symbol => undefinedSymbols.add(symbol))
      }
      return { undefinedSymbols }
    }
    case AstNodeType.SpecialExpression: {
      const specialExpression = asNonUndefined(builtin.specialExpressions[astNode.n], astNode.tkn?.sourceCodeInfo)
      const result = specialExpression.analyze(astNode, contextStack, {
        analyzeAst,
        builtin,
      })
      return result
    }
  }
}
