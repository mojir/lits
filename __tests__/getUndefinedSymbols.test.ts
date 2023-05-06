import { Lits } from '../src'
import { findUndefinedSymbols } from '../src/analyze/undefinedSymbols'
import { builtin } from '../src/builtin'
import { ContextStack } from '../src/ContextStack'
import { AstNode } from '../src/parser/interface'
import { getUndefinedSymbolNames } from './testUtils'

describe(`getUndefinedSymbols.`, () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    test(`example`, () => {
      const program = `(+ a b ::number)`
      const tokens = lits.tokenize(program)
      const ast = lits.parse(tokens)
      const astNode = ast.body[0] as AstNode
      const analyzeResult = findUndefinedSymbols(astNode, ContextStack.create(), builtin)
      expect(getUndefinedSymbolNames(analyzeResult)).toEqual(new Set([`a`, `b`]))
    })
  }
})
