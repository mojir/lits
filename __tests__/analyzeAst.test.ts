import { describe, expect, it } from 'vitest'
import { Lits } from '../src'
import { createContextStack } from '../src/Lits/Lits'
import { analyzeAst } from '../src/analyze'
import { builtin } from '../src/builtin'
import type { AstNode } from '../src/parser/interface'
import { getUndefinedSymbolNames } from './testUtils'

describe('analyzeAst.', () => {
  for (const lits of [new Lits(), new Lits({ debug: true })]) {
    it('example', () => {
      const program = '(+ a b)'
      const tokens = lits.tokenize(program)
      const ast = lits.parse(tokens)
      const astNode = ast.b[0] as AstNode
      const analyzeResult = analyzeAst(astNode, createContextStack(), builtin)
      expect(getUndefinedSymbolNames(analyzeResult)).toEqual(new Set(['a', 'b']))
    })
  }
})
