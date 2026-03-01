import { describe, expect, it } from 'vitest'
import { NodeTypes } from '../constants/constants'
import { UserDefinedError } from '../errors'
import type { Any } from '../interface'
import { parse } from '../parser'
import type { AstNode, EffectRef, NumberNode, StringNode } from '../parser/types'
import { bindingTargetTypes } from '../parser/types'
import { minifyTokenStream } from '../tokenizer/minifyTokenStream'
import { tokenize } from '../tokenizer/tokenize'
import { EFFECT_SYMBOL } from '../utils/symbols'
import type { ContextStack } from './ContextStack'
import { createContextStack } from './ContextStack'
import type {
  AndFrame,
  ArrayBuildFrame,
  CondFrame,
  ContinuationStack,
  Frame,
  IfBranchFrame,
  LetBindFrame,
  NanCheckFrame,
  ObjectBuildFrame,
  OrFrame,
  QqFrame,
  RecurFrame,
  SequenceFrame,
  ThrowFrame,
  TryCatchFrame,
  TryWithFrame,
} from './frames'
import type { Step } from './step'
import { applyFrame, runSyncTrampoline, stepNode, tick } from './trampoline'

// Helper: parse a Lits program and return its first AST node
function parseFirst(program: string) {
  const tokenStream = tokenize(program, true, undefined)
  const minified = minifyTokenStream(tokenStream, { removeWhiteSpace: true })
  const ast = parse(minified)
  return ast[0]!
}

// Helper: create a fresh, empty context stack
function emptyEnv(): ContextStack {
  return createContextStack()
}

// Helper: apply a frame synchronously (for unit tests where async is not expected)
function applyFrameSync(frame: Frame, value: Any, k: ContinuationStack): Step {
  const result = applyFrame(frame, value, k)
  if (result instanceof Promise) {
    throw new TypeError('Unexpected async result in applyFrameSync')
  }
  return result
}

// Helper: stepNode synchronously (for unit tests where async is not expected)
function stepNodeSync(node: AstNode, env: ContextStack, k: ContinuationStack): Step {
  const result = stepNode(node, env, k)
  if (result instanceof Promise) {
    throw new TypeError('Unexpected async result in stepNodeSync')
  }
  return result
}

// Helper: run the trampoline to completion using runSyncTrampoline
function runTrampoline(step: Step): Any {
  return runSyncTrampoline(step)
}

// ---------------------------------------------------------------------------
// tick — core step engine
// ---------------------------------------------------------------------------

describe('tick', () => {
  it('should return terminal ValueStep unchanged when k is empty', () => {
    const step: Step = { type: 'Value', value: 42, k: [] }
    const next = tick(step)
    expect(next).toEqual({ type: 'Value', value: 42, k: [] })
  })

  it('should apply top frame when ValueStep has non-empty k', () => {
    const thenNode: NumberNode = [NodeTypes.Number, 99]
    const frame: IfBranchFrame = { type: 'IfBranch', thenNode, elseNode: undefined, inverted: false, env: emptyEnv() }
    const step: Step = { type: 'Value', value: true, k: [frame] }
    const next = tick(step) as Step
    expect(next.type).toBe('Eval')
    if (next.type === 'Eval') {
      expect(next.node).toBe(thenNode)
    }
  })

  it('should dispatch EvalStep via stepNode', () => {
    const node = parseFirst('42')
    const step: Step = { type: 'Eval', node, env: emptyEnv(), k: [] }
    const next = tick(step) as Step
    expect(next).toEqual({ type: 'Value', value: 42, k: [] })
  })

  it('should dispatch ApplyStep via applyFrame', () => {
    const frame: NanCheckFrame = { type: 'NanCheck' }
    const step: Step = { type: 'Apply', frame, value: 42, k: [] }
    const next = tick(step) as Step
    expect(next).toEqual({ type: 'Value', value: 42, k: [] })
  })

  it('should throw on PerformStep (effects not implemented yet)', () => {
    const effect: EffectRef = { [EFFECT_SYMBOL]: true, name: 'test.effect' }
    const step: Step = { type: 'Perform', effect, args: [], k: [] }
    expect(() => tick(step)).toThrow('Unhandled effect')
  })

  it('should run a full program via tick loop', () => {
    const node = parseFirst('1 + 2 + 3')
    const initial: Step = { type: 'Eval', node, env: emptyEnv(), k: [] }
    let step: Step | Promise<Step> = initial
    for (let i = 0; i < 1000; i++) {
      if (step instanceof Promise)
        throw new TypeError('Unexpected async')
      if (step.type === 'Value' && step.k.length === 0) {
        expect(step.value).toBe(6)
        return
      }
      step = tick(step)
    }
    throw new Error('tick loop did not terminate')
  })
})

// ---------------------------------------------------------------------------
// runSyncTrampoline / runAsyncTrampoline
// ---------------------------------------------------------------------------

describe('runSyncTrampoline', () => {
  it('should evaluate a simple expression', () => {
    const node = parseFirst('42')
    const initial: Step = { type: 'Eval', node, env: emptyEnv(), k: [] }
    expect(runSyncTrampoline(initial)).toBe(42)
  })

  it('should evaluate a complex expression', () => {
    const node = parseFirst('(1 + 2) * (3 + 4)')
    const initial: Step = { type: 'Eval', node, env: emptyEnv(), k: [] }
    expect(runSyncTrampoline(initial)).toBe(21)
  })

  it('should evaluate a terminal ValueStep immediately', () => {
    const initial: Step = { type: 'Value', value: 'done', k: [] }
    expect(runSyncTrampoline(initial)).toBe('done')
  })
})

// ---------------------------------------------------------------------------
// stepNode — leaf nodes
// ---------------------------------------------------------------------------

describe('stepNode', () => {
  describe('leaf nodes', () => {
    it('should return ValueStep for number literals', () => {
      const node = parseFirst('42')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Value')
      expect((step as { value: Any }).value).toBe(42)
    })

    it('should return ValueStep for negative number literals', () => {
      const node = parseFirst('-3.14')
      const env = emptyEnv()
      const step = stepNodeSync(node, env, [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toBe(-3.14)
      }
    })

    it('should return ValueStep for string literals', () => {
      const node = parseFirst('"hello"')
      const env = emptyEnv()
      const step = stepNodeSync(node, env, [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toBe('hello')
      }
    })

    it('should return ValueStep for empty string', () => {
      const node = parseFirst('""')
      const env = emptyEnv()
      const step = stepNodeSync(node, env, [])
      expect(step).toEqual({ type: 'Value', value: '', k: [] })
    })

    it('should return ValueStep for reserved symbol true', () => {
      const node = parseFirst('true')
      const env = emptyEnv()
      const step = stepNodeSync(node, env, [])
      expect(step).toEqual({ type: 'Value', value: true, k: [] })
    })

    it('should return ValueStep for reserved symbol false', () => {
      const node = parseFirst('false')
      const env = emptyEnv()
      const step = stepNodeSync(node, env, [])
      expect(step).toEqual({ type: 'Value', value: false, k: [] })
    })

    it('should return ValueStep for reserved symbol null', () => {
      const node = parseFirst('null')
      const env = emptyEnv()
      const step = stepNodeSync(node, env, [])
      expect(step).toEqual({ type: 'Value', value: null, k: [] })
    })

    it('should return ValueStep for user-defined symbol', () => {
      const node = parseFirst('x')
      const env = createContextStack({ globalContext: { x: { value: 10 } } })
      const step = stepNodeSync(node, env, [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toBe(10)
      }
    })

    it('should return ValueStep for builtin symbol', () => {
      const node = parseFirst('inc')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Value')
    })
  })

  // ---------------------------------------------------------------------------
  // stepNode — normal expressions
  // ---------------------------------------------------------------------------

  describe('normal expressions', () => {
    it('should push EvalArgsFrame for normal expression with args', () => {
      const node = parseFirst('1 + 2')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.length).toBe(2) // EvalArgsFrame + NanCheckFrame
        expect(step.k[0]!.type).toBe('EvalArgs')
        expect(step.k[1]!.type).toBe('NanCheck')
      }
    })

    it('should dispatch immediately for no-arg normal expression', () => {
      const node = parseFirst('object()')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Value')
    })
  })

  // ---------------------------------------------------------------------------
  // stepNode — special expressions
  // ---------------------------------------------------------------------------

  describe('special expressions', () => {
    it('should push IfBranchFrame for if expression', () => {
      const node = parseFirst('if true then 1 else 2 end')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.length).toBe(1)
        expect(step.k[0]!.type).toBe('IfBranch')
      }
    })

    it('should push AndFrame for && expression', () => {
      const node = parseFirst('&&(true, false)')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.length).toBe(1)
        expect(step.k[0]!.type).toBe('And')
      }
    })

    it('should return true immediately for empty && expression', () => {
      const node = parseFirst('&&()')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toBe(true)
      }
    })

    it('should push OrFrame for || expression', () => {
      const node = parseFirst('||(false, true)')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.length).toBe(1)
        expect(step.k[0]!.type).toBe('Or')
      }
    })

    it('should return false immediately for empty || expression', () => {
      const node = parseFirst('||()')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toBe(false)
      }
    })

    it('should return null immediately for empty cond', () => {
      const node = parseFirst('cond end')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toBe(null)
      }
    })

    it('should push CondFrame for non-empty cond', () => {
      const node = parseFirst('cond case true then 1 end')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.length).toBe(1)
        expect(step.k[0]!.type).toBe('Cond')
      }
    })

    it('should return null for empty block', () => {
      const node = parseFirst('do end')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toBe(null)
      }
    })

    it('should eval single-node block without SequenceFrame', () => {
      const node = parseFirst('do 42 end')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.length).toBe(0)
      }
    })

    it('should push SequenceFrame for multi-node block', () => {
      const node = parseFirst('do 1; 2; 3 end')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.length).toBe(1)
        expect(step.k[0]!.type).toBe('Sequence')
      }
    })

    it('should push LetBindFrame for let expression', () => {
      const node = parseFirst('let x = 10;')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.length).toBe(1)
        expect(step.k[0]!.type).toBe('LetBind')
      }
    })

    it('should push ThrowFrame for throw expression', () => {
      const node = parseFirst('throw("error")')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.length).toBe(1)
        expect(step.k[0]!.type).toBe('Throw')
      }
    })

    it('should return empty array for empty array literal', () => {
      const node = parseFirst('[]')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toEqual([])
      }
    })

    it('should push ArrayBuildFrame for non-empty array literal', () => {
      const node = parseFirst('[1, 2, 3]')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.length).toBe(1)
        expect(step.k[0]!.type).toBe('ArrayBuild')
      }
    })

    it('should return empty object for empty object literal', () => {
      const node = parseFirst('{}')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toEqual({})
      }
    })

    it('should push ObjectBuildFrame for non-empty object literal', () => {
      const node = parseFirst('{ a: 1 }')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.length).toBe(1)
        expect(step.k[0]!.type).toBe('ObjectBuild')
      }
    })

    it('should return a LitsFunction for lambda', () => {
      const node = parseFirst('(x) -> x')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toHaveProperty('functionType', 'UserDefined')
      }
    })

    it('should return boolean for defined?', () => {
      const node = parseFirst('defined?(x)')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toBe(false)
      }
    })

    it('should return true for defined? on an existing symbol', () => {
      const node = parseFirst('defined?(x)')
      const env = createContextStack({ globalContext: { x: { value: 42 } } })
      const step = stepNodeSync(node, env, [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toBe(true)
      }
    })

    it('should push MatchFrame for match expression', () => {
      const node = parseFirst('match 1 case 1 then "one" case 2 then "two" end')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.length).toBe(1)
        expect(step.k[0]!.type).toBe('Match')
      }
    })

    it('should push TryCatchFrame for try expression', () => {
      const node = parseFirst('try 1 catch (e) e end')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.some(f => f.type === 'TryCatch')).toBe(true)
      }
    })

    it('should push QqFrame for ?? expression', () => {
      const node = parseFirst('??(null, 1)')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.length).toBe(1)
        expect(step.k[0]!.type).toBe('Qq')
      }
    })

    it('should push RecurFrame for recur', () => {
      const node = parseFirst('recur(1, 2)')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.length).toBe(1)
        expect(step.k[0]!.type).toBe('Recur')
      }
    })

    it('should throw LitsError for recur outside loop/function', () => {
      const node = parseFirst('recur()')
      expect(() => stepNodeSync(node, emptyEnv(), [])).toThrow('recur called outside of loop or function body')
    })

    it('should push LoopBindFrame for loop expression', () => {
      const node = parseFirst('loop (x = 0) -> x')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.length).toBe(1)
        expect(step.k[0]!.type).toBe('LoopBind')
      }
    })

    it('should push ForLoopFrame for for expression', () => {
      const node = parseFirst('for (x in [1, 2, 3]) -> x')
      const step = stepNodeSync(node, emptyEnv(), [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.some(f => f.type === 'ForLoop')).toBe(true)
      }
    })
  })
})

// ---------------------------------------------------------------------------
// applyFrame — individual frame types
// ---------------------------------------------------------------------------

describe('applyFrame', () => {
  describe('ifBranchFrame', () => {
    it('should evaluate then-branch when condition is truthy', () => {
      const thenNode: NumberNode = [NodeTypes.Number, 1]
      const elseNode: NumberNode = [NodeTypes.Number, 2]
      const frame: IfBranchFrame = {
        type: 'IfBranch',
        thenNode,
        elseNode,
        inverted: false,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, true, [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.node).toBe(thenNode)
      }
    })

    it('should evaluate else-branch when condition is falsy', () => {
      const thenNode: NumberNode = [NodeTypes.Number, 1]
      const elseNode: NumberNode = [NodeTypes.Number, 2]
      const frame: IfBranchFrame = {
        type: 'IfBranch',
        thenNode,
        elseNode,
        inverted: false,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, false, [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.node).toBe(elseNode)
      }
    })

    it('should return null when condition is falsy and no else-branch', () => {
      const thenNode: NumberNode = [NodeTypes.Number, 1]
      const frame: IfBranchFrame = {
        type: 'IfBranch',
        thenNode,
        elseNode: undefined,
        inverted: false,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, false, [])
      expect(step).toEqual({ type: 'Value', value: null, k: [] })
    })

    it('should invert condition for unless', () => {
      const thenNode: NumberNode = [NodeTypes.Number, 1]
      const elseNode: NumberNode = [NodeTypes.Number, 2]
      const frame: IfBranchFrame = {
        type: 'IfBranch',
        thenNode,
        elseNode,
        inverted: true,
        env: emptyEnv(),
      }
      // falsy inverted → truthy → evaluate thenNode
      const step = applyFrameSync(frame, false, [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.node).toBe(thenNode)
      }
    })
  })

  describe('sequenceFrame', () => {
    it('should return value when all nodes evaluated', () => {
      const nodes: NumberNode[] = [[NodeTypes.Number, 1], [NodeTypes.Number, 2]]
      const frame: SequenceFrame = {
        type: 'Sequence',
        nodes,
        index: 2, // past the last node
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, 42, [])
      expect(step).toEqual({ type: 'Value', value: 42, k: [] })
    })

    it('should evaluate next node when more remain', () => {
      const nodes: NumberNode[] = [[NodeTypes.Number, 1], [NodeTypes.Number, 2], [NodeTypes.Number, 3]]
      const frame: SequenceFrame = {
        type: 'Sequence',
        nodes,
        index: 1,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, 'ignored', [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.node).toBe(nodes[1])
      }
    })

    it('should not push frame for last node', () => {
      const nodes: NumberNode[] = [[NodeTypes.Number, 1], [NodeTypes.Number, 2]]
      const frame: SequenceFrame = {
        type: 'Sequence',
        nodes,
        index: 1, // last node index
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, 'ignored', [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.k.length).toBe(0) // no additional frame for last node
      }
    })
  })

  describe('andFrame', () => {
    it('should short-circuit on falsy value', () => {
      const nodes: NumberNode[] = [[NodeTypes.Number, 1], [NodeTypes.Number, 2]]
      const frame: AndFrame = {
        type: 'And',
        nodes,
        index: 1,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, false, [])
      expect(step).toEqual({ type: 'Value', value: false, k: [] })
    })

    it('should continue on truthy value with more nodes', () => {
      const nodes: NumberNode[] = [[NodeTypes.Number, 1], [NodeTypes.Number, 2], [NodeTypes.Number, 3]]
      const frame: AndFrame = {
        type: 'And',
        nodes,
        index: 1,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, true, [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.node).toBe(nodes[1])
      }
    })

    it('should return value when all truthy and at last node', () => {
      const nodes: NumberNode[] = [[NodeTypes.Number, 1]]
      const frame: AndFrame = {
        type: 'And',
        nodes,
        index: 1, // past last
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, 42, [])
      expect(step).toEqual({ type: 'Value', value: 42, k: [] })
    })
  })

  describe('orFrame', () => {
    it('should short-circuit on truthy value', () => {
      const nodes: NumberNode[] = [[NodeTypes.Number, 1], [NodeTypes.Number, 2]]
      const frame: OrFrame = {
        type: 'Or',
        nodes,
        index: 1,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, 42, [])
      expect(step).toEqual({ type: 'Value', value: 42, k: [] })
    })

    it('should continue on falsy value', () => {
      const nodes: NumberNode[] = [[NodeTypes.Number, 1], [NodeTypes.Number, 2], [NodeTypes.Number, 3]]
      const frame: OrFrame = {
        type: 'Or',
        nodes,
        index: 1,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, false, [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.node).toBe(nodes[1])
      }
    })
  })

  describe('qqFrame', () => {
    it('should return value if non-null', () => {
      const nodes: NumberNode[] = [[NodeTypes.Number, 1], [NodeTypes.Number, 2]]
      const frame: QqFrame = {
        type: 'Qq',
        nodes,
        index: 1,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, 42, [])
      expect(step).toEqual({ type: 'Value', value: 42, k: [] })
    })

    it('should advance when value is null', () => {
      const nodes: NumberNode[] = [[NodeTypes.Number, 1], [NodeTypes.Number, 2]]
      const frame: QqFrame = {
        type: 'Qq',
        nodes,
        index: 1,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, null, [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.node).toBe(nodes[1])
      }
    })
  })

  describe('condFrame', () => {
    it('should evaluate body when test is truthy', () => {
      const testNode: NumberNode = [NodeTypes.Number, 1]
      const bodyNode: StringNode = [NodeTypes.String, 'yes']
      const frame: CondFrame = {
        type: 'Cond',
        phase: 'test',
        cases: [[testNode, bodyNode]],
        index: 0,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, true, [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.node).toBe(bodyNode)
      }
    })

    it('should try next case when test is falsy', () => {
      const test1: NumberNode = [NodeTypes.Number, 1]
      const body1: StringNode = [NodeTypes.String, 'a']
      const test2: NumberNode = [NodeTypes.Number, 2]
      const body2: StringNode = [NodeTypes.String, 'b']
      const frame: CondFrame = {
        type: 'Cond',
        phase: 'test',
        cases: [[test1, body1], [test2, body2]],
        index: 0,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, false, [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.node).toBe(test2)
        expect(step.k[0]!.type).toBe('Cond')
      }
    })

    it('should return null when no case matches', () => {
      const testNode: NumberNode = [NodeTypes.Number, 1]
      const bodyNode: StringNode = [NodeTypes.String, 'yes']
      const frame: CondFrame = {
        type: 'Cond',
        phase: 'test',
        cases: [[testNode, bodyNode]],
        index: 0,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, false, [])
      expect(step).toEqual({ type: 'Value', value: null, k: [] })
    })
  })

  describe('arrayBuildFrame', () => {
    it('should add value and advance', () => {
      const nodes: NumberNode[] = [[NodeTypes.Number, 1], [NodeTypes.Number, 2]]
      const frame: ArrayBuildFrame = {
        type: 'ArrayBuild',
        nodes,
        index: 0,
        result: [],
        isSpread: false,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, 10, [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.node).toBe(nodes[1])
      }
    })

    it('should return result when all elements are done', () => {
      const nodes: NumberNode[] = [[NodeTypes.Number, 1]]
      const result = [10]
      const frame: ArrayBuildFrame = {
        type: 'ArrayBuild',
        nodes,
        index: 0,
        result,
        isSpread: false,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, 20, [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toEqual([10, 20])
      }
    })

    it('should spread array values', () => {
      const nodes: NumberNode[] = [[NodeTypes.Number, 1]]
      const frame: ArrayBuildFrame = {
        type: 'ArrayBuild',
        nodes,
        index: 0,
        result: [],
        isSpread: true,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, [1, 2, 3], [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toEqual([1, 2, 3])
      }
    })
  })

  describe('objectBuildFrame', () => {
    it('should store key and evaluate value', () => {
      const keyNode: StringNode = [NodeTypes.String, 'a']
      const valueNode: NumberNode = [NodeTypes.Number, 1]
      const frame: ObjectBuildFrame = {
        type: 'ObjectBuild',
        nodes: [keyNode, valueNode],
        index: 0,
        result: {},
        currentKey: null,
        isSpread: false,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, 'a', [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.node).toBe(valueNode)
        expect(step.k[0]!.type).toBe('ObjectBuild')
        const newFrame = step.k[0] as ObjectBuildFrame
        expect(newFrame.currentKey).toBe('a')
      }
    })

    it('should store value and return object when done', () => {
      const keyNode: StringNode = [NodeTypes.String, 'a']
      const valueNode: NumberNode = [NodeTypes.Number, 1]
      const frame: ObjectBuildFrame = {
        type: 'ObjectBuild',
        nodes: [keyNode, valueNode],
        index: 0,
        result: {},
        currentKey: 'a',
        isSpread: false,
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, 42, [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toEqual({ a: 42 })
      }
    })
  })

  describe('throwFrame', () => {
    it('should throw UserDefinedError', () => {
      const frame: ThrowFrame = {
        type: 'Throw',
      }
      expect(() => applyFrameSync(frame, 'boom', [])).toThrow(UserDefinedError)
    })
  })

  describe('recurFrame', () => {
    it('should throw LitsError when recur has no target frame', () => {
      const nodes: NumberNode[] = [[NodeTypes.Number, 1]]
      const frame: RecurFrame = {
        type: 'Recur',
        nodes,
        index: 1,
        params: [],
        env: emptyEnv(),
      }
      expect(() => applyFrameSync(frame, 42, [])).toThrow('recur called outside of loop or function body')
    })

    it('should continue collecting when more params remain', () => {
      const nodes: NumberNode[] = [[NodeTypes.Number, 1], [NodeTypes.Number, 2]]
      const frame: RecurFrame = {
        type: 'Recur',
        nodes,
        index: 1,
        params: [],
        env: emptyEnv(),
      }
      const step = applyFrameSync(frame, 10, [])
      expect(step.type).toBe('Eval')
      if (step.type === 'Eval') {
        expect(step.node).toBe(nodes[1])
        expect(step.k[0]!.type).toBe('Recur')
      }
    })
  })

  describe('tryCatchFrame', () => {
    it('should pass value through when try body succeeds', () => {
      const frame: TryCatchFrame = { type: 'TryCatch', errorSymbol: 'e', catchNode: [NodeTypes.Number, 0], env: emptyEnv() }
      const step = applyFrameSync(frame, 42, [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toBe(42)
      }
    })
  })

  describe('tryWithFrame', () => {
    it('should pass value through when try body succeeds', () => {
      const frame: TryWithFrame = { type: 'TryWith', handlers: [], env: emptyEnv() }
      const step = applyFrameSync(frame, 42, [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toBe(42)
      }
    })
  })

  describe('nanCheckFrame', () => {
    it('should pass non-NaN values through', () => {
      const frame: NanCheckFrame = { type: 'NanCheck' }
      const step = applyFrameSync(frame, 42, [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toBe(42)
      }
    })

    it('should throw on NaN value', () => {
      const frame: NanCheckFrame = { type: 'NanCheck' }
      expect(() => applyFrameSync(frame, Number.NaN, [])).toThrow('NaN')
    })
  })

  describe('letBindFrame', () => {
    it('should bind a simple value', () => {
      const env = emptyEnv()
      const frame: LetBindFrame = {
        type: 'LetBind',
        target: [bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, 'x'], undefined]],
        env,
      }
      const step = applyFrameSync(frame, 42, [])
      expect(step.type).toBe('Value')
      if (step.type === 'Value') {
        expect(step.value).toBe(42)
      }
    })
  })
})

// ---------------------------------------------------------------------------
// Integration: full trampoline evaluation
// ---------------------------------------------------------------------------

describe('trampoline integration', () => {
  it('should evaluate number literal', () => {
    const node = parseFirst('42')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(42)
  })

  it('should evaluate string literal', () => {
    const node = parseFirst('"hello"')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe('hello')
  })

  it('should evaluate boolean literals', () => {
    expect(runTrampoline(stepNodeSync(parseFirst('true'), emptyEnv(), []))).toBe(true)
    expect(runTrampoline(stepNodeSync(parseFirst('false'), emptyEnv(), []))).toBe(false)
  })

  it('should evaluate null', () => {
    expect(runTrampoline(stepNodeSync(parseFirst('null'), emptyEnv(), []))).toBe(null)
  })

  it('should evaluate addition', () => {
    const node = parseFirst('1 + 2')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(3)
  })

  it('should evaluate nested arithmetic', () => {
    const node = parseFirst('2 * 3 + 4')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(10)
  })

  it('should evaluate if expression', () => {
    const node = parseFirst('if true then 1 else 2 end')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(1)
  })

  it('should evaluate if with false condition', () => {
    const node = parseFirst('if false then 1 else 2 end')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(2)
  })

  it('should evaluate unless expression', () => {
    const node = parseFirst('unless true then 1 else 2 end')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(2)
  })

  it('should evaluate && short circuit', () => {
    expect(runTrampoline(stepNodeSync(parseFirst('&&(false, 1)'), emptyEnv(), []))).toBe(false)
    expect(runTrampoline(stepNodeSync(parseFirst('&&(true, 42)'), emptyEnv(), []))).toBe(42)
  })

  it('should evaluate || short circuit', () => {
    expect(runTrampoline(stepNodeSync(parseFirst('||(42, false)'), emptyEnv(), []))).toBe(42)
    expect(runTrampoline(stepNodeSync(parseFirst('||(false, 99)'), emptyEnv(), []))).toBe(99)
  })

  it('should evaluate cond expression', () => {
    const node = parseFirst('cond case false then 1 case true then 2 end')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(2)
  })

  it('should evaluate ?? nullish coalescing', () => {
    expect(runTrampoline(stepNodeSync(parseFirst('??(null, 42)'), emptyEnv(), []))).toBe(42)
    expect(runTrampoline(stepNodeSync(parseFirst('??(10, 42)'), emptyEnv(), []))).toBe(10)
  })

  it('should evaluate do block', () => {
    const node = parseFirst('do 1; 2; 3 end')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(3)
  })

  it('should evaluate let binding', () => {
    const node = parseFirst('let x = 42;')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(42)
  })

  it('should evaluate array literal', () => {
    const node = parseFirst('[1, 2, 3]')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toEqual([1, 2, 3])
  })

  it('should evaluate object literal', () => {
    const node = parseFirst('{ a: 1, b: 2 }')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toEqual({ a: 1, b: 2 })
  })

  it('should evaluate lambda and immediate call', () => {
    const node = parseFirst('((x) -> x + 1)(10)')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(11)
  })

  it('should evaluate throw', () => {
    const node = parseFirst('throw("test error")')
    expect(() => runTrampoline(stepNodeSync(node, emptyEnv(), []))).toThrow(UserDefinedError)
  })

  it('should evaluate try/catch success', () => {
    const node = parseFirst('try 42 catch (e) 0 end')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(42)
  })

  it('should evaluate string functions', () => {
    const node = parseFirst('str("hello", " ", "world")')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe('hello world')
  })

  it('should evaluate defined?', () => {
    expect(runTrampoline(stepNodeSync(parseFirst('defined?(inc)'), emptyEnv(), []))).toBe(true)
    expect(runTrampoline(stepNodeSync(parseFirst('defined?(xyz)'), emptyEnv(), []))).toBe(false)
  })

  it('should evaluate match expression', () => {
    const node = parseFirst('match 2 case 1 then "one" case 2 then "two" end')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe('two')
  })

  it('should evaluate user-defined variable', () => {
    const node = parseFirst('x')
    const env = createContextStack({ globalContext: { x: { value: 'hello' } } })
    const step = stepNodeSync(node, env, [])
    expect(runTrampoline(step)).toBe('hello')
  })

  it('should evaluate loop with recur', () => {
    const node = parseFirst('loop (x = 0) -> if x < 5 then recur(x + 1) else x end')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(5)
  })

  it('should evaluate for loop', () => {
    const node = parseFirst('for (x in [1, 2, 3]) -> x * 2')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toEqual([2, 4, 6])
  })

  it('should evaluate doseq', () => {
    const node = parseFirst('doseq (x in [1, 2, 3]) -> x')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(null)
  })

  it('should evaluate nested function calls', () => {
    const node = parseFirst('(1 + 2) + (3 + 4)')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(10)
  })

  it('should evaluate array as function', () => {
    const node = parseFirst('[1, 2, 3](1)')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(2)
  })

  it('should evaluate object as function', () => {
    const node = parseFirst('{ a: 1, b: 2 }("b")')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(2)
  })

  it('should evaluate string as function', () => {
    const node = parseFirst('"a"({ a: 42 })')
    const step = stepNodeSync(node, emptyEnv(), [])
    expect(runTrampoline(step)).toBe(42)
  })
})
