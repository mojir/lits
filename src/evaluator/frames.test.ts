import { describe, expect, it } from 'vitest'
import type {
  AndFrame,
  ArrayBuildFrame,
  BindingDefaultFrame,
  CallFnFrame,
  CondFrame,
  ContinuationStack,
  EffectResumeFrame,
  EvalArgsFrame,
  FnBodyFrame,
  ForBindingLevelState,
  ForLoopFrame,
  Frame,
  IfBranchFrame,
  LetBindFrame,
  LoopBindFrame,
  LoopIterateFrame,
  MatchFrame,
  NanCheckFrame,
  ObjectBuildFrame,
  OrFrame,
  PerformArgsFrame,
  QqFrame,
  RecurFrame,
  SequenceFrame,
  ThrowFrame,
  TryCatchFrame,
  TryWithFrame,
} from './frames'

describe('frame types', () => {
  // All 20 frame types are type-only — no runtime code to cover.
  // These tests verify that:
  // 1. All frame interfaces are importable and well-typed
  // 2. The discriminated union works correctly
  // 3. Frame instances can be created with correct shapes

  it('should discriminate frame types via type field', () => {
    // Verify that each Frame.type is unique and the union discriminant works.
    // We build a map from type string to boolean.
    const frameTypes: Record<Frame['type'], boolean> = {
      Sequence: true,
      IfBranch: true,
      Cond: true,
      Match: true,
      And: true,
      Or: true,
      Qq: true,
      ArrayBuild: true,
      ObjectBuild: true,
      LetBind: true,
      LoopBind: true,
      LoopIterate: true,
      ForLoop: true,
      Throw: true,
      Recur: true,
      PerformArgs: true,
      TryCatch: true,
      TryWith: true,
      EffectResume: true,
      EvalArgs: true,
      CallFn: true,
      FnBody: true,
      BindingDefault: true,
      NanCheck: true,
    }
    expect(Object.keys(frameTypes)).toHaveLength(24)
  })

  it('should support ContinuationStack as Frame array', () => {
    const stack: ContinuationStack = []
    expect(Array.isArray(stack)).toBe(true)
    expect(stack).toHaveLength(0)
  })

  it('should cover all frame type discriminants exhaustively', () => {
    // This function uses the TypeScript exhaustiveness check pattern.
    // If a frame type is added but not handled, TypeScript will flag it.
    function getFrameCategory(frame: Frame): string {
      switch (frame.type) {
        case 'Sequence': return 'flow'
        case 'IfBranch': return 'branch'
        case 'Cond': return 'branch'
        case 'Match': return 'branch'
        case 'And': return 'short-circuit'
        case 'Or': return 'short-circuit'
        case 'Qq': return 'short-circuit'
        case 'ArrayBuild': return 'collection'
        case 'ObjectBuild': return 'collection'
        case 'LetBind': return 'binding'
        case 'LoopBind': return 'binding'
        case 'LoopIterate': return 'binding'
        case 'ForLoop': return 'binding'
        case 'Throw': return 'control'
        case 'Recur': return 'control'
        case 'PerformArgs': return 'control'
        case 'TryCatch': return 'exception'
        case 'TryWith': return 'effect'
        case 'EffectResume': return 'effect'
        case 'EvalArgs': return 'call'
        case 'CallFn': return 'call'
        case 'FnBody': return 'call'
        case 'BindingDefault': return 'destructure'
        case 'NanCheck': return 'post'
        default: {
          // Exhaustiveness check: if this line is reached, a frame type is missing
          const _exhaustive: never = frame
          throw new Error(`Unhandled frame type: ${(_exhaustive as Frame).type}`)
        }
      }
    }

    // Just verify the function is well-typed (compilation is the real test)
    expect(typeof getFrameCategory).toBe('function')
  })

  it('should allow ForBindingLevelState to be used in ForLoopFrame', () => {
    const levelState: ForBindingLevelState = {
      collection: [1, 2, 3],
      index: 0,
    }
    expect(levelState.collection).toEqual([1, 2, 3])
    expect(levelState.index).toBe(0)
  })

  it('should ensure frame type names are unique strings', () => {
    // Build the set of all type discriminants.
    // TypeScript's type system ensures uniqueness, but we also verify at runtime.
    const types: Frame['type'][] = [
      'Sequence',
      'IfBranch',
      'Cond',
      'Match',
      'And',
      'Or',
      'Qq',
      'ArrayBuild',
      'ObjectBuild',
      'LetBind',
      'LoopBind',
      'LoopIterate',
      'ForLoop',
      'Throw',
      'Recur',
      'PerformArgs',
      'TryCatch',
      'TryWith',
      'EffectResume',
      'EvalArgs',
      'CallFn',
      'FnBody',
      'BindingDefault',
      'NanCheck',
    ]
    const uniqueTypes = new Set(types)
    expect(uniqueTypes.size).toBe(types.length)
    expect(uniqueTypes.size).toBe(24)
  })

  it('should export individual frame interfaces for typed access', () => {
    // Verify all individual frame types are importable (compile-time check).
    // We use type assertions to verify the type field matches.
    const _sequence: SequenceFrame['type'] = 'Sequence'
    const _ifBranch: IfBranchFrame['type'] = 'IfBranch'
    const _cond: CondFrame['type'] = 'Cond'
    const _match: MatchFrame['type'] = 'Match'
    const _and: AndFrame['type'] = 'And'
    const _or: OrFrame['type'] = 'Or'
    const _qq: QqFrame['type'] = 'Qq'
    const _arrayBuild: ArrayBuildFrame['type'] = 'ArrayBuild'
    const _objectBuild: ObjectBuildFrame['type'] = 'ObjectBuild'
    const _letBind: LetBindFrame['type'] = 'LetBind'
    const _loopBind: LoopBindFrame['type'] = 'LoopBind'
    const _loopIterate: LoopIterateFrame['type'] = 'LoopIterate'
    const _forLoop: ForLoopFrame['type'] = 'ForLoop'
    const _throw: ThrowFrame['type'] = 'Throw'
    const _recur: RecurFrame['type'] = 'Recur'
    const _performArgs: PerformArgsFrame['type'] = 'PerformArgs'
    const _tryCatch: TryCatchFrame['type'] = 'TryCatch'
    const _tryWith: TryWithFrame['type'] = 'TryWith'
    const _effectResume: EffectResumeFrame['type'] = 'EffectResume'
    const _evalArgs: EvalArgsFrame['type'] = 'EvalArgs'
    const _callFn: CallFnFrame['type'] = 'CallFn'
    const _fnBody: FnBodyFrame['type'] = 'FnBody'
    const _bindingDefault: BindingDefaultFrame['type'] = 'BindingDefault'
    const _nanCheck: NanCheckFrame['type'] = 'NanCheck'

    // All type assignments above are verified by TypeScript at compile time.
    // If any type field doesn't match, compilation fails.
    expect(_sequence).toBe('Sequence')
    expect(_ifBranch).toBe('IfBranch')
    expect(_cond).toBe('Cond')
    expect(_match).toBe('Match')
    expect(_and).toBe('And')
    expect(_or).toBe('Or')
    expect(_qq).toBe('Qq')
    expect(_arrayBuild).toBe('ArrayBuild')
    expect(_objectBuild).toBe('ObjectBuild')
    expect(_letBind).toBe('LetBind')
    expect(_loopBind).toBe('LoopBind')
    expect(_loopIterate).toBe('LoopIterate')
    expect(_forLoop).toBe('ForLoop')
    expect(_throw).toBe('Throw')
    expect(_recur).toBe('Recur')
    expect(_performArgs).toBe('PerformArgs')
    expect(_tryCatch).toBe('TryCatch')
    expect(_tryWith).toBe('TryWith')
    expect(_effectResume).toBe('EffectResume')
    expect(_evalArgs).toBe('EvalArgs')
    expect(_callFn).toBe('CallFn')
    expect(_fnBody).toBe('FnBody')
    expect(_bindingDefault).toBe('BindingDefault')
    expect(_nanCheck).toBe('NanCheck')
  })
})
