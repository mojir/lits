import { describe, expect, it } from 'vitest'
import type {
  ApplyStep,
  EvalStep,
  ParallelResumeStep,
  ParallelStep,
  PerformStep,
  RaceStep,
  Step,
  ValueStep,
} from './step'

describe('step types', () => {
  // Step types are type-only — no runtime code to cover.
  // These tests verify that:
  // 1. All step interfaces are importable and well-typed
  // 2. The discriminated union works correctly
  // 3. Step instances can be created with correct shapes

  it('should discriminate step types via type field', () => {
    const stepTypes: Record<Step['type'], boolean> = {
      Value: true,
      Eval: true,
      Apply: true,
      Perform: true,
      Parallel: true,
      Race: true,
      ParallelResume: true,
    }
    expect(Object.keys(stepTypes)).toHaveLength(7)
  })

  it('should cover all step type discriminants exhaustively', () => {
    function getStepCategory(step: Step): string {
      switch (step.type) {
        case 'Value': return 'result'
        case 'Eval': return 'evaluate'
        case 'Apply': return 'apply'
        case 'Perform': return 'effect'
        case 'Parallel': return 'concurrent'
        case 'Race': return 'concurrent'
        case 'ParallelResume': return 'concurrent'
        default: {
          const _exhaustive: never = step
          throw new Error(`Unhandled step type: ${(_exhaustive as Step).type}`)
        }
      }
    }

    expect(typeof getStepCategory).toBe('function')
  })

  it('should ensure step type names are unique strings', () => {
    const types: Step['type'][] = [
      'Value',
      'Eval',
      'Apply',
      'Perform',
      'Parallel',
      'Race',
      'ParallelResume',
    ]
    const uniqueTypes = new Set(types)
    expect(uniqueTypes.size).toBe(types.length)
    expect(uniqueTypes.size).toBe(7)
  })

  it('should export individual step interfaces for typed access', () => {
    const _value: ValueStep['type'] = 'Value'
    const _eval: EvalStep['type'] = 'Eval'
    const _apply: ApplyStep['type'] = 'Apply'
    const _perform: PerformStep['type'] = 'Perform'
    const _parallel: ParallelStep['type'] = 'Parallel'
    const _race: RaceStep['type'] = 'Race'
    const _parallelResume: ParallelResumeStep['type'] = 'ParallelResume'

    expect(_value).toBe('Value')
    expect(_eval).toBe('Eval')
    expect(_apply).toBe('Apply')
    expect(_perform).toBe('Perform')
    expect(_parallel).toBe('Parallel')
    expect(_race).toBe('Race')
    expect(_parallelResume).toBe('ParallelResume')
  })
})
