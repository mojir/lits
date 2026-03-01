import { describe, expect, it } from 'vitest'
import { EFFECT_SYMBOL, FUNCTION_SYMBOL, REGEXP_SYMBOL } from '../utils/symbols'
import type { Any } from '../interface'
import type {
  CompFunction,
  ComplementFunction,
  ConstantlyFunction,
  EffectRef,
  EveryPredFunction,
  FNullFunction,
  JuxtFunction,
  ModuleFunction,
  NativeJsFunction,
  NormalBuiltinFunction,
  PartialFunction,
  SomePredFunction,
  SpecialBuiltinFunction,
  UserDefinedFunction,
} from '../parser/types'
import { describeSerializationIssue, isSerializable } from './serialization'

function makeNativeJsFunction(name?: string): NativeJsFunction {
  const fn: NativeJsFunction = {
    [FUNCTION_SYMBOL]: true,
    functionType: 'NativeJsFunction',
    name: name ?? 'testFn',
    nativeFn: { fn: () => 42 },
    docString: '',
    arity: {},
  }
  return fn
}

function makeUserDefinedFunction(): UserDefinedFunction {
  const fn: UserDefinedFunction = {
    [FUNCTION_SYMBOL]: true,
    functionType: 'UserDefined',
    name: 'myFn',
    evaluatedfunction: [[], [], {}],
    docString: '',
    arity: {},
  }
  return fn
}

function makeBuiltinFunction(): NormalBuiltinFunction {
  const fn: NormalBuiltinFunction = {
    [FUNCTION_SYMBOL]: true,
    functionType: 'Builtin',
    normalBuiltinSymbolType: 0,
    name: '+',
    arity: {},
  }
  return fn
}

function makeSpecialBuiltinFunction(): SpecialBuiltinFunction {
  const fn: SpecialBuiltinFunction = {
    [FUNCTION_SYMBOL]: true,
    functionType: 'SpecialBuiltin',
    specialBuiltinSymbolType: 0 as SpecialBuiltinFunction['specialBuiltinSymbolType'],
    arity: {},
  }
  return fn
}

function makeModuleFunction(): ModuleFunction {
  const fn: ModuleFunction = {
    [FUNCTION_SYMBOL]: true,
    functionType: 'Module',
    moduleName: 'math',
    functionName: 'sin',
    arity: {},
  }
  return fn
}

describe('isSerializable', () => {
  describe('primitives', () => {
    it('should accept null', () => {
      expect(isSerializable(null)).toBe(true)
    })
    it('should accept numbers', () => {
      expect(isSerializable(0)).toBe(true)
      expect(isSerializable(42)).toBe(true)
      expect(isSerializable(-3.14)).toBe(true)
    })
    it('should accept strings', () => {
      expect(isSerializable('')).toBe(true)
      expect(isSerializable('hello')).toBe(true)
    })
    it('should accept booleans', () => {
      expect(isSerializable(true)).toBe(true)
      expect(isSerializable(false)).toBe(true)
    })
  })

  describe('arrays', () => {
    it('should accept empty arrays', () => {
      expect(isSerializable([])).toBe(true)
    })
    it('should accept arrays of primitives', () => {
      expect(isSerializable([1, 'two', true, null])).toBe(true)
    })
    it('should accept nested arrays', () => {
      expect(isSerializable([[1, 2], [3, [4, 5]]])).toBe(true)
    })
    it('should reject arrays containing NativeJsFunction', () => {
      const arr: Any = [1, makeNativeJsFunction()]
      expect(isSerializable(arr)).toBe(false)
    })
  })

  describe('objects', () => {
    it('should accept empty objects', () => {
      expect(isSerializable({})).toBe(true)
    })
    it('should accept objects with primitive values', () => {
      expect(isSerializable({ a: 1, b: 'two', c: true, d: null })).toBe(true)
    })
    it('should accept nested objects', () => {
      expect(isSerializable({ a: { b: { c: 42 } } })).toBe(true)
    })
    it('should reject objects containing NativeJsFunction', () => {
      const obj: Any = { fn: makeNativeJsFunction() }
      expect(isSerializable(obj)).toBe(false)
    })
  })

  describe('regularExpression', () => {
    it('should accept regular expressions', () => {
      const re: Any = { [REGEXP_SYMBOL]: true, s: 'abc', f: 'gi' }
      expect(isSerializable(re)).toBe(true)
    })
  })

  describe('effectRef', () => {
    it('should accept EffectRef values', () => {
      const ref: EffectRef = { [EFFECT_SYMBOL]: true, name: 'llm.complete' }
      expect(isSerializable(ref as Any)).toBe(true)
    })

    it('should accept EffectRef with dotted name', () => {
      const ref: EffectRef = { [EFFECT_SYMBOL]: true, name: 'com.myco.human.approve' }
      expect(isSerializable(ref as Any)).toBe(true)
    })

    it('should accept arrays containing EffectRef', () => {
      const ref: EffectRef = { [EFFECT_SYMBOL]: true, name: 'lits.log' }
      expect(isSerializable([1, ref] as Any)).toBe(true)
    })

    it('should accept objects containing EffectRef', () => {
      const ref: EffectRef = { [EFFECT_SYMBOL]: true, name: 'lits.now' }
      const obj: Any = { eff: ref }
      expect(isSerializable(obj)).toBe(true)
    })
  })

  describe('litsFunction types', () => {
    it('should accept UserDefinedFunction', () => {
      expect(isSerializable(makeUserDefinedFunction() as Any)).toBe(true)
    })
    it('should accept NormalBuiltinFunction', () => {
      expect(isSerializable(makeBuiltinFunction() as Any)).toBe(true)
    })
    it('should accept SpecialBuiltinFunction', () => {
      expect(isSerializable(makeSpecialBuiltinFunction() as Any)).toBe(true)
    })
    it('should accept ModuleFunction', () => {
      expect(isSerializable(makeModuleFunction() as Any)).toBe(true)
    })
    it('should reject NativeJsFunction', () => {
      expect(isSerializable(makeNativeJsFunction() as Any)).toBe(false)
    })
  })

  describe('compound function types', () => {
    it('should accept PartialFunction with serializable inner function', () => {
      const partial: PartialFunction = {
        [FUNCTION_SYMBOL]: true,
        functionType: 'Partial',
        function: makeUserDefinedFunction(),
        params: [1, 'two'],
        placeholders: [2],
        arity: {},
      }
      expect(isSerializable(partial as Any)).toBe(true)
    })

    it('should reject PartialFunction with NativeJsFunction', () => {
      const partial: PartialFunction = {
        [FUNCTION_SYMBOL]: true,
        functionType: 'Partial',
        function: makeNativeJsFunction(),
        params: [1],
        placeholders: [1],
        arity: {},
      }
      expect(isSerializable(partial as Any)).toBe(false)
    })

    it('should accept CompFunction with serializable functions', () => {
      const comp: CompFunction = {
        [FUNCTION_SYMBOL]: true,
        functionType: 'Comp',
        params: [makeUserDefinedFunction(), makeBuiltinFunction()],
        arity: {},
      }
      expect(isSerializable(comp as Any)).toBe(true)
    })

    it('should reject CompFunction with NativeJsFunction', () => {
      const comp: CompFunction = {
        [FUNCTION_SYMBOL]: true,
        functionType: 'Comp',
        params: [makeUserDefinedFunction(), makeNativeJsFunction()],
        arity: {},
      }
      expect(isSerializable(comp as Any)).toBe(false)
    })

    it('should accept ConstantlyFunction with serializable value', () => {
      const constantly: ConstantlyFunction = {
        [FUNCTION_SYMBOL]: true,
        functionType: 'Constantly',
        value: 42,
        arity: {},
      }
      expect(isSerializable(constantly as Any)).toBe(true)
    })

    it('should accept JuxtFunction with serializable functions', () => {
      const juxt: JuxtFunction = {
        [FUNCTION_SYMBOL]: true,
        functionType: 'Juxt',
        params: [makeUserDefinedFunction()],
        arity: {},
      }
      expect(isSerializable(juxt as Any)).toBe(true)
    })

    it('should accept ComplementFunction with serializable function', () => {
      const complement: ComplementFunction = {
        [FUNCTION_SYMBOL]: true,
        functionType: 'Complement',
        function: makeUserDefinedFunction(),
        arity: {},
      }
      expect(isSerializable(complement as Any)).toBe(true)
    })

    it('should reject ComplementFunction with NativeJsFunction', () => {
      const complement: ComplementFunction = {
        [FUNCTION_SYMBOL]: true,
        functionType: 'Complement',
        function: makeNativeJsFunction(),
        arity: {},
      }
      expect(isSerializable(complement as Any)).toBe(false)
    })

    it('should accept EveryPredFunction with serializable predicates', () => {
      const everyPred: EveryPredFunction = {
        [FUNCTION_SYMBOL]: true,
        functionType: 'EveryPred',
        params: [makeUserDefinedFunction()],
        arity: {},
      }
      expect(isSerializable(everyPred as Any)).toBe(true)
    })

    it('should accept SomePredFunction with serializable predicates', () => {
      const somePred: SomePredFunction = {
        [FUNCTION_SYMBOL]: true,
        functionType: 'SomePred',
        params: [makeUserDefinedFunction()],
        arity: {},
      }
      expect(isSerializable(somePred as Any)).toBe(true)
    })

    it('should accept FNullFunction with serializable inner function and params', () => {
      const fnull: FNullFunction = {
        [FUNCTION_SYMBOL]: true,
        functionType: 'Fnull',
        function: makeUserDefinedFunction(),
        params: [1, 'default'],
        arity: {},
      }
      expect(isSerializable(fnull as Any)).toBe(true)
    })

    it('should reject FNullFunction with NativeJsFunction', () => {
      const fnull: FNullFunction = {
        [FUNCTION_SYMBOL]: true,
        functionType: 'Fnull',
        function: makeNativeJsFunction(),
        params: [1],
        arity: {},
      }
      expect(isSerializable(fnull as Any)).toBe(false)
    })
  })

  describe('circular references', () => {
    it('should return false for circular arrays', () => {
      const arr: unknown[] = [1, 2]
      arr.push(arr)
      expect(isSerializable(arr)).toBe(false)
    })

    it('should return false for circular objects', () => {
      const obj: Record<string, unknown> = { a: 1 }
      obj.self = obj
      expect(isSerializable(obj)).toBe(false)
    })
  })
})

describe('describeSerializationIssue', () => {
  it('should return null for serializable primitives', () => {
    expect(describeSerializationIssue(null)).toBeNull()
    expect(describeSerializationIssue(42)).toBeNull()
    expect(describeSerializationIssue('hello')).toBeNull()
    expect(describeSerializationIssue(true)).toBeNull()
  })

  it('should return null for serializable functions', () => {
    expect(describeSerializationIssue(makeUserDefinedFunction() as Any)).toBeNull()
    expect(describeSerializationIssue(makeBuiltinFunction() as Any)).toBeNull()
  })

  it('should describe NativeJsFunction issue', () => {
    const issue = describeSerializationIssue(makeNativeJsFunction('myFn') as Any)
    expect(issue).toContain('NativeJsFunction')
    expect(issue).toContain('myFn')
  })

  it('should describe nested NativeJsFunction issue with path', () => {
    const value: Any = { data: [1, makeNativeJsFunction('deepFn')] }
    const issue = describeSerializationIssue(value)
    expect(issue).toContain('value.data[1]')
    expect(issue).toContain('NativeJsFunction')
    expect(issue).toContain('deepFn')
  })

  it('should describe NativeJsFunction inside PartialFunction', () => {
    const partial: PartialFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'Partial',
      function: makeNativeJsFunction('innerFn'),
      params: [],
      placeholders: [],
      arity: {},
    }
    const issue = describeSerializationIssue(partial as Any)
    expect(issue).toContain('value.function')
    expect(issue).toContain('NativeJsFunction')
  })

  it('should return null for fully serializable compound types', () => {
    const partial: PartialFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'Partial',
      function: makeUserDefinedFunction(),
      params: [1, 'two'],
      placeholders: [2],
      arity: {},
    }
    expect(describeSerializationIssue(partial as Any)).toBeNull()
  })

  it('should describe NativeJsFunction inside PartialFunction params', () => {
    const partial: PartialFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'Partial',
      function: makeUserDefinedFunction(),
      params: [1, makeNativeJsFunction('paramFn')],
      placeholders: [],
      arity: {},
    }
    const issue = describeSerializationIssue(partial as Any)
    expect(issue).toContain('value.params[1]')
    expect(issue).toContain('NativeJsFunction')
  })

  it('should describe NativeJsFunction inside CompFunction', () => {
    const comp: CompFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'Comp',
      params: [makeUserDefinedFunction(), makeNativeJsFunction('compFn')],
      arity: {},
    }
    const issue = describeSerializationIssue(comp as Any)
    expect(issue).toContain('value.params[1]')
    expect(issue).toContain('NativeJsFunction')
  })

  it('should return null for serializable CompFunction', () => {
    const comp: CompFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'Comp',
      params: [makeUserDefinedFunction()],
      arity: {},
    }
    expect(describeSerializationIssue(comp as Any)).toBeNull()
  })

  it('should describe NativeJsFunction inside ComplementFunction', () => {
    const complement: ComplementFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'Complement',
      function: makeNativeJsFunction('complFn'),
      arity: {},
    }
    const issue = describeSerializationIssue(complement as Any)
    expect(issue).toContain('value.function')
    expect(issue).toContain('NativeJsFunction')
  })

  it('should return null for serializable ComplementFunction', () => {
    const complement: ComplementFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'Complement',
      function: makeUserDefinedFunction(),
      arity: {},
    }
    expect(describeSerializationIssue(complement as Any)).toBeNull()
  })

  it('should describe NativeJsFunction inside ConstantlyFunction', () => {
    const constantly: ConstantlyFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'Constantly',
      value: makeNativeJsFunction('constFn') as Any,
      arity: {},
    }
    const issue = describeSerializationIssue(constantly as Any)
    expect(issue).toContain('value.value')
    expect(issue).toContain('NativeJsFunction')
  })

  it('should return null for serializable ConstantlyFunction', () => {
    const constantly: ConstantlyFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'Constantly',
      value: 42,
      arity: {},
    }
    expect(describeSerializationIssue(constantly as Any)).toBeNull()
  })

  it('should describe NativeJsFunction inside JuxtFunction', () => {
    const juxt: JuxtFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'Juxt',
      params: [makeNativeJsFunction('juxtFn')],
      arity: {},
    }
    const issue = describeSerializationIssue(juxt as Any)
    expect(issue).toContain('value.params[0]')
    expect(issue).toContain('NativeJsFunction')
  })

  it('should return null for serializable JuxtFunction', () => {
    const juxt: JuxtFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'Juxt',
      params: [makeUserDefinedFunction()],
      arity: {},
    }
    expect(describeSerializationIssue(juxt as Any)).toBeNull()
  })

  it('should describe NativeJsFunction inside EveryPredFunction', () => {
    const everyPred: EveryPredFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'EveryPred',
      params: [makeNativeJsFunction('epFn')],
      arity: {},
    }
    const issue = describeSerializationIssue(everyPred as Any)
    expect(issue).toContain('value.params[0]')
    expect(issue).toContain('NativeJsFunction')
  })

  it('should return null for serializable EveryPredFunction', () => {
    const everyPred: EveryPredFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'EveryPred',
      params: [makeUserDefinedFunction()],
      arity: {},
    }
    expect(describeSerializationIssue(everyPred as Any)).toBeNull()
  })

  it('should describe NativeJsFunction inside SomePredFunction', () => {
    const somePred: SomePredFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'SomePred',
      params: [makeNativeJsFunction('spFn')],
      arity: {},
    }
    const issue = describeSerializationIssue(somePred as Any)
    expect(issue).toContain('value.params[0]')
    expect(issue).toContain('NativeJsFunction')
  })

  it('should return null for serializable SomePredFunction', () => {
    const somePred: SomePredFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'SomePred',
      params: [makeUserDefinedFunction()],
      arity: {},
    }
    expect(describeSerializationIssue(somePred as Any)).toBeNull()
  })

  it('should describe NativeJsFunction inside FNullFunction function', () => {
    const fnull: FNullFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'Fnull',
      function: makeNativeJsFunction('fnullFn'),
      params: [1],
      arity: {},
    }
    const issue = describeSerializationIssue(fnull as Any)
    expect(issue).toContain('value.function')
    expect(issue).toContain('NativeJsFunction')
  })

  it('should describe NativeJsFunction inside FNullFunction params', () => {
    const fnull: FNullFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'Fnull',
      function: makeUserDefinedFunction(),
      params: [makeNativeJsFunction('fnullParamFn')],
      arity: {},
    }
    const issue = describeSerializationIssue(fnull as Any)
    expect(issue).toContain('value.params[0]')
    expect(issue).toContain('NativeJsFunction')
  })

  it('should return null for serializable FNullFunction', () => {
    const fnull: FNullFunction = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'Fnull',
      function: makeUserDefinedFunction(),
      params: [1, 'default'],
      arity: {},
    }
    expect(describeSerializationIssue(fnull as Any)).toBeNull()
  })

  it('should return null for serializable arrays', () => {
    expect(describeSerializationIssue([1, 'two', true])).toBeNull()
  })

  it('should describe issue in array element', () => {
    const arr: Any = [1, makeNativeJsFunction('arrFn')]
    const issue = describeSerializationIssue(arr)
    expect(issue).toContain('value[1]')
    expect(issue).toContain('NativeJsFunction')
  })

  it('should return null for serializable objects', () => {
    expect(describeSerializationIssue({ a: 1, b: 'two' })).toBeNull()
  })

  it('should describe issue in object value', () => {
    const obj: Any = { ok: 1, bad: makeNativeJsFunction('objFn') }
    const issue = describeSerializationIssue(obj)
    expect(issue).toContain('value.bad')
    expect(issue).toContain('NativeJsFunction')
  })

  it('should return null for RegularExpression', () => {
    const re: Any = { [REGEXP_SYMBOL]: true, s: 'abc', f: 'gi' }
    expect(describeSerializationIssue(re)).toBeNull()
  })

  it('should return null for EffectRef', () => {
    const ref: EffectRef = { [EFFECT_SYMBOL]: true, name: 'llm.complete' }
    expect(describeSerializationIssue(ref as Any)).toBeNull()
  })

  it('should return null for SpecialBuiltinFunction', () => {
    expect(describeSerializationIssue(makeSpecialBuiltinFunction() as Any)).toBeNull()
  })

  it('should return null for ModuleFunction', () => {
    expect(describeSerializationIssue(makeModuleFunction() as Any)).toBeNull()
  })

  it('should describe anonymous NativeJsFunction', () => {
    const fn = makeNativeJsFunction()
    fn.name = undefined
    const issue = describeSerializationIssue(fn as Any)
    expect(issue).toContain('anonymous')
  })

  it('should describe unknown function type', () => {
    const unknownFn = {
      [FUNCTION_SYMBOL]: true,
      functionType: 'UnknownType',
      arity: {},
    }
    const issue = describeSerializationIssue(unknownFn as Any)
    expect(issue).toContain('unknown function type')
  })

  it('should describe unexpected type', () => {
    // Force a non-object, non-primitive value through
    const issue = describeSerializationIssue(undefined as unknown as Any)
    expect(issue).toContain('unexpected type')
  })
})

describe('isSerializable edge cases', () => {
  it('should return false for unexpected non-object non-primitive value', () => {
    // Force an unexpected type (e.g. undefined) through the type system
    expect(isSerializable(undefined as unknown as Any)).toBe(false)
  })
})
