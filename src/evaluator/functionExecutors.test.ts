import { describe, expect, it } from 'vitest'
import { LitsError } from '../errors'
import { FUNCTION_SYMBOL } from '../utils/symbols'
import type { ModuleFunction } from '../parser/types'
import type { Arr } from '../interface'
import type { SourceCodeInfo } from '../tokenizer/token'
import { gridModule } from '../builtin/modules/grid'
import { ContextStackImpl } from './ContextStack'
import { functionExecutors } from './functionExecutors'
import type { ExecuteFunction } from './interface'

describe('functionExecutors', () => {
  describe('module executor', () => {
    const dummySourceCodeInfo: SourceCodeInfo = {
      code: 'test',
      position: { line: 1, column: 1 },
    }

    const modules = new Map([[gridModule.name, gridModule]])
    const dummyContextStack = new ContextStackImpl({ contexts: [{}], modules })

    const dummyExecuteFunction: ExecuteFunction = (_fn, _params: Arr, _contextStack, _sourceCodeInfo) => {
      return null
    }

    const dummyEvaluateNode = () => null

    it('should throw LitsError when module is not found', () => {
      const moduleFunction: ModuleFunction = {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo: dummySourceCodeInfo,
        functionType: 'Module',
        moduleName: 'NonExistentModule',
        functionName: 'someFunction',
        arity: { min: 0 },
      }

      expect(() =>
        functionExecutors.Module(
          moduleFunction,
          [],
          dummySourceCodeInfo,
          dummyContextStack,
          { evaluateNode: dummyEvaluateNode, executeFunction: dummyExecuteFunction },
        ),
      ).toThrow(LitsError)

      expect(() =>
        functionExecutors.Module(
          moduleFunction,
          [],
          dummySourceCodeInfo,
          dummyContextStack,
          { evaluateNode: dummyEvaluateNode, executeFunction: dummyExecuteFunction },
        ),
      ).toThrow('Module \'NonExistentModule\' not found.')
    })

    it('should throw LitsError when function is not found in module', () => {
      const moduleFunction: ModuleFunction = {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo: dummySourceCodeInfo,
        functionType: 'Module',
        moduleName: 'Grid', // Grid module exists
        functionName: 'nonExistentFunction',
        arity: { min: 0 },
      }

      expect(() =>
        functionExecutors.Module(
          moduleFunction,
          [],
          dummySourceCodeInfo,
          dummyContextStack,
          { evaluateNode: dummyEvaluateNode, executeFunction: dummyExecuteFunction },
        ),
      ).toThrow(LitsError)

      expect(() =>
        functionExecutors.Module(
          moduleFunction,
          [],
          dummySourceCodeInfo,
          dummyContextStack,
          { evaluateNode: dummyEvaluateNode, executeFunction: dummyExecuteFunction },
        ),
      ).toThrow('Function \'nonExistentFunction\' not found in module \'Grid\'.')
    })
  })
})
