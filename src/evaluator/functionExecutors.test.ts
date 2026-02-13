import { describe, expect, it } from 'vitest'
import { LitsError } from '../errors'
import { FUNCTION_SYMBOL } from '../utils/symbols'
import type { NamespaceFunction } from '../parser/types'
import type { Any } from '../interface'
import type { SourceCodeInfo } from '../tokenizer/token'
import { ContextStackImpl } from './ContextStack'
import { functionExecutors } from './functionExecutors'
// Import namespaces to ensure they are registered
import '../namespaces'

describe('functionExecutors', () => {
  describe('namespace executor', () => {
    const dummySourceCodeInfo: SourceCodeInfo = {
      code: 'test',
      position: { line: 1, column: 1 },
    }

    const dummyContextStack = new ContextStackImpl({ contexts: [{}] })

    const dummyExecuteFunction = (fn: Any, params: Any[], _contextStack: Any, _sourceCodeInfo: SourceCodeInfo | undefined) => {
      return null
    }

    const dummyEvaluateNode = () => null

    it('should throw LitsError when namespace is not found', () => {
      const namespaceFunction: NamespaceFunction = {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo: dummySourceCodeInfo,
        functionType: 'Namespace',
        namespaceName: 'NonExistentNamespace',
        functionName: 'someFunction',
        arity: { min: 0 },
      }

      expect(() =>
        functionExecutors.Namespace(
          namespaceFunction,
          [],
          dummySourceCodeInfo,
          dummyContextStack,
          { evaluateNode: dummyEvaluateNode, executeFunction: dummyExecuteFunction },
        ),
      ).toThrow(LitsError)

      expect(() =>
        functionExecutors.Namespace(
          namespaceFunction,
          [],
          dummySourceCodeInfo,
          dummyContextStack,
          { evaluateNode: dummyEvaluateNode, executeFunction: dummyExecuteFunction },
        ),
      ).toThrow('Namespace \'NonExistentNamespace\' not found.')
    })

    it('should throw LitsError when function is not found in namespace', () => {
      const namespaceFunction: NamespaceFunction = {
        [FUNCTION_SYMBOL]: true,
        sourceCodeInfo: dummySourceCodeInfo,
        functionType: 'Namespace',
        namespaceName: 'Grid', // Grid namespace exists
        functionName: 'nonExistentFunction',
        arity: { min: 0 },
      }

      expect(() =>
        functionExecutors.Namespace(
          namespaceFunction,
          [],
          dummySourceCodeInfo,
          dummyContextStack,
          { evaluateNode: dummyEvaluateNode, executeFunction: dummyExecuteFunction },
        ),
      ).toThrow(LitsError)

      expect(() =>
        functionExecutors.Namespace(
          namespaceFunction,
          [],
          dummySourceCodeInfo,
          dummyContextStack,
          { evaluateNode: dummyEvaluateNode, executeFunction: dummyExecuteFunction },
        ),
      ).toThrow('Function \'nonExistentFunction\' not found in namespace \'Grid\'.')
    })
  })
})
