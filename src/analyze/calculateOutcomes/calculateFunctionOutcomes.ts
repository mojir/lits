import type { DefnNode, DefnsNode, FnNode } from '../../builtin/specialExpressions/functions'
import type { FunctionOverload } from '../../builtin/utils'
import { combinate } from '../utils'
import type { CalculatePossibleAstNodesHelper, CombinateAstNodes } from '.'

function calculateFunctionOverloadOutcomes(combinateAstNodes: CombinateAstNodes, functionOverloads: FunctionOverload[]) {
  return combinate(functionOverloads
    // For each overload, calculate the possible outcomes for each parameter
    .map<FunctionOverload[]>(functionOverload =>
      combinateAstNodes(functionOverload.b, [
        functionOverload.as.m,
        functionOverload.as.b.map(bindingNode => bindingNode.n),
        functionOverload.as.r ?? [],
      ].flat())

      // For each combination of parameter outcomes, create a new overload
        .map<FunctionOverload>(body => ({
          ...functionOverload,
          b: body,
        })),
    ),
  // For each combination of overloads, create a new DefnNode
  )
}

export const calculateDefnOutcomes: CalculatePossibleAstNodesHelper<DefnNode> = ({
  astNode,
  combinateAstNodes,
  addGlobalIdentifier,
}) => {
  addGlobalIdentifier(astNode.f.v)
  // astNode.o is an array of overloads
  return calculateFunctionOverloadOutcomes(combinateAstNodes, astNode.o).map<DefnNode>(functionOverloads => ({
    ...astNode,
    o: functionOverloads,
  }))
}

export const calculateDefnsOutcomes: CalculatePossibleAstNodesHelper<DefnsNode> = ({
  astNode,
  calculatePossibleAstNodes,
  combinateAstNodes,
}) => {
  // astNode.o is an array of overloads
  return calculatePossibleAstNodes(astNode.f).flatMap(functionName =>
    calculateFunctionOverloadOutcomes(combinateAstNodes, astNode.o).map<DefnsNode>(functionOverloads => ({
      ...astNode,
      f: functionName,
      o: functionOverloads,
    })),
  )
}

export const calculateFnOutcomes: CalculatePossibleAstNodesHelper<FnNode> = ({
  astNode,
  combinateAstNodes,
}) => {
  // astNode.o is an array of overloads
  return calculateFunctionOverloadOutcomes(combinateAstNodes, astNode.o).map<FnNode>(functionOverloads => ({
    ...astNode,
    o: functionOverloads,
  }))
}
