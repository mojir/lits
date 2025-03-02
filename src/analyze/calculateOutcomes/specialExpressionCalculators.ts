import type { SpecialExpressionName } from '../../builtin'
import type { AndNode } from '../../builtin/specialExpressions/and'
import type { CommentExpressionNode } from '../../builtin/specialExpressions/comment'
import type { CondNode } from '../../builtin/specialExpressions/cond'
import type { DeclaredNode } from '../../builtin/specialExpressions/declared'
import type { DefNode } from '../../builtin/specialExpressions/def'
import type { DefsNode } from '../../builtin/specialExpressions/defs'
import type { DoNode } from '../../builtin/specialExpressions/do'
import type { DefnNode, DefnsNode, FnNode } from '../../builtin/specialExpressions/functions'
import type { IfNode } from '../../builtin/specialExpressions/if'
import type { IfLetNode } from '../../builtin/specialExpressions/if_let'
import type { IfNotNode } from '../../builtin/specialExpressions/if_not'
import type { LetNode } from '../../builtin/specialExpressions/let'
import type { LoopNode } from '../../builtin/specialExpressions/loop'
import type { DoSeqNode, ForNode } from '../../builtin/specialExpressions/loops'
import type { OrNode } from '../../builtin/specialExpressions/or'
import type { QqNode } from '../../builtin/specialExpressions/qq'
import type { RecurNode } from '../../builtin/specialExpressions/recur'
import type { ThrowNode } from '../../builtin/specialExpressions/throw'
import type { TimeNode } from '../../builtin/specialExpressions/time'
import type { TryNode } from '../../builtin/specialExpressions/try'
import type { WhenNode } from '../../builtin/specialExpressions/when'
import type { WhenFirstNode } from '../../builtin/specialExpressions/when_first'
import type { WhenLetNode } from '../../builtin/specialExpressions/when_let'
import type { WhenNotNode } from '../../builtin/specialExpressions/when_not'
import type { AstNode } from '../../parser/interface'
import { calculateAndOutcomes } from './calculateAndOutcomes'
import { calculateCondOutcomes } from './calculateCondOutcomes'
import { calculateDeclaredOutcomes } from './calculateDeclaredOutcomes'
import { calculateDefOutcomes } from './calculateDefOutcomes'
import { calculateDefsOutcomes } from './calculateDefsOutcomes'
import { calculateDoOutcomes } from './calculateDoOutcomes'
import { calculateDefnOutcomes, calculateDefnsOutcomes, calculateFnOutcomes } from './calculateFunctionOutcomes'
import { calculateIfLetOutcomes } from './calculateIfLetOutcomes'
import { calculateIfNotOutcomes } from './calculateIfNotOutcomes'
import { calculateIfOutcomes } from './calculateIfOutcomes'
import { calculateLetOutcomes } from './calculateLetOutcomes'
import { calculateDoSeqOutcomes, calculateForOutcomes } from './calculateLoopsOutcomes'
import { calculateOrOutcomes } from './calculateOrOutcomes'
import { calculateQqOutcomes } from './calculateQqOutcomes'
import { calculateThrowOutcomes } from './calculateThrowOutcomes'
import { calculateTimeOutcomes } from './calculateTimeOutcomes'
import { calculateTryOutcomes } from './calculateTryOutcomes'
import { calculateWhenFirstOutcomes } from './calculateWhenFirstOutcomes'
import { calculateWhenLetOutcomes } from './calculateWhenLetOutcomes'
import { calculateWhenNotOutcomes } from './calculateWhenNotOutcomes'
import { calculateWhenOutcomes } from './calculateWhenOutcomes'
import { calculateRecurOutcomes } from './calculateRecurOutcomes'
import { calculateCommentOutcomes } from './calculateCommentOutcomes'
import { calculateLoopOutcomes } from './calculateLoopOutcomes'
import type { CalculatePossibleAstNodesHelperOptions } from '.'

export const specialExpressionCalculator = {
  'and': (astNode: AndNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateAndOutcomes({ astNode, ...helperOptions }),
  'comment': (astNode: CommentExpressionNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateCommentOutcomes({ astNode, ...helperOptions }),
  'cond': (astNode: CondNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateCondOutcomes({ astNode, ...helperOptions }),
  'declared?': (astNode: DeclaredNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateDeclaredOutcomes({ astNode, ...helperOptions }),
  'defn': (astNode: DefnNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateDefnOutcomes({ astNode, ...helperOptions }),
  'def': (astNode: DefNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateDefOutcomes({ astNode, ...helperOptions }),
  'defns': (astNode: DefnsNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateDefnsOutcomes({ astNode, ...helperOptions }),
  'defs': (astNode: DefsNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateDefsOutcomes({ astNode, ...helperOptions }),
  'do': (astNode: DoNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateDoOutcomes({ astNode, ...helperOptions }),
  'doseq': (astNode: DoSeqNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateDoSeqOutcomes({ astNode, ...helperOptions }),
  'fn': (astNode: FnNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateFnOutcomes({ astNode, ...helperOptions }),
  'for': (astNode: ForNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateForOutcomes({ astNode, ...helperOptions }),
  'if_let': (astNode: IfLetNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateIfLetOutcomes({ astNode, ...helperOptions }),
  'if': (astNode: IfNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateIfOutcomes({ astNode, ...helperOptions }),
  'if_not': (astNode: IfNotNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateIfNotOutcomes({ astNode, ...helperOptions }),
  'let': (astNode: LetNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateLetOutcomes({ astNode, ...helperOptions }),
  'loop': (astNode: LoopNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateLoopOutcomes({ astNode, ...helperOptions }),
  'or': (astNode: OrNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateOrOutcomes({ astNode, ...helperOptions }),
  '??': (astNode: QqNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateQqOutcomes({ astNode, ...helperOptions }),
  'recur': (astNode: RecurNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateRecurOutcomes({ astNode, ...helperOptions }),
  'time!': (astNode: TimeNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateTimeOutcomes({ astNode, ...helperOptions }),
  'throw': (astNode: ThrowNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateThrowOutcomes({ astNode, ...helperOptions }),
  'try': (astNode: TryNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateTryOutcomes({ astNode, ...helperOptions }),
  'when_first': (astNode: WhenFirstNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateWhenFirstOutcomes({ astNode, ...helperOptions }),
  'when_let': (astNode: WhenLetNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateWhenLetOutcomes({ astNode, ...helperOptions }),
  'when': (astNode: WhenNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateWhenOutcomes({ astNode, ...helperOptions }),
  'when_not': (astNode: WhenNotNode, helperOptions: Omit<CalculatePossibleAstNodesHelperOptions<AstNode>, 'astNode'>) => calculateWhenNotOutcomes({ astNode, ...helperOptions }),

} satisfies Record<SpecialExpressionName, unknown>
