/**
 * Frame types for the trampoline evaluator.
 *
 * Each frame type represents a continuation — "what to do with the result of a
 * sub-evaluation." When the trampoline evaluates a sub-expression, it pushes a
 * frame onto the continuation stack (k). When the sub-expression produces a
 * value, the trampoline pops the frame and calls `applyFrame(frame, value, k)`
 * to determine the next step.
 *
 * All frame types are plain serializable objects — no functions, no closures.
 * This enables continuation serialization in Phase 4 (suspension & resume).
 *
 * The `env` field uses `ContextStack` for Phase 1 runtime use. In Phase 4,
 * this will be replaced with a serializable representation (the `Context[]`
 * chain without host bindings, which are re-injected on resume).
 */

import type { Any, Arr, Obj } from '../interface'
import type { AstNode, BindingNode, BindingTarget, NormalExpressionNode, UserDefinedFunction } from '../parser/types'
import type { MatchCase } from '../builtin/specialExpressions/match'
import type { LoopBindingNode } from '../builtin/specialExpressions/loops'
import type { SourceCodeInfo } from '../tokenizer/token'
import type { ContextStack } from './ContextStack'
import type { Context } from './interface'

// ---------------------------------------------------------------------------
// Program flow
// ---------------------------------------------------------------------------

/**
 * Evaluate a sequence of AST nodes in order, returning the last value.
 *
 * Used by: `do...end` (block), top-level program evaluation, function body.
 *
 * The trampoline evaluates `nodes[index]`. When the value comes back it
 * advances `index`. When all nodes are done, the last value propagates up.
 */
export interface SequenceFrame {
  type: 'Sequence'
  nodes: AstNode[]
  index: number // next node to evaluate (0-based)
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

// ---------------------------------------------------------------------------
// Special expressions — branching
// ---------------------------------------------------------------------------

/**
 * Binary conditional branch — used by both `if` and `unless`.
 *
 * Pushed when the condition expression is being evaluated. When the condition
 * value arrives, the trampoline picks `thenNode` or `elseNode` (or returns
 * `null` if there is no else branch). `inverted` flips the test for `unless`.
 */
export interface IfBranchFrame {
  type: 'IfBranch'
  thenNode: AstNode
  elseNode: AstNode | undefined
  inverted: boolean // true for `unless`, false for `if`
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

/**
 * Multi-way conditional (`cond`).
 *
 * When `phase` is `'test'`: a test expression is being evaluated. If truthy,
 * transition to body evaluation. If falsy, advance to the next case.
 * When `phase` is `'body'`: the body expression is being evaluated. The
 * resulting value propagates up.
 */
export interface CondFrame {
  type: 'Cond'
  phase: 'test' | 'body'
  cases: [AstNode, AstNode][] // [test, body] pairs
  index: number // current case (0-based)
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

/**
 * Pattern matching (`match`).
 *
 * Phase `'matchValue'`: the match-value expression is being evaluated.
 * Phase `'guard'`: a pattern matched and boundary bindings were created;
 *   the guard expression is being evaluated.
 * Phase `'body'`: the body for the matched case is being evaluated.
 *
 * `matchValue` is `null` during the `'matchValue'` phase and set once known.
 * `bindings` holds the names captured by `tryMatch` (empty until a pattern
 * matches).
 */
export interface MatchFrame {
  type: 'Match'
  phase: 'matchValue' | 'guard' | 'body'
  matchValue: Any | null
  cases: MatchCase[]
  index: number // current case (0-based)
  bindings: Record<string, Any>
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

// ---------------------------------------------------------------------------
// Special expressions — short-circuit sequential
// ---------------------------------------------------------------------------

/**
 * Short-circuit `&&` — evaluates nodes sequentially, returning the first
 * falsy value or the last value if all are truthy.
 */
export interface AndFrame {
  type: 'And'
  nodes: AstNode[]
  index: number // next node to evaluate
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

/**
 * Short-circuit `||` — evaluates nodes sequentially, returning the first
 * truthy value or the last value if all are falsy.
 */
export interface OrFrame {
  type: 'Or'
  nodes: AstNode[]
  index: number // next node to evaluate
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

/**
 * Nullish coalescing `??` — evaluates nodes sequentially, returning the
 * first non-null value. Undefined user-defined symbols are treated as null
 * (skipped without throwing `UndefinedSymbolError`).
 */
export interface QqFrame {
  type: 'Qq'
  nodes: AstNode[]
  index: number // next node to evaluate
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

// ---------------------------------------------------------------------------
// Special expressions — collection construction
// ---------------------------------------------------------------------------

/**
 * Array literal construction (`[]` / `array`).
 *
 * Evaluates elements sequentially. Spread nodes (`...expr`) evaluate the
 * inner expression and flatten the resulting array into `result`.
 */
export interface ArrayBuildFrame {
  type: 'ArrayBuild'
  nodes: AstNode[]
  index: number // next element to evaluate
  result: Arr // accumulated array
  isSpread: boolean // whether the current node is a spread
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

/**
 * Object literal construction (`{}` / `object`).
 *
 * Evaluates key-value pairs sequentially. For normal entries, evaluates
 * key then value (stepping by 2). For spread entries, evaluates the spread
 * expression and merges the result into `result`.
 *
 * `currentKey` holds the evaluated key string when we're between key and
 * value evaluation (null otherwise).
 */
export interface ObjectBuildFrame {
  type: 'ObjectBuild'
  nodes: AstNode[]
  index: number // current position in nodes array
  result: Obj // accumulated object
  currentKey: string | null // evaluated key awaiting its value
  isSpread: boolean // whether current node is a spread
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

// ---------------------------------------------------------------------------
// Special expressions — binding
// ---------------------------------------------------------------------------

/**
 * `let` binding — evaluate the value expression, then process destructuring.
 *
 * The trampoline evaluates the value expression. When it completes,
 * `applyFrame` processes `evaluateBindingNodeValues(target, value, ...)`
 * and adds the resulting bindings to the context. The result of the let
 * expression is the evaluated value itself.
 */
export interface LetBindFrame {
  type: 'LetBind'
  target: BindingTarget
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

/**
 * `loop` binding setup — evaluate binding values sequentially.
 *
 * Each binding's value expression is evaluated in a context that includes
 * all previously bound values (bindings can depend on earlier bindings).
 *
 * Phase `'value'`: evaluating a binding's value expression.
 * Phase `'destructure'`: value evaluated, processing destructuring defaults.
 */
export interface LoopBindFrame {
  type: 'LoopBind'
  phase: 'value' | 'destructure'
  bindingNodes: BindingNode[]
  index: number // current binding (0-based)
  context: Context // accumulated bindings so far
  body: AstNode // loop body (stored for LoopIterateFrame creation)
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

/**
 * `loop` body iteration — evaluate body with `recur` handling.
 *
 * In the trampoline, `recur` does NOT throw `RecurSignal`. Instead, when
 * recur args are collected, the trampoline pops back to this frame, rebinds
 * the variables, and re-evaluates the body. This gives proper tail-call
 * elimination without stack growth.
 */
export interface LoopIterateFrame {
  type: 'LoopIterate'
  bindingNodes: BindingNode[]
  bindingContext: Context // mutable context with loop bindings
  body: AstNode
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

/**
 * State for a single binding level in a `for`/`doseq` loop.
 *
 * Each level iterates over a `collection`. Inner-level collections are
 * re-evaluated when outer-level bindings change.
 */
export interface ForBindingLevelState {
  collection: Arr // evaluated collection for this level
  index: number // current element index
}

/**
 * `for`/`doseq` multi-binding nested iteration.
 *
 * Multi-level nested loop with optional let-bindings, when-guards, and
 * while-guards at each binding level. `for` collects results; `doseq`
 * discards them (side-effects only).
 *
 * Phase describes what sub-expression is currently being evaluated:
 * - `'evalCollection'`: evaluating the collection expression for a level
 * - `'evalElement'`: processing destructuring for the current element
 * - `'evalLet'`: evaluating let-bindings at the current level
 * - `'evalWhen'`: evaluating the when-guard
 * - `'evalWhile'`: evaluating the while-guard
 * - `'evalBody'`: evaluating the loop body
 */
export interface ForLoopFrame {
  type: 'ForLoop'
  returnResult: boolean // true for `for`, false for `doseq`
  bindingNodes: LoopBindingNode[]
  body: AstNode
  result: Arr // accumulated results (for `for`)
  phase: 'evalCollection' | 'evalElement' | 'evalLet' | 'evalWhen' | 'evalWhile' | 'evalBody'
  bindingLevel: number // which binding level is being processed (0-based)
  levelStates: ForBindingLevelState[] // resolved states for levels 0..bindingLevel
  context: Context // aggregated bindings from all resolved levels
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

// ---------------------------------------------------------------------------
// Special expressions — control flow
// ---------------------------------------------------------------------------

/**
 * `throw` — evaluate the expression, then throw `UserDefinedError`.
 *
 * The trampoline evaluates the throw's argument expression. When the value
 * arrives, `applyFrame` asserts it's a non-empty string and throws.
 */
export interface ThrowFrame {
  type: 'Throw'
  sourceCodeInfo?: SourceCodeInfo
}

/**
 * `perform` argument collection — evaluate effect ref + args sequentially.
 *
 * First evaluates the effect expression (index 0) to get an EffectRef.
 * Then evaluates each argument expression. When all are collected, produces
 * a `PerformStep` with the resolved EffectRef and argument values.
 */
export interface PerformArgsFrame {
  type: 'PerformArgs'
  argNodes: AstNode[] // all argument nodes (effect expr at index 0, then actual args)
  index: number // next node to evaluate (0-based)
  params: Arr // accumulated evaluated values
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

/**
 * `recur` — evaluate parameters sequentially, then signal tail-call.
 *
 * In the trampoline, instead of throwing `RecurSignal`, the completed
 * recur frame pops the continuation stack to the nearest `LoopIterateFrame`
 * or `FnBodyFrame`, rebinds parameters, and re-enters the loop/body.
 * This eliminates the exception-based control flow used by the recursive
 * evaluator.
 */
export interface RecurFrame {
  type: 'Recur'
  nodes: AstNode[] // parameter expressions
  index: number // next param to evaluate
  params: Arr // accumulated evaluated params
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

// ---------------------------------------------------------------------------
// Special expressions — exception & effect handling
// ---------------------------------------------------------------------------

/**
 * `try/catch` exception boundary.
 *
 * Pushed around the try body evaluation. If an error is thrown during
 * sub-evaluation, the trampoline pops frames up to this one and evaluates
 * the catch body in a new context with the error bound to `errorSymbol`.
 *
 * `TryCatchFrame` only catches errors — it ignores `perform` (effects).
 * `TryWithFrame` only handles effects — it ignores errors.
 * The two frame types are independent and stack correctly.
 */
export interface TryCatchFrame {
  type: 'TryCatch'
  errorSymbol: string | null // null when catch has no error binding: `catch () ...`
  catchNode: AstNode
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

/**
 * `try/with` effect handler boundary.
 *
 * Pushed around the try body evaluation. When `perform` is called, the
 * trampoline searches the continuation stack for a matching `TryWithFrame`.
 * The delimited continuation between `perform` and this frame is captured.
 *
 * Handlers are stored as evaluated `[EffectRef, handlerFn AstNode]` pairs.
 * The effect expressions are evaluated eagerly when entering the `try/with`
 * scope (i.e., when the frame is created) via `evaluateNodeRecursive`.
 * This ensures handler matching is deterministic and doesn't depend on
 * state changes inside the try body.
 */
export interface TryWithFrame {
  type: 'TryWith'
  handlers: EvaluatedWithHandler[]
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

/**
 * A pre-evaluated effect handler pair for `TryWithFrame`.
 * `effectRef` is the resolved EffectRef value.
 * `handlerNode` is the AST node for the handler function expression.
 */
export interface EvaluatedWithHandler {
  effectRef: Any // Should be EffectRef at runtime — stored as Any for serialization
  handlerNode: AstNode
}

/**
 * Bridges a handler's return value back to the perform call site.
 *
 * When `perform` matches a `TryWithFrame`, the handler runs with a continuation
 * that excludes the current try/with/catch scope (so errors and effects from
 * the handler propagate upward per P&P semantics). However, the handler's
 * return value needs to resume the body at the perform call site with the
 * TryWithFrame still on the stack (so subsequent performs in the same body
 * can still match handlers).
 *
 * `EffectResumeFrame` is placed below the handler's function frames in the
 * continuation stack. When the handler returns a value, this frame replaces
 * the continuation with `resumeK` — the original continuation from the
 * perform call site, with the TryWithFrame intact.
 *
 * Error/effect semantics:
 * - Handler RETURNS value → EffectResumeFrame redirects to resumeK → body continues
 * - Handler THROWS error → error walks past EffectResumeFrame to outer_k → correct
 * - Handler PERFORMS effect → effect walks past EffectResumeFrame to outer_k → correct
 */
export interface EffectResumeFrame {
  type: 'EffectResume'
  resumeK: ContinuationStack
  sourceCodeInfo?: SourceCodeInfo
}

// ---------------------------------------------------------------------------
// Parallel resume
// ---------------------------------------------------------------------------

/**
 * Resume a `parallel(...)` expression after suspension.
 *
 * When a `parallel` has some branches that suspended and some that completed,
 * the continuation is suspended with this frame at the top. On resume, the
 * host provides a value for the first pending suspended branch.
 *
 * `applyFrame` converts this to a `ParallelResumeStep` so that `tick` can
 * handle it with access to `handlers` and `signal`.
 *
 * Fields:
 * - `branchCount`: total number of branches (for ordered result array)
 * - `completedBranches`: branches that already finished `{ index, value }`
 * - `suspendedBranches`: remaining suspended branches `{ index, blob, meta? }`
 *   The first entry is the one being resumed — its blob is NOT used because
 *   the value was already provided by the host. Subsequent entries are pending.
 */
export interface ParallelResumeFrame {
  type: 'ParallelResume'
  branchCount: number
  completedBranches: Array<{ index: number, value: Any }>
  suspendedBranches: Array<{ index: number, blob: string, meta?: Any }>
}

// ---------------------------------------------------------------------------
// Function calls
// ---------------------------------------------------------------------------

/**
 * Evaluate function call arguments.
 *
 * Evaluates argument expressions sequentially, collecting results into
 * `params`. Handles spread nodes (flatten arrays) and placeholder `_`
 * nodes (record indices for partial application).
 *
 * `fnNode` is the first element of the `NormalExpressionNode` payload —
 * either a symbol node (for named calls) or an expression node (for
 * anonymous calls like `((fn [x] x) 5)`).
 *
 * When all arguments are collected:
 * - Named builtin symbol → dispatch to builtin's evaluate
 * - Named user symbol → look up value, push `CallFnFrame`, dispatch
 * - Anonymous expression → push `CallFnFrame`, evaluate function expression
 */
export interface EvalArgsFrame {
  type: 'EvalArgs'
  node: NormalExpressionNode // the full expression node (for dispatch info)
  index: number // next argument to evaluate
  params: Arr // accumulated evaluated arguments
  placeholders: number[] // indices of `_` placeholders
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

/**
 * Dispatch a function call after the function value has been resolved.
 *
 * Pushed when an anonymous function expression needs to be evaluated before
 * calling, or when a compound function type (Comp, Juxt, etc.) needs to
 * chain sub-calls. Receives the resolved function value and dispatches
 * using `executeFunction`.
 */
export interface CallFnFrame {
  type: 'CallFn'
  params: Arr // pre-evaluated arguments
  placeholders: number[] // placeholder indices (for partial application)
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

/**
 * User-defined function body evaluation.
 *
 * After parameter setup (binding destructuring, defaults, rest args), the
 * function body is evaluated as a sequence. This frame also serves as the
 * target for `recur` — when recur params are collected, the trampoline
 * pops to this frame, rebinds parameters, and re-evaluates the body.
 *
 * `fn` is stored for recur handling: the parameter definitions
 * (`fn.evaluatedfunction[0]`) and captured environment
 * (`fn.evaluatedfunction[2]`) are needed to rebind.
 */
export interface FnBodyFrame {
  type: 'FnBody'
  fn: UserDefinedFunction
  bodyIndex: number // next body node to evaluate (0-based)
  env: ContextStack // function scope (includes parameter bindings)
  outerEnv: ContextStack // calling environment (needed for recur to rebind params)
  sourceCodeInfo?: SourceCodeInfo
}

// ---------------------------------------------------------------------------
// Binding destructuring
// ---------------------------------------------------------------------------

/**
 * Evaluate default values during binding destructuring.
 *
 * `evaluateBindingNodeValues` processes destructuring patterns and may need
 * to evaluate default value expressions (e.g., `let [a (= 10) b] [1]` —
 * the default `10` for `b` needs evaluation). This frame captures the
 * state needed to resume destructuring after a default is evaluated.
 *
 * `target` is the overall binding target being destructured.
 * `value` is the value being destructured against the target.
 * `record` accumulates the name→value mappings produced so far.
 */
export interface BindingDefaultFrame {
  type: 'BindingDefault'
  target: BindingTarget
  value: Any
  record: Record<string, Any>
  env: ContextStack
  sourceCodeInfo?: SourceCodeInfo
}

// ---------------------------------------------------------------------------
// Post-processing
// ---------------------------------------------------------------------------

/**
 * NaN guard after evaluating a normal expression.
 *
 * Normal expressions go through a NaN check: if the result is `NaN`, a
 * `LitsError` is thrown. This frame wraps that check.
 */
export interface NanCheckFrame {
  type: 'NanCheck'
  sourceCodeInfo?: SourceCodeInfo
}

// ---------------------------------------------------------------------------
// Frame union type
// ---------------------------------------------------------------------------

/**
 * Discriminated union of all frame types.
 *
 * Each frame captures the continuation state for one recursive call pattern
 * in the evaluator. The `type` field serves as the discriminant.
 *
 * Frame categories:
 * - **Program flow**: SequenceFrame
 * - **Branching**: IfBranchFrame, CondFrame, MatchFrame
 * - **Short-circuit**: AndFrame, OrFrame, QqFrame
 * - **Collection construction**: ArrayBuildFrame, ObjectBuildFrame
 * - **Binding**: LetBindFrame, LoopBindFrame, LoopIterateFrame, ForLoopFrame
 * - **Control flow**: ThrowFrame, RecurFrame
 * - **Exception & effect handling**: TryCatchFrame, TryWithFrame
 * - **Function calls**: EvalArgsFrame, CallFnFrame, FnBodyFrame
 * - **Destructuring**: BindingDefaultFrame
 * - **Post-processing**: NanCheckFrame
 */
export type Frame =
  // Program flow
  | SequenceFrame
  // Branching
  | IfBranchFrame
  | CondFrame
  | MatchFrame
  // Short-circuit
  | AndFrame
  | OrFrame
  | QqFrame
  // Collection construction
  | ArrayBuildFrame
  | ObjectBuildFrame
  // Binding
  | LetBindFrame
  | LoopBindFrame
  | LoopIterateFrame
  | ForLoopFrame
  // Control flow
  | ThrowFrame
  | RecurFrame
  | PerformArgsFrame
  // Exception & effect handling
  | TryCatchFrame
  | TryWithFrame
  | EffectResumeFrame
  // Parallel resume
  | ParallelResumeFrame
  // Function calls
  | EvalArgsFrame
  | CallFnFrame
  | FnBodyFrame
  // Destructuring
  | BindingDefaultFrame
  // Post-processing
  | NanCheckFrame

/**
 * Array type alias for readability — a continuation stack is just
 * an array of frames. The top of the stack is index 0.
 */
export type ContinuationStack = Frame[]
