import '../../../../src/initReferenceData'
import type { TutorialEntry } from './index'

export const controlFlowTutorial: TutorialEntry = {
  id: 'tutorial-control-flow',
  title: 'Control Flow',
  elements: [
    { type: 'paragraph', text: 'Since everything in Lits is an expression, all control flow constructs return values.' },

    { type: 'header', text: 'If / Then / Else' },
    { type: 'paragraph', text: 'The basic conditional. Without `else`, the expression returns `null` when the condition is false:' },
    { type: 'example', code: ['if 10 > 5 then "big" else "small" end'] },
    { type: 'example', code: ['if false then "nope" end'] },

    { type: 'header', text: 'Unless' },
    { type: 'paragraph', text: '`unless` is a negated `if` — the body runs when the condition is false:' },
    { type: 'example', code: ['unless 3 > 10 then "as expected" end'] },

    { type: 'header', text: 'Cond (Multi-branch)' },
    { type: 'paragraph', text: '`cond` evaluates multiple conditions in order and returns the first match:' },
    { type: 'example', code: [
      'let x = 12;',
      'cond',
      '  case x < 5 then "small"',
      '  case x < 15 then "medium"',
      '  case true then "large"',
      'end',
    ] },

    { type: 'header', text: 'Match (Pattern Matching)' },
    { type: 'paragraph', text: '`match` compares a value against specific cases:' },
    { type: 'example', code: [
      'let day = 3;',
      'match day',
      '  case 1 then "Mon"',
      '  case 2 then "Tue"',
      '  case 3 then "Wed"',
      'end',
    ] },

    { type: 'header', text: 'Ternary Operator' },
    { type: 'paragraph', text: 'A compact conditional with `? :`:' },
    { type: 'example', code: ['let n = 7;', 'n > 0 ? "positive" : "non-positive"'] },

    { type: 'header', text: 'Logical Short-circuit' },
    { type: 'paragraph', text: '`&&` returns the first falsy value (or the last value). `||` returns the first truthy value (or the last value):' },
    { type: 'example', code: ['true && "second"'] },
    { type: 'example', code: ['null || false || "found it"'] },

    { type: 'header', text: 'Nullish Coalescing' },
    { type: 'paragraph', text: '`??` returns the left side unless it is `null`. Unlike `||`, it does not coalesce `false` or `0`:' },
    { type: 'example', code: ['0 ?? "default"'] },
    { type: 'example', code: ['null ?? "default"'] },

    { type: 'header', text: 'Do Blocks' },
    { type: 'paragraph', text: 'Group multiple expressions with `do` / `end`. The block returns its last expression:' },
    { type: 'example', code: [
      'do',
      '  let a = 10;',
      '  let b = 20;',
      '  a + b',
      'end',
    ] },

    { type: 'header', text: 'Error Handling' },
    { type: 'paragraph', text: 'Use `try` / `catch` to handle errors. `throw` raises an error:' },
    { type: 'example', code: [
      'try',
      '  throw("oops")',
      'catch(e)',
      '  e.message',
      'end',
    ] },
    { type: 'example', code: [
      'let safe-div = (a, b) ->',
      '  try',
      '    a / b',
      '  catch',
      '    "error"',
      '  end;',
      'safe-div(10, 0)',
    ] },
  ],
}
