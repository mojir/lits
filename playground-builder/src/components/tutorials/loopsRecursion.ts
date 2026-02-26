import '../../../../src/initReferenceData'
import type { TutorialEntry } from './index'

export const loopsRecursionTutorial: TutorialEntry = {
  id: 'tutorial-loops-recursion',
  title: 'Loops & Recursion',
  elements: [
    { type: 'paragraph', text: 'Lits provides `for` comprehensions for building arrays, `doseq` for side effects, and `loop`/`recur` for tail-recursive iteration.' },

    { type: 'header', text: 'For Comprehensions' },
    { type: 'paragraph', text: '`for` iterates over a collection and returns a new array:' },
    { type: 'example', code: ['for (x in [1, 2, 3, 4]) -> x * 2'] },

    { type: 'header', text: 'Filtering with when' },
    { type: 'paragraph', text: 'Use `when` to skip elements that don\'t match a condition:' },
    { type: 'example', code: ['for (x in range(10) when odd?(x)) -> x * x'] },

    { type: 'header', text: 'Early Exit with while' },
    { type: 'paragraph', text: '`while` stops the iteration entirely when the condition becomes false:' },
    { type: 'example', code: ['for (x in range(100) while x < 5) -> x * 10'] },

    { type: 'header', text: 'Local Bindings with let' },
    { type: 'paragraph', text: 'Bind intermediate values inside the comprehension:' },
    { type: 'example', code: ['for (x in [1, 2, 3] let sq = x * x) -> sq + 1'] },

    { type: 'header', text: 'Multiple Iterators' },
    { type: 'paragraph', text: 'Multiple bindings produce a cartesian product:' },
    { type: 'example', code: ['for (i in [1, 2], j in [10, 20]) -> i + j'] },

    { type: 'header', text: 'Complex Comprehension' },
    { type: 'paragraph', text: 'Combine `let`, `when`, and `while` for powerful queries:' },
    { type: 'example', code: [
      'for (',
      '  i in range(10)',
      '  let sq = i ^ 2',
      '  while sq < 50',
      '  when sq % 3 == 0',
      ') -> sq',
    ] },

    { type: 'header', text: 'Loop / Recur' },
    { type: 'paragraph', text: '`loop` sets up initial bindings, and `recur` jumps back to the top with new values. This is tail-recursive and efficient:' },
    { type: 'example', code: [
      'loop (n = 5, acc = 1) ->',
      '  if n <= 1 then',
      '    acc',
      '  else',
      '    recur(n - 1, acc * n)',
      '  end',
    ] },

    { type: 'header', text: 'Self Recursion' },
    { type: 'paragraph', text: 'Inside a lambda, `self` refers to the enclosing function:' },
    { type: 'example', code: [
      'let fib = n ->',
      '  if n <= 1 then',
      '    n',
      '  else',
      '    self(n - 1) + self(n - 2)',
      '  end',
      'fib(10)',
    ] },

    { type: 'header', text: 'Doseq (Side Effects)' },
    { type: 'paragraph', text: '`doseq` iterates for side effects and returns `null`:' },
    { type: 'example', code: ['doseq (x in [1, 2, 3]) -> write!(x)'] },
  ],
}
