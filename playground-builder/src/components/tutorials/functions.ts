import '../../../../src/initReferenceData'
import type { TutorialEntry } from './index'

export const functionsTutorial: TutorialEntry = {
  id: 'tutorial-functions',
  title: 'Functions',
  elements: [
    { type: 'paragraph', text: 'Functions are first-class values in Lits. You can define them, pass them around, and compose them freely.' },

    { type: 'header', text: 'Arrow Functions' },
    { type: 'paragraph', text: 'Define functions with the arrow (`->`) syntax. For a single parameter, parentheses are optional:' },
    { type: 'example', code: ['let double = x -> x * 2;', 'double(21)'] },

    { type: 'header', text: 'Multiple Parameters' },
    { type: 'paragraph', text: 'Wrap multiple parameters in parentheses:' },
    { type: 'example', code: ['let add = (a, b) -> a + b;', 'add(3, 4)'] },

    { type: 'header', text: 'No Parameters' },
    { type: 'paragraph', text: 'Use empty parentheses for functions that take no arguments:' },
    { type: 'example', code: ['let greet = () -> "Hello!";', 'greet()'] },

    { type: 'header', text: 'Default Parameters' },
    { type: 'paragraph', text: 'Parameters can have default values:' },
    { type: 'example', code: ['let greet = (name = "World") -> "Hello, " ++ name;', 'greet()'] },

    { type: 'header', text: 'Rest Parameters' },
    { type: 'paragraph', text: 'Collect remaining arguments with the rest (`...`) syntax:' },
    { type: 'example', code: ['let sum-all = (...nums) -> reduce(nums, +, 0);', 'sum-all(1, 2, 3, 4, 5)'] },

    { type: 'header', text: 'Short-hand Lambdas' },
    { type: 'paragraph', text: 'For quick one-liners, use `->` with `$` (or `$1`, `$2`, …) for positional arguments:' },
    { type: 'example', code: ['map([1, 2, 3], -> $ * $)'] },
    { type: 'example', code: ['map([1, 2, 3, 4], -> $1 + 10)'] },

    { type: 'header', text: 'Recursion with self' },
    { type: 'paragraph', text: 'A lambda can call itself using `self`:' },
    { type: 'example', code: [
      'let factorial = n ->',
      '  if n <= 1 then',
      '    1',
      '  else',
      '    n * self(n - 1)',
      '  end;',
      'factorial(6)',
    ] },

    { type: 'header', text: 'Composition' },
    { type: 'paragraph', text: '`comp` composes functions right-to-left — the rightmost runs first:' },
    { type: 'example', code: ['(comp str inc)(41)'] },

    { type: 'header', text: 'Higher-order Functions' },
    { type: 'paragraph', text: 'Functions can be passed as arguments. This is the heart of functional programming:' },
    { type: 'example', code: ['let double = x -> x * 2;', 'map([1, 2, 3, 4], double)'] },

    { type: 'header', text: 'Apply' },
    { type: 'paragraph', text: 'Call a function with an array of arguments using `apply`:' },
    { type: 'example', code: ['apply(+, [1, 2, 3, 4, 5])'] },
  ],
}
