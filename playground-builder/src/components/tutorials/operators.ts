import '../../../../src/initReferenceData'
import type { TutorialEntry } from './index'

export const operatorsTutorial: TutorialEntry = {
  id: 'tutorial-operators',
  title: 'Operators',
  elements: [
    { type: 'paragraph', text: 'Lits has a rich set of operators. A unique feature is that operators and functions are interchangeable — operators can be called as functions and two-argument functions can be used as infix operators.' },

    { type: 'header', text: 'Arithmetic' },
    { type: 'paragraph', text: 'The standard math operators, with whitespace required:' },
    { type: 'example', code: ['2 + 3 * 4'] },
    { type: 'example', code: ['2 ^ 10'] },
    { type: 'example', code: ['17 % 5'] },

    { type: 'header', text: 'Comparison' },
    { type: 'paragraph', text: 'Comparison operators use structural equality (`==`), not reference equality:' },
    { type: 'example', code: ['3 > 2'] },
    { type: 'example', code: ['[1, 2] == [1, 2]'] },
    { type: 'example', code: ['1 != 2'] },

    { type: 'header', text: 'String Concatenation' },
    { type: 'paragraph', text: 'Use `++` to concatenate strings and sequences:' },
    { type: 'example', code: ['"Hello" ++ ", " ++ "World!"'] },
    { type: 'example', code: ['[1, 2] ++ [3, 4]'] },

    { type: 'header', text: 'Logical Operators' },
    { type: 'paragraph', text: '`&&` and `||` are short-circuit. `??` is the nullish coalescing operator:' },
    { type: 'example', code: ['true && "yes"'] },
    { type: 'example', code: ['false || "fallback"'] },
    { type: 'example', code: ['null ?? "default"'] },

    { type: 'header', text: 'Operators as Functions' },
    { type: 'paragraph', text: 'Every operator can be called in function (prefix) form. Some are variadic:' },
    { type: 'example', code: ['+(1, 2, 3, 4, 5)'] },
    { type: 'example', code: ['*(2, 3, 4)'] },
    { type: 'example', code: ['<(1, 2, 3, 4)'] },

    { type: 'header', text: 'Functions as Operators' },
    { type: 'paragraph', text: 'Any two-argument function can be used as an infix operator:' },
    { type: 'example', code: ['5 max 10'] },
    { type: 'example', code: ['[1, 2, 3, 4] filter odd?'] },

    { type: 'header', text: 'Partial Application' },
    { type: 'paragraph', text: 'Use `_` as a placeholder to create partially applied functions from operators:' },
    { type: 'example', code: ['let add5 = +(5, _);', 'add5(10)'] },
    { type: 'example', code: ['let half = /(_, 2);', 'half(20)'] },

    { type: 'header', text: 'Ternary Operator' },
    { type: 'paragraph', text: 'The ternary `? :` works as you would expect:' },
    { type: 'example', code: ['let age = 25;', 'age >= 18 ? "adult" : "minor"'] },
  ],
}
