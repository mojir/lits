import '../../../../src/initReferenceData'
import type { TutorialEntry } from './index'

export const gettingStartedTutorial: TutorialEntry = {
  id: 'tutorial-getting-started',
  title: 'Getting Started',
  elements: [
    { type: 'paragraph', text: 'Welcome to Lits — a pure functional programming language that runs in JavaScript. Every piece of syntax produces a value (there are no statements), all data is immutable, and functions are first-class values.' },

    { type: 'header', text: 'Expressions' },
    { type: 'paragraph', text: 'Everything in Lits is an expression that evaluates to a value. Lits uses algebraic notation, so you can write math naturally:' },
    { type: 'example', code: ['10 + 20'] },
    { type: 'paragraph', text: 'Operators require whitespace around them. `x+1` is a variable name, not `x + 1`.' },

    { type: 'header', text: 'Variables' },
    { type: 'paragraph', text: 'Use `let` to bind values to names. Variables are immutable — once bound, they cannot be reassigned:' },
    { type: 'example', code: ['let greeting = "World";', 'str("Hello, ", greeting, "!")'] },
    { type: 'paragraph', text: 'Semicolons separate expressions. The last expression is the return value — no trailing semicolon needed.' },

    { type: 'header', text: 'Functions' },
    { type: 'paragraph', text: 'Define functions with the arrow (`->`) syntax:' },
    { type: 'example', code: ['let square = x -> x * x;', 'square(5)'] },

    { type: 'header', text: 'Collections' },
    { type: 'paragraph', text: 'Arrays and objects are the primary data structures. Use functions like `map`, `filter`, and `reduce` to work with them:' },
    { type: 'example', code: ['let numbers = [1, 2, 3, 4, 5];', 'reduce(numbers, +, 0)'] },

    { type: 'header', text: 'Control Flow' },
    { type: 'paragraph', text: 'Use `if` / `then` / `else` / `end` for conditional expressions. Since everything is an expression, `if` returns a value:' },
    { type: 'example', code: ['if 10 > 5 then "big" else "small" end'] },

    { type: 'header', text: 'Immutability' },
    { type: 'paragraph', text: 'Data in Lits is always immutable. Operations return new values rather than modifying existing ones:' },
    { type: 'example', code: ['let original = [1, 2, 3];', 'let extended = push(original, 4);', 'original'] },
    { type: 'paragraph', text: 'The original array is unchanged. `push` returned a new array.' },
  ],
}
