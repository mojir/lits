import '../../../../src/initReferenceData'
import type { TutorialEntry } from './index'

export const pipesDataFlowTutorial: TutorialEntry = {
  id: 'tutorial-pipes-data-flow',
  title: 'Pipes & Data Flow',
  elements: [
    { type: 'paragraph', text: 'Lits has powerful features for building readable data transformation pipelines: the pipe operator, data-as-functions, and operator-style calling.' },

    { type: 'header', text: 'The Pipe Operator' },
    { type: 'paragraph', text: 'Use `|>` to pass a value through a chain of transformations. Use `_` to mark where the piped value goes:' },
    { type: 'example', code: [
      '[1, 2, 3, 4, 5, 6]',
      '  |> filter(_, odd?)',
      '  |> map(_, -> $ * $)',
      '  |> reduce(_, +, 0)',
    ] },

    { type: 'header', text: 'Pipe Without Placeholder' },
    { type: 'paragraph', text: 'When piping to a single-argument function, no placeholder is needed:' },
    { type: 'example', code: [
      '"hello world"',
      '  |> upper-case',
      '  |> split(_, " ")',
      '  |> reverse',
      '  |> join(_, "-")',
    ] },

    { type: 'header', text: 'Operator Style' },
    { type: 'paragraph', text: 'Any two-argument function can be used as an infix operator. The left value becomes the first argument:' },
    { type: 'example', code: ['[1, 2, 3, 4] filter odd?'] },
    { type: 'example', code: ['[1, 2, 3] map inc'] },

    { type: 'header', text: 'Chaining Operators' },
    { type: 'paragraph', text: 'Chain multiple operator-style calls for a fluent reading:' },
    { type: 'example', code: ['[1, 2, 3, 4, 5, 6] filter even? map -> $ * $'] },

    { type: 'header', text: 'Arrays as Functions' },
    { type: 'paragraph', text: 'An array can be called as a function with an index to get that element:' },
    { type: 'example', code: ['let arr = [10, 20, 30];', 'arr(1)'] },

    { type: 'header', text: 'Numbers as Functions' },
    { type: 'paragraph', text: 'A number can be called with a collection to access that index:' },
    { type: 'example', code: ['let words = ["alpha", "beta", "gamma"];', '1(words)'] },

    { type: 'header', text: 'Strings as Functions' },
    { type: 'paragraph', text: 'A string can be called with an object to access that property. This is powerful with `map`:' },
    { type: 'example', code: ['let people = [{ name: "Alice" }, { name: "Bob" }];', 'people map "name"'] },

    { type: 'header', text: 'Objects as Functions' },
    { type: 'paragraph', text: 'An object can be called with a key to get the value:' },
    { type: 'example', code: ['let config = { host: "localhost", port: 8080 };', 'config("port")'] },

    { type: 'header', text: 'Putting it Together' },
    { type: 'paragraph', text: 'Combine pipes, data-as-functions, and operators for expressive data processing:' },
    { type: 'example', code: [
      'let data = [',
      '  { name: "Alice", score: 95 },',
      '  { name: "Bob", score: 60 },',
      '  { name: "Carol", score: 82 }',
      '];',
      'data',
      '  |> filter(_, -> $.score >= 80)',
      '  |> map(_, "name")',
    ] },
  ],
}
