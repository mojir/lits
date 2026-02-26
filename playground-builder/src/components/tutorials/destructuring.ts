import '../../../../src/initReferenceData'
import type { TutorialEntry } from './index'

export const destructuringTutorial: TutorialEntry = {
  id: 'tutorial-destructuring',
  title: 'Destructuring',
  elements: [
    { type: 'paragraph', text: 'Destructuring lets you extract values from arrays and objects directly into named bindings. It works in `let` bindings and function parameters.' },

    { type: 'header', text: 'Object Destructuring' },
    { type: 'paragraph', text: 'Pull out properties by name:' },
    { type: 'example', code: ['let { name, age } = { name: "Alice", age: 30 };', 'name'] },

    { type: 'header', text: 'Default Values' },
    { type: 'paragraph', text: 'Provide defaults for missing properties:' },
    { type: 'example', code: ['let { name, role = "guest" } = { name: "Bob" };', 'role'] },

    { type: 'header', text: 'Renaming with as' },
    { type: 'paragraph', text: 'Use `as` to bind a property to a different variable name:' },
    { type: 'example', code: ['let { name as userName } = { name: "Carol" };', 'userName'] },

    { type: 'header', text: 'Nested Object Destructuring' },
    { type: 'paragraph', text: 'Reach into nested structures:' },
    { type: 'example', code: ['let { profile: { age } } = { profile: { age: 25 } };', 'age'] },

    { type: 'header', text: 'Array Destructuring' },
    { type: 'paragraph', text: 'Extract elements by position. Use commas to skip elements:' },
    { type: 'example', code: ['let [a, b, c] = [10, 20, 30];', 'b'] },
    { type: 'example', code: ['let [, , third] = [1, 2, 3];', 'third'] },

    { type: 'header', text: 'Array Defaults' },
    { type: 'paragraph', text: 'Array elements can have defaults too:' },
    { type: 'example', code: ['let [x, y = 99] = [42];', 'y'] },

    { type: 'header', text: 'Rest Patterns' },
    { type: 'paragraph', text: 'Collect remaining elements with `...`:' },
    { type: 'example', code: ['let [head, ...tail] = [1, 2, 3, 4, 5];', 'tail'] },
    { type: 'example', code: ['let { user, ...others } = { user: "Eve", age: 28, city: "NYC" };', 'others'] },

    { type: 'header', text: 'Function Parameter Destructuring' },
    { type: 'paragraph', text: 'Destructure directly in function parameters:' },
    { type: 'example', code: ['let greet = ({ name }) -> "Hello, " ++ name;', 'greet({ name: "Pat" })'] },
    { type: 'example', code: ['let sum-pair = ([a, b]) -> a + b;', 'sum-pair([3, 7])'] },

    { type: 'header', text: 'Combining Patterns' },
    { type: 'paragraph', text: 'Mix array and object destructuring for complex data:' },
    { type: 'example', code: ['let [{ name }, { age }] = [{ name: "Tina" }, { age: 33 }];', 'name ++ " is " ++ str(age)'] },
  ],
}
