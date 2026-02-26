import '../../../../src/initReferenceData'
import type { TutorialEntry } from './index'

export const dataTypesTutorial: TutorialEntry = {
  id: 'tutorial-data-types',
  title: 'Data Types',
  elements: [
    { type: 'paragraph', text: 'Lits has a small set of data types. Every value is immutable and fully serializable as JSON.' },

    { type: 'header', text: 'Numbers' },
    { type: 'paragraph', text: 'Numbers can be integers or floats. Lits also supports hexadecimal, binary, octal, and scientific notation:' },
    { type: 'example', code: ['42'] },
    { type: 'example', code: ['3.14'] },
    { type: 'example', code: ['0xFF'] },
    { type: 'example', code: ['0b1010'] },
    { type: 'example', code: ['-2.3e-2'] },

    { type: 'header', text: 'Strings' },
    { type: 'paragraph', text: 'Strings are enclosed in double quotes and support escape sequences:' },
    { type: 'example', code: ['"Hello, World!"'] },
    { type: 'example', code: ['"Line 1\\nLine 2"'] },

    { type: 'header', text: 'Booleans and Null' },
    { type: 'paragraph', text: 'The boolean values `true` and `false`, plus `null`:' },
    { type: 'example', code: ['true'] },
    { type: 'example', code: ['null'] },

    { type: 'header', text: 'Arrays' },
    { type: 'paragraph', text: 'Arrays hold ordered collections of any types:' },
    { type: 'example', code: ['[1, "two", true, null]'] },
    { type: 'paragraph', text: 'Use spread to merge arrays:' },
    { type: 'example', code: ['[1, 2, ...[3, 4], 5]'] },

    { type: 'header', text: 'Objects' },
    { type: 'paragraph', text: 'Objects are key-value maps. Keys are strings:' },
    { type: 'example', code: ['{ name: "Alice", age: 30 }'] },
    { type: 'paragraph', text: 'Spread works in objects too:' },
    { type: 'example', code: ['let defaults = { theme: "dark", lang: "en" };', '{ ...defaults, lang: "sv" }'] },

    { type: 'header', text: 'Regular Expressions' },
    { type: 'paragraph', text: 'Regexp literals start with `#"`. No need to escape backslashes:' },
    { type: 'example', code: ['re-match("abc123", #"[a-z]+(\\d+)")'] },

    { type: 'header', text: 'Type Predicates' },
    { type: 'paragraph', text: 'Check the type of a value with predicate functions that end in `?`:' },
    { type: 'example', code: ['number?(42)'] },
    { type: 'example', code: ['string?("hello")'] },
    { type: 'example', code: ['array?([1, 2, 3])'] },
    { type: 'example', code: ['object?({ a: 1 })'] },

    { type: 'header', text: 'Structural Equality' },
    { type: 'paragraph', text: 'Values are compared by structure, not by reference. Two arrays with the same elements are equal:' },
    { type: 'example', code: ['[1, 2, 3] == [1, 2, 3]'] },
    { type: 'example', code: ['{ a: 1 } == { a: 1 }'] },
  ],
}
