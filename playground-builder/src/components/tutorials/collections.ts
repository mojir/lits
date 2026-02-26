import '../../../../src/initReferenceData'
import type { TutorialEntry } from './index'

export const collectionsTutorial: TutorialEntry = {
  id: 'tutorial-collections',
  title: 'Collections',
  elements: [
    { type: 'paragraph', text: 'Lits has three collection types: arrays, objects, and strings. All are immutable — every operation returns a new collection.' },

    { type: 'header', text: 'Arrays' },
    { type: 'paragraph', text: 'Arrays are ordered, mixed-type collections. Access elements by index:' },
    { type: 'example', code: ['let arr = [10, 20, 30, 40];', 'arr[2]'] },
    { type: 'example', code: ['first([1, 2, 3])'] },
    { type: 'example', code: ['rest([1, 2, 3])'] },

    { type: 'header', text: 'Map, Filter, Reduce' },
    { type: 'paragraph', text: 'The core trio for transforming collections:' },
    { type: 'example', code: ['map([1, 2, 3, 4], -> $ * $)'] },
    { type: 'example', code: ['filter([1, 2, 3, 4, 5, 6], even?)'] },
    { type: 'example', code: ['reduce([1, 2, 3, 4], +, 0)'] },

    { type: 'header', text: 'More Sequence Operations' },
    { type: 'paragraph', text: 'Slicing, reversing, sorting, and more:' },
    { type: 'example', code: ['slice([10, 20, 30, 40, 50], 1, 4)'] },
    { type: 'example', code: ['reverse([1, 2, 3])'] },
    { type: 'example', code: ['sort([3, 1, 4, 1, 5])'] },

    { type: 'header', text: 'Building Arrays' },
    { type: 'paragraph', text: 'Generate arrays with `range`, `repeat`, and `push`:' },
    { type: 'example', code: ['range(5)'] },
    { type: 'example', code: ['range(2, 10, 3)'] },
    { type: 'example', code: ['repeat("x", 4)'] },
    { type: 'example', code: ['push([1, 2], 3, 4)'] },

    { type: 'header', text: 'Objects' },
    { type: 'paragraph', text: 'Objects are key-value maps. Use `get`, `assoc`, and `dissoc` to work with them:' },
    { type: 'example', code: ['let user = { name: "Alice", age: 30 };', 'get(user, "name")'] },
    { type: 'example', code: ['let user = { name: "Alice", age: 30 };', 'assoc(user, "age", 31)'] },
    { type: 'example', code: ['keys({ a: 1, b: 2, c: 3 })'] },
    { type: 'example', code: ['vals({ a: 1, b: 2, c: 3 })'] },

    { type: 'header', text: 'Merging Objects' },
    { type: 'paragraph', text: 'Combine objects with `merge` — later keys win:' },
    { type: 'example', code: ['merge({ a: 1, b: 2 }, { b: 3, c: 4 })'] },

    { type: 'header', text: 'Collection Predicates' },
    { type: 'paragraph', text: 'Test properties of collections:' },
    { type: 'example', code: ['empty?([])'] },
    { type: 'example', code: ['contains?([1, 2, 3], 1)'] },
    { type: 'example', code: ['count("hello")'] },
  ],
}
