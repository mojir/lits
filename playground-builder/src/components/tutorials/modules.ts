import '../../../../src/initReferenceData'
import type { TutorialEntry } from './index'

export const modulesTutorial: TutorialEntry = {
  id: 'tutorial-modules',
  title: 'Modules',
  elements: [
    { type: 'paragraph', text: 'Lits provides domain-specific function libraries as opt-in modules. Import them to access specialized functionality.' },

    { type: 'header', text: 'Importing a Module' },
    { type: 'paragraph', text: 'Use `import` to load a module. It returns an object whose keys are the module\'s functions:' },
    { type: 'example', code: ['let m = import(math);', 'm.sin(PI / 2)'] },

    { type: 'header', text: 'Destructured Import' },
    { type: 'paragraph', text: 'Combine `import` with destructuring to pull out individual functions:' },
    { type: 'example', code: ['let { sin, cos } = import(math);', 'sin(PI / 6)'] },

    { type: 'header', text: 'Math Module' },
    { type: 'paragraph', text: 'Trigonometric, logarithmic, and angle-conversion functions:' },
    { type: 'example', code: ['let { ln, log10 } = import(math);', '[ln(E), log10(1000)]'] },

    { type: 'header', text: 'Sequence Module' },
    { type: 'paragraph', text: 'Extended sequence operations — `sort-by`, `distinct`, `group-by`, and more:' },
    { type: 'example', code: ['let seq = import(sequence);', 'seq.distinct([1, 2, 2, 3, 3, 3])'] },
    { type: 'example', code: ['let seq = import(sequence);', 'seq.sort-by(["banana", "fig", "apple"], count)'] },

    { type: 'header', text: 'Collection Module' },
    { type: 'paragraph', text: 'Deep access and advanced aggregation:' },
    { type: 'example', code: ['let col = import(collection);', 'let data = { user: { name: "Alice" } };', 'col.get-in(data, ["user", "name"])'] },

    { type: 'header', text: 'Vector Module' },
    { type: 'paragraph', text: 'Statistical functions for number arrays:' },
    { type: 'example', code: ['let vec = import(vector);', 'vec.cumsum([1, 2, 3, 4])'] },

    { type: 'header', text: 'Linear Algebra Module' },
    { type: 'paragraph', text: 'Vector math — dot products, distances, normalization:' },
    { type: 'example', code: ['let lin = import(linear-algebra);', 'lin.dot([1, 2, 3], [4, 5, 6])'] },

    { type: 'header', text: 'Matrix Module' },
    { type: 'paragraph', text: 'Matrix operations — multiplication, determinant, inverse:' },
    { type: 'example', code: ['let mat = import(matrix);', 'mat.det([[1, 2], [3, 4]])'] },

    { type: 'header', text: 'Random Module' },
    { type: 'paragraph', text: 'Random number generation (functions end with `!` to signal side effects):' },
    { type: 'example', code: ['let rng = import(random);', 'rng.random-int!(1, 100)'] },

    { type: 'header', text: 'String Module' },
    { type: 'paragraph', text: 'Additional string utilities:' },
    { type: 'example', code: ['let s = import(string);', 's.pad-left("42", 5, "0")'] },

    { type: 'header', text: 'Number Theory Module' },
    { type: 'paragraph', text: 'GCD, LCM, primality-related functions:' },
    { type: 'example', code: ['let nt = import(number-theory);', 'nt.gcd(24, 36)'] },
  ],
}
