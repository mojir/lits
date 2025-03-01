import type { ShorthandName } from './api'
import type { ShorthandReference } from '.'

export const shorthand: Record<ShorthandName, ShorthandReference> = {
  '-short-regexp': {
    shorthand: true,
    title: '#"pattern"',
    category: 'Shorthand',
    linkName: '-short-regexp',
    clojureDocs: null,
    description: 'Shorthand for ``(regexp pattern)``',
    examples: [
      '#"^\\s*(.*)$"',
      '#"albert"ig',
    ],
    seeAlso: ['regexp'],
  },
  '-short-fn': {
    shorthand: true,
    title: '#(expression)',
    category: 'Shorthand',
    linkName: '-short-fn',
    clojureDocs: null,
    description: `
Shorthand for \`\`(fn [args] (expression))\`\`.
\`%1, %2, %3, ...\` are shorthand for the first, second, third, ... argument.

You can reference the first argument using either \`%1\` or \`%\`.
However, please note that \`%1\` and \`%\` are mutually exclusive and cannot be used simultaneously.
E.g. \`\`#(* % %1)\`\` is not valid.`,
    examples: [
      '#(+ %1 %2)',
      '#(* % %)',
    ],
    seeAlso: ['fn'],
  },
  '-short-string': {
    shorthand: true,
    title: ':abc',
    category: 'Shorthand',
    linkName: '-short-string',
    clojureDocs: null,
    description: 'Shorthand for ``"abc"``. The string can only contain `name` characters',
    examples: [
      ':abc',
      ':a-b',
    ],
  },
  '-short-dot': {
    shorthand: true,
    title: 'foo.bar',
    category: 'Shorthand',
    linkName: '-short-dot',
    clojureDocs: null,
    description: 'Shorthand for ``(foo "bar")``.',
    examples: [
      `
(def foo {:bar {:baz 42}})
foo.bar.baz`,
    ],
  },
  '-short-hash': {
    shorthand: true,
    title: 'foo#3',
    category: 'Shorthand',
    linkName: '-short-hash',
    clojureDocs: null,
    description: 'Shorthand for ``(foo 3)``.',
    examples: [
      `
(def foo {:bar [1 2 3]})
foo.bar#2`,
    ],
  },
}
