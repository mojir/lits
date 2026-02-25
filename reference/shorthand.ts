import type { ShorthandName } from './api'
import type { ShorthandReference } from '.'

export const shorthand: Record<ShorthandName, ShorthandReference> = {
  '-short-regexp': {
    shorthand: true,
    title: '#"pattern"',
    category: 'shorthand',
    description: 'Shorthand for `regexp(pattern)`. Only difference is that escaping is not needed.',
    examples: [
      '#"^\\s*(.*)$"',
      '#"albert"ig',
    ],
    seeAlso: ['regexp', 're-match', 'replace', 'replace-all'],
  },
  '-short-fn': {
    shorthand: true,
    title: '-> expression',
    category: 'shorthand',
    description: `
Shorthand for \`(args, ...) -> expression\`.
\`$1, $2, $3, ...\` are shorthand for the first, second, third, ... argument.

You can reference the first argument using either \`$1\` or \`$\`.
However, please note that \`$1\` and \`$\` are mutually exclusive and cannot be used simultaneously.
E.g. \`-> $ * $1\` is not valid.`,
    examples: [
      '-> $1 + $2',
      '(-> $ * $)(9)',
    ],
  },
}
