const { antfu } = require('@antfu/eslint-config')

const config = antfu({
  stylistic: {
    indent: 2,
  },
  typescript: {
    tsconfigPath: 'tsconfig.compile.json',
    overrides: {
      'ts/restrict-template-expressions': ['off'],
      'no-labels': ['off'],
      'no-restricted-syntax': ['off'],
      'quotes': ['error', 'single', { avoidEscape: true }],
      'ts/no-shadow': 'error',
      'ts/consistent-type-imports': 'error',
      'ts/consistent-generic-constructors': ['error', 'constructor'],
      'ts/consistent-indexed-object-style': 'error',
      'ts/consistent-type-assertions': [
        'error',
        {
          assertionStyle: 'as',
          objectLiteralTypeAssertions: 'never',
        },
      ],
    },
  },
  ignores: [
    'coverage/**/*',
    'README.md',
    'docs',
  ],
})

module.exports = config
