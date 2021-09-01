module.exports = {
  root: true,
  parser: '@typescript-eslint/parser',
  plugins: ['@typescript-eslint', 'prettier'],
  extends: [
    'eslint:recommended',
    'plugin:@typescript-eslint/eslint-recommended',
    'plugin:@typescript-eslint/recommended',
    'prettier',
  ],
  rules: {
    'prettier/prettier': ['error'],
    'object-shorthand': 'error',
    'no-console': 'warn',
    'no-debugger': 'error',
    '@typescript-eslint/no-unused-vars': ['error', { argsIgnorePattern: '^_', ignoreRestSiblings: true }],
  },
  parserOptions: {
    sourceType: 'module',
    ecmaVersion: 2015,
  },
}
