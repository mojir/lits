module.exports = {
  root: true,
  plugins: ['prettier'],
  extends: ['eslint:recommended', 'prettier'],
  rules: {
    'prettier/prettier': ['error'],
    'object-shorthand': 'error',
    'no-console': 'warn',
    'no-debugger': 'error',
    quotes: ['error', 'backtick'],
  },
  parserOptions: {
    sourceType: 'module',
    ecmaVersion: 2015,
  },
  env: {
    node: true,
  },
}
