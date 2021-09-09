module.exports = {
  '+': {
    name: '+',
    linkName: '_plus',
    syntax: '+ numbers(zero or more) => number',
    arguments: [
      {
        name: 'numbers',
        type: 'numeric values',
      },
    ],
    shortDescription: '+ function computes sum of numbers',
    longDescription: '+ function computes sum of numbers',
    examples: ['(+)', '(+ 1)', '(+ 2 4)', '(+ 1 2 3 4)'],
    specialExpression: false,
    sideEffects: [],
  },
}
