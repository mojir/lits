const listReference = require('./categories/list')
const mathReference = require('./categories/math')
const miscReference = require('./categories/misc')
const objectReference = require('./categories/object')
const predicateReference = require('./categories/predicate')
const regularExpressionReference = require('./categories/regularExpression')
const specialExpressionsReference = require('./categories/specialExpressions')
const stringReference = require('./categories/string')
const bitwiseReference = require('./categories/bitwise')

const functionReference = {
  ...listReference,
  ...mathReference,
  ...miscReference,
  ...objectReference,
  ...predicateReference,
  ...regularExpressionReference,
  ...specialExpressionsReference,
  ...stringReference,
  ...bitwiseReference,
}

const categoryNames = [
  'Special expression',
  'Predicate',
  'List',
  'Object',
  'String',
  'Math',
  'Regular expression',
  'Bitwise',
  'Misc',
]

const categories = Object.values(functionReference)
  .reduce((result, item) => {
    if (!result.includes(item.category)) {
      result.push(item.category)
    }
    return result
  }, [])
  .sort((a, b) => categoryNames.indexOf(a) - categoryNames.indexOf(b))

module.exports = {
  functionReference,
  categories,
  categoryNames,
}
