const collectionReference = require(`./categories/collection`)
const functionalReference = require(`./categories/functional`)
const arrayReference = require(`./categories/array`)
const sequenceReference = require(`./categories/sequence`)
const mathReference = require(`./categories/math`)
const miscReference = require(`./categories/misc`)
const assertReference = require(`./categories/assert`)
const objectReference = require(`./categories/object`)
const predicateReference = require(`./categories/predicate`)
const regularExpressionReference = require(`./categories/regularExpression`)
const specialExpressionsReference = require(`./categories/specialExpressions`)
const stringReference = require(`./categories/string`)
const bitwiseReference = require(`./categories/bitwise`)
const typeReference = require(`./categories/type`)

const functionReference = Object.assign(
  {},
  collectionReference,
  arrayReference,
  sequenceReference,
  mathReference,
  functionalReference,
  miscReference,
  objectReference,
  predicateReference,
  regularExpressionReference,
  specialExpressionsReference,
  stringReference,
  bitwiseReference,
  assertReference,
  typeReference,
)

const categories = [
  `Special expression`,
  `Predicate`,
  `Sequence`,
  `Collection`,
  `Array`,
  `Object`,
  `String`,
  `Math`,
  `Functional`,
  `Regular expression`,
  `Bitwise`,
  `Misc`,
  `Assert`,
  `Type`,
]

const categorizedFunctions = Object.values(functionReference)
  .reduce((result, item) => {
    if (!result.includes(item.category)) {
      result.push(item.category)
    }
    return result
  }, [])
  .sort((a, b) => categories.indexOf(a) - categories.indexOf(b))

module.exports = {
  functionReference,
  categorizedFunctions,
  categories,
}
