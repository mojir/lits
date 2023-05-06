module.exports = {
  'type-of': {
    name: `type-of`,
    category: `Type`,
    linkName: `type-of`,
    returns: {
      type: `type`,
    },
    arguments: [
      {
        name: `value`,
        type: `Any`,
      },
    ],
    description: `Returns the type of \`value\`.`,
    examples: [
      `(type-of -1.1)`,
      `(type-of -1)`,
      `(type-of 0)`,
      `(type-of 1)`,
      `(type-of 1.1)`,
      `(type-of true)`,
      `(type-of false)`,
      `(type-of (/ 0 0))`,
      `(type-of (/ 1 0))`,
      `(type-of (/ -1 0))`,
      `(type-of "")`,
      `(type-of :Foo)`,
      `(type-of [])`,
      `(type-of [1 2 3])`,
      `(type-of {})`,
      `(type-of { :foo :bar })`,
      `(type-of #"^foo")`,
      `(type-of #(identity %1))`,
      `(type-of nil)`,
    ],
  },
  'type-split': {
    name: `type-split`,
    category: `Type`,
    linkName: `type-split`,
    returns: {
      type: `type[]`,
    },
    arguments: [
      {
        name: `type`,
        type: `type`,
      },
    ],
    description: `Splits a type into its primitive components.`,
    examples: [
      `(type-split ::zero)`,
      `(type-split ::array)`,
      `(type-split ::infinity)`,
      `(type-split ::float)`,
      `(type-split ::string)`,
      `(type-split ::unknown)`,
    ],
  },
  'type-to-value': {
    name: `type-to-value`,
    category: `Type`,
    linkName: `type-to-value`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `If \`value\` is not a type, \`value\` is returned. If \`value\` is a type then a value representing the value is returned, if no such value exists, \`value\` is returned.`,
    examples: [
      `(type-to-value 1)`,
      `(type-to-value "A string")`,
      `(type-to-value ::zero)`,
      `(type-to-value ::nan)`,
      `(type-to-value ::empty-string)`,
      `(type-to-value ::nil)`,
      `(type-to-value ::false)`,
      `(type-to-value ::true)`,
      `(type-to-value ::boolean)`,
      `(type-to-value (type-exclude ::boolean ::false))`,
      `(type-to-value ::string)`,
    ],
  },
  'type-or': {
    name: `type-or`,
    category: `Type`,
    linkName: `type-or`,
    returns: {
      type: `type`,
    },
    arguments: [
      {
        name: `types`,
        type: `type[]`,
        description: `one or many`,
      },
    ],
    description: `Returns the disjuntion of the \`types\`.`,
    examples: [
      `(type-or ::float ::positive-infinity ::negative-infinity ::nan)`,
      `(type-or ::float ::true ::false)`,
      `(type-or ::positive-integer ::negative-integer ::zero)`,
    ],
  },
  'type-and': {
    name: `type-and`,
    category: `Type`,
    linkName: `type-and`,
    returns: {
      type: `type`,
    },
    arguments: [
      {
        name: `types`,
        type: `type[]`,
        description: `one or many`,
      },
    ],
    description: `Return the conjuction of the \`types\`.`,
    examples: [
      `(type-and ::float ::integer)`,
      `(type-and ::float ::string)`,
      `(type-and ::non-positive-integer ::non-negative-float)`,
    ],
  },
  'type-exclude': {
    name: `type-exclude`,
    category: `Type`,
    linkName: `type-exclude`,
    returns: {
      type: `type`,
    },
    arguments: [
      {
        name: `types`,
        type: `type[]`,
        description: `one or many`,
      },
    ],
    description: `Subtract types from the first parameter.`,
    examples: [
      `(type-exclude ::float ::integer)`,
      `(type-exclude ::float ::string)`,
      `(type-exclude ::float ::non-negative-float)`,
    ],
  },
  'type-is?': {
    name: `type-is?`,
    category: `Type`,
    linkName: `type-is_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `input`,
        type: `any`,
      },
      {
        name: `type`,
        type: `type`,
      },
    ],
    description: `Check if \`input\` is of type \`type\`.`,
    examples: [
      `(type-is? ::float ::integer)`,
      `(type-is? ::integer ::float)`,
      `(type-is? ::empty-string ::string)`,
      `(type-is? ::empty-string ::falsy)`,
      `(type-is? ::empty-string ::truthy)`,
      `(type-is? [1 2 3] (::array ::positive-integer))`,
      `(type-is? [0 1 2 3] (::array ::positive-integer))`,
    ],
  },
  'type-equals?': {
    name: `type-equals?`,
    category: `Type`,
    linkName: `type-equals_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `type1`,
        type: `type`,
      },
      {
        name: `type2`,
        type: `type`,
      },
    ],
    description: `Check if \`type1\` is equal to \`type2\`.`,
    examples: [
      `(type-equals? ::integer ::float)`,
      `(type-equals? ::empty-string ::string)`,
      `(type-equals? ::empty-string (type-and ::falsy ::boolean))`,
    ],
  },
  'type-intersects?': {
    name: `type-intersects?`,
    category: `Type`,
    linkName: `type-intersects_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `type1`,
        type: `type`,
      },
      {
        name: `type2`,
        type: `type`,
      },
    ],
    description: `Check if \`type1\` intersects \`type2\`.`,
    examples: [
      `(type-intersects? ::float ::integer)`,
      `(type-intersects? ::integer ::float)`,
      `(type-intersects? ::empty-string ::string)`,
      `(type-intersects? ::empty-string ::falsy)`,
      `(type-intersects? ::empty-string ::truthy)`,
    ],
  },
}
