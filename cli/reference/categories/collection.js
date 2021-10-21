module.exports = {
  count: {
    name: `count`,
    category: `Collection`,
    linkName: `count`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `coll`,
        type: `collection | string`,
      },
    ],
    description: `Returns number of elements in \`coll\`.`,
    examples: [`(count [1 2 3])`, `(count [])`, `(count (object "a" 1))`, `(count "")`, `(count "Albert")`],
    specialExpression: false,
  },
  get: {
    name: `get`,
    category: `Collection`,
    linkName: `get`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `coll`,
        type: `collection`,
      },
      {
        name: `key`,
        type: `string or integer`,
      },
      {
        name: `default`,
        type: `any`,
        description: `optional`,
      },
    ],
    description: `Returns value in \`coll\` mapped at \`key\`.`,
    examples: [
      `(get [1 2 3] 1)`,
      `(get [] 1)`,
      `(get [] 1 "default")`,
      `(get (object "a" 1) "a")`,
      `(get (object "a" 1) "b")`,
      `(get (object "a" 1) "b" "default")`,
    ],
    specialExpression: false,
  },
  'contains?': {
    name: `contains?`,
    category: `Collection`,
    linkName: `contains_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `coll`,
        type: `collection`,
      },
      {
        name: `key`,
        type: `string or number`,
      },
    ],
    description: `Returns \`true\` if \`collection\` contains \`key\`, otherwise returns \`false\`.`,
    examples: [
      `(contains? [] 1)`,
      `(contains? [1] 1)`,
      `(contains? [1 2 3] 1)`,
      `(contains? {} "a")`,
      `(contains? {"a" 1 "b" 2} "a")`,
    ],
    specialExpression: false,
  },
  'has?': {
    name: `has?`,
    category: `Collection`,
    linkName: `has_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `coll`,
        type: `collection`,
      },
      {
        name: `key`,
        type: `string or number`,
      },
    ],
    description: `Returns \`true\` if \`collection\` has \`value\`, otherwise returns \`false\`.`,
    examples: [
      `(has? [1 2 3] 1)`,
      `(has? [1 2 3] 0)`,
      `(has? {"a" 1 "b" 2} 1)`,
      `(has? {"a" 1 "b" 2} 0)`,
      `(has? "Albert" "A")`,
      `(has? "Albert" "a")`,
    ],
    specialExpression: false,
  },
  assoc: {
    name: `assoc`,
    category: `Collection`,
    linkName: `assoc`,
    returns: {
      type: `collection`,
    },
    arguments: [
      {
        name: `coll`,
        type: `collection`,
      },
      {
        name: `key`,
        type: `string or number`,
      },
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Sets \`value\` on specified element of \`coll\`.`,
    examples: [
      `(assoc [1 2 3] 1 "Two")`,
      `(assoc [1 2 3] 3 "Four")`,
      `(assoc {"a" 1 "b" 2} "a" "One")`,
      `(assoc {"a" 1 "b" 2} "c" "Three")`,
    ],
    specialExpression: false,
  },
  concat: {
    name: `concat`,
    category: `Collection`,
    linkName: `concat`,
    returns: {
      type: `collection`,
    },
    arguments: [
      {
        name: `value`,
        type: `collection`,
        description: `zero or more`,
      },
    ],
    description: `Concatenates array or object arguments into one collection.`,
    examples: [
      `(concat [1 2] [3 4])`,
      `(concat [] [3 4])`,
      `(concat [1 2] [])`,
      `(concat [1 2] [3 4] [5 6])`,
      `(concat [])`,
      `(concat {"a" 1 "b" 2} {"b" 1 "c" 2})`,
      `(concat {} {"a" 1})`,
    ],
    specialExpression: false,
  },
  'empty?': {
    name: `empty?`,
    category: `Collection`,
    linkName: `empty_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `coll`,
        type: `collection or string`,
      },
    ],
    description: `Returns \`true\` if \`coll\` is empty, otherwise \`false\`.`,
    examples: [
      `(empty? [])`,
      `(empty? [1 2 3])`,
      `(empty? {})`,
      `(empty? {"a" 2})`,
      `(empty? "")`,
      `(empty? "Albert")`,
    ],
    specialExpression: false,
  },
  'every?': {
    name: `every?`,
    category: `Collection`,
    linkName: `every_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `coll`,
        type: `Coll`,
      },
      {
        name: `finder`,
        type: `function`,
      },
    ],
    description: `Returns true if all entries in \`coll\` pass the test implemented by \`finder\`, otherwise returns false.`,
    examples: [
      `(every? string? ["Albert" "Mojir" 160 [1 2]])`,
      `(every? (fn [x] (> x 10)) [50 100 150 200])`,
      `(every? number? [])`,
      `(every? number? "")`,
      `(every? number? {})`,
      `(every? #(even? (second %1)) {"a" 2 "b" 4})`,
      `(every? #(even? (second %1)) {"a" 2 "b" 3})`,
    ],
    specialExpression: false,
  },
  'not-every?': {
    name: `not-every?`,
    category: `Collection`,
    linkName: `not-every_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `coll`,
        type: `Coll`,
      },
      {
        name: `finder`,
        type: `function`,
      },
    ],
    description: `Returns true if all entries in \`coll\` pass the test implemented by \`finder\`, otherwise returns false.`,
    examples: [
      `(not-every? string? ["Albert" "Mojir" 160 [1 2]])`,
      `(not-every? (fn [x] (> x 10)) [50 100 150 200])`,
      `(not-every? number? [])`,
      `(not-every? number? "")`,
      `(not-every? number? {})`,
      `(not-every? #(even? (second %1)) {"a" 2 "b" 4})`,
      `(not-every? #(even? (second %1)) {"a" 2 "b" 3})`,
    ],
    specialExpression: false,
  },
  'any?': {
    name: `any?`,
    category: `Collection`,
    linkName: `any_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `coll`,
        type: `Coll`,
      },
      {
        name: `finder`,
        type: `function`,
      },
    ],
    description: `Returns true if any entry in \`coll\` pass the test implemented by \`finder\`, otherwise returns false.`,
    examples: [
      `(any? string? ["Albert" "Mojir" 160 [1 2]])`,
      `(any? (fn [x] (> x 10)) [50 100 150 200])`,
      `(any? number? [])`,
      `(any? number? "")`,
      `(any? number? {})`,
      `(any? #(even? (second %1)) {"a" 2 "b" 3})`,
      `(any? #(even? (second %1)) {"a" 1 "b" 3})`,
    ],
    specialExpression: false,
  },
  'not-any?': {
    name: `not-any?`,
    category: `Collection`,
    linkName: `not-any_question`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `coll`,
        type: `Coll`,
      },
      {
        name: `finder`,
        type: `function`,
      },
    ],
    description: `Returns false if any entry in \`coll\` pass the test implemented by \`finder\`, otherwise returns true.`,
    examples: [
      `(not-any? string? ["Albert" "Mojir" 160 [1 2]])`,
      `(not-any? (fn [x] (> x 10)) [50 100 150 200])`,
      `(not-any? number? [])`,
      `(not-any? number? "")`,
      `(not-any? number? {})`,
      `(not-any? #(even? (second %1)) {"a" 2 "b" 3})`,
      `(not-any? #(even? (second %1)) {"a" 1 "b" 3})`,
    ],
    specialExpression: false,
  },
}
