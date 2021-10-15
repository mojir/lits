module.exports = {
  nth: {
    name: `nth`,
    category: `Sequence`,
    linkName: `nth`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `input`,
        type: `string | array`,
      },
      {
        name: `index`,
        type: `integer`,
      },
    ],
    description: `Accesses element \`index\` of \`input\`. Negative \`index\` counts backwards. Accessing out-of-bounds indices returns \`undefined\`.`,
    examples: [
      `(nth [1 2 3] 1)`,
      `(nth [1 2 3] 3)`,
      `(nth [1 2 3] -1)`,
      `(nth [1 2 3] -1)`,
      `(nth "A string" 1)`,
      `(nth "A string" 3)`,
      `(nth "A string" -3)`,
      `(nth "A string" 30)`,
      `(nth "A string" -30)`,
    ],
    specialExpression: false,
  },
  push: {
    name: `push`,
    category: `Sequence`,
    linkName: `push`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
      {
        name: `values`,
        type: `array`,
        description: `one or more`,
      },
    ],
    description: `Pushes \`values\` to the end of \`array\`.`,
    examples: [`(push [1 2 3] 4)`, `(push [1 2 3] 4 5 6)`, `(def l [1 2 3]) (push l 4) l`],
    specialExpression: false,
  },
  pop: {
    name: `pop`,
    category: `Sequence`,
    linkName: `pop`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Removes and returns then last item of \`array\`. If \`array\` is empty, \`undefined\` is returned.`,
    examples: [`(pop [1 2 3])`, `(pop [])`],
    specialExpression: false,
  },
  unshift: {
    name: `unshift`,
    category: `Sequence`,
    linkName: `unshift`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
      {
        name: `values`,
        type: `array`,
        description: `one or more`,
      },
    ],
    description: `Inserts \`values\` at the beginning of \`array\`.`,
    examples: [`(unshift [1 2 3] 4)`, `(unshift [1 2 3] 4 5 6)`, `(def l [1 2 3]) (unshift l 4) l`],
    specialExpression: false,
  },
  shift: {
    name: `shift`,
    category: `Sequence`,
    linkName: `shift`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Removes and returns the first item of \`array\`. If \`array\` is empty, \`undefined\` is returned.`,
    examples: [`(shift [1 2 3])`, `(shift [])`],
    specialExpression: false,
  },
  slice: {
    name: `slice`,
    category: `Sequence`,
    linkName: `slice`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
      {
        name: `start`,
        type: `number`,
        description: `optional`,
      },
      {
        name: `end`,
        type: `number`,
        description: `optional`,
      },
    ],
    description: `Returns a shallow copy of a portion of \`array\` into a new array selected from index \`start\` (inclusive) to index \`end\` (exclusive). If \`start\` is not provided it defaults to \`0\`. If \`end\` is not provided, the rest of the array will be copied.`,
    examples: [`(slice [1 2 3 4 5] 2 4)`, `(slice [1 2 3 4 5] 2)`],
    specialExpression: false,
  },
  reduce: {
    name: `reduce`,
    category: `Sequence`,
    linkName: `reduce`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
      {
        name: `reducer`,
        type: `function`,
      },
      {
        name: `startValue`,
        type: `any`,
      },
    ],
    description: `Runs \`reducer\` function on each element of the \`array\`, passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the array is a single value.`,
    examples: [
      `(reduce + [1 2 3] 0)`,
      `(reduce + [] 0)`,
      `(reduce (fn [result value] (+ result (if (even? value) value 0))) [1 2 3 4 5 6 7 8 9] 0)`,
    ],
    specialExpression: false,
  },
  'reduce-right': {
    name: `reduce-right`,
    category: `Sequence`,
    linkName: `reduce-right`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
      {
        name: `reducer`,
        type: `function`,
      },
      {
        name: `startValue`,
        type: `any`,
      },
    ],
    description: `Runs \`reducer\` function on each element of the \`array\` (starting from the last item), passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the array is a single value.`,
    examples: [`(reduce-right str ["A" "B" "C"] "")`],
    specialExpression: false,
  },
  map: {
    name: `map`,
    category: `Sequence`,
    linkName: `map`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `mapper`,
        type: `function`,
      },
      {
        name: `array`,
        type: `array`,
        description: `one or many`,
      },
    ],
    description: `Creates a new array populated with the results of calling \`mapper\` on every elements in the calling \`array\`s.`,
    examples: [
      `(map string ["Albert" "Mojir"])`,
      `(map string [])`,
      `(map + [1 2 3] [1 2 3])`,
      `(map max [2 6 3] [2 4 7] [1 6 2])`,
    ],
    specialExpression: false,
  },
  filter: {
    name: `filter`,
    category: `Sequence`,
    linkName: `filter`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
      {
        name: `filter`,
        type: `function`,
      },
    ],
    description: `Creates a new array with all elements that pass the test implemented by \`filter\`.`,
    examples: [`(filter string? ["Albert" "Mojir" 160 [1 2]])`, `(filter (fn [x] (> x 10)) [5 10 15 20])`],
    specialExpression: false,
  },
  position: {
    name: `position`,
    category: `Sequence`,
    linkName: `position`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
      {
        name: `finder`,
        type: `function`,
      },
    ],
    description: `Returns the index of the first elements that pass the test implemented by \`finder\`. If no element was found, \`undefined\` is returned.`,
    examples: [
      `(position string? ["Albert" "Mojir" 160 [1 2]])`,
      `(position (fn [x] (> x 10)) [5 10 15 20])`,
      `(position (fn [x] (> x 100)) [5 10 15 20])`,
    ],
    specialExpression: false,
  },
  'index-of': {
    name: `index-of`,
    category: `Sequence`,
    linkName: `index-of`,
    returns: {
      type: `number`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns the index of \`value\` in array. If element is not present in \`array\` \`undefined\` is returned.`,
    examples: [
      `(index-of "Mojir" ["Albert" "Mojir" 160 [1 2]])`,
      `(index-of 15 [5 10 15 20])`,
      `(index-of 1 [5 10 15 20])`,
    ],
    specialExpression: false,
  },
  some: {
    name: `some`,
    category: `Sequence`,
    linkName: `some`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
      {
        name: `finder`,
        type: `function`,
      },
    ],
    description: `Returns the first elements that pass the test implemented by \`finder\`. I no element was found, \`undefined\` is returned.`,
    examples: [
      `(some string? ["Albert" "Mojir" 160 [1 2]])`,
      `(some (fn [x] (> x 10)) [5 10 15 20])`,
      `(some (fn [x] (> x 10)) [1 2 3 4])`,
      `(some (fn [x] (> x 10)) [])`,
    ],
    specialExpression: false,
  },
  reverse: {
    name: `reverse`,
    category: `Sequence`,
    linkName: `reverse`,
    returns: {
      type: `array or string`,
    },
    arguments: [
      {
        name: `input`,
        type: `input or string`,
      },
    ],
    description: `If \`input\` is an array, creates a new array with the elements from \`input\` in reversed order. If \`input\` is a string, returns new reversed string.`,
    examples: [`(reverse ["Albert" "Mojir" 160 [1 2]])`, `(reverse [])`, `(reverse "Albert")`],
    specialExpression: false,
  },
  first: {
    name: `first`,
    category: `Sequence`,
    linkName: `first`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
    ],
    description: `Returns the first element of \`array\`. If \`array\` is empty, \`undefined\` is returned.`,
    examples: [`(first ["Albert" "Mojir" 160 [1 2]])`, `(first [])`],
    specialExpression: false,
  },
  second: {
    name: `second`,
    category: `Sequence`,
    linkName: `second`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
    ],
    description: `Returns the second element of \`array\`. If \`array\` has less than two elements, \`undefined\` is returned.`,
    examples: [`(second ["Albert" "Mojir" 160 [1 2]])`, `(second [1])`, `(second [])`],
    specialExpression: false,
  },
  last: {
    name: `last`,
    category: `Sequence`,
    linkName: `last`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
    ],
    description: `Returns the last element of \`array\`. If \`array\` is empty, \`undefined\` is returned.`,
    examples: [`(last ["Albert" "Mojir" 160 [1 2]])`, `(last [1 2])`, `(last [1])`, `(last [])`],
    specialExpression: false,
  },
  rest: {
    name: `rest`,
    category: `Sequence`,
    linkName: `rest`,
    returns: {
      type: `array | string`,
    },
    arguments: [
      {
        name: `input`,
        type: `array | string`,
      },
    ],
    description: `If \`input\` is an array, returns a new array with all but the first element from \`input\`. If \`input\` has less than two elements, an empty array is returned. For string \`input\` returns all but the first characters in \`input\`.`,
    examples: [
      `(rest ["Albert" "Mojir" 160 [1 2]])`,
      `(rest ["Albert"])`,
      `(rest [])`,
      `(rest "Albert")`,
      `(rest "A")`,
      `(rest "")`,
    ],
    specialExpression: false,
  },
  next: {
    name: `next`,
    category: `Sequence`,
    linkName: `next`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
    ],
    description: `If \`input\` is an array, returns a new array with all but the first element from \`input\`. If \`input\` has less than two elements, an empty array is returned. For string \`input\` returns all but the first characters in \`input\`. If length of string \`input\` is less than two, undefined is returned.`,
    examples: [
      `(next ["Albert" "Mojir" 160 [1 2]])`,
      `(next ["Albert"])`,
      `(next [])`,
      `(next "Albert")`,
      `(next "A")`,
      `(next "")`,
    ],
    specialExpression: false,
  },
  cons: {
    name: `cons`,
    category: `Sequence`,
    linkName: `cons`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
    ],
    description: `Constructs a new array with \`element\` as first element and \`rest\` as the rest.`,
    examples: [`(cons "Hi" ["Albert" "Mojir" 160 [1 2]])`, `(cons "Hi" [])`],
    specialExpression: false,
  },
  take: {
    name: `take`,
    category: `Sequence`,
    linkName: `take`,
    returns: {
      type: `array | string`,
    },
    arguments: [
      {
        name: `count`,
        type: `integer`,
      },
      {
        name: `input`,
        type: `array | string`,
      },
    ],
    description: `Constructs a new array/string with the \`count\` first elements from \`input\`.`,
    examples: [`(take 3 [1 2 3 4 5])`, `(take 0 [1 2 3 4 5])`, `(take 2 "Albert")`, `(take 50 "Albert")`],
    specialExpression: false,
  },

  'take-last': {
    name: `take-last`,
    category: `Sequence`,
    linkName: `take-last`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
      {
        name: `count`,
        type: `integer`,
      },
    ],
    description: `Constructs a new array with the \`count\` last elements from \`array\`.`,
    examples: [`(take-last 3 [1 2 3 4 5])`, `(take-last 0 [1 2 3 4 5])`],
    specialExpression: false,
  },

  'take-while': {
    name: `take-while`,
    category: `Sequence`,
    linkName: `take-while`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `predicate`,
        type: `function`,
      },
      {
        name: `array`,
        type: `array`,
      },
    ],
    description: `Returns the members of \`array\` in order, stopping before the first one for which \`predicate\` returns a falsy value.`,
    examples: [`(take-while (fn [x] (< x 3)) [1 2 3 2 1])`, `(take-while (fn [x] (> x 3)) [1 2 3 2 1])`],
    specialExpression: false,
  },

  sort: {
    name: `sort`,
    category: `Sequence`,
    linkName: `sort`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `comparer`,
        type: `function`,
      },
      {
        name: `array`,
        type: `array`,
      },
    ],
    description: `Returns a new array with the elements from \`array\` sorted according to \`comparer\`.`,
    examples: [
      `(sort (fn [a b] (cond (< a b) -1 (> a b) 1 true -1)) [3 1 2])`,
      `(sort (fn [a b] (cond (> a b) -1 (< a b) 1 true -1)) [3 1 2])`,
    ],
    specialExpression: false,
  },
  join: {
    name: `join`,
    category: `Sequence`,
    linkName: `join`,
    returns: {
      type: `string`,
    },
    arguments: [
      {
        name: `array`,
        type: `array of strings`,
      },
      {
        name: `delimiter`,
        type: `string`,
      },
    ],
    description: `Returns a new string by concatenating all of the elements in \`array\`, separated by \`delimiter\`.`,
    examples: [`(join ["Albert" "Mojir"] " ")`, `(join (map number-to-string [0 1 2 3 4 5 6 7 8 9]) ", ")`],
    specialExpression: false,
  },
  'random-sample': {
    name: `random-sample`,
    category: `Sequence`,
    linkName: `random-sample`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `prob`,
        type: `number`,
        description: `between 0 and 1`,
      },
      {
        name: `array`,
        type: `array`,
      },
    ],
    description: `Returns an array. Each element from \`array\` has the probability \`prob\` to be included in the result.`,
    examples: [`(random-sample 0.5 [1 2 3 4 5 6 7 8 9 10])`],
    specialExpression: false,
  },
  shuffle: {
    name: `shuffle`,
    category: `Sequence`,
    linkName: `shuffle`,
    returns: {
      type: `Seq`,
    },
    arguments: [
      {
        input: `input`,
        type: `Seq`,
      },
    ],
    description: `Returns a shuffled copy of \`input\`.`,
    examples: [
      `(shuffle [1 2 3 4 5 6 7 8 9 10])`,
      `(shuffle "Albert Mojir")`,
      `(shuffle [1 2])`,
      `(shuffle [1])`,
      `(shuffle [])`,
    ],
    specialExpression: false,
  },
}
