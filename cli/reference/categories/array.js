module.exports = {
  array: {
    name: `array`,
    category: `Array`,
    linkName: `array`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `values`,
        type: `array`,
      },
    ],
    shortDescription: `Makes new array from \`values\`.`,
    longDescription: `Makes new array from \`values\`.`,
    examples: [
      `(array 1 2 3)`,
      `(array (array null undefined false true))`,
      `[]`,
      `[1 2 3]`,
      `[[null undefined false true]]`,
      `[]`,
      `([1 2 3] 1)`,
      `([1 2 3 4 5 6 7 8 9] 3)`,
    ],
    specialExpression: false,
  },
  range: {
    name: `range`,
    category: `Array`,
    linkName: `range`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `a`,
        type: `number`,
      },
      {
        name: `length`,
        type: `number`,
        description: `optional`,
      },
      {
        name: `length`,
        type: `number`,
        description: `optional`,
      },
    ],
    shortDescription: `Create an array with a range of numbers.`,
    longDescription: `Create an array with a range of numbers. If only one argument: \`0...a\`, otherwise: \`a...b\`. \`step\` defaults to \`1\`.`,
    examples: [`(range 4)`, `(range 1 4)`, `(range 0.4 4.9)`, `(range 0.25 1 0.25)`],
    specialExpression: false,
  },
  nth: {
    name: `nth`,
    category: `Array`,
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
    shortDescription: `Accesses element \`index\` of \`input\`. Negative \`index\` counts backwards.`,
    longDescription: `Accesses element \`index\` of \`input\`. Negative \`index\` counts backwards. Accessing out-of-bounds indices returns \`undefined\`.`,
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
    category: `Array`,
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
    shortDescription: `Pushes \`values\` to the end of \`array\`.`,
    longDescription: `Pushes \`values\` to the end of \`array\`.`,
    examples: [`(push [1 2 3] 4)`, `(push [1 2 3] 4 5 6)`, `(def l [1 2 3]) (push l 4) l`],
    specialExpression: false,
  },
  pop: {
    name: `pop`,
    category: `Array`,
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
    shortDescription: `Removes and returns the last item of \`array\`.`,
    longDescription: `Removes and returns then last item of \`array\`. If \`array\` is empty, \`undefined\` is returned.`,
    examples: [`(pop [1 2 3])`, `(pop [])`],
    specialExpression: false,
  },
  unshift: {
    name: `unshift`,
    category: `Array`,
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
    shortDescription: `Inserts \`values\` at the beginning of \`array\`.`,
    longDescription: `Inserts \`values\` at the beginning of \`array\`.`,
    examples: [`(unshift [1 2 3] 4)`, `(unshift [1 2 3] 4 5 6)`, `(def l [1 2 3]) (unshift l 4) l`],
    specialExpression: false,
  },
  shift: {
    name: `shift`,
    category: `Array`,
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
    shortDescription: `Removes and returns the first item of \`array\`.`,
    longDescription: `Removes and returns the first item of \`array\`. If \`array\` is empty, \`undefined\` is returned.`,
    examples: [`(shift [1 2 3])`, `(shift [])`],
    specialExpression: false,
  },
  slice: {
    name: `slice`,
    category: `Array`,
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
    shortDescription: `Returns a shallow copy of a portion of \`array\` into a new array selected from index \`start\` (inclusive) to index \`end\` (exclusive).`,
    longDescription: `Returns a shallow copy of a portion of \`array\` into a new array selected from index \`start\` (inclusive) to index \`end\` (exclusive). If \`start\` is not provided it defaults to \`0\`. If \`end\` is not provided, the rest of the array will be copied.`,
    examples: [`(slice [1 2 3 4 5] 2 4)`, `(slice [1 2 3 4 5] 2)`],
    specialExpression: false,
  },
  reduce: {
    name: `reduce`,
    category: `Array`,
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
    shortDescription: `Runs \`reducer\` function on each element of the \`array\`, passing in the return value from the calculation on the preceding element.`,
    longDescription: `Runs \`reducer\` function on each element of the \`array\`, passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the array is a single value.`,
    examples: [
      `(reduce + [1 2 3] 0)`,
      `(reduce + [] 0)`,
      `(reduce (fn [result value] (+ result (if (even? value) value 0))) [1 2 3 4 5 6 7 8 9] 0)`,
    ],
    specialExpression: false,
  },
  'reduce-right': {
    name: `reduce-right`,
    category: `Array`,
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
    shortDescription: `Runs \`reducer\` function on each element of the \`array\` (starting from the last item), passing in the return value from the calculation on the preceding element.`,
    longDescription: `Runs \`reducer\` function on each element of the \`array\` (starting from the last item), passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the array is a single value.`,
    examples: [`(reduce-right str ["A" "B" "C"] "")`],
    specialExpression: false,
  },
  map: {
    name: `map`,
    category: `Array`,
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
    shortDescription: `Creates a new array populated with the results of calling \`mapper\` on every elements in the calling \`array\`s.`,
    longDescription: `Creates a new array populated with the results of calling \`mapper\` on every elements in the calling \`array\`s.`,
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
    category: `Array`,
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
    shortDescription: `Creates a new array with all elements that pass the test implemented by \`filter\`.`,
    longDescription: `Creates a new array with all elements that pass the test implemented by \`filter\`.`,
    examples: [`(filter string? ["Albert" "Mojir" 160 [1 2]])`, `(filter (fn [x] (> x 10)) [5 10 15 20])`],
    specialExpression: false,
  },
  position: {
    name: `position`,
    category: `Array`,
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
    shortDescription: `Returns the index of the first elements that pass the test implemented by \`finder\`.`,
    longDescription: `Returns the index of the first elements that pass the test implemented by \`finder\`. If no element was found, \`undefined\` is returned.`,
    examples: [
      `(position string? ["Albert" "Mojir" 160 [1 2]])`,
      `(position (fn [x] (> x 10)) [5 10 15 20])`,
      `(position (fn [x] (> x 100)) [5 10 15 20])`,
    ],
    specialExpression: false,
  },
  'index-of': {
    name: `index-of`,
    category: `Array`,
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
    shortDescription: `Returns the index of \`value\` in array.`,
    longDescription: `Returns the index of \`value\` in array. If element is not present in \`array\` \`undefined\` is returned.`,
    examples: [
      `(index-of "Mojir" ["Albert" "Mojir" 160 [1 2]])`,
      `(index-of 15 [5 10 15 20])`,
      `(index-of 1 [5 10 15 20])`,
    ],
    specialExpression: false,
  },
  some: {
    name: `some`,
    category: `Array`,
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
    shortDescription: `Returns the first elements that pass the test implemented by \`finder\`.`,
    longDescription: `Returns the first elements that pass the test implemented by \`finder\`. I no element was found, \`undefined\` is returned.`,
    examples: [
      `(some string? ["Albert" "Mojir" 160 [1 2]])`,
      `(some (fn [x] (> x 10)) [5 10 15 20])`,
      `(some (fn [x] (> x 10)) [1 2 3 4])`,
      `(some (fn [x] (> x 10)) [])`,
    ],
    specialExpression: false,
  },
  'every?': {
    name: `every?`,
    category: `Array`,
    linkName: `every_question`,
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
    shortDescription: `Returns true if all elements pass the test implemented by \`finder\`, otherwise returns false.`,
    longDescription: `Returns true if all elements pass the test implemented by \`finder\`, otherwise returns false.`,
    examples: [
      `(every? string? ["Albert" "Mojir" 160 [1 2]])`,
      `(every? (fn [x] (> x 10)) [50 100 150 200])`,
      `(every? (fn [x] (> x 10)) [])`,
    ],
    specialExpression: false,
  },
  reverse: {
    name: `reverse`,
    category: `Array`,
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
    shortDescription: `If \`input\` is an array, creates a new array with the elements from \`input\` in reversed order. If \`input\` is a string, returns new reversed string.`,
    longDescription: `If \`input\` is an array, creates a new array with the elements from \`input\` in reversed order. If \`input\` is a string, returns new reversed string.`,
    examples: [`(reverse ["Albert" "Mojir" 160 [1 2]])`, `(reverse [])`, `(reverse "Albert")`],
    specialExpression: false,
  },
  first: {
    name: `first`,
    category: `Array`,
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
    shortDescription: `Returns the first element of \`array\`.`,
    longDescription: `Returns the first element of \`array\`. If \`array\` is empty, \`undefined\` is returned.`,
    examples: [`(first ["Albert" "Mojir" 160 [1 2]])`, `(first [])`],
    specialExpression: false,
  },
  second: {
    name: `second`,
    category: `Array`,
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
    shortDescription: `Returns the second element of \`array\`.`,
    longDescription: `Returns the second element of \`array\`. If \`array\` has less than two elements, \`undefined\` is returned.`,
    examples: [`(second ["Albert" "Mojir" 160 [1 2]])`, `(second [1])`, `(second [])`],
    specialExpression: false,
  },
  last: {
    name: `last`,
    category: `Array`,
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
    shortDescription: `Returns the last element of \`array\`.`,
    longDescription: `Returns the last element of \`array\`. If \`array\` is empty, \`undefined\` is returned.`,
    examples: [`(last ["Albert" "Mojir" 160 [1 2]])`, `(last [1 2])`, `(last [1])`, `(last [])`],
    specialExpression: false,
  },
  rest: {
    name: `rest`,
    category: `Array`,
    linkName: `rest`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
    ],
    shortDescription: `Returns a new array with all but the first element from \`array\`.`,
    longDescription: `Returns a new array with all but the first element from \`array\`. If \`array\` has less than two elements, undefined is returned.`,
    examples: [`(rest ["Albert" "Mojir" 160 [1 2]])`, `(rest ["Albert"])`, `(rest [])`],
    specialExpression: false,
  },
  cons: {
    name: `cons`,
    category: `Array`,
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
    shortDescription: `Constructs a new array with \`element\` as first element and \`rest\` as the rest.`,
    longDescription: `Constructs a new array with \`element\` as first element and \`rest\` as the rest.`,
    examples: [`(cons "Hi" ["Albert" "Mojir" 160 [1 2]])`, `(cons "Hi" [])`],
    specialExpression: false,
  },
  take: {
    name: `take`,
    category: `Array`,
    linkName: `take`,
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
    shortDescription: `Constructs a new array with the \`count\` first elements from \`array\`.`,
    longDescription: `Constructs a new array with the \`count\` first elements from \`array\`.`,
    examples: [`(take [1 2 3 4 5] 3)`, `(take [1 2 3 4 5] 0)`],
    specialExpression: false,
  },

  'take-last': {
    name: `take-last`,
    category: `Array`,
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
    shortDescription: `Constructs a new array with the \`count\` last elements from \`array\`.`,
    longDescription: `Constructs a new array with the \`count\` last elements from \`array\`.`,
    examples: [`(take-last [1 2 3 4 5] 3)`, `(take-last [1 2 3 4 5] 0)`],
    specialExpression: false,
  },

  'take-while': {
    name: `take-while`,
    category: `Array`,
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
    shortDescription: `Returns the members of \`array\` in order, stopping before the first one for which \`predicate\` returns a falsy value.`,
    longDescription: `Returns the members of \`array\` in order, stopping before the first one for which \`predicate\` returns a falsy value.`,
    examples: [`(take-while (fn [x] (< x 3)) [1 2 3 2 1])`, `(take-while (fn [x] (> x 3)) [1 2 3 2 1])`],
    specialExpression: false,
  },

  sort: {
    name: `sort`,
    category: `Array`,
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
    shortDescription: `Returns a new array with the elements from \`array\` sorted according to \`comparer\`.`,
    longDescription: `Returns a new array with the elements from \`array\` sorted according to \`comparer\`.`,
    examples: [
      `(sort (fn [a b] (cond ((< a b) -1) ((> a b) 1) (true -1))) [3 1 2])`,
      `(sort (fn [a b] (cond ((> a b) -1) ((< a b) 1) (true -1))) [3 1 2])`,
    ],
    specialExpression: false,
  },
  join: {
    name: `join`,
    category: `Array`,
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
    shortDescription: `Returns a new string by concatenating all of the elements in \`array\`, separated by \`delimiter\`.`,
    longDescription: `Returns a new string by concatenating all of the elements in \`array\`, separated by \`delimiter\`.`,
    examples: [`(join ["Albert" "Mojir"] " ")`, `(join (map number-to-string [0 1 2 3 4 5 6 7 8 9]) ", ")`],
    specialExpression: false,
  },
  'random-sample': {
    name: `random-sample`,
    category: `Array`,
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
    shortDescription: `Returns an array. Each element from \`array\` has the probability \`prob\` to be included in the result.`,
    longDescription: `Returns an array. Each element from \`array\` has the probability \`prob\` to be included in the result.`,
    examples: [`(random-sample 0.5 [1 2 3 4 5 6 7 8 9 10])`],
    specialExpression: false,
  },
  repeat: {
    name: `repeat`,
    category: `Array`,
    linkName: `repeat`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `count`,
        type: `non negative integer`,
      },
      {
        name: `value`,
        type: `any`,
      },
    ],
    shortDescription: `Returns an array with \`value\` repeated \`count\` times.`,
    longDescription: `Returns an array with \`value\` repeated \`count\` times.`,
    examples: [`(repeat 3 10)`, `(repeat 0 10)`, `(repeat 5 "Albert")`],
    specialExpression: false,
  },
}
