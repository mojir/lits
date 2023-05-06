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
        type: `string | array | nil`,
      },
      {
        name: `index`,
        type: `integer`,
      },
      {
        name: `notFound`,
        type: `any`,
        description: `optional`,
      },
    ],
    description: `Accesses element \`index\` of \`input\`. Accessing out-of-bounds indices returns \`notFound\` or \`nil\`.`,
    examples: [
      `(nth [1 2 3] 1)`,
      `(nth [1 2 3] 3)`,
      `(nth [1 2 3] -1)`,
      `(nth [1 2 3] 3 99)`,
      `(nth "A string" 1)`,
      `(nth "A string" 3)`,
      `(nth "A string" -3)`,
      `(nth "A string" 30 :X)`,
      `(nth nil 1)`,
      `(nth nil 1 "Default value")`,
    ],
  },
  push: {
    name: `push`,
    category: `Sequence`,
    linkName: `push`,
    clojureDocs: null,
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
    description: `Removes and returns then last item of \`array\`. If \`array\` is empty, \`nil\` is returned.`,
    examples: [`(pop [1 2 3])`, `(pop [])`],
  },
  unshift: {
    name: `unshift`,
    category: `Sequence`,
    linkName: `unshift`,
    clojureDocs: null,
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
  },
  shift: {
    name: `shift`,
    category: `Sequence`,
    linkName: `shift`,
    clojureDocs: null,
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
    description: `Removes and returns the first item of \`array\`. If \`array\` is empty, \`nil\` is returned.`,
    examples: [`(shift [1 2 3])`, `(shift [])`],
  },
  slice: {
    name: `slice`,
    category: `Sequence`,
    linkName: `slice`,
    clojureDocs: null,
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
  },
  reductions: {
    name: `reductions`,
    category: `Sequence`,
    linkName: `reductions`,
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
    description: `Returns an array of the intermediate values of the reduction (see \`reduce\`) of \`seq\` by \`reducer\`.`,
    examples: [
      `(reductions + [1 2 3])`,
      `(reductions + 0 [1 2 3])`,
      `(reductions + 0 [])`,
      `(reductions (fn [result value] (+ result (if (even? value) value 0))) 0 [1 2 3 4 5 6 7 8 9])`,
    ],
  },
  reduce: {
    name: `reduce`,
    category: `Sequence`,
    linkName: `reduce`,
    returns: {
      type: `sequence`,
    },
    arguments: [
      {
        name: `reducer`,
        type: `function`,
      },
      {
        name: `startValue`,
        type: `any`,
        description: `optional`,
      },
      {
        name: `seq`,
        type: `Seq`,
      },
    ],
    description: `Runs \`reducer\` function on each element of the \`seq\`, passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the \`seq\` is a single value.`,
    examples: [
      `(reduce + [1 2 3])`,
      `(reduce + 0 [1 2 3])`,
      `(reduce + 0 [])`,
      `(reduce (fn [result value] (+ result (if (even? value) value 0))) 0 [1 2 3 4 5 6 7 8 9])`,
    ],
  },
  'reduce-right': {
    name: `reduce-right`,
    category: `Sequence`,
    linkName: `reduce-right`,
    clojureDocs: null,
    returns: {
      type: `sequence`,
    },
    arguments: [
      {
        name: `reducer`,
        type: `function`,
      },
      {
        name: `startValue`,
        type: `any`,
      },
      {
        name: `seq`,
        type: `Seq`,
      },
    ],
    description: `Runs \`reducer\` function on each element of the \`seq\` (starting from the last item), passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the \`seq\` is a single value.`,
    examples: [`(reduce-right str [:A :B :C] "")`],
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
        name: `sequence`,
        type: `Seq`,
        description: `one or many`,
      },
    ],
    description: `Creates a new array populated with the results of calling \`mapper\` on every elements in the calling \`sequence\`s.`,
    examples: [
      `(map str ["Albert" "Mojir" 42])`,
      `(map str [])`,
      `(map + [1 2 3] [1 2 3])`,
      `(map max [2 6 3] [2 4 7] [1 6 2])`,
    ],
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
  },
  position: {
    name: `position`,
    category: `Sequence`,
    linkName: `position`,
    clojureDocs: null,
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
    description: `Returns the index of the first elements that pass the test implemented by \`finder\`. If no element was found, \`nil\` is returned.`,
    examples: [
      `(position string? ["Albert" "Mojir" 160 [1 2]])`,
      `(position (fn [x] (> x 10)) [5 10 15 20])`,
      `(position (fn [x] (> x 100)) [5 10 15 20])`,
    ],
  },
  'index-of': {
    name: `index-of`,
    category: `Sequence`,
    linkName: `index-of`,
    clojureDocs: null,
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
    description: `Returns the index of \`value\` in array. If element is not present in \`array\` \`nil\` is returned.`,
    examples: [
      `(index-of ["Albert" "Mojir" 160 [1 2]] "Mojir")`,
      `(index-of [5 10 15 20] 15)`,
      `(index-of [5 10 15 20] 1)`,
    ],
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
    description: `Returns the first elements that pass the test implemented by \`finder\`. I no element was found, \`nil\` is returned.`,
    examples: [
      `(some string? ["Albert" "Mojir" 160 [1 2]])`,
      `(some (fn [x] (> x 10)) [5 10 15 20])`,
      `(some (fn [x] (> x 10)) [1 2 3 4])`,
      `(some (fn [x] (> x 10)) [])`,
    ],
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
    description: `Returns the first element of \`array\`. If \`array\` is empty, \`nil\` is returned.`,
    examples: [`(first ["Albert" "Mojir" 160 [1 2]])`, `(first [])`],
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
    description: `Returns the second element of \`array\`. If \`array\` has less than two elements, \`nil\` is returned.`,
    examples: [`(second ["Albert" "Mojir" 160 [1 2]])`, `(second [1])`, `(second [])`],
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
    description: `Returns the last element of \`array\`. If \`array\` is empty, \`nil\` is returned.`,
    examples: [`(last ["Albert" "Mojir" 160 [1 2]])`, `(last [1 2])`, `(last [1])`, `(last [])`],
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
      `(rest :A)`,
      `(rest "")`,
    ],
  },
  nthrest: {
    name: `nthrest`,
    category: `Sequence`,
    linkName: `nthrest`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
    ],
    description: `If \`input\` is an array, returns a new array with all but the first \`count\` elements from \`input\`. For string \`input\` returns all but the first \`count\` characters in \`input\`.`,
    examples: [
      `(nthrest ["Albert" "Mojir" 160 [1 2]] 2)`,
      `(nthrest "Albert" 3)`,
      `(nthrest "Albert" 6)`,
      `(nthrest [] 0)`,
      `(nthrest "" 0)`,
    ],
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
    description: `If \`input\` is an array, returns a new array with all but the first element from \`input\`. If \`input\` has less than two elements, \`nil\` is returned. For string \`input\` returns all but the first characters in \`input\`. If length of string \`input\` is less than two, \`nil\` is returned.`,
    examples: [
      `(next ["Albert" "Mojir" 160 [1 2]])`,
      `(next ["Albert"])`,
      `(next [])`,
      `(next "Albert")`,
      `(next :A)`,
      `(next "")`,
    ],
  },
  nthnext: {
    name: `nthnext`,
    category: `Sequence`,
    linkName: `nthnext`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `array`,
        type: `array`,
      },
    ],
    description: `If \`input\` is an array, returns a new array with all but the first \`count\` elements from \`input\`. If \`input\` has less or equal than \`count\` elements, \`nil\` returned. For string \`input\` returns all but the first \`count\` characters in \`input\`. If length of string \`input\` is less or equal than \`count\`, \`nil\` is returned.`,
    examples: [
      `(nthnext ["Albert" "Mojir" 160 [1 2]] 2)`,
      `(nthnext "Albert" 3)`,
      `(nthnext "Albert" 6)`,
      `(nthnext [] 0)`,
      `(nthnext "" 0)`,
    ],
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
  },

  drop: {
    name: `drop`,
    category: `Sequence`,
    linkName: `drop`,
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
    description: `Constructs a new array/string with the \`count\` first elements dropped from \`input\`.`,
    examples: [`(drop 3 [1 2 3 4 5])`, `(drop 0 [1 2 3 4 5])`, `(drop 2 "Albert")`, `(drop 50 "Albert")`],
  },

  'drop-last': {
    name: `drop-last`,
    category: `Sequence`,
    linkName: `drop-last`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `count`,
        type: `integer`,
      },
      {
        name: `array`,
        type: `array`,
      },
    ],
    description: `Constructs a new array with the \`count\` last elements dropped from \`array\`.`,
    examples: [`(drop-last 3 [1 2 3 4 5])`, `(drop-last 0 [1 2 3 4 5])`],
  },

  'drop-while': {
    name: `drop-while`,
    category: `Sequence`,
    linkName: `drop-while`,
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
    description: `Returns the members of \`array\` in order, skipping the fist elements for witch the \`predicate\` returns a truethy value.`,
    examples: [`(drop-while (fn [x] (< x 3)) [1 2 3 2 1])`, `(drop-while (fn [x] (> x 3)) [1 2 3 2 1])`],
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
        description: `optional`,
      },
      {
        name: `seq`,
        type: `Seq`,
      },
    ],
    description: `Returns a new Seq with the elements from \`seq\` sorted according to \`comparer\`. If no \`comparer\` is supplied, builtin \`compare\` will be used.`,
    examples: [
      `(sort [3 1 2])`,
      `(sort (fn [a b] (cond (< a b) -1 (> a b) 1 true -1)) [3 1 2])`,
      `(sort (fn [a b] (cond (> a b) -1 (< a b) 1 true -1)) [3 1 2])`,
    ],
  },
  'sort-by': {
    name: `sort-by`,
    category: `Sequence`,
    linkName: `sort-by`,
    returns: {
      type: `array`,
    },
    arguments: [
      {
        name: `keyfn`,
        type: `function`,
      },
      {
        name: `comparer`,
        type: `function`,
        description: `optional`,
      },
      {
        name: `seq`,
        type: `Seq`,
      },
    ],
    description: `Returns a sorted sequence of the items in \`seq\`, where the sort order is determined by comparing \`(keyfn item)\`. If no \`comparer\` is supplied, uses builtin \`compare\`.`,
    examples: [`(sort-by count ["Albert" "Mojir" "Nina"])`, `(sort-by lower-case "Albert")`],
  },
  join: {
    name: `join`,
    category: `Sequence`,
    linkName: `join`,
    clojureDocs: null,
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
  },
  'random-sample!': {
    name: `random-sample!`,
    category: `Sequence`,
    linkName: `random-sample_exclamation`,
    clojureDocs: `random-sample`,
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
    examples: [`(random-sample! 0.5 [1 2 3 4 5 6 7 8 9 10])`, `(random-sample! 0.5 "Albert")`],
  },
  'rand-nth!': {
    name: `rand-nth!`,
    category: `Sequence`,
    linkName: `rand-nth_exclamation`,
    clojureDocs: `rand-nth`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `seq`,
        type: `seq`,
      },
    ],
    description: `Returns a random element from \`seq\`. Returns \`nil\` if \`seq\` is empty.`,
    examples: [`(rand-nth! [1 2 3 4 5 6 7 8 9 10])`, `(rand-nth! "Albert")`, `(rand-nth! [])`],
  },
  'shuffle!': {
    name: `shuffle!`,
    category: `Sequence`,
    linkName: `shuffle_exclamation`,
    clojureDocs: `shuffle`,
    returns: {
      type: `Seq`,
    },
    arguments: [
      {
        name: `input`,
        type: `Seq`,
      },
    ],
    description: `Returns a shuffled copy of \`input\`.`,
    examples: [
      `(shuffle! [1 2 3 4 5 6 7 8 9 10])`,
      `(shuffle! "Albert Mojir")`,
      `(shuffle! [1 2])`,
      `(shuffle! [1])`,
      `(shuffle! [])`,
    ],
  },
  distinct: {
    name: `distinct`,
    category: `Sequence`,
    linkName: `distinct`,
    returns: {
      type: `Seq`,
    },
    arguments: [
      {
        name: `input`,
        type: `Seq`,
      },
    ],
    description: `Returns a copy of \`input\` with no duplicates.`,
    examples: [`(distinct [1 2 3 1 3 5])`, `(distinct "Albert Mojir")`, `(distinct [])`, `(distinct "")`],
  },

  remove: {
    name: `remove`,
    category: `Sequence`,
    linkName: `remove`,
    returns: {
      type: `Seq`,
    },
    arguments: [
      {
        name: `pred`,
        type: `Function`,
      },
      {
        name: `input`,
        type: `Seq`,
      },
    ],
    description: `Returns a new sequence of items in \`input\` for witch \`(pred item)\` returns a falsy value.`,
    examples: [`(remove even? [1 2 3 1 3 5])`, `(remove #(has? "aoueiyAOUEIY" %1) "Albert Mojir")`],
  },

  'remove-at': {
    name: `remove-at`,
    category: `Sequence`,
    linkName: `remove-at`,
    clojureDocs: null,
    returns: {
      type: `Seq`,
    },
    arguments: [
      {
        name: `index`,
        type: `number`,
      },
      {
        name: `input`,
        type: `Seq`,
      },
    ],
    description: `Returns a new sequence of all items in \`input\` except item at \`index\`.`,
    examples: [`(remove-at 0 [1 2 3 1 3 5])`, `(remove-at -1 [1 2 3 1 3 5])`, `(remove-at 6 "Albert Mojir")`],
  },

  'split-at': {
    name: `split-at`,
    category: `Sequence`,
    linkName: `split-at`,
    returns: {
      type: `Seq`,
    },
    arguments: [
      {
        name: `pos`,
        type: `number`,
      },
      {
        name: `input`,
        type: `Seq`,
      },
    ],
    description: `Returns a new array/string of \`[(take pos input) (drop pos input)]\`.`,
    examples: [`(split-at 2 [1 2 3 4 5])`, `(split-at 2 "Albert")`],
  },

  'split-with': {
    name: `split-with`,
    category: `Sequence`,
    linkName: `split-with`,
    returns: {
      type: `Seq`,
    },
    arguments: [
      {
        name: `pos`,
        type: `number`,
      },
      {
        name: `input`,
        type: `Seq`,
      },
    ],
    description: `Returns a new array/string of \`[(take-while pos input) (drop-while pos input)]\`.`,
    examples: [`(split-with #(> %1 3) [1 2 3 4 5])`, `(split-with #(<= %1 :Z) "Albert")`],
  },

  frequencies: {
    name: `frequencies`,
    category: `Sequence`,
    linkName: `frequencies`,
    returns: {
      type: `Obj`,
    },
    arguments: [
      {
        name: `input`,
        type: `Seq`,
      },
    ],
    description: `Returns an object from distinct items in \`seq\` to the number of times they appear. Note that all items in \`seq\` must be valid object keys i.e. strings.`,
    examples: [
      `(frequencies ["Albert" "Mojir" "Nina" "Mojir"])`,
      `(frequencies "Pneumonoultramicroscopicsilicovolcanoconiosis")`,
    ],
  },

  'group-by': {
    name: `group-by`,
    category: `Sequence`,
    linkName: `group-by`,
    returns: {
      type: `Obj`,
    },
    arguments: [
      {
        name: `fn`,
        type: `Function`,
      },
      {
        name: `input`,
        type: `Seq`,
      },
    ],
    description: `Returns an object of the elements of \`seq\` keyed by the result of \`fn\` on each element. The value at each key will be an array of the corresponding elements.`,
    examples: [
      `(group-by "name" [{"name" "Albert"} {"name" "Albert"} {"name" "Mojir"}])`,
      `(group-by (fn [char] (if (has? "aoueiAOUEI" char) "vowel" "other")) "Albert Mojir")`,
    ],
  },

  partition: {
    name: `partition`,
    category: `Sequence`,
    linkName: `partition`,
    returns: {
      type: `Seq`,
    },
    arguments: [
      {
        name: `n`,
        type: `number`,
      },
      {
        name: `step`,
        type: `number`,
        description: `optional`,
      },
      {
        name: `pad`,
        type: `Arr`,
        description: `optional`,
      },
      {
        name: `seq`,
        type: `Seq`,
      },
    ],
    description: `Returns an array of sequences of \`n\` items each, at offsets \`step\` apart. If \`step\` is not supplied, defaults to \`n\`. If a \`pad\` array is supplied, use its elements as necessary to complete last partition upto \`n\` items. In case there are not enough padding elements, return a partition with less than \`n\` items.`,
    examples: [
      `(partition 4 (range 20))`,
      `(partition 4 (range 22))`,
      `(partition 4 6 (range 20))`,
      `(partition 4 3 (range 20))`,
      `(partition 3 6 [:a] (range 20))`,
      `(partition 4 6 [:a] (range 20))`,
      `(partition 4 6 [:a :b :c :d] (range 20))`,
      `(partition 3 1 [:a :b :c :d :e :f])`,
      `(partition 10 [1 2 3 4])`,
      `(partition 10 10 [1 2 3 4])`,
      `(partition 10 10 [] [1 2 3 4])`,
      `(partition 10 10 nil [1 2 3 4])`,
      `(partition 5 "superfragilistic")`,
      `(partition 5 5 nil "superfragilistic")`,
      `(def foo [5 6 7 8]) (partition 2 1 foo foo)`,
    ],
  },

  'partition-all': {
    name: `partition-all`,
    category: `Sequence`,
    linkName: `partition-all`,
    returns: {
      type: `Seq`,
    },
    arguments: [
      {
        name: `n`,
        type: `number`,
      },
      {
        name: `step`,
        type: `number`,
        description: `optional`,
      },
      {
        name: `seq`,
        type: `Seq`,
      },
    ],
    description: `Returns an array of sequences like partition, but may include partitions with fewer than n items at the end.`,
    examples: [
      `(partition-all 4 [0 1 2 3 4 5 6 7 8 9])`,
      `(partition 4 [0 1 2 3 4 5 6 7 8 9])`,
      `(partition-all 2 4 [0 1 2 3 4 5 6 7 8 9])`,
    ],
  },

  'partition-by': {
    name: `partition-by`,
    category: `Sequence`,
    linkName: `partition-by`,
    returns: {
      type: `Seq`,
    },
    arguments: [
      {
        name: `fn`,
        type: `function`,
      },
      {
        name: `seq`,
        type: `Seq`,
      },
    ],
    description: `Applies \`fn\` to each value in \`seq\`, splitting it each time \`fn\` returns a new value. Returns an array of sequences.`,
    examples: [
      `(partition-by #(= 3 %1) [1 2 3 4 5])`,
      `(partition-by odd? [1 1 1 2 2 3 3])`,
      `(partition-by identity "Leeeeeerrroyyy")`,
    ],
  },
}
