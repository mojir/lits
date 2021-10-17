module.exports = {
  apply: {
    name: `apply`,
    category: `Functional`,
    linkName: `apply`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `fn`,
        type: `function`,
      },
      {
        name: `args`,
        type: `array`,
      },
    ],
    description: `Call supplied function with specified arguments.`,
    examples: [`(apply + [1 2 3])`, `(apply (fn [x y] (sqrt (+ (* x x) (* y y)))) [3 4])`],
    specialExpression: false,
  },
  identity: {
    name: `identity`,
    category: `Functional`,
    linkName: `identity`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns \`value\`.`,
    examples: [`(identity 1)`, `(identity "Albert")`, `(identity {"a" 1})`, `(identity null)`],
    specialExpression: false,
  },
  partial: {
    name: `partial`,
    category: `Functional`,
    linkName: `partial`,
    returns: {
      type: `function`,
    },
    arguments: [
      {
        name: `fn`,
        type: `function`,
      },
      {
        name: `args`,
        type: `any`,
        description: `zero or more`,
      },
    ],
    description: `Takes a function \`fn\` and fewer (or equal) than the normal arguments to \`fn\`, and returns a function that takes a variable number of additional args. When called, the returned function calls f with \`args\` + additional args.`,
    examples: [`(partial + 100)`, `(def addHundred (partial + 100)) (addHundred 10)`],
    specialExpression: false,
  },
  comp: {
    name: `comp`,
    category: `Functional`,
    linkName: `comp`,
    returns: {
      type: `function`,
    },
    arguments: [
      {
        name: `fn`,
        type: `function`,
        description: `zero or more`,
      },
      {
        name: `fns`,
        type: `function[]`,
        description: `optional`,
      },
    ],
    description: `Takes a set of functions and returns a fn that is the composition of those. The returned functions takes a variable number of arguments, applies the rightmost function to the args, the next function (right-to-left) to the result, etc.`,
    examples: [
      `(def negative-quotient (comp - /)) (negative-quotient 9 3)`,
      `(#((apply comp first (repeat %2 rest)) %1) [1 2 3 4 5 6 7] 3)`,
      `(def x {"bar" {"foo" 42}}) ((comp "foo" "bar") x)`,
    ],
    specialExpression: false,
  },
  constantly: {
    name: `constantly`,
    category: `Functional`,
    linkName: `constantly`,
    returns: {
      type: `function`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    description: `Returns a function that takes any number of arguments and returns \`value\`.`,
    examples: [
      `(def negative-quotient (constantly - /)) (negative-quotient 9 3)`,
      `(#((apply constantly first (repeat %2 rest)) %1) [1 2 3 4 5 6 7] 3)`,
      `(def x {"bar" {"foo" 42}}) ((constantly "foo" "bar") x)`,
    ],
    specialExpression: false,
  },
  juxt: {
    name: `juxt`,
    category: `Functional`,
    linkName: `juxt`,
    returns: {
      type: `Function`,
    },
    arguments: [
      {
        name: `functions`,
        type: `Function`,
        description: `one or more`,
      },
    ],
    description: `Takes variable number of \`functions\` and returns a function that is the juxtaposition of those \`functions\`.  The returned function takes a variable number of args, and returns a vector containing the result of applying each \`function\` to the args (left-to-right).`,
    examples: [
      `((juxt + * min max) 3 4 6)`,
      `((juxt "a" "b") {"a" 1, "b" 2, "c" 3, "d" 4})`,
      `(apply (juxt + * min max) (range 1 11))`,
    ],
    specialExpression: false,
  },
  complement: {
    name: `complement`,
    category: `Functional`,
    linkName: `complement`,
    returns: {
      type: `Function`,
    },
    arguments: [
      {
        name: `function`,
        type: `Function`,
      },
    ],
    description: `Takes a \`function\` and returns a new function that takes the same arguments as f, has the same effects, if any, and returns the opposite truth value.`,
    examples: [`((complement >) 1 3)`, `((complement <) 1 3)`, `((complement +) 1 3)`, `((complement +) 0 0)`],
    specialExpression: false,
  },
  'every-pred': {
    name: `every-pred`,
    category: `Functional`,
    linkName: `every-pred`,
    returns: {
      type: `Function`,
    },
    arguments: [
      {
        name: `predicates`,
        type: `Function`,
        description: `one or more`,
      },
    ],
    description: `Takes a number of \`predicates\` and returns a function that returns true if all of the \`predicates\` return a truthy true value against all of its arguments, else it returns false.`,
    examples: [
      `((every-pred string? #(> (count %1) 3)) "Albert" "Mojir")`,
      `((every-pred string? #(> (count %1) 3)) "Albert" "M")`,
      `((every-pred string? #(> (count %1) 3)) "Albert" [1 2 3])`,
    ],
    specialExpression: false,
  },
  'some-pred': {
    name: `some-pred`,
    category: `Functional`,
    linkName: `some-pred`,
    returns: {
      type: `Function`,
    },
    arguments: [
      {
        name: `predicates`,
        type: `Function`,
        description: `one or more`,
      },
    ],
    description: `Takes a number of \`predicates\` and returns a function that returns true if at least one of the \`predicates\` return a truthy true value against at least one of its arguments, else it returns false.`,
    examples: [
      `((some-pred string? #(> (count %1) 3)) "Albert" "Mojir")`,
      `((some-pred string? #(> (count %1) 3)) "A" "M")`,
      `((some-pred string? #(> (count %1) 3)) "A" [1 2 3])`,
      `((some-pred string? #(> (count %1) 3)) [1 2 3] [2])`,
    ],
    specialExpression: false,
  },
}
