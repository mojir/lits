module.exports = {
  and: {
    name: `and`,
    category: `Special expression`,
    linkName: `and`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `forms`,
        type: `form[]`,
      },
    ],
    shortDescription: `Computes logical "and" function.`,
    longDescription: `Computes logical "and" function. \`forms\` evaluation starts from left. Value from the first form that decides result is returned so \`forms\` at end of argument array may not evaluated.`,
    examples: [
      `(and 1 1)`,
      `(and (> 3 2) "string")`,
      `(and (< 3 2) "string")`,
      `(and true true true true)`,
      `(and true true 0 true)`,
    ],
    specialExpression: true,
    sideEffects: [],
  },
  or: {
    name: `or`,
    category: `Special expression`,
    linkName: `or`,
    returns: {
      type: `boolean`,
    },
    arguments: [
      {
        name: `forms`,
        type: `form[]`,
      },
    ],
    shortDescription: `Computes logical "or" function.`,
    longDescription: `Computes logical "or" function. \`forms\` evaluation starts from left. Value from the first form that decides result is returned so forms at end of argument array may not evaluated.`,
    examples: [
      `(or 1 1)`,
      `(or (> 3 2) "string")`,
      `(or (< 3 2) "string")`,
      `(or true true true true)`,
      `(or 1 2 3 4)`,
    ],
    specialExpression: true,
    sideEffects: [],
  },
  def: {
    name: `def`,
    category: `Special expression`,
    linkName: `def`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `variable`,
        type: `name`,
      },
      {
        name: `value`,
        type: `any`,
      },
    ],
    shortDescription: `Bind \`value\` to \`variable\`. If \`variable\` isn't defined, a new global variable is created.`,
    longDescription: `Bind \`value\` to \`variable\`. If \`variable\` isn't defined, a new global variable is created.`,
    examples: [`(def a (object))`, `(def a (object "x" 10 "y" true "z" "A string"))`],
    specialExpression: true,
    sideEffects: [],
  },
  const: {
    name: `const`,
    category: `Special expression`,
    linkName: `const`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `variable`,
        type: `name`,
      },
      {
        name: `value`,
        type: `any`,
      },
    ],
    shortDescription: `Bind \`value\` to \`variable\` and makes it constant. Throws if \`variable\` exists.`,
    longDescription: `Bind \`value\` to \`variable\` and makes it constant. Throws if \`variable\` exists.`,
    examples: [`(const x 10)`, `(const x 10) (def x 20)`],
    specialExpression: true,
    sideEffects: [],
  },
  'def-local': {
    name: `def-local`,
    category: `Special expression`,
    linkName: `def-local`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `variable`,
        type: `name`,
      },
      {
        name: `value`,
        type: `any`,
      },
    ],
    shortDescription: `Bind \`value\` to local \`variable\`. If local \`variable\` isn't defined, a new local variable is created.`,
    longDescription: `Bind \`value\` to local \`variable\`. If local \`variable\` isn't defined, a new local variable is created.`,
    examples: [`(def-local x (object))`, `(def x 10) (progn (def-local x 20)) x`],
    specialExpression: true,
    sideEffects: [],
  },
  'def-local-constant': {
    name: `def-local-constant`,
    category: `Special expression`,
    linkName: `def-local-constant`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `variable`,
        type: `name`,
      },
      {
        name: `value`,
        type: `any`,
      },
    ],
    shortDescription: `Bind \`value\` to new local \`variable\` and makes it constant. Throws if local \`variable\` exists.`,
    longDescription: `Bind \`value\` to new local \`variable\` and makes it constant. Throws if local \`variable\` exists.`,
    examples: [`(def-local-constant x (object))`, `(def x 10) (progn (def-local-constant x 20)) x`],
    specialExpression: true,
    sideEffects: [],
  },
  'create-variable': {
    name: `create-variable`,
    category: `Special expression`,
    linkName: `create-variable`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `variable`,
        type: `form`,
      },
      {
        name: `value`,
        type: `any`,
      },
    ],
    shortDescription: `Creates a variable with name set to \`variable\` evaluated and value set to \`value\`.`,
    longDescription: `Creates a variable with name set to \`variable\` evaluated and value set to \`value\`. If a variable with name \`variable\` isn't found a new global variable is created.`,
    examples: [
      `(create-variable "a" "b")`,
      `(create-variable (concat "a" "1") (object "x" 10 "y" true "z" "A string")) a1`,
      `(create-variable "a" "b") (create-variable a "c") b`,
    ],
    specialExpression: true,
    sideEffects: [],
  },
  'create-local-variable': {
    name: `create-local-variable`,
    category: `Special expression`,
    linkName: `create-local-variable`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `variable`,
        type: `form`,
      },
      {
        name: `value`,
        type: `any`,
      },
    ],
    shortDescription: `Creates a local variable with name set to \`variable\` evaluated and value set to \`value\`.`,
    longDescription: `Creates a local variable with name set to \`variable\` evaluated and value set to \`value\`.`,
    examples: [
      `(create-local-variable "a" "b")`,
      `(create-local-variable (concat "a" "1") (object "x" 10 "y" true "z" "A string")) a1`,
      `(create-local-variable "a" "b") (create-local-variable a "c") b`,
    ],
    specialExpression: true,
    sideEffects: [],
  },
  'create-global-variable': {
    name: `create-global-variable`,
    category: `Special expression`,
    linkName: `create-global-variable`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `variable`,
        type: `form`,
      },
      {
        name: `value`,
        type: `any`,
      },
    ],
    shortDescription: `Creates a global variable with name set to \`variable\` evaluated and value set to \`value\`.`,
    longDescription: `Creates a global variable with name set to \`variable\` evaluated and value set to \`value\`.`,
    examples: [
      `(create-global-variable "a" "b")`,
      `(progn (create-global-variable "a" 20) (create-local-variable "a" 10)) a`,
      `(create-global-variable (concat "a" "1") (object "x" 10 "y" true "z" "A string")) a1`,
      `(create-global-variable "a" "b") (create-global-variable a "c") b`,
    ],
    specialExpression: true,
    sideEffects: [],
  },
  'create-constant-variable': {
    name: `create-constant-variable`,
    category: `Special expression`,
    linkName: `create-constant-variable`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `variable`,
        type: `form`,
      },
      {
        name: `value`,
        type: `any`,
      },
    ],
    shortDescription: `Creates a constant global variable with name set to \`variable\` evaluated and value set to \`value\`.`,
    longDescription: `Creates a constant global variable with name set to \`variable\` evaluated and value set to \`value\`.`,
    examples: [
      `(create-constant-variable "a" "b")`,
      `(create-constant-variable (concat "a" "1") (object "x" 10 "y" true "z" "A string")) a1`,
      `(create-constant-variable "a" "b") (create-constant-variable a "c") b`,
    ],
    specialExpression: true,
    sideEffects: [],
  },
  let: {
    name: `let`,
    category: `Special expression`,
    linkName: `let`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `bindings`,
        type: `bindings`,
      },
      {
        name: `body`,
        type: `lisp expressions`,
      },
    ],
    shortDescription: `Binds local variables. The variables lives only within the body.`,
    longDescription: `Binds local variables. The variables lives only within the body. It returns evaluation of the last expression in the body.`,
    examples: [`(let ((a (+ 1 2 3 4)) (b (* 1 2 3 4))) (write a b))`],
    specialExpression: true,
    sideEffects: [],
  },
  lambda: {
    name: `lambda`,
    category: `Special expression`,
    linkName: `lambda`,
    returns: {
      type: `function`,
    },
    arguments: [
      {
        name: `arguments`,
        type: `arguments`,
      },
      {
        name: `body`,
        type: `lisp expressions`,
      },
    ],
    shortDescription: `Creates a function. When called, evaluation of the last expression in the body is returned.`,
    longDescription: `Creates a function. When called, evaluation of the last expression in the body is returned.`,
    examples: [`(lambda (a b) (sqrt (+ (* a a) (* b b))))`, `((lambda (a b) (sqrt (+ (* a a) (* b b)))) 3 4)`],
    specialExpression: true,
    sideEffects: [],
  },
  defun: {
    name: `defun`,
    category: `Special expression`,
    linkName: `defun`,
    returns: {
      type: `function`,
    },
    arguments: [
      {
        name: `name`,
        type: `name`,
      },
      {
        name: `arguments`,
        type: `arguments`,
      },
      {
        name: `body`,
        type: `lisp expressions`,
      },
    ],
    shortDescription: `Creates a named global function. When called, evaluation of the last expression in the body is returned.`,
    longDescription: `Creates a named global function. When called, evaluation of the last expression in the body is returned.`,
    examples: [
      `(defun hyp (a b) (sqrt (+ (* a a) (* b b)))) hyp`,
      `(defun hyp (a b) (sqrt (+ (* a a) (* b b)))) (hyp 3 4)`,
      `(defun sumOfSquares (&rest s) (apply + (map (lambda (x) (* x x)) s))) (sumOfSquares 1 2 3 4 5)`,
    ],
    specialExpression: true,
    sideEffects: [`Creates a new function`],
  },
  'create-function': {
    name: `create-function`,
    category: `Special expression`,
    linkName: `create-function`,
    returns: {
      type: `function`,
    },
    arguments: [
      {
        name: `name`,
        type: `form`,
      },
      {
        name: `arguments`,
        type: `arguments`,
      },
      {
        name: `body`,
        type: `lisp expressions`,
      },
    ],
    shortDescription: `Creates a named global function with its name set to \`name\` evaluated. When called, evaluation of the last expression in the body is returned.`,
    longDescription: `Creates a named global function with its name set to \`name\` evaluated. When called, evaluation of the last expression in the body is returned.`,
    examples: [
      `(create-function "hyp" (a b) (sqrt (+ (* a a) (* b b)))) hyp`,
      `(create-function (concat "h" "y" "p") (a b) (sqrt (+ (* a a) (* b b)))) (hyp 3 4)`,
      `(create-function "sumOfSquares" (&rest s) (apply + (map (lambda (x) (* x x)) s))) (sumOfSquares 1 2 3 4 5)`,
    ],
    specialExpression: true,
    sideEffects: [`Creates a new function`],
  },
  'return-from': {
    name: `return-from`,
    category: `Special expression`,
    linkName: `return-from`,
    returns: {
      type: `nothing`,
    },
    arguments: [
      {
        name: `blockName`,
        type: `name`,
      },
      {
        name: `value`,
        type: `any`,
      },
    ],
    shortDescription: `Returns control and value from a named enclosing block.`,
    longDescription: `Returns control and value from a named enclosing block.`,
    examples: [`(defun fn () (write "Alpha") (return-from fn "Beta") (write "Gamma")) (fn)`],
    specialExpression: true,
    sideEffects: [],
  },
  return: {
    name: `return`,
    category: `Special expression`,
    linkName: `return`,
    returns: {
      type: `nothing`,
    },
    arguments: [
      {
        name: `value`,
        type: `any`,
      },
    ],
    shortDescription: `Returns control and value from the enclosing block.`,
    longDescription: `Returns control and value from the enclosing block.`,
    examples: [`(defun fn () (write "Alpha") (return "Beta") (write "Gamma")) (fn)`],
    specialExpression: true,
    sideEffects: [],
  },
  block: {
    name: `block`,
    category: `Special expression`,
    linkName: `block`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `blockName`,
        type: `name`,
      },
      {
        name: `forms`,
        type: `form[]`,
      },
    ],
    shortDescription: `Establishes a block named \`blockName\` and then evaluates forms as an implicit progn.`,
    longDescription: `Establishes a block named \`blockName\` and then evaluates forms as an implicit progn.`,
    examples: [
      `(block b (write "Alpha") (write "Gamma"))`,
      `(block b (write "Alpha") (return-from b undefined) (write "Gamma"))`,
    ],
    specialExpression: true,
    sideEffects: [],
  },
  try: {
    name: `try`,
    category: `Special expression`,
    linkName: `try`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `tryExpression`,
        type: `form`,
      },
      {
        name: `catchBlock`,
        type: `catchBlock`,
      },
    ],
    shortDescription: `Executes tryExpression. If that throws, the catchBlock gets executed.`,
    longDescription: `Executes tryExpression. If that throws, the catchBlock gets executed. See examples for details.`,
    examples: [`(try (/ 2 4) ((error) (/ 2 1)))`, `(try (/ 2 0) ((error) (/ 2 1)))`, `(try (/ 2 0) ((error) error))`],
    specialExpression: true,
    sideEffects: [],
  },
  throw: {
    name: `throw`,
    category: `Special expression`,
    linkName: `throw`,
    returns: {
      type: `nothing`,
    },
    arguments: [
      {
        name: `message`,
        type: `form`,
      },
    ],
    shortDescription: `Throws \`UserDefinedError\` with message set to \`message\` evaluated.`,
    longDescription: `Throws \`UserDefinedError\` with message set to \`message\` evaluated. \`message\` must evaluate to a \`string\`.`,
    examples: [
      `(throw "You shall not pass!")`,
      `(throw (substring "You shall not pass!" 0 3))`,
      `(try (throw "You shall not pass!") ((error) error))`,
    ],
    specialExpression: true,
    sideEffects: [],
  },
  if: {
    name: `if`,
    category: `Special expression`,
    linkName: `if`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `test`,
        type: `any`,
      },
      {
        name: `then`,
        type: `any`,
      },
      {
        name: `else`,
        type: `any`,
      },
    ],
    shortDescription: `Either \`then\` or \`else\` branch is taken. Then branch is selected when \`test\` result is truthy.`,
    longDescription: `Either \`then\` or \`else\` branch is taken. Then branch is selected when \`test\` result is truthy.`,
    examples: [`(if true (write "TRUE") (write "FALSE"))`, `(if false (write "TRUE") (write "FALSE"))`],
    specialExpression: true,
    sideEffects: [],
  },
  cond: {
    name: `cond`,
    category: `Special expression`,
    linkName: `cond`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `variants`,
        type: `variants`,
      },
    ],
    shortDescription: `Used for branching. Variants are tested sequentially from the top.`,
    longDescription: `Used for branching. Variants are tested sequentially from the top. I no branch is tested truthy, \`undefined\` is returned.`,
    examples: [
      `(cond (false (write "FALSE")) (null (write "NULL")) (true (write "TRUE")))`,
      `(cond (false (write "FALSE")) (null (write "NULL")))`,
    ],
    specialExpression: true,
    sideEffects: [],
  },
  when: {
    name: `when`,
    category: `Special expression`,
    linkName: `when`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `test`,
        type: `form`,
      },
      {
        name: `form`,
        type: `form`,
        description: `zero or more`,
      },
    ],
    shortDescription: `If \`test\` yields a truthy value, the forms are evaluated in order from left to right and the value returned by the last \`form\` is returned.`,
    longDescription: `If \`test\` yields a thruthy value, the forms are evaluated in order from left to right and the value returned by the last \`form\` is returned. Otherwise, if \`test\` yields a falsy value, the forms are not evaluated, and \`undefined\` is returned. If no \`form\` is provided, undefined is returned.`,
    examples: [
      `(when true (write "Hi") (write "There"))`,
      `(when false (write "Hi") (write "There"))`,
      `(when true)`,
      `(when false)`,
    ],
    specialExpression: true,
    sideEffects: [],
  },
  unless: {
    name: `unless`,
    category: `Special expression`,
    linkName: `unless`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `test`,
        type: `form`,
      },
      {
        name: `form`,
        type: `form`,
        description: `zero or more`,
      },
    ],
    shortDescription: `If \`test\` yields a falsy value, the forms are evaluated in order from left to right and the value returned by the last \`form\` is returned.`,
    longDescription: `If \`test\` yields a falsy value, the forms are evaluated in order from left to right and the value returned by the last \`form\` is returned. Otherwise, if \`test\` yields a truthy value, the forms are not evaluated, and \`undefined\` is returned. If no \`form\` is provided, undefined is returned.`,
    examples: [
      `(unless true (write "Hi") (write "There"))`,
      `(unless false (write "Hi") (write "There"))`,
      `(unless true)`,
      `(unless false)`,
    ],
    specialExpression: true,
    sideEffects: [],
  },
  loop: {
    name: `loop`,
    category: `Special expression`,
    linkName: `loop`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `form`,
        type: `form`,
        description: `zero or more`,
      },
    ],
    shortDescription: `Runs the \`form\`s repeatedly until \`return\` or \`throw\`.`,
    longDescription: `Runs the \`form\`s repeatedly until \`return\` or \`throw\`.`,
    examples: [`(def i 0) (loop (write "Hi") (def i (1+ i)) (when (> i 3) (return i)))`],
    specialExpression: true,
    sideEffects: [],
  },
  doarray: {
    name: `doarray`,
    category: `Special expression`,
    linkName: `doarray`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `(var array [result])`,
        type: `doarray form`,
      },
      {
        name: `form`,
        type: `form`,
        description: `zero or more`,
      },
    ],
    shortDescription: `Runs the \`form\`s once for each element in the array. if \`result\` is present, it will be evaluated after all iterations and its value will be returned.`,
    longDescription: `Runs the \`form\`s once for each element in the array. if \`result\` is present, it will be evaluated after all iterations and its value will be returned.`,
    examples: [
      `(def l [1 2 3]) (def x 0) (doarray (el l) (def x (+ x el))) x`,
      `(def l [1 2 3]) (doarray (el l))`,
      `(def l [1 2 3]) (let ((x 0)) (doarray (el l x) (def x (+ x el))))`,
      `(def l [1 2 3]) (let ((x 0)) (doarray (el l x) (def x (+ x el)) (return x)))`,
      `(def l [1 2 3]) (let ((x 0)) (doarray (el l x) (def x (+ x el)) (throw "Oops")))`,
    ],
    specialExpression: true,
    sideEffects: [],
  },
  progn: {
    name: `progn`,
    category: `Special expression`,
    linkName: `progn`,
    returns: {
      type: `any`,
    },
    arguments: [
      {
        name: `forms`,
        type: `form[]`,
      },
    ],
    shortDescription: `Calls \`forms\` in the order they have been written. Resulting value is the value of the last form.`,
    longDescription: `Calls \`forms\` in the order they have been written. Resulting value is the value of the last form.`,
    examples: [`(progn (write "Hi") (write "Albert"))`, `(progn)`],
    specialExpression: true,
    sideEffects: [],
  },
}
