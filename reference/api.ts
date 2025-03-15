import type { Argument } from '.'

export const api = {
  collection: [
    'count',
    'get',
    'get-in',
    'contains?',
    'assoc',
    'assoc-in',
    '++',
    'not-empty',
    'every?',
    'not-every?',
    'any?',
    'not-any?',
    'update',
    'update-in',
  ] as const,
  array: [
    'array',
    'range',
    'repeat',
    'flatten',
    'mapcat',
  ] as const,
  sequence: [
    'nth',
    'push',
    'pop',
    'unshift',
    'shift',
    'slice',
    'splice',
    'reductions',
    'reduce',
    'reduce-right',
    'map',
    'filter',
    'position',
    'index-of',
    'last-index-of',
    'some',
    'reverse',
    'first',
    'second',
    'last',
    'rest',
    'next',
    'take',
    'take-last',
    'take-while',
    'drop',
    'drop-last',
    'drop-while',
    'sort',
    'sort-by',
    'distinct',
    'remove',
    'remove-at',
    'split-at',
    'split-with',
    'frequencies',
    'group-by',
    'partition',
    'partition-all',
    'partition-by',
    'starts-with?',
    'ends-with?',
    'interleave',
    'interpose',
  ] as const,
  math: [
    '+',
    '-',
    '*',
    '/',
    'mod',
    '%',
    'quot',
    'inc',
    'dec',
    '√',
    '∛',
    '**',
    'round',
    'trunc',
    'floor',
    'ceil',
    'min',
    'max',
    'abs',
    'sign',
    'log',
    'log2',
    'log10',
    'sin',
    'cos',
    'tan',
    'asin',
    'acos',
    'atan',
    'sinh',
    'cosh',
    'tanh',
    'asinh',
    'acosh',
    'atanh',
  ] as const,
  functional: [
    'apply',
    'identity',
    'partial',
    'comp',
    'constantly',
    'juxt',
    'complement',
    'every-pred',
    'some-pred',
    'fnull',
  ] as const,
  misc: [
    '≠',
    '=',
    '<',
    '>',
    '≤',
    '≥',
    '!',
    'write!',
    'iso-date->epoch',
    'epoch->iso-date',
    'boolean',
    'compare',
    'identical?',
    'json-parse',
    'json-stringify',
  ] as const,
  object: [
    'dissoc',
    'object',
    'keys',
    'vals',
    'entries',
    'find',
    'merge',
    'merge-with',
    'zipmap',
    'select-keys',
  ] as const,
  predicate: [
    'boolean?',
    'null?',
    'number?',
    'string?',
    'function?',
    'integer?',
    'array?',
    'object?',
    'coll?',
    'seq?',
    'regexp?',
    'zero?',
    'pos?',
    'neg?',
    'even?',
    'odd?',
    'finite?',
    'nan?',
    'negative-infinity?',
    'positive-infinity?',
    'false?',
    'true?',
    'empty?',
    'not-empty?',
  ] as const,
  regularExpression: [
    'regexp',
    'match',
    'replace',
    'replace-all',
  ] as const,
  specialExpressions: [
    '&&',
    '||',
    'def',
    'let',
    'fn',
    'defn',
    'function',
    'try',
    'throw',
    'if',
    'unless',
    'cond',
    'switch',
    'do',
    'recur',
    'loop',
    'doseq',
    'for',
    'defined?',
    '??',
  ] as const,
  string: [
    'string-repeat',
    'str',
    'number',
    'lower-case',
    'upper-case',
    'trim',
    'trim-left',
    'trim-right',
    'pad-left',
    'pad-right',
    'split',
    'split-lines',
    'template',
    'to-char-code',
    'from-char-code',
    'encode-base64',
    'decode-base64',
    'encode-uri-component',
    'decode-uri-component',
    'join',
    'capitalize',
    'blank?',
  ] as const,
  bitwise: [
    '<<',
    '>>',
    '>>>',
    '~',
    '&',
    'bit-and-not',
    '|',
    '^',
    'bit-flip',
    'bit-clear',
    'bit-set',
    'bit-test',
  ] as const,
  // TODO, remove some, add some. E.g. type guards, assert-number, assert-string, etc.
  assert: [
    'assert',
    'assert=',
    'assert!=',
    'assert-gt',
    'assert-lt',
    'assert-gte',
    'assert-lte',
    'assert-true',
    'assert-false',
    'assert-truthy',
    'assert-falsy',
    'assert-null',
    'assert-throws',
    'assert-throws-error',
    'assert-not-throws',
  ] as const,
  shorthand: [
    '-short-regexp',
    '-short-fn',
  ] as const satisfies `-short-${string}`[],
  datatype: [
    '-type-number',
    '-type-string',
    '-type-object',
    '-type-array',
    '-type-boolean',
    '-type-function',
    '-type-integer',
    '-type-any',
    '-type-null',
    '-type-collection',
    '-type-sequence',
    '-type-regexp',
    '-type-never',
  ] as const satisfies `-type-${string}`[],
} as const

export type CollectionApiName = typeof api.collection[number]
export type ArrayApiName = typeof api.array[number]
export type SequenceApiName = typeof api.sequence[number]
export type MathApiName = typeof api.math[number]
export type FunctionalApiName = typeof api.functional[number]
export type MiscApiName = typeof api.misc[number]
export type ObjectApiName = typeof api.object[number]
export type PredicateApiName = typeof api.predicate[number]
export type RegularExpressionApiName = typeof api.regularExpression[number]
export type SpecialExpressionsApiName = string
export type StringApiName = typeof api.string[number]
export type BitwiseApiName = typeof api.bitwise[number]
export type AssertApiName = typeof api.assert[number]

export type NormalExpressionName =
  | CollectionApiName
  | ArrayApiName
  | SequenceApiName
  | MathApiName
  | FunctionalApiName
  | MiscApiName
  | ObjectApiName
  | PredicateApiName
  | RegularExpressionApiName
  | StringApiName
  | BitwiseApiName
  | AssertApiName

export type FunctionName =
  | NormalExpressionName
  | SpecialExpressionsApiName

export type ShorthandName = typeof api.shorthand[number]

export type DatatypeName = typeof api.datatype[number]

const apiFunctionNames = [
  ...api.collection,
  ...api.array,
  ...api.sequence,
  ...api.math,
  ...api.functional,
  ...api.misc,
  ...api.object,
  ...api.predicate,
  ...api.regularExpression,
  ...api.string,
  ...api.bitwise,
  ...api.assert,
] as const

const apiNames = [
  ...apiFunctionNames,
  ...api.shorthand,
  ...api.datatype,
] as const

export type ApiName = typeof apiNames[number]

export function isApiName(arg: string): arg is ApiName {
  return apiNames.includes(arg as ApiName)
}

export const categoryRecord = {
  'Special expression': true,
  'Predicate': true,
  'Sequence': true,
  'Collection': true,
  'Array': true,
  'Object': true,
  'String': true,
  'Math': true,
  'Functional': true,
  'Regular expression': true,
  'Bitwise': true,
  'Misc': true,
  'Assert': true,
  'Shorthand': true,
  'Datatype': true,
} as const

export type Category = keyof typeof categoryRecord

export const categories = Object.keys(categoryRecord) as Category[]

const dataTypes = [
  'number',
  'string',
  'object',
  'array',
  'boolean',
  'function',
  'integer',
  'any',
  'null',
  'collection',
  'sequence',
  'regexp',
  'never',
] as const
export type DataType = typeof dataTypes[number]

export function isDataType(arg: string): arg is DataType {
  return dataTypes.includes(arg as DataType)
}

export function getOperatorArgs(a: DataType | DataType[], b: DataType | DataType[]): Record<string, Argument> {
  return { a: { type: a }, b: { type: b } }
}

// const a = `
// // Functional Text Adventure Game in Lits (without pipe operator)

// // Define locations
// export let locations := {
//   "forest" := {
//     "description" := "You are in a dense forest. Light filters through the leaves above.",
//     "exits" := { "north" := "cave", "east" := "river", "south" := "meadow" }
//   },
//   "cave" := {
//     "description" := "You stand in a dark cave. Water drips from stalactites overhead.",
//     "exits" := { "south" := "forest", "east" := "tunnel" },
//     "items" := ["torch"]
//   },
//   "river" := {
//     "description" := "A swift river flows from the mountains. The water is crystal clear.",
//     "exits" := { "west" := "forest", "north" := "waterfall" },
//     "items" := ["fishing rod"]
//   },
//   "meadow" := {
//     "description" := "A peaceful meadow stretches before you, filled with wildflowers.",
//     "exits" := { "north" := "forest", "east" := "cottage" },
//     "items" := ["flowers"]
//   },
//   "waterfall" := {
//     "description" := "A magnificent waterfall cascades down from high cliffs.",
//     "exits" := { "south" := "river" },
//     "items" := ["shiny stone"]
//   },
//   "tunnel" := {
//     "description" := "A narrow tunnel leads deeper into the mountain.",
//     "exits" := { "west" := "cave", "east" := "treasure room" }
//   },
//   "treasure room" := {
//     "description" := "A small chamber glittering with treasure!",
//     "exits" := { "west" := "tunnel" },
//     "items" := ["gold key", "ancient map", "jeweled crown"]
//   },
//   "cottage" := {
//     "description" := "A cozy cottage with a smoking chimney stands here.",
//     "exits" := { "west" := "meadow" },
//     "items" := ["bread"]
//   }
// };

// // Define game state
// export let initial-state := {
//   "current-location" := "forest",
//   "inventory" := [],
//   "visited" := {},
//   "game-over" := false,
//   "moves" := 0,
//   "light-source" := false
// };

// // Helper functions
// function has-item?(state, item)
//   contains?(state.inventory, item);
// end;

// function location-has-item?(location, item)
//   contains?(get(location, "items", []), item);
// end;

// function describe-location(state)
//   let location := get(locations, state.current-location);
//   let description := location.description;

//   // Add visited status
//   let visited-status := if get(state.visited, state.current-location, false) then
//     "You've been here before."
//   else
//     "This is your first time here."
//   end;

//   // Check if location has items
//   let items-desc := if !(empty?(get(location, "items", []))) then
//     "You see: " ++ join(location.items, ", ")
//   end;

//   // Describe exits
//   let exits := join(keys(location.exits), ", ");
//   let exits-desc := "Exits: " ++ exits;

//   // Join all descriptions
//   join(filter([description, visited-status, items-desc, exits-desc], -> !(empty?($))), "\n");
// end;

// function get-location-items(state)
//   let location := get(locations, state.current-location);
//   get(location, "items", []);
// end;

// // Game actions
// function move(state, direction)
//   let location := get(locations, state.current-location);
//   let exits := get(location, "exits", {});

//   // Check if direction is valid
//   if contains?(exits, direction) then
//     let new-location := get(exits, direction);
//     let is-dark := new-location = "tunnel" || new-location = "treasure room";

//     // Check if player has light source for dark areas
//     if is-dark && !state.light-source then
//       let result := [state, "It's too dark to go that way without a light source."];
//       result
//     else
//       let new-visited := assoc(state.visited, new-location, true);
//       let new-state := assoc(assoc(assoc(state, "current-location", new-location),
//                                    "visited", new-visited),
//                              "moves", state.moves + 1);

//       let result := [new-state, "You move " ++ direction ++ " to the " ++ new-location ++ "."];
//       result
//     end
//   else
//     let result := [state, "You can't go that way."];
//     result
//   end
// end;

// function take!(state, item)
//   let items := get-location-items(state);

//   if contains?(items, item) then
//     let location := get(locations, state.current-location);
//     let new-location-items := filter(-> $ ≠ item, items);
//     let new-inventory := push(state.inventory, item);

//     // Update game state
//     let new-locations := assoc(locations, state.current-location,
//                               assoc(location, "items", new-location-items));

//     // Special case for torch
//     let has-light := item = "torch" || state.light-source;

//     // Update locations and state
//     let locations := new-locations;
//     let new-state := assoc(assoc(assoc(state, "inventory", new-inventory),
//                                 "light-source", has-light),
//                           "moves", state.moves + 1);

//     let result := [new-state, "You take the " ++ item ++ "."];
//     result
//   else
//     let result := [state, "There is no " ++ item ++ " here."];
//     result
//   end
// end;

// function drop!(state, item)
//   if has-item?(state, item) then
//     let location := get(locations, state.current-location);
//     let location-items := get(location, "items", []);
//     let new-location-items := push(location-items, item);
//     let new-inventory := filter(-> $ ≠ item, state.inventory);

//     // Special case for torch
//     let still-has-light := !(item = "torch") || contains?(new-inventory, "torch");

//     // Update locations and state
//     let new-location := assoc(location, "items", new-location-items);
//     let locations := assoc(locations, state.current-location, new-location);

//     let new-state := assoc(assoc(assoc(state, "inventory", new-inventory),
//                                 "light-source", still-has-light),
//                           "moves", state.moves + 1);

//     let result := [new-state, "You drop the " ++ item ++ "."];
//     result
//   else
//     let result := [state, "You don't have a " ++ item ++ " in your inventory."];
//     result
//   end
// end;

// function inventory(state)
//   if empty?(state.inventory) then
//     let result := [state, "Your inventory is empty."];
//     result
//   else
//     let result := [state, "Inventory: " ++ join(state.inventory, ", ")];
//     result
//   end
// end;

// function use(state, item)
//   switch item
//     case "fishing rod" then
//       if state.current-location = "river" then
//         let result := [assoc(state, "moves", state.moves + 1), "You catch a small fish, but it slips away."];
//         result
//       else
//         let result := [state, "There's no place to use a fishing rod here."];
//         result
//       end
//     case "torch" then
//       if has-item?(state, item) then
//         let result := [assoc(assoc(state, "light-source", true), "moves", state.moves + 1),
//          "The torch illuminates the area with a warm glow."];
//         result
//       else
//         let result := [state, "You don't have a torch."];
//         result
//       end
//     case "gold key" then
//       if has-item?(state, item) && state.current-location = "treasure room" then
//         let result := [assoc(assoc(state, "game-over", true), "moves", state.moves + 1),
//          "You use the gold key to unlock a secret compartment, revealing a fabulous diamond! You win!"];
//         result
//       else
//         let result := [state, "The key doesn't fit anything here."];
//         result
//       end
//     case "bread" then
//       if has-item?(state, item) then
//         let new-inventory := filter(state.inventory, -> $ ≠ item);
//         let result := [assoc(assoc(state, "inventory", new-inventory), "moves", state.moves + 1),
//          "You eat the bread. It's delicious and nourishing."];
//         result
//       else
//         let result := [state, "You don't have any bread."];
//         result
//       end
//     case "shiny stone" then
//       if has-item?(state, item) then
//         let result := [assoc(state, "moves", state.moves + 1),
//          "The stone glows with a faint blue light. It seems magical but you're not sure how to use it yet."];
//         result
//       else
//         let result := [state, "You don't have a shiny stone."];
//         result
//       end
//     case "flowers" then
//       if has-item?(state, item) then
//         let result := [assoc(state, "moves", state.moves + 1),
//          "You smell the flowers. They have a sweet, calming fragrance."];
//         result
//       else
//         let result := [state, "You don't have any flowers."];
//         result
//       end
//     case "ancient map" then
//       if has-item?(state, item) then
//         let result := [assoc(state, "moves", state.moves + 1),
//          "The map shows the layout of the area. All locations are now marked as visited."];
//         result
//       else
//         let result := [state, "You don't have a map."];
//         result
//       end
//     case "jeweled crown" then
//       if has-item?(state, item) then
//         let result := [assoc(state, "moves", state.moves + 1),
//          "You place the crown on your head. You feel very regal."];
//         result
//       else
//         let result := [state, "You don't have a crown."];
//         result
//       end
//     case _ then
//       let result := [state, "You can't use that."];
//       result
//   end
// end;

// // Command parser
// function parse-command(state, input)
//   let tokens := split(lower-case(input), " ");
//   let command := first(tokens);
//   let args := join(rest(tokens), " ");

//   let result := switch command
//     case "go" then
//       move(state, args)
//     case "north" then
//       move(state, "north")
//     case "south" then
//       move(state, "south")
//     case "east" then
//       move(state, "east")
//     case "west" then
//       move(state, "west")
//     case "take" then
//       take!(state, args)
//     case "drop" then
//       drop!(state, args)
//     case "inventory" then
//       inventory(state)
//     case "i" then
//       inventory(state)
//     case "look" then
//       let look_result := [assoc(state, "moves", state.moves + 1), describe-location(state)];
//       look_result
//     case "use" then
//       use(state, args)
//     case "help" then
//       let help_result := [state, "Commands: go [direction], north, south, east, west, take [item], drop [item], inventory, look, use [item], help, quit"];
//       help_result
//     case "quit" then
//       let quit_result := [assoc(state, "game-over", true), "Thanks for playing!"];
//       quit_result
//     case _ then
//       let unknown_result := [state, "I don't understand that command. Type 'help' for a list of commands."];
//       unknown_result
//   end;

//   result
// end;

// // Game loop
// function game-loop(state)
//   alert!(describe-location(state));
//   alert!("\nWhat do you do? ");

//   let input := read-line!();
//   let command_result := parse-command(state, input);
//   let new-state := first(command_result);
//   let message := second(command_result);

//   alert!("\n" ++ message ++ "\n");

//   if new-state.game-over then
//     alert!("\nGame over! You made " ++ str(new-state.moves) ++ " moves.");
//     new-state
//   else
//     game-loop(new-state)
//   end
// end;

// // Start game
// function start-game()
//   alert!("=== Lits Adventure Game ===\n");
//   alert!("Type 'help' for a list of commands.\n\n");
//   game-loop(initial-state)
// end;

// // Call the function to start the game
// start-game()
// `
