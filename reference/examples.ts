export interface Example {
  id: string
  name: string
  description: string
  code: string
  context?: {
    values?: Record<string, unknown>
    jsFunctions?: Record<string, string>
  }
}

export const examples: Example[] = [
  {
    id: 'default',
    name: 'Simple Lits program',
    description: 'A super simple example.',
    code: `
10 + 20
    `.trim(),
  },
  {
    id: 'collection-accessor',
    name: 'Collection accessors',
    description: 'Syntactic sugar for accessing object, array and string elements.',
    code: `
// Access object properies with .
// Access string and array elements with []

let data = {
  numbers: [1, 2, 3],
  chars: ["a", "b", "c"],
  string: "Albert"
};

write!(data.numbers[0]);
write!(data.chars[2]);
write!(data.string[0]);

write!({a: 1, b: 2, c: 3}.b);
write!("Albert"[3]);
write!([1, 2, 3][2]);
    `.trim(),
  },
  {
    id: 'simple-context-example',
    name: 'Using context',
    description: 'Simple example using a context.',
    context: {
      values: { x: 15, y: 27 },
      jsFunctions: { plus: '(a, b) => a + b' },
    },
    code: `
  plus(x, y)
      `.trim(),
  },
  {
    id: 'text-based-game',
    name: 'A game',
    description: 'Text based adventure game.',
    context: {
      jsFunctions: {
        'alert!': '(message) => alert(message)',
        'read-line!': '(message) => prompt(message)',
      },
    },
    code: `
// Functional Text Adventure Game in Lits

// Define locations
let locations = {
  forest: {
    description: "You are in a dense forest. Light filters through the leaves above.",
    exits: { north: "cave", east: "river", south: "meadow" }
  },
  cave: {
    description: "You stand in a dark cave. Water drips from stalactites overhead.",
    exits: { south: "forest", east: "tunnel" },
    items: ["torch"]
  },
  river: {
    description: "A swift river flows from the mountains. The water is crystal clear.",
    exits: { west: "forest", north: "waterfall" },
    items: ["fishing rod"]
  },
  meadow: {
    description: "A peaceful meadow stretches before you, filled with wildflowers.",
    exits: { north: "forest", east: "cottage" },
    items: ["flowers"]
  },
  waterfall: {
    description: "A magnificent waterfall cascades down from high cliffs.",
    exits: { south: "river" },
    items: ["shiny stone"]
  },
  tunnel: {
    description: "A narrow tunnel leads deeper into the mountain.",
    exits: { west: "cave", east: "treasure room" }
  },
  "treasure room": {
    description: "A small chamber glittering with treasure!",
    exits: { west: "tunnel" },
    items: ["gold key", "ancient map", "jeweled crown"]
  },
  cottage: {
    description: "A cozy cottage with a smoking chimney stands here.",
    exits: { west: "meadow" },
    items: ["bread"]
  }
};

// Define game state
let initial-state = {
  current-location: "forest",
  inventory: [],
  visited: {},
  game-over: false,
  moves: 0,
  light-source: false
};

// Helper functions
let has-item? = (state, item) -> {
  contains?(state.inventory, item);
};

let location-has-item? = (location, item) -> {
  contains?(get(location, "items", []), item);
};

let describe-location = (state) -> {
  let location = get(locations, state.current-location);
  let description = location.description;

  // Add visited status
  let visited-status = if get(state.visited, state.current-location, 0) > 1 then {
    "You've been here before."
  } else {
    "This is your first time here."
  };

  // Check if location has items
  let items-desc = if !(empty?(get(location, "items", []))) then {
    "You see: " ++ join(location.items, ", ")
  } else {
    ""
  };

  // Describe exits
  let exits = keys(location.exits) join ", ";
  let exits-desc = "Exits: " ++ exits;

  // Join all descriptions
  filter([description, visited-status, items-desc, exits-desc], -> !(empty?($))) join "\\n"
};

let get-location-items = (state) -> {
  let location = get(locations, state.current-location);
  get(location, "items", [])
};

// Game actions
let move = (state, direction) -> {
  let location = get(locations, state.current-location);
  let exits = get(location, "exits", {});

  // Check if direction is valid
  if contains?(exits, direction) then {
    let new-location = get(exits, direction);
    let is-dark = new-location == "tunnel" || new-location == "treasure room";

    // Check if player has light source for dark areas
    if is-dark && !(state.light-source) then {
      [state, "It's too dark to go that way without a light source."]
    } else {
      let new-visited = assoc(
        state.visited,
        new-location,
        inc(state.visited["new-location"] ?? 0)
      );
      let new-state = assoc(
        assoc(
          assoc(state, "current-location", new-location),
          "visited",
          new-visited
        ),
        "moves",
        state.moves + 1
      );

      [new-state, "You move " ++ direction ++ " to the " ++ new-location ++ "."]
    }
  } else {
    [state, "You can't go that way."]
  }
};

let take! = (state, item) -> {
  let items = get-location-items(state);

  if contains?(items, item) then {
    let location = get(locations, state.current-location);
    let new-location-items = filter(items, -> $ ≠ item);
    let new-inventory = push(state.inventory, item);

    // Update game state
    let new-locations = assoc(
      locations, 
      state.current-location,
      assoc(location, "items", new-location-items)
    );

    // Special case for torch
    let has-light = item == "torch" || state.light-source;

    // Update locations and state
    let locations = new-locations;
    let new-state = assoc(
      assoc(
        assoc(state, "inventory", new-inventory),
        "light-source", has-light
      ),
      "moves",
      state.moves + 1
    );
    [new-state, "You take the " ++ item ++ "."]
  } else {
    [state, "There is no " ++ item ++ " here."]
  }
};

let drop! = (state, item) -> {
  if has-item?(state, item) then {
    let location = get(locations, state.current-location);
    let location-items = get(location, "items", []);
    let new-location-items = push(location-items, item);
    let new-inventory = filter(-> $ ≠ item, state.inventory);

    // Special case for torch
    let still-has-light = !(item == "torch") || contains?(new-inventory, "torch");

    // Update locations and state
    let new-location = assoc(location, "items", new-location-items);
    let locations = assoc(locations, state.current-location, new-location);

    let new-state = assoc(
      assoc(
        assoc(
          state, "inventory", new-inventory),
          "light-source",
          still-has-light
        ),
        "moves",
        state.moves + 1
      );
    [new-state, "You drop the " ++ item ++ "."]
  } else {
    [state, "You don't have a " ++ item ++ " in your inventory."]
  }
};

let inventory = (state) -> {
  if empty?(state.inventory) then {
    [state, "Your inventory is empty."]
  } else {
    [state, "Inventory: " ++ join(state.inventory, ", ")]
  }
};

let use = (state, item) -> {
  switch item
    case "fishing rod" then
      if state.current-location == "river" then {
        [assoc(state, "moves", state.moves + 1), "You catch a small fish, but it slips away."]
      } else {
        [state, "There's no place to use a fishing rod here."]
      }
    case "torch" then
      if has-item?(state, item) then {
        [
          assoc(assoc(state, "light-source", true), "moves", state.moves + 1),
          "The torch illuminates the area with a warm glow."
        ]
      } else {
        [state, "You don't have a torch."]
      }
    case "gold key" then
      if has-item?(state, item) && state.current-location == "treasure room" then {
        [
          assoc(
            assoc(state, "game-over", true),
            "moves",
            state.moves + 1
          ),
         "You use the gold key to unlock a secret compartment, revealing a fabulous diamond! You win!"
        ]
      } else {
        [state, "The key doesn't fit anything here."]
      }
    case "bread" then
      if has-item?(state, item) then {
        let new-inventory = filter(state.inventory, -> $ ≠ item);
        [
          assoc(
            assoc(state, "inventory", new-inventory),
            "moves",
            state.moves + 1
          ),
          "You eat the bread. It's delicious and nourishing."
        ]
      } else {
        [state, "You don't have any bread."]
      }
    case "shiny stone" then
      if has-item?(state, item) then {
        [
          assoc(state, "moves", state.moves + 1),
          "The stone glows with a faint blue light. It seems magical but you're not sure how to use it yet."
        ]
      } else {
        [state, "You don't have a shiny stone."]
      }
    case "flowers" then
      if has-item?(state, item) then {
        [
          assoc(state, "moves", state.moves + 1),
          "You smell the flowers. They have a sweet, calming fragrance."
        ]
      } else {
        [state, "You don't have any flowers."]
      }
    case "ancient map" then
      if has-item?(state, item) then {
        [
          assoc(state, "moves", state.moves + 1),
          "The map shows the layout of the area. All locations are now marked as visited."
        ]
      } else {
        [state, "You don't have a map."]
      }
    case "jeweled crown" then
      if has-item?(state, item) then {
        [
          assoc(state, "moves", state.moves + 1),
          "You place the crown on your head. You feel very regal."
        ]
      } else {
        [state, "You don't have a crown."]
      }
  end ?? [state, "You can't use that."]
};

// Command parser
let parse-command = (state, input) -> {
  let tokens = lower-case(input) split " ";
  let command = first(tokens);
  let args = rest(tokens) join " ";

  let result = switch command
    case "go" then
      move(state, args)
    case "north" then
      move(state, "north")
    case "south" then
      move(state, "south")
    case "east" then
      move(state, "east")
    case "west" then
      move(state, "west")
    case "take" then
      take!(state, args)
    case "drop" then
      drop!(state, args)
    case "inventory" then
      inventory(state)
    case "i" then
      inventory(state)
    case "look" then
      [assoc(state, "moves", state.moves + 1), describe-location(state)]
    case "use" then
      use(state, args)
    case "help" then
      [state, "Commands then go [direction], north, south, east, west, take [item], drop [item], inventory, look, use [item], help, quit"]
    case "quit" then
      [assoc(state, "game-over", true), "Thanks for playing!"]
  end ?? [state, "I don't understand that command. Type 'help' for a list of commands."];

  result
};

// Game loop
let game-loop = (state) -> {
  alert!(describe-location(state) ++ "\\nWhat do you do? ");

  let input = read-line!();
  let command_result = parse-command(state, input);
  let new-state = first(command_result);
  let message = second(command_result);

  alert!("\\n" ++ message ++ "\\n");

  if new-state.game-over then {
    alert!("\\nGame over! You made " ++ str(new-state.moves) ++ " moves.");
    new-state
  } else {
    game-loop(new-state)
  }
};

// Start game
let start-game = () -> {
  alert!("=== Lits Adventure Game ===\\n" ++ "Type 'help' for a list of commands.\\n\\n");
  game-loop(initial-state)
};

// Call the function to start the game
start-game()    
    `.trim(),
  },
  {
    id: 'determinant',
    name: 'Determinant',
    description: 'Determinant function for square matrices.',
    code: `
// Determinant function for square matrices
let determinant = (matrix) -> {
  // Check if input is an array
  unless array?(matrix) then {
    throw("Input must be an array");
  };

  // Check if matrix is empty
  if empty?(matrix) then {
    throw("Matrix cannot be empty");
  };

  let rows = count(matrix);
  
  // Get first row to check column count
  let firstRow = first(matrix);
  
  // Check if first row is an array
  unless array?(firstRow) then {
    throw("Input must be a 2D array");
  };
  
  let cols = count(firstRow);
  
  // Ensure matrix is square
  if rows ≠ cols then {
    throw("Matrix must be square");
  };
  
  // Base case: 1x1 matrix
  if rows == 1 then {
    matrix[0][0];
  } else {
    // Base case: 2x2 matrix
    if rows == 2 then {
      let a = matrix[0][0];
      let b = matrix[0][1];
      let c = matrix[1][0];
      let d = matrix[1][1];
      
      a * d - b * c;
    } else {
      // For larger matrices, use cofactor expansion along first row
      // Use reduce to calculate the determinant without mutating variables
      reduce(
        range(cols),
        (acc, j) -> {
          let minor = getMinor(matrix, 0, j);
          let cofactor = determinant(minor);
          let signFactor = even?(j) ? 1 : -1;
          let term = signFactor * matrix[0][j] * cofactor;
          
          acc + term;
        },
        0,
      );
    }
  }
};

// Helper function to get minor (submatrix) by removing specific row and column
let getMinor = (matrix, rowToRemove, colToRemove) -> {
  // Use map with filter to create the new matrix without mutating
  map(
    range(count(matrix)),
    i -> {
      if i == rowToRemove then {
        null; // This will be filtered out
      } else {
        let row = get(matrix, i);
        // Filter out the column to remove
        map(
          range(count(row)),
          j -> {
            if j == colToRemove then {
              null; // This will be filtered out
            } else {
              get(row, j)
            }
          }
        ) filter (item -> item ≠ null);
      }
    }
  ) filter (row -> row ≠ null);
};
  
// 4x4 invertible matrix
let matrix4x4 = [
  [4,  3,  2,  2],
  [0,  1, -3,  3],
  [0, -1,  3,  3],
  [0,  3,  1,  1]
];
determinant(matrix4x4);
    `.trim(),
  },
  {
    id: 'matrix-multiplication',
    name: 'Matrix multiplication',
    description: 'Matrix multiplication with correct syntax.',
    code: `
// Matrix multiplication with correct syntax
let matrixMultiply = (matrixA, matrixB) -> {
  // Check if inputs are arrays
  unless array?(matrixA) then throw("First input must be an array");
  unless array?(matrixB) then throw("Second input must be an array");

  // Check if matrices are not empty
  if empty?(matrixA) || empty?(matrixB) then throw("Matrices cannot be empty");

  // Check if matrices are 2D arrays
  unless array?(first(matrixA)) then throw("First input must be a 2D array");
  unless array?(first(matrixB)) then throw("Second input must be a 2D array");

  // Get dimensions
  let rowsA = count(matrixA);
  let colsA = count(first(matrixA));
  let rowsB = count(matrixB);
  let colsB = count(first(matrixB));

  // Check if all rows have consistent length
  unless every?(matrixA, row -> array?(row) && count(row) == colsA) then {
    throw("First matrix has inconsistent row lengths")
  };
  
  unless every?(matrixB, row -> array?(row) && count(row) == colsB) then {
    throw("Second matrix has inconsistent row lengths")
  };

  // Check if matrices can be multiplied
  unless colsA == rowsB then {
    throw("Matrix dimensions mismatch: first matrix columns must equal second matrix rows");
  };

  // Create a row of the result matrix
  let createRow = (rowIndex) -> {
    for (j in range(colsB)) -> {
      reduce(
        range(colsA),
        (sum, k) -> {
          let aValue = matrixA[rowIndex][k];
          let bValue = matrixB[k][j];
          sum + (aValue * bValue);
        },
        0
      )
    }
  };

  // Create the result matrix row by row
  for (i in range(rowsA)) -> createRow(i);
};

let matrixA = [
  [1, 2, 3],
  [4, 5, 6]
];

let matrixB = [
  [7, 8],
  [9, 10],
  [11, 12]
];

matrixMultiply(matrixA, matrixB);
`.trim(),
  },
  {
    id: 'phone-number-formatter',
    name: 'Phone number formatter',
    description: 'Pretty prints a US phone number.',
    code: `
let formatPhoneNumber = (data) -> {
  if string?(data) then {
    let phoneNumber = data[0] == "+" ? data slice 2 : data;
    let length = count(phoneNumber);

    cond
      case length > 6 then
        "(" ++ slice(phoneNumber, 0, 3) ++ ") " ++ slice(phoneNumber, 3, 6) ++ "-" ++ slice(phoneNumber, 6)
      case length > 3 then
        "(" ++ slice(phoneNumber, 0, 3) ++ ") " ++ slice(phoneNumber, 3)
      case length > 0 then
        "(" ++ slice(phoneNumber, 0)
    end ?? ""
  } else {
    ""
  }
};


write!(formatPhoneNumber);
write!(formatPhoneNumber(123234));
write!(formatPhoneNumber("123234"));
write!(formatPhoneNumber("1232343456"));
write!(formatPhoneNumber("+11232343456789"));
write!(formatPhoneNumber("+11232343456"));
  `.trim(),
  },
  {
    id: 'factorial',
    name: 'Factorial',
    description: 'A recursive implementation of the factorial function.',
    code: `
let factorial = (x) -> {
  if x == 1 then {
    1
  } else {
    x * self(x - 1)
  }
};

factorial(5)
  `.trim(),

  },
  {
    id: 'sort',
    name: 'Sort',
    description: 'Sort an array of numbers.',
    code: `
let l = [7, 39, 45, 0, 23, 1, 50, 100, 12, -5];
let numberComparer = (a, b) -> {
  cond
    case a < b then -1
    case a > b then 1
  end ?? 0
};

sort(l, numberComparer)
      `.trim(),
  },
  {
    id: 'isoDateString',
    name: 'Is ISO date string',
    description: 'Check if string is formatted as an ISO date string.',
    code: `
let isoDateString? = (data) -> {
  let m = data match #"^(\\d{4})-(\\d{2})-(\\d{2})$";

  if m then {
    let [year, month, day] = slice(m, 1) map number;
    let leapYear = zero?(year mod 4) && (!zero?(year mod 100) || zero?(year mod 400));

    let invalid = 
      (year < 1900 || year > 2100)
      || (month < 1 || month > 12)
      || (day < 1 || day > 31)
      || day > 30 && (month == 4 || month == 6 || month == 9 || month == 11)
      || month == 2 && (leapYear && day > 29 || !leapYear && day > 28);

    !(invalid)
  } else {
    false
  }
};

write!(isoDateString?("1978-12-21"));
write!(isoDateString?("197-12-21"));
  `.trim(),
  },

  {
    id: 'label-from-value',
    name: 'label-from-value',
    description: 'Find label to corresponding value in array of { label, value }-objects.',
    code: `
let label-from-value = (items, value) -> {
  let entry = items some (-> value == $["value"]);
  if entry == null then {
    null
  } else {
    entry["label"]
  }
};


let items = [
  { label: "Name", value: "name" },
  { label: "Age", value: "age" }
];

label-from-value(items, "name");
  `.trim(),
  },

  {
    id: 'labels-from-values',
    name: 'labels-from-values',
    description: 'Find labels to corresponding values in array of { label, value }-objects.',
    code: `
let labels-from-values = ($array, $values) -> {
  for (
    value in $values
    let label = {
      let entry = $array some -> value == $["value"];
      if entry == null then {
        value
      } else {
        entry["label"]
      }
    }
  ) -> label
};

let arr = [
  { label: "Name", value: "name" },
  { label: "Age", value: "age" },
  { label: "Email", value: "email" },
];

labels-from-values(arr, ["name", "age"])
`.trim(),
  },
]
