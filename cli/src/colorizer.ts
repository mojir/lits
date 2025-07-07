/* eslint-disable node/prefer-global/process */
export type Colorizer = ReturnType<typeof createColorizer>

const useColor = !process.env.NO_COLOR

export const Colors = {
  Reset: useColor ? '\x1B[0m' : '',
  Bright: useColor ? '\x1B[1m' : '',
  ResetBright: useColor ? '\x1B[21m' : '',
  Dim: useColor ? '\x1B[2m' : '',
  ResetDim: useColor ? '\x1B[22m' : '',
  Italic: useColor ? '\x1B[3m' : '',
  ResetItalic: useColor ? '\x1B[23m' : '',
  Underscore: useColor ? '\x1B[4m' : '',
  ResetUnderscore: useColor ? '\x1B[24m' : '',
  Blink: useColor ? '\x1B[5m' : '',
  ResetBlink: useColor ? '\x1B[25m' : '',
  Reverse: useColor ? '\x1B[7m' : '',
  ResetReverse: useColor ? '\x1B[27m' : '',
  Hidden: useColor ? '\x1B[8m' : '',
  ResetHidden: useColor ? '\x1B[28m' : '',

  FgBlack: useColor ? '\x1B[30m' : '',
  FgRed: useColor ? '\x1B[31m' : '',
  FgGreen: useColor ? '\x1B[32m' : '',
  FgYellow: useColor ? '\x1B[33m' : '',
  FgBlue: useColor ? '\x1B[34m' : '',
  FgMagenta: useColor ? '\x1B[35m' : '',
  FgCyan: useColor ? '\x1B[36m' : '',
  FgWhite: useColor ? '\x1B[37m' : '',
  FgGray: useColor ? '\x1B[90m' : '',

  BgBlack: useColor ? '\x1B[40m' : '',
  BgRed: useColor ? '\x1B[41m' : '',
  BgGreen: useColor ? '\x1B[42m' : '',
  BgYellow: useColor ? '\x1B[43m' : '',
  BgBlue: useColor ? '\x1B[44m' : '',
  BgMagenta: useColor ? '\x1B[45m' : '',
  BgCyan: useColor ? '\x1B[46m' : '',
  BgWhite: useColor ? '\x1B[47m' : '',
  BgGray: useColor ? '\x1B[100m' : '',
} as const

function createLeftPad(length: number, char: string = ' ') {
  return (text: string) => {
    return text.padStart(length, char)
  }
}

function createRightPad(length: number, char: string = ' ') {
  return (text: string) => {
    return text.padEnd(length, char)
  }
}

type Formatter = (text: string) => string

function createFormatter(enableColors: boolean, colors: typeof Colors[keyof typeof Colors][], formatters: Formatter[]): {
  (text: string): string
  black: ReturnType<typeof createFormatter>
  red: ReturnType<typeof createFormatter>
  green: ReturnType<typeof createFormatter>
  yellow: ReturnType<typeof createFormatter>
  blue: ReturnType<typeof createFormatter>
  magenta: ReturnType<typeof createFormatter>
  cyan: ReturnType<typeof createFormatter>
  white: ReturnType<typeof createFormatter>
  gray: ReturnType<typeof createFormatter>
  bgBlack: ReturnType<typeof createFormatter>
  bgRed: ReturnType<typeof createFormatter>
  bgGreen: ReturnType<typeof createFormatter>
  bgYellow: ReturnType<typeof createFormatter>
  bgBlue: ReturnType<typeof createFormatter>
  bgMagenta: ReturnType<typeof createFormatter>
  bgCyan: ReturnType<typeof createFormatter>
  bgWhite: ReturnType<typeof createFormatter>
  bgGray: ReturnType<typeof createFormatter>
  reset: ReturnType<typeof createFormatter>
  bright: ReturnType<typeof createFormatter>
  dim: ReturnType<typeof createFormatter>
  italic: ReturnType<typeof createFormatter>
  underscore: ReturnType<typeof createFormatter>
  blink: ReturnType<typeof createFormatter>
  reverse: ReturnType<typeof createFormatter>
  hidden: ReturnType<typeof createFormatter>
  leftPad: (length: number, char?: string) => ReturnType<typeof createFormatter>
  rightPad: (length: number, char?: string) => ReturnType<typeof createFormatter>
} {
  const fn = (text: string) => {
    for (const formatter of formatters)
      text = formatter(text)

    return enableColors
      ? colors.join('') + text + Colors.Reset
      : text
  }

  Object.defineProperty(fn, 'black', {
    get: () => createFormatter(enableColors, [...colors, Colors.FgBlack], formatters),
  })
  Object.defineProperty(fn, 'red', {
    get: () => createFormatter(enableColors, [...colors, Colors.FgRed], formatters),
  })
  Object.defineProperty(fn, 'green', {
    get: () => createFormatter(enableColors, [...colors, Colors.FgGreen], formatters),
  })
  Object.defineProperty(fn, 'yellow', {
    get: () => createFormatter(enableColors, [...colors, Colors.FgYellow], formatters),
  })
  Object.defineProperty(fn, 'blue', {
    get: () => createFormatter(enableColors, [...colors, Colors.FgBlue], formatters),
  })
  Object.defineProperty(fn, 'magenta', {
    get: () => createFormatter(enableColors, [...colors, Colors.FgMagenta], formatters),
  })
  Object.defineProperty(fn, 'cyan', {
    get: () => createFormatter(enableColors, [...colors, Colors.FgCyan], formatters),
  })
  Object.defineProperty(fn, 'white', {
    get: () => createFormatter(enableColors, [...colors, Colors.FgWhite], formatters),
  })
  Object.defineProperty(fn, 'gray', {
    get: () => createFormatter(enableColors, [...colors, Colors.FgGray], formatters),
  })
  Object.defineProperty(fn, 'bgBlack', {
    get: () => createFormatter(enableColors, [...colors, Colors.BgBlack], formatters),
  })
  Object.defineProperty(fn, 'bgRed', {
    get: () => createFormatter(enableColors, [...colors, Colors.BgRed], formatters),
  })
  Object.defineProperty(fn, 'bgGreen', {
    get: () => createFormatter(enableColors, [...colors, Colors.BgGreen], formatters),
  })
  Object.defineProperty(fn, 'bgYellow', {
    get: () => createFormatter(enableColors, [...colors, Colors.BgYellow], formatters),
  })
  Object.defineProperty(fn, 'bgBlue', {
    get: () => createFormatter(enableColors, [...colors, Colors.BgBlue], formatters),
  })
  Object.defineProperty(fn, 'bgMagenta', {
    get: () => createFormatter(enableColors, [...colors, Colors.BgMagenta], formatters),
  })
  Object.defineProperty(fn, 'bgCyan', {
    get: () => createFormatter(enableColors, [...colors, Colors.BgCyan], formatters),
  })
  Object.defineProperty(fn, 'bgWhite', {
    get: () => createFormatter(enableColors, [...colors, Colors.BgWhite], formatters),
  })
  Object.defineProperty(fn, 'bgGray', {
    get: () => createFormatter(enableColors, [...colors, Colors.BgGray], formatters),
  })
  Object.defineProperty(fn, 'reset', {
    get: () => createFormatter(enableColors, [...colors, Colors.Reset], formatters),
  })
  Object.defineProperty(fn, 'bright', {
    get: () => createFormatter(enableColors, [...colors, Colors.Bright], formatters),
  })
  Object.defineProperty(fn, 'dim', {
    get: () => createFormatter(enableColors, [...colors, Colors.Dim], formatters),
  })
  Object.defineProperty(fn, 'italic', {
    get: () => createFormatter(enableColors, [...colors, Colors.Dim], formatters),
  })
  Object.defineProperty(fn, 'underscore', {
    get: () => createFormatter(enableColors, [...colors, Colors.Underscore], formatters),
  })
  Object.defineProperty(fn, 'blink', {
    get: () => createFormatter(enableColors, [...colors, Colors.Blink], formatters),
  })
  Object.defineProperty(fn, 'reverse', {
    get: () => createFormatter(enableColors, [...colors, Colors.Reverse], formatters),
  })
  Object.defineProperty(fn, 'hidden', {
    get: () => createFormatter(enableColors, [...colors, Colors.Hidden], formatters),
  })

  fn.leftPad = (length: number, char?: string) => createFormatter(enableColors, colors, [createLeftPad(length, char)])
  fn.rightPad = (length: number, char?: string) => createFormatter(enableColors, colors, [createRightPad(length, char)])
  return fn as ReturnType<typeof createFormatter>
}

export function createColorizer(enableColors: boolean = true) {
  return {
    black: createFormatter(enableColors, [Colors.FgBlack], []),
    red: createFormatter(enableColors, [Colors.FgRed], []),
    green: createFormatter(enableColors, [Colors.FgGreen], []),
    yellow: createFormatter(enableColors, [Colors.FgYellow], []),
    blue: createFormatter(enableColors, [Colors.FgBlue], []),
    magenta: createFormatter(enableColors, [Colors.FgMagenta], []),
    cyan: createFormatter(enableColors, [Colors.FgCyan], []),
    white: createFormatter(enableColors, [Colors.FgWhite], []),
    gray: createFormatter(enableColors, [Colors.FgGray], []),

    bgBlack: createFormatter(enableColors, [Colors.BgBlack], []),
    bgRed: createFormatter(enableColors, [Colors.BgRed], []),
    bgGreen: createFormatter(enableColors, [Colors.BgGreen], []),
    bgYellow: createFormatter(enableColors, [Colors.BgYellow], []),
    bgBlue: createFormatter(enableColors, [Colors.BgBlue], []),
    bgMagenta: createFormatter(enableColors, [Colors.BgMagenta], []),
    bgCyan: createFormatter(enableColors, [Colors.BgCyan], []),
    bgWhite: createFormatter(enableColors, [Colors.BgWhite], []),
    bgGray: createFormatter(enableColors, [Colors.BgGray], []),

    reset: createFormatter(enableColors, [Colors.Reset], []),
    bright: createFormatter(enableColors, [Colors.Bright], []),
    dim: createFormatter(enableColors, [Colors.Dim], []),
    italic: createFormatter(enableColors, [Colors.Italic], []),
    underscore: createFormatter(enableColors, [Colors.Underscore], []),
    blink: createFormatter(enableColors, [Colors.Blink], []),
    reverse: createFormatter(enableColors, [Colors.Reverse], []),
    hidden: createFormatter(enableColors, [Colors.Hidden], []),

    leftPad: (length: number, char?: string) => createFormatter(enableColors, [], [createLeftPad(length, char)]),
    rightPad: (length: number, char?: string) => createFormatter(enableColors, [], [createRightPad(length, char)]),
  }
}
