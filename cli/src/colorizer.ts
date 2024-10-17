export type Colorizer = ReturnType<typeof createColorizer>

export enum ColorEnum {
  Reset = '\x1B[0m',
  Bright = '\x1B[1m',
  ResetBright = '\x1B[21m',
  Dim = '\x1B[2m',
  ResetDim = '\x1B[22m',
  Italic = '\x1B[3m',
  ResetItalic = '\x1B[23m',
  Underscore = '\x1B[4m',
  ResetUnderscore = '\x1B[24m',
  Blink = '\x1B[5m',
  ResetBlink = '\x1B[25m',
  Reverse = '\x1B[7m',
  ResetReverse = '\x1B[27m',
  Hidden = '\x1B[8m',
  ResetHidden = '\x1B[28m',

  FgBlack = '\x1B[30m',
  FgRed = '\x1B[31m',
  FgGreen = '\x1B[32m',
  FgYellow = '\x1B[33m',
  FgBlue = '\x1B[34m',
  FgMagenta = '\x1B[35m',
  FgCyan = '\x1B[36m',
  FgWhite = '\x1B[37m',
  FgGray = '\x1B[90m',

  BgBlack = '\x1B[40m',
  BgRed = '\x1B[41m',
  BgGreen = '\x1B[42m',
  BgYellow = '\x1B[43m',
  BgBlue = '\x1B[44m',
  BgMagenta = '\x1B[45m',
  BgCyan = '\x1B[46m',
  BgWhite = '\x1B[47m',
  BgGray = '\x1B[100m',
}

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

function createFormatter(enableColors: boolean, colors: ColorEnum[], formatters: Formatter[]): {
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
      ? colors.join('') + text + ColorEnum.Reset
      : text
  }

  Object.defineProperty(fn, 'black', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.FgBlack], formatters),
  })
  Object.defineProperty(fn, 'red', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.FgRed], formatters),
  })
  Object.defineProperty(fn, 'green', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.FgGreen], formatters),
  })
  Object.defineProperty(fn, 'yellow', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.FgYellow], formatters),
  })
  Object.defineProperty(fn, 'blue', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.FgBlue], formatters),
  })
  Object.defineProperty(fn, 'magenta', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.FgMagenta], formatters),
  })
  Object.defineProperty(fn, 'cyan', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.FgCyan], formatters),
  })
  Object.defineProperty(fn, 'white', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.FgWhite], formatters),
  })
  Object.defineProperty(fn, 'gray', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.FgGray], formatters),
  })
  Object.defineProperty(fn, 'bgBlack', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.BgBlack], formatters),
  })
  Object.defineProperty(fn, 'bgRed', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.BgRed], formatters),
  })
  Object.defineProperty(fn, 'bgGreen', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.BgGreen], formatters),
  })
  Object.defineProperty(fn, 'bgYellow', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.BgYellow], formatters),
  })
  Object.defineProperty(fn, 'bgBlue', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.BgBlue], formatters),
  })
  Object.defineProperty(fn, 'bgMagenta', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.BgMagenta], formatters),
  })
  Object.defineProperty(fn, 'bgCyan', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.BgCyan], formatters),
  })
  Object.defineProperty(fn, 'bgWhite', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.BgWhite], formatters),
  })
  Object.defineProperty(fn, 'bgGray', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.BgGray], formatters),
  })
  Object.defineProperty(fn, 'reset', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.Reset], formatters),
  })
  Object.defineProperty(fn, 'bright', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.Bright], formatters),
  })
  Object.defineProperty(fn, 'dim', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.Dim], formatters),
  })
  Object.defineProperty(fn, 'italic', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.Dim], formatters),
  })
  Object.defineProperty(fn, 'underscore', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.Underscore], formatters),
  })
  Object.defineProperty(fn, 'blink', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.Blink], formatters),
  })
  Object.defineProperty(fn, 'reverse', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.Reverse], formatters),
  })
  Object.defineProperty(fn, 'hidden', {
    get: () => createFormatter(enableColors, [...colors, ColorEnum.Hidden], formatters),
  })

  fn.leftPad = (length: number, char?: string) => createFormatter(enableColors, colors, [createLeftPad(length, char)])
  fn.rightPad = (length: number, char?: string) => createFormatter(enableColors, colors, [createRightPad(length, char)])
  return fn as ReturnType<typeof createFormatter>
}

export function createColorizer(enableColors: boolean = true) {
  return {
    black: createFormatter(enableColors, [ColorEnum.FgBlack], []),
    red: createFormatter(enableColors, [ColorEnum.FgRed], []),
    green: createFormatter(enableColors, [ColorEnum.FgGreen], []),
    yellow: createFormatter(enableColors, [ColorEnum.FgYellow], []),
    blue: createFormatter(enableColors, [ColorEnum.FgBlue], []),
    magenta: createFormatter(enableColors, [ColorEnum.FgMagenta], []),
    cyan: createFormatter(enableColors, [ColorEnum.FgCyan], []),
    white: createFormatter(enableColors, [ColorEnum.FgWhite], []),
    gray: createFormatter(enableColors, [ColorEnum.FgGray], []),

    bgBlack: createFormatter(enableColors, [ColorEnum.BgBlack], []),
    bgRed: createFormatter(enableColors, [ColorEnum.BgRed], []),
    bgGreen: createFormatter(enableColors, [ColorEnum.BgGreen], []),
    bgYellow: createFormatter(enableColors, [ColorEnum.BgYellow], []),
    bgBlue: createFormatter(enableColors, [ColorEnum.BgBlue], []),
    bgMagenta: createFormatter(enableColors, [ColorEnum.BgMagenta], []),
    bgCyan: createFormatter(enableColors, [ColorEnum.BgCyan], []),
    bgWhite: createFormatter(enableColors, [ColorEnum.BgWhite], []),
    bgGray: createFormatter(enableColors, [ColorEnum.BgGray], []),

    reset: createFormatter(enableColors, [ColorEnum.Reset], []),
    bright: createFormatter(enableColors, [ColorEnum.Bright], []),
    dim: createFormatter(enableColors, [ColorEnum.Dim], []),
    italic: createFormatter(enableColors, [ColorEnum.Italic], []),
    underscore: createFormatter(enableColors, [ColorEnum.Underscore], []),
    blink: createFormatter(enableColors, [ColorEnum.Blink], []),
    reverse: createFormatter(enableColors, [ColorEnum.Reverse], []),
    hidden: createFormatter(enableColors, [ColorEnum.Hidden], []),

    leftPad: (length: number, char?: string) => createFormatter(enableColors, [], [createLeftPad(length, char)]),
    rightPad: (length: number, char?: string) => createFormatter(enableColors, [], [createRightPad(length, char)]),
  }
}
