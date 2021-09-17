/* istanbul ignore file */

// Ignoring file, code coverage not working as expected.
// File                          | % Stmts | % Branch | % Funcs | % Lines | Uncovered Line #s
// errors.ts                     |     100 |       50 |     100 |     100 | 5-16

export class ReturnFromSignal extends Error {
  public blockName: string
  public value: unknown
  constructor(blockName: string, value: unknown) {
    super(`return-from block "${blockName}" with value: ${value}`)
    Object.setPrototypeOf(this, ReturnFromSignal.prototype)
    this.name = 'ReturnFromSignal'
    this.blockName = blockName
    this.value = value
  }
}

export class ReturnSignal extends Error {
  public value: unknown
  constructor(value: unknown) {
    super(`return with value: ${value}`)
    Object.setPrototypeOf(this, ReturnSignal.prototype)
    this.name = 'ReturnSignal'
    this.value = value
  }
}
