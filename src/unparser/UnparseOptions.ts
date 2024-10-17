import type { Unparse } from './unparse'

export class UnparseOptions {
  public readonly indent: string
  constructor(
    public readonly unparse: Unparse,
    public readonly lineLength: number,
    public readonly col = 0,
    public readonly inlined = false,
    public readonly locked = false,
  ) {
    this.indent = ' '.repeat(col)
  }

  public inc(count = 1) {
    return new UnparseOptions(this.unparse, this.lineLength, this.col + count, this.inlined, this.locked)
  }

  public inline() {
    return new UnparseOptions(this.unparse, this.lineLength, this.col, true, this.locked)
  }

  public noInline() {
    return new UnparseOptions(this.unparse, this.lineLength, this.col, false, this.locked)
  }

  public lock() {
    return new UnparseOptions(this.unparse, this.lineLength, this.col, this.inlined, true)
  }

  public assertNotOverflown(value: string): string {
    value.split('\n').forEach((line, index) => {
      const fullLine = (index === 0 && this.inlined) ? this.indent + line : line
      const length = fullLine.length
      if (length > this.lineLength)
        throw new Error(`Line length exceeded ${this.lineLength} chars, value: "${fullLine}" (${fullLine.length} chars)`)
    })

    return value
  }
}
