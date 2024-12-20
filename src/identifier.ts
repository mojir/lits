export type Identifier = string & { __brand: 'Identifier' }

export const identifierCharacterClass = '[\\w@%^?=!$<>+*/:-]'
export const identifierFirstCharacterClass = '[a-zA-Z_@%^?=!$<>+*/-]'

// eslint-disable-next-line regexp/use-ignore-case
export const identifierRegExp = new RegExp(`^${identifierFirstCharacterClass}${identifierCharacterClass}*$`)

export function isIdentifier(value: string): value is Identifier {
  return identifierRegExp.test(value)
}

export function assertIdentifier(value: string): asserts value is Identifier {
  if (!isIdentifier(value)) {
    throw new Error(`Expected '${value}' to be an identifier`)
  }
}
