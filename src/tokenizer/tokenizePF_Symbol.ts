import { postfixIdentifierCharacterClass } from '../identifier'
import { NO_MATCH } from './common/tokenizers'
import type { Tokenizer } from './interface'
import type { PF_SymbolToken } from './Token'

export const PF_symbolRegExp = new RegExp(postfixIdentifierCharacterClass)

export const tokenizePF_Symbol: Tokenizer<PF_SymbolToken> = (input, position) => {
  let char = input[position]
  let length = 0
  let value = ''

  if (!char || !PF_symbolRegExp.test(char))
    return NO_MATCH

  while (char && PF_symbolRegExp.test(char)) {
    value += char
    length += 1
    char = input[position + length]
  }

  return [length, ['PF_Symbol', value]]
}
