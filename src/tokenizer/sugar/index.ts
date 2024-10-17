import type { TokenStream } from '../interface'
import { applyCollectionAccessors } from './applyCollectionAccessor'

export type SugarFunction = (tokenStream: TokenStream) => TokenStream

export function getSugar(): SugarFunction[] {
  return [applyCollectionAccessors]
}
