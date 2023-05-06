import { ContextEntry } from '../ContextStack/interface'
import { BuiltinFunction } from '../parser/interface'

export type LookUpResult = {
  contextEntry: ContextEntry | null
  builtinFunction: BuiltinFunction | null
  specialExpression: true | null
}
