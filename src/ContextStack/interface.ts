import { Any } from '../interface'

export type ContextEntry = { value: Any }
export type Context = Record<string, ContextEntry>
