import { Context } from '../ContextStack/interface'
import { Obj } from '../interface'

export type LocationGetter = (line: number, col: number) => string

export type LitsParams = {
  contexts?: Context[]
  globals?: Obj
  globalContext?: Context
  getLocation?: LocationGetter
}
