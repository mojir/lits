import { Any } from '../interface'
import { toAny } from '../utils'

const delimiterRegExp = /[[.]/
export function getPath(obj: Any, path: string): Any {
  const destructedPath = destructPath(path)
  for (const part of destructedPath) {
    try {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      obj = toAny((obj as any)[part])
    } catch {
      return null
    }
  }

  return obj
}

export function destructPath(path: string): Array<string | number> {
  if (!path) {
    return []
  }
  const match = delimiterRegExp.exec(path)
  if (!match) {
    return [path]
  }
  if (match.index > 0) {
    return [path.substring(0, match.index), ...destructPath(path.substring(match.index))]
  }
  if (path[0] === `.`) {
    if (path.length < 2) {
      throw Error(`Ill formed path: ${path}`)
    }
    return destructPath(path.substring(1))
  }

  const [length, value] = parseBracketNotation(path)
  if (path.length > length && path[length] !== `.` && path[length] !== `[`) {
    throw Error(`Ill formed path: ${path}`)
  }
  return [value, ...destructPath(path.substring(length))]
}

const singleQuoteBracketStringRegExp = /^\[\s*'(.*)'\s*\]/
const doubleQuoteBracketStringRegExp = /^\[\s*"(.*)"\s*\]/
const numberBracketStringRegExp = /^\[\s*(\d+)\s*\]/

export function parseBracketNotation(path: string): [number, string | number] {
  const stringMatch = singleQuoteBracketStringRegExp.exec(path) || doubleQuoteBracketStringRegExp.exec(path)
  if (stringMatch) {
    const length = (stringMatch[0] as string).length
    const value = stringMatch[1] as string
    return [length, value]
  }

  const numberMatch = numberBracketStringRegExp.exec(path)
  if (numberMatch) {
    const length = (numberMatch[0] as string).length
    const value = Number(numberMatch[1] as string)
    return [length, value]
  }

  throw Error(`Ill formed path: ${path}`)
}
