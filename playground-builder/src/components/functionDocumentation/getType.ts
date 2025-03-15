import type { Argument, TypedValue } from '../../../../reference'
import { isDataType } from '../../../../reference/api'
import { styles } from '../../styles'

export function getType(arg: Argument | TypedValue) {
  const types = Array.isArray(arg.type) ? arg.type : [arg.type]
  const typeString = types.map((type) => {
    return isDataType(type)
      ? `<span ${styles('text-color-Rose')}><a onclick="Playground.showPage('-type-${type}', 'smooth')">${type}</a></span>`
      : `<span ${styles('text-color-Rose')}>${type}</span>`
  }).join(' | ')
  const result = arg.array || arg.rest
    ? `<span ${styles('text-color-Mint')
    }>Array</span><span ${styles('text-color-gray-200')
    }>&lt;</span>${typeString
    }<span ${styles('text-color-gray-200')
    }>&gt;`
    : typeString

  return `<span ${styles('font-mono', 'text-color-gray-300')}>${result}</span>`
}
