import type { Colorizer } from '../colorizer'
import type { Reference } from '../../../reference'

export function getCliTitle(fmt: Colorizer, reference: Reference) {
  return `${fmt.bright.blue(reference.title)} - ${fmt.gray(reference.category)}`
}
