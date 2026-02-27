import type { Reference } from '../../../../reference'
import { renderExample } from '../../renderExample'

export function getFunctionExamples(reference: Reference) {
  const { examples, title: name } = reference
  return examples
    .map(example => example.trim())
    .map(example => renderExample(example, name))
    .join('\n')
}
