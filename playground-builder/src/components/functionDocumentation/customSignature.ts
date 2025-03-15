import type { CustomReference } from '../../../../reference'
import { formatLitsExpression } from '../../formatter/rules'

export function getCustomSignature(customVariants: CustomReference['customVariants']) {
  return `<table>
  ${customVariants.map(variant => `
    <tr>
      <td>${formatLitsExpression(variant)}</td>
    </tr>`,
  ).join('')}
  </table>`
}
