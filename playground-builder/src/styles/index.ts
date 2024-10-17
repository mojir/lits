import type { CssClass } from './classes'
import { getColorStyles } from './colorStyles'
import { getSpacingStyles } from './spacingStyles'
import { getTextStyles } from './textStyles'
import { getFlexStyles } from './flexStyles'
import { getAlignAndJustifyStyles } from './alignAndJustifyStyles'
import { getWhitespaceStyles } from './whitespaceStyles'
import { getCursorStyles } from './cursorStyles'
import { getBorderStyles } from './borderStyles'
import { getSizeStyles } from './sizeStyles'
import { getZStyles } from './zStyles'
import { getPositionStyles } from './positionStyles'
import { getDisplayStyles } from './displayStyles'
import { getFloatStyles } from './floatStyles'
import { getTopRightBottomLeftStyles } from './topRightBottomLeftStyles'

export type Styles = Record<CssClass, string[]>
export type CssTemplateFunction = ReturnType<typeof createCssTag>
export type StylesParam = CssClass | `${string}: ${string};`

const basicCss = createCssTag()
const basicStyles: Styles = {
  ...getColorStyles(basicCss),
  ...getTextStyles(basicCss),
  ...getSpacingStyles(basicCss),
  ...getFlexStyles(basicCss),
  ...getAlignAndJustifyStyles(basicCss),
  ...getWhitespaceStyles(basicCss),
  ...getCursorStyles(basicCss),
  ...getBorderStyles(basicCss),
  ...getSizeStyles(basicCss),
  ...getZStyles(basicCss),
  ...getPositionStyles(basicCss),
  ...getDisplayStyles(basicCss),
  ...getTopRightBottomLeftStyles(basicCss),
  ...getFloatStyles(basicCss),
}

export const css = createCssTag(basicStyles)

export function createStyles<T extends string>(extraStyles: Record<T, string[]>): (...classes: (StylesParam | T)[]) => string {
  return (...classes: (StylesParam | T)[]): string => {
    return `style="${[...new Set(classes)].flatMap((c) => {
      if (c.includes(':'))
        return c
      else
      return basicStyles[c as CssClass] || extraStyles[c as T]
    }).join(' ')}"`
  }
}

export const styles = createStyles({})

function createCssTag(dependencies: Record<string, string[]> = {}) {
  return (strings: TemplateStringsArray, ...values: string[]): string[] => {
    const rawString = String.raw({ raw: strings }, ...values)
    return rawString
      .split('\n')
      .map(s => s.trim())
      .filter(s => s.length > 0)
      .flatMap((expression) => {
        const applyMatch = expression.match(/^@apply (\S+);$/)
        if (applyMatch) {
          const cssClass = applyMatch[1] as CssClass
          const cssRules = dependencies[cssClass]
          if (!cssRules)
            throw new Error(`Unknown class: ${expression.slice(7)}`)

          return cssRules
        }
        else { return expression }
      })
  }
}
