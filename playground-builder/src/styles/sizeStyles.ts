import type { CssTemplateFunction, Styles } from '.'

export function getSizeStyles(css: CssTemplateFunction) {
  const sizeStyles = {
    'w-full': css`
      width: 100%;
    `,
    'h-full': css`
      height: 100%;
    `,
  } satisfies Partial<Styles>

  return sizeStyles
}
