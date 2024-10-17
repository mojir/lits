import type { CssTemplateFunction, Styles } from '.'

export function getPositionStyles(css: CssTemplateFunction) {
  const positionStyles = {
    static: css`
      position: static;
    `,
    fixed: css`
      position: fixed;
    `,
    absolute: css`
      position: absolute;
    `,
    relative: css`
      position: relative;
    `,
    sticky: css`
      position: sticky;
    `,
  } satisfies Partial<Styles>

  return positionStyles
}
