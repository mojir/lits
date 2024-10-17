import type { CssTemplateFunction, Styles } from '.'

export function getZStyles(css: CssTemplateFunction) {
  const zStyles = {
    'z-0': css`
      z-index: 0;
    `,
    'z-10': css`
      z-index: 10;
    `,
    'z-20': css`
      z-index: 20;
    `,
    'z-30': css`
      z-index: 30;
    `,
    'z-40': css`
      z-index: 40;
    `,
    'z-50': css`
      z-index: 50;
    `,
    'z-auto': css`
      z-index: auto;
    `,
  } satisfies Partial<Styles>

  return zStyles
}
