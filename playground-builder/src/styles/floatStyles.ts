import type { CssTemplateFunction, Styles } from '.'

export function getFloatStyles(css: CssTemplateFunction) {
  const floatStyles = {
    'float-start': css`
      float: inline-start;
    `,
    'float-end': css`
      float: inline-end;
    `,
    'float-right': css`
      float: right;
    `,
    'float-left': css`
      float: left;
    `,
    'float-none': css`
      float: none;
    `,
  } satisfies Partial<Styles>

  return floatStyles
}
