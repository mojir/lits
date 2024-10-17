import type { CssTemplateFunction, Styles } from '.'

export function getWhitespaceStyles(css: CssTemplateFunction) {
  const whitespaceStyles = {
    'whitespace-normal': css`
      white-space: normal;
    `,
    'whitespace-nowrap': css`
      white-space: nowrap;
    `,
    'whitespace-pre': css`
      white-space: pre;
    `,
    'whitespace-pre-line': css`
      white-space: pre-line;
    `,
    'whitespace-pre-wrap': css`
      white-space: pre-wrap;
    `,
    'whitespace-break-spaces': css`
      white-space: break-spaces;
    `,
  } satisfies Partial<Styles>
  return whitespaceStyles
}
