import type { CssTemplateFunction, Styles } from '.'

export function getBorderStyles(css: CssTemplateFunction) {
  const borderStyles = {
    'border-solid': css`
      border-style: solid;
    `,
    'border-dashed': css`
      border-style: dashed;
    `,
    'border-dotted': css`
      border-style: dotted;
    `,
    'border-double': css`
      border-style: double;
    `,
    'border-none': css`
      border-style: none;
    `,
    'border-hidden': css`
      border-style: hidden;
    `,
    'border-0': css`
      border-width: 0;
    `,
    'border-2': css`
      border-width: 2px;
    `,
    'border-3': css`
      border-width: 3px;
    `,
    'border-4': css`
      border-width: 4px;
    `,
    'border-8': css`
      border-width: 8px;
    `,
    'border': css`
      border-width: 1px;
    `,
    'border-t-0': css`
      border-top-width: 0;
    `,
    'border-t-2': css`
      border-top-width: 2px;
    `,
    'border-t-3': css`
      border-top-width: 3px;
    `,
    'border-t-4': css`
      border-top-width: 4px;
    `,
    'border-t-8': css`
      border-top-width: 8px;
    `,
    'border-t': css`
      border-top-width: 1px;
    `,
    'border-x-0': css`
      border-left-width: 0;
      border-right-width: 0;
    `,
    'border-x-2': css`
      border-left-width: 2px;
      border-right-width: 2px;
    `,
    'border-x-3': css`
      border-left-width: 3px;
      border-right-width: 3px;
    `,
    'border-x-4': css`
      border-left-width: 4px;
      border-right-width: 4px;
    `,
    'border-x-8': css`
      border-left-width: 8px;
      border-right-width: 8px;
    `,
    'border-x': css`
      border-left-width: 1px;
      border-right-width: 1px;
    `,
    'border-y-0': css`
      border-top-width: 0;
      border-bottom-width: 0;
    `,
    'border-y-2': css`
      border-top-width: 2px;
      border-bottom-width: 2px;
    `,
    'border-y-3': css`
      border-top-width: 3px;
      border-bottom-width: 3px;
    `,
    'border-y-4': css`
      border-top-width: 4px;
      border-bottom-width: 4px;
    `,
    'border-y-8': css`
      border-top-width: 8px;
      border-bottom-width: 8px;
    `,
    'border-y': css`
      border-top-width: 1px;
      border-bottom-width: 1px;
    `,
    'border-r-0': css`
      border-right-width: 0;
    `,
    'border-r-2': css`
      border-right-width: 2px;
    `,
    'border-r-3': css`
      border-right-width: 3px;
    `,
    'border-r-4': css`
      border-right-width: 4px;
    `,
    'border-r-8': css`
      border-right-width: 8px;
    `,
    'border-r': css`
      border-right-width: 1px;
    `,
    'border-b-0': css`
      border-bottom-width: 0;
    `,
    'border-b-2': css`
      border-bottom-width: 2px;
    `,
    'border-b-3': css`
      border-bottom-width: 3px;
    `,
    'border-b-4': css`
      border-bottom-width: 4px;
    `,
    'border-b-8': css`
      border-bottom-width: 8px;
    `,
    'border-b': css`
      border-bottom-width: 1px;
    `,
    'border-l-0': css`
      border-left-width: 0;
    `,
    'border-l-2': css`
      border-left-width: 2px;
    `,
    'border-l-3': css`
      border-left-width: 3px;
    `,
    'border-l-4': css`
      border-left-width: 4px;
    `,
    'border-l-8': css`
      border-left-width: 8px;
    `,
    'border-l': css`
      border-left-width: 1px;
    `,
  } satisfies Partial<Styles>

  return borderStyles
}
