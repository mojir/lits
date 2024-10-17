import type { CssTemplateFunction, Styles } from '.'

export function getDisplayStyles(css: CssTemplateFunction) {
  const displayStyles = {
    'block': css`
      display: block;
    `,
    'inline-block': css`
      display: inline-block;
    `,
    'inline': css`
      display: inline;
    `,
    'flex': css`
      display: flex;
    `,
    'inline-flex': css`
      display: inline-flex;
    `,
    'table': css`
      display: table;
    `,
    'inline-table': css`
      display: inline-table;
    `,
    'table-caption': css`
      display: table-caption;
    `,
    'table-cell': css`
      display: table-cell;
    `,
    'table-column': css`
      display: table-column;
    `,
    'table-column-group': css`
      display: table-column-group;
    `,
    'table-footer-group': css`
      display: table-footer-group;
    `,
    'table-header-group': css`
      display: table-header-group;
    `,
    'table-row-group': css`
      display: table-row-group;
    `,
    'table-row': css`
      display: table-row;
    `,
    'flow-root': css`
      display: flow-root;
    `,
    'grid': css`
      display: grid;
    `,
    'inline-grid': css`
      display: inline-grid;
    `,
    'contents': css`
      display: contents;
    `,
    'list-item': css`
      display: list-item;
    `,
    'hidden': css`
      display: none;
    `,
  } satisfies Partial<Styles>

  return displayStyles
}
