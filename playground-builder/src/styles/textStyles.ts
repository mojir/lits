import type { CssTemplateFunction, Styles } from '.'

export function getTextStyles(css: CssTemplateFunction) {
  const textStyles = {
    'text-xs': css`
    font-size: 0.75rem;
  `,
    'text-sm': css`
    font-size: 0.875rem;
  `,
    'text-base': css`
    font-size: 1rem;
  `,
    'text-lg': css`
    font-size: 1.125rem;
  `,
    'text-xl': css`
    font-size: 1.25rem;
  `,
    'text-2xl': css`
    font-size: 1.5rem;
  `,
    'text-3xl': css`
    font-size: 1.875rem;
  `,
    'text-4xl': css`
    font-size: 2.25rem;
  `,
    'text-huge': css`
    font-size: 12rem;
  `,
    'font-bold': css`
    font-weight: bold;
  `,
    'italic': css`
    font-style: italic;
  `,
    'font-sans': css`
    font-family: 'Roboto', sans-serif;
  `,
    'font-mono': css`
    font-family: 'Fira Code', monospace;
  `,
    'font-serif': css`
    font-family: 'Merriweather', serif;
  `,
    'underline': css`
    text-decoration: underline;
  `,
    'overline': css`
    text-decoration: overline;
  `,
    'line-through': css`
    text-decoration: line-through;
  `,
    'no-underline': css`
    text-decoration: none;
  `,
    'truncate': css`
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  `,
    'select-none': css`
    user-select: none;
  `,
  } satisfies Partial<Styles>
  return textStyles
}
