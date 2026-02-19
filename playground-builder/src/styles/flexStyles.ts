import type { CssTemplateFunction, Styles } from '.'

export function getFlexStyles(css: CssTemplateFunction) {
  const flexStyles = {
    'flex': css`
      display: flex;
    `,
    'flex-row': css`
      flex-direction: row;
    `,
    'flex-col': css`
      flex-direction: column;
    `,
    'flex-1': css`
      flex: 1 1 0%;
    `,
    'flex-auto': css`
      flex: 1 1 auto;
    `,
    'flex-initial': css`
      flex: 0 1 auto;
    `,
    'flex-none': css`
      flex: none;
    `,
    'gap-0': css`
      gap: 0;
    `,
    'gap-px': css`
      gap: 1px;
    `,
    'gap-0.5': css`
      gap: 0.125rem;
    `,
    'gap-1': css`
      gap: 0.25rem;
    `,
    'gap-2': css`
      gap: 0.5rem;
    `,
    'gap-3': css`
      gap: 0.75rem;
    `,
    'gap-4': css`
      gap: 1rem;
    `,
    'gap-5': css`
      gap: 1.25rem;
    `,
    'gap-6': css`
      gap: 1.5rem;
    `,
    'gap-8': css`
      gap: 2rem;
    `,
    'gap-16': css`
      gap: 4rem;
    `,
  } satisfies Partial<Styles>

  return flexStyles
}
