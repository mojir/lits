import type { CssTemplateFunction, Styles } from '.'

export function getTopRightBottomLeftStyles(css: CssTemplateFunction) {
  const topRightBottomLeftStyles = {
    'top-0': css`
      top: 0;
    `,
    'top-px': css`
      top: 1px;
    `,
    'top-0.5': css`
      top: 0.125rem;
    `,
    'top-1': css`
      top: 0.25rem;
    `,
    'top-2': css`
      top: 0.5rem;
    `,
    'top-3': css`
      top: 0.75rem;
    `,
    'top-4': css`
      top: 1rem;
    `,
    'top-5': css`
      top: 1.25rem;
    `,
    'top-6': css`
      top: 1.5rem;
    `,
    'top-8': css`
      top: 2rem;
    `,
    'top-full': css`
      top: 100%;
    `,

    'right-0': css`
      right: 0;
    `,
    'right-px': css`
      right: 1px;
    `,
    'right-0.5': css`
      right: 0.125rem;
    `,
    'right-1': css`
      right: 0.25rem;
    `,
    'right-2': css`
      right: 0.5rem;
    `,
    'right-3': css`
      right: 0.75rem;
    `,
    'right-4': css`
      right: 1rem;
    `,
    'right-5': css`
      right: 1.25rem;
    `,
    'right-6': css`
      right: 1.5rem;
    `,
    'right-8': css`
      right: 2rem;
    `,
    'right-full': css`
      right: 100%;
    `,

    'bottom-0': css`
      bottom: 0;
    `,
    'bottom-px': css`
      bottom: 1px;
    `,
    'bottom-0.5': css`
      bottom: 0.125rem;
    `,
    'bottom-1': css`
      bottom: 0.25rem;
    `,
    'bottom-2': css`
      bottom: 0.5rem;
    `,
    'bottom-3': css`
      bottom: 0.75rem;
    `,
    'bottom-4': css`
      bottom: 1rem;
    `,
    'bottom-5': css`
      bottom: 1.25rem;
    `,
    'bottom-6': css`
      bottom: 1.5rem;
    `,
    'bottom-8': css`
      bottom: 2rem;
    `,
    'bottom-full': css`
      bottom: 100%;
    `,

    'left-0': css`
      left: 0;
    `,
    'left-px': css`
      left: 1px;
    `,
    'left-0.5': css`
      left: 0.125rem;
    `,
    'left-1': css`
      left: 0.25rem;
    `,
    'left-2': css`
      left: 0.5rem;
    `,
    'left-3': css`
      left: 0.75rem;
    `,
    'left-4': css`
      left: 1rem;
    `,
    'left-5': css`
      left: 1.25rem;
    `,
    'left-6': css`
      left: 1.5rem;
    `,
    'left-8': css`
      left: 2rem;
    `,
    'left-full': css`
      left: 100%;
    `,

  } satisfies Partial<Styles>

  return topRightBottomLeftStyles
}
