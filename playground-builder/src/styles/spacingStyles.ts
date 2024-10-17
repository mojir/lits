import type { CssTemplateFunction, Styles } from '.'

export function getSpacingStyles(css: CssTemplateFunction) {
  const textStyles = {
    'm-0': css`
      margin: 0;
    `,
    'm-px': css`
      margin: 1px;
    `,
    'm-0.5': css`
      margin: 0.125rem;
    `,
    'm-1': css`
      margin: 0.25rem;
    `,
    'm-2': css`
      margin: 0.5rem;
    `,
    'm-3': css`
      margin: 0.75rem;
    `,
    'm-4': css`
      margin: 1rem;
    `,
    'm-5': css`
      margin: 1.25rem;
    `,
    'm-6': css`
      margin: 1.5rem;
    `,
    'm-8': css`
      margin: 2rem;
    `,
    'my-0': css`
      margin-top: 0;
      margin-bottom: 0;
    `,
    'my-px': css`
      margin-top: 1px;
      margin-bottom: 1px;
    `,
    'my-0.5': css`
      margin-top: 0.125rem;
      margin-bottom: 0.125rem;
    `,
    'my-1': css`
      margin-top: 0.25rem;
      margin-bottom: 0.25rem;
    `,
    'my-2': css`
      margin-top: 0.5rem;
      margin-bottom: 0.5rem;
    `,
    'my-3': css`
      margin-top: 0.75rem;
      margin-bottom: 0.75rem;
    `,
    'my-4': css`
      margin-top: 1rem;
      margin-bottom: 1rem;
    `,
    'my-5': css`
      margin-top: 1.25rem;
      margin-bottom: 1.25rem;
    `,
    'my-6': css`
      margin-top: 1.5rem;
      margin-bottom: 1.5rem;
    `,
    'my-8': css`
      margin-top: 2rem;
      margin-bottom: 2rem;
    `,
    'mx-0': css`
      margin-left: 0;
      margin-right: 0;
    `,
    'mx-px': css`
      margin-left: 1px;
      margin-right: 1px;
    `,
    'mx-0.5': css`
      margin-left: 0.125rem;
      margin-right: 0.125rem;
    `,
    'mx-1': css`
      margin-left: 0.25rem;
      margin-right: 0.25rem;
    `,
    'mx-2': css`
      margin-left: 0.5rem;
      margin-right: 0.5rem;
    `,
    'mx-3': css`
      margin-left: 0.75rem;
      margin-right: 0.75rem;
    `,
    'mx-4': css`
      margin-left: 1rem;
      margin-right: 1rem;
    `,
    'mx-5': css`
      margin-left: 1.25rem;
      margin-right: 1.25rem;
    `,
    'mx-6': css`
      margin-left: 1.5rem;
      margin-right: 1.5rem;
    `,
    'mx-8': css`
      margin-left: 2rem;
      margin-right: 2rem;
    `,
    'mt-0': css`
      margin-top: 0;
    `,
    'mt-px': css`
      margin-top: 1px;
    `,
    'mt-0.5': css`
      margin-top: 0.125rem;
    `,
    'mt-1': css`
      margin-top: 0.25rem;
    `,
    'mt-2': css`
      margin-top: 0.5rem;
    `,
    'mt-3': css`
      margin-top: 0.75rem;
    `,
    'mt-4': css`
      margin-top: 1rem;
    `,
    'mt-5': css`
      margin-top: 1.25rem;
    `,
    'mt-6': css`
      margin-top: 1.5rem;
    `,
    'mt-8': css`
      margin-top: 2rem;
    `,
    'mr-0': css`
      margin-right: 0;
    `,
    'mr-px': css`
      margin-right: 1px;
    `,
    'mr-0.5': css`
      margin-right: 0.125rem;
    `,
    'mr-1': css`
      margin-right: 0.25rem;
    `,
    'mr-2': css`
      margin-right: 0.5rem;
    `,
    'mr-3': css`
      margin-right: 0.75rem;
    `,
    'mr-4': css`
      margin-right: 1rem;
    `,
    'mr-5': css`
      margin-right: 1.25rem;
    `,
    'mr-6': css`
      margin-right: 1.5rem;
    `,
    'mr-8': css`
      margin-right: 2rem;
    `,
    'mb-0': css`
      margin-bottom: 0;
    `,
    'mb-px': css`
      margin-bottom: 1px;
    `,
    'mb-0.5': css`
      margin-bottom: 0.125rem;
    `,
    'mb-1': css`
      margin-bottom: 0.25rem;
    `,
    'mb-2': css`
      margin-bottom: 0.5rem;
    `,
    'mb-3': css`
      margin-bottom: 0.75rem;
    `,
    'mb-4': css`
      margin-bottom: 1rem;
    `,
    'mb-5': css`
      margin-bottom: 1.25rem;
    `,
    'mb-6': css`
      margin-bottom: 1.5rem;
    `,
    'mb-8': css`
      margin-bottom: 2rem;
    `,
    'ml-0': css`
      margin-left: 0;
    `,
    'ml-px': css`
      margin-left: 1px;
    `,
    'ml-0.5': css`
      margin-left: 0.125rem;
    `,
    'ml-1': css`
      margin-left: 0.25rem;
    `,
    'ml-2': css`
      margin-left: 0.5rem;
    `,
    'ml-3': css`
      margin-left: 0.75rem;
    `,
    'ml-4': css`
      margin-left: 1rem;
    `,
    'ml-5': css`
      margin-left: 1.25rem;
    `,
    'ml-6': css`
      margin-left: 1.5rem;
    `,
    'ml-8': css`
      margin-left: 2rem;
    `,

    'scroll-m-0': css`
      scroll-margin: 0;
    `,
    'scroll-m-px': css`
      scroll-margin: 1px;
    `,
    'scroll-m-0.5': css`
      scroll-margin: 0.125rem;
    `,
    'scroll-m-1': css`
      scroll-margin: 0.25rem;
    `,
    'scroll-m-2': css`
      scroll-margin: 0.5rem;
    `,
    'scroll-m-3': css`
      scroll-margin: 0.75rem;
    `,
    'scroll-m-4': css`
      scroll-margin: 1rem;
    `,
    'scroll-m-5': css`
      scroll-margin: 1.25rem;
    `,
    'scroll-m-6': css`
      scroll-margin: 1.5rem;
    `,
    'scroll-m-8': css`
      scroll-margin: 2rem;
    `,
    'scroll-my-0': css`
      scroll-margin-top: 0;
      scroll-margin-bottom: 0;
    `,
    'scroll-my-px': css`
      scroll-margin-top: 1px;
      scroll-margin-bottom: 1px;
    `,
    'scroll-my-0.5': css`
      scroll-margin-top: 0.125rem;
      scroll-margin-bottom: 0.125rem;
    `,
    'scroll-my-1': css`
      scroll-margin-top: 0.25rem;
      scroll-margin-bottom: 0.25rem;
    `,
    'scroll-my-2': css`
      scroll-margin-top: 0.5rem;
      scroll-margin-bottom: 0.5rem;
    `,
    'scroll-my-3': css`
      scroll-margin-top: 0.75rem;
      scroll-margin-bottom: 0.75rem;
    `,
    'scroll-my-4': css`
      scroll-margin-top: 1rem;
      scroll-margin-bottom: 1rem;
    `,
    'scroll-my-5': css`
      scroll-margin-top: 1.25rem;
      scroll-margin-bottom: 1.25rem;
    `,
    'scroll-my-6': css`
      scroll-margin-top: 1.5rem;
      scroll-margin-bottom: 1.5rem;
    `,
    'scroll-my-8': css`
      scroll-margin-top: 2rem;
      scroll-margin-bottom: 2rem;
    `,
    'scroll-mx-0': css`
      scroll-margin-left: 0;
      scroll-margin-right: 0;
    `,
    'scroll-mx-px': css`
      scroll-margin-left: 1px;
      scroll-margin-right: 1px;
    `,
    'scroll-mx-0.5': css`
      scroll-margin-left: 0.125rem;
      scroll-margin-right: 0.125rem;
    `,
    'scroll-mx-1': css`
      scroll-margin-left: 0.25rem;
      scroll-margin-right: 0.25rem;
    `,
    'scroll-mx-2': css`
      scroll-margin-left: 0.5rem;
      scroll-margin-right: 0.5rem;
    `,
    'scroll-mx-3': css`
      scroll-margin-left: 0.75rem;
      scroll-margin-right: 0.75rem;
    `,
    'scroll-mx-4': css`
      scroll-margin-left: 1rem;
      scroll-margin-right: 1rem;
    `,
    'scroll-mx-5': css`
      scroll-margin-left: 1.25rem;
      scroll-margin-right: 1.25rem;
    `,
    'scroll-mx-6': css`
      scroll-margin-left: 1.5rem;
      scroll-margin-right: 1.5rem;
    `,
    'scroll-mx-8': css`
      scroll-margin-left: 2rem;
      scroll-margin-right: 2rem;
    `,
    'scroll-mt-0': css`
      scroll-margin-top: 0;
    `,
    'scroll-mt-px': css`
      scroll-margin-top: 1px;
    `,
    'scroll-mt-0.5': css`
      scroll-margin-top: 0.125rem;
    `,
    'scroll-mt-1': css`
      scroll-margin-top: 0.25rem;
    `,
    'scroll-mt-2': css`
      scroll-margin-top: 0.5rem;
    `,
    'scroll-mt-3': css`
      scroll-margin-top: 0.75rem;
    `,
    'scroll-mt-4': css`
      scroll-margin-top: 1rem;
    `,
    'scroll-mt-5': css`
      scroll-margin-top: 1.25rem;
    `,
    'scroll-mt-6': css`
      scroll-margin-top: 1.5rem;
    `,
    'scroll-mt-8': css`
      scroll-margin-top: 2rem;
    `,
    'scroll-mr-0': css`
      scroll-margin-right: 0;
    `,
    'scroll-mr-px': css`
      scroll-margin-right: 1px;
    `,
    'scroll-mr-0.5': css`
      scroll-margin-right: 0.125rem;
    `,
    'scroll-mr-1': css`
      scroll-margin-right: 0.25rem;
    `,
    'scroll-mr-2': css`
      scroll-margin-right: 0.5rem;
    `,
    'scroll-mr-3': css`
      scroll-margin-right: 0.75rem;
    `,
    'scroll-mr-4': css`
      scroll-margin-right: 1rem;
    `,
    'scroll-mr-5': css`
      scroll-margin-right: 1.25rem;
    `,
    'scroll-mr-6': css`
      scroll-margin-right: 1.5rem;
    `,
    'scroll-mr-8': css`
      scroll-margin-right: 2rem;
    `,
    'scroll-mb-0': css`
      scroll-margin-bottom: 0;
    `,
    'scroll-mb-px': css`
      scroll-margin-bottom: 1px;
    `,
    'scroll-mb-0.5': css`
      scroll-margin-bottom: 0.125rem;
    `,
    'scroll-mb-1': css`
      scroll-margin-bottom: 0.25rem;
    `,
    'scroll-mb-2': css`
      scroll-margin-bottom: 0.5rem;
    `,
    'scroll-mb-3': css`
      scroll-margin-bottom: 0.75rem;
    `,
    'scroll-mb-4': css`
      scroll-margin-bottom: 1rem;
    `,
    'scroll-mb-5': css`
      scroll-margin-bottom: 1.25rem;
    `,
    'scroll-mb-6': css`
      scroll-margin-bottom: 1.5rem;
    `,
    'scroll-mb-8': css`
      scroll-margin-bottom: 2rem;
    `,
    'scroll-ml-0': css`
      scroll-margin-left: 0;
    `,
    'scroll-ml-px': css`
      scroll-margin-left: 1px;
    `,
    'scroll-ml-0.5': css`
      scroll-margin-left: 0.125rem;
    `,
    'scroll-ml-1': css`
      scroll-margin-left: 0.25rem;
    `,
    'scroll-ml-2': css`
      scroll-margin-left: 0.5rem;
    `,
    'scroll-ml-3': css`
      scroll-margin-left: 0.75rem;
    `,
    'scroll-ml-4': css`
      scroll-margin-left: 1rem;
    `,
    'scroll-ml-5': css`
      scroll-margin-left: 1.25rem;
    `,
    'scroll-ml-6': css`
      scroll-margin-left: 1.5rem;
    `,
    'scroll-ml-8': css`
      scroll-margin-left: 2rem;
    `,

    '-m-px': css`
      margin: -1px;
    `,
    '-m-0.5': css`
      margin: -0.125rem;
    `,
    '-m-1': css`
      margin: -0.25rem;
    `,
    '-m-2': css`
      margin: -0.5rem;
    `,
    '-m-3': css`
      margin: -0.75rem;
    `,
    '-m-4': css`
      margin: -1rem;
    `,
    '-m-5': css`
      margin: -1.25rem;
    `,
    '-m-6': css`
      margin: -1.5rem;
    `,
    '-m-8': css`
      margin: -2rem;
    `,
    '-my-px': css`
      margin-top: -1px;
      margin-bottom: -1px;
    `,
    '-my-0.5': css`
      margin-top: -0.125rem;
      margin-bottom: -0.125rem;
    `,
    '-my-1': css`
      margin-top: -0.25rem;
      margin-bottom: -0.25rem;
    `,
    '-my-2': css`
      margin-top: -0.5rem;
      margin-bottom: -0.5rem;
    `,
    '-my-3': css`
      margin-top: -0.75rem;
      margin-bottom: -0.75rem;
    `,
    '-my-4': css`
      margin-top: -1rem;
      margin-bottom: -1rem;
    `,
    '-my-5': css`
      margin-top: -1.25rem;
      margin-bottom: -1.25rem;
    `,
    '-my-6': css`
      margin-top: -1.5rem;
      margin-bottom: -1.5rem;
    `,
    '-my-8': css`
      margin-top: -2rem;
      margin-bottom: -2rem;
    `,
    '-mx-px': css`
      margin-left: -1px;
      margin-right: -1px;
    `,
    '-mx-0.5': css`
      margin-left: -0.125rem;
      margin-right: -0.125rem;
    `,
    '-mx-1': css`
      margin-left: -0.25rem;
      margin-right: -0.25rem;
    `,
    '-mx-2': css`
      margin-left: -0.5rem;
      margin-right: -0.5rem;
    `,
    '-mx-3': css`
      margin-left: -0.75rem;
      margin-right: -0.75rem;
    `,
    '-mx-4': css`
      margin-left: -1rem;
      margin-right: -1rem;
    `,
    '-mx-5': css`
      margin-left: -1.25rem;
      margin-right: -1.25rem;
    `,
    '-mx-6': css`
      margin-left: -1.5rem;
      margin-right: -1.5rem;
    `,
    '-mx-8': css`
      margin-left: -2rem;
      margin-right: -2rem;
    `,
    '-mt-px': css`
      margin-top: -1px;
    `,
    '-mt-0.5': css`
      margin-top: -0.125rem;
    `,
    '-mt-1': css`
      margin-top: -0.25rem;
    `,
    '-mt-2': css`
      margin-top: -0.5rem;
    `,
    '-mt-3': css`
      margin-top: -0.75rem;
    `,
    '-mt-4': css`
      margin-top: -1rem;
    `,
    '-mt-5': css`
      margin-top: -1.25rem;
    `,
    '-mt-6': css`
      margin-top: -1.5rem;
    `,
    '-mt-8': css`
      margin-top: -2rem;
    `,
    '-mr-px': css`
      margin-right: -1px;
    `,
    '-mr-0.5': css`
      margin-right: -0.125rem;
    `,
    '-mr-1': css`
      margin-right: -0.25rem;
    `,
    '-mr-2': css`
      margin-right: -0.5rem;
    `,
    '-mr-3': css`
      margin-right: -0.75rem;
    `,
    '-mr-4': css`
      margin-right: -1rem;
    `,
    '-mr-5': css`
      margin-right: -1.25rem;
    `,
    '-mr-6': css`
      margin-right: -1.5rem;
    `,
    '-mr-8': css`
      margin-right: -2rem;
    `,
    '-mb-px': css`
      margin-bottom: -1px;
    `,
    '-mb-0.5': css`
      margin-bottom: -0.125rem;
    `,
    '-mb-1': css`
      margin-bottom: -0.25rem;
    `,
    '-mb-2': css`
      margin-bottom: -0.5rem;
    `,
    '-mb-3': css`
      margin-bottom: -0.75rem;
    `,
    '-mb-4': css`
      margin-bottom: -1rem;
    `,
    '-mb-5': css`
      margin-bottom: -1.25rem;
    `,
    '-mb-6': css`
      margin-bottom: -1.5rem;
    `,
    '-mb-8': css`
      margin-bottom: -2rem;
    `,
    '-ml-px': css`
      margin-left: -1px;
    `,
    '-ml-0.5': css`
      margin-left: -0.125rem;
    `,
    '-ml-1': css`
      margin-left: -0.25rem;
    `,
    '-ml-2': css`
      margin-left: -0.5rem;
    `,
    '-ml-3': css`
      margin-left: -0.75rem;
    `,
    '-ml-4': css`
      margin-left: -1rem;
    `,
    '-ml-5': css`
      margin-left: -1.25rem;
    `,
    '-ml-6': css`
      margin-left: -1.5rem;
    `,
    '-ml-8': css`
      margin-left: -2rem;
    `,

    'p-0': css`
      padding: 0;
    `,
    'p-px': css`
      padding: 1px;
    `,
    'p-0.5': css`
      padding: 0.125rem;
    `,
    'p-1': css`
      padding: 0.25rem;
    `,
    'p-2': css`
      padding: 0.5rem;
    `,
    'p-3': css`
      padding: 0.75rem;
    `,
    'p-4': css`
      padding: 1rem;
    `,
    'p-5': css`
      padding: 1.25rem;
    `,
    'p-6': css`
      padding: 1.5rem;
    `,
    'p-8': css`
      padding: 2rem;
    `,
    'py-0': css`
      padding-top: 0;
      padding-bottom: 0;
    `,
    'py-px': css`
      padding-top: 1px;
      padding-bottom: 1px;
    `,
    'py-0.5': css`
      padding-top: 0.125rem;
      padding-bottom: 0.125rem;
    `,
    'py-1': css`
      padding-top: 0.25rem;
      padding-bottom: 0.25rem;
    `,
    'py-2': css`
      padding-top: 0.5rem;
      padding-bottom: 0.5rem;
    `,
    'py-3': css`
      padding-top: 0.75rem;
      padding-bottom: 0.75rem;
    `,
    'py-4': css`
      padding-top: 1rem;
      padding-bottom: 1rem;
    `,
    'py-5': css`
      padding-top: 1.25rem;
      padding-bottom: 1.25rem;
    `,
    'py-6': css`
      padding-top: 1.5rem;
      padding-bottom: 1.5rem;
    `,
    'py-8': css`
      padding-top: 2rem;
      padding-bottom: 2rem;
    `,
    'px-0': css`
      padding-left: 0;
      padding-right: 0;
    `,
    'px-px': css`
      padding-left: 1px;
      padding-right: 1px;
    `,
    'px-0.5': css`
      padding-left: 0.125rem;
      padding-right: 0.125rem;
    `,
    'px-1': css`
      padding-left: 0.25rem;
      padding-right: 0.25rem;
    `,
    'px-2': css`
      padding-left: 0.5rem;
      padding-right: 0.5rem;
    `,
    'px-3': css`
      padding-left: 0.75rem;
      padding-right: 0.75rem;
    `,
    'px-4': css`
      padding-left: 1rem;
      padding-right: 1rem;
    `,
    'px-5': css`
      padding-left: 1.25rem;
      padding-right: 1.25rem;
    `,
    'px-6': css`
      padding-left: 1.5rem;
      padding-right: 1.5rem;
    `,
    'px-8': css`
      padding-left: 2rem;
      padding-right: 2rem;
    `,
    'pt-0': css`
      padding-top: 0;
    `,
    'pt-px': css`
      padding-top: 1px;
    `,
    'pt-0.5': css`
      padding-top: 0.125rem;
    `,
    'pt-1': css`
      padding-top: 0.25rem;
    `,
    'pt-2': css`
      padding-top: 0.5rem;
    `,
    'pt-3': css`
      padding-top: 0.75rem;
    `,
    'pt-4': css`
      padding-top: 1rem;
    `,
    'pt-5': css`
      padding-top: 1.25rem;
    `,
    'pt-6': css`
      padding-top: 1.5rem;
    `,
    'pt-8': css`
      padding-top: 2rem;
    `,
    'pr-0': css`
      padding-right: 0;
    `,
    'pr-px': css`
      padding-right: 1px;
    `,
    'pr-0.5': css`
      padding-right: 0.125rem;
    `,
    'pr-1': css`
      padding-right: 0.25rem;
    `,
    'pr-2': css`
      padding-right: 0.5rem;
    `,
    'pr-3': css`
      padding-right: 0.75rem;
    `,
    'pr-4': css`
      padding-right: 1rem;
    `,
    'pr-5': css`
      padding-right: 1.25rem;
    `,
    'pr-6': css`
      padding-right: 1.5rem;
    `,
    'pr-8': css`
      padding-right: 2rem;
    `,
    'pb-0': css`
      padding-bottom: 0;
    `,
    'pb-px': css`
      padding-bottom: 1px;
    `,
    'pb-0.5': css`
      padding-bottom: 0.125rem;
    `,
    'pb-1': css`
      padding-bottom: 0.25rem;
    `,
    'pb-2': css`
      padding-bottom: 0.5rem;
    `,
    'pb-3': css`
      padding-bottom: 0.75rem;
    `,
    'pb-4': css`
      padding-bottom: 1rem;
    `,
    'pb-5': css`
      padding-bottom: 1.25rem;
    `,
    'pb-6': css`
      padding-bottom: 1.5rem;
    `,
    'pb-8': css`
      padding-bottom: 2rem;
    `,
    'pl-0': css`
      padding-left: 0;
    `,
    'pl-px': css`
      padding-left: 1px;
    `,
    'pl-0.5': css`
      padding-left: 0.125rem;
    `,
    'pl-1': css`
      padding-left: 0.25rem;
    `,
    'pl-2': css`
      padding-left: 0.5rem;
    `,
    'pl-3': css`
      padding-left: 0.75rem;
    `,
    'pl-4': css`
      padding-left: 1rem;
    `,
    'pl-5': css`
      padding-left: 1.25rem;
    `,
    'pl-6': css`
      padding-left: 1.5rem;
    `,
    'pl-8': css`
      padding-left: 2rem;
    `,

  } satisfies Partial<Styles>
  return textStyles
}
