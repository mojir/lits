import { type StylesParam, createStyles, css } from '../../styles'

const styles = createStyles({
  SubHeader: css`
    @apply text-color-gray-200;
    @apply text-base;
    @apply font-bold;
    @apply mb-2;
  `,
})

export function getSection(name: string, content: string, ...classes: StylesParam[]): string {
  if (!content)
    return ''

  return `
    <div ${styles('mb-6', ...classes)}>
      <div ${styles('SubHeader')}>${name}</div>
      ${content}
    </div>`
}
