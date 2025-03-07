import { examples } from '../../../reference/examples'
import { lampIcon } from '../icons'
import { styles } from '../styles'

export function getExamplePage(): string {
  return `
  <div id="example-page" class="content">
    <div ${styles('flex', 'justify-center', 'text-3xl')}>Examples</div>
    <div ${styles('flex', 'flex-col', 'gap-4', 'my-4')}>
    ${examples
      .map((example) => {
        const encodedExample = btoa(encodeURIComponent(JSON.stringify(example)))
        return `
          <div
            class='example'
            onclick="Playground.setPlayground('${example.name}', \`${encodedExample}\`)"
            ${styles('select-none', 'text-color-gray-400', 'flex', 'flex-row', 'items-center', 'pr-4', 'bg-gray-700')}
          >
            <div ${styles('font-size: 3.5rem;', 'flex', 'items-center', 'justify-center', 'width: 5rem;')}>${lampIcon}</div>
            <div ${styles('flex', 'flex-col', 'gap-2', 'my-4')}>
              <div ${styles('text-xl', 'text-color-gray-200')}>${example.name}</div>
              <div ${styles('text-base', 'text-color-gray-400')}>${example.description}</div>
            </div>
          </div>
      `
      })
      .join('\n')}
    </div>
  </div>
  `
}
