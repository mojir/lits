import { examples } from '../../../reference/examples'
import { formatLitsExpression } from '../formatter/rules'
import { lampIcon } from '../icons'
import { styles } from '../styles'

export function getExamplePage(): string {
  return `
  <style>
    .example .code.hidden {
      display: none;
    }
    .example .link:hover {
      text-decoration: underline;
      color: #fff;
    }
  </style>
  <div id="example-page" class="content">
    <div ${styles('flex', 'justify-center', 'text-3xl')}>Examples</div>
    <div ${styles('flex', 'flex-col', 'gap-4', 'my-4')}>
    ${examples
      .map((example, index) => {
        const encodedExample = btoa(encodeURIComponent(JSON.stringify(example)))
        return `
          <div
            class='example'
            ${styles('flex', 'flex-col', 'bg-gray-700')}
          >
            <div
              onclick="document.getElementById('${`example-${index}`}').classList.toggle('hidden')"
              ${styles('select-none', 'text-color-gray-400', 'flex', 'flex-row', 'items-center', 'pr-4')}
            >
              <div ${styles('font-size: 3.5rem;', 'flex', 'items-center', 'justify-center', 'width: 5rem;')}>${lampIcon}</div>
              <div ${styles('flex', 'flex-1', 'flex-col', 'gap-2', 'my-4')}>
                <div ${styles('text-xl', 'text-color-gray-200')}>${example.name}</div>
                <div ${styles('text-base', 'text-color-gray-400')}>${example.description}</div>
              </div>
              <div class="link" onclick="Playground.setPlayground('${example.name}', \`${encodedExample}\`)" ${styles('text-color-gray-400', 'cursor-pointer')}>Play!</div>
            </div>
            <div id="${`example-${index}`}" ${styles('mt-1', 'p-4', 'm-1', 'bg-gray-800')} class="hidden code"> 
              <pre>${formatLitsExpression(example.code)}</pre>
            </div>
          </div>
      `
      })
      .join('\n')}
    </div>
  </div>
  `
}
