import { hamburgerIcon, labIcon, linkIcon, playIcon, plusIcon, resetIcon, tokenIcon, trashIcon, treeIcon } from '../icons'
import { createStyles, css } from '../styles'

const styles = createStyles({
  PanelHeader: css`
    @apply px-2;
    @apply w-full;
    @apply text-color-gray-400;
    @apply bg-gray-800;
    @apply justify-between;
    @apply flex;
    @apply flex-row;
    @apply items-center;
    @apply border-0;
    @apply border-b;
    @apply border-solid;
    @apply border-gray-600;
    height: 1.6rem;
    user-select: none;
  `,
})
export function getPlayground() {
  return `
  <div id="playground" ${styles(
    'fixed',
    'bottom-0',
    'left-0',
    'right-0',
    'bg-gray-800',
    'bg-transparent',
  )}>
    <div id="resize-playground" ${styles('height: 5px;', 'bg-gray-600', 'cursor-row-resize')}></div>
    <div id="panels-container" ${styles('h-full', 'w-full', 'flex', 'flex-row', 'whitespace-nowrap')}>
      <div id="context-panel" ${styles('h-full')}>
        <div ${styles('PanelHeader')}>
          <div ${styles('text-lg', 'font-sans')}>Context</div>
          <div id="context-links" ${styles('h-full', 'text-color-gray-400', 'bg-gray-800')}>
            <div ${styles('flex', 'flex-row', 'gap-1', 'text-sm', 'text-color-gray-400', 'h-full', 'items-center')}>
              <a onclick="Playground.addParam()" ${styles('text-xl', 'flex', 'items-center')}>${plusIcon}</a>
              <a onclick="Playground.resetContext()" ${styles('text-xl', 'flex', 'items-center')}>${trashIcon}</a>
            </div>
          </div>
        </div>
        <textarea ${styles('height: calc(100% - 32px);', 'border-0', 'pb-1')} id="context-textarea" class="fancy-scroll" spellcheck="false"></textarea>
      </div
  
      ><div id="resize-divider-1" ${styles('width: 5px;', 'h-full', 'cursor-col-resize', 'bg-gray-600')}></div
  
      ><div id="lits-panel" ${styles('h-full')}>
        <div ${styles('PanelHeader')}>
          <div ${styles('text-lg', 'font-sans')}>Lits Code</div>
          <div
            id="lits-links"
            onclick="event.preventDefault(); event.stopPropagation()"
            ${styles('text-color-gray-400', 'bg-gray-800', 'h-full')}
          >
            <div ${styles('h-full', 'flex', 'flex-row', 'gap-1', 'text-sm', 'text-color-gray-400', 'items-center')}>
              <a onclick="Playground.run()" ${styles('text-xl', 'flex', 'items-center')}>${playIcon}</a>
              <a onclick="Playground.resetLitsCode()" ${styles('text-xl', 'flex', 'items-center')}>${trashIcon}</a>
              <div>
                <a onclick="Playground.toggleMoreMenu()" ${styles('text-xl', 'flex', 'items-center')}>${hamburgerIcon}</a>
                <div id="more-menu" ${styles('hidden', 'max-width: 20rem;', 'absolute', 'p-2', 'border-0', 'border-solid', 'border-gray-300', 'bg-gray-700')}>
                  <div ${styles('flex', 'flex-col', 'gap-2', 'text-base')}>
                    <a ${styles('flex', 'justify-between', 'w-full', 'items-center')} onclick="Playground.closeMoreMenu(); Playground.run()">
                      <div ${styles('flex', 'gap-2', 'w-full', 'items-center')}>
                        <span ${styles('text-color-SkyLavender', 'items-center', 'flex')}>${playIcon}</span>
                        <span ${styles('mr-8')}>Run</span>
                      </div>
                      F5
                    </a>
                    <a ${styles('flex', 'gap-2', 'w-full', 'items-center')} onclick="Playground.closeMoreMenu(); Playground.analyze()">
                      <span ${styles('text-color-Blue', 'items-center', 'flex')}>${labIcon}</span>
                      <span ${styles('mr-8')}>Analyze</span>
                    </a>
                    <a ${styles('flex', 'gap-2', 'w-full', 'items-center')} onclick="Playground.closeMoreMenu(); Playground.tokenize(false)">
                      <span ${styles('text-color-Mint', 'items-center', 'flex')}>${tokenIcon}</span>
                      <span ${styles('mr-8')}>Tokenize</span>
                    </a>
                    <a ${styles('flex', 'gap-2', 'w-full', 'items-center')} onclick="Playground.closeMoreMenu(); Playground.parse(false)">
                      <span ${styles('text-color-Viola', 'items-center', 'flex')}>${treeIcon}</span>
                      <span ${styles('mr-8')}>Parse</span>
                    </a>
                    <a ${styles('flex', 'gap-2', 'w-full', 'items-center', 'pt-2', 'border-0', 'border-t', 'border-solid', 'border-gray-500')} onclick="Playground.closeMoreMenu(); Playground.share();">
                      <span ${styles('text-color-Pink', 'items-center', 'flex')}>${linkIcon}</span>
                      <span>Share</span>
                    </a>
                    <a ${styles('flex', 'gap-2', 'w-full', 'items-center', 'pt-2', 'border-0', 'border-t', 'border-solid', 'border-gray-500')} onclick="Playground.closeMoreMenu(); Playground.resetPlayground();">
                      <span ${styles('text-color-Rose', 'items-center', 'flex')}>${resetIcon}</span>
                      <span>Reset Playground</span>
                    </a>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
        <textarea ${styles('height: calc(100% - 32px);', 'border-0')} id="lits-textarea" class="fancy-scroll" spellcheck="false"></textarea>
      </div
  
      ><div id="resize-divider-2" ${styles('width: 5px;', 'h-full', 'cursor-col-resize', 'bg-gray-600', 'h-full')}></div
  
      ><div id="output-panel" ${styles('h-full')}>
        <div ${styles('PanelHeader')}>
          <div ${styles('text-lg', 'font-sans')}>Output</div>
          <div
            id="output-links"
            onclick="event => event.preventDefault()"
          >
            <div ${styles('flex', 'flex-row', 'gap-2', 'text-sm', 'text-color-gray-400')}>
              <a onclick="Playground.resetOutput()" ${styles('text-xl', 'flex', 'items-center')}>${trashIcon}</a>
            </div>
          </div>
        </div>
        <div class="fancy-scroll" ${styles('font-mono', 'bg-gray-850', 'p-2', 'text-sm', 'flex', 'flex-col', 'gap-2', 'height: calc(100% - 32px);', 'overflow-y: auto;')} id="output-result"></div>
      </div>
    </div>
  </div>
  `
}
