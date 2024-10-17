import { checkIcon } from '../icons'
import { styles } from '../styles'
import { version } from '../../../package.json'

export function getStartPage(): string {
  return `
  <div id="index" class="content">
    <div ${styles('flex', 'justify-end', 'mr-2')}>
      v${version}
    </div>
    <div ${styles('flex', 'text-center', 'flex-col')}>
      <div ${styles('text-huge')}>&lambda;its</div>
      <div ${styles('text-4xl')}>/lits/</div>
    </div>
    <div ${styles('flex', 'justify-center', 'mt-8', 'mb-4')}>
      <div ${styles('font-sans', 'max-width: 600px;', 'text-color-gray-300', 'flex', 'flex-col')}>
        <div ${styles('mt-4')}>&lambda;its is a Lisp dialect implemented in TypeScript, drawing heavy inspiration from Clojure. Most core functions have been ported to &lambda;its, ensuring a robust and familiar experience for Clojure users.</div>
        <div ${styles('mt-4')}>Key features of &lambda;its include:</div>
        <div ${styles('mt-4', 'ml-8', 'flex', 'flex-col', 'text-base', 'gap-2')}>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Dependencies:</span>
              <span ${styles('text-color-gray-400')}>No third party dependencies.</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Immutability:</span>
              <span ${styles('text-color-gray-400')}>All datatypes in &lambda;its are immutable.</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Pure Functions:</span>
              <span ${styles('text-color-gray-400')}>Functions are <a class="external-link" href="https://en.wikipedia.org/wiki/Pure_function">pure</a> by default. Functions with side effects have names ending in an exclamation mark (!), such as <span ${styles('text-color-Blue')}><a onclick="Playground.showPage('write_exclamation')">write!</a></span> or <span ${styles('text-color-Blue')}><a onclick="Playground.showPage('rand_exclamation')">rand!</a></span>.</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Type Mapping:</span>
              <span ${styles('text-color-gray-400')}>All datatypes in &lambda;its map directly to JavaScript types.</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Evaluation:</span>
              <span ${styles('text-color-gray-400')}>&lambda;its does not support lazy evaluation.</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Macros:</span>
              <span ${styles('text-color-gray-400')}>Macros are not supported in &lambda;its.</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Keyword Symbols:</span>
              <span ${styles('text-color-gray-400')}>There are no keyword symbols. The notation <span ${styles('text-color-Pink')}>:foo</span> is simply shorthand for the string <span ${styles('text-color-Pink')}>"foo"</span>.</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Scoping:</span>
              <span ${styles('text-color-gray-400')}>&lambda;its uses <a class="external-link" href="https://en.wikipedia.org/wiki/Scope_(computer_science)#Dynamic_scope">dynamic scoping</a>, not <a class="external-link" href="https://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scope">lexical scoping</a>.</span>
            </div>
          </div>
        </div>
        <div ${styles('mt-4')}>For more information, visit the <a class="external-link" href="https://github.com/YouCruit/lits">&lambda;its GitHub repository</a></div>
        <div ${styles('mt-4')}>Happy coding!</div>
      </div>
    </div>
  </div>
  `
}
