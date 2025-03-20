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
      <div ${styles('text-huge')}>Lits</div>
    </div>
    <div ${styles('flex', 'justify-center', 'mt-8', 'mb-4')}>
      <div ${styles('font-sans', 'max-width: 600px;', 'text-color-gray-300', 'flex', 'flex-col')}>
        <div ${styles('mt-4')}>Lits is a lexically scoped pure functional language with algebraic notation. It combines the power of functional programming with an intuitive, readable syntax.</div>
        <div ${styles('mt-4')}>Features</div>
        <div ${styles('mt-4', 'ml-8', 'flex', 'flex-col', 'text-base', 'gap-2')}>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Pure functional language</span>
              <span ${styles('text-color-gray-400')}> - Variables cannot be changed, ensuring predictable behavior and easier reasoning about code</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>First-class functions</span>
              <span ${styles('text-color-gray-400')}> - Functions are treated as values that can be passed to other functions</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Algebraic notation</span>
              <span ${styles('text-color-gray-400')}> - All operators can be used as functions, and functions that take two parameters can be used as operators</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Clojure-inspired functions</span>
              <span ${styles('text-color-gray-400')}> - Most core functions are inspired by Clojure</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Comprehensive standard library</span>
              <span ${styles('text-color-gray-400')}> - Rich set of functions for collections, math, strings, and more</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Structural equality</span>
              <span ${styles('text-color-gray-400')}> - Objects are compared by value, not by reference</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Destructuring</span>
              <span ${styles('text-color-gray-400')}> - Extract values from complex data structures with ease</span>
            </div>
          </div>
        </div>
        <div ${styles('mt-4')}>For more information, visit the <a class="external-link" href="https://github.com/mojir/lits">Lits GitHub repository</a></div>
        <div ${styles('mt-4')}>Happy coding!</div>
      </div>
    </div>
  </div>
  `
}
