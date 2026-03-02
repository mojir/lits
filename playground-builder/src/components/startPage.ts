import { checkIcon, githubIcon, labIcon, lampIcon } from '../icons'
import { styles } from '../styles'
import { version } from '../../../package.json'
import { tutorials } from './tutorials'

export function getStartPage(): string {
  return `
  <div id="index" class="content">
    <div ${styles('flex', 'justify-end', 'mr-2')}>
      v${version}
    </div>
    <div ${styles('flex', 'text-center', 'flex-col', 'items-center')}>
      <div ${styles('text-huge')}>Lits</div>
      <div ${styles('flex', 'gap-16', 'text-2xl')}>
        <a class="external-links" ${styles('flex', 'items-center', 'gap-2')} onclick="Playground.showPage('example-page', 'smooth')" title="Examples">${labIcon}<span ${styles('text-base')}>Examples</span></a>
        <a class="external-links" ${styles('flex', 'items-center', 'gap-2')} onclick="Playground.showPage('${tutorials[0]!.id}', 'smooth')" title="Tutorials">${lampIcon}<span ${styles('text-base')}>Tutorials</span></a>
        <a class="external-links" ${styles('flex', 'items-center', 'gap-2')} href="https://github.com/mojir/lits" target="_blank" title="GitHub">${githubIcon}<span ${styles('text-base')}>GitHub</span></a>
      </div>
    </div>
    <div ${styles('flex', 'justify-center', 'mt-6', 'mb-4')}>
      <div ${styles('font-sans', 'max-width: 600px;', 'text-color-gray-300', 'flex', 'flex-col')}>
        <div ${styles('mt-4')}>Lits is a sandboxed, suspendable virtual machine that embeds in any JavaScript runtime. With algebraic effects and serializable continuations, execution state becomes a JSON blob — pause anywhere, resume across processes and time, and debug backward through history.</div>
        <div ${styles('mt-4')}>Features</div>
        <div ${styles('mt-4', 'ml-6', 'flex', 'flex-col', 'text-base', 'gap-2')}>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Algebraic effects</span>
              <span ${styles('text-color-gray-400')}> - perform is the only IO boundary; host-side handlers decide whether to resume, await, or suspend the VM</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Serializable continuations</span>
              <span ${styles('text-color-gray-400')}> - When a handler suspends, the entire execution state freezes to a JSON blob, resumable across processes, machines, and time</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Time-travel debugging</span>
              <span ${styles('text-color-gray-400')}> - Step backward through any execution, jump to any captured state, and explore alternate timelines</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Parallel &amp; race</span>
              <span ${styles('text-color-gray-400')}> - Run effects concurrently; parallel waits for all, race takes the first to complete</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Sandboxed execution</span>
              <span ${styles('text-color-gray-400')}> - No file system, network, or global access — safe for user-supplied code</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>Pure functional</span>
              <span ${styles('text-color-gray-400')}> - Immutable data and no side effects, ensuring predictable behavior and easier reasoning about code</span>
            </div>
          </div>
          <div ${styles('flex', 'flex-row', 'gap-4', 'items-start')}>
            <div ${styles('mt-px')}>${checkIcon}</div>
            <div>
              <span ${styles('font-bold')}>JavaScript interoperability</span>
              <span ${styles('text-color-gray-400')}> - JavaScript values and functions can easily be exposed in Lits</span>
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
              <span ${styles('font-bold')}>Comprehensive standard library</span>
              <span ${styles('text-color-gray-400')}> - Rich set of functions for collections, math, strings, regular expressions, and more</span>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
  `
}
