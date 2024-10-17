import { createStyles, css } from '../../styles'
import { Color } from '../../styles/colorStyles'

const styles = createStyles({
  Wrapper: css`
    display: flex;
    justify-content: center;
    align-items: top;
    padding-top: 80px;
    max-height: calc(100% - 80px);
  `,

  SearchResult: css`
    @apply hidden;
    @apply flex-col;  
    @apply gap-4;
    @apply w-full;
    @apply px-4;
    @apply py-3;
    @apply text-xl;
    @apply text-color-gray-500;
    @apply h-full;
    overflow-y: auto;
  `,
  Dialog: css`
    @apply text-color-gray-500;
    @apply bg-gray-700;
    @apply border-8;
    @apply border-gray-600;
    @apply border-solid;
    border-radius: 4px;
    width: 800px;
  `,
  SearchIntro: css`
    flex: 0;
    @apply self-center;
    @apply text-xl;
    @apply h-full;
    @apply text-color-gray-400;
  `,
  NoResult: css`
    @apply hidden;
    @apply self-center;
    @apply m-8;
    @apply text-xl;
    @apply h-full;
    @apply text-color-gray-400;
  `,
})

export function getSearchDialog() {
  return `
    <style>
      .search-entry {
        box-shadow: 5px 5px 10px ${Color.Gray_850};
        background-color: ${Color.Gray_800};
      }
      .search-entry:hover {
        background-color: ${Color.Gray_750};
        box-shadow: none;
        outline: 1px solid ${Color.Gray_600};
      }
      .search-entry.selected {
        outline: 2px solid ${Color.Gray_400};
        box-shadow: none;
      }
    </style>
    <div id="search-dialog-overlay" class="dialog-overlay">
    
      <div ${styles('Wrapper')}>
        <div id="search-dialog" ${styles('Dialog')}>
          <div ${styles('flex', 'flex-col', 'gap-4', 'py-4', 'h-full')}>
            <div id="search-intro" ${styles('SearchIntro')}>
              Search for functions and special expressions
            </div>
            <div ${styles('flex', 'justify-center')}>
              <form autocomplete="off">
                <input placeholder="Search" id="search-input" ${styles('w-full', 'px-3', 'py-2')}/>
              </form>
            </div>
            <div id="no-search-result" ${styles('NoResult')}>
              Nothing found
            </div>
            <div id="search-result" ${styles('SearchResult')} class="fancy-scroll-invert">
            </div>
          </div>
        </div>
      </div>
    </div>
    `
}
