type sizes =
  | '-0'
  | '-px'
  | '-0.5'
  | '-1'
  | '-2'
  | '-3'
  | '-4'
  | '-5'
  | '-6'
  | '-8'
  | '-16'

  type borderSizes =
    | ''
    | '-0'
    | '-2'
    | '-3'
    | '-4'
    | '-8'

type standardColors =
  | '-inherit'
  | '-current'
  | '-transparent'
  | '-white'
  | '-gray-50'
  | '-gray-100'
  | '-gray-150'
  | '-gray-200'
  | '-gray-250'
  | '-gray-300'
  | '-gray-350'
  | '-gray-400'
  | '-gray-450'
  | '-gray-500'
  | '-gray-550'
  | '-gray-600'
  | '-gray-650'
  | '-gray-700'
  | '-gray-750'
  | '-gray-800'
  | '-gray-850'
  | '-gray-900'
  | '-gray-950'
  | '-black'

type customColors =
  | '-BrightYellow'
  | '-GoldenYellow'
  | '-Rose'
  | '-Viola'
  | '-Blue'
  | '-Mint'
  | '-Beige'
  | '-Pink'
  | '-SkyLavender'
  | '-Orange'
  | '-Crimson'

export type CssClass =
  | 'flex'
  | 'flex-row'
  | 'flex-col'
  | 'flex-1'
  | 'flex-auto'
  | 'flex-initial'
  | 'flex-none'
  | 'w-full'
  | 'h-full'
  | `gap${sizes}`
  | `top${sizes}`
  | `right${sizes}`
  | `bottom${sizes}`
  | `left${sizes}`
  | 'top-full'
  | 'right-full'
  | 'bottom-full'
  | 'left-full'

  | `m${sizes}`
  | `mx${sizes}`
  | `my${sizes}`
  | `mt${sizes}`
  | `mr${sizes}`
  | `mb${sizes}`
  | `ml${sizes}`

  | `-m${Exclude<sizes, '-0'>}`
  | `-mx${Exclude<sizes, '-0'>}`
  | `-my${Exclude<sizes, '-0'>}`
  | `-mt${Exclude<sizes, '-0'>}`
  | `-mr${Exclude<sizes, '-0'>}`
  | `-mb${Exclude<sizes, '-0'>}`
  | `-ml${Exclude<sizes, '-0'>}`

  | `scroll-m${sizes}`
  | `scroll-mx${sizes}`
  | `scroll-my${sizes}`
  | `scroll-mt${sizes}`
  | `scroll-mr${sizes}`
  | `scroll-mb${sizes}`
  | `scroll-ml${sizes}`

  | `p${sizes}`
  | `px${sizes}`
  | `py${sizes}`
  | `pt${sizes}`
  | `pr${sizes}`
  | `pb${sizes}`
  | `pl${sizes}`

  | 'text-xs'
  | 'text-sm'
  | 'text-base'
  | 'text-lg'
  | 'text-xl'
  | 'text-2xl'
  | 'text-3xl'
  | 'text-4xl'
  | 'text-huge'
  | 'font-bold'
  | 'font-sans'
  | 'font-mono'
  | 'font-serif'
  | 'italic'
  | 'select-none'

  | 'underline'
  | 'overline'
  | 'line-through'
  | 'no-underline'
  | 'truncate'

  | 'border-solid'
  | 'border-dashed'
  | 'border-dotted'
  | 'border-double'
  | 'border-hidden'
  | `border${borderSizes}`
  | `border-x${borderSizes}`
  | `border-y${borderSizes}`
  | `border-t${borderSizes}`
  | `border-r${borderSizes}`
  | `border-b${borderSizes}`
  | `border-l${borderSizes}`
  | 'border-none'

  | 'z-0'
  | 'z-10'
  | 'z-20'
  | 'z-30'
  | 'z-40'
  | 'z-50'
  | 'z-auto'

  | 'static'
  | 'fixed'
  | 'absolute'
  | 'relative'
  | 'sticky'

  | 'block'
  | 'inline-block'
  | 'inline'
  | 'flex'
  | 'inline-flex'
  | 'table'
  | 'inline-table'
  | 'table-caption'
  | 'table-cell'
  | 'table-column'
  | 'table-column-group'
  | 'table-footer-group'
  | 'table-header-group'
  | 'table-row-group'
  | 'table-row'
  | 'flow-root'
  | 'grid'
  | 'inline-grid'
  | 'contents'
  | 'list-item'
  | 'hidden'

  | 'float-start'
  | 'float-end'
  | 'float-right'
  | 'float-left'
  | 'float-none'

  | `text-color${standardColors}`
  | `text-color${customColors}`
  | `bg${standardColors}`
  | `border${standardColors}`

  | 'justify-normal'
  | 'justify-start'
  | 'justify-end'
  | 'justify-center'
  | 'justify-between'
  | 'justify-around'
  | 'justify-evenly'
  | 'justify-stretch'
  | 'justify-items-start'
  | 'justify-items-end'
  | 'justify-items-center'
  | 'justify-items-stretch'
  | 'justify-self-auto'
  | 'justify-self-start'
  | 'justify-self-end'
  | 'justify-self-center'
  | 'justify-self-stretch'

  | 'content-normal'
  | 'content-center'
  | 'content-start'
  | 'content-end'
  | 'content-between'
  | 'content-around'
  | 'content-evenly'
  | 'content-baseline'
  | 'content-stretch'

  | 'items-start'
  | 'items-end'
  | 'items-center'
  | 'items-baseline'
  | 'items-stretch'

  | 'self-auto'
  | 'self-start'
  | 'self-end'
  | 'self-center'
  | 'self-stretch'
  | 'self-baseline'

  | 'align-baseline'
  | 'align-top'
  | 'align-middle'
  | 'align-bottom'
  | 'align-text-top'
  | 'align-text-bottom'
  | 'align-sub'
  | 'align-super'

  | 'text-left'
  | 'text-center'
  | 'text-right'
  | 'text-justify'
  | 'text-start'
  | 'text-end'

  | 'cursor-auto'
  | 'cursor-default'
  | 'cursor-pointer'
  | 'cursor-wait'
  | 'cursor-text'
  | 'cursor-move'
  | 'cursor-help'
  | 'cursor-not-allowed'
  | 'cursor-row-resize'
  | 'cursor-col-resize'

  | 'whitespace-normal'
  | 'whitespace-nowrap'
  | 'whitespace-pre'
  | 'whitespace-pre-line'
  | 'whitespace-pre-wrap'
  | 'whitespace-break-spaces'
