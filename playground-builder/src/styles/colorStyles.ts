import type { CssTemplateFunction, Styles } from '.'

export enum Color {
  GoldenYellow = '#e6c07b',
  BrightYellow = '#f0e68c',
  Viola = '#c586c0',
  Mint = '#4ec9b0',
  Rose = '#d16969',
  Blue = '#569cd6',
  Beige = '#dcdcaa',
  SkyLavender = '#c5cbe3',
  Pink = '#cc8f77',
  Orange = '#ffa500',
  Crimson = '#dc143c',
  White = '#ffffff',
  Gray_50 = 'rgb(250 250 250)',
  Gray_100 = 'rgb(245 245 245)',
  Gray_150 = 'rgb(237 237 237)',
  Gray_200 = 'rgb(229 229 229)',
  Gray_250 = 'rgb(220 220 220)',
  Gray_300 = 'rgb(212 212 212)',
  Gray_350 = 'rgb(187 187 187)',
  Gray_400 = 'rgb(163 163 163)',
  Gray_450 = 'rgb(139 139 139)',
  Gray_500 = 'rgb(115 115 115)',
  Gray_550 = 'rgb(98 98 98)',
  Gray_600 = 'rgb(82 82 82)',
  Gray_650 = 'rgb(73 73 73)',
  Gray_700 = 'rgb(64 64 64)',
  Gray_750 = 'rgb(51 51 51)',
  Gray_800 = 'rgb(38 38 38)',
  Gray_850 = 'rgb(30 30 30)',
  Gray_900 = 'rgb(23 23 23)',
  Gray_950 = 'rgb(10 10 10)',
  Black = '#000000',
}

export function getColorStyles(css: CssTemplateFunction) {
  const colorStyles = {
    'text-color-Viola': css`
      color: ${Color.Viola};
    `,
    'text-color-Rose': css`
      color: ${Color.Rose};
    `,
    'text-color-GoldenYellow': css`
      color: ${Color.GoldenYellow};
    `,
    'text-color-BrightYellow': css`
      color: ${Color.BrightYellow};
    `,
    'text-color-Mint': css`
      color: ${Color.Mint};
    `,
    'text-color-Blue': css`
      color: ${Color.Blue};
    `,
    'text-color-Beige': css`
      color: ${Color.Beige};
    `,
    'text-color-SkyLavender': css`
      color: ${Color.SkyLavender};
    `,
    'text-color-Pink': css`
      color: ${Color.Pink};
    `,
    'text-color-Orange': css`
      color: ${Color.Orange};
    `,
    'text-color-Crimson': css`
      color: ${Color.Crimson};
    `,
    'text-color-inherit': css`
      color: inherit;
    `,
    'text-color-current': css`
      color: currentColor;
    `,
    'text-color-transparent': css`
      color: transparent;
    `,
    'text-color-white': css`
      color: ${Color.White};
    `,
    'text-color-gray-50': css`
      color: ${Color.Gray_50};
    `,
    'text-color-gray-100': css`
      color: ${Color.Gray_100};
    `,
    'text-color-gray-150': css`
      color: ${Color.Gray_150};
    `,
    'text-color-gray-200': css`
      color: ${Color.Gray_200};
    `,
    'text-color-gray-250': css`
      color: ${Color.Gray_250};
    `,
    'text-color-gray-300': css`
      color: ${Color.Gray_300};
    `,
    'text-color-gray-350': css`
      color: ${Color.Gray_350};
    `,
    'text-color-gray-400': css`
      color: ${Color.Gray_400};
    `,
    'text-color-gray-450': css`
      color: ${Color.Gray_450};
    `,
    'text-color-gray-500': css`
      color: ${Color.Gray_500};
    `,
    'text-color-gray-550': css`
      color: ${Color.Gray_550};
    `,
    'text-color-gray-600': css`
      color: ${Color.Gray_600};
    `,
    'text-color-gray-650': css`
      color: ${Color.Gray_650};
    `,
    'text-color-gray-700': css`
      color: ${Color.Gray_700};
    `,
    'text-color-gray-750': css`
      color: ${Color.Gray_750};
    `,
    'text-color-gray-800': css`
      color: ${Color.Gray_800};
    `,
    'text-color-gray-850': css`
      color: ${Color.Gray_850};
    `,
    'text-color-gray-900': css`
      color: ${Color.Gray_900};
    `,
    'text-color-gray-950': css`
      color: ${Color.Gray_950};
    `,
    'text-color-black': css`
      color: ${Color.Black};
    `,
    'bg-inherit': css`
      background-color: inherit;
    `,
    'bg-current': css`
      background-color: currentColor;
    `,
    'bg-transparent': css`
      background-color: transparent;
    `,
    'bg-white': css`
      background-color: ${Color.White};
    `,
    'bg-gray-50': css`
      background-color: ${Color.Gray_50};
    `,
    'bg-gray-100': css`
      background-color: ${Color.Gray_100};
    `,
    'bg-gray-150': css`
      background-color: ${Color.Gray_150};
    `,
    'bg-gray-200': css`
      background-color: ${Color.Gray_200};
    `,
    'bg-gray-250': css`
      background-color: ${Color.Gray_250};
    `,
    'bg-gray-300': css`
      background-color: ${Color.Gray_300};
    `,
    'bg-gray-350': css`
      background-color: ${Color.Gray_350};
    `,
    'bg-gray-400': css`
      background-color: ${Color.Gray_400};
    `,
    'bg-gray-450': css`
      background-color: ${Color.Gray_450};
    `,
    'bg-gray-500': css`
      background-color: ${Color.Gray_500};
    `,
    'bg-gray-550': css`
      background-color: ${Color.Gray_550};
    `,
    'bg-gray-600': css`
      background-color: ${Color.Gray_600};
    `,
    'bg-gray-650': css`
      background-color: ${Color.Gray_650};
    `,
    'bg-gray-700': css`
      background-color: ${Color.Gray_700};
    `,
    'bg-gray-750': css`
      background-color: ${Color.Gray_750};
    `,
    'bg-gray-800': css`
      background-color: ${Color.Gray_800};
    `,
    'bg-gray-850': css`
      background-color: ${Color.Gray_850};
    `,
    'bg-gray-900': css`
      background-color: ${Color.Gray_900};
    `,
    'bg-gray-950': css`
      background-color: ${Color.Gray_950};
    `,
    'bg-black': css`
      background-color: ${Color.Black};
    `,
    'border-inherit': css`
      color: inherit;
    `,
    'border-current': css`
      color: currentColor;
    `,
    'border-transparent': css`
      color: transparent;
    `,
    'border-white': css`
      border-color: ${Color.White};
    `,
    'border-gray-50': css`
      border-color: ${Color.Gray_50};
    `,
    'border-gray-100': css`
      border-color: ${Color.Gray_100};
    `,
    'border-gray-150': css`
      border-color: ${Color.Gray_150};
    `,
    'border-gray-200': css`
      border-color: ${Color.Gray_200};
    `,
    'border-gray-250': css`
      border-color: ${Color.Gray_250};
    `,
    'border-gray-300': css`
      border-color: ${Color.Gray_300};
    `,
    'border-gray-350': css`
      border-color: ${Color.Gray_350};
    `,
    'border-gray-400': css`
      border-color: ${Color.Gray_400};
    `,
    'border-gray-450': css`
      border-color: ${Color.Gray_450};
    `,
    'border-gray-500': css`
      border-color: ${Color.Gray_500};
    `,
    'border-gray-550': css`
      border-color: ${Color.Gray_550};
    `,
    'border-gray-600': css`
      border-color: ${Color.Gray_600};
    `,
    'border-gray-650': css`
      border-color: ${Color.Gray_650};
    `,
    'border-gray-700': css`
      border-color: ${Color.Gray_700};
    `,
    'border-gray-750': css`
      border-color: ${Color.Gray_750};
    `,
    'border-gray-800': css`
      border-color: ${Color.Gray_800};
    `,
    'border-gray-850': css`
      border-color: ${Color.Gray_850};
    `,
    'border-gray-900': css`
      border-color: ${Color.Gray_900};
    `,
    'border-gray-950': css`
      border-color: ${Color.Gray_950};
    `,
    'border-black': css`
      border-color: ${Color.Black};
    `,
  } satisfies Partial<Styles>

  return colorStyles
}
