import type { CssTemplateFunction, Styles } from '.'

export function getAlignAndJustifyStyles(css: CssTemplateFunction) {
  const alignStyles = {
    'text-left': css`
      text-align: left;
    `,
    'text-center': css`
      text-align: center;
    `,
    'text-right': css`
      text-align: right;
    `,
    'text-justify': css`
      text-align: justify;
    `,
    'text-start': css`
      text-align: start;
    `,
    'text-end': css`
      text-align: end;
    `,
    'align-baseline': css`
      vertical-align: baseline;
    `,
    'align-top': css`
      vertical-align: top;
    `,
    'align-middle': css`
      vertical-align: middle;
    `,
    'align-bottom': css`
      vertical-align: bottom;
    `,
    'align-text-top': css`
      vertical-align: text-top;
    `,
    'align-text-bottom': css`
      vertical-align: text-bottom;
    `,
    'align-sub': css`
      vertical-align: sub;
    `,
    'align-super': css`
      vertical-align: super;
    `,
    'self-auto': css`
      align-self: auto;
    `,
    'self-start': css`
      align-self: flex-start;
    `,
    'self-end': css`
      align-self: flex-end;
    `,
    'self-center': css`
      align-self: center;
    `,
    'self-baseline': css`
      align-self: baseline;
    `,
    'self-stretch': css`
      align-self: stretch;
    `,
    'content-normal': css`
      align-content: normal;
    `,
    'content-center': css`
      align-content: center;
    `,
    'content-start': css`
      align-content: flex-start;
    `,
    'content-end': css`
      align-content: flex-end;
    `,
    'content-between': css`
      align-content: space-between;
    `,
    'content-around': css`
      align-content: space-around;
    `,
    'content-evenly': css`
      align-content: space-evenly;
    `,
    'content-baseline': css`
      align-content: baseline;
    `,
    'content-stretch': css`
      align-content: stretch;
    `,
    'items-start': css`
      align-items: start;
    `,
    'items-end': css`
      align-items: end;
    `,
    'items-center': css`
      align-items: center;
    `,
    'items-baseline': css`
      align-items: baseline;
    `,
    'items-stretch': css`
      align-items: stretch;
    `,
    'justify-normal': css`
      justify-content: normal;
    `,
    'justify-start': css`
      justify-content: flex-start;
    `,
    'justify-end': css`
      justify-content: flex-end;
    `,
    'justify-center': css`
      justify-content: center;
    `,
    'justify-between': css`
      justify-content: space-between;
    `,
    'justify-around': css`
      justify-content: space-around;
    `,
    'justify-evenly': css`
      justify-content: space-evenly;
    `,
    'justify-stretch': css`
      justify-content: stretch;
    `,
    'justify-items-start': css`
      justify-items: start;
    `,
    'justify-items-end': css`
      justify-items: end;
    `,
    'justify-items-center': css`
      justify-items: center;
    `,
    'justify-items-stretch': css`
      justify-items: stretch;
    `,
    'justify-self-auto': css`
      justify-self: auto;
    `,
    'justify-self-start': css`
      justify-self: start;
    `,
    'justify-self-end': css`
      justify-self: end;
    `,
    'justify-self-center': css`
      justify-self: center;
    `,
    'justify-self-stretch': css`
      justify-self: stretch;
    `,
  } satisfies Partial<Styles>
  return alignStyles
}
