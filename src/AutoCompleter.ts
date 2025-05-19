import { normalExpressionKeys, specialExpressionKeys } from './builtin'
import type { ContextParams } from './Lits/Lits'
import type { TokenType } from './tokenizer/token'
import type { TokenStream } from './tokenizer/tokenize'

const autoCompleteTokenTypes: TokenType[] = [
  'Operator',
  'ReservedSymbol',
  'Symbol',
]

export type AutoCompleteSuggestion = {
  suggestion: string
  searchPattern: string
}

const litsCommands = new Set([...normalExpressionKeys, ...specialExpressionKeys])

// TODO: replace with get suggestions function
export class AutoCompleter {
  private searchPattern: string = ''
  private suggestions: string[] = []
  private suggestionIndex: null | number = null

  constructor(tokenStream: TokenStream | null, params: ContextParams) {
    if (!tokenStream) {
      return
    }

    const lastToken = tokenStream.tokens.at(-1)
    if (!lastToken) {
      return
    }

    const [tokenType, tokenValue] = lastToken
    if (!autoCompleteTokenTypes.includes(tokenType)) {
      return
    }

    this.searchPattern = tokenValue.toLowerCase()
    this.suggestions = this.getAllSuggestions(params)
  }

  public getNextSuggestion(): AutoCompleteSuggestion | null {
    if (this.suggestions.length === 0) {
      return null
    }

    if (this.suggestionIndex === null) {
      this.suggestionIndex = 0
    }
    else {
      this.suggestionIndex += 1
      if (this.suggestionIndex >= this.suggestions.length) {
        this.suggestionIndex = 0
      }
    }

    return {
      suggestion: this.suggestions[this.suggestionIndex]!,
      searchPattern: this.searchPattern,
    }
  }

  public getPreviousSuggestion(): AutoCompleteSuggestion | null {
    if (this.suggestions.length === 0) {
      return null
    }

    if (this.suggestionIndex === null) {
      this.suggestionIndex = this.suggestions.length - 1
    }
    else {
      this.suggestionIndex -= 1
      if (this.suggestionIndex < 0) {
        this.suggestionIndex = this.suggestions.length - 1
      }
    }

    return {
      suggestion: this.suggestions[this.suggestionIndex]!,
      searchPattern: this.searchPattern,
    }
  }

  private getAllSuggestions(params: ContextParams): string[] {
    const suggestions = new Set<string>()

    litsCommands.forEach((name) => {
      if (name.toLowerCase().startsWith(this.searchPattern)) {
        suggestions.add(name)
      }
    })

    Object.keys(params.globalContext?.values ?? {})
      .filter(name => name.toLowerCase().startsWith(this.searchPattern))
      .forEach(name => suggestions.add(name))

    params.contexts?.forEach((context) => {
      Object.keys(context.values ?? {})
        .filter(name => name.toLowerCase().startsWith(this.searchPattern))
        .forEach(name => suggestions.add(name))
    })

    Object.keys(params.jsFunctions ?? {})
      .filter(name => name.toLowerCase().startsWith(this.searchPattern))
      .forEach(name => suggestions.add(name))

    Object.keys(params.values ?? {})
      .filter(name => name.toLowerCase().startsWith(this.searchPattern))
      .forEach(name => suggestions.add(name))

    return [...suggestions].sort((a, b) => a.toLowerCase().localeCompare(b.toLowerCase()))
  }
}
