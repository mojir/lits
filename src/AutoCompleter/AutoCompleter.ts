import { normalExpressionKeys, specialExpressionKeys } from '../builtin'
import type { ContextParams } from '../Lits/Lits'
import type { TokenType } from '../tokenizer/token'
import type { TokenStream } from '../tokenizer/tokenize'

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
  private searchPrefix: string = ''
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

    this.searchPrefix = tokenValue
    this.generateSuggestions(params)
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
      searchPattern: this.searchPrefix,
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
      searchPattern: this.searchPrefix,
    }
  }

  public getSuggestions(): string[] {
    return [...this.suggestions]
  }

  public getSearchPrefix(): string {
    return this.searchPrefix
  }

  private generateSuggestions(params: ContextParams) {
    const suggestions = new Set<string>()

    litsCommands.forEach((name) => {
      if (name.startsWith(this.searchPrefix)) {
        suggestions.add(name)
      }
    })

    Object.keys(params.globalContext ?? {})
      .filter(name => name.startsWith(this.searchPrefix))
      .forEach(name => suggestions.add(name))

    params.contexts?.forEach((context) => {
      Object.keys(context)
        .filter(name => name.startsWith(this.searchPrefix))
        .forEach(name => suggestions.add(name))
    })

    Object.keys(params.jsFunctions ?? {})
      .filter(name => name.startsWith(this.searchPrefix))
      .forEach(name => suggestions.add(name))

    Object.keys(params.values ?? {})
      .filter(name => name.startsWith(this.searchPrefix))
      .forEach(name => suggestions.add(name))

    this.suggestions = [...suggestions].sort((a, b) => a.localeCompare(b))
  }
}
