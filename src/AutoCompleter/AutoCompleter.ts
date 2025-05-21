import { normalExpressionKeys, specialExpressionKeys } from '../builtin'
import type { ContextParams, Lits } from '../Lits/Lits'
import type { TokenStream } from '../tokenizer/tokenize'

export type AutoCompleteSuggestion = {
  program: string
  position: number
}

const litsCommands = new Set([...normalExpressionKeys, ...specialExpressionKeys])

// TODO: replace with get suggestions function
export class AutoCompleter {
  private prefixProgram: string = ''
  private suffixProgram: string = ''
  private searchString: string = ''
  private suggestions: string[] = []
  private suggestionIndex: null | number = null

  constructor(private originalProgram: string, private originalPosition: number, lits: Lits, params: ContextParams) {
    const partialProgram = this.originalProgram.slice(0, this.originalPosition)
    let tokenStream: TokenStream | null = null
    try {
      tokenStream = lits.tokenize(partialProgram)
    }
    catch {
      // do nothing
    }

    if (!tokenStream) {
      return
    }

    const lastToken = tokenStream.tokens.at(-1)
    if (!lastToken) {
      return
    }

    this.searchString = lastToken[1]
    this.prefixProgram = this.originalProgram.slice(0, this.originalPosition - this.searchString.length)
    this.suffixProgram = this.originalProgram.slice(this.prefixProgram.length + this.searchString.length)
    this.originalProgram.slice(this.prefixProgram.length + this.searchString.length)
    this.suggestions = this.generateSuggestions(params)
  }

  public getNextSuggestion(): AutoCompleteSuggestion | null {
    return this.getAutoCompleteSuggestionResult(this.getNextSuggestionSymbol())
  }

  public getPreviousSuggestion(): AutoCompleteSuggestion | null {
    return this.getAutoCompleteSuggestionResult(this.getPreviousSuggestionSymbol())
  }

  private getAutoCompleteSuggestionResult(suggestion: string | null): AutoCompleteSuggestion | null {
    if (suggestion === null) {
      return null
    }

    return {
      program: this.prefixProgram + suggestion + this.suffixProgram,
      position: this.prefixProgram.length + suggestion.length,
    }
  }

  private getNextSuggestionSymbol(): string | null {
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

    return this.suggestions[this.suggestionIndex]!
  }

  private getPreviousSuggestionSymbol(): string | null {
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

    return this.suggestions[this.suggestionIndex]!
  }

  public getSuggestions(): string[] {
    return [...this.suggestions]
  }

  public getSearchString(): string {
    return this.searchString
  }

  private generateSuggestions(params: ContextParams): string[] {
    const suggestions = new Set<string>()

    litsCommands.forEach((name) => {
      if (name.startsWith(this.searchString)) {
        suggestions.add(name)
      }
    })

    Object.keys(params.globalContext ?? {})
      .filter(name => name.startsWith(this.searchString))
      .forEach(name => suggestions.add(name))

    params.contexts?.forEach((context) => {
      Object.keys(context)
        .filter(name => name.startsWith(this.searchString))
        .forEach(name => suggestions.add(name))
    })

    Object.keys(params.jsFunctions ?? {})
      .filter(name => name.startsWith(this.searchString))
      .forEach(name => suggestions.add(name))

    Object.keys(params.values ?? {})
      .filter(name => name.startsWith(this.searchString))
      .forEach(name => suggestions.add(name))

    return [...suggestions].sort((a, b) => a.localeCompare(b))
  }
}
