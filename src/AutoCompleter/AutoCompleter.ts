import { normalExpressionKeys, specialExpressionKeys } from '../builtin'
import type { ContextParams, Lits } from '../Lits/Lits'
import { reservedSymbolRecord } from '../tokenizer/reservedNames'

type AutoCompleteSuggestion = {
  program: string
  position: number
}

const litsCommands = new Set([...normalExpressionKeys, ...specialExpressionKeys, ...Object.keys(reservedSymbolRecord)])

// TODO: replace with get suggestions function
export class AutoCompleter {
  private prefixProgram: string = ''
  private suffixProgram: string = ''
  private searchString: string = ''
  private suggestions: string[] = []
  private suggestionIndex: null | number = null

  constructor(public readonly originalProgram: string, public readonly originalPosition: number, lits: Lits, params: ContextParams) {
    const partialProgram = this.originalProgram.slice(0, this.originalPosition)
    const tokenStream = lits.tokenize(partialProgram)

    const lastToken = tokenStream.tokens.at(-1)
    if (!lastToken) {
      return
    }

    if (lastToken[0] === 'Error') {
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
    const blacklist = new Set<string>(['0_def', '0_defn', '0_lambda'])

    const startsWithCaseSensitive = this.generateWithPredicate(params, suggestion =>
      !blacklist.has(suggestion) && suggestion.startsWith(this.searchString))
    startsWithCaseSensitive.forEach(suggestion => blacklist.add(suggestion))

    const startsWithCaseInsensitive = this.generateWithPredicate(params, suggestion =>
      !blacklist.has(suggestion) && suggestion.toLowerCase().startsWith(this.searchString.toLowerCase()))
    startsWithCaseInsensitive.forEach(suggestion => blacklist.add(suggestion))

    const includesCaseSensitive = this.generateWithPredicate(params, suggestion =>
      !blacklist.has(suggestion) && suggestion.includes(this.searchString))
    includesCaseSensitive.forEach(suggestion => blacklist.add(suggestion))

    const includesCaseInsensitive = this.generateWithPredicate(params, suggestion =>
      !blacklist.has(suggestion) && suggestion.includes(this.searchString.toLowerCase()))
    includesCaseInsensitive.forEach(suggestion => blacklist.add(suggestion))

    return [...startsWithCaseSensitive, ...startsWithCaseInsensitive, ...includesCaseSensitive, ...includesCaseInsensitive]
  }

  private generateWithPredicate(params: ContextParams, shouldInclude: (suggestion: string) => boolean): string[] {
    const suggestions = new Set<string>()

    litsCommands.forEach((suggestion) => {
      if (shouldInclude(suggestion)) {
        suggestions.add(suggestion)
      }
    })

    Object.keys(params.globalContext ?? {})
      .filter(shouldInclude)
      .forEach(suggestion => suggestions.add(suggestion))

    params.contexts?.forEach((context) => {
      Object.keys(context)
        .filter(shouldInclude)
        .forEach(suggestion => suggestions.add(suggestion))
    })

    Object.keys(params.bindings ?? {})
      .filter(shouldInclude)
      .forEach(suggestion => suggestions.add(suggestion))

    return [...suggestions].sort((a, b) => a.localeCompare(b))
  }
}
