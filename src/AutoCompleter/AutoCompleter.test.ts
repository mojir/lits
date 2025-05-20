import { describe, expect, test } from 'vitest'
import type { ContextParams } from '../Lits/Lits'
import type { TokenStream } from '../tokenizer/tokenize'
import { Lits } from '../Lits/Lits'
import { AutoCompleter } from './AutoCompleter'

describe('autoCompleter', () => {
  function createMockTokenStream(program: string): TokenStream {
    return new Lits().tokenize(program)
  }

  function createMockContextParams(): ContextParams {
    return {
      globalContext: {
        fooGlobal1: { value: 1 },
        fooGlobal2: { value: 2 },
      },
      contexts: [{
        fooContext3: { value: 1 },
        fooContext4: { value: 2 },
      }],
      jsFunctions: {
        fun1: {
          fn: () => {},
        },
        fun2: {
          fn: () => {},
        },
      },
      values: {
        fooValue5: 1,
        fooValue6: 2,
      },
    }
  }

  describe('initialization', () => {
    test('should initialize with no params', () => {
      const tokenStream = createMockTokenStream('fun')
      const completer = new AutoCompleter(tokenStream, {})
      expect(completer.getSearchPrefix()).toBe('fun')
    })

    test('should initialize with valid token stream', () => {
      const tokenStream = createMockTokenStream('fun')
      const params = createMockContextParams()
      const completer = new AutoCompleter(tokenStream, params)
      expect(completer.getSearchPrefix()).toBe('fun')
    })

    test('should not initialize with null token stream', () => {
      const params = createMockContextParams()
      const completer = new AutoCompleter(null, params)
      expect(completer.getSuggestions()).toEqual([])
    })

    test('should not initialize with non-matching token type', () => {
      const tokenStream = createMockTokenStream('123')
      const params = createMockContextParams()
      const completer = new AutoCompleter(tokenStream, params)
      expect(completer.getSuggestions()).toEqual([])
    })
  })

  describe('suggestions', () => {
    test('should generate suggestions from all contexts', () => {
      const tokenStream = createMockTokenStream('f')
      const params = createMockContextParams()
      const completer = new AutoCompleter(tokenStream, params)
      const suggestions = completer.getSuggestions()

      expect(suggestions).toContain('fooGlobal1')
      expect(suggestions).toContain('fooGlobal2')
      expect(suggestions).toContain('fooContext3')
      expect(suggestions).toContain('fooContext4')
      expect(suggestions).toContain('fooValue5')
      expect(suggestions).toContain('fooValue6')
      expect(suggestions).toContain('fun1')
      expect(suggestions).toContain('fun2')
      // built-in functions
      expect(suggestions).toContain('filter')
    })

    test('should filter suggestions based on search prefix', () => {
      const tokenStream = createMockTokenStream('fooGlo')
      const params = createMockContextParams()
      const completer = new AutoCompleter(tokenStream, params)
      const suggestions = completer.getSuggestions()

      expect(suggestions).toContain('fooGlobal1')
      expect(suggestions).toContain('fooGlobal2')
      expect(suggestions).not.toContain('fooContext3')
    })

    test('should return empty suggestions when no matches found', () => {
      const tokenStream = createMockTokenStream('xyz')
      const params = createMockContextParams()
      const completer = new AutoCompleter(tokenStream, params)
      expect(completer.getSuggestions()).toEqual([])
    })
  })

  describe('suggestion navigation', () => {
    test('should cycle through suggestions forward', () => {
      const tokenStream = createMockTokenStream('fooValue')
      const params = createMockContextParams()
      const completer = new AutoCompleter(tokenStream, params)

      const first = completer.getNextSuggestion()
      const second = completer.getNextSuggestion()
      const third = completer.getNextSuggestion()

      expect(first).not.toBeNull()
      expect(second).not.toBeNull()
      expect(third).not.toBeNull()
      expect(first?.suggestion).not.toBe(second?.suggestion)
      expect(second?.suggestion).not.toBe(third?.suggestion)
    })

    test('should cycle through suggestions backward', () => {
      const tokenStream = createMockTokenStream('fooValue')
      const params = createMockContextParams()
      const completer = new AutoCompleter(tokenStream, params)

      const first = completer.getPreviousSuggestion()
      const second = completer.getPreviousSuggestion()
      const third = completer.getPreviousSuggestion()

      expect(first).not.toBeNull()
      expect(second).not.toBeNull()
      expect(third).not.toBeNull()
      expect(first?.suggestion).not.toBe(second?.suggestion)
      expect(second?.suggestion).not.toBe(third?.suggestion)
    })

    test('should return null when no suggestions available', () => {
      const tokenStream = createMockTokenStream('xyz')
      const params = createMockContextParams()
      const completer = new AutoCompleter(tokenStream, params)

      expect(completer.getNextSuggestion()).toBeNull()
      expect(completer.getPreviousSuggestion()).toBeNull()
    })
  })
})
