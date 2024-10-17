import type { AnalyzeResult } from './interface'

export function joinAnalyzeResults(...results: AnalyzeResult[]): AnalyzeResult {
  const result: AnalyzeResult = {
    undefinedSymbols: new Set(),
  }
  for (const input of results)
    input.undefinedSymbols.forEach(symbol => result.undefinedSymbols.add(symbol))

  return result
}

export function addAnalyzeResults(target: AnalyzeResult, source: AnalyzeResult): void {
  source.undefinedSymbols.forEach(symbol => target.undefinedSymbols.add(symbol))
}
