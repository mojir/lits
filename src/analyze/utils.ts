import type { UnresolvedSymbol, UnresolvedSymbols } from '.'

export function joinAnalyzeResults(...results: UnresolvedSymbols[]): UnresolvedSymbols {
  const result = new Set<UnresolvedSymbol>()
  for (const symbols of results)
    symbols.forEach(symbol => result.add(symbol))

  return result
}

export function addAnalyzeResults(target: UnresolvedSymbols, source: UnresolvedSymbols): void {
  source.forEach(symbol => target.add(symbol))
}

export function combinate<T>(arrays: T[][]): T[][] {
  return arrays.reduce((acc: T[][], curr) => {
    return acc.flatMap(a => curr.map(c => [...a, c]))
  }, [[]])
}
