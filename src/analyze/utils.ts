import type { UnresolvedIdentifier, UnresolvedIdentifiers } from '.'

export function joinAnalyzeResults(...results: UnresolvedIdentifiers[]): UnresolvedIdentifiers {
  const result = new Set<UnresolvedIdentifier>()
  for (const identifier of results)
    identifier.forEach(symbol => result.add(symbol))

  return result
}

export function addAnalyzeResults(target: UnresolvedIdentifiers, source: UnresolvedIdentifiers): void {
  source.forEach(symbol => target.add(symbol))
}

export function combinate<T>(arrays: T[][]): T[][] {
  return arrays.reduce((acc: T[][], curr) => {
    return acc.flatMap(a => curr.map(c => [...a, c]))
  }, [[]])
}
