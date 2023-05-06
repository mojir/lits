import { UndefinedSymbolEntry } from './interface'

export function joinUndefinedSymbols(
  ...undefinedSymbolsSets: Array<Set<UndefinedSymbolEntry>>
): Set<UndefinedSymbolEntry> {
  const undefinedSymbols = new Set<UndefinedSymbolEntry>()

  for (const set of undefinedSymbolsSets) {
    set.forEach(symbol => undefinedSymbols.add(symbol))
  }
  return undefinedSymbols
}

export function addAnalyzeResults(target: Set<UndefinedSymbolEntry>, source: Set<UndefinedSymbolEntry>): void {
  source.forEach(symbol => target.add(symbol))
}
