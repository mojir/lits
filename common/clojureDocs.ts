export function getClojureDocsLink(functionName: string, clojureDocs?: string | null): string | null {
  const path = clojureDocs === null ? null : clojureDocs ?? functionName.replaceAll('_', '-').replace('?', '_q')
  return path
    ? path.startsWith('clojure.')
      ? `https://clojuredocs.org/${path}`
      : `https://clojuredocs.org/clojure.core/${path}`
    : null
}
