import type { LitsNamespace } from './interface'

/**
 * Global registry for Lits namespaces.
 * Namespaces are registered here and can be imported via import("namespaceName")
 */
const namespaceRegistry = new Map<string, LitsNamespace>()
const aliasMapRegistry = new Map<string, Record<string, string>>()

/**
 * Register a namespace so it can be imported in Lits code.
 * @param namespace The namespace to register
 */
export function registerNamespace(namespace: LitsNamespace): void {
  if (namespaceRegistry.has(namespace.name)) {
    throw new Error(`Namespace '${namespace.name}' is already registered`)
  }
  const aliasMap: Record<string, string> = {}
  for (const [fnName, expr] of Object.entries(namespace.functions)) {
    for (const alias of expr.aliases ?? []) {
      aliasMap[alias] = fnName
    }
  }
  namespaceRegistry.set(namespace.name, namespace)
  aliasMapRegistry.set(namespace.name, aliasMap)
}

/**
 * Get a registered namespace by name.
 * @param name The namespace name
 * @returns The namespace or undefined if not found
 */
export function getNamespace(name: string): LitsNamespace | undefined {
  return namespaceRegistry.get(name)
}

/**
 * Get the alias map for a registered namespace.
 * @param name The namespace name
 * @returns Record mapping alias → canonical function name, or undefined if not found
 */
export function getNamespaceAliasMap(name: string): Record<string, string> | undefined {
  return aliasMapRegistry.get(name)
}

/**
 * Check if a namespace is registered.
 * @param name The namespace name
 * @returns True if the namespace is registered
 */
export function hasNamespace(name: string): boolean {
  return namespaceRegistry.has(name)
}

/**
 * Get all registered namespace names.
 * @returns Array of namespace names
 */
export function getNamespaceNames(): string[] {
  return Array.from(namespaceRegistry.keys())
}
