import type { LitsNamespace } from './interface'

/**
 * Global registry for Lits namespaces.
 * Namespaces are registered here and can be imported via import("namespaceName")
 */
const namespaceRegistry = new Map<string, LitsNamespace>()

/**
 * Register a namespace so it can be imported in Lits code.
 * @param namespace The namespace to register
 */
export function registerNamespace(namespace: LitsNamespace): void {
  if (namespaceRegistry.has(namespace.name)) {
    throw new Error(`Namespace '${namespace.name}' is already registered`)
  }
  namespaceRegistry.set(namespace.name, namespace)
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
