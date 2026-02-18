/**
 * Side-effect import: wires up reference data for the `doc` builtin function.
 * Import this module before using `doc()` on built-in functions.
 *
 * In the full entry point (src/full.ts), this is done automatically.
 * In the minimal entry point (src/index.ts), `doc()` returns '' for builtins.
 */
import { normalExpressionReference } from '../reference/index'
import { setNormalExpressionReference } from './builtin/normalExpressions'

setNormalExpressionReference(normalExpressionReference)
