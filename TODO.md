export TokenStream
export valid-symbol regexp

executeFunction should work for subset of special expressions, all that syntactically behaves as normal expressions, like &&, ||, etc.
But not ?? or defined?, sinse all parameters needs to be evaluated as for normal expressions.
This would need these special expressions to implement evaluateAsNormalExpression

reference:
Missing for and doseq

try - catch - FINALLY
