var Lits = (function (exports) {
  'use strict';

  var FUNCTION_SYMBOL = "__LITS_FUNCTION__";

  // eslint-disable-next-line @typescript-eslint/no-explicit-any,@typescript-eslint/explicit-module-boundary-types
  function getSourceCodeInfo(anyValue, sourceCodeInfo) {
      return (anyValue === null || anyValue === void 0 ? void 0 : anyValue.sourceCodeInfo) || sourceCodeInfo;
  }
  function valueToString$1(value) {
      if (isLitsFunction(value)) {
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          return "<function " + (value.name || "\u03BB") + ">";
      }
      if (isToken(value)) {
          return value.type + "-token \"" + value.value + "\"";
      }
      if (isAstNode(value)) {
          return value.type + "-node";
      }
      if (value === null) {
          return "null";
      }
      if (typeof value === "object" && value instanceof RegExp) {
          return "" + value;
      }
      if (typeof value === "object" && value instanceof Error) {
          return value.toString();
      }
      return JSON.stringify(value);
  }
  var tokenTypes = {
      fnShorthand: true,
      modifier: true,
      name: true,
      number: true,
      paren: true,
      regexpShorthand: true,
      reservedName: true,
      string: true,
  };
  function isToken(value) {
      if (typeof value !== "object" || value === null) {
          return false;
      }
      var tkn = value;
      if (!tkn.type || typeof tkn.value !== "string") {
          return false;
      }
      if (!tkn.sourceCodeInfo && tkn.sourceCodeInfo !== null) {
          return false;
      }
      return !!tokenTypes[tkn.type];
  }
  var astTypes = {
      Number: true,
      String: true,
      NormalExpression: true,
      SpecialExpression: true,
      Name: true,
      Modifier: true,
      ReservedName: true,
      Binding: true,
      Argument: true,
      Partial: true,
  };
  function isAstNode(value) {
      if (value === null || typeof value !== "object") {
          return false;
      }
      if (!value.token) {
          return false;
      }
      if (!astTypes[value.type]) {
          return false;
      }
      return true;
  }
  function isLitsFunction(func) {
      if (func === null || typeof func !== "object") {
          return false;
      }
      return !!func[FUNCTION_SYMBOL];
  }

  /*! *****************************************************************************
  Copyright (c) Microsoft Corporation.

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
  REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
  AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
  INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
  OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
  PERFORMANCE OF THIS SOFTWARE.
  ***************************************************************************** */
  /* global Reflect, Promise */

  var extendStatics = function(d, b) {
      extendStatics = Object.setPrototypeOf ||
          ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
          function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
      return extendStatics(d, b);
  };

  function __extends(d, b) {
      if (typeof b !== "function" && b !== null)
          throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
      extendStatics(d, b);
      function __() { this.constructor = d; }
      d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
  }

  var __assign = function() {
      __assign = Object.assign || function __assign(t) {
          for (var s, i = 1, n = arguments.length; i < n; i++) {
              s = arguments[i];
              for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p)) t[p] = s[p];
          }
          return t;
      };
      return __assign.apply(this, arguments);
  };

  function __spreadArray(to, from) {
      for (var i = 0, il = from.length, j = to.length; i < il; i++, j++)
          to[j] = from[i];
      return to;
  }

  /* istanbul ignore file */
  var RecurSignal = /** @class */ (function (_super) {
      __extends(RecurSignal, _super);
      function RecurSignal(params) {
          var _this = _super.call(this, "recur, params: " + params) || this;
          Object.setPrototypeOf(_this, RecurSignal.prototype);
          _this.name = "RecurSignal";
          _this.params = params;
          return _this;
      }
      return RecurSignal;
  }(Error));
  var LitsError = /** @class */ (function (_super) {
      __extends(LitsError, _super);
      function LitsError(message, sourceCodeInfo) {
          var _this = _super.call(this, "" + message + (sourceCodeInfo ? " " + sourceCodeInfo : "")) || this;
          _this.line = sourceCodeInfo === "EOF" || sourceCodeInfo === null ? null : sourceCodeInfo.line;
          _this.column = sourceCodeInfo === "EOF" || sourceCodeInfo === null ? null : sourceCodeInfo.column;
          Object.setPrototypeOf(_this, LitsError.prototype);
          _this.name = "LitsError";
          return _this;
      }
      return LitsError;
  }(Error));
  var NotAFunctionError = /** @class */ (function (_super) {
      __extends(NotAFunctionError, _super);
      function NotAFunctionError(fn, sourceCodeInfo) {
          var _this = _super.call(this, "Expected function, got " + valueToString$1(fn) + "." + (sourceCodeInfo ? " " + sourceCodeInfo : "")) || this;
          _this.line = sourceCodeInfo === "EOF" || sourceCodeInfo === null ? null : sourceCodeInfo.line;
          _this.column = sourceCodeInfo === "EOF" || sourceCodeInfo === null ? null : sourceCodeInfo.column;
          Object.setPrototypeOf(_this, NotAFunctionError.prototype);
          _this.name = "NotAFunctionError";
          return _this;
      }
      return NotAFunctionError;
  }(Error));
  var UserDefinedError = /** @class */ (function (_super) {
      __extends(UserDefinedError, _super);
      function UserDefinedError(message, sourceCodeInfo) {
          var _this = _super.call(this, "" + message + (sourceCodeInfo ? " " + sourceCodeInfo : "")) || this;
          _this.line = sourceCodeInfo === "EOF" || sourceCodeInfo === null ? null : sourceCodeInfo.line;
          _this.column = sourceCodeInfo === "EOF" || sourceCodeInfo === null ? null : sourceCodeInfo.column;
          Object.setPrototypeOf(_this, UserDefinedError.prototype);
          _this.name = "UserDefinedError";
          return _this;
      }
      return UserDefinedError;
  }(Error));
  var AssertionError = /** @class */ (function (_super) {
      __extends(AssertionError, _super);
      function AssertionError(message, sourceCodeInfo) {
          var _this = _super.call(this, "" + message + (sourceCodeInfo ? " " + sourceCodeInfo : "")) || this;
          _this.line = sourceCodeInfo === "EOF" || sourceCodeInfo === null ? null : sourceCodeInfo.line;
          _this.column = sourceCodeInfo === "EOF" || sourceCodeInfo === null ? null : sourceCodeInfo.column;
          Object.setPrototypeOf(_this, AssertionError.prototype);
          _this.name = "AssertionError";
          return _this;
      }
      return AssertionError;
  }(Error));
  var UndefinedSymbolError = /** @class */ (function (_super) {
      __extends(UndefinedSymbolError, _super);
      function UndefinedSymbolError(symbolName, sourceCodeInfo) {
          var _this = _super.call(this, "Undefined symbol '" + symbolName + "'" + (sourceCodeInfo ? " " + sourceCodeInfo : "")) || this;
          _this.line = sourceCodeInfo === "EOF" || sourceCodeInfo === null ? null : sourceCodeInfo.line;
          _this.column = sourceCodeInfo === "EOF" || sourceCodeInfo === null ? null : sourceCodeInfo.column;
          Object.setPrototypeOf(_this, UndefinedSymbolError.prototype);
          _this.name = "UndefinedSymbolError";
          return _this;
      }
      return UndefinedSymbolError;
  }(Error));

  function is$2(value, options) {
      if (options === void 0) { options = {}; }
      if (typeof value !== "string") {
          return false;
      }
      if (options.nonEmpty && value.length === 0) {
          return false;
      }
      if (options.char && value.length !== 1) {
          return false;
      }
      return true;
  }
  function assert$2(value, sourceCodeInfo, options) {
      if (options === void 0) { options = {}; }
      if (!is$2(value, options)) {
          throw new LitsError("Expected " + (options.nonEmpty ? "non empty string" : options.char ? "character" : "string") + ", got " + valueToString$1(value) + ".", getSourceCodeInfo(value, sourceCodeInfo));
      }
  }
  function as$2(value, sourceCodeInfo, options) {
      if (options === void 0) { options = {}; }
      assert$2(value, sourceCodeInfo, options);
      return value;
  }
  var string = {
      is: is$2,
      as: as$2,
      assert: assert$2,
  };

  function getRangeString(options) {
      if ((typeof options.gt === "number" || typeof options.gte === "number") &&
          (typeof options.lt === "number" || typeof options.lte === "number")) {
          return "" + (typeof options.gt === "number" ? options.gt + " < n " : options.gte + " <= n ") + (typeof options.lt === "number" ? "< " + options.lt : "<= " + options.lte);
      }
      if (typeof options.gt === "number" || typeof options.gte === "number") {
          return "" + (typeof options.gt === "number" ? "n > " + options.gt : "n >= " + options.gte);
      }
      if (typeof options.lt === "number" || typeof options.lte === "number") {
          return "" + (typeof options.lt === "number" ? "n < " + options.lt : "n <= " + options.lte);
      }
      return "";
  }
  function getNumberTypeName(options) {
      if (options.zero) {
          return "zero";
      }
      var sign = options.positive
          ? "positive"
          : options.negative
              ? "negative"
              : options.nonNegative
                  ? "non negative"
                  : options.nonPositive
                      ? "non positive"
                      : options.nonZero
                          ? "non zero"
                          : "";
      var numberType = options.integer ? "integer" : "number";
      var finite = options.finite ? "finite" : "";
      var range = getRangeString(options);
      return [sign, finite, numberType, range].filter(function (x) { return !!x; }).join(" ");
  }
  function is$1(value, options) {
      if (options === void 0) { options = {}; }
      if (typeof value !== "number") {
          return false;
      }
      if (options.integer && !Number.isInteger(value)) {
          return false;
      }
      if (options.finite && !Number.isFinite(value)) {
          return false;
      }
      if (options.zero && value !== 0) {
          return false;
      }
      if (options.nonZero && value === 0) {
          return false;
      }
      if (options.positive && value <= 0) {
          return false;
      }
      if (options.negative && value >= 0) {
          return false;
      }
      if (options.nonPositive && value > 0) {
          return false;
      }
      if (options.nonNegative && value < 0) {
          return false;
      }
      if (typeof options.gt === "number" && value <= options.gt) {
          return false;
      }
      if (typeof options.gte === "number" && value < options.gte) {
          return false;
      }
      if (typeof options.lt === "number" && value >= options.lt) {
          return false;
      }
      if (typeof options.lte === "number" && value > options.lte) {
          return false;
      }
      return true;
  }
  function assert$1(value, sourceCodeInfo, options) {
      if (options === void 0) { options = {}; }
      if (!is$1(value, options)) {
          throw new LitsError("Expected " + getNumberTypeName(options) + ", got " + valueToString$1(value) + ".", getSourceCodeInfo(value, sourceCodeInfo));
      }
  }
  function as$1(value, sourceCodeInfo, options) {
      if (options === void 0) { options = {}; }
      assert$1(value, sourceCodeInfo, options);
      return value;
  }
  var number = {
      is: is$1,
      as: as$1,
      assert: assert$1,
  };

  function is(value, options) {
      if (options === void 0) { options = {}; }
      if (!isToken(value)) {
          return false;
      }
      if (options.type && value.type !== options.type) {
          return false;
      }
      if (options.value && value.value !== options.value) {
          return false;
      }
      return true;
  }
  function assert(value, sourceCodeInfo, options) {
      if (options === void 0) { options = {}; }
      if (!is(value, options)) {
          if (isToken(value)) {
              sourceCodeInfo = value.sourceCodeInfo;
          }
          throw new LitsError("Expected " + (options.type ? options.type + "-" : "") + "token" + (typeof options.value === "string" ? " value='" + options.value + "'" : "") + ", got " + valueToString$1(value) + ".", getSourceCodeInfo(value, sourceCodeInfo));
      }
  }
  function as(value, sourceCodeInfo, options) {
      if (options === void 0) { options = {}; }
      assert(value, sourceCodeInfo, options);
      return value;
  }
  var token = {
      is: is,
      as: as,
      assert: assert,
  };

  var Asserter = /** @class */ (function () {
      function Asserter(typeName, predicate) {
          this.typeName = typeName;
          this.predicate = predicate;
      }
      Asserter.prototype.is = function (value) {
          return this.predicate(value);
      };
      Asserter.prototype.assert = function (value, sourceCodeInfo) {
          if (!this.predicate(value)) {
              throw new LitsError("Expected " + this.typeName + ", got " + valueToString$1(value) + ".", getSourceCodeInfo(value, sourceCodeInfo));
          }
      };
      Asserter.prototype.as = function (value, sourceCodeInfo) {
          this.assert(value, sourceCodeInfo);
          return value;
      };
      return Asserter;
  }());
  var litsFunction = new Asserter("LitsFunction", isLitsFunction);
  var stringOrNumber = new Asserter("string or number", function (value) { return typeof value === "string" || typeof value === "number"; });
  var any = new Asserter("Any", function (value) { return value !== undefined; });
  var sequence = new Asserter("Seq", function (value) { return Array.isArray(value) || string.is(value); });
  var object = new Asserter("Obj", function (value) {
      return !(value === null ||
          typeof value !== "object" ||
          Array.isArray(value) ||
          value instanceof RegExp ||
          isLitsFunction(value));
  });
  var collection = new Asserter("Coll", function (value) { return sequence.is(value) || object.is(value); });
  var array = new Asserter("Arr", function (value) { return Array.isArray(value); });
  var astNode = new Asserter("AstNode", isAstNode);
  var nameNode = new Asserter("NameNode", function (value) {
      if (!isAstNode(value)) {
          return false;
      }
      var nodeType = "Name";
      return value.type === nodeType;
  });
  var normalExpressionNodeWithName = new Asserter("Normal expression node with name", function (value) {
      if (!isAstNode(value)) {
          return false;
      }
      var nodeType = "NormalExpression";
      return value.type === nodeType && typeof value.name === "string";
  });
  var stringArray = new Asserter("string array", function (value) { return Array.isArray(value) && value.every(function (v) { return typeof v === "string"; }); });
  var charArray = new Asserter("character array", function (value) { return Array.isArray(value) && value.every(function (v) { return typeof v === "string" && v.length === 1; }); });
  var regExp = new Asserter("RegExp", function (value) { return value instanceof RegExp; });
  var stringOrRegExp = new Asserter("string or RegExp", function (value) { return value instanceof RegExp || typeof value === "string"; });
  var expressionNode = new Asserter("expression node", function (value) {
      if (!astNode.is(value)) {
          return false;
      }
      return (value.type === "NormalExpression" ||
          value.type === "SpecialExpression" ||
          value.type === "Number" ||
          value.type === "String");
  });
  function assertNumberOfParams(count, node) {
      var length = node.params.length;
      var sourceCodeInfo = node.token.sourceCodeInfo;
      if (typeof count === "number") {
          if (length !== count) {
              throw new LitsError("Wrong number of arguments to \"" + node.name + "\", expected " + count + ", got " + valueToString$1(length) + ".", node.token.sourceCodeInfo);
          }
      }
      else {
          var min = count.min, max = count.max;
          if (min === undefined && max === undefined) {
              throw new LitsError("Min or max must be specified.", sourceCodeInfo);
          }
          if (typeof min === "number" && length < min) {
              throw new LitsError("Wrong number of arguments to \"" + node.name + "\", expected at least " + min + ", got " + valueToString$1(length) + ".", sourceCodeInfo);
          }
          if (typeof max === "number" && length > max) {
              throw new LitsError("Wrong number of arguments to \"" + node.name + "\", expected at most " + max + ", got " + valueToString$1(length) + ".", sourceCodeInfo);
          }
      }
  }
  function assertEventNumberOfParams(node) {
      var length = node.params.length;
      if (length % 2 !== 0) {
          throw new LitsError("Wrong number of arguments, expected an even number, got " + valueToString$1(length) + ".", node.token.sourceCodeInfo);
      }
  }
  function asValue(value, sourceCodeInfo) {
      if (value === undefined) {
          throw new LitsError("Unexpected nil", getSourceCodeInfo(value, sourceCodeInfo));
      }
      return value;
  }
  function assertValue(value, sourceCodeInfo) {
      if (value === undefined) {
          throw new LitsError("Unexpected nil.", getSourceCodeInfo(value, sourceCodeInfo));
      }
  }

  var andSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
          return [
              newPosition + 1,
              {
                  type: "SpecialExpression",
                  name: "and",
                  params: params,
                  token: firstToken,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var value = true;
          for (var _i = 0, _b = node.params; _i < _b.length; _i++) {
              var param = _b[_i];
              value = evaluateAstNode(param, contextStack);
              if (!value) {
                  break;
              }
          }
          return value;
      },
  };

  function parseConditions(tokens, position, parseToken) {
      var _a, _b;
      var conditions = [];
      var tkn = token.as(tokens[position], "EOF");
      while (!token.is(tkn, { type: "paren", value: ")" })) {
          var test_1 = void 0;
          _a = parseToken(tokens, position), position = _a[0], test_1 = _a[1];
          var form = void 0;
          _b = parseToken(tokens, position), position = _b[0], form = _b[1];
          conditions.push({ test: test_1, form: form });
          tkn = token.as(tokens[position], "EOF");
      }
      return [position, conditions];
  }
  var condSpecialExpression = {
      parse: function (tokens, position, _a) {
          var _b;
          var parseToken = _a.parseToken;
          var firstToken = token.as(tokens[position], "EOF");
          var conditions;
          _b = parseConditions(tokens, position, parseToken), position = _b[0], conditions = _b[1];
          return [
              position + 1,
              {
                  type: "SpecialExpression",
                  name: "cond",
                  conditions: conditions,
                  params: [],
                  token: firstToken,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          for (var _i = 0, _b = node.conditions; _i < _b.length; _i++) {
              var condition = _b[_i];
              var value = evaluateAstNode(condition.test, contextStack);
              if (!value) {
                  continue;
              }
              return evaluateAstNode(condition.form, contextStack);
          }
          return null;
      },
  };

  var reservedNamesRecord = {
      true: { value: true },
      false: { value: false },
      nil: { value: null },
  };
  var reservedNames = Object.keys(reservedNamesRecord);

  function assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo) {
      if (typeof name !== "string") {
          return;
      }
      if (builtin.specialExpressions[name]) {
          throw new LitsError("Cannot define variable " + name + ", it's a special expression.", sourceCodeInfo);
      }
      if (builtin.normalExpressions[name]) {
          throw new LitsError("Cannot define variable " + name + ", it's a builtin function.", sourceCodeInfo);
      }
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      if (reservedNamesRecord[name]) {
          throw new LitsError("Cannot define variable " + name + ", it's a reserved name.", sourceCodeInfo);
      }
      if (contextStack.globalContext[name]) {
          throw new LitsError("Name already defined \"" + name + "\".", sourceCodeInfo);
      }
  }

  function createParser(expressionName) {
      return function (tokens, position, parsers) {
          var _a, _b;
          var firstToken = token.as(tokens[position], "EOF");
          var parseToken = parsers.parseToken;
          var functionName = undefined;
          if (expressionName === "defn" || expressionName === "defns") {
              _a = parseToken(tokens, position), position = _a[0], functionName = _a[1];
              if (expressionName === "defn") {
                  nameNode.assert(functionName, functionName.token.sourceCodeInfo);
              }
          }
          var functionOverloades;
          _b = parseFunctionOverloades(tokens, position, parsers), position = _b[0], functionOverloades = _b[1];
          if (expressionName === "defn" || expressionName === "defns") {
              return [
                  position,
                  {
                      type: "SpecialExpression",
                      name: expressionName,
                      functionName: functionName,
                      params: [],
                      overloads: functionOverloades,
                      token: firstToken,
                  },
              ];
          }
          return [
              position,
              {
                  type: "SpecialExpression",
                  name: expressionName,
                  params: [],
                  overloads: functionOverloades,
                  token: firstToken,
              },
          ];
      };
  }
  function getFunctionName(expressionName, node, contextStack, evaluateAstNode) {
      var sourceCodeInfo = node.token.sourceCodeInfo;
      if (expressionName === "defn") {
          return node.functionName.value;
      }
      if (expressionName === "defns") {
          var name_1 = evaluateAstNode(node.functionName, contextStack);
          string.assert(name_1, sourceCodeInfo);
          return name_1;
      }
      return undefined;
  }
  function createEvaluator(expressionName) {
      return function (node, contextStack, _a) {
          var _b;
          var evaluateAstNode = _a.evaluateAstNode, builtin = _a.builtin;
          var name = getFunctionName(expressionName, node, contextStack, evaluateAstNode);
          assertNameNotDefined(name, contextStack, builtin, node.token.sourceCodeInfo);
          var evaluatedFunctionOverloades = [];
          for (var _i = 0, _c = node.overloads; _i < _c.length; _i++) {
              var functionOverload = _c[_i];
              var functionContext = {};
              for (var _d = 0, _e = functionOverload.arguments.bindings; _d < _e.length; _d++) {
                  var binding = _e[_d];
                  var bindingValueNode = binding.value;
                  var bindingValue = evaluateAstNode(bindingValueNode, contextStack);
                  functionContext[binding.name] = { value: bindingValue };
              }
              var evaluatedFunctionOverload = {
                  arguments: {
                      mandatoryArguments: functionOverload.arguments.mandatoryArguments,
                      restArgument: functionOverload.arguments.restArgument,
                  },
                  arity: functionOverload.arity,
                  body: functionOverload.body,
                  functionContext: functionContext,
              };
              evaluatedFunctionOverloades.push(evaluatedFunctionOverload);
          }
          var litsFunction = (_b = {},
              _b[FUNCTION_SYMBOL] = true,
              _b.sourceCodeInfo = node.token.sourceCodeInfo,
              _b.type = "user-defined",
              _b.name = name,
              _b.overloads = evaluatedFunctionOverloades,
              _b);
          if (expressionName === "fn") {
              return litsFunction;
          }
          contextStack.globalContext[name] = { value: litsFunction };
          return null;
      };
  }
  var defnSpecialExpression = {
      parse: createParser("defn"),
      evaluate: createEvaluator("defn"),
  };
  var defnsSpecialExpression = {
      parse: createParser("defns"),
      evaluate: createEvaluator("defns"),
  };
  var fnSpecialExpression = {
      parse: createParser("fn"),
      evaluate: createEvaluator("fn"),
  };
  function arityOk(overloadedFunctions, arity) {
      if (typeof arity === "number") {
          return overloadedFunctions.every(function (fun) {
              if (typeof fun.arity === "number") {
                  return fun.arity !== arity;
              }
              return fun.arity.min > arity;
          });
      }
      return overloadedFunctions.every(function (fun) {
          if (typeof fun.arity === "number") {
              return fun.arity < arity.min;
          }
          return false;
      });
  }
  function parseFunctionBody(tokens, position, _a) {
      var _b;
      var parseToken = _a.parseToken;
      var tkn = token.as(tokens[position], "EOF");
      var body = [];
      while (!(tkn.type === "paren" && tkn.value === ")")) {
          var bodyNode = void 0;
          _b = parseToken(tokens, position), position = _b[0], bodyNode = _b[1];
          body.push(bodyNode);
          tkn = token.as(tokens[position], "EOF");
      }
      if (body.length === 0) {
          throw new LitsError("Missing body in function", tkn.sourceCodeInfo);
      }
      return [position + 1, body];
  }
  function parseFunctionOverloades(tokens, position, parsers) {
      var _a, _b, _c, _d;
      var tkn = token.as(tokens[position], "EOF", { type: "paren" });
      if (tkn.value === "(") {
          var functionOverloades = [];
          while (!(tkn.type === "paren" && tkn.value === ")")) {
              position += 1;
              tkn = token.as(tokens[position], "EOF");
              var functionArguments = void 0;
              _a = parseFunctionArguments(tokens, position, parsers), position = _a[0], functionArguments = _a[1];
              var arity = functionArguments.restArgument
                  ? { min: functionArguments.mandatoryArguments.length }
                  : functionArguments.mandatoryArguments.length;
              if (!arityOk(functionOverloades, arity)) {
                  throw new LitsError("All overloaded functions must have different arity", tkn.sourceCodeInfo);
              }
              var functionBody = void 0;
              _b = parseFunctionBody(tokens, position, parsers), position = _b[0], functionBody = _b[1];
              functionOverloades.push({
                  arguments: functionArguments,
                  body: functionBody,
                  arity: arity,
              });
              tkn = token.as(tokens[position], "EOF", { type: "paren" });
              if (tkn.value !== ")" && tkn.value !== "(") {
                  throw new LitsError("Expected ( or ) token, got " + valueToString$1(tkn) + ".", tkn.sourceCodeInfo);
              }
          }
          return [position + 1, functionOverloades];
      }
      else if (tkn.value === "[") {
          var functionArguments = void 0;
          _c = parseFunctionArguments(tokens, position, parsers), position = _c[0], functionArguments = _c[1];
          var arity = functionArguments.restArgument
              ? { min: functionArguments.mandatoryArguments.length }
              : functionArguments.mandatoryArguments.length;
          var functionBody = void 0;
          _d = parseFunctionBody(tokens, position, parsers), position = _d[0], functionBody = _d[1];
          return [
              position,
              [
                  {
                      arguments: functionArguments,
                      body: functionBody,
                      arity: arity,
                  },
              ],
          ];
      }
      else {
          throw new LitsError("Expected [ or ( token, got " + valueToString$1(tkn), tkn.sourceCodeInfo);
      }
  }
  function parseFunctionArguments(tokens, position, parsers) {
      var _a;
      var parseArgument = parsers.parseArgument, parseBindings = parsers.parseBindings;
      var bindings = [];
      var restArgument = undefined;
      var mandatoryArguments = [];
      var argNames = {};
      var state = "mandatory";
      var tkn = token.as(tokens[position], "EOF");
      position += 1;
      tkn = token.as(tokens[position], "EOF");
      while (!(tkn.type === "paren" && tkn.value === "]")) {
          if (state === "let") {
              _a = parseBindings(tokens, position), position = _a[0], bindings = _a[1];
              break;
          }
          else {
              var _b = parseArgument(tokens, position), newPosition = _b[0], node = _b[1];
              position = newPosition;
              tkn = token.as(tokens[position], "EOF");
              if (node.type === "Modifier") {
                  switch (node.value) {
                      case "&":
                          if (state === "rest") {
                              throw new LitsError("& can only appear once", tkn.sourceCodeInfo);
                          }
                          state = "rest";
                          break;
                      case "&let":
                          if (state === "rest" && !restArgument) {
                              throw new LitsError("No rest argument was spcified", tkn.sourceCodeInfo);
                          }
                          state = "let";
                          break;
                      default:
                          throw new LitsError("Illegal modifier: " + node.value, tkn.sourceCodeInfo);
                  }
              }
              else {
                  if (argNames[node.name]) {
                      throw new LitsError("Duplicate argument \"" + node.name + "\"", tkn.sourceCodeInfo);
                  }
                  else {
                      argNames[node.name] = true;
                  }
                  switch (state) {
                      case "mandatory":
                          mandatoryArguments.push(node.name);
                          break;
                      case "rest":
                          if (restArgument !== undefined) {
                              throw new LitsError("Can only specify one rest argument", tkn.sourceCodeInfo);
                          }
                          restArgument = node.name;
                          break;
                  }
              }
          }
      }
      if (state === "rest" && restArgument === undefined) {
          throw new LitsError("Missing rest argument name", tkn.sourceCodeInfo);
      }
      position += 1;
      var args = {
          mandatoryArguments: mandatoryArguments,
          restArgument: restArgument,
          bindings: bindings,
      };
      return [position, args];
  }

  var defSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
          nameNode.assert(params[0], firstToken.sourceCodeInfo);
          return [
              newPosition + 1,
              {
                  type: "SpecialExpression",
                  name: "def",
                  params: params,
                  token: firstToken,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode, builtin = _a.builtin;
          var sourceCodeInfo = node.token.sourceCodeInfo;
          var name = nameNode.as(node.params[0], sourceCodeInfo).value;
          assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo);
          var value = evaluateAstNode(astNode.as(node.params[1], sourceCodeInfo), contextStack);
          contextStack.globalContext[name] = { value: value };
          return value;
      },
      validate: function (node) { return assertNumberOfParams(2, node); },
  };

  var defsSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
          return [
              newPosition + 1,
              {
                  type: "SpecialExpression",
                  name: "defs",
                  params: params,
                  token: firstToken,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode, builtin = _a.builtin;
          var sourceCodeInfo = node.token.sourceCodeInfo;
          var name = evaluateAstNode(astNode.as(node.params[0], sourceCodeInfo), contextStack);
          string.assert(name, sourceCodeInfo);
          assertNameNotDefined(name, contextStack, builtin, node.token.sourceCodeInfo);
          var value = evaluateAstNode(astNode.as(node.params[1], sourceCodeInfo), contextStack);
          contextStack.globalContext[name] = { value: value };
          return value;
      },
      validate: function (node) { return assertNumberOfParams(2, node); },
  };

  var doSpecialExpression = {
      parse: function (tokens, position, _a) {
          var _b;
          var parseToken = _a.parseToken;
          var tkn = token.as(tokens[position], "EOF");
          var node = {
              type: "SpecialExpression",
              name: "do",
              params: [],
              token: tkn,
          };
          while (!token.is(tkn, { type: "paren", value: ")" })) {
              var bodyNode = void 0;
              _b = parseToken(tokens, position), position = _b[0], bodyNode = _b[1];
              node.params.push(bodyNode);
              tkn = token.as(tokens[position], "EOF");
          }
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var newContext = {};
          var newContextStack = contextStack.withContext(newContext);
          var result = null;
          for (var _i = 0, _b = node.params; _i < _b.length; _i++) {
              var form = _b[_i];
              result = evaluateAstNode(form, newContextStack);
          }
          return result;
      },
  };

  function parseLoopBinding(tokens, position, _a) {
      var _b, _c, _d, _e;
      var parseBinding = _a.parseBinding, parseBindings = _a.parseBindings, parseToken = _a.parseToken;
      var bindingNode;
      _b = parseBinding(tokens, position), position = _b[0], bindingNode = _b[1];
      var loopBinding = {
          binding: bindingNode,
          modifiers: [],
      };
      var tkn = token.as(tokens[position], "EOF");
      while (tkn.type === "modifier") {
          switch (tkn.value) {
              case "&let":
                  if (loopBinding.letBindings) {
                      throw new LitsError("Only one &let modifier allowed", tkn.sourceCodeInfo);
                  }
                  _c = parseBindings(tokens, position + 1), position = _c[0], loopBinding.letBindings = _c[1];
                  loopBinding.modifiers.push("&let");
                  break;
              case "&when":
                  if (loopBinding.whenNode) {
                      throw new LitsError("Only one &when modifier allowed", tkn.sourceCodeInfo);
                  }
                  _d = parseToken(tokens, position + 1), position = _d[0], loopBinding.whenNode = _d[1];
                  loopBinding.modifiers.push("&when");
                  break;
              case "&while":
                  if (loopBinding.whileNode) {
                      throw new LitsError("Only one &while modifier allowed", tkn.sourceCodeInfo);
                  }
                  _e = parseToken(tokens, position + 1), position = _e[0], loopBinding.whileNode = _e[1];
                  loopBinding.modifiers.push("&while");
                  break;
              default:
                  throw new LitsError("Illegal modifier: " + tkn.value, tkn.sourceCodeInfo);
          }
          tkn = token.as(tokens[position], "EOF");
      }
      return [position, loopBinding];
  }
  function addToContext(bindings, context, contextStack, evaluateAstNode, sourceCodeInfo) {
      for (var _i = 0, bindings_1 = bindings; _i < bindings_1.length; _i++) {
          var binding = bindings_1[_i];
          if (context[binding.name]) {
              throw new LitsError("Variable already defined: " + binding.name + ".", sourceCodeInfo);
          }
          context[binding.name] = { value: evaluateAstNode(binding.value, contextStack) };
      }
  }
  function parseLoopBindings(tokens, position, parsers) {
      var _a;
      token.assert(tokens[position], "EOF", { type: "paren", value: "[" });
      position += 1;
      var loopBindings = [];
      var tkn = token.as(tokens[position], "EOF");
      while (!token.is(tkn, { type: "paren", value: "]" })) {
          var loopBinding = void 0;
          _a = parseLoopBinding(tokens, position, parsers), position = _a[0], loopBinding = _a[1];
          loopBindings.push(loopBinding);
          tkn = token.as(tokens[position], "EOF");
      }
      return [position + 1, loopBindings];
  }
  var forSpecialExpression = {
      parse: function (tokens, position, parsers) {
          var _a, _b;
          var firstToken = token.as(tokens[position], "EOF");
          var parseToken = parsers.parseToken;
          var loopBindings;
          _a = parseLoopBindings(tokens, position, parsers), position = _a[0], loopBindings = _a[1];
          var expression;
          _b = parseToken(tokens, position), position = _b[0], expression = _b[1];
          token.assert(tokens[position], "EOF", { type: "paren", value: ")" });
          var node = {
              name: "for",
              type: "SpecialExpression",
              loopBindings: loopBindings,
              params: [expression],
              token: firstToken,
          };
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var sourceCodeInfo = node.token.sourceCodeInfo;
          var loopBindings = node.loopBindings, params = node.params;
          var expression = astNode.as(params[0], sourceCodeInfo);
          var result = [];
          var bindingIndices = loopBindings.map(function () { return 0; });
          var abort = false;
          while (!abort) {
              var context = {};
              var newContextStack = contextStack.withContext(context);
              var skip = false;
              bindingsLoop: for (var bindingIndex = 0; bindingIndex < loopBindings.length; bindingIndex += 1) {
                  var _b = asValue(loopBindings[bindingIndex], sourceCodeInfo), binding = _b.binding, letBindings = _b.letBindings, whenNode = _b.whenNode, whileNode = _b.whileNode, modifiers = _b.modifiers;
                  var coll = collection.as(evaluateAstNode(binding.value, newContextStack), sourceCodeInfo);
                  var seq = sequence.is(coll) ? coll : Object.entries(coll);
                  if (seq.length === 0) {
                      skip = true;
                      abort = true;
                      break;
                  }
                  var index = asValue(bindingIndices[bindingIndex], sourceCodeInfo);
                  if (index >= seq.length) {
                      skip = true;
                      if (bindingIndex === 0) {
                          abort = true;
                          break;
                      }
                      bindingIndices[bindingIndex] = 0;
                      bindingIndices[bindingIndex - 1] = asValue(bindingIndices[bindingIndex - 1], sourceCodeInfo) + 1;
                      break;
                  }
                  if (context[binding.name]) {
                      throw new LitsError("Variable already defined: " + binding.name + ".", sourceCodeInfo);
                  }
                  context[binding.name] = {
                      value: any.as(seq[index], sourceCodeInfo),
                  };
                  for (var _i = 0, modifiers_1 = modifiers; _i < modifiers_1.length; _i++) {
                      var modifier = modifiers_1[_i];
                      switch (modifier) {
                          case "&let":
                              addToContext(asValue(letBindings, sourceCodeInfo), context, newContextStack, evaluateAstNode, sourceCodeInfo);
                              break;
                          case "&when":
                              if (!evaluateAstNode(astNode.as(whenNode, sourceCodeInfo), newContextStack)) {
                                  bindingIndices[bindingIndex] = asValue(bindingIndices[bindingIndex], sourceCodeInfo) + 1;
                                  skip = true;
                                  break bindingsLoop;
                              }
                              break;
                          case "&while":
                              if (!evaluateAstNode(astNode.as(whileNode, sourceCodeInfo), newContextStack)) {
                                  bindingIndices[bindingIndex] = Number.POSITIVE_INFINITY;
                                  skip = true;
                                  break bindingsLoop;
                              }
                              break;
                      }
                  }
              }
              if (!skip) {
                  result.push(evaluateAstNode(expression, newContextStack));
                  bindingIndices[bindingIndices.length - 1] += 1;
              }
          }
          return result;
      },
  };

  var ifLetSpecialExpression = {
      parse: function (tokens, position, _a) {
          var _b, _c;
          var parseBindings = _a.parseBindings, parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var bindings;
          _b = parseBindings(tokens, position), position = _b[0], bindings = _b[1];
          if (bindings.length !== 1) {
              throw new LitsError("Expected exactly one binding, got " + valueToString$1(bindings.length), firstToken.sourceCodeInfo);
          }
          var params;
          _c = parseTokens(tokens, position), position = _c[0], params = _c[1];
          var node = {
              type: "SpecialExpression",
              name: "if-let",
              binding: asValue(bindings[0], firstToken.sourceCodeInfo),
              params: params,
              token: firstToken,
          };
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var sourceCodeInfo = node.token.sourceCodeInfo;
          var locals = {};
          var bindingValue = evaluateAstNode(node.binding.value, contextStack);
          if (bindingValue) {
              locals[node.binding.name] = { value: bindingValue };
              var newContextStack = contextStack.withContext(locals);
              var thenForm = astNode.as(node.params[0], sourceCodeInfo);
              return evaluateAstNode(thenForm, newContextStack);
          }
          if (node.params.length === 2) {
              var elseForm = astNode.as(node.params[1], sourceCodeInfo);
              return evaluateAstNode(elseForm, contextStack);
          }
          return null;
      },
      validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
  };

  var ifNotSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
          return [
              newPosition + 1,
              {
                  type: "SpecialExpression",
                  name: "if-not",
                  params: params,
                  token: firstToken,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var sourceCodeInfo = node.token.sourceCodeInfo;
          var _b = node.params, conditionNode = _b[0], trueNode = _b[1], falseNode = _b[2];
          if (!evaluateAstNode(astNode.as(conditionNode, sourceCodeInfo), contextStack)) {
              return evaluateAstNode(astNode.as(trueNode, sourceCodeInfo), contextStack);
          }
          else {
              if (node.params.length === 3) {
                  return evaluateAstNode(astNode.as(falseNode, sourceCodeInfo), contextStack);
              }
              else {
                  return null;
              }
          }
      },
      validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
  };

  var ifSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
          return [
              newPosition + 1,
              {
                  type: "SpecialExpression",
                  name: "if",
                  params: params,
                  token: firstToken,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var sourceCodeInfo = node.token.sourceCodeInfo;
          var _b = node.params, conditionNode = _b[0], trueNode = _b[1], falseNode = _b[2];
          if (evaluateAstNode(astNode.as(conditionNode, sourceCodeInfo), contextStack)) {
              return evaluateAstNode(astNode.as(trueNode, sourceCodeInfo), contextStack);
          }
          else {
              if (node.params.length === 3) {
                  return evaluateAstNode(astNode.as(falseNode, sourceCodeInfo), contextStack);
              }
              else {
                  return null;
              }
          }
      },
      validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
  };

  var letSpecialExpression = {
      parse: function (tokens, position, _a) {
          var _b, _c;
          var parseBindings = _a.parseBindings, parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var bindings;
          _b = parseBindings(tokens, position), position = _b[0], bindings = _b[1];
          var params;
          _c = parseTokens(tokens, position), position = _c[0], params = _c[1];
          var node = {
              type: "SpecialExpression",
              name: "let",
              params: params,
              bindings: bindings,
              token: firstToken,
          };
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var locals = {};
          var newContextStack = contextStack.withContext(locals);
          for (var _i = 0, _b = node.bindings; _i < _b.length; _i++) {
              var binding = _b[_i];
              var bindingValueNode = binding.value;
              var bindingValue = evaluateAstNode(bindingValueNode, newContextStack);
              locals[binding.name] = { value: bindingValue };
          }
          var result = null;
          for (var _c = 0, _d = node.params; _c < _d.length; _c++) {
              var astNode = _d[_c];
              result = evaluateAstNode(astNode, newContextStack);
          }
          return result;
      },
  };

  var loopSpecialExpression = {
      parse: function (tokens, position, _a) {
          var _b, _c;
          var parseTokens = _a.parseTokens, parseBindings = _a.parseBindings;
          var firstToken = token.as(tokens[position], "EOF");
          var bindings;
          _b = parseBindings(tokens, position), position = _b[0], bindings = _b[1];
          var params;
          _c = parseTokens(tokens, position), position = _c[0], params = _c[1];
          var node = {
              type: "SpecialExpression",
              name: "loop",
              params: params,
              bindings: bindings,
              token: firstToken,
          };
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var sourceCodeInfo = node.token.sourceCodeInfo;
          var bindingContext = node.bindings.reduce(function (result, binding) {
              result[binding.name] = { value: evaluateAstNode(binding.value, contextStack) };
              return result;
          }, {});
          var newContextStack = contextStack.withContext(bindingContext);
          var _loop_1 = function () {
              var result = null;
              try {
                  for (var _i = 0, _b = node.params; _i < _b.length; _i++) {
                      var form = _b[_i];
                      result = evaluateAstNode(form, newContextStack);
                  }
              }
              catch (error) {
                  if (error instanceof RecurSignal) {
                      var params_1 = error.params;
                      if (params_1.length !== node.bindings.length) {
                          throw new LitsError("recur expected " + node.bindings.length + " parameters, got " + valueToString$1(params_1.length), sourceCodeInfo);
                      }
                      node.bindings.forEach(function (binding, index) {
                          asValue(bindingContext[binding.name], sourceCodeInfo).value = any.as(params_1[index], sourceCodeInfo);
                      });
                      return "continue";
                  }
                  throw error;
              }
              return { value: result };
          };
          for (;;) {
              var state_1 = _loop_1();
              if (typeof state_1 === "object")
                  return state_1.value;
          }
      },
  };

  var orSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
          return [
              newPosition + 1,
              {
                  type: "SpecialExpression",
                  name: "or",
                  params: params,
                  token: firstToken,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var value = false;
          for (var _i = 0, _b = node.params; _i < _b.length; _i++) {
              var param = _b[_i];
              value = evaluateAstNode(param, contextStack);
              if (value) {
                  break;
              }
          }
          return value;
      },
  };

  var recurSpecialExpression = {
      parse: function (tokens, position, _a) {
          var _b;
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var params;
          _b = parseTokens(tokens, position), position = _b[0], params = _b[1];
          var node = {
              type: "SpecialExpression",
              name: "recur",
              params: params,
              token: firstToken,
          };
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var params = node.params.map(function (paramNode) { return evaluateAstNode(paramNode, contextStack); });
          throw new RecurSignal(params);
      },
  };

  var throwSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseToken = _a.parseToken;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = parseToken(tokens, position), newPosition = _b[0], messageNode = _b[1];
          position = newPosition;
          token.assert(tokens[position], "EOF", { type: "paren", value: ")" });
          position += 1;
          var node = {
              type: "SpecialExpression",
              name: "throw",
              params: [],
              messageNode: messageNode,
              token: firstToken,
          };
          return [position, node];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var message = string.as(evaluateAstNode(node.messageNode, contextStack), node.token.sourceCodeInfo, {
              nonEmpty: true,
          });
          throw new UserDefinedError(message, node.token.sourceCodeInfo);
      },
  };

  var timeSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseToken = _a.parseToken;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = parseToken(tokens, position), newPosition = _b[0], astNode = _b[1];
          var node = {
              type: "SpecialExpression",
              name: "time!",
              params: [astNode],
              token: firstToken,
          };
          return [newPosition + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var param = node.params[0];
          astNode.assert(param, node.token.sourceCodeInfo);
          var startTime = Date.now();
          var result = evaluateAstNode(param, contextStack);
          var totalTime = Date.now() - startTime;
          // eslint-disable-next-line no-console
          console.log("Elapsed time: " + totalTime + " ms");
          return result;
      },
      validate: function (node) { return assertNumberOfParams(1, node); },
  };

  var trySpecialExpression = {
      parse: function (tokens, position, _a) {
          var _b, _c, _d;
          var parseToken = _a.parseToken;
          var firstToken = token.as(tokens[position], "EOF");
          var tryExpression;
          _b = parseToken(tokens, position), position = _b[0], tryExpression = _b[1];
          token.assert(tokens[position], "EOF", { type: "paren", value: "(" });
          position += 1;
          token.assert(tokens[position], "EOF", { type: "paren", value: "(" });
          position += 1;
          var error;
          _c = parseToken(tokens, position), position = _c[0], error = _c[1];
          nameNode.assert(error, error.token.sourceCodeInfo);
          token.assert(tokens[position], "EOF", { type: "paren", value: ")" });
          position += 1;
          var catchExpression;
          _d = parseToken(tokens, position), position = _d[0], catchExpression = _d[1];
          token.assert(tokens[position], "EOF", { type: "paren", value: ")" });
          position += 1;
          token.assert(tokens[position], "EOF", { type: "paren", value: ")" });
          position += 1;
          var node = {
              type: "SpecialExpression",
              name: "try",
              params: [],
              tryExpression: tryExpression,
              catchExpression: catchExpression,
              error: error,
              token: firstToken,
          };
          return [position, node];
      },
      evaluate: function (node, contextStack, _a) {
          var _b;
          var evaluateAstNode = _a.evaluateAstNode;
          try {
              return evaluateAstNode(node.tryExpression, contextStack);
          }
          catch (error) {
              var newContext = (_b = {},
                  _b[node.error.value] = { value: any.as(error, node.token.sourceCodeInfo) },
                  _b);
              return evaluateAstNode(node.catchExpression, contextStack.withContext(newContext));
          }
      },
  };

  function collHasKey(coll, key) {
      if (!collection.is(coll)) {
          return false;
      }
      if (string.is(coll) || array.is(coll)) {
          if (!number.is(key, { integer: true })) {
              return false;
          }
          return key >= 0 && key < coll.length;
      }
      return !!Object.getOwnPropertyDescriptor(coll, key);
  }
  var sortOrderByType = {
      boolean: 0,
      number: 1,
      string: 2,
      array: 3,
      object: 4,
      regexp: 5,
      unknown: 6,
      null: 7,
  };
  function getType(value) {
      if (value === null) {
          return "null";
      }
      else if (typeof value === "boolean") {
          return "boolean";
      }
      else if (typeof value === "number") {
          return "number";
      }
      else if (typeof value === "string") {
          return "string";
      }
      else if (array.is(value)) {
          return "array";
      }
      else if (object.is(value)) {
          return "object";
      }
      else if (regExp.is(value)) {
          return "regexp";
      }
      else {
          return "unknown";
      }
  }
  function compare(a, b) {
      var aType = getType(a);
      var bType = getType(b);
      if (aType !== bType) {
          return Math.sign(sortOrderByType[aType] - sortOrderByType[bType]);
      }
      switch (aType) {
          case "null":
              return 0;
          case "boolean":
              if (a === b) {
                  return 0;
              }
              return a === false ? -1 : 1;
          case "number":
              return Math.sign(a - b);
          case "string": {
              var aString = a;
              var bString = b;
              return aString < bString ? -1 : aString > bString ? 1 : 0;
          }
          case "array": {
              var aArray = a;
              var bArray = b;
              if (aArray.length < bArray.length) {
                  return -1;
              }
              else if (aArray.length > bArray.length) {
                  return 1;
              }
              for (var i = 0; i < aArray.length; i += 1) {
                  var innerComp = compare(aArray[i], bArray[i]);
                  if (innerComp !== 0) {
                      return innerComp;
                  }
              }
              return 0;
          }
          case "object": {
              var aObj = a;
              var bObj = b;
              return Math.sign(Object.keys(aObj).length - Object.keys(bObj).length);
          }
          case "regexp": {
              var aString = a.source;
              var bString = b.source;
              return aString < bString ? -1 : aString > bString ? 1 : 0;
          }
          case "unknown":
              return 0;
      }
  }
  function deepEqual(a, b, sourceCodeInfo) {
      if (a === b) {
          return true;
      }
      if (typeof a === "number" && typeof b === "number") {
          return Math.abs(a - b) < Number.EPSILON;
      }
      if (array.is(a) && array.is(b)) {
          if (a.length !== b.length) {
              return false;
          }
          for (var i = 0; i < a.length; i += 1) {
              if (!deepEqual(any.as(a[i], sourceCodeInfo), any.as(b[i], sourceCodeInfo), sourceCodeInfo)) {
                  return false;
              }
          }
          return true;
      }
      if (a instanceof RegExp && b instanceof RegExp) {
          return a.toString() === b.toString();
      }
      if (typeof a === "object" && a !== null && typeof b === "object" && b !== null) {
          var aObj = a;
          var bObj = b;
          var aKeys = Object.keys(aObj);
          var bKeys = Object.keys(bObj);
          if (aKeys.length !== bKeys.length) {
              return false;
          }
          for (var i = 0; i < aKeys.length; i += 1) {
              var key = string.as(aKeys[i], sourceCodeInfo);
              if (!deepEqual(toAny(aObj[key]), toAny(bObj[key]), sourceCodeInfo)) {
                  return false;
              }
          }
          return true;
      }
      return false;
  }
  function toNonNegativeInteger(number) {
      return Math.max(0, Math.ceil(number));
  }
  function toAny(value) {
      return (value !== null && value !== void 0 ? value : null);
  }
  function clone(value) {
      if (object.is(value)) {
          return Object.entries(value).reduce(function (result, entry) {
              var key = entry[0], val = entry[1];
              result[key] = clone(val);
              return result;
          }, {});
      }
      if (array.is(value)) {
          return value.map(function (item) { return clone(item); });
      }
      return value;
  }
  function cloneColl(value) {
      return clone(value);
  }
  function createContextFromValues(values) {
      if (!values) {
          return {};
      }
      return Object.entries(values).reduce(function (context, _a) {
          var key = _a[0], value = _a[1];
          context[key] = { value: toAny(value) };
          return context;
      }, {});
  }

  var whenFirstSpecialExpression = {
      parse: function (tokens, position, _a) {
          var _b, _c;
          var parseBindings = _a.parseBindings, parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var bindings;
          _b = parseBindings(tokens, position), position = _b[0], bindings = _b[1];
          if (bindings.length !== 1) {
              throw new LitsError("Expected exactly one binding, got " + valueToString$1(bindings.length), firstToken.sourceCodeInfo);
          }
          var params;
          _c = parseTokens(tokens, position), position = _c[0], params = _c[1];
          var node = {
              type: "SpecialExpression",
              name: "when-first",
              binding: asValue(bindings[0], firstToken.sourceCodeInfo),
              params: params,
              token: firstToken,
          };
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var locals = {};
          var evaluatedBindingForm = evaluateAstNode(node.binding.value, contextStack);
          if (!sequence.is(evaluatedBindingForm)) {
              throw new LitsError("Expected undefined or a sequence, got " + valueToString$1(evaluatedBindingForm), node.token.sourceCodeInfo);
          }
          if (evaluatedBindingForm.length === 0) {
              return null;
          }
          var bindingValue = toAny(evaluatedBindingForm[0]);
          locals[node.binding.name] = { value: bindingValue };
          var newContextStack = contextStack.withContext(locals);
          var result = null;
          for (var _i = 0, _b = node.params; _i < _b.length; _i++) {
              var form = _b[_i];
              result = evaluateAstNode(form, newContextStack);
          }
          return result;
      },
      validate: function (node) { return assertNumberOfParams({ min: 0 }, node); },
  };

  var whenLetSpecialExpression = {
      parse: function (tokens, position, _a) {
          var _b, _c;
          var parseBindings = _a.parseBindings, parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var bindings;
          _b = parseBindings(tokens, position), position = _b[0], bindings = _b[1];
          if (bindings.length !== 1) {
              throw new LitsError("Expected exactly one binding, got " + valueToString$1(bindings.length), firstToken.sourceCodeInfo);
          }
          var params;
          _c = parseTokens(tokens, position), position = _c[0], params = _c[1];
          var node = {
              type: "SpecialExpression",
              name: "when-let",
              binding: asValue(bindings[0], firstToken.sourceCodeInfo),
              params: params,
              token: firstToken,
          };
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var locals = {};
          var bindingValue = evaluateAstNode(node.binding.value, contextStack);
          if (!bindingValue) {
              return null;
          }
          locals[node.binding.name] = { value: bindingValue };
          var newContextStack = contextStack.withContext(locals);
          var result = null;
          for (var _i = 0, _b = node.params; _i < _b.length; _i++) {
              var form = _b[_i];
              result = evaluateAstNode(form, newContextStack);
          }
          return result;
      },
      validate: function (node) { return assertNumberOfParams({ min: 0 }, node); },
  };

  var whenNotSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
          var node = {
              type: "SpecialExpression",
              name: "when-not",
              params: params,
              token: firstToken,
          };
          return [newPosition + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var _b = node.params, whenExpression = _b[0], body = _b.slice(1);
          astNode.assert(whenExpression, node.token.sourceCodeInfo);
          if (evaluateAstNode(whenExpression, contextStack)) {
              return null;
          }
          var result = null;
          for (var _i = 0, body_1 = body; _i < body_1.length; _i++) {
              var form = body_1[_i];
              result = evaluateAstNode(form, contextStack);
          }
          return result;
      },
      validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
  };

  var whenSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
          var node = {
              type: "SpecialExpression",
              name: "when",
              params: params,
              token: firstToken,
          };
          return [newPosition + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var _b = node.params, whenExpression = _b[0], body = _b.slice(1);
          astNode.assert(whenExpression, node.token.sourceCodeInfo);
          if (!evaluateAstNode(whenExpression, contextStack)) {
              return null;
          }
          var result = null;
          for (var _i = 0, body_1 = body; _i < body_1.length; _i++) {
              var form = body_1[_i];
              result = evaluateAstNode(form, contextStack);
          }
          return result;
      },
      validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
  };

  var bitwiseNormalExpression = {
      'bit-shift-left': {
          evaluate: function (_a, sourceCodeInfo) {
              var num = _a[0], count = _a[1];
              number.assert(num, sourceCodeInfo, { integer: true });
              number.assert(count, sourceCodeInfo, { integer: true, nonNegative: true });
              return num << count;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'bit-shift-right': {
          evaluate: function (_a, sourceCodeInfo) {
              var num = _a[0], count = _a[1];
              number.assert(num, sourceCodeInfo, { integer: true });
              number.assert(count, sourceCodeInfo, { integer: true, nonNegative: true });
              return num >> count;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'bit-not': {
          evaluate: function (_a, sourceCodeInfo) {
              var num = _a[0];
              number.assert(num, sourceCodeInfo, { integer: true });
              return ~num;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'bit-and': {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0], rest = _a.slice(1);
              number.assert(first, sourceCodeInfo, { integer: true });
              return rest.reduce(function (result, value) {
                  number.assert(value, sourceCodeInfo, { integer: true });
                  return result & value;
              }, first);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
      'bit-and-not': {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0], rest = _a.slice(1);
              number.assert(first, sourceCodeInfo, { integer: true });
              return rest.reduce(function (result, value) {
                  number.assert(value, sourceCodeInfo, { integer: true });
                  return result & ~value;
              }, first);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
      'bit-or': {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0], rest = _a.slice(1);
              number.assert(first, sourceCodeInfo, { integer: true });
              return rest.reduce(function (result, value) {
                  number.assert(value, sourceCodeInfo, { integer: true });
                  return result | value;
              }, first);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
      'bit-xor': {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0], rest = _a.slice(1);
              number.assert(first, sourceCodeInfo, { integer: true });
              return rest.reduce(function (result, value) {
                  number.assert(value, sourceCodeInfo, { integer: true });
                  return result ^ value;
              }, first);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
      'bit-flip': {
          evaluate: function (_a, sourceCodeInfo) {
              var num = _a[0], index = _a[1];
              number.assert(num, sourceCodeInfo, { integer: true });
              number.assert(index, sourceCodeInfo, { integer: true, nonNegative: true });
              var mask = 1 << index;
              return (num ^= mask);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'bit-set': {
          evaluate: function (_a, sourceCodeInfo) {
              var num = _a[0], index = _a[1];
              number.assert(num, sourceCodeInfo, { integer: true });
              number.assert(index, sourceCodeInfo, { integer: true, nonNegative: true });
              var mask = 1 << index;
              return (num |= mask);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'bit-clear': {
          evaluate: function (_a, sourceCodeInfo) {
              var num = _a[0], index = _a[1];
              number.assert(num, sourceCodeInfo, { integer: true });
              number.assert(index, sourceCodeInfo, { integer: true, nonNegative: true });
              var mask = 1 << index;
              return (num &= ~mask);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'bit-test': {
          evaluate: function (_a, sourceCodeInfo) {
              var num = _a[0], index = _a[1];
              number.assert(num, sourceCodeInfo, { integer: true });
              number.assert(index, sourceCodeInfo, { integer: true, nonNegative: true });
              var mask = 1 << index;
              return !!(num & mask);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
  };

  function cloneAndGetMeta(originalColl, keys, sourceCodeInfo) {
      var coll = cloneColl(originalColl);
      var butLastKeys = keys.slice(0, keys.length - 1);
      var innerCollMeta = butLastKeys.reduce(function (result, key) {
          var resultColl = result.coll;
          var newResultColl;
          if (array.is(resultColl)) {
              number.assert(key, sourceCodeInfo);
              // eslint-disable-next-line @typescript-eslint/no-explicit-any
              newResultColl = collection.as(resultColl[key], sourceCodeInfo);
          }
          else {
              object.assert(resultColl, sourceCodeInfo);
              string.assert(key, sourceCodeInfo);
              if (!collHasKey(result.coll, key)) {
                  resultColl[key] = {};
              }
              newResultColl = collection.as(resultColl[key], sourceCodeInfo);
          }
          return { coll: newResultColl, parent: resultColl };
      }, { coll: coll, parent: {} });
      return { coll: coll, innerCollMeta: innerCollMeta };
  }
  function get(coll, key, sourceCodeInfo) {
      if (array.is(coll)) {
          number.assert(key, sourceCodeInfo, { integer: true });
          if (key < coll.length) {
              return toAny(coll[key]);
          }
      }
      else if (object.is(coll)) {
          string.assert(key, sourceCodeInfo);
          if (collHasKey(coll, key)) {
              return toAny(coll[key]);
          }
      }
      else {
          number.assert(key, sourceCodeInfo, { integer: true });
          if (key < coll.length) {
              return toAny(coll[key]);
          }
      }
      return undefined;
  }
  function update(coll, key, fn, params, sourceCodeInfo, contextStack, executeFunction) {
      if (object.is(coll)) {
          string.assert(key, sourceCodeInfo);
          var result = __assign({}, coll);
          result[key] = executeFunction(fn, __spreadArray([result[key]], params), sourceCodeInfo, contextStack);
          return result;
      }
      else {
          number.assert(key, sourceCodeInfo);
          var intKey_1 = toNonNegativeInteger(key);
          number.assert(intKey_1, sourceCodeInfo, { lte: coll.length });
          if (Array.isArray(coll)) {
              var result = coll.map(function (elem, index) {
                  if (intKey_1 === index) {
                      return executeFunction(fn, __spreadArray([elem], params), sourceCodeInfo, contextStack);
                  }
                  return elem;
              });
              if (intKey_1 === coll.length) {
                  result[intKey_1] = executeFunction(fn, __spreadArray([undefined], params), sourceCodeInfo, contextStack);
              }
              return result;
          }
          else {
              var result = coll.split("").map(function (elem, index) {
                  if (intKey_1 === index) {
                      return string.as(executeFunction(fn, __spreadArray([elem], params), sourceCodeInfo, contextStack), sourceCodeInfo, {
                          char: true,
                      });
                  }
                  return elem;
              });
              if (intKey_1 === coll.length) {
                  result[intKey_1] = string.as(executeFunction(fn, __spreadArray([undefined], params), sourceCodeInfo, contextStack), sourceCodeInfo, {
                      char: true,
                  });
              }
              return result.join("");
          }
      }
  }
  function assoc(coll, key, value, sourceCodeInfo) {
      collection.assert(coll, sourceCodeInfo);
      stringOrNumber.assert(key, sourceCodeInfo);
      if (Array.isArray(coll) || typeof coll === "string") {
          number.assert(key, sourceCodeInfo, { integer: true });
          number.assert(key, sourceCodeInfo, { gte: 0 });
          number.assert(key, sourceCodeInfo, { lte: coll.length });
          if (typeof coll === "string") {
              string.assert(value, sourceCodeInfo, { char: true });
              return "" + coll.slice(0, key) + value + coll.slice(key + 1);
          }
          var copy_1 = __spreadArray([], coll);
          copy_1[key] = value;
          return copy_1;
      }
      string.assert(key, sourceCodeInfo);
      var copy = __assign({}, coll);
      copy[key] = value;
      return copy;
  }
  var collectionNormalExpression = {
      get: {
          evaluate: function (params, sourceCodeInfo) {
              var coll = params[0], key = params[1];
              var defaultValue = toAny(params[2]);
              collection.assert(coll, sourceCodeInfo);
              stringOrNumber.assert(key, sourceCodeInfo);
              var result = get(coll, key, sourceCodeInfo);
              return result === undefined ? defaultValue : result;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'get-in': {
          evaluate: function (params, sourceCodeInfo) {
              var coll = params[0];
              var keys = params[1];
              var defaultValue = toAny(params[2]);
              collection.assert(coll, sourceCodeInfo);
              array.assert(keys, sourceCodeInfo);
              for (var _i = 0, keys_1 = keys; _i < keys_1.length; _i++) {
                  var key = keys_1[_i];
                  stringOrNumber.assert(key, sourceCodeInfo);
                  if (collection.is(coll)) {
                      coll = get(coll, key, sourceCodeInfo);
                  }
                  else {
                      return defaultValue;
                  }
              }
              return any.is(coll) ? coll : defaultValue;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      count: {
          evaluate: function (_a, sourceCodeInfo) {
              var coll = _a[0];
              if (typeof coll === "string") {
                  return coll.length;
              }
              collection.assert(coll, sourceCodeInfo);
              if (Array.isArray(coll)) {
                  return coll.length;
              }
              return Object.keys(coll).length;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'contains?': {
          evaluate: function (_a, sourceCodeInfo) {
              var coll = _a[0], key = _a[1];
              collection.assert(coll, sourceCodeInfo);
              stringOrNumber.assert(key, sourceCodeInfo);
              if (sequence.is(coll)) {
                  if (!number.is(key, { integer: true })) {
                      return false;
                  }
                  number.assert(key, sourceCodeInfo, { integer: true });
                  return key >= 0 && key < coll.length;
              }
              return !!Object.getOwnPropertyDescriptor(coll, key);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'has?': {
          evaluate: function (_a, sourceCodeInfo) {
              var coll = _a[0], value = _a[1];
              collection.assert(coll, sourceCodeInfo);
              if (array.is(coll)) {
                  return coll.includes(value);
              }
              if (string.is(coll)) {
                  return string.is(value) ? coll.split("").includes(value) : false;
              }
              return Object.values(coll).includes(value);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'has-some?': {
          evaluate: function (_a, sourceCodeInfo) {
              var coll = _a[0], seq = _a[1];
              collection.assert(coll, sourceCodeInfo);
              sequence.assert(seq, sourceCodeInfo);
              if (array.is(coll)) {
                  for (var _i = 0, seq_1 = seq; _i < seq_1.length; _i++) {
                      var value = seq_1[_i];
                      if (coll.includes(value)) {
                          return true;
                      }
                  }
                  return false;
              }
              if (string.is(coll)) {
                  for (var _b = 0, seq_2 = seq; _b < seq_2.length; _b++) {
                      var value = seq_2[_b];
                      if (string.is(value, { char: true }) ? coll.split("").includes(value) : false) {
                          return true;
                      }
                  }
                  return false;
              }
              for (var _c = 0, seq_3 = seq; _c < seq_3.length; _c++) {
                  var value = seq_3[_c];
                  if (Object.values(coll).includes(value)) {
                      return true;
                  }
              }
              return false;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'has-every?': {
          evaluate: function (_a, sourceCodeInfo) {
              var coll = _a[0], seq = _a[1];
              collection.assert(coll, sourceCodeInfo);
              sequence.assert(seq, sourceCodeInfo);
              if (array.is(coll)) {
                  for (var _i = 0, seq_4 = seq; _i < seq_4.length; _i++) {
                      var value = seq_4[_i];
                      if (!coll.includes(value)) {
                          return false;
                      }
                  }
                  return true;
              }
              if (string.is(coll)) {
                  for (var _b = 0, seq_5 = seq; _b < seq_5.length; _b++) {
                      var value = seq_5[_b];
                      if (!string.is(value, { char: true }) || !coll.split("").includes(value)) {
                          return false;
                      }
                  }
                  return true;
              }
              for (var _c = 0, seq_6 = seq; _c < seq_6.length; _c++) {
                  var value = seq_6[_c];
                  if (!Object.values(coll).includes(value)) {
                      return false;
                  }
              }
              return true;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      assoc: {
          evaluate: function (_a, sourceCodeInfo) {
              var coll = _a[0], key = _a[1], value = _a[2];
              collection.assert(coll, sourceCodeInfo);
              stringOrNumber.assert(key, sourceCodeInfo);
              any.assert(value, sourceCodeInfo);
              return assoc(coll, key, value, sourceCodeInfo);
          },
          validate: function (node) { return assertNumberOfParams(3, node); },
      },
      'assoc-in': {
          evaluate: function (_a, sourceCodeInfo) {
              var originalColl = _a[0], keys = _a[1], value = _a[2];
              collection.assert(originalColl, sourceCodeInfo);
              array.assert(keys, sourceCodeInfo);
              any.assert(value, sourceCodeInfo);
              if (keys.length === 1) {
                  stringOrNumber.assert(keys[0], sourceCodeInfo);
                  return assoc(originalColl, keys[0], value, sourceCodeInfo);
              }
              var _b = cloneAndGetMeta(originalColl, keys, sourceCodeInfo), coll = _b.coll, innerCollMeta = _b.innerCollMeta;
              var lastKey = stringOrNumber.as(keys[keys.length - 1], sourceCodeInfo);
              var parentKey = stringOrNumber.as(keys[keys.length - 2], sourceCodeInfo);
              if (array.is(innerCollMeta.parent)) {
                  number.assert(parentKey, sourceCodeInfo);
                  innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value, sourceCodeInfo);
              }
              else {
                  string.assert(parentKey, sourceCodeInfo);
                  innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value, sourceCodeInfo);
              }
              return coll;
          },
          validate: function (node) { return assertNumberOfParams(3, node); },
      },
      update: {
          evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
              var coll = _a[0], key = _a[1], fn = _a[2], params = _a.slice(3);
              var executeFunction = _b.executeFunction;
              collection.assert(coll, sourceCodeInfo);
              stringOrNumber.assert(key, sourceCodeInfo);
              litsFunction.assert(fn, sourceCodeInfo);
              return update(coll, key, fn, params, sourceCodeInfo, contextStack, executeFunction);
          },
          validate: function (node) { return assertNumberOfParams({ min: 3 }, node); },
      },
      'update-in': {
          evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
              var originalColl = _a[0], keys = _a[1], fn = _a[2], params = _a.slice(3);
              var executeFunction = _b.executeFunction;
              collection.assert(originalColl, sourceCodeInfo);
              array.assert(keys, sourceCodeInfo);
              litsFunction.assert(fn, sourceCodeInfo);
              if (keys.length === 1) {
                  stringOrNumber.assert(keys[0], sourceCodeInfo);
                  return update(originalColl, keys[0], fn, params, sourceCodeInfo, contextStack, executeFunction);
              }
              var _c = cloneAndGetMeta(originalColl, keys, sourceCodeInfo), coll = _c.coll, innerCollMeta = _c.innerCollMeta;
              var lastKey = stringOrNumber.as(keys[keys.length - 1], sourceCodeInfo);
              var parentKey = stringOrNumber.as(keys[keys.length - 2], sourceCodeInfo);
              if (array.is(innerCollMeta.parent)) {
                  number.assert(parentKey, sourceCodeInfo);
                  innerCollMeta.parent[parentKey] = update(innerCollMeta.coll, lastKey, fn, params, sourceCodeInfo, contextStack, executeFunction);
              }
              else {
                  string.assert(parentKey, sourceCodeInfo);
                  innerCollMeta.parent[parentKey] = update(innerCollMeta.coll, lastKey, fn, params, sourceCodeInfo, contextStack, executeFunction);
              }
              return coll;
          },
          validate: function (node) { return assertNumberOfParams({ min: 3 }, node); },
      },
      concat: {
          evaluate: function (params, sourceCodeInfo) {
              collection.assert(params[0], sourceCodeInfo);
              if (array.is(params[0])) {
                  return params.reduce(function (result, arr) {
                      array.assert(arr, sourceCodeInfo);
                      return result.concat(arr);
                  }, []);
              }
              else if (string.is(params[0])) {
                  return params.reduce(function (result, s) {
                      string.assert(s, sourceCodeInfo);
                      return "" + result + s;
                  }, "");
              }
              else {
                  return params.reduce(function (result, obj) {
                      object.assert(obj, sourceCodeInfo);
                      return Object.assign(result, obj);
                  }, {});
              }
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      'empty?': {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              collection.assert(first, sourceCodeInfo);
              if (string.is(first)) {
                  return first.length === 0;
              }
              if (Array.isArray(first)) {
                  return first.length === 0;
              }
              return Object.keys(first).length === 0;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'every?': {
          evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
              var fn = _a[0], coll = _a[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, sourceCodeInfo);
              collection.assert(coll, sourceCodeInfo);
              if (Array.isArray(coll)) {
                  return coll.every(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
              }
              if (string.is(coll)) {
                  return coll.split("").every(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
              }
              return Object.entries(coll).every(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'any?': {
          evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
              var fn = _a[0], coll = _a[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, sourceCodeInfo);
              collection.assert(coll, sourceCodeInfo);
              if (Array.isArray(coll)) {
                  return coll.some(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
              }
              if (string.is(coll)) {
                  return coll.split("").some(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
              }
              return Object.entries(coll).some(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'not-any?': {
          evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
              var fn = _a[0], coll = _a[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, sourceCodeInfo);
              collection.assert(coll, sourceCodeInfo);
              if (Array.isArray(coll)) {
                  return !coll.some(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
              }
              if (string.is(coll)) {
                  return !coll.split("").some(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
              }
              return !Object.entries(coll).some(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'not-every?': {
          evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
              var fn = _a[0], coll = _a[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, sourceCodeInfo);
              collection.assert(coll, sourceCodeInfo);
              if (Array.isArray(coll)) {
                  return !coll.every(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
              }
              if (string.is(coll)) {
                  return !coll.split("").every(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
              }
              return !Object.entries(coll).every(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
  };

  var evaluateMap = function (params, sourceCodeInfo, contextStack, _a) {
      var executeFunction = _a.executeFunction;
      var fn = params[0], firstList = params[1];
      litsFunction.assert(fn, sourceCodeInfo);
      sequence.assert(firstList, sourceCodeInfo);
      var isStringSeq = string.is(firstList);
      var length = firstList.length;
      if (params.length === 2) {
          if (array.is(firstList)) {
              return firstList.map(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
          }
          else {
              return firstList
                  .split("")
                  .map(function (elem) {
                  var newVal = executeFunction(fn, [elem], sourceCodeInfo, contextStack);
                  string.assert(newVal, sourceCodeInfo, { char: true });
                  return newVal;
              })
                  .join("");
          }
      }
      else {
          params.slice(2).forEach(function (collParam) {
              if (isStringSeq) {
                  string.assert(collParam, sourceCodeInfo);
              }
              else {
                  array.assert(collParam, sourceCodeInfo);
              }
              if (length !== collParam.length) {
                  throw new LitsError("All arguments to \"map\" must have the same length.", sourceCodeInfo);
              }
          });
          if (isStringSeq) {
              var result = "";
              var _loop_1 = function (i) {
                  var fnParams = params.slice(1).map(function (l) { return l[i]; });
                  var newValue = executeFunction(fn, fnParams, sourceCodeInfo, contextStack);
                  string.assert(newValue, sourceCodeInfo, { char: true });
                  result += newValue;
              };
              for (var i = 0; i < length; i += 1) {
                  _loop_1(i);
              }
              return result;
          }
          else {
              var result = [];
              var _loop_2 = function (i) {
                  var fnParams = params.slice(1).map(function (l) { return toAny(l[i]); });
                  result.push(executeFunction(fn, fnParams, sourceCodeInfo, contextStack));
              };
              for (var i = 0; i < length; i += 1) {
                  _loop_2(i);
              }
              return result;
          }
      }
  };
  var sequenceNormalExpression = {
      cons: {
          evaluate: function (_a, sourceCodeInfo) {
              var elem = _a[0], seq = _a[1];
              any.assert(elem, sourceCodeInfo);
              sequence.assert(seq, sourceCodeInfo);
              if (Array.isArray(seq)) {
                  return __spreadArray([elem], seq);
              }
              string.assert(elem, sourceCodeInfo, { char: true });
              return "" + elem + seq;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      nth: {
          evaluate: function (_a, sourceCodeInfo) {
              var seq = _a[0], i = _a[1];
              sequence.assert(seq, sourceCodeInfo);
              number.assert(i, sourceCodeInfo, { integer: true });
              return toAny(seq[i]);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      filter: {
          evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
              var fn = _a[0], seq = _a[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, sourceCodeInfo);
              sequence.assert(seq, sourceCodeInfo);
              if (Array.isArray(seq)) {
                  return seq.filter(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
              }
              return seq
                  .split("")
                  .filter(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); })
                  .join("");
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      first: {
          evaluate: function (_a, sourceCodeInfo) {
              var array = _a[0];
              sequence.assert(array, sourceCodeInfo);
              return toAny(array[0]);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      last: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              sequence.assert(first, sourceCodeInfo);
              return toAny(first[first.length - 1]);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      map: {
          evaluate: evaluateMap,
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
      pop: {
          evaluate: function (_a, sourceCodeInfo) {
              var seq = _a[0];
              sequence.assert(seq, sourceCodeInfo);
              if (string.is(seq)) {
                  return seq.substr(0, seq.length - 1);
              }
              var copy = __spreadArray([], seq);
              copy.pop();
              return copy;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      position: {
          evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
              var fn = _a[0], seq = _a[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, sourceCodeInfo);
              sequence.assert(seq, sourceCodeInfo);
              if (string.is(seq)) {
                  var index = seq.split("").findIndex(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
                  return index !== -1 ? index : null;
              }
              else {
                  var index = seq.findIndex(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
                  return index !== -1 ? index : null;
              }
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'index-of': {
          evaluate: function (_a, sourceCodeInfo) {
              var seq = _a[0], value = _a[1];
              any.assert(value, sourceCodeInfo);
              sequence.assert(seq, sourceCodeInfo);
              if (string.is(seq)) {
                  string.assert(value, sourceCodeInfo);
                  var index = seq.indexOf(value);
                  return index !== -1 ? index : null;
              }
              else {
                  var index = seq.indexOf(value);
                  return index !== -1 ? index : null;
              }
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      push: {
          evaluate: function (_a, sourceCodeInfo) {
              var seq = _a[0], values = _a.slice(1);
              sequence.assert(seq, sourceCodeInfo);
              if (string.is(seq)) {
                  charArray.assert(values, sourceCodeInfo);
                  return __spreadArray([seq], values).join("");
              }
              else {
                  return __spreadArray(__spreadArray([], seq, true), values);
              }
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
      reduce: {
          evaluate: function (params, sourceCodeInfo, contextStack, _a) {
              var executeFunction = _a.executeFunction;
              var fn = params[0];
              litsFunction.assert(fn, sourceCodeInfo);
              if (params.length === 2) {
                  var arr = params[1];
                  sequence.assert(arr, sourceCodeInfo);
                  if (arr.length === 0) {
                      return executeFunction(fn, [], sourceCodeInfo, contextStack);
                  }
                  else if (arr.length === 1) {
                      return toAny(arr[0]);
                  }
                  if (string.is(arr)) {
                      var chars = arr.split("");
                      return chars.slice(1).reduce(function (result, elem) {
                          var val = executeFunction(fn, [result, elem], sourceCodeInfo, contextStack);
                          return val;
                      }, any.as(chars[0], sourceCodeInfo));
                  }
                  else {
                      return arr.slice(1).reduce(function (result, elem) {
                          return executeFunction(fn, [result, elem], sourceCodeInfo, contextStack);
                      }, toAny(arr[0]));
                  }
              }
              else {
                  var val = params[1], seq = params[2];
                  any.assert(val, sourceCodeInfo);
                  sequence.assert(seq, sourceCodeInfo);
                  if (string.is(seq)) {
                      string.assert(val, sourceCodeInfo);
                      if (seq.length === 0) {
                          return val;
                      }
                      return seq.split("").reduce(function (result, elem) {
                          var newVal = executeFunction(fn, [result, elem], sourceCodeInfo, contextStack);
                          return newVal;
                      }, val);
                  }
                  else {
                      if (seq.length === 0) {
                          return val;
                      }
                      return seq.reduce(function (result, elem) {
                          return executeFunction(fn, [result, elem], sourceCodeInfo, contextStack);
                      }, val);
                  }
              }
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'reduce-right': {
          evaluate: function (params, sourceCodeInfo, contextStack, _a) {
              var executeFunction = _a.executeFunction;
              var fn = params[0];
              litsFunction.assert(fn, sourceCodeInfo);
              if (params.length === 2) {
                  var seq = params[1];
                  sequence.assert(seq, sourceCodeInfo);
                  if (seq.length === 0) {
                      return executeFunction(fn, [], sourceCodeInfo, contextStack);
                  }
                  else if (seq.length === 1) {
                      return toAny(seq[0]);
                  }
                  if (string.is(seq)) {
                      var chars = seq.split("");
                      return chars.slice(0, chars.length - 1).reduceRight(function (result, elem) {
                          var newVal = executeFunction(fn, [result, elem], sourceCodeInfo, contextStack);
                          string.assert(newVal, sourceCodeInfo);
                          return newVal;
                      }, chars[chars.length - 1]);
                  }
                  else {
                      return seq.slice(0, seq.length - 1).reduceRight(function (result, elem) {
                          return executeFunction(fn, [result, elem], sourceCodeInfo, contextStack);
                      }, any.as(seq[seq.length - 1], sourceCodeInfo));
                  }
              }
              else {
                  var val = params[1], seq = params[2];
                  any.assert(val, sourceCodeInfo);
                  sequence.assert(seq, sourceCodeInfo);
                  if (string.is(seq)) {
                      if (seq.length === 0) {
                          return val;
                      }
                      return seq.split("").reduceRight(function (result, elem) {
                          var newVal = executeFunction(fn, [result, elem], sourceCodeInfo, contextStack);
                          return newVal;
                      }, val);
                  }
                  else {
                      if (seq.length === 0) {
                          return val;
                      }
                      return seq.reduceRight(function (result, elem) {
                          return executeFunction(fn, [result, elem], sourceCodeInfo, contextStack);
                      }, val);
                  }
              }
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      rest: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              sequence.assert(first, sourceCodeInfo);
              if (Array.isArray(first)) {
                  if (first.length <= 1) {
                      return [];
                  }
                  return first.slice(1);
              }
              return first.substr(1);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      nthrest: {
          evaluate: function (_a, sourceCodeInfo) {
              var seq = _a[0], count = _a[1];
              sequence.assert(seq, sourceCodeInfo);
              number.assert(count, sourceCodeInfo, { finite: true });
              var integerCount = Math.max(Math.ceil(count), 0);
              if (Array.isArray(seq)) {
                  return seq.slice(integerCount);
              }
              return seq.substr(integerCount);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      next: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              sequence.assert(first, sourceCodeInfo);
              if (Array.isArray(first)) {
                  if (first.length <= 1) {
                      return null;
                  }
                  return first.slice(1);
              }
              if (first.length <= 1) {
                  return null;
              }
              return first.substr(1);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      nthnext: {
          evaluate: function (_a, sourceCodeInfo) {
              var seq = _a[0], count = _a[1];
              sequence.assert(seq, sourceCodeInfo);
              number.assert(count, sourceCodeInfo, { finite: true });
              var integerCount = Math.max(Math.ceil(count), 0);
              if (seq.length <= count) {
                  return null;
              }
              if (Array.isArray(seq)) {
                  return seq.slice(integerCount);
              }
              return seq.substr(integerCount);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      reverse: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              sequence.assert(first, sourceCodeInfo);
              if (Array.isArray(first)) {
                  return __spreadArray([], first).reverse();
              }
              return first.split("").reverse().join("");
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      second: {
          evaluate: function (_a, sourceCodeInfo) {
              var array = _a[0];
              sequence.assert(array, sourceCodeInfo);
              return toAny(array[1]);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      shift: {
          evaluate: function (_a, sourceCodeInfo) {
              var seq = _a[0];
              sequence.assert(seq, sourceCodeInfo);
              if (string.is(seq)) {
                  return seq.substr(1);
              }
              var copy = __spreadArray([], seq);
              copy.shift();
              return copy;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      slice: {
          evaluate: function (params, sourceCodeInfo) {
              var seq = params[0], from = params[1], to = params[2];
              sequence.assert(seq, sourceCodeInfo);
              if (params.length === 1) {
                  return seq;
              }
              number.assert(from, sourceCodeInfo, { integer: true });
              if (params.length === 2) {
                  return seq.slice(from);
              }
              number.assert(to, sourceCodeInfo, { integer: true });
              return seq.slice(from, to);
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 3 }, node); },
      },
      some: {
          evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
              var _c;
              var fn = _a[0], seq = _a[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, sourceCodeInfo);
              sequence.assert(seq, sourceCodeInfo);
              if (seq.length === 0) {
                  return null;
              }
              if (string.is(seq)) {
                  return (_c = seq.split("").find(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); })) !== null && _c !== void 0 ? _c : null;
              }
              return toAny(seq.find(function (elem) { return executeFunction(fn, [elem], sourceCodeInfo, contextStack); }));
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      sort: {
          evaluate: function (params, sourceCodeInfo, contextStack, _a) {
              var executeFunction = _a.executeFunction;
              var defaultComparer = params.length === 1;
              var seq = defaultComparer ? params[0] : params[1];
              var comparer = defaultComparer ? null : params[0];
              sequence.assert(seq, sourceCodeInfo);
              if (string.is(seq)) {
                  var result_1 = seq.split("");
                  if (defaultComparer) {
                      result_1.sort(compare);
                  }
                  else {
                      litsFunction.assert(comparer, sourceCodeInfo);
                      result_1.sort(function (a, b) {
                          var compareValue = executeFunction(comparer, [a, b], sourceCodeInfo, contextStack);
                          number.assert(compareValue, sourceCodeInfo, { finite: true });
                          return compareValue;
                      });
                  }
                  return result_1.join("");
              }
              var result = __spreadArray([], seq);
              if (defaultComparer) {
                  result.sort(compare);
              }
              else {
                  result.sort(function (a, b) {
                      litsFunction.assert(comparer, sourceCodeInfo);
                      var compareValue = executeFunction(comparer, [a, b], sourceCodeInfo, contextStack);
                      number.assert(compareValue, sourceCodeInfo, { finite: true });
                      return compareValue;
                  });
              }
              return result;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
      'sort-by': {
          evaluate: function (params, sourceCodeInfo, contextStack, _a) {
              var executeFunction = _a.executeFunction;
              var defaultComparer = params.length === 2;
              var keyfn = any.as(params[0], sourceCodeInfo);
              var comparer = defaultComparer ? null : params[1];
              var seq = sequence.as(defaultComparer ? params[1] : params[2], sourceCodeInfo);
              if (string.is(seq)) {
                  var result_2 = seq.split("");
                  if (defaultComparer) {
                      result_2.sort(function (a, b) {
                          var aKey = executeFunction(keyfn, [a], sourceCodeInfo, contextStack);
                          var bKey = executeFunction(keyfn, [b], sourceCodeInfo, contextStack);
                          return compare(aKey, bKey);
                      });
                  }
                  else {
                      litsFunction.assert(comparer, sourceCodeInfo);
                      result_2.sort(function (a, b) {
                          var aKey = executeFunction(keyfn, [a], sourceCodeInfo, contextStack);
                          var bKey = executeFunction(keyfn, [b], sourceCodeInfo, contextStack);
                          var compareValue = executeFunction(comparer, [aKey, bKey], sourceCodeInfo, contextStack);
                          number.assert(compareValue, sourceCodeInfo, { finite: true });
                          return compareValue;
                      });
                  }
                  return result_2.join("");
              }
              var result = __spreadArray([], seq);
              if (defaultComparer) {
                  result.sort(function (a, b) {
                      var aKey = executeFunction(keyfn, [a], sourceCodeInfo, contextStack);
                      var bKey = executeFunction(keyfn, [b], sourceCodeInfo, contextStack);
                      return compare(aKey, bKey);
                  });
              }
              else {
                  litsFunction.assert(comparer, sourceCodeInfo);
                  result.sort(function (a, b) {
                      var aKey = executeFunction(keyfn, [a], sourceCodeInfo, contextStack);
                      var bKey = executeFunction(keyfn, [b], sourceCodeInfo, contextStack);
                      var compareValue = executeFunction(comparer, [aKey, bKey], sourceCodeInfo, contextStack);
                      number.assert(compareValue, sourceCodeInfo, { finite: true });
                      return compareValue;
                  });
              }
              return result;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      take: {
          evaluate: function (_a, sourceCodeInfo) {
              var n = _a[0], input = _a[1];
              number.assert(n, sourceCodeInfo);
              sequence.assert(input, sourceCodeInfo);
              var num = Math.max(Math.ceil(n), 0);
              return input.slice(0, num);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'take-last': {
          evaluate: function (_a, sourceCodeInfo) {
              var n = _a[0], array = _a[1];
              sequence.assert(array, sourceCodeInfo);
              number.assert(n, sourceCodeInfo);
              var num = Math.max(Math.ceil(n), 0);
              var from = array.length - num;
              return array.slice(from);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'take-while': {
          evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
              var fn = _a[0], seq = _a[1];
              var executeFunction = _b.executeFunction;
              sequence.assert(seq, sourceCodeInfo);
              litsFunction.assert(fn, sourceCodeInfo);
              var result = [];
              for (var _i = 0, seq_1 = seq; _i < seq_1.length; _i++) {
                  var item = seq_1[_i];
                  if (executeFunction(fn, [item], sourceCodeInfo, contextStack)) {
                      result.push(item);
                  }
                  else {
                      break;
                  }
              }
              return string.is(seq) ? result.join("") : result;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      drop: {
          evaluate: function (_a, sourceCodeInfo) {
              var n = _a[0], input = _a[1];
              number.assert(n, sourceCodeInfo);
              var num = Math.max(Math.ceil(n), 0);
              sequence.assert(input, sourceCodeInfo);
              return input.slice(num);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'drop-last': {
          evaluate: function (_a, sourceCodeInfo) {
              var n = _a[0], array = _a[1];
              sequence.assert(array, sourceCodeInfo);
              number.assert(n, sourceCodeInfo);
              var num = Math.max(Math.ceil(n), 0);
              var from = array.length - num;
              return array.slice(0, from);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'drop-while': {
          evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
              var fn = _a[0], seq = _a[1];
              var executeFunction = _b.executeFunction;
              sequence.assert(seq, sourceCodeInfo);
              litsFunction.assert(fn, sourceCodeInfo);
              if (Array.isArray(seq)) {
                  var from_1 = seq.findIndex(function (elem) { return !executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
                  return seq.slice(from_1);
              }
              var charArray = seq.split("");
              var from = charArray.findIndex(function (elem) { return !executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
              return charArray.slice(from).join("");
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      unshift: {
          evaluate: function (_a, sourceCodeInfo) {
              var seq = _a[0], values = _a.slice(1);
              sequence.assert(seq, sourceCodeInfo);
              if (string.is(seq)) {
                  charArray.assert(values, sourceCodeInfo);
                  return __spreadArray(__spreadArray([], values, true), [seq]).join("");
              }
              var copy = __spreadArray([], seq);
              copy.unshift.apply(copy, values);
              return copy;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
      'random-sample!': {
          evaluate: function (_a, sourceCodeInfo) {
              var prob = _a[0], seq = _a[1];
              number.assert(prob, sourceCodeInfo, { finite: true });
              sequence.assert(seq, sourceCodeInfo);
              if (string.is(seq)) {
                  return seq
                      .split("")
                      .filter(function () { return Math.random() < prob; })
                      .join("");
              }
              else {
                  return seq.filter(function () { return Math.random() < prob; });
              }
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'rand-nth!': {
          evaluate: function (_a, sourceCodeInfo) {
              var seq = _a[0];
              sequence.assert(seq, sourceCodeInfo);
              if (seq.length === 0) {
                  return null;
              }
              var index = Math.floor(Math.random() * seq.length);
              if (string.is(seq)) {
                  return toAny(seq.split("")[index]);
              }
              return toAny(seq[index]);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      shuffle: {
          evaluate: function (_a, sourceCodeInfo) {
              var input = _a[0];
              sequence.assert(input, sourceCodeInfo);
              var array = string.is(input) ? __spreadArray([], input.split("")) : __spreadArray([], input);
              var remainingLength = array.length;
              var arrayElement;
              var pickedIndex;
              // Fisher–Yates Shuffle
              while (remainingLength) {
                  remainingLength -= 1;
                  // Pick a remaining element
                  pickedIndex = Math.floor(Math.random() * remainingLength);
                  // And swap it with the current element.
                  arrayElement = toAny(array[remainingLength]);
                  array[remainingLength] = toAny(array[pickedIndex]);
                  array[pickedIndex] = arrayElement;
              }
              return string.is(input) ? array.join("") : array;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      distinct: {
          evaluate: function (_a, sourceCodeInfo) {
              var input = _a[0];
              sequence.assert(input, sourceCodeInfo);
              if (Array.isArray(input)) {
                  return Array.from(new Set(input));
              }
              return Array.from(new Set(input.split(""))).join("");
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      remove: {
          evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
              var fn = _a[0], input = _a[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, sourceCodeInfo);
              sequence.assert(input, sourceCodeInfo);
              if (Array.isArray(input)) {
                  return input.filter(function (elem) { return !executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
              }
              return input
                  .split("")
                  .filter(function (elem) { return !executeFunction(fn, [elem], sourceCodeInfo, contextStack); })
                  .join("");
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'remove-at': {
          evaluate: function (_a, sourceCodeInfo) {
              var index = _a[0], input = _a[1];
              number.assert(index, sourceCodeInfo);
              sequence.assert(input, sourceCodeInfo);
              var intIndex = Math.ceil(index);
              if (intIndex < 0 || intIndex >= input.length) {
                  return input;
              }
              if (Array.isArray(input)) {
                  var copy = __spreadArray([], input);
                  copy.splice(index, 1);
                  return copy;
              }
              return "" + input.substring(0, index) + input.substring(index + 1);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'split-at': {
          evaluate: function (_a, sourceCodeInfo) {
              var pos = _a[0], seq = _a[1];
              number.assert(pos, sourceCodeInfo, { finite: true });
              var intPos = toNonNegativeInteger(pos);
              sequence.assert(seq, sourceCodeInfo);
              return [seq.slice(0, intPos), seq.slice(intPos)];
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'split-with': {
          evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
              var fn = _a[0], seq = _a[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, sourceCodeInfo);
              sequence.assert(seq, sourceCodeInfo);
              var seqIsArray = Array.isArray(seq);
              var arr = seqIsArray ? seq : seq.split("");
              var index = arr.findIndex(function (elem) { return !executeFunction(fn, [elem], sourceCodeInfo, contextStack); });
              if (index === -1) {
                  return [seq, seqIsArray ? [] : ""];
              }
              return [seq.slice(0, index), seq.slice(index)];
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      frequencies: {
          evaluate: function (_a, sourceCodeInfo) {
              var seq = _a[0];
              sequence.assert(seq, sourceCodeInfo);
              var arr = string.is(seq) ? seq.split("") : seq;
              return arr.reduce(function (result, val) {
                  string.assert(val, sourceCodeInfo);
                  if (collHasKey(result, val)) {
                      result[val] = result[val] + 1;
                  }
                  else {
                      result[val] = 1;
                  }
                  return result;
              }, {});
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'group-by': {
          evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
              var fn = _a[0], seq = _a[1];
              var executeFunction = _b.executeFunction;
              any.assert(fn, sourceCodeInfo);
              sequence.assert(seq, sourceCodeInfo);
              var arr = Array.isArray(seq) ? seq : seq.split("");
              return arr.reduce(function (result, val) {
                  var key = executeFunction(fn, [val], sourceCodeInfo, contextStack);
                  string.assert(key, sourceCodeInfo);
                  if (!collHasKey(result, key)) {
                      result[key] = [];
                  }
                  result[key].push(val);
                  return result;
              }, {});
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      partition: {
          evaluate: function (params, sourceCodeInfo) {
              var len = params.length;
              var n = toNonNegativeInteger(number.as(params[0], sourceCodeInfo));
              var seq = len === 2
                  ? sequence.as(params[1], sourceCodeInfo)
                  : len === 3
                      ? sequence.as(params[2], sourceCodeInfo)
                      : sequence.as(params[3], sourceCodeInfo);
              var step = len >= 3 ? toNonNegativeInteger(number.as(params[1], sourceCodeInfo)) : n;
              var pad = len === 4 ? (params[2] === null ? [] : array.as(params[2], sourceCodeInfo)) : undefined;
              return partition(n, step, seq, pad, sourceCodeInfo);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 4 }, node); },
      },
      'partition-all': {
          evaluate: function (params, sourceCodeInfo) {
              var len = params.length;
              var n = toNonNegativeInteger(number.as(params[0], sourceCodeInfo));
              var seq = len === 2 ? sequence.as(params[1], sourceCodeInfo) : sequence.as(params[2], sourceCodeInfo);
              var step = len >= 3 ? toNonNegativeInteger(number.as(params[1], sourceCodeInfo)) : n;
              return partition(n, step, seq, [], sourceCodeInfo);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'partition-by': {
          evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
              var fn = _a[0], seq = _a[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, sourceCodeInfo);
              sequence.assert(seq, sourceCodeInfo);
              var isStringSeq = string.is(seq);
              var oldValue = undefined;
              var result = (isStringSeq ? seq.split("") : seq).reduce(function (result, elem) {
                  var value = executeFunction(fn, [elem], sourceCodeInfo, contextStack);
                  if (value !== oldValue) {
                      result.push([]);
                      oldValue = value;
                  }
                  result[result.length - 1].push(elem);
                  return result;
              }, []);
              return isStringSeq ? result.map(function (elem) { return elem.join(""); }) : result;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
  };
  function partition(n, step, seq, pad, sourceCodeInfo) {
      number.assert(step, sourceCodeInfo, { positive: true });
      var isStringSeq = string.is(seq);
      var result = [];
      var start = 0;
      outer: while (start < seq.length) {
          var innerArr = [];
          for (var i = start; i < start + n; i += 1) {
              if (i >= seq.length) {
                  var padIndex = i - seq.length;
                  if (!pad) {
                      start += step;
                      continue outer;
                  }
                  if (padIndex >= pad.length) {
                      break;
                  }
                  innerArr.push(pad[padIndex]);
              }
              else {
                  innerArr.push(seq[i]);
              }
          }
          result.push(innerArr);
          start += step;
      }
      return isStringSeq ? result.map(function (x) { return x.join(""); }) : result;
  }

  var arrayNormalExpression = {
      array: {
          evaluate: function (params) { return params; },
      },
      range: {
          evaluate: function (params, sourceCodeInfo) {
              var first = params[0], second = params[1], third = params[2];
              var from;
              var to;
              var step;
              number.assert(first, sourceCodeInfo, { finite: true });
              if (params.length === 1) {
                  from = 0;
                  to = first;
                  step = to >= 0 ? 1 : -1;
              }
              else if (params.length === 2) {
                  number.assert(second, sourceCodeInfo, { finite: true });
                  from = first;
                  to = second;
                  step = to >= from ? 1 : -1;
              }
              else {
                  number.assert(second, sourceCodeInfo, { finite: true });
                  number.assert(third, sourceCodeInfo, { finite: true });
                  from = first;
                  to = second;
                  step = third;
                  if (to > from) {
                      number.assert(step, sourceCodeInfo, { positive: true });
                  }
                  else if (to < from) {
                      number.assert(step, sourceCodeInfo, { negative: true });
                  }
                  else {
                      number.assert(step, sourceCodeInfo, { nonZero: true });
                  }
              }
              var result = [];
              for (var i = from; step < 0 ? i > to : i < to; i += step) {
                  result.push(i);
              }
              return result;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 3 }, node); },
      },
      repeat: {
          evaluate: function (_a, sourceCodeInfo) {
              var count = _a[0], value = _a[1];
              number.assert(count, sourceCodeInfo, { integer: true, nonNegative: true });
              var result = [];
              for (var i = 0; i < count; i += 1) {
                  result.push(value);
              }
              return result;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      flatten: {
          evaluate: function (_a) {
              var seq = _a[0];
              if (!array.is(seq)) {
                  return [];
              }
              return seq.flat(Number.POSITIVE_INFINITY);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      mapcat: {
          evaluate: function (params, sourceCodeInfo, contextStack, helpers) {
              params.slice(1).forEach(function (arr) {
                  array.assert(arr, sourceCodeInfo);
              });
              var mapResult = evaluateMap(params, sourceCodeInfo, contextStack, helpers);
              array.assert(mapResult, sourceCodeInfo);
              return mapResult.flat(1);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
  };

  var mathNormalExpression = {
      inc: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              number.assert(first, sourceCodeInfo);
              return first + 1;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      dec: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              number.assert(first, sourceCodeInfo);
              return first - 1;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      '+': {
          evaluate: function (params, sourceCodeInfo) {
              return params.reduce(function (result, param) {
                  number.assert(param, sourceCodeInfo);
                  return result + param;
              }, 0);
          },
      },
      '*': {
          evaluate: function (params, sourceCodeInfo) {
              return params.reduce(function (result, param) {
                  number.assert(param, sourceCodeInfo);
                  return result * param;
              }, 1);
          },
      },
      '/': {
          evaluate: function (params, sourceCodeInfo) {
              if (params.length === 0) {
                  return 1;
              }
              var first = params[0], rest = params.slice(1);
              number.assert(first, sourceCodeInfo);
              if (rest.length === 0) {
                  number.assert(first, sourceCodeInfo);
                  return 1 / first;
              }
              return rest.reduce(function (result, param) {
                  number.assert(param, sourceCodeInfo);
                  return result / param;
              }, first);
          },
      },
      '-': {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0], rest = _a.slice(1);
              if (!first) {
                  return 0;
              }
              number.assert(first, sourceCodeInfo);
              if (rest.length === 0) {
                  return -first;
              }
              return rest.reduce(function (result, param) {
                  number.assert(param, sourceCodeInfo);
                  return result - param;
              }, first);
          },
      },
      quot: {
          evaluate: function (_a, sourceCodeInfo) {
              var dividend = _a[0], divisor = _a[1];
              number.assert(dividend, sourceCodeInfo);
              number.assert(divisor, sourceCodeInfo);
              var quotient = Math.trunc(dividend / divisor);
              return quotient;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      mod: {
          evaluate: function (_a, sourceCodeInfo) {
              var dividend = _a[0], divisor = _a[1];
              number.assert(dividend, sourceCodeInfo);
              number.assert(divisor, sourceCodeInfo);
              var quotient = Math.floor(dividend / divisor);
              return dividend - divisor * quotient;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      rem: {
          evaluate: function (_a, sourceCodeInfo) {
              var dividend = _a[0], divisor = _a[1];
              number.assert(dividend, sourceCodeInfo);
              number.assert(divisor, sourceCodeInfo);
              var quotient = Math.trunc(dividend / divisor);
              return dividend - divisor * quotient;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      sqrt: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              number.assert(first, sourceCodeInfo);
              return Math.sqrt(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      cbrt: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              number.assert(first, sourceCodeInfo);
              return Math.cbrt(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      pow: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0], second = _a[1];
              number.assert(first, sourceCodeInfo);
              number.assert(second, sourceCodeInfo);
              return Math.pow(first, second);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      round: {
          evaluate: function (params, sourceCodeInfo) {
              var value = params[0], decimals = params[1];
              number.assert(value, sourceCodeInfo);
              if (params.length === 1 || decimals === 0) {
                  return Math.round(value);
              }
              number.assert(decimals, sourceCodeInfo, { integer: true, nonNegative: true });
              var factor = Math.pow(10, decimals);
              return Math.round(value * factor) / factor;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
      trunc: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              number.assert(first, sourceCodeInfo);
              return Math.trunc(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      floor: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              number.assert(first, sourceCodeInfo);
              return Math.floor(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      ceil: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              number.assert(first, sourceCodeInfo);
              return Math.ceil(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'rand!': {
          evaluate: function (parameters, sourceCodeInfo) {
              var num = number.as(parameters.length === 1 ? parameters[0] : 1, sourceCodeInfo);
              return Math.random() * num;
          },
          validate: function (node) { return assertNumberOfParams({ min: 0, max: 1 }, node); },
      },
      'rand-int!': {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              number.assert(first, sourceCodeInfo);
              return Math.floor(Math.random() * Math.abs(first)) * Math.sign(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      min: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0], rest = _a.slice(1);
              number.assert(first, sourceCodeInfo);
              if (rest.length === 0) {
                  return first;
              }
              return rest.reduce(function (min, value) {
                  number.assert(value, sourceCodeInfo);
                  return Math.min(min, value);
              }, first);
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      max: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0], rest = _a.slice(1);
              number.assert(first, sourceCodeInfo);
              if (rest.length === 0) {
                  return first;
              }
              return rest.reduce(function (min, value) {
                  number.assert(value, sourceCodeInfo);
                  return Math.max(min, value);
              }, first);
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      abs: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.abs(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      sign: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.sign(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'max-safe-integer': {
          evaluate: function () {
              return Number.MAX_SAFE_INTEGER;
          },
          validate: function (node) { return assertNumberOfParams(0, node); },
      },
      'min-safe-integer': {
          evaluate: function () {
              return Number.MIN_SAFE_INTEGER;
          },
          validate: function (node) { return assertNumberOfParams(0, node); },
      },
      'max-value': {
          evaluate: function () {
              return Number.MAX_VALUE;
          },
          validate: function (node) { return assertNumberOfParams(0, node); },
      },
      'min-value': {
          evaluate: function () {
              return Number.MIN_VALUE;
          },
          validate: function (node) { return assertNumberOfParams(0, node); },
      },
      epsilon: {
          evaluate: function () {
              return Number.EPSILON;
          },
          validate: function (node) { return assertNumberOfParams(0, node); },
      },
      'positive-infinity': {
          evaluate: function () {
              return Number.POSITIVE_INFINITY;
          },
          validate: function (node) { return assertNumberOfParams(0, node); },
      },
      'negative-infinity': {
          evaluate: function () {
              return Number.NEGATIVE_INFINITY;
          },
          validate: function (node) { return assertNumberOfParams(0, node); },
      },
      nan: {
          evaluate: function () {
              return Number.NaN;
          },
          validate: function (node) { return assertNumberOfParams(0, node); },
      },
      e: {
          evaluate: function () {
              return Math.E;
          },
          validate: function (node) { return assertNumberOfParams(0, node); },
      },
      pi: {
          evaluate: function () {
              return Math.PI;
          },
          validate: function (node) { return assertNumberOfParams(0, node); },
      },
      exp: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.exp(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      log: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.log(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      log2: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.log2(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      log10: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.log10(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      sin: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.sin(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      asin: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.asin(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      sinh: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.sinh(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      asinh: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.asinh(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      cos: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.cos(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      acos: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.acos(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      cosh: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.cosh(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      acosh: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.acosh(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      tan: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.tan(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      atan: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.atan(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      tanh: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.tanh(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      atanh: {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Math.atanh(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
  };

  var version = "1.0.0-alpha.12";

  var miscNormalExpression = {
      'not=': {
          evaluate: function (params) {
              for (var i = 0; i < params.length - 1; i += 1) {
                  for (var j = i + 1; j < params.length; j += 1) {
                      if (params[i] === params[j]) {
                          return false;
                      }
                  }
              }
              return true;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      '=': {
          evaluate: function (_a) {
              var first = _a[0], rest = _a.slice(1);
              for (var _i = 0, rest_1 = rest; _i < rest_1.length; _i++) {
                  var param = rest_1[_i];
                  if (param !== first) {
                      return false;
                  }
              }
              return true;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      'equal?': {
          evaluate: function (_a, sourceCodeInfo) {
              var a = _a[0], b = _a[1];
              return deepEqual(any.as(a, sourceCodeInfo), any.as(b, sourceCodeInfo), sourceCodeInfo);
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      '>': {
          evaluate: function (_a) {
              var first = _a[0], rest = _a.slice(1);
              var currentValue = first;
              for (var _i = 0, rest_2 = rest; _i < rest_2.length; _i++) {
                  var param = rest_2[_i];
                  if (compare(currentValue, param) <= 0) {
                      return false;
                  }
                  currentValue = param;
              }
              return true;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      '<': {
          evaluate: function (_a) {
              var first = _a[0], rest = _a.slice(1);
              var currentValue = first;
              for (var _i = 0, rest_3 = rest; _i < rest_3.length; _i++) {
                  var param = rest_3[_i];
                  if (compare(currentValue, param) >= 0) {
                      return false;
                  }
                  currentValue = param;
              }
              return true;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      '>=': {
          evaluate: function (_a) {
              var first = _a[0], rest = _a.slice(1);
              var currentValue = first;
              for (var _i = 0, rest_4 = rest; _i < rest_4.length; _i++) {
                  var param = rest_4[_i];
                  if (compare(currentValue, param) < 0) {
                      return false;
                  }
                  currentValue = param;
              }
              return true;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      '<=': {
          evaluate: function (_a) {
              var first = _a[0], rest = _a.slice(1);
              var currentValue = first;
              for (var _i = 0, rest_5 = rest; _i < rest_5.length; _i++) {
                  var param = rest_5[_i];
                  if (compare(currentValue, param) > 0) {
                      return false;
                  }
                  currentValue = param;
              }
              return true;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      not: {
          evaluate: function (_a) {
              var first = _a[0];
              return !first;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'inst-ms': {
          evaluate: function () {
              return Date.now();
          },
          validate: function (node) { return assertNumberOfParams(0, node); },
      },
      'write!': {
          evaluate: function (params, sourceCodeInfo) {
              // eslint-disable-next-line no-console
              console.log.apply(console, params);
              if (params.length > 0) {
                  return any.as(params[params.length - 1], sourceCodeInfo);
              }
              return null;
          },
      },
      'debug!': {
          evaluate: function (params, sourceCodeInfo, contextStack) {
              if (params.length === 0) {
                  // eslint-disable-next-line no-console
                  console.warn("*** LITS DEBUG ***\n" + contextStackToString(contextStack) + "\n");
                  return null;
              }
              // eslint-disable-next-line no-console
              console.warn("*** LITS DEBUG ***\n" + JSON.stringify(params[0], null, 2) + "\n");
              return any.as(params[0], sourceCodeInfo);
          },
          validate: function (node) { return assertNumberOfParams({ max: 1 }, node); },
      },
      boolean: {
          evaluate: function (_a) {
              var value = _a[0];
              return !!value;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      compare: {
          evaluate: function (_a) {
              var a = _a[0], b = _a[1];
              return compare(a, b);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      assert: {
          evaluate: function (params, sourceCodeInfo) {
              var value = params[0];
              var message = params.length === 2 ? params[1] : "" + value;
              string.assert(message, sourceCodeInfo);
              if (!value) {
                  throw new AssertionError(message, sourceCodeInfo);
              }
              return any.as(value, sourceCodeInfo);
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
      'lits-version': {
          evaluate: function () {
              return version;
          },
          validate: function (node) { return assertNumberOfParams(0, node); },
      },
  };
  function contextStackToString(contextStack) {
      return contextStack.stack.reduce(function (result, context, index) {
          return result + "Context " + index + (context === contextStack.globalContext ? " - Global context" : "") + "\n" + contextToString(context) + "\n";
      }, "");
  }
  function contextToString(context) {
      if (Object.keys(context).length === 0) {
          return "  <empty>\n";
      }
      var maxKeyLength = Math.max.apply(Math, Object.keys(context).map(function (key) { return key.length; }));
      return Object.entries(context).reduce(function (result, entry) {
          var key = ("" + entry[0]).padEnd(maxKeyLength + 2, " ");
          return result + "  " + key + valueToString(entry[1]) + "\n";
      }, "");
  }
  function valueToString(contextEntry) {
      var value = contextEntry.value;
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      var name = value.name;
      if (litsFunction.is(value)) {
          if (name) {
              return "<" + value.type + " function " + name + ">";
          }
          else {
              return "<" + value.type + " function \u03BB>";
          }
      }
      return JSON.stringify(contextEntry.value);
  }

  var objectNormalExpression = {
      object: {
          evaluate: function (params, sourceCodeInfo) {
              var result = {};
              for (var i = 0; i < params.length; i += 2) {
                  var key = params[i];
                  var value = params[i + 1];
                  string.assert(key, sourceCodeInfo);
                  result[key] = value;
              }
              return result;
          },
          validate: function (node) { return assertEventNumberOfParams(node); },
      },
      keys: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              object.assert(first, sourceCodeInfo);
              return Object.keys(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      vals: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              object.assert(first, sourceCodeInfo);
              return Object.values(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      entries: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              object.assert(first, sourceCodeInfo);
              return Object.entries(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      find: {
          evaluate: function (_a, sourceCodeInfo) {
              var obj = _a[0], key = _a[1];
              object.assert(obj, sourceCodeInfo);
              string.assert(key, sourceCodeInfo);
              if (collHasKey(obj, key)) {
                  return [key, obj[key]];
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      dissoc: {
          evaluate: function (_a, sourceCodeInfo) {
              var obj = _a[0], key = _a[1];
              object.assert(obj, sourceCodeInfo);
              string.assert(key, sourceCodeInfo);
              var result = toAny(obj[key]);
              delete obj[key];
              return result;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      merge: {
          evaluate: function (params, sourceCodeInfo) {
              if (params.length === 0) {
                  return null;
              }
              var first = params[0], rest = params.slice(1);
              object.assert(first, sourceCodeInfo);
              return rest.reduce(function (result, obj) {
                  object.assert(obj, sourceCodeInfo);
                  return __assign(__assign({}, result), obj);
              }, __assign({}, first));
          },
          validate: function (node) { return assertNumberOfParams({ min: 0 }, node); },
      },
      'merge-with': {
          evaluate: function (params, sourceCodeInfo, contextStack, _a) {
              var executeFunction = _a.executeFunction;
              var fn = params[0], first = params[1], rest = params.slice(2);
              litsFunction.assert(fn, sourceCodeInfo);
              if (params.length === 1) {
                  return null;
              }
              object.assert(first, sourceCodeInfo);
              return rest.reduce(function (result, obj) {
                  object.assert(obj, sourceCodeInfo);
                  Object.entries(obj).forEach(function (entry) {
                      var key = string.as(entry[0], sourceCodeInfo);
                      var val = toAny(entry[1]);
                      if (collHasKey(result, key)) {
                          result[key] = executeFunction(fn, [result[key], val], sourceCodeInfo, contextStack);
                      }
                      else {
                          result[key] = val;
                      }
                  });
                  return result;
              }, __assign({}, first));
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      zipmap: {
          evaluate: function (_a, sourceCodeInfo) {
              var keys = _a[0], values = _a[1];
              stringArray.assert(keys, sourceCodeInfo);
              array.assert(values, sourceCodeInfo);
              var length = Math.min(keys.length, values.length);
              var result = {};
              for (var i = 0; i < length; i += 1) {
                  var key = string.as(keys[i], sourceCodeInfo);
                  result[key] = toAny(values[i]);
              }
              return result;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'select-keys': {
          evaluate: function (_a, sourceCodeInfo) {
              var obj = _a[0], keys = _a[1];
              stringArray.assert(keys, sourceCodeInfo);
              object.assert(obj, sourceCodeInfo);
              return keys.reduce(function (result, key) {
                  if (collHasKey(obj, key)) {
                      result[key] = toAny(obj[key]);
                  }
                  return result;
              }, {});
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
  };

  var predicatesNormalExpression = {
      'function?': {
          evaluate: function (_a) {
              var first = _a[0];
              return litsFunction.is(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'string?': {
          evaluate: function (_a) {
              var first = _a[0];
              return typeof first === "string";
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'number?': {
          evaluate: function (_a) {
              var first = _a[0];
              return typeof first === "number";
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'integer?': {
          evaluate: function (_a) {
              var first = _a[0];
              return typeof first === "number" && number.is(first, { integer: true });
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'boolean?': {
          evaluate: function (_a) {
              var first = _a[0];
              return typeof first === "boolean";
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'nil?': {
          evaluate: function (_a) {
              var first = _a[0];
              return first === null || first === undefined;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'zero?': {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              number.assert(first, sourceCodeInfo, { finite: true });
              return first === 0;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'pos?': {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              number.assert(first, sourceCodeInfo, { finite: true });
              return first > 0;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'neg?': {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              number.assert(first, sourceCodeInfo, { finite: true });
              return first < 0;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'even?': {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              number.assert(first, sourceCodeInfo, { finite: true });
              return first % 2 === 0;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'odd?': {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0];
              number.assert(first, sourceCodeInfo, { finite: true });
              return number.is(first, { integer: true }) && first % 2 !== 0;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'array?': {
          evaluate: function (_a) {
              var first = _a[0];
              return array.is(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'coll?': {
          evaluate: function (_a) {
              var first = _a[0];
              return collection.is(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'seq?': {
          evaluate: function (_a) {
              var first = _a[0];
              return sequence.is(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'object?': {
          evaluate: function (_a) {
              var first = _a[0];
              return object.is(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'regexp?': {
          evaluate: function (_a) {
              var first = _a[0];
              return first !== null && !Array.isArray(first) && typeof first === "object" && first instanceof RegExp;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'finite?': {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Number.isFinite(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'nan?': {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return Number.isNaN(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'positive-infinity?': {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return value === Number.POSITIVE_INFINITY;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'negative-infinity?': {
          evaluate: function (_a, sourceCodeInfo) {
              var value = _a[0];
              number.assert(value, sourceCodeInfo);
              return value === Number.NEGATIVE_INFINITY;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'true?': {
          evaluate: function (_a) {
              var value = _a[0];
              return value === true;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'false?': {
          evaluate: function (_a) {
              var value = _a[0];
              return value === false;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
  };

  var regexpNormalExpression = {
      regexp: {
          evaluate: function (params, sourceCodeInfo) {
              var first = params[0], second = params[1];
              string.assert(first, sourceCodeInfo);
              if (params.length === 1) {
                  return new RegExp(first);
              }
              string.assert(second, sourceCodeInfo);
              return new RegExp(first, second);
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
      match: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0], second = _a[1];
              regExp.assert(first, sourceCodeInfo);
              string.assert(second, sourceCodeInfo);
              var match = first.exec(second);
              if (match) {
                  return __spreadArray([], match);
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      replace: {
          evaluate: function (_a, sourceCodeInfo) {
              var str = _a[0], regexp = _a[1], value = _a[2];
              string.assert(str, sourceCodeInfo);
              regExp.assert(regexp, sourceCodeInfo);
              string.assert(value, sourceCodeInfo);
              return str.replace(regexp, value);
          },
          validate: function (node) { return assertNumberOfParams(3, node); },
      },
  };

  var stringNormalExpression = {
      subs: {
          evaluate: function (_a, sourceCodeInfo) {
              var first = _a[0], second = _a[1], third = _a[2];
              string.assert(first, sourceCodeInfo);
              number.assert(second, sourceCodeInfo, { integer: true, nonNegative: true });
              if (third === undefined) {
                  return first.substring(second);
              }
              number.assert(third, sourceCodeInfo, { gte: second });
              return first.substring(second, third);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'string-repeat': {
          evaluate: function (_a, sourceCodeInfo) {
              var str = _a[0], count = _a[1];
              string.assert(str, sourceCodeInfo);
              number.assert(count, sourceCodeInfo, { integer: true, nonNegative: true });
              return str.repeat(count);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      str: {
          evaluate: function (params) {
              return params.reduce(function (result, param) {
                  var paramStr = param === undefined || param === null
                      ? ""
                      : object.is(param)
                          ? JSON.stringify(param)
                          : Array.isArray(param)
                              ? JSON.stringify(param)
                              : "" + param;
                  return result + paramStr;
              }, "");
          },
      },
      number: {
          evaluate: function (_a, sourceCodeInfo) {
              var str = _a[0];
              string.assert(str, sourceCodeInfo);
              var number = Number(str);
              if (Number.isNaN(number)) {
                  throw new LitsError("Could not convert '" + str + "' to a number.", sourceCodeInfo);
              }
              return number;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'number-to-string': {
          evaluate: function (params, sourceCodeInfo) {
              var num = params[0], base = params[1];
              number.assert(num, sourceCodeInfo, { finite: true });
              if (params.length === 1) {
                  return "" + num;
              }
              else {
                  number.assert(base, sourceCodeInfo, { finite: true });
                  if (base !== 2 && base !== 8 && base !== 10 && base !== 16) {
                      throw new LitsError("Expected \"number-to-string\" base argument to be 2, 8, 10 or 16, got: " + base, sourceCodeInfo);
                  }
                  if (base === 10) {
                      return "" + num;
                  }
                  number.assert(num, sourceCodeInfo, { integer: true, nonNegative: true });
                  return Number(num).toString(base);
              }
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
      'from-char-code': {
          evaluate: function (_a, sourceCodeInfo) {
              var num = _a[0];
              number.assert(num, sourceCodeInfo, { finite: true });
              var int = toNonNegativeInteger(num);
              return String.fromCodePoint(int);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'to-char-code': {
          evaluate: function (_a, sourceCodeInfo) {
              var str = _a[0];
              string.assert(str, sourceCodeInfo, { nonEmpty: true });
              return asValue(str.codePointAt(0), sourceCodeInfo);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'lower-case': {
          evaluate: function (_a, sourceCodeInfo) {
              var str = _a[0];
              string.assert(str, sourceCodeInfo);
              return str.toLowerCase();
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'upper-case': {
          evaluate: function (_a, sourceCodeInfo) {
              var str = _a[0];
              string.assert(str, sourceCodeInfo);
              return str.toUpperCase();
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      trim: {
          evaluate: function (_a, sourceCodeInfo) {
              var str = _a[0];
              string.assert(str, sourceCodeInfo);
              return str.trim();
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'trim-left': {
          evaluate: function (_a, sourceCodeInfo) {
              var str = _a[0];
              string.assert(str, sourceCodeInfo);
              return str.replace(/^\s+/, "");
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'trim-right': {
          evaluate: function (_a, sourceCodeInfo) {
              var str = _a[0];
              string.assert(str, sourceCodeInfo);
              return str.replace(/\s+$/, "");
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      join: {
          evaluate: function (_a, sourceCodeInfo) {
              var stringList = _a[0], delimiter = _a[1];
              array.assert(stringList, sourceCodeInfo);
              stringList.forEach(function (str) { return string.assert(str, sourceCodeInfo); });
              string.assert(delimiter, sourceCodeInfo);
              return stringList.join(delimiter);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      split: {
          evaluate: function (_a, sourceCodeInfo) {
              var str = _a[0], delimiter = _a[1], limit = _a[2];
              string.assert(str, sourceCodeInfo);
              stringOrRegExp.assert(delimiter, sourceCodeInfo);
              if (limit !== undefined) {
                  number.assert(limit, sourceCodeInfo, { integer: true, nonNegative: true });
              }
              return str.split(delimiter, limit);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'pad-left': {
          evaluate: function (_a, sourceCodeInfo) {
              var str = _a[0], length = _a[1], padString = _a[2];
              string.assert(str, sourceCodeInfo);
              number.assert(length, sourceCodeInfo, { integer: true });
              if (padString !== undefined) {
                  string.assert(padString, sourceCodeInfo);
              }
              return str.padStart(length, padString);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'pad-right': {
          evaluate: function (_a, sourceCodeInfo) {
              var str = _a[0], length = _a[1], padString = _a[2];
              string.assert(str, sourceCodeInfo);
              number.assert(length, sourceCodeInfo, { integer: true });
              if (padString !== undefined) {
                  string.assert(padString, sourceCodeInfo);
              }
              return str.padEnd(length, padString);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      template: {
          evaluate: function (_a, sourceCodeInfo) {
              var templateString = _a[0], placeholders = _a.slice(1);
              string.assert(templateString, sourceCodeInfo);
              var templateStrings = templateString.split("||||");
              if (templateStrings.length === 1) {
                  stringArray.assert(placeholders, sourceCodeInfo);
                  return applyPlaceholders(templateStrings[0], placeholders, sourceCodeInfo);
              }
              else if (templateStrings.length === 2) {
                  var firstPlaceholder = placeholders[0];
                  number.assert(firstPlaceholder, sourceCodeInfo, { integer: true, nonNegative: true });
                  var stringPlaceholders = __spreadArray(["" + firstPlaceholder], placeholders.slice(1));
                  if (firstPlaceholder === 1) {
                      return applyPlaceholders(templateStrings[0], stringPlaceholders, sourceCodeInfo);
                  }
                  else {
                      return applyPlaceholders(templateStrings[1], stringPlaceholders, sourceCodeInfo);
                  }
              }
              else {
                  throw new LitsError("Invalid template string, only one \"||||\" separator allowed.", sourceCodeInfo);
              }
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 10 }, node); },
      },
  };
  var doubleDollarRegexp = /\$\$/g;
  function applyPlaceholders(templateString, placeholders, sourceCodeInfo) {
      for (var i = 0; i < 9; i += 1) {
          var re = new RegExp("(?<=^|[^$]|\\$\\$)\\$" + (i + 1), "g");
          if (re.test(templateString)) {
              var placeholder = placeholders[i];
              string.assert(placeholder, sourceCodeInfo);
              templateString = templateString.replace(re, placeholder);
          }
      }
      return templateString.replace(doubleDollarRegexp, "$");
  }

  var functionalNormalExpression = {
      apply: {
          evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
              var func = _a[0], params = _a.slice(1);
              var executeFunction = _b.executeFunction;
              litsFunction.assert(func, sourceCodeInfo);
              var paramsLength = params.length;
              var last = params[paramsLength - 1];
              array.assert(last, sourceCodeInfo);
              var applyArray = __spreadArray(__spreadArray([], params.slice(0, -1), true), last);
              return executeFunction(func, applyArray, sourceCodeInfo, contextStack);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
      identity: {
          evaluate: function (_a) {
              var value = _a[0];
              return toAny(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      partial: {
          evaluate: function (_a, sourceCodeInfo) {
              var _b;
              var fn = _a[0], params = _a.slice(1);
              return _b = {},
                  _b[FUNCTION_SYMBOL] = true,
                  _b.sourceCodeInfo = sourceCodeInfo,
                  _b.type = "partial",
                  _b.fn = toAny(fn),
                  _b.params = params,
                  _b;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      comp: {
          evaluate: function (fns, sourceCodeInfo) {
              var _a;
              if (fns.length > 1) {
                  var last = fns[fns.length - 1];
                  if (array.is(last)) {
                      fns = __spreadArray(__spreadArray([], fns.slice(0, -1), true), last);
                  }
              }
              return _a = {},
                  _a[FUNCTION_SYMBOL] = true,
                  _a.sourceCodeInfo = sourceCodeInfo,
                  _a.type = "comp",
                  _a.fns = fns,
                  _a;
          },
      },
      constantly: {
          evaluate: function (_a, sourceCodeInfo) {
              var _b;
              var value = _a[0];
              return _b = {},
                  _b[FUNCTION_SYMBOL] = true,
                  _b.sourceCodeInfo = sourceCodeInfo,
                  _b.type = "constantly",
                  _b.value = toAny(value),
                  _b;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      juxt: {
          evaluate: function (fns, sourceCodeInfo) {
              var _a;
              return _a = {},
                  _a[FUNCTION_SYMBOL] = true,
                  _a.sourceCodeInfo = sourceCodeInfo,
                  _a.type = "juxt",
                  _a.fns = fns,
                  _a;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      complement: {
          evaluate: function (_a, sourceCodeInfo) {
              var _b;
              var fn = _a[0];
              return _b = {},
                  _b[FUNCTION_SYMBOL] = true,
                  _b.sourceCodeInfo = sourceCodeInfo,
                  _b.type = "complement",
                  _b.fn = toAny(fn),
                  _b;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'every-pred': {
          evaluate: function (fns, sourceCodeInfo) {
              var _a;
              return _a = {},
                  _a[FUNCTION_SYMBOL] = true,
                  _a.sourceCodeInfo = sourceCodeInfo,
                  _a.type = "every-pred",
                  _a.fns = fns,
                  _a;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      'some-pred': {
          evaluate: function (fns, sourceCodeInfo) {
              var _a;
              return _a = {},
                  _a[FUNCTION_SYMBOL] = true,
                  _a.sourceCodeInfo = sourceCodeInfo,
                  _a.type = "some-pred",
                  _a.fns = fns,
                  _a;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      fnil: {
          evaluate: function (_a, sourceCodeInfo) {
              var _b;
              var fn = _a[0], params = _a.slice(1);
              return _b = {},
                  _b[FUNCTION_SYMBOL] = true,
                  _b.sourceCodeInfo = sourceCodeInfo,
                  _b.type = "fnil",
                  _b.fn = toAny(fn),
                  _b.params = params,
                  _b;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
  };

  var normalExpressions = __assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign({}, bitwiseNormalExpression), collectionNormalExpression), arrayNormalExpression), sequenceNormalExpression), mathNormalExpression), miscNormalExpression), objectNormalExpression), predicatesNormalExpression), regexpNormalExpression), stringNormalExpression), functionalNormalExpression);

  var specialExpressions = {
      and: andSpecialExpression,
      cond: condSpecialExpression,
      def: defSpecialExpression,
      defn: defnSpecialExpression,
      defns: defnsSpecialExpression,
      defs: defsSpecialExpression,
      do: doSpecialExpression,
      for: forSpecialExpression,
      fn: fnSpecialExpression,
      if: ifSpecialExpression,
      'if-let': ifLetSpecialExpression,
      'if-not': ifNotSpecialExpression,
      let: letSpecialExpression,
      loop: loopSpecialExpression,
      or: orSpecialExpression,
      recur: recurSpecialExpression,
      throw: throwSpecialExpression,
      'time!': timeSpecialExpression,
      try: trySpecialExpression,
      when: whenSpecialExpression,
      'when-first': whenFirstSpecialExpression,
      'when-let': whenLetSpecialExpression,
      'when-not': whenNotSpecialExpression,
  };
  Object.keys(specialExpressions).forEach(function (key) {
      /* istanbul ignore next */
      if (normalExpressions[key]) {
          throw Error("Expression " + key + " is defined as both a normal expression and a special expression");
      }
  });
  var builtin = {
      normalExpressions: normalExpressions,
      specialExpressions: specialExpressions,
  };
  var normalExpressionKeys = Object.keys(normalExpressions);
  var specialExpressionKeys = Object.keys(specialExpressions);

  function findOverloadFunction(overloads, nbrOfParams, sourceCodeInfo) {
      var overloadFunction = overloads.find(function (overload) {
          var arity = overload.arity;
          if (typeof arity === "number") {
              return arity === nbrOfParams;
          }
          else {
              return arity.min <= nbrOfParams;
          }
      });
      if (!overloadFunction) {
          throw new LitsError("Unexpected number of arguments, got " + valueToString$1(nbrOfParams) + ".", sourceCodeInfo);
      }
      return overloadFunction;
  }
  var functionExecutors = {
      'user-defined': function (fn, params, sourceCodeInfo, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          for (;;) {
              var overloadFunction = findOverloadFunction(fn.overloads, params.length, sourceCodeInfo);
              var args = overloadFunction.arguments;
              var nbrOfMandatoryArgs = args.mandatoryArguments.length;
              var newContext = __assign({}, overloadFunction.functionContext);
              var length_1 = Math.max(params.length, args.mandatoryArguments.length);
              var rest = [];
              for (var i = 0; i < length_1; i += 1) {
                  if (i < nbrOfMandatoryArgs) {
                      var param = toAny(params[i]);
                      var key = string.as(args.mandatoryArguments[i], sourceCodeInfo);
                      newContext[key] = { value: param };
                  }
                  else {
                      rest.push(toAny(params[i]));
                  }
              }
              if (args.restArgument) {
                  newContext[args.restArgument] = { value: rest };
              }
              try {
                  var result = null;
                  for (var _i = 0, _b = overloadFunction.body; _i < _b.length; _i++) {
                      var node = _b[_i];
                      result = evaluateAstNode(node, contextStack.withContext(newContext));
                  }
                  return result;
              }
              catch (error) {
                  if (error instanceof RecurSignal) {
                      params = error.params;
                      continue;
                  }
                  throw error;
              }
          }
      },
      partial: function (fn, params, sourceCodeInfo, contextStack, _a) {
          var executeFunction = _a.executeFunction;
          return executeFunction(fn.fn, __spreadArray(__spreadArray([], fn.params, true), params), sourceCodeInfo, contextStack);
      },
      comp: function (fn, params, sourceCodeInfo, contextStack, _a) {
          var executeFunction = _a.executeFunction;
          var fns = fn.fns;
          if (fns.length === 0) {
              if (params.length !== 1) {
                  throw new LitsError("(comp) expects one argument, got " + valueToString$1(params.length) + ".", sourceCodeInfo);
              }
              return any.as(params[0], sourceCodeInfo);
          }
          return any.as(fns.reduceRight(function (result, fn) {
              return [executeFunction(toAny(fn), result, sourceCodeInfo, contextStack)];
          }, params)[0], sourceCodeInfo);
      },
      constantly: function (fn) {
          return fn.value;
      },
      juxt: function (fn, params, sourceCodeInfo, contextStack, _a) {
          var executeFunction = _a.executeFunction;
          return fn.fns.map(function (fn) { return executeFunction(toAny(fn), params, sourceCodeInfo, contextStack); });
      },
      complement: function (fn, params, sourceCodeInfo, contextStack, _a) {
          var executeFunction = _a.executeFunction;
          return !executeFunction(fn.fn, params, sourceCodeInfo, contextStack);
      },
      'every-pred': function (fn, params, sourceCodeInfo, contextStack, _a) {
          var executeFunction = _a.executeFunction;
          for (var _i = 0, _b = fn.fns; _i < _b.length; _i++) {
              var f = _b[_i];
              for (var _c = 0, params_1 = params; _c < params_1.length; _c++) {
                  var param = params_1[_c];
                  var result = executeFunction(toAny(f), [param], sourceCodeInfo, contextStack);
                  if (!result) {
                      return false;
                  }
              }
          }
          return true;
      },
      'some-pred': function (fn, params, sourceCodeInfo, contextStack, _a) {
          var executeFunction = _a.executeFunction;
          for (var _i = 0, _b = fn.fns; _i < _b.length; _i++) {
              var f = _b[_i];
              for (var _c = 0, params_2 = params; _c < params_2.length; _c++) {
                  var param = params_2[_c];
                  var result = executeFunction(toAny(f), [param], sourceCodeInfo, contextStack);
                  if (result) {
                      return true;
                  }
              }
          }
          return false;
      },
      fnil: function (fn, params, sourceCodeInfo, contextStack, _a) {
          var executeFunction = _a.executeFunction;
          var fniledParams = params.map(function (param, index) { return (param === null ? toAny(fn.params[index]) : param); });
          return executeFunction(toAny(fn.fn), fniledParams, sourceCodeInfo, contextStack);
      },
      builtin: function (fn, params, sourceCodeInfo, contextStack, _a) {
          var executeFunction = _a.executeFunction;
          var normalExpression = asValue(normalExpressions[fn.name], sourceCodeInfo);
          return normalExpression.evaluate(params, sourceCodeInfo, contextStack, { executeFunction: executeFunction });
      },
  };

  function createContextStack(contexts) {
      if (contexts === void 0) { contexts = []; }
      if (contexts.length === 0) {
          contexts.push({});
      }
      return new ContextStackImpl(contexts, 0);
  }
  var ContextStackImpl = /** @class */ (function () {
      function ContextStackImpl(contexts, globalContextIndex) {
          this.stack = contexts;
          this.numberOfImportedContexts = contexts.length - (globalContextIndex + 1);
          this.globalContext = contexts[globalContextIndex];
      }
      ContextStackImpl.prototype.withContext = function (context) {
          return new ContextStackImpl(__spreadArray([context], this.stack), this.stack.length - this.numberOfImportedContexts);
      };
      return ContextStackImpl;
  }());
  function evaluate(ast, contextStack) {
      var result = null;
      for (var _i = 0, _a = ast.body; _i < _a.length; _i++) {
          var node = _a[_i];
          result = evaluateAstNode(node, contextStack);
      }
      return result;
  }
  var evaluateAstNode = function (node, contextStack) {
      switch (node.type) {
          case "Number":
              return evaluateNumber(node);
          case "String":
              return evaluateString(node);
          case "Name":
              return evaluateName(node, contextStack);
          case "ReservedName":
              return evaluateReservedName(node);
          case "NormalExpression":
              return evaluateNormalExpression(node, contextStack);
          case "SpecialExpression":
              return evaluateSpecialExpression(node, contextStack);
          default:
              throw new LitsError(node.type + "-node cannot be evaluated", node.token.sourceCodeInfo);
      }
  };
  function evaluateNumber(node) {
      return node.value;
  }
  function evaluateString(node) {
      return node.value;
  }
  function evaluateReservedName(node) {
      return asValue(reservedNamesRecord[node.value], node.token.sourceCodeInfo).value;
  }
  function evaluateName(node, contextStack) {
      var _a;
      var value = node.value, sourceCodeInfo = node.token.sourceCodeInfo;
      for (var _i = 0, _b = contextStack.stack; _i < _b.length; _i++) {
          var context = _b[_i];
          var variable = context[value];
          if (variable) {
              return variable.value;
          }
      }
      if (builtin.normalExpressions[value]) {
          var builtinFunction = (_a = {},
              _a[FUNCTION_SYMBOL] = true,
              _a.sourceCodeInfo = node.token.sourceCodeInfo,
              _a.type = "builtin",
              _a.name = value,
              _a);
          return builtinFunction;
      }
      throw new UndefinedSymbolError(value, sourceCodeInfo);
  }
  function evaluateNormalExpression(node, contextStack) {
      var _a;
      var params = node.params.map(function (paramNode) { return evaluateAstNode(paramNode, contextStack); });
      var sourceCodeInfo = node.token.sourceCodeInfo;
      if (normalExpressionNodeWithName.is(node)) {
          for (var _i = 0, _b = contextStack.stack; _i < _b.length; _i++) {
              var context = _b[_i];
              var fn = (_a = context[node.name]) === null || _a === void 0 ? void 0 : _a.value;
              if (fn === undefined) {
                  continue;
              }
              return executeFunction(fn, params, sourceCodeInfo, contextStack);
          }
          return evaluateBuiltinNormalExpression(node, params, contextStack);
      }
      else {
          var fn = evaluateAstNode(node.expression, contextStack);
          return executeFunction(fn, params, sourceCodeInfo, contextStack);
      }
  }
  var executeFunction = function (fn, params, sourceCodeInfo, contextStack) {
      if (litsFunction.is(fn)) {
          return functionExecutors[fn.type](fn, params, sourceCodeInfo, contextStack, { evaluateAstNode: evaluateAstNode, executeFunction: executeFunction });
      }
      if (Array.isArray(fn)) {
          return evaluateArrayAsFunction(fn, params, sourceCodeInfo);
      }
      if (object.is(fn)) {
          return evalueateObjectAsFunction(fn, params, sourceCodeInfo);
      }
      if (string.is(fn)) {
          return evaluateStringAsFunction(fn, params, sourceCodeInfo);
      }
      if (number.is(fn)) {
          return evaluateNumberAsFunction(fn, params, sourceCodeInfo);
      }
      throw new NotAFunctionError(fn, sourceCodeInfo);
  };
  function evaluateBuiltinNormalExpression(node, params, contextStack) {
      var normalExpression = builtin.normalExpressions[node.name];
      if (!normalExpression) {
          throw new UndefinedSymbolError(node.name, node.token.sourceCodeInfo);
      }
      return normalExpression.evaluate(params, node.token.sourceCodeInfo, contextStack, { executeFunction: executeFunction });
  }
  function evaluateSpecialExpression(node, contextStack) {
      var specialExpression = asValue(builtin.specialExpressions[node.name], node.token.sourceCodeInfo);
      return specialExpression.evaluate(node, contextStack, { evaluateAstNode: evaluateAstNode, builtin: builtin });
  }
  function evalueateObjectAsFunction(fn, params, sourceCodeInfo) {
      if (params.length !== 1) {
          throw new LitsError("Object as function requires one string parameter.", sourceCodeInfo);
      }
      var key = params[0];
      string.assert(key, sourceCodeInfo);
      return toAny(fn[key]);
  }
  function evaluateArrayAsFunction(fn, params, sourceCodeInfo) {
      if (params.length !== 1) {
          throw new LitsError("Array as function requires one non negative integer parameter.", sourceCodeInfo);
      }
      var index = params[0];
      number.assert(index, sourceCodeInfo, { integer: true, nonNegative: true });
      return toAny(fn[index]);
  }
  function evaluateStringAsFunction(fn, params, sourceCodeInfo) {
      if (params.length !== 1) {
          throw new LitsError("String as function requires one Obj parameter.", sourceCodeInfo);
      }
      var param = toAny(params[0]);
      if (object.is(param)) {
          return toAny(param[fn]);
      }
      if (number.is(param, { integer: true })) {
          return toAny(fn[param]);
      }
      throw new LitsError("string as function expects Obj or integer parameter, got " + valueToString$1(param), sourceCodeInfo);
  }
  function evaluateNumberAsFunction(fn, params, sourceCodeInfo) {
      number.assert(fn, sourceCodeInfo, { integer: true });
      if (params.length !== 1) {
          throw new LitsError("String as function requires one Arr parameter.", sourceCodeInfo);
      }
      var param = params[0];
      sequence.assert(param, sourceCodeInfo);
      return toAny(param[fn]);
  }

  var parseNumber = function (tokens, position) {
      var tkn = token.as(tokens[position], "EOF");
      return [position + 1, { type: "Number", value: Number(tkn.value), token: tkn }];
  };
  var parseString = function (tokens, position) {
      var tkn = token.as(tokens[position], "EOF");
      return [position + 1, { type: "String", value: tkn.value, token: tkn }];
  };
  var parseName = function (tokens, position) {
      var tkn = token.as(tokens[position], "EOF");
      return [position + 1, { type: "Name", value: tkn.value, token: tkn }];
  };
  var parseReservedName = function (tokens, position) {
      var tkn = token.as(tokens[position], "EOF");
      return [position + 1, { type: "ReservedName", value: tkn.value, token: tkn }];
  };
  var parseTokens = function (tokens, position) {
      var _a;
      var tkn = token.as(tokens[position], "EOF");
      var astNodes = [];
      var astNode;
      while (!(tkn.value === ")" || tkn.value === "]")) {
          _a = parseToken(tokens, position), position = _a[0], astNode = _a[1];
          astNodes.push(astNode);
          tkn = token.as(tokens[position], "EOF");
      }
      return [position, astNodes];
  };
  var parseExpression = function (tokens, position) {
      position += 1; // Skip parenthesis
      var tkn = token.as(tokens[position], "EOF");
      if (tkn.type === "name" && builtin.specialExpressions[tkn.value]) {
          return parseSpecialExpression(tokens, position);
      }
      return parseNormalExpression(tokens, position);
  };
  var parseArrayLitteral = function (tokens, position) {
      var _a;
      var firstToken = token.as(tokens[position], "EOF");
      position = position + 1;
      var tkn = token.as(tokens[position], "EOF");
      var params = [];
      var param;
      while (!(tkn.type === "paren" && tkn.value === "]")) {
          _a = parseToken(tokens, position), position = _a[0], param = _a[1];
          params.push(param);
          tkn = token.as(tokens[position], "EOF");
      }
      position = position + 1;
      var node = {
          type: "NormalExpression",
          name: "array",
          params: params,
          token: firstToken,
      };
      return [position, node];
  };
  var parseObjectLitteral = function (tokens, position) {
      var _a;
      var firstToken = token.as(tokens[position], "EOF");
      position = position + 1;
      var tkn = token.as(tokens[position], "EOF");
      var params = [];
      var param;
      while (!(tkn.type === "paren" && tkn.value === "}")) {
          _a = parseToken(tokens, position), position = _a[0], param = _a[1];
          params.push(param);
          tkn = token.as(tokens[position], "EOF");
      }
      position = position + 1;
      var node = {
          type: "NormalExpression",
          name: "object",
          params: params,
          token: firstToken,
      };
      assertEventNumberOfParams(node);
      return [position, node];
  };
  var parseRegexpShorthand = function (tokens, position) {
      var tkn = token.as(tokens[position], "EOF");
      var stringNode = {
          type: "String",
          value: tkn.value,
          token: tkn,
      };
      assertValue(tkn.options, tkn.sourceCodeInfo);
      var optionsNode = {
          type: "String",
          value: "" + (tkn.options.g ? "g" : "") + (tkn.options.i ? "i" : ""),
          token: tkn,
      };
      var node = {
          type: "NormalExpression",
          name: "regexp",
          params: [stringNode, optionsNode],
          token: tkn,
      };
      return [position + 1, node];
  };
  var placeholderRegexp = /^%([1-9][0-9]?$)/;
  var parseFnShorthand = function (tokens, position) {
      var firstToken = token.as(tokens[position], "EOF");
      position += 2;
      var _a = parseNormalExpression(tokens, position), newPosition = _a[0], normalExpressionNode = _a[1];
      var arity = 0;
      for (var pos = position + 1; pos < newPosition - 1; pos += 1) {
          var tkn = token.as(tokens[pos], "EOF");
          if (tkn.type === "name") {
              var match = placeholderRegexp.exec(tkn.value);
              if (match) {
                  arity = Math.max(arity, Number(match[1]));
                  if (arity > 20) {
                      throw new LitsError("Can't specify more than 20 arguments", firstToken.sourceCodeInfo);
                  }
              }
          }
          if (tkn.type === "fnShorthand") {
              throw new LitsError("Nested shortcut functions are not allowed", firstToken.sourceCodeInfo);
          }
      }
      var mandatoryArguments = [];
      for (var i = 1; i <= arity; i += 1) {
          mandatoryArguments.push("%" + i);
      }
      var args = {
          bindings: [],
          mandatoryArguments: mandatoryArguments,
      };
      var node = {
          type: "SpecialExpression",
          name: "fn",
          params: [],
          overloads: [
              {
                  arguments: args,
                  body: [normalExpressionNode],
                  arity: args.mandatoryArguments.length,
              },
          ],
          token: firstToken,
      };
      return [newPosition, node];
  };
  var parseArgument = function (tokens, position) {
      var tkn = token.as(tokens[position], "EOF");
      if (tkn.type === "name") {
          return [position + 1, { type: "Argument", name: tkn.value, token: tkn }];
      }
      else if (tkn.type === "modifier") {
          var value = tkn.value;
          return [position + 1, { type: "Modifier", value: value, token: tkn }];
      }
      else {
          throw new LitsError("Expected name or modifier token, got " + valueToString$1(tkn) + ".", tkn.sourceCodeInfo);
      }
  };
  var parseBindings = function (tokens, position) {
      var _a;
      var tkn = token.as(tokens[position], "EOF", { type: "paren", value: "[" });
      position += 1;
      tkn = token.as(tokens[position], "EOF");
      var bindings = [];
      var binding;
      while (!(tkn.type === "paren" && tkn.value === "]")) {
          _a = parseBinding(tokens, position), position = _a[0], binding = _a[1];
          bindings.push(binding);
          tkn = token.as(tokens[position], "EOF");
      }
      position += 1;
      return [position, bindings];
  };
  var parseBinding = function (tokens, position) {
      var _a;
      var firstToken = token.as(tokens[position], "EOF", { type: "name" });
      var name = firstToken.value;
      position += 1;
      var value;
      _a = parseToken(tokens, position), position = _a[0], value = _a[1];
      var node = {
          type: "Binding",
          name: name,
          value: value,
          token: firstToken,
      };
      return [position, node];
  };
  var parseNormalExpression = function (tokens, position) {
      var _a;
      var _b;
      var _c = parseToken(tokens, position), newPosition = _c[0], fnNode = _c[1];
      var params;
      _a = parseTokens(tokens, newPosition), position = _a[0], params = _a[1];
      position += 1;
      if (expressionNode.is(fnNode)) {
          var node_1 = {
              type: "NormalExpression",
              expression: fnNode,
              params: params,
              token: fnNode.token,
          };
          return [position, node_1];
      }
      nameNode.assert(fnNode, fnNode.token.sourceCodeInfo);
      var node = {
          type: "NormalExpression",
          name: fnNode.value,
          params: params,
          token: fnNode.token,
      };
      var builtinExpression = builtin.normalExpressions[node.name];
      if (builtinExpression) {
          (_b = builtinExpression.validate) === null || _b === void 0 ? void 0 : _b.call(builtinExpression, node);
      }
      return [position, node];
  };
  var parseSpecialExpression = function (tokens, position) {
      var _a = token.as(tokens[position], "EOF"), expressionName = _a.value, sourceCodeInfo = _a.sourceCodeInfo;
      position += 1;
      var _b = asValue(builtin.specialExpressions[expressionName], sourceCodeInfo), parse = _b.parse, validate = _b.validate;
      var _c = parse(tokens, position, {
          parseExpression: parseExpression,
          parseTokens: parseTokens,
          parseToken: parseToken,
          parseBinding: parseBinding,
          parseBindings: parseBindings,
          parseArgument: parseArgument,
      }), positionAfterParse = _c[0], node = _c[1];
      validate === null || validate === void 0 ? void 0 : validate(node);
      return [positionAfterParse, node];
  };
  var parseToken = function (tokens, position) {
      var tkn = token.as(tokens[position], "EOF");
      var nodeDescriptor = undefined;
      switch (tkn.type) {
          case "number":
              nodeDescriptor = parseNumber(tokens, position);
              break;
          case "string":
              nodeDescriptor = parseString(tokens, position);
              break;
          case "name":
              nodeDescriptor = parseName(tokens, position);
              break;
          case "reservedName":
              nodeDescriptor = parseReservedName(tokens, position);
              break;
          case "paren":
              if (tkn.value === "(") {
                  nodeDescriptor = parseExpression(tokens, position);
              }
              else if (tkn.value === "[") {
                  nodeDescriptor = parseArrayLitteral(tokens, position);
              }
              else if (tkn.value === "{") {
                  nodeDescriptor = parseObjectLitteral(tokens, position);
              }
              break;
          case "regexpShorthand":
              nodeDescriptor = parseRegexpShorthand(tokens, position);
              break;
          case "fnShorthand":
              nodeDescriptor = parseFnShorthand(tokens, position);
              break;
      }
      if (!nodeDescriptor) {
          throw new LitsError("Unrecognized token: " + tkn.type + " value=" + tkn.value, tkn.sourceCodeInfo);
      }
      return nodeDescriptor;
  };

  function parse(tokens) {
      var _a;
      var ast = {
          type: "Program",
          body: [],
      };
      var position = 0;
      var node;
      while (position < tokens.length) {
          _a = parseToken(tokens, position), position = _a[0], node = _a[1];
          ast.body.push(node);
      }
      return ast;
  }

  var NO_MATCH = [0, undefined];
  // A name (function or variable) can contain a lot of different characters
  var nameRegExp = /[@%0-9a-zA-ZàáâãăäāåæćčçèéêĕëēìíîĭïðłñòóôõöőøšùúûüűýÿþÀÁÂÃĂÄĀÅÆĆČÇÈÉÊĔËĒÌÍÎĬÏÐŁÑÒÓÔÕÖŐØŠÙÚÛÜŰÝÞß_^?=!$%<>.+*/-]/;
  var whitespaceRegExp = /\s|,/;
  var skipWhiteSpace = function (input, current) {
      return whitespaceRegExp.test(input[current]) ? [1, undefined] : NO_MATCH;
  };
  var skipComment = function (input, current) {
      if (input[current] === ";") {
          var length_1 = 1;
          while (input[current + length_1] !== "\n" && current + length_1 < input.length) {
              length_1 += 1;
          }
          if (input[current + length_1] === "\n" && current + length_1 < input.length) {
              length_1 += 1;
          }
          return [length_1, undefined];
      }
      return NO_MATCH;
  };
  var tokenizeLeftParen = function (input, position, sourceCodeInfo) {
      return tokenizeCharacter("paren", "(", input, position, sourceCodeInfo);
  };
  var tokenizeRightParen = function (input, position, sourceCodeInfo) {
      return tokenizeCharacter("paren", ")", input, position, sourceCodeInfo);
  };
  var tokenizeLeftBracket = function (input, position, sourceCodeInfo) {
      return tokenizeCharacter("paren", "[", input, position, sourceCodeInfo);
  };
  var tokenizeRightBracket = function (input, position, sourceCodeInfo) {
      return tokenizeCharacter("paren", "]", input, position, sourceCodeInfo);
  };
  var tokenizeLeftCurly = function (input, position, sourceCodeInfo) {
      return tokenizeCharacter("paren", "{", input, position, sourceCodeInfo);
  };
  var tokenizeRightCurly = function (input, position, sourceCodeInfo) {
      return tokenizeCharacter("paren", "}", input, position, sourceCodeInfo);
  };
  var tokenizeString = function (input, position, sourceCodeInfo) {
      if (input[position] !== "'") {
          return NO_MATCH;
      }
      var value = "";
      var length = 1;
      var char = input[position + length];
      var escape = false;
      while (char !== "'" || escape) {
          if (char === undefined) {
              throw new LitsError("Unclosed string at position " + position + ".", sourceCodeInfo);
          }
          length += 1;
          if (escape) {
              escape = false;
              if (char === "'" || char === "\\") {
                  value += char;
              }
              else {
                  value += "\\";
                  value += char;
              }
          }
          else {
              if (char === "\\") {
                  escape = true;
              }
              else {
                  value += char;
              }
          }
          char = input[position + length];
      }
      return [length + 1, { type: "string", value: value, sourceCodeInfo: sourceCodeInfo }];
  };
  var tokenizeSymbolString = function (input, position, sourceCodeInfo) {
      if (input[position] !== ":") {
          return NO_MATCH;
      }
      var value = "";
      var length = 1;
      var char = input[position + length];
      while (char && nameRegExp.test(char)) {
          length += 1;
          value += char;
          char = input[position + length];
      }
      if (length === 1) {
          return NO_MATCH;
      }
      return [length, { type: "string", value: value, sourceCodeInfo: sourceCodeInfo }];
  };
  var tokenizeRegexpShorthand = function (input, position, sourceCodeInfo) {
      var _a;
      if (input[position] !== "#") {
          return NO_MATCH;
      }
      var _b = tokenizeString(input, position + 1, sourceCodeInfo), stringLength = _b[0], token = _b[1];
      if (!token) {
          return NO_MATCH;
      }
      position += stringLength + 1;
      var length = stringLength + 1;
      var options = {};
      while (input[position] === "g" || input[position] === "i") {
          if (input[position] === "g") {
              if (options.g) {
                  throw new LitsError("Duplicated regexp option \"" + input[position] + "\" at position " + position + ".", sourceCodeInfo);
              }
              length += 1;
              options.g = true;
          }
          else {
              if (options.i) {
                  throw new LitsError("Duplicated regexp option \"" + input[position] + "\" at position " + position + ".", sourceCodeInfo);
              }
              length += 1;
              options.i = true;
          }
          position += 1;
      }
      if (nameRegExp.test((_a = input[position]) !== null && _a !== void 0 ? _a : "")) {
          throw new LitsError("Unexpected regexp option \"" + input[position] + "\" at position " + position + ".", sourceCodeInfo);
      }
      return [
          length,
          {
              type: "regexpShorthand",
              value: token.value,
              options: options,
              sourceCodeInfo: sourceCodeInfo,
          },
      ];
  };
  var tokenizeFnShorthand = function (input, position, sourceCodeInfo) {
      if (input.slice(position, position + 2) !== "#(") {
          return NO_MATCH;
      }
      return [
          1,
          {
              type: "fnShorthand",
              value: "#",
              sourceCodeInfo: sourceCodeInfo,
          },
      ];
  };
  var endOfNumberRegExp = /\s|[)\]},]/;
  var decimalNumberRegExp = /[0-9]/;
  var octalNumberRegExp = /[0-7]/;
  var hexNumberRegExp = /[0-9a-fA-F]/;
  var binaryNumberRegExp = /[0-1]/;
  var firstCharRegExp = /[0-9.-]/;
  var tokenizeNumber = function (input, position, sourceCodeInfo) {
      var type = "decimal";
      var firstChar = input[position];
      if (!firstCharRegExp.test(firstChar)) {
          return NO_MATCH;
      }
      var hasDecimals = firstChar === ".";
      var i;
      for (i = position + 1; i < input.length; i += 1) {
          var char = string.as(input[i], sourceCodeInfo, { char: true });
          if (endOfNumberRegExp.test(char)) {
              break;
          }
          if (i === position + 1 && firstChar === "0") {
              if (char === "b" || char === "B") {
                  type = "binary";
                  continue;
              }
              if (char === "o" || char === "O") {
                  type = "octal";
                  continue;
              }
              if (char === "x" || char === "X") {
                  type = "hex";
                  continue;
              }
          }
          if (type === "decimal" && hasDecimals) {
              if (!decimalNumberRegExp.test(char)) {
                  return NO_MATCH;
              }
          }
          else if (type === "binary") {
              if (!binaryNumberRegExp.test(char)) {
                  return NO_MATCH;
              }
          }
          else if (type === "octal") {
              if (!octalNumberRegExp.test(char)) {
                  return NO_MATCH;
              }
          }
          else if (type === "hex") {
              if (!hexNumberRegExp.test(char)) {
                  return NO_MATCH;
              }
          }
          else {
              if (char === ".") {
                  hasDecimals = true;
                  continue;
              }
              if (!decimalNumberRegExp.test(char)) {
                  return NO_MATCH;
              }
          }
      }
      var length = i - position;
      var value = input.substring(position, i);
      if ((type !== "decimal" && length <= 2) || value === "." || value === "-") {
          return NO_MATCH;
      }
      return [length, { type: "number", value: value, sourceCodeInfo: sourceCodeInfo }];
  };
  var tokenizeReservedName = function (input, position, sourceCodeInfo) {
      for (var _i = 0, _a = Object.keys(reservedNamesRecord); _i < _a.length; _i++) {
          var reservedName = _a[_i];
          var length_2 = reservedName.length;
          var nextChar = input[position + length_2];
          if (nextChar && nameRegExp.test(nextChar)) {
              continue;
          }
          if (input.substr(position, length_2) === reservedName) {
              return [length_2, { type: "reservedName", value: reservedName, sourceCodeInfo: sourceCodeInfo }];
          }
      }
      return NO_MATCH;
  };
  var tokenizeName = function (input, position, sourceCodeInfo) {
      return tokenizePattern("name", nameRegExp, input, position, sourceCodeInfo);
  };
  var tokenizeModifier = function (input, position, sourceCodeInfo) {
      var modifiers = ["&", "&let", "&when", "&while"];
      for (var _i = 0, modifiers_1 = modifiers; _i < modifiers_1.length; _i++) {
          var modifier = modifiers_1[_i];
          var length_3 = modifier.length;
          var charAfterModifier = input[position + length_3];
          if (input.substr(position, length_3) === modifier && (!charAfterModifier || !nameRegExp.test(charAfterModifier))) {
              var value = modifier;
              return [length_3, { type: "modifier", value: value, sourceCodeInfo: sourceCodeInfo }];
          }
      }
      return NO_MATCH;
  };
  function tokenizeCharacter(type, value, input, position, sourceCodeInfo) {
      if (value === input[position]) {
          return [1, { type: type, value: value, sourceCodeInfo: sourceCodeInfo }];
      }
      else {
          return NO_MATCH;
      }
  }
  function tokenizePattern(type, pattern, input, position, sourceCodeInfo) {
      var char = input[position];
      var length = 0;
      var value = "";
      if (!char || !pattern.test(char)) {
          return NO_MATCH;
      }
      while (char && pattern.test(char)) {
          value += char;
          length += 1;
          char = input[position + length];
      }
      return [length, { type: type, value: value, sourceCodeInfo: sourceCodeInfo }];
  }

  // All tokenizers, order matters!
  var tokenizers = [
      skipComment,
      skipWhiteSpace,
      tokenizeLeftParen,
      tokenizeRightParen,
      tokenizeLeftBracket,
      tokenizeRightBracket,
      tokenizeLeftCurly,
      tokenizeRightCurly,
      tokenizeString,
      tokenizeSymbolString,
      tokenizeNumber,
      tokenizeReservedName,
      tokenizeName,
      tokenizeModifier,
      tokenizeRegexpShorthand,
      tokenizeFnShorthand,
  ];
  var TokenMetaImpl = /** @class */ (function () {
      function TokenMetaImpl(line, column, sourceCodeLine) {
          this.line = line;
          this.column = column;
          this.sourceCodeLine = sourceCodeLine;
      }
      Object.defineProperty(TokenMetaImpl.prototype, "position", {
          get: function () {
              return "(" + this.line + ":" + this.column + ")";
          },
          enumerable: false,
          configurable: true
      });
      TokenMetaImpl.prototype.getMarker = function (unindent) {
          return "\n" + " ".repeat(this.column - 1 - unindent) + "^";
      };
      Object.defineProperty(TokenMetaImpl.prototype, "debugInfo", {
          get: function () {
              var unindent = this.sourceCodeLine.replace(/^( *).*/, "$1").length;
              return "\n" + this.sourceCodeLine.substr(unindent) + this.getMarker(unindent);
          },
          enumerable: false,
          configurable: true
      });
      TokenMetaImpl.prototype.toString = function () {
          return "" + this.position + this.debugInfo;
      };
      return TokenMetaImpl;
  }());
  function getSourceCodeLine(input, lineNbr) {
      return input.split(/\r\n|\r|\n/)[lineNbr];
  }
  function createSourceCodeInfo(input, position) {
      var lines = input.substr(0, position + 1).split(/\r\n|\r|\n/);
      var lastLine = lines[lines.length - 1];
      var sourceCodeLine = getSourceCodeLine(input, lines.length - 1);
      return new TokenMetaImpl(lines.length, lastLine.length, sourceCodeLine);
  }
  function tokenize(input, debug) {
      var tokens = [];
      var position = 0;
      var tokenized = false;
      while (position < input.length) {
          tokenized = false;
          // Loop through all tokenizer until one matches
          var sourceCodeInfo = debug ? createSourceCodeInfo(input, position) : null;
          for (var _i = 0, tokenizers_1 = tokenizers; _i < tokenizers_1.length; _i++) {
              var tokenize_1 = tokenizers_1[_i];
              var _a = tokenize_1(input, position, sourceCodeInfo), nbrOfCharacters = _a[0], token = _a[1];
              // tokenizer matched
              if (nbrOfCharacters > 0) {
                  tokenized = true;
                  position += nbrOfCharacters;
                  if (token) {
                      tokens.push(token);
                  }
                  break;
              }
          }
          if (!tokenized) {
              throw new LitsError("Unrecognized character '" + input[position] + "'.", sourceCodeInfo);
          }
      }
      return tokens;
  }

  var Cache = /** @class */ (function () {
      function Cache(maxSize) {
          this.cache = {};
          this.firstEntry = undefined;
          this.lastEntry = undefined;
          this._size = 0;
          this.maxSize = toNonNegativeInteger(maxSize);
          if (this.maxSize < 1) {
              throw Error("1 is the minimum maxSize, got " + valueToString$1(maxSize));
          }
      }
      Object.defineProperty(Cache.prototype, "size", {
          get: function () {
              return this._size;
          },
          enumerable: false,
          configurable: true
      });
      Cache.prototype.get = function (key) {
          var _a;
          return (_a = this.cache[key]) === null || _a === void 0 ? void 0 : _a.value;
      };
      Cache.prototype.clear = function () {
          this.cache = {};
          this.firstEntry = undefined;
          this.lastEntry = undefined;
          this._size = 0;
      };
      Cache.prototype.has = function (key) {
          return !!this.cache[key];
      };
      Cache.prototype.set = function (key, value) {
          if (this.has(key)) {
              throw Error("AstCache - key already present: " + key);
          }
          var newEntry = { value: value, nextEntry: undefined, key: key };
          this.cache[key] = newEntry;
          this._size += 1;
          if (this.lastEntry) {
              this.lastEntry.nextEntry = newEntry;
          }
          this.lastEntry = newEntry;
          if (!this.firstEntry) {
              this.firstEntry = this.lastEntry;
          }
          while (this.size > this.maxSize) {
              this.dropFirstEntry();
          }
      };
      Cache.prototype.dropFirstEntry = function () {
          var firstEntry = this.firstEntry;
          delete this.cache[firstEntry.key];
          this._size -= 1;
          this.firstEntry = firstEntry.nextEntry;
      };
      return Cache;
  }());

  var Lits = /** @class */ (function () {
      function Lits(config) {
          if (config === void 0) { config = {}; }
          var _a;
          this.debug = (_a = config.debug) !== null && _a !== void 0 ? _a : false;
          if (config.astCacheSize && config.astCacheSize > 0) {
              this.astCache = new Cache(config.astCacheSize);
          }
          else {
              this.astCache = null;
          }
      }
      Lits.prototype.run = function (program, params) {
          var ast = this.generateAst(program);
          var result = this.evaluate(ast, params);
          return result;
      };
      Lits.prototype.context = function (program, params) {
          if (params === void 0) { params = {}; }
          var contextStack = createContextStackFromParams(params);
          var ast = this.generateAst(program);
          evaluate(ast, contextStack);
          return contextStack.globalContext;
      };
      Lits.prototype.tokenize = function (program) {
          return tokenize(program, this.debug);
      };
      Lits.prototype.parse = function (tokens) {
          return parse(tokens);
      };
      Lits.prototype.evaluate = function (ast, params) {
          var contextStack = createContextStackFromParams(params);
          return evaluate(ast, contextStack);
      };
      Lits.prototype.generateAst = function (program) {
          var _a;
          if (this.astCache) {
              var cachedAst = this.astCache.get(program);
              if (cachedAst) {
                  return cachedAst;
              }
          }
          var tokens = this.tokenize(program);
          var ast = this.parse(tokens);
          (_a = this.astCache) === null || _a === void 0 ? void 0 : _a.set(program, ast);
          return ast;
      };
      return Lits;
  }());
  function createContextStackFromParams(params) {
      var _a, _b;
      var globalContext = (_a = params === null || params === void 0 ? void 0 : params.globalContext) !== null && _a !== void 0 ? _a : {};
      Object.assign(globalContext, createContextFromValues(params === null || params === void 0 ? void 0 : params.globals));
      var contextStack = createContextStack(__spreadArray([globalContext], ((_b = params === null || params === void 0 ? void 0 : params.contexts) !== null && _b !== void 0 ? _b : [])));
      return contextStack;
  }

  exports.Lits = Lits;
  exports.isLitsFunction = isLitsFunction;
  exports.normalExpressionKeys = normalExpressionKeys;
  exports.reservedNames = reservedNames;
  exports.specialExpressionKeys = specialExpressionKeys;

  Object.defineProperty(exports, '__esModule', { value: true });

  return exports;

})({});
//# sourceMappingURL=lits.iife.js.map
