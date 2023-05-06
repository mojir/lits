var Lits = (function (exports) {
  'use strict';

  var FUNCTION_SYMBOL = "__LITS_FUNCTION__";
  var REGEXP_SYMBOL = "__REGEXP__";

  // eslint-disable-next-line @typescript-eslint/no-explicit-any,@typescript-eslint/explicit-module-boundary-types
  function getDebugInfo(anyValue, debugInfo) {
      var _a;
      return (_a = anyValue === null || anyValue === void 0 ? void 0 : anyValue.debugInfo) !== null && _a !== void 0 ? _a : debugInfo;
  }
  function getCodeMarker(sourceCodeInfo) {
      var leftPadding = sourceCodeInfo.column - 1;
      var rightPadding = sourceCodeInfo.code.length - leftPadding - 1;
      return "".concat(" ".repeat(Math.max(leftPadding, 0)), "^").concat(" ".repeat(Math.max(rightPadding, 0)));
  }
  function valueToString$1(value) {
      if (isLitsFunction(value)) {
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          return "<function ".concat(value.name || "\u03BB", ">");
      }
      if (isToken(value)) {
          return "".concat(value.type, "-token \"").concat(value.value, "\"");
      }
      if (isAstNode(value)) {
          return "".concat(value.type, "-node");
      }
      if (value === null) {
          return "null";
      }
      if (typeof value === "object" && value instanceof RegExp) {
          return "".concat(value);
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
  function isRegularExpression(regexp) {
      if (regexp === null || typeof regexp !== "object") {
          return false;
      }
      return !!regexp[REGEXP_SYMBOL];
  }

  /******************************************************************************
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

  function __values(o) {
      var s = typeof Symbol === "function" && Symbol.iterator, m = s && o[s], i = 0;
      if (m) return m.call(o);
      if (o && typeof o.length === "number") return {
          next: function () {
              if (o && i >= o.length) o = void 0;
              return { value: o && o[i++], done: !o };
          }
      };
      throw new TypeError(s ? "Object is not iterable." : "Symbol.iterator is not defined.");
  }

  function __read(o, n) {
      var m = typeof Symbol === "function" && o[Symbol.iterator];
      if (!m) return o;
      var i = m.call(o), r, ar = [], e;
      try {
          while ((n === void 0 || n-- > 0) && !(r = i.next()).done) ar.push(r.value);
      }
      catch (error) { e = { error: error }; }
      finally {
          try {
              if (r && !r.done && (m = i["return"])) m.call(i);
          }
          finally { if (e) throw e.error; }
      }
      return ar;
  }

  function __spreadArray(to, from, pack) {
      if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
          if (ar || !(i in from)) {
              if (!ar) ar = Array.prototype.slice.call(from, 0, i);
              ar[i] = from[i];
          }
      }
      return to.concat(ar || Array.prototype.slice.call(from));
  }

  function getLitsErrorMessage(message, debugInfo) {
      return "".concat(message).concat(debugInfo ? "\n".concat(debugInfo === "EOF" ? "EOF" : "".concat(debugInfo.code, "\n").concat(getCodeMarker(debugInfo))) : "");
  }
  var RecurSignal = /** @class */ (function (_super) {
      __extends(RecurSignal, _super);
      function RecurSignal(params) {
          var _this = _super.call(this, "recur, params: ".concat(params)) || this;
          Object.setPrototypeOf(_this, RecurSignal.prototype);
          _this.name = "RecurSignal";
          _this.params = params;
          return _this;
      }
      return RecurSignal;
  }(Error));
  var AbstractLitsError = /** @class */ (function (_super) {
      __extends(AbstractLitsError, _super);
      function AbstractLitsError(message, debugInfo) {
          var _this = this;
          if (message instanceof Error) {
              message = "".concat(message.name).concat(message.message ? ": ".concat(message.message) : "");
          }
          _this = _super.call(this, getLitsErrorMessage(message, debugInfo)) || this;
          _this.shortMessage = message;
          _this.debugInfo = debugInfo;
          Object.setPrototypeOf(_this, AbstractLitsError.prototype);
          _this.name = "AbstractLitsError";
          return _this;
      }
      return AbstractLitsError;
  }(Error));
  var LitsError = /** @class */ (function (_super) {
      __extends(LitsError, _super);
      function LitsError(message, debugInfo) {
          var _this = _super.call(this, message, debugInfo) || this;
          Object.setPrototypeOf(_this, LitsError.prototype);
          _this.name = "LitsError";
          return _this;
      }
      return LitsError;
  }(AbstractLitsError));
  var NotAFunctionError = /** @class */ (function (_super) {
      __extends(NotAFunctionError, _super);
      function NotAFunctionError(fn, debugInfo) {
          var _this = this;
          var message = "Expected function, got ".concat(valueToString$1(fn), ".");
          _this = _super.call(this, message, debugInfo) || this;
          Object.setPrototypeOf(_this, NotAFunctionError.prototype);
          _this.name = "NotAFunctionError";
          return _this;
      }
      return NotAFunctionError;
  }(AbstractLitsError));
  var UserDefinedError = /** @class */ (function (_super) {
      __extends(UserDefinedError, _super);
      function UserDefinedError(message, debugInfo) {
          var _this = _super.call(this, message, debugInfo) || this;
          Object.setPrototypeOf(_this, UserDefinedError.prototype);
          _this.name = "UserDefinedError";
          return _this;
      }
      return UserDefinedError;
  }(AbstractLitsError));
  var AssertionError = /** @class */ (function (_super) {
      __extends(AssertionError, _super);
      function AssertionError(message, debugInfo) {
          var _this = _super.call(this, message, debugInfo) || this;
          Object.setPrototypeOf(_this, AssertionError.prototype);
          _this.name = "AssertionError";
          return _this;
      }
      return AssertionError;
  }(AbstractLitsError));
  var UndefinedSymbolError = /** @class */ (function (_super) {
      __extends(UndefinedSymbolError, _super);
      function UndefinedSymbolError(symbolName, debugInfo) {
          var _this = this;
          var message = "Undefined symbol '".concat(symbolName, "'.");
          _this = _super.call(this, message, debugInfo) || this;
          _this.symbol = symbolName;
          Object.setPrototypeOf(_this, UndefinedSymbolError.prototype);
          _this.name = "UndefinedSymbolError";
          return _this;
      }
      return UndefinedSymbolError;
  }(AbstractLitsError));

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
  function assert$2(value, debugInfo, options) {
      if (options === void 0) { options = {}; }
      if (!is$2(value, options)) {
          throw new LitsError("Expected ".concat(options.nonEmpty ? "non empty string" : options.char ? "character" : "string", ", got ").concat(valueToString$1(value), "."), getDebugInfo(value, debugInfo));
      }
  }
  function as$2(value, debugInfo, options) {
      if (options === void 0) { options = {}; }
      assert$2(value, debugInfo, options);
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
          return "".concat(typeof options.gt === "number" ? "".concat(options.gt, " < n ") : "".concat(options.gte, " <= n ")).concat(typeof options.lt === "number" ? "< ".concat(options.lt) : "<= ".concat(options.lte));
      }
      if (typeof options.gt === "number" || typeof options.gte === "number") {
          return "".concat(typeof options.gt === "number" ? "n > ".concat(options.gt) : "n >= ".concat(options.gte));
      }
      if (typeof options.lt === "number" || typeof options.lte === "number") {
          return "".concat(typeof options.lt === "number" ? "n < ".concat(options.lt) : "n <= ".concat(options.lte));
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
  function assert$1(value, debugInfo, options) {
      if (options === void 0) { options = {}; }
      if (!is$1(value, options)) {
          throw new LitsError("Expected ".concat(getNumberTypeName(options), ", got ").concat(valueToString$1(value), "."), getDebugInfo(value, debugInfo));
      }
  }
  function as$1(value, debugInfo, options) {
      if (options === void 0) { options = {}; }
      assert$1(value, debugInfo, options);
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
  function assert(value, debugInfo, options) {
      if (options === void 0) { options = {}; }
      if (!is(value, options)) {
          if (isToken(value)) {
              debugInfo = value.debugInfo;
          }
          throw new LitsError("Expected ".concat(options.type ? "".concat(options.type, "-") : "", "token").concat(typeof options.value === "string" ? " value='".concat(options.value, "'") : "", ", got ").concat(valueToString$1(value), "."), getDebugInfo(value, debugInfo));
      }
  }
  function as(value, debugInfo, options) {
      if (options === void 0) { options = {}; }
      assert(value, debugInfo, options);
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
      Asserter.prototype.assert = function (value, debugInfo) {
          if (!this.predicate(value)) {
              throw new LitsError("Expected ".concat(this.typeName, ", got ").concat(valueToString$1(value), "."), getDebugInfo(value, debugInfo));
          }
      };
      Asserter.prototype.as = function (value, debugInfo) {
          this.assert(value, debugInfo);
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
          isLitsFunction(value) ||
          isRegularExpression(value));
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
  var regularExpression = new Asserter("regularExpression", isRegularExpression);
  var stringOrRegExp = new Asserter("string or regularExpression", function (value) { return isRegularExpression(value) || typeof value === "string"; });
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
      var _a, _b;
      var length = node.params.length;
      var debugInfo = (_a = node.token) === null || _a === void 0 ? void 0 : _a.debugInfo;
      if (typeof count === "number") {
          if (length !== count) {
              throw new LitsError("Wrong number of arguments to \"".concat(node.name, "\", expected ").concat(count, ", got ").concat(valueToString$1(length), "."), (_b = node.token) === null || _b === void 0 ? void 0 : _b.debugInfo);
          }
      }
      else {
          var min = count.min, max = count.max;
          if (min === undefined && max === undefined) {
              throw new LitsError("Min or max must be specified.", debugInfo);
          }
          if (typeof min === "number" && length < min) {
              throw new LitsError("Wrong number of arguments to \"".concat(node.name, "\", expected at least ").concat(min, ", got ").concat(valueToString$1(length), "."), debugInfo);
          }
          if (typeof max === "number" && length > max) {
              throw new LitsError("Wrong number of arguments to \"".concat(node.name, "\", expected at most ").concat(max, ", got ").concat(valueToString$1(length), "."), debugInfo);
          }
      }
  }
  function assertEventNumberOfParams(node) {
      var _a;
      var length = node.params.length;
      if (length % 2 !== 0) {
          throw new LitsError("Wrong number of arguments, expected an even number, got ".concat(valueToString$1(length), "."), (_a = node.token) === null || _a === void 0 ? void 0 : _a.debugInfo);
      }
  }
  function asValue(value, debugInfo) {
      if (value === undefined) {
          throw new LitsError("Unexpected nil", getDebugInfo(value, debugInfo));
      }
      return value;
  }
  function assertValue(value, debugInfo) {
      if (value === undefined) {
          throw new LitsError("Unexpected nil.", getDebugInfo(value, debugInfo));
      }
  }

  var andSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = __read(parseTokens(tokens, position), 2), newPosition = _b[0], params = _b[1];
          return [
              newPosition + 1,
              {
                  type: "SpecialExpression",
                  name: "and",
                  params: params,
                  token: firstToken.debugInfo ? firstToken : undefined,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var e_1, _b;
          var evaluateAstNode = _a.evaluateAstNode;
          var value = true;
          try {
              for (var _c = __values(node.params), _d = _c.next(); !_d.done; _d = _c.next()) {
                  var param = _d.value;
                  value = evaluateAstNode(param, contextStack);
                  if (!value) {
                      break;
                  }
              }
          }
          catch (e_1_1) { e_1 = { error: e_1_1 }; }
          finally {
              try {
                  if (_d && !_d.done && (_b = _c.return)) _b.call(_c);
              }
              finally { if (e_1) throw e_1.error; }
          }
          return value;
      },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          return analyzeAst(node.params, contextStack, builtin);
      },
  };

  function parseConditions(tokens, position, parseToken) {
      var _a, _b;
      var conditions = [];
      var tkn = token.as(tokens[position], "EOF");
      while (!token.is(tkn, { type: "paren", value: ")" })) {
          var test_1 = void 0;
          _a = __read(parseToken(tokens, position), 2), position = _a[0], test_1 = _a[1];
          var form = void 0;
          _b = __read(parseToken(tokens, position), 2), position = _b[0], form = _b[1];
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
          _b = __read(parseConditions(tokens, position, parseToken), 2), position = _b[0], conditions = _b[1];
          return [
              position + 1,
              {
                  type: "SpecialExpression",
                  name: "cond",
                  conditions: conditions,
                  params: [],
                  token: firstToken.debugInfo ? firstToken : undefined,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var e_1, _b;
          var evaluateAstNode = _a.evaluateAstNode;
          try {
              for (var _c = __values(node.conditions), _d = _c.next(); !_d.done; _d = _c.next()) {
                  var condition = _d.value;
                  var value = evaluateAstNode(condition.test, contextStack);
                  if (!value) {
                      continue;
                  }
                  return evaluateAstNode(condition.form, contextStack);
              }
          }
          catch (e_1_1) { e_1 = { error: e_1_1 }; }
          finally {
              try {
                  if (_d && !_d.done && (_b = _c.return)) _b.call(_c);
              }
              finally { if (e_1) throw e_1.error; }
          }
          return null;
      },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          var astNodes = node.conditions.flatMap(function (condition) { return [condition.test, condition.form]; });
          return analyzeAst(astNodes, contextStack, builtin);
      },
  };

  function joinAnalyzeResults() {
      var e_1, _a;
      var results = [];
      for (var _i = 0; _i < arguments.length; _i++) {
          results[_i] = arguments[_i];
      }
      var result = {
          undefinedSymbols: new Set(),
      };
      try {
          for (var results_1 = __values(results), results_1_1 = results_1.next(); !results_1_1.done; results_1_1 = results_1.next()) {
              var input = results_1_1.value;
              input.undefinedSymbols.forEach(function (symbol) { return result.undefinedSymbols.add(symbol); });
          }
      }
      catch (e_1_1) { e_1 = { error: e_1_1 }; }
      finally {
          try {
              if (results_1_1 && !results_1_1.done && (_a = results_1.return)) _a.call(results_1);
          }
          finally { if (e_1) throw e_1.error; }
      }
      return result;
  }
  function addAnalyzeResults(target, source) {
      source.undefinedSymbols.forEach(function (symbol) { return target.undefinedSymbols.add(symbol); });
  }

  var reservedNamesRecord = {
      true: { value: true },
      false: { value: false },
      nil: { value: null },
      null: { value: null, forbidden: true },
      undefined: { value: null, forbidden: true },
      '===': { value: null, forbidden: true },
      '!==': { value: null, forbidden: true },
      '&&': { value: null, forbidden: true },
      '||': { value: null, forbidden: true },
  };
  var reservedNames = Object.keys(reservedNamesRecord);

  function assertNameNotDefined(name, contextStack, builtin, debugInfo) {
      if (typeof name !== "string") {
          return;
      }
      if (builtin.specialExpressions[name]) {
          throw new LitsError("Cannot define variable ".concat(name, ", it's a special expression."), debugInfo);
      }
      if (builtin.normalExpressions[name]) {
          throw new LitsError("Cannot define variable ".concat(name, ", it's a builtin function."), debugInfo);
      }
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      if (reservedNamesRecord[name]) {
          throw new LitsError("Cannot define variable ".concat(name, ", it's a reserved name."), debugInfo);
      }
      if (contextStack.globalContext[name]) {
          throw new LitsError("Name already defined \"".concat(name, "\"."), debugInfo);
      }
  }

  var defnSpecialExpression = {
      parse: function (tokens, position, parsers) {
          var _a, _b;
          var _c;
          var firstToken = token.as(tokens[position], "EOF");
          var parseToken = parsers.parseToken;
          var functionName = undefined;
          _a = __read(parseToken(tokens, position), 2), position = _a[0], functionName = _a[1];
          nameNode.assert(functionName, (_c = functionName.token) === null || _c === void 0 ? void 0 : _c.debugInfo);
          var functionOverloades;
          _b = __read(parseFunctionOverloades(tokens, position, parsers), 2), position = _b[0], functionOverloades = _b[1];
          return [
              position,
              {
                  type: "SpecialExpression",
                  name: "defn",
                  functionName: functionName,
                  params: [],
                  overloads: functionOverloades,
                  token: firstToken.debugInfo ? firstToken : undefined,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var _b;
          var _c, _d;
          var builtin = _a.builtin, evaluateAstNode = _a.evaluateAstNode;
          var name = getFunctionName("defn", node, contextStack, evaluateAstNode);
          assertNameNotDefined(name, contextStack, builtin, (_c = node.token) === null || _c === void 0 ? void 0 : _c.debugInfo);
          var evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode);
          var litsFunction = (_b = {},
              _b[FUNCTION_SYMBOL] = true,
              _b.debugInfo = (_d = node.token) === null || _d === void 0 ? void 0 : _d.debugInfo,
              _b.type = "user-defined",
              _b.name = name,
              _b.overloads = evaluatedFunctionOverloades,
              _b);
          contextStack.globalContext[name] = { value: litsFunction };
          return null;
      },
      analyze: function (node, contextStack, _a) {
          var _b;
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          contextStack.globalContext[node.functionName.value] = { value: true };
          var newContext = (_b = {}, _b[node.functionName.value] = { value: true }, _b);
          return addOverloadsUndefinedSymbols(node.overloads, contextStack, analyzeAst, builtin, newContext);
      },
  };
  var defnsSpecialExpression = {
      parse: function (tokens, position, parsers) {
          var _a, _b;
          var firstToken = token.as(tokens[position], "EOF");
          var parseToken = parsers.parseToken;
          var functionName;
          _a = __read(parseToken(tokens, position), 2), position = _a[0], functionName = _a[1];
          var functionOverloades;
          _b = __read(parseFunctionOverloades(tokens, position, parsers), 2), position = _b[0], functionOverloades = _b[1];
          return [
              position,
              {
                  type: "SpecialExpression",
                  name: "defns",
                  functionName: functionName,
                  params: [],
                  overloads: functionOverloades,
                  token: firstToken.debugInfo ? firstToken : undefined,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var _b;
          var _c, _d;
          var builtin = _a.builtin, evaluateAstNode = _a.evaluateAstNode;
          var name = getFunctionName("defns", node, contextStack, evaluateAstNode);
          assertNameNotDefined(name, contextStack, builtin, (_c = node.token) === null || _c === void 0 ? void 0 : _c.debugInfo);
          var evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode);
          var litsFunction = (_b = {},
              _b[FUNCTION_SYMBOL] = true,
              _b.debugInfo = (_d = node.token) === null || _d === void 0 ? void 0 : _d.debugInfo,
              _b.type = "user-defined",
              _b.name = name,
              _b.overloads = evaluatedFunctionOverloades,
              _b);
          contextStack.globalContext[name] = { value: litsFunction };
          return null;
      },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          return addOverloadsUndefinedSymbols(node.overloads, contextStack, analyzeAst, builtin);
      },
  };
  var fnSpecialExpression = {
      parse: function (tokens, position, parsers) {
          var _a;
          var firstToken = token.as(tokens[position], "EOF");
          var functionOverloades;
          _a = __read(parseFunctionOverloades(tokens, position, parsers), 2), position = _a[0], functionOverloades = _a[1];
          return [
              position,
              {
                  type: "SpecialExpression",
                  name: "fn",
                  params: [],
                  overloads: functionOverloades,
                  token: firstToken.debugInfo ? firstToken : undefined,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var _b;
          var _c;
          var evaluateAstNode = _a.evaluateAstNode;
          var evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode);
          var litsFunction = (_b = {},
              _b[FUNCTION_SYMBOL] = true,
              _b.debugInfo = (_c = node.token) === null || _c === void 0 ? void 0 : _c.debugInfo,
              _b.type = "user-defined",
              _b.name = undefined,
              _b.overloads = evaluatedFunctionOverloades,
              _b);
          return litsFunction;
      },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          return addOverloadsUndefinedSymbols(node.overloads, contextStack, analyzeAst, builtin);
      },
  };
  function getFunctionName(expressionName, node, contextStack, evaluateAstNode) {
      var _a;
      var debugInfo = (_a = node.token) === null || _a === void 0 ? void 0 : _a.debugInfo;
      if (expressionName === "defn") {
          return node.functionName.value;
      }
      var name = evaluateAstNode(node.functionName, contextStack);
      string.assert(name, debugInfo);
      return name;
  }
  function evaluateFunctionOverloades(node, contextStack, evaluateAstNode) {
      var e_1, _a, e_2, _b;
      var evaluatedFunctionOverloades = [];
      try {
          for (var _c = __values(node.overloads), _d = _c.next(); !_d.done; _d = _c.next()) {
              var functionOverload = _d.value;
              var functionContext = {};
              try {
                  for (var _e = (e_2 = void 0, __values(functionOverload.arguments.bindings)), _f = _e.next(); !_f.done; _f = _e.next()) {
                      var binding = _f.value;
                      var bindingValueNode = binding.value;
                      var bindingValue = evaluateAstNode(bindingValueNode, contextStack);
                      functionContext[binding.name] = { value: bindingValue };
                  }
              }
              catch (e_2_1) { e_2 = { error: e_2_1 }; }
              finally {
                  try {
                      if (_f && !_f.done && (_b = _e.return)) _b.call(_e);
                  }
                  finally { if (e_2) throw e_2.error; }
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
      }
      catch (e_1_1) { e_1 = { error: e_1_1 }; }
      finally {
          try {
              if (_d && !_d.done && (_a = _c.return)) _a.call(_c);
          }
          finally { if (e_1) throw e_1.error; }
      }
      return evaluatedFunctionOverloades;
  }
  function addOverloadsUndefinedSymbols(overloads, contextStack, analyzeAst, builtin, functionNameContext) {
      var e_3, _a;
      var result = { undefinedSymbols: new Set() };
      var contextStackWithFunctionName = functionNameContext
          ? contextStack.withContext(functionNameContext)
          : contextStack;
      var _loop_1 = function (overload) {
          var newContext = {};
          overload.arguments.bindings.forEach(function (binding) {
              var bindingResult = analyzeAst(binding.value, contextStack, builtin);
              addAnalyzeResults(result, bindingResult);
              newContext[binding.name] = { value: true };
          });
          overload.arguments.mandatoryArguments.forEach(function (arg) {
              newContext[arg] = { value: true };
          });
          if (typeof overload.arguments.restArgument === "string") {
              newContext[overload.arguments.restArgument] = { value: true };
          }
          var newContextStack = contextStackWithFunctionName.withContext(newContext);
          var overloadResult = analyzeAst(overload.body, newContextStack, builtin);
          addAnalyzeResults(result, overloadResult);
      };
      try {
          for (var overloads_1 = __values(overloads), overloads_1_1 = overloads_1.next(); !overloads_1_1.done; overloads_1_1 = overloads_1.next()) {
              var overload = overloads_1_1.value;
              _loop_1(overload);
          }
      }
      catch (e_3_1) { e_3 = { error: e_3_1 }; }
      finally {
          try {
              if (overloads_1_1 && !overloads_1_1.done && (_a = overloads_1.return)) _a.call(overloads_1);
          }
          finally { if (e_3) throw e_3.error; }
      }
      return result;
  }
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
          _b = __read(parseToken(tokens, position), 2), position = _b[0], bodyNode = _b[1];
          body.push(bodyNode);
          tkn = token.as(tokens[position], "EOF");
      }
      if (body.length === 0) {
          throw new LitsError("Missing body in function", tkn.debugInfo);
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
              _a = __read(parseFunctionArguments(tokens, position, parsers), 2), position = _a[0], functionArguments = _a[1];
              var arity = functionArguments.restArgument
                  ? { min: functionArguments.mandatoryArguments.length }
                  : functionArguments.mandatoryArguments.length;
              if (!arityOk(functionOverloades, arity)) {
                  throw new LitsError("All overloaded functions must have different arity", tkn.debugInfo);
              }
              var functionBody = void 0;
              _b = __read(parseFunctionBody(tokens, position, parsers), 2), position = _b[0], functionBody = _b[1];
              functionOverloades.push({
                  arguments: functionArguments,
                  body: functionBody,
                  arity: arity,
              });
              tkn = token.as(tokens[position], "EOF", { type: "paren" });
              if (tkn.value !== ")" && tkn.value !== "(") {
                  throw new LitsError("Expected ( or ) token, got ".concat(valueToString$1(tkn), "."), tkn.debugInfo);
              }
          }
          return [position + 1, functionOverloades];
      }
      else if (tkn.value === "[") {
          var functionArguments = void 0;
          _c = __read(parseFunctionArguments(tokens, position, parsers), 2), position = _c[0], functionArguments = _c[1];
          var arity = functionArguments.restArgument
              ? { min: functionArguments.mandatoryArguments.length }
              : functionArguments.mandatoryArguments.length;
          var functionBody = void 0;
          _d = __read(parseFunctionBody(tokens, position, parsers), 2), position = _d[0], functionBody = _d[1];
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
          throw new LitsError("Expected [ or ( token, got ".concat(valueToString$1(tkn)), tkn.debugInfo);
      }
  }
  function parseFunctionArguments(tokens, position, parsers) {
      var _a;
      var parseArgument = parsers.parseArgument, parseBindings = parsers.parseBindings;
      var bindings = [];
      var restArgument = undefined;
      var mandatoryArguments = [];
      var state = "mandatory";
      var tkn = token.as(tokens[position], "EOF");
      position += 1;
      tkn = token.as(tokens[position], "EOF");
      while (!(tkn.type === "paren" && tkn.value === "]")) {
          if (state === "let") {
              _a = __read(parseBindings(tokens, position), 2), position = _a[0], bindings = _a[1];
              break;
          }
          else {
              var _b = __read(parseArgument(tokens, position), 2), newPosition = _b[0], node = _b[1];
              position = newPosition;
              tkn = token.as(tokens[position], "EOF");
              if (node.type === "Modifier") {
                  switch (node.value) {
                      case "&":
                          if (state === "rest") {
                              throw new LitsError("& can only appear once", tkn.debugInfo);
                          }
                          state = "rest";
                          break;
                      case "&let":
                          if (state === "rest" && !restArgument) {
                              throw new LitsError("No rest argument was specified", tkn.debugInfo);
                          }
                          state = "let";
                          break;
                      default:
                          throw new LitsError("Illegal modifier: ".concat(node.value), tkn.debugInfo);
                  }
              }
              else {
                  switch (state) {
                      case "mandatory":
                          mandatoryArguments.push(node.name);
                          break;
                      case "rest":
                          if (restArgument !== undefined) {
                              throw new LitsError("Can only specify one rest argument", tkn.debugInfo);
                          }
                          restArgument = node.name;
                          break;
                  }
              }
          }
      }
      if (state === "rest" && restArgument === undefined) {
          throw new LitsError("Missing rest argument name", tkn.debugInfo);
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
          var _b = __read(parseTokens(tokens, position), 2), newPosition = _b[0], params = _b[1];
          nameNode.assert(params[0], firstToken.debugInfo);
          return [
              newPosition + 1,
              {
                  type: "SpecialExpression",
                  name: "def",
                  params: params,
                  token: firstToken.debugInfo ? firstToken : undefined,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var _b;
          var evaluateAstNode = _a.evaluateAstNode, builtin = _a.builtin;
          var debugInfo = (_b = node.token) === null || _b === void 0 ? void 0 : _b.debugInfo;
          var name = nameNode.as(node.params[0], debugInfo).value;
          assertNameNotDefined(name, contextStack, builtin, debugInfo);
          var value = evaluateAstNode(astNode.as(node.params[1], debugInfo), contextStack);
          contextStack.globalContext[name] = { value: value };
          return value;
      },
      validate: function (node) { return assertNumberOfParams(2, node); },
      analyze: function (node, contextStack, _a) {
          var _b;
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          var debugInfo = (_b = node.token) === null || _b === void 0 ? void 0 : _b.debugInfo;
          var subNode = astNode.as(node.params[1], debugInfo);
          var result = analyzeAst(subNode, contextStack, builtin);
          var name = nameNode.as(node.params[0], debugInfo).value;
          assertNameNotDefined(name, contextStack, builtin, debugInfo);
          contextStack.globalContext[name] = { value: true };
          return result;
      },
  };

  var defsSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = __read(parseTokens(tokens, position), 2), newPosition = _b[0], params = _b[1];
          return [
              newPosition + 1,
              {
                  type: "SpecialExpression",
                  name: "defs",
                  params: params,
                  token: firstToken.debugInfo ? firstToken : undefined,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var _b, _c;
          var evaluateAstNode = _a.evaluateAstNode, builtin = _a.builtin;
          var debugInfo = (_b = node.token) === null || _b === void 0 ? void 0 : _b.debugInfo;
          var name = evaluateAstNode(astNode.as(node.params[0], debugInfo), contextStack);
          string.assert(name, debugInfo);
          assertNameNotDefined(name, contextStack, builtin, (_c = node.token) === null || _c === void 0 ? void 0 : _c.debugInfo);
          var value = evaluateAstNode(astNode.as(node.params[1], debugInfo), contextStack);
          contextStack.globalContext[name] = { value: value };
          return value;
      },
      validate: function (node) { return assertNumberOfParams(2, node); },
      analyze: function (node, contextStack, _a) {
          var _b;
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          var subNode = astNode.as(node.params[1], (_b = node.token) === null || _b === void 0 ? void 0 : _b.debugInfo);
          return analyzeAst(subNode, contextStack, builtin);
      },
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
              token: tkn.debugInfo ? tkn : undefined,
          };
          while (!token.is(tkn, { type: "paren", value: ")" })) {
              var bodyNode = void 0;
              _b = __read(parseToken(tokens, position), 2), position = _b[0], bodyNode = _b[1];
              node.params.push(bodyNode);
              tkn = token.as(tokens[position], "EOF");
          }
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var e_1, _b;
          var evaluateAstNode = _a.evaluateAstNode;
          var newContext = {};
          var newContextStack = contextStack.withContext(newContext);
          var result = null;
          try {
              for (var _c = __values(node.params), _d = _c.next(); !_d.done; _d = _c.next()) {
                  var form = _d.value;
                  result = evaluateAstNode(form, newContextStack);
              }
          }
          catch (e_1_1) { e_1 = { error: e_1_1 }; }
          finally {
              try {
                  if (_d && !_d.done && (_b = _c.return)) _b.call(_c);
              }
              finally { if (e_1) throw e_1.error; }
          }
          return result;
      },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          return analyzeAst(node.params, contextStack, builtin);
      },
  };

  function parseLoopBinding(tokens, position, _a) {
      var _b, _c, _d, _e;
      var parseBinding = _a.parseBinding, parseBindings = _a.parseBindings, parseToken = _a.parseToken;
      var bindingNode;
      _b = __read(parseBinding(tokens, position), 2), position = _b[0], bindingNode = _b[1];
      var loopBinding = {
          binding: bindingNode,
          modifiers: [],
      };
      var tkn = token.as(tokens[position], "EOF");
      while (tkn.type === "modifier") {
          switch (tkn.value) {
              case "&let":
                  if (loopBinding.letBindings) {
                      throw new LitsError("Only one &let modifier allowed", tkn.debugInfo);
                  }
                  _c = __read(parseBindings(tokens, position + 1), 2), position = _c[0], loopBinding.letBindings = _c[1];
                  loopBinding.modifiers.push("&let");
                  break;
              case "&when":
                  if (loopBinding.whenNode) {
                      throw new LitsError("Only one &when modifier allowed", tkn.debugInfo);
                  }
                  _d = __read(parseToken(tokens, position + 1), 2), position = _d[0], loopBinding.whenNode = _d[1];
                  loopBinding.modifiers.push("&when");
                  break;
              case "&while":
                  if (loopBinding.whileNode) {
                      throw new LitsError("Only one &while modifier allowed", tkn.debugInfo);
                  }
                  _e = __read(parseToken(tokens, position + 1), 2), position = _e[0], loopBinding.whileNode = _e[1];
                  loopBinding.modifiers.push("&while");
                  break;
              default:
                  throw new LitsError("Illegal modifier: ".concat(tkn.value), tkn.debugInfo);
          }
          tkn = token.as(tokens[position], "EOF");
      }
      return [position, loopBinding];
  }
  function addToContext(bindings, context, contextStack, evaluateAstNode, debugInfo) {
      var e_1, _a;
      try {
          for (var bindings_1 = __values(bindings), bindings_1_1 = bindings_1.next(); !bindings_1_1.done; bindings_1_1 = bindings_1.next()) {
              var binding = bindings_1_1.value;
              if (context[binding.name]) {
                  throw new LitsError("Variable already defined: ".concat(binding.name, "."), debugInfo);
              }
              context[binding.name] = { value: evaluateAstNode(binding.value, contextStack) };
          }
      }
      catch (e_1_1) { e_1 = { error: e_1_1 }; }
      finally {
          try {
              if (bindings_1_1 && !bindings_1_1.done && (_a = bindings_1.return)) _a.call(bindings_1);
          }
          finally { if (e_1) throw e_1.error; }
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
          _a = __read(parseLoopBinding(tokens, position, parsers), 2), position = _a[0], loopBinding = _a[1];
          loopBindings.push(loopBinding);
          tkn = token.as(tokens[position], "EOF");
      }
      return [position + 1, loopBindings];
  }
  function evaluateLoop(returnResult, node, contextStack, evaluateAstNode) {
      var e_2, _a;
      var _b;
      var debugInfo = (_b = node.token) === null || _b === void 0 ? void 0 : _b.debugInfo;
      var _c = node, loopBindings = _c.loopBindings, params = _c.params;
      var expression = astNode.as(params[0], debugInfo);
      var result = [];
      var bindingIndices = loopBindings.map(function () { return 0; });
      var abort = false;
      while (!abort) {
          var context = {};
          var newContextStack = contextStack.withContext(context);
          var skip = false;
          bindingsLoop: for (var bindingIndex = 0; bindingIndex < loopBindings.length; bindingIndex += 1) {
              var _d = asValue(loopBindings[bindingIndex], debugInfo), binding = _d.binding, letBindings = _d.letBindings, whenNode = _d.whenNode, whileNode = _d.whileNode, modifiers = _d.modifiers;
              var coll = collection.as(evaluateAstNode(binding.value, newContextStack), debugInfo);
              var seq = sequence.is(coll) ? coll : Object.entries(coll);
              if (seq.length === 0) {
                  skip = true;
                  abort = true;
                  break;
              }
              var index = asValue(bindingIndices[bindingIndex], debugInfo);
              if (index >= seq.length) {
                  skip = true;
                  if (bindingIndex === 0) {
                      abort = true;
                      break;
                  }
                  bindingIndices[bindingIndex] = 0;
                  bindingIndices[bindingIndex - 1] = asValue(bindingIndices[bindingIndex - 1], debugInfo) + 1;
                  break;
              }
              if (context[binding.name]) {
                  throw new LitsError("Variable already defined: ".concat(binding.name, "."), debugInfo);
              }
              context[binding.name] = {
                  value: any.as(seq[index], debugInfo),
              };
              try {
                  for (var modifiers_1 = (e_2 = void 0, __values(modifiers)), modifiers_1_1 = modifiers_1.next(); !modifiers_1_1.done; modifiers_1_1 = modifiers_1.next()) {
                      var modifier = modifiers_1_1.value;
                      switch (modifier) {
                          case "&let":
                              addToContext(asValue(letBindings, debugInfo), context, newContextStack, evaluateAstNode, debugInfo);
                              break;
                          case "&when":
                              if (!evaluateAstNode(astNode.as(whenNode, debugInfo), newContextStack)) {
                                  bindingIndices[bindingIndex] = asValue(bindingIndices[bindingIndex], debugInfo) + 1;
                                  skip = true;
                                  break bindingsLoop;
                              }
                              break;
                          case "&while":
                              if (!evaluateAstNode(astNode.as(whileNode, debugInfo), newContextStack)) {
                                  bindingIndices[bindingIndex] = Number.POSITIVE_INFINITY;
                                  skip = true;
                                  break bindingsLoop;
                              }
                              break;
                      }
                  }
              }
              catch (e_2_1) { e_2 = { error: e_2_1 }; }
              finally {
                  try {
                      if (modifiers_1_1 && !modifiers_1_1.done && (_a = modifiers_1.return)) _a.call(modifiers_1);
                  }
                  finally { if (e_2) throw e_2.error; }
              }
          }
          if (!skip) {
              var value = evaluateAstNode(expression, newContextStack);
              if (returnResult) {
                  result.push(value);
              }
              bindingIndices[bindingIndices.length - 1] += 1;
          }
      }
      return returnResult ? result : null;
  }
  function analyze(node, contextStack, analyzeAst, builtin) {
      var result = {
          undefinedSymbols: new Set(),
      };
      var newContext = {};
      var loopBindings = node.loopBindings;
      loopBindings.forEach(function (loopBinding) {
          var binding = loopBinding.binding, letBindings = loopBinding.letBindings, whenNode = loopBinding.whenNode, whileNode = loopBinding.whileNode;
          analyzeAst(binding.value, contextStack.withContext(newContext), builtin).undefinedSymbols.forEach(function (symbol) {
              return result.undefinedSymbols.add(symbol);
          });
          newContext[binding.name] = { value: true };
          if (letBindings) {
              letBindings.forEach(function (letBinding) {
                  analyzeAst(letBinding.value, contextStack.withContext(newContext), builtin).undefinedSymbols.forEach(function (symbol) {
                      return result.undefinedSymbols.add(symbol);
                  });
                  newContext[letBinding.name] = { value: true };
              });
          }
          if (whenNode) {
              analyzeAst(whenNode, contextStack.withContext(newContext), builtin).undefinedSymbols.forEach(function (symbol) {
                  return result.undefinedSymbols.add(symbol);
              });
          }
          if (whileNode) {
              analyzeAst(whileNode, contextStack.withContext(newContext), builtin).undefinedSymbols.forEach(function (symbol) {
                  return result.undefinedSymbols.add(symbol);
              });
          }
      });
      analyzeAst(node.params, contextStack.withContext(newContext), builtin).undefinedSymbols.forEach(function (symbol) {
          return result.undefinedSymbols.add(symbol);
      });
      return result;
  }
  var forSpecialExpression = {
      parse: function (tokens, position, parsers) {
          var _a, _b;
          var firstToken = token.as(tokens[position], "EOF");
          var parseToken = parsers.parseToken;
          var loopBindings;
          _a = __read(parseLoopBindings(tokens, position, parsers), 2), position = _a[0], loopBindings = _a[1];
          var expression;
          _b = __read(parseToken(tokens, position), 2), position = _b[0], expression = _b[1];
          token.assert(tokens[position], "EOF", { type: "paren", value: ")" });
          var node = {
              name: "for",
              type: "SpecialExpression",
              loopBindings: loopBindings,
              params: [expression],
              token: firstToken.debugInfo ? firstToken : undefined,
          };
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, helpers) { return evaluateLoop(true, node, contextStack, helpers.evaluateAstNode); },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          return analyze(node, contextStack, analyzeAst, builtin);
      },
  };
  var doseqSpecialExpression = {
      parse: function (tokens, position, parsers) {
          var _a, _b;
          var firstToken = token.as(tokens[position], "EOF");
          var parseToken = parsers.parseToken;
          var loopBindings;
          _a = __read(parseLoopBindings(tokens, position, parsers), 2), position = _a[0], loopBindings = _a[1];
          var expression;
          _b = __read(parseToken(tokens, position), 2), position = _b[0], expression = _b[1];
          token.assert(tokens[position], "EOF", { type: "paren", value: ")" });
          var node = {
              name: "doseq",
              type: "SpecialExpression",
              loopBindings: loopBindings,
              params: [expression],
              token: firstToken.debugInfo ? firstToken : undefined,
          };
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, helpers) {
          evaluateLoop(false, node, contextStack, helpers.evaluateAstNode);
          return null;
      },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          return analyze(node, contextStack, analyzeAst, builtin);
      },
  };

  var ifLetSpecialExpression = {
      parse: function (tokens, position, _a) {
          var _b, _c;
          var parseBindings = _a.parseBindings, parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var bindings;
          _b = __read(parseBindings(tokens, position), 2), position = _b[0], bindings = _b[1];
          if (bindings.length !== 1) {
              throw new LitsError("Expected exactly one binding, got ".concat(valueToString$1(bindings.length)), firstToken.debugInfo);
          }
          var params;
          _c = __read(parseTokens(tokens, position), 2), position = _c[0], params = _c[1];
          var node = {
              type: "SpecialExpression",
              name: "if-let",
              binding: asValue(bindings[0], firstToken.debugInfo),
              params: params,
              token: firstToken.debugInfo ? firstToken : undefined,
          };
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var _b;
          var evaluateAstNode = _a.evaluateAstNode;
          var debugInfo = (_b = node.token) === null || _b === void 0 ? void 0 : _b.debugInfo;
          var locals = {};
          var bindingValue = evaluateAstNode(node.binding.value, contextStack);
          if (bindingValue) {
              locals[node.binding.name] = { value: bindingValue };
              var newContextStack = contextStack.withContext(locals);
              var thenForm = astNode.as(node.params[0], debugInfo);
              return evaluateAstNode(thenForm, newContextStack);
          }
          if (node.params.length === 2) {
              var elseForm = astNode.as(node.params[1], debugInfo);
              return evaluateAstNode(elseForm, contextStack);
          }
          return null;
      },
      validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      analyze: function (node, contextStack, _a) {
          var _b;
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          var newContext = (_b = {}, _b[node.binding.name] = { value: true }, _b);
          var bindingResult = analyzeAst(node.binding.value, contextStack, builtin);
          var paramsResult = analyzeAst(node.params, contextStack.withContext(newContext), builtin);
          return joinAnalyzeResults(bindingResult, paramsResult);
      },
  };

  var ifNotSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = __read(parseTokens(tokens, position), 2), newPosition = _b[0], params = _b[1];
          return [
              newPosition + 1,
              {
                  type: "SpecialExpression",
                  name: "if-not",
                  params: params,
                  token: firstToken.debugInfo ? firstToken : undefined,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var _b;
          var evaluateAstNode = _a.evaluateAstNode;
          var debugInfo = (_b = node.token) === null || _b === void 0 ? void 0 : _b.debugInfo;
          var _c = __read(node.params, 3), conditionNode = _c[0], trueNode = _c[1], falseNode = _c[2];
          if (!evaluateAstNode(astNode.as(conditionNode, debugInfo), contextStack)) {
              return evaluateAstNode(astNode.as(trueNode, debugInfo), contextStack);
          }
          else {
              if (node.params.length === 3) {
                  return evaluateAstNode(astNode.as(falseNode, debugInfo), contextStack);
              }
              else {
                  return null;
              }
          }
      },
      validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          return analyzeAst(node.params, contextStack, builtin);
      },
  };

  var ifSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = __read(parseTokens(tokens, position), 2), newPosition = _b[0], params = _b[1];
          return [
              newPosition + 1,
              {
                  type: "SpecialExpression",
                  name: "if",
                  params: params,
                  token: firstToken.debugInfo ? firstToken : undefined,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var _b;
          var evaluateAstNode = _a.evaluateAstNode;
          var debugInfo = (_b = node.token) === null || _b === void 0 ? void 0 : _b.debugInfo;
          var _c = __read(node.params, 3), conditionNode = _c[0], trueNode = _c[1], falseNode = _c[2];
          if (evaluateAstNode(astNode.as(conditionNode, debugInfo), contextStack)) {
              return evaluateAstNode(astNode.as(trueNode, debugInfo), contextStack);
          }
          else {
              if (node.params.length === 3) {
                  return evaluateAstNode(astNode.as(falseNode, debugInfo), contextStack);
              }
              else {
                  return null;
              }
          }
      },
      validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          return analyzeAst(node.params, contextStack, builtin);
      },
  };

  var letSpecialExpression = {
      parse: function (tokens, position, _a) {
          var _b, _c;
          var parseBindings = _a.parseBindings, parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var bindings;
          _b = __read(parseBindings(tokens, position), 2), position = _b[0], bindings = _b[1];
          var params;
          _c = __read(parseTokens(tokens, position), 2), position = _c[0], params = _c[1];
          var node = {
              type: "SpecialExpression",
              name: "let",
              params: params,
              bindings: bindings,
              token: firstToken.debugInfo ? firstToken : undefined,
          };
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var e_1, _b, e_2, _c;
          var evaluateAstNode = _a.evaluateAstNode;
          var locals = {};
          var newContextStack = contextStack.withContext(locals);
          try {
              for (var _d = __values(node.bindings), _e = _d.next(); !_e.done; _e = _d.next()) {
                  var binding = _e.value;
                  var bindingValueNode = binding.value;
                  var bindingValue = evaluateAstNode(bindingValueNode, newContextStack);
                  locals[binding.name] = { value: bindingValue };
              }
          }
          catch (e_1_1) { e_1 = { error: e_1_1 }; }
          finally {
              try {
                  if (_e && !_e.done && (_b = _d.return)) _b.call(_d);
              }
              finally { if (e_1) throw e_1.error; }
          }
          var result = null;
          try {
              for (var _f = __values(node.params), _g = _f.next(); !_g.done; _g = _f.next()) {
                  var astNode = _g.value;
                  result = evaluateAstNode(astNode, newContextStack);
              }
          }
          catch (e_2_1) { e_2 = { error: e_2_1 }; }
          finally {
              try {
                  if (_g && !_g.done && (_c = _f.return)) _c.call(_f);
              }
              finally { if (e_2) throw e_2.error; }
          }
          return result;
      },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          var newContext = node.bindings
              .map(function (binding) { return binding.name; })
              .reduce(function (context, name) {
              context[name] = { value: true };
              return context;
          }, {});
          var bindingContext = {};
          var bindingResults = node.bindings.map(function (bindingNode) {
              var valueNode = bindingNode.value;
              var bindingsResult = analyzeAst(valueNode, contextStack.withContext(bindingContext), builtin);
              bindingContext[bindingNode.name] = { value: true };
              return bindingsResult;
          });
          var paramsResult = analyzeAst(node.params, contextStack.withContext(newContext), builtin);
          return joinAnalyzeResults.apply(void 0, __spreadArray(__spreadArray([], __read(bindingResults), false), [paramsResult], false));
      },
  };

  var loopSpecialExpression = {
      parse: function (tokens, position, _a) {
          var _b, _c;
          var parseTokens = _a.parseTokens, parseBindings = _a.parseBindings;
          var firstToken = token.as(tokens[position], "EOF");
          var bindings;
          _b = __read(parseBindings(tokens, position), 2), position = _b[0], bindings = _b[1];
          var params;
          _c = __read(parseTokens(tokens, position), 2), position = _c[0], params = _c[1];
          var node = {
              type: "SpecialExpression",
              name: "loop",
              params: params,
              bindings: bindings,
              token: firstToken.debugInfo ? firstToken : undefined,
          };
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var _b;
          var evaluateAstNode = _a.evaluateAstNode;
          var debugInfo = (_b = node.token) === null || _b === void 0 ? void 0 : _b.debugInfo;
          var bindingContext = node.bindings.reduce(function (result, binding) {
              result[binding.name] = { value: evaluateAstNode(binding.value, contextStack) };
              return result;
          }, {});
          var newContextStack = contextStack.withContext(bindingContext);
          var _loop_1 = function () {
              var e_1, _c;
              var result = null;
              try {
                  try {
                      for (var _d = (e_1 = void 0, __values(node.params)), _e = _d.next(); !_e.done; _e = _d.next()) {
                          var form = _e.value;
                          result = evaluateAstNode(form, newContextStack);
                      }
                  }
                  catch (e_1_1) { e_1 = { error: e_1_1 }; }
                  finally {
                      try {
                          if (_e && !_e.done && (_c = _d.return)) _c.call(_d);
                      }
                      finally { if (e_1) throw e_1.error; }
                  }
              }
              catch (error) {
                  if (error instanceof RecurSignal) {
                      var params_1 = error.params;
                      if (params_1.length !== node.bindings.length) {
                          throw new LitsError("recur expected ".concat(node.bindings.length, " parameters, got ").concat(valueToString$1(params_1.length)), debugInfo);
                      }
                      node.bindings.forEach(function (binding, index) {
                          asValue(bindingContext[binding.name], debugInfo).value = any.as(params_1[index], debugInfo);
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
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          var newContext = node.bindings
              .map(function (binding) { return binding.name; })
              .reduce(function (context, name) {
              context[name] = { value: true };
              return context;
          }, {});
          var bindingValueNodes = node.bindings.map(function (binding) { return binding.value; });
          var bindingsResult = analyzeAst(bindingValueNodes, contextStack, builtin);
          var paramsResult = analyzeAst(node.params, contextStack.withContext(newContext), builtin);
          return joinAnalyzeResults(bindingsResult, paramsResult);
      },
  };

  var orSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = __read(parseTokens(tokens, position), 2), newPosition = _b[0], params = _b[1];
          return [
              newPosition + 1,
              {
                  type: "SpecialExpression",
                  name: "or",
                  params: params,
                  token: firstToken.debugInfo ? firstToken : undefined,
              },
          ];
      },
      evaluate: function (node, contextStack, _a) {
          var e_1, _b;
          var evaluateAstNode = _a.evaluateAstNode;
          var value = false;
          try {
              for (var _c = __values(node.params), _d = _c.next(); !_d.done; _d = _c.next()) {
                  var param = _d.value;
                  value = evaluateAstNode(param, contextStack);
                  if (value) {
                      break;
                  }
              }
          }
          catch (e_1_1) { e_1 = { error: e_1_1 }; }
          finally {
              try {
                  if (_d && !_d.done && (_b = _c.return)) _b.call(_c);
              }
              finally { if (e_1) throw e_1.error; }
          }
          return value;
      },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          return analyzeAst(node.params, contextStack, builtin);
      },
  };

  var recurSpecialExpression = {
      parse: function (tokens, position, _a) {
          var _b;
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var params;
          _b = __read(parseTokens(tokens, position), 2), position = _b[0], params = _b[1];
          var node = {
              type: "SpecialExpression",
              name: "recur",
              params: params,
              token: firstToken.debugInfo ? firstToken : undefined,
          };
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var evaluateAstNode = _a.evaluateAstNode;
          var params = node.params.map(function (paramNode) { return evaluateAstNode(paramNode, contextStack); });
          throw new RecurSignal(params);
      },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          return analyzeAst(node.params, contextStack, builtin);
      },
  };

  var throwSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseToken = _a.parseToken;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = __read(parseToken(tokens, position), 2), newPosition = _b[0], messageNode = _b[1];
          position = newPosition;
          token.assert(tokens[position], "EOF", { type: "paren", value: ")" });
          position += 1;
          var node = {
              type: "SpecialExpression",
              name: "throw",
              params: [],
              messageNode: messageNode,
              token: firstToken.debugInfo ? firstToken : undefined,
          };
          return [position, node];
      },
      evaluate: function (node, contextStack, _a) {
          var _b, _c;
          var evaluateAstNode = _a.evaluateAstNode;
          var message = string.as(evaluateAstNode(node.messageNode, contextStack), (_b = node.token) === null || _b === void 0 ? void 0 : _b.debugInfo, {
              nonEmpty: true,
          });
          throw new UserDefinedError(message, (_c = node.token) === null || _c === void 0 ? void 0 : _c.debugInfo);
      },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          return analyzeAst(node.messageNode, contextStack, builtin);
      },
  };

  var timeSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseToken = _a.parseToken;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = __read(parseToken(tokens, position), 2), newPosition = _b[0], astNode = _b[1];
          var node = {
              type: "SpecialExpression",
              name: "time!",
              params: [astNode],
              token: firstToken.debugInfo ? firstToken : undefined,
          };
          return [newPosition + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var _b;
          var evaluateAstNode = _a.evaluateAstNode;
          var _c = __read(node.params, 1), param = _c[0];
          astNode.assert(param, (_b = node.token) === null || _b === void 0 ? void 0 : _b.debugInfo);
          var startTime = Date.now();
          var result = evaluateAstNode(param, contextStack);
          var totalTime = Date.now() - startTime;
          // eslint-disable-next-line no-console
          console.log("Elapsed time: ".concat(totalTime, " ms"));
          return result;
      },
      validate: function (node) { return assertNumberOfParams(1, node); },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          return analyzeAst(node.params, contextStack, builtin);
      },
  };

  var trySpecialExpression = {
      parse: function (tokens, position, _a) {
          var _b, _c, _d, _e;
          var _f, _g, _h;
          var parseToken = _a.parseToken;
          var firstToken = token.as(tokens[position], "EOF");
          var tryExpression;
          _b = __read(parseToken(tokens, position), 2), position = _b[0], tryExpression = _b[1];
          token.assert(tokens[position], "EOF", { type: "paren", value: "(" });
          position += 1;
          var catchNode;
          _c = __read(parseToken(tokens, position), 2), position = _c[0], catchNode = _c[1];
          nameNode.assert(catchNode, (_f = catchNode.token) === null || _f === void 0 ? void 0 : _f.debugInfo);
          if (catchNode.value !== "catch") {
              throw new LitsError("Expected 'catch', got '".concat(catchNode.value, "'."), getDebugInfo(catchNode, (_g = catchNode.token) === null || _g === void 0 ? void 0 : _g.debugInfo));
          }
          var error;
          _d = __read(parseToken(tokens, position), 2), position = _d[0], error = _d[1];
          nameNode.assert(error, (_h = error.token) === null || _h === void 0 ? void 0 : _h.debugInfo);
          var catchExpression;
          _e = __read(parseToken(tokens, position), 2), position = _e[0], catchExpression = _e[1];
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
              token: firstToken.debugInfo ? firstToken : undefined,
          };
          return [position, node];
      },
      evaluate: function (node, contextStack, _a) {
          var _b;
          var _c;
          var evaluateAstNode = _a.evaluateAstNode;
          var _d = node, tryExpression = _d.tryExpression, catchExpression = _d.catchExpression, errorNode = _d.error;
          try {
              return evaluateAstNode(tryExpression, contextStack);
          }
          catch (error) {
              var newContext = (_b = {},
                  _b[errorNode.value] = { value: any.as(error, (_c = node.token) === null || _c === void 0 ? void 0 : _c.debugInfo) },
                  _b);
              return evaluateAstNode(catchExpression, contextStack.withContext(newContext));
          }
      },
      analyze: function (node, contextStack, _a) {
          var _b;
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          var _c = node, tryExpression = _c.tryExpression, catchExpression = _c.catchExpression, errorNode = _c.error;
          var tryResult = analyzeAst(tryExpression, contextStack, builtin);
          var newContext = (_b = {},
              _b[errorNode.value] = { value: true },
              _b);
          var catchResult = analyzeAst(catchExpression, contextStack.withContext(newContext), builtin);
          return joinAnalyzeResults(tryResult, catchResult);
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
      else if (regularExpression.is(value)) {
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
  function deepEqual(a, b, debugInfo) {
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
              if (!deepEqual(any.as(a[i], debugInfo), any.as(b[i], debugInfo), debugInfo)) {
                  return false;
              }
          }
          return true;
      }
      if (isRegularExpression(a) && isRegularExpression(b)) {
          return a.source === b.source && a.flags === b.flags;
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
              var key = string.as(aKeys[i], debugInfo);
              if (!deepEqual(toAny(aObj[key]), toAny(bObj[key]), debugInfo)) {
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
              var _a = __read(entry, 2), key = _a[0], val = _a[1];
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
          var _b = __read(_a, 2), key = _b[0], value = _b[1];
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
          _b = __read(parseBindings(tokens, position), 2), position = _b[0], bindings = _b[1];
          if (bindings.length !== 1) {
              throw new LitsError("Expected exactly one binding, got ".concat(valueToString$1(bindings.length)), firstToken.debugInfo);
          }
          var params;
          _c = __read(parseTokens(tokens, position), 2), position = _c[0], params = _c[1];
          var node = {
              type: "SpecialExpression",
              name: "when-first",
              binding: asValue(bindings[0], firstToken.debugInfo),
              params: params,
              token: firstToken.debugInfo ? firstToken : undefined,
          };
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var e_1, _b;
          var _c;
          var evaluateAstNode = _a.evaluateAstNode;
          var locals = {};
          var binding = node.binding;
          var evaluatedBindingForm = evaluateAstNode(binding.value, contextStack);
          if (!sequence.is(evaluatedBindingForm)) {
              throw new LitsError("Expected undefined or a sequence, got ".concat(valueToString$1(evaluatedBindingForm)), (_c = node.token) === null || _c === void 0 ? void 0 : _c.debugInfo);
          }
          if (evaluatedBindingForm.length === 0) {
              return null;
          }
          var bindingValue = toAny(evaluatedBindingForm[0]);
          locals[binding.name] = { value: bindingValue };
          var newContextStack = contextStack.withContext(locals);
          var result = null;
          try {
              for (var _d = __values(node.params), _e = _d.next(); !_e.done; _e = _d.next()) {
                  var form = _e.value;
                  result = evaluateAstNode(form, newContextStack);
              }
          }
          catch (e_1_1) { e_1 = { error: e_1_1 }; }
          finally {
              try {
                  if (_e && !_e.done && (_b = _d.return)) _b.call(_d);
              }
              finally { if (e_1) throw e_1.error; }
          }
          return result;
      },
      validate: function (node) { return assertNumberOfParams({ min: 0 }, node); },
      analyze: function (node, contextStack, _a) {
          var _b;
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          var binding = node.binding;
          var newContext = (_b = {}, _b[binding.name] = { value: true }, _b);
          var bindingResult = analyzeAst(binding.value, contextStack, builtin);
          var paramsResult = analyzeAst(node.params, contextStack.withContext(newContext), builtin);
          return joinAnalyzeResults(bindingResult, paramsResult);
      },
  };

  var whenLetSpecialExpression = {
      parse: function (tokens, position, _a) {
          var _b, _c;
          var parseBindings = _a.parseBindings, parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var bindings;
          _b = __read(parseBindings(tokens, position), 2), position = _b[0], bindings = _b[1];
          if (bindings.length !== 1) {
              throw new LitsError("Expected exactly one binding, got ".concat(valueToString$1(bindings.length)), firstToken.debugInfo);
          }
          var params;
          _c = __read(parseTokens(tokens, position), 2), position = _c[0], params = _c[1];
          var node = {
              type: "SpecialExpression",
              name: "when-let",
              binding: asValue(bindings[0], firstToken.debugInfo),
              params: params,
              token: firstToken.debugInfo ? firstToken : undefined,
          };
          return [position + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var e_1, _b;
          var evaluateAstNode = _a.evaluateAstNode;
          var binding = node.binding;
          var locals = {};
          var bindingValue = evaluateAstNode(binding.value, contextStack);
          if (!bindingValue) {
              return null;
          }
          locals[binding.name] = { value: bindingValue };
          var newContextStack = contextStack.withContext(locals);
          var result = null;
          try {
              for (var _c = __values(node.params), _d = _c.next(); !_d.done; _d = _c.next()) {
                  var form = _d.value;
                  result = evaluateAstNode(form, newContextStack);
              }
          }
          catch (e_1_1) { e_1 = { error: e_1_1 }; }
          finally {
              try {
                  if (_d && !_d.done && (_b = _c.return)) _b.call(_c);
              }
              finally { if (e_1) throw e_1.error; }
          }
          return result;
      },
      validate: function (node) { return assertNumberOfParams({ min: 0 }, node); },
      analyze: function (node, contextStack, _a) {
          var _b;
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          var binding = node.binding;
          var newContext = (_b = {}, _b[binding.name] = { value: true }, _b);
          var bindingResult = analyzeAst(binding.value, contextStack, builtin);
          var paramsResult = analyzeAst(node.params, contextStack.withContext(newContext), builtin);
          return joinAnalyzeResults(bindingResult, paramsResult);
      },
  };

  var whenNotSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = __read(parseTokens(tokens, position), 2), newPosition = _b[0], params = _b[1];
          var node = {
              type: "SpecialExpression",
              name: "when-not",
              params: params,
              token: firstToken.debugInfo ? firstToken : undefined,
          };
          return [newPosition + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var e_1, _b;
          var _c;
          var evaluateAstNode = _a.evaluateAstNode;
          var _d = __read(node.params), whenExpression = _d[0], body = _d.slice(1);
          astNode.assert(whenExpression, (_c = node.token) === null || _c === void 0 ? void 0 : _c.debugInfo);
          if (evaluateAstNode(whenExpression, contextStack)) {
              return null;
          }
          var result = null;
          try {
              for (var body_1 = __values(body), body_1_1 = body_1.next(); !body_1_1.done; body_1_1 = body_1.next()) {
                  var form = body_1_1.value;
                  result = evaluateAstNode(form, contextStack);
              }
          }
          catch (e_1_1) { e_1 = { error: e_1_1 }; }
          finally {
              try {
                  if (body_1_1 && !body_1_1.done && (_b = body_1.return)) _b.call(body_1);
              }
              finally { if (e_1) throw e_1.error; }
          }
          return result;
      },
      validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          return analyzeAst(node.params, contextStack, builtin);
      },
  };

  var whenSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = __read(parseTokens(tokens, position), 2), newPosition = _b[0], params = _b[1];
          var node = {
              type: "SpecialExpression",
              name: "when",
              params: params,
              token: firstToken.debugInfo ? firstToken : undefined,
          };
          return [newPosition + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var e_1, _b;
          var _c;
          var evaluateAstNode = _a.evaluateAstNode;
          var _d = __read(node.params), whenExpression = _d[0], body = _d.slice(1);
          astNode.assert(whenExpression, (_c = node.token) === null || _c === void 0 ? void 0 : _c.debugInfo);
          if (!evaluateAstNode(whenExpression, contextStack)) {
              return null;
          }
          var result = null;
          try {
              for (var body_1 = __values(body), body_1_1 = body_1.next(); !body_1_1.done; body_1_1 = body_1.next()) {
                  var form = body_1_1.value;
                  result = evaluateAstNode(form, contextStack);
              }
          }
          catch (e_1_1) { e_1 = { error: e_1_1 }; }
          finally {
              try {
                  if (body_1_1 && !body_1_1.done && (_b = body_1.return)) _b.call(body_1);
              }
              finally { if (e_1) throw e_1.error; }
          }
          return result;
      },
      validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          return analyzeAst(node.params, contextStack, builtin);
      },
  };

  var bitwiseNormalExpression = {
      'bit-shift-left': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), num = _b[0], count = _b[1];
              number.assert(num, debugInfo, { integer: true });
              number.assert(count, debugInfo, { integer: true, nonNegative: true });
              return num << count;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'bit-shift-right': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), num = _b[0], count = _b[1];
              number.assert(num, debugInfo, { integer: true });
              number.assert(count, debugInfo, { integer: true, nonNegative: true });
              return num >> count;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'bit-not': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), num = _b[0];
              number.assert(num, debugInfo, { integer: true });
              return ~num;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'bit-and': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a), first = _b[0], rest = _b.slice(1);
              number.assert(first, debugInfo, { integer: true });
              return rest.reduce(function (result, value) {
                  number.assert(value, debugInfo, { integer: true });
                  return result & value;
              }, first);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
      'bit-and-not': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a), first = _b[0], rest = _b.slice(1);
              number.assert(first, debugInfo, { integer: true });
              return rest.reduce(function (result, value) {
                  number.assert(value, debugInfo, { integer: true });
                  return result & ~value;
              }, first);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
      'bit-or': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a), first = _b[0], rest = _b.slice(1);
              number.assert(first, debugInfo, { integer: true });
              return rest.reduce(function (result, value) {
                  number.assert(value, debugInfo, { integer: true });
                  return result | value;
              }, first);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
      'bit-xor': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a), first = _b[0], rest = _b.slice(1);
              number.assert(first, debugInfo, { integer: true });
              return rest.reduce(function (result, value) {
                  number.assert(value, debugInfo, { integer: true });
                  return result ^ value;
              }, first);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
      'bit-flip': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), num = _b[0], index = _b[1];
              number.assert(num, debugInfo, { integer: true });
              number.assert(index, debugInfo, { integer: true, nonNegative: true });
              var mask = 1 << index;
              return (num ^= mask);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'bit-set': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), num = _b[0], index = _b[1];
              number.assert(num, debugInfo, { integer: true });
              number.assert(index, debugInfo, { integer: true, nonNegative: true });
              var mask = 1 << index;
              return (num |= mask);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'bit-clear': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), num = _b[0], index = _b[1];
              number.assert(num, debugInfo, { integer: true });
              number.assert(index, debugInfo, { integer: true, nonNegative: true });
              var mask = 1 << index;
              return (num &= ~mask);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'bit-test': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), num = _b[0], index = _b[1];
              number.assert(num, debugInfo, { integer: true });
              number.assert(index, debugInfo, { integer: true, nonNegative: true });
              var mask = 1 << index;
              return !!(num & mask);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
  };

  function cloneAndGetMeta(originalColl, keys, debugInfo) {
      var coll = cloneColl(originalColl);
      var butLastKeys = keys.slice(0, keys.length - 1);
      var innerCollMeta = butLastKeys.reduce(function (result, key) {
          var resultColl = result.coll;
          var newResultColl;
          if (array.is(resultColl)) {
              number.assert(key, debugInfo);
              // eslint-disable-next-line @typescript-eslint/no-explicit-any
              newResultColl = collection.as(resultColl[key], debugInfo);
          }
          else {
              object.assert(resultColl, debugInfo);
              string.assert(key, debugInfo);
              if (!collHasKey(result.coll, key)) {
                  resultColl[key] = {};
              }
              newResultColl = collection.as(resultColl[key], debugInfo);
          }
          return { coll: newResultColl, parent: resultColl };
      }, { coll: coll, parent: {} });
      return { coll: coll, innerCollMeta: innerCollMeta };
  }
  function get(coll, key) {
      if (object.is(coll)) {
          if (string.is(key) && collHasKey(coll, key)) {
              return toAny(coll[key]);
          }
      }
      else {
          if (number.is(key, { nonNegative: true, integer: true }) && key >= 0 && key < coll.length) {
              return toAny(coll[key]);
          }
      }
      return undefined;
  }
  function update(coll, key, fn, params, contextStack, executeFunction, debugInfo) {
      if (object.is(coll)) {
          string.assert(key, debugInfo);
          var result = __assign({}, coll);
          result[key] = executeFunction(fn, __spreadArray([result[key]], __read(params), false), contextStack, debugInfo);
          return result;
      }
      else {
          number.assert(key, debugInfo);
          var intKey_1 = toNonNegativeInteger(key);
          number.assert(intKey_1, debugInfo, { lte: coll.length });
          if (Array.isArray(coll)) {
              var result = coll.map(function (elem, index) {
                  if (intKey_1 === index) {
                      return executeFunction(fn, __spreadArray([elem], __read(params), false), contextStack, debugInfo);
                  }
                  return elem;
              });
              if (intKey_1 === coll.length) {
                  result[intKey_1] = executeFunction(fn, __spreadArray([undefined], __read(params), false), contextStack, debugInfo);
              }
              return result;
          }
          else {
              var result = coll.split("").map(function (elem, index) {
                  if (intKey_1 === index) {
                      return string.as(executeFunction(fn, __spreadArray([elem], __read(params), false), contextStack, debugInfo), debugInfo, {
                          char: true,
                      });
                  }
                  return elem;
              });
              if (intKey_1 === coll.length) {
                  result[intKey_1] = string.as(executeFunction(fn, __spreadArray([undefined], __read(params), false), contextStack, debugInfo), debugInfo, {
                      char: true,
                  });
              }
              return result.join("");
          }
      }
  }
  function assoc(coll, key, value, debugInfo) {
      collection.assert(coll, debugInfo);
      stringOrNumber.assert(key, debugInfo);
      if (Array.isArray(coll) || typeof coll === "string") {
          number.assert(key, debugInfo, { integer: true });
          number.assert(key, debugInfo, { gte: 0 });
          number.assert(key, debugInfo, { lte: coll.length });
          if (typeof coll === "string") {
              string.assert(value, debugInfo, { char: true });
              return "".concat(coll.slice(0, key)).concat(value).concat(coll.slice(key + 1));
          }
          var copy_1 = __spreadArray([], __read(coll), false);
          copy_1[key] = value;
          return copy_1;
      }
      string.assert(key, debugInfo);
      var copy = __assign({}, coll);
      copy[key] = value;
      return copy;
  }
  var collectionNormalExpression = {
      get: {
          evaluate: function (params, debugInfo) {
              var _a = __read(params, 2), coll = _a[0], key = _a[1];
              var defaultValue = toAny(params[2]);
              stringOrNumber.assert(key, debugInfo);
              if (coll === null) {
                  return defaultValue;
              }
              collection.assert(coll, debugInfo);
              var result = get(coll, key);
              return result === undefined ? defaultValue : result;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'get-in': {
          evaluate: function (params, debugInfo) {
              var e_1, _a;
              var _b;
              var coll = toAny(params[0]);
              var keys = (_b = params[1]) !== null && _b !== void 0 ? _b : []; // nil behaves as empty array
              var defaultValue = toAny(params[2]);
              array.assert(keys, debugInfo);
              try {
                  for (var keys_1 = __values(keys), keys_1_1 = keys_1.next(); !keys_1_1.done; keys_1_1 = keys_1.next()) {
                      var key = keys_1_1.value;
                      stringOrNumber.assert(key, debugInfo);
                      if (collection.is(coll)) {
                          var nextValue = get(coll, key);
                          if (nextValue !== undefined) {
                              coll = nextValue;
                          }
                          else {
                              return defaultValue;
                          }
                      }
                      else {
                          return defaultValue;
                      }
                  }
              }
              catch (e_1_1) { e_1 = { error: e_1_1 }; }
              finally {
                  try {
                      if (keys_1_1 && !keys_1_1.done && (_a = keys_1.return)) _a.call(keys_1);
                  }
                  finally { if (e_1) throw e_1.error; }
              }
              return coll;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      count: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), coll = _b[0];
              if (typeof coll === "string") {
                  return coll.length;
              }
              collection.assert(coll, debugInfo);
              if (Array.isArray(coll)) {
                  return coll.length;
              }
              return Object.keys(coll).length;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'contains?': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), coll = _b[0], key = _b[1];
              collection.assert(coll, debugInfo);
              stringOrNumber.assert(key, debugInfo);
              if (sequence.is(coll)) {
                  if (!number.is(key, { integer: true })) {
                      return false;
                  }
                  number.assert(key, debugInfo, { integer: true });
                  return key >= 0 && key < coll.length;
              }
              return !!Object.getOwnPropertyDescriptor(coll, key);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'has?': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), coll = _b[0], value = _b[1];
              collection.assert(coll, debugInfo);
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
          evaluate: function (_a, debugInfo) {
              var e_2, _b, e_3, _c, e_4, _d;
              var _e = __read(_a, 2), coll = _e[0], seq = _e[1];
              collection.assert(coll, debugInfo);
              sequence.assert(seq, debugInfo);
              if (array.is(coll)) {
                  try {
                      for (var seq_1 = __values(seq), seq_1_1 = seq_1.next(); !seq_1_1.done; seq_1_1 = seq_1.next()) {
                          var value = seq_1_1.value;
                          if (coll.includes(value)) {
                              return true;
                          }
                      }
                  }
                  catch (e_2_1) { e_2 = { error: e_2_1 }; }
                  finally {
                      try {
                          if (seq_1_1 && !seq_1_1.done && (_b = seq_1.return)) _b.call(seq_1);
                      }
                      finally { if (e_2) throw e_2.error; }
                  }
                  return false;
              }
              if (string.is(coll)) {
                  try {
                      for (var seq_2 = __values(seq), seq_2_1 = seq_2.next(); !seq_2_1.done; seq_2_1 = seq_2.next()) {
                          var value = seq_2_1.value;
                          if (string.is(value, { char: true }) ? coll.split("").includes(value) : false) {
                              return true;
                          }
                      }
                  }
                  catch (e_3_1) { e_3 = { error: e_3_1 }; }
                  finally {
                      try {
                          if (seq_2_1 && !seq_2_1.done && (_c = seq_2.return)) _c.call(seq_2);
                      }
                      finally { if (e_3) throw e_3.error; }
                  }
                  return false;
              }
              try {
                  for (var seq_3 = __values(seq), seq_3_1 = seq_3.next(); !seq_3_1.done; seq_3_1 = seq_3.next()) {
                      var value = seq_3_1.value;
                      if (Object.values(coll).includes(value)) {
                          return true;
                      }
                  }
              }
              catch (e_4_1) { e_4 = { error: e_4_1 }; }
              finally {
                  try {
                      if (seq_3_1 && !seq_3_1.done && (_d = seq_3.return)) _d.call(seq_3);
                  }
                  finally { if (e_4) throw e_4.error; }
              }
              return false;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'has-every?': {
          evaluate: function (_a, debugInfo) {
              var e_5, _b, e_6, _c, e_7, _d;
              var _e = __read(_a, 2), coll = _e[0], seq = _e[1];
              collection.assert(coll, debugInfo);
              sequence.assert(seq, debugInfo);
              if (array.is(coll)) {
                  try {
                      for (var seq_4 = __values(seq), seq_4_1 = seq_4.next(); !seq_4_1.done; seq_4_1 = seq_4.next()) {
                          var value = seq_4_1.value;
                          if (!coll.includes(value)) {
                              return false;
                          }
                      }
                  }
                  catch (e_5_1) { e_5 = { error: e_5_1 }; }
                  finally {
                      try {
                          if (seq_4_1 && !seq_4_1.done && (_b = seq_4.return)) _b.call(seq_4);
                      }
                      finally { if (e_5) throw e_5.error; }
                  }
                  return true;
              }
              if (string.is(coll)) {
                  try {
                      for (var seq_5 = __values(seq), seq_5_1 = seq_5.next(); !seq_5_1.done; seq_5_1 = seq_5.next()) {
                          var value = seq_5_1.value;
                          if (!string.is(value, { char: true }) || !coll.split("").includes(value)) {
                              return false;
                          }
                      }
                  }
                  catch (e_6_1) { e_6 = { error: e_6_1 }; }
                  finally {
                      try {
                          if (seq_5_1 && !seq_5_1.done && (_c = seq_5.return)) _c.call(seq_5);
                      }
                      finally { if (e_6) throw e_6.error; }
                  }
                  return true;
              }
              try {
                  for (var seq_6 = __values(seq), seq_6_1 = seq_6.next(); !seq_6_1.done; seq_6_1 = seq_6.next()) {
                      var value = seq_6_1.value;
                      if (!Object.values(coll).includes(value)) {
                          return false;
                      }
                  }
              }
              catch (e_7_1) { e_7 = { error: e_7_1 }; }
              finally {
                  try {
                      if (seq_6_1 && !seq_6_1.done && (_d = seq_6.return)) _d.call(seq_6);
                  }
                  finally { if (e_7) throw e_7.error; }
              }
              return true;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      assoc: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 3), coll = _b[0], key = _b[1], value = _b[2];
              collection.assert(coll, debugInfo);
              stringOrNumber.assert(key, debugInfo);
              any.assert(value, debugInfo);
              return assoc(coll, key, value, debugInfo);
          },
          validate: function (node) { return assertNumberOfParams(3, node); },
      },
      'assoc-in': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 3), originalColl = _b[0], keys = _b[1], value = _b[2];
              collection.assert(originalColl, debugInfo);
              array.assert(keys, debugInfo);
              any.assert(value, debugInfo);
              if (keys.length === 1) {
                  stringOrNumber.assert(keys[0], debugInfo);
                  return assoc(originalColl, keys[0], value, debugInfo);
              }
              var _c = cloneAndGetMeta(originalColl, keys, debugInfo), coll = _c.coll, innerCollMeta = _c.innerCollMeta;
              var lastKey = stringOrNumber.as(keys[keys.length - 1], debugInfo);
              var parentKey = stringOrNumber.as(keys[keys.length - 2], debugInfo);
              if (array.is(innerCollMeta.parent)) {
                  number.assert(parentKey, debugInfo);
                  innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value, debugInfo);
              }
              else {
                  string.assert(parentKey, debugInfo);
                  innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value, debugInfo);
              }
              return coll;
          },
          validate: function (node) { return assertNumberOfParams(3, node); },
      },
      update: {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a), coll = _c[0], key = _c[1], fn = _c[2], params = _c.slice(3);
              var executeFunction = _b.executeFunction;
              collection.assert(coll, debugInfo);
              stringOrNumber.assert(key, debugInfo);
              litsFunction.assert(fn, debugInfo);
              return update(coll, key, fn, params, contextStack, executeFunction, debugInfo);
          },
          validate: function (node) { return assertNumberOfParams({ min: 3 }, node); },
      },
      'update-in': {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a), originalColl = _c[0], keys = _c[1], fn = _c[2], params = _c.slice(3);
              var executeFunction = _b.executeFunction;
              collection.assert(originalColl, debugInfo);
              array.assert(keys, debugInfo);
              litsFunction.assert(fn, debugInfo);
              if (keys.length === 1) {
                  stringOrNumber.assert(keys[0], debugInfo);
                  return update(originalColl, keys[0], fn, params, contextStack, executeFunction, debugInfo);
              }
              var _d = cloneAndGetMeta(originalColl, keys, debugInfo), coll = _d.coll, innerCollMeta = _d.innerCollMeta;
              var lastKey = stringOrNumber.as(keys[keys.length - 1], debugInfo);
              var parentKey = stringOrNumber.as(keys[keys.length - 2], debugInfo);
              if (array.is(innerCollMeta.parent)) {
                  number.assert(parentKey, debugInfo);
                  innerCollMeta.parent[parentKey] = update(innerCollMeta.coll, lastKey, fn, params, contextStack, executeFunction, debugInfo);
              }
              else {
                  string.assert(parentKey, debugInfo);
                  innerCollMeta.parent[parentKey] = update(innerCollMeta.coll, lastKey, fn, params, contextStack, executeFunction, debugInfo);
              }
              return coll;
          },
          validate: function (node) { return assertNumberOfParams({ min: 3 }, node); },
      },
      concat: {
          evaluate: function (params, debugInfo) {
              collection.assert(params[0], debugInfo);
              if (array.is(params[0])) {
                  return params.reduce(function (result, arr) {
                      array.assert(arr, debugInfo);
                      return result.concat(arr);
                  }, []);
              }
              else if (string.is(params[0])) {
                  return params.reduce(function (result, s) {
                      string.assert(s, debugInfo);
                      return "".concat(result).concat(s);
                  }, "");
              }
              else {
                  return params.reduce(function (result, obj) {
                      object.assert(obj, debugInfo);
                      return Object.assign(result, obj);
                  }, {});
              }
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      'not-empty': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), coll = _b[0];
              collection.assert(coll, debugInfo);
              if (string.is(coll)) {
                  return coll.length > 0 ? coll : null;
              }
              if (Array.isArray(coll)) {
                  return coll.length > 0 ? coll : null;
              }
              return Object.keys(coll).length > 0 ? coll : null;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'every?': {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a, 2), fn = _c[0], coll = _c[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, debugInfo);
              collection.assert(coll, debugInfo);
              if (Array.isArray(coll)) {
                  return coll.every(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); });
              }
              if (string.is(coll)) {
                  return coll.split("").every(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); });
              }
              return Object.entries(coll).every(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); });
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'any?': {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a, 2), fn = _c[0], coll = _c[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, debugInfo);
              collection.assert(coll, debugInfo);
              if (Array.isArray(coll)) {
                  return coll.some(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); });
              }
              if (string.is(coll)) {
                  return coll.split("").some(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); });
              }
              return Object.entries(coll).some(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); });
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'not-any?': {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a, 2), fn = _c[0], coll = _c[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, debugInfo);
              collection.assert(coll, debugInfo);
              if (Array.isArray(coll)) {
                  return !coll.some(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); });
              }
              if (string.is(coll)) {
                  return !coll.split("").some(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); });
              }
              return !Object.entries(coll).some(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); });
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'not-every?': {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a, 2), fn = _c[0], coll = _c[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, debugInfo);
              collection.assert(coll, debugInfo);
              if (Array.isArray(coll)) {
                  return !coll.every(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); });
              }
              if (string.is(coll)) {
                  return !coll.split("").every(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); });
              }
              return !Object.entries(coll).every(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); });
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
  };

  var evaluateMap = function (params, debugInfo, contextStack, _a) {
      var executeFunction = _a.executeFunction;
      var _b = __read(params, 2), fn = _b[0], firstList = _b[1];
      litsFunction.assert(fn, debugInfo);
      sequence.assert(firstList, debugInfo);
      var isStringSeq = string.is(firstList);
      var length = firstList.length;
      if (params.length === 2) {
          if (array.is(firstList)) {
              return firstList.map(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); });
          }
          else {
              return firstList
                  .split("")
                  .map(function (elem) {
                  var newVal = executeFunction(fn, [elem], contextStack, debugInfo);
                  string.assert(newVal, debugInfo, { char: true });
                  return newVal;
              })
                  .join("");
          }
      }
      else {
          params.slice(2).forEach(function (collParam) {
              if (isStringSeq) {
                  string.assert(collParam, debugInfo);
              }
              else {
                  array.assert(collParam, debugInfo);
              }
              if (length !== collParam.length) {
                  throw new LitsError("All arguments to \"map\" must have the same length.", debugInfo);
              }
          });
          if (isStringSeq) {
              var result = "";
              var _loop_1 = function (i) {
                  var fnParams = params.slice(1).map(function (l) { return l[i]; });
                  var newValue = executeFunction(fn, fnParams, contextStack, debugInfo);
                  string.assert(newValue, debugInfo, { char: true });
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
                  result.push(executeFunction(fn, fnParams, contextStack, debugInfo));
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
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), elem = _b[0], seq = _b[1];
              any.assert(elem, debugInfo);
              sequence.assert(seq, debugInfo);
              if (Array.isArray(seq)) {
                  return __spreadArray([elem], __read(seq), false);
              }
              string.assert(elem, debugInfo, { char: true });
              return "".concat(elem).concat(seq);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      nth: {
          evaluate: function (params, debugInfo) {
              var _a = __read(params, 2), seq = _a[0], i = _a[1];
              var defaultValue = toAny(params[2]);
              number.assert(i, debugInfo, { integer: true });
              if (seq === null) {
                  return defaultValue;
              }
              sequence.assert(seq, debugInfo);
              return i >= 0 && i < seq.length ? toAny(seq[i]) : defaultValue;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      filter: {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a, 2), fn = _c[0], seq = _c[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, debugInfo);
              sequence.assert(seq, debugInfo);
              if (Array.isArray(seq)) {
                  return seq.filter(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); });
              }
              return seq
                  .split("")
                  .filter(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); })
                  .join("");
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      first: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), array = _b[0];
              sequence.assert(array, debugInfo);
              return toAny(array[0]);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      last: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              sequence.assert(first, debugInfo);
              return toAny(first[first.length - 1]);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      map: {
          evaluate: evaluateMap,
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
      pop: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), seq = _b[0];
              sequence.assert(seq, debugInfo);
              if (string.is(seq)) {
                  return seq.substr(0, seq.length - 1);
              }
              var copy = __spreadArray([], __read(seq), false);
              copy.pop();
              return copy;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      position: {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a, 2), fn = _c[0], seq = _c[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, debugInfo);
              sequence.assert(seq, debugInfo);
              if (string.is(seq)) {
                  var index = seq.split("").findIndex(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); });
                  return index !== -1 ? index : null;
              }
              else {
                  var index = seq.findIndex(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); });
                  return index !== -1 ? index : null;
              }
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'index-of': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), seq = _b[0], value = _b[1];
              any.assert(value, debugInfo);
              sequence.assert(seq, debugInfo);
              if (string.is(seq)) {
                  string.assert(value, debugInfo);
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
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a), seq = _b[0], values = _b.slice(1);
              sequence.assert(seq, debugInfo);
              if (string.is(seq)) {
                  charArray.assert(values, debugInfo);
                  return __spreadArray([seq], __read(values), false).join("");
              }
              else {
                  return __spreadArray(__spreadArray([], __read(seq), false), __read(values), false);
              }
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
      reductions: {
          evaluate: function (params, debugInfo, contextStack, _a) {
              var executeFunction = _a.executeFunction;
              var fn = params[0];
              litsFunction.assert(fn, debugInfo);
              if (params.length === 2) {
                  var _b = __read(params, 2), arr = _b[1];
                  sequence.assert(arr, debugInfo);
                  if (arr.length === 0) {
                      return [executeFunction(fn, [], contextStack, debugInfo)];
                  }
                  else if (arr.length === 1) {
                      return [toAny(arr[0])];
                  }
                  if (string.is(arr)) {
                      var chars = arr.split("");
                      var resultArray_1 = [any.as(chars[0], debugInfo)];
                      chars.slice(1).reduce(function (result, elem) {
                          var newVal = executeFunction(fn, [result, elem], contextStack, debugInfo);
                          resultArray_1.push(newVal);
                          return newVal;
                      }, any.as(chars[0], debugInfo));
                      return resultArray_1;
                  }
                  else {
                      var resultArray_2 = [toAny(arr[0])];
                      arr.slice(1).reduce(function (result, elem) {
                          var newVal = executeFunction(fn, [result, elem], contextStack, debugInfo);
                          resultArray_2.push(newVal);
                          return newVal;
                      }, toAny(arr[0]));
                      return resultArray_2;
                  }
              }
              else {
                  var _c = __read(params, 3), val = _c[1], seq = _c[2];
                  any.assert(val, debugInfo);
                  sequence.assert(seq, debugInfo);
                  if (string.is(seq)) {
                      string.assert(val, debugInfo);
                      if (seq.length === 0) {
                          return [val];
                      }
                      var resultArray_3 = [val];
                      seq.split("").reduce(function (result, elem) {
                          var newVal = executeFunction(fn, [result, elem], contextStack, debugInfo);
                          resultArray_3.push(newVal);
                          return newVal;
                      }, val);
                      return resultArray_3;
                  }
                  else {
                      if (seq.length === 0) {
                          return [val];
                      }
                      var resultArray_4 = [val];
                      seq.reduce(function (result, elem) {
                          var newVal = executeFunction(fn, [result, elem], contextStack, debugInfo);
                          resultArray_4.push(newVal);
                          return newVal;
                      }, val);
                      return resultArray_4;
                  }
              }
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      reduce: {
          evaluate: function (params, debugInfo, contextStack, _a) {
              var executeFunction = _a.executeFunction;
              var fn = params[0];
              litsFunction.assert(fn, debugInfo);
              if (params.length === 2) {
                  var _b = __read(params, 2), arr = _b[1];
                  sequence.assert(arr, debugInfo);
                  if (arr.length === 0) {
                      return executeFunction(fn, [], contextStack, debugInfo);
                  }
                  else if (arr.length === 1) {
                      return toAny(arr[0]);
                  }
                  if (string.is(arr)) {
                      var chars = arr.split("");
                      return chars.slice(1).reduce(function (result, elem) {
                          var val = executeFunction(fn, [result, elem], contextStack, debugInfo);
                          return val;
                      }, any.as(chars[0], debugInfo));
                  }
                  else {
                      return arr.slice(1).reduce(function (result, elem) {
                          return executeFunction(fn, [result, elem], contextStack, debugInfo);
                      }, toAny(arr[0]));
                  }
              }
              else {
                  var _c = __read(params, 3), val = _c[1], seq = _c[2];
                  any.assert(val, debugInfo);
                  sequence.assert(seq, debugInfo);
                  if (string.is(seq)) {
                      string.assert(val, debugInfo);
                      if (seq.length === 0) {
                          return val;
                      }
                      return seq.split("").reduce(function (result, elem) {
                          var newVal = executeFunction(fn, [result, elem], contextStack, debugInfo);
                          return newVal;
                      }, val);
                  }
                  else {
                      if (seq.length === 0) {
                          return val;
                      }
                      return seq.reduce(function (result, elem) {
                          return executeFunction(fn, [result, elem], contextStack, debugInfo);
                      }, val);
                  }
              }
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'reduce-right': {
          evaluate: function (params, debugInfo, contextStack, _a) {
              var executeFunction = _a.executeFunction;
              var fn = params[0];
              litsFunction.assert(fn, debugInfo);
              if (params.length === 2) {
                  var _b = __read(params, 2), seq = _b[1];
                  sequence.assert(seq, debugInfo);
                  if (seq.length === 0) {
                      return executeFunction(fn, [], contextStack, debugInfo);
                  }
                  else if (seq.length === 1) {
                      return toAny(seq[0]);
                  }
                  if (string.is(seq)) {
                      var chars = seq.split("");
                      return chars.slice(0, chars.length - 1).reduceRight(function (result, elem) {
                          var newVal = executeFunction(fn, [result, elem], contextStack, debugInfo);
                          string.assert(newVal, debugInfo);
                          return newVal;
                      }, chars[chars.length - 1]);
                  }
                  else {
                      return seq.slice(0, seq.length - 1).reduceRight(function (result, elem) {
                          return executeFunction(fn, [result, elem], contextStack, debugInfo);
                      }, any.as(seq[seq.length - 1], debugInfo));
                  }
              }
              else {
                  var _c = __read(params, 3), val = _c[1], seq = _c[2];
                  any.assert(val, debugInfo);
                  sequence.assert(seq, debugInfo);
                  if (string.is(seq)) {
                      if (seq.length === 0) {
                          return val;
                      }
                      return seq.split("").reduceRight(function (result, elem) {
                          var newVal = executeFunction(fn, [result, elem], contextStack, debugInfo);
                          return newVal;
                      }, val);
                  }
                  else {
                      if (seq.length === 0) {
                          return val;
                      }
                      return seq.reduceRight(function (result, elem) {
                          return executeFunction(fn, [result, elem], contextStack, debugInfo);
                      }, val);
                  }
              }
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      rest: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              sequence.assert(first, debugInfo);
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
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), seq = _b[0], count = _b[1];
              sequence.assert(seq, debugInfo);
              number.assert(count, debugInfo, { finite: true });
              var integerCount = Math.max(Math.ceil(count), 0);
              if (Array.isArray(seq)) {
                  return seq.slice(integerCount);
              }
              return seq.substr(integerCount);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      next: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              sequence.assert(first, debugInfo);
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
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), seq = _b[0], count = _b[1];
              sequence.assert(seq, debugInfo);
              number.assert(count, debugInfo, { finite: true });
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
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              sequence.assert(first, debugInfo);
              if (Array.isArray(first)) {
                  return __spreadArray([], __read(first), false).reverse();
              }
              return first.split("").reverse().join("");
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      second: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), array = _b[0];
              sequence.assert(array, debugInfo);
              return toAny(array[1]);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      shift: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), seq = _b[0];
              sequence.assert(seq, debugInfo);
              if (string.is(seq)) {
                  return seq.substr(1);
              }
              var copy = __spreadArray([], __read(seq), false);
              copy.shift();
              return copy;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      slice: {
          evaluate: function (params, debugInfo) {
              var _a = __read(params, 3), seq = _a[0], from = _a[1], to = _a[2];
              sequence.assert(seq, debugInfo);
              if (params.length === 1) {
                  return seq;
              }
              number.assert(from, debugInfo, { integer: true });
              if (params.length === 2) {
                  return seq.slice(from);
              }
              number.assert(to, debugInfo, { integer: true });
              return seq.slice(from, to);
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 3 }, node); },
      },
      some: {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c;
              var _d = __read(_a, 2), fn = _d[0], seq = _d[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, debugInfo);
              sequence.assert(seq, debugInfo);
              if (seq.length === 0) {
                  return null;
              }
              if (string.is(seq)) {
                  return (_c = seq.split("").find(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); })) !== null && _c !== void 0 ? _c : null;
              }
              return toAny(seq.find(function (elem) { return executeFunction(fn, [elem], contextStack, debugInfo); }));
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      sort: {
          evaluate: function (params, debugInfo, contextStack, _a) {
              var executeFunction = _a.executeFunction;
              var defaultComparer = params.length === 1;
              var seq = defaultComparer ? params[0] : params[1];
              var comparer = defaultComparer ? null : params[0];
              sequence.assert(seq, debugInfo);
              if (string.is(seq)) {
                  var result_1 = seq.split("");
                  if (defaultComparer) {
                      result_1.sort(compare);
                  }
                  else {
                      litsFunction.assert(comparer, debugInfo);
                      result_1.sort(function (a, b) {
                          var compareValue = executeFunction(comparer, [a, b], contextStack, debugInfo);
                          number.assert(compareValue, debugInfo, { finite: true });
                          return compareValue;
                      });
                  }
                  return result_1.join("");
              }
              var result = __spreadArray([], __read(seq), false);
              if (defaultComparer) {
                  result.sort(compare);
              }
              else {
                  result.sort(function (a, b) {
                      litsFunction.assert(comparer, debugInfo);
                      var compareValue = executeFunction(comparer, [a, b], contextStack, debugInfo);
                      number.assert(compareValue, debugInfo, { finite: true });
                      return compareValue;
                  });
              }
              return result;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
      'sort-by': {
          evaluate: function (params, debugInfo, contextStack, _a) {
              var executeFunction = _a.executeFunction;
              var defaultComparer = params.length === 2;
              var keyfn = any.as(params[0], debugInfo);
              var comparer = defaultComparer ? null : params[1];
              var seq = sequence.as(defaultComparer ? params[1] : params[2], debugInfo);
              if (string.is(seq)) {
                  var result_2 = seq.split("");
                  if (defaultComparer) {
                      result_2.sort(function (a, b) {
                          var aKey = executeFunction(keyfn, [a], contextStack, debugInfo);
                          var bKey = executeFunction(keyfn, [b], contextStack, debugInfo);
                          return compare(aKey, bKey);
                      });
                  }
                  else {
                      litsFunction.assert(comparer, debugInfo);
                      result_2.sort(function (a, b) {
                          var aKey = executeFunction(keyfn, [a], contextStack, debugInfo);
                          var bKey = executeFunction(keyfn, [b], contextStack, debugInfo);
                          var compareValue = executeFunction(comparer, [aKey, bKey], contextStack, debugInfo);
                          number.assert(compareValue, debugInfo, { finite: true });
                          return compareValue;
                      });
                  }
                  return result_2.join("");
              }
              var result = __spreadArray([], __read(seq), false);
              if (defaultComparer) {
                  result.sort(function (a, b) {
                      var aKey = executeFunction(keyfn, [a], contextStack, debugInfo);
                      var bKey = executeFunction(keyfn, [b], contextStack, debugInfo);
                      return compare(aKey, bKey);
                  });
              }
              else {
                  litsFunction.assert(comparer, debugInfo);
                  result.sort(function (a, b) {
                      var aKey = executeFunction(keyfn, [a], contextStack, debugInfo);
                      var bKey = executeFunction(keyfn, [b], contextStack, debugInfo);
                      var compareValue = executeFunction(comparer, [aKey, bKey], contextStack, debugInfo);
                      number.assert(compareValue, debugInfo, { finite: true });
                      return compareValue;
                  });
              }
              return result;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      take: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), n = _b[0], input = _b[1];
              number.assert(n, debugInfo);
              sequence.assert(input, debugInfo);
              var num = Math.max(Math.ceil(n), 0);
              return input.slice(0, num);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'take-last': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), n = _b[0], array = _b[1];
              sequence.assert(array, debugInfo);
              number.assert(n, debugInfo);
              var num = Math.max(Math.ceil(n), 0);
              var from = array.length - num;
              return array.slice(from);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'take-while': {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var e_1, _c;
              var _d = __read(_a, 2), fn = _d[0], seq = _d[1];
              var executeFunction = _b.executeFunction;
              sequence.assert(seq, debugInfo);
              litsFunction.assert(fn, debugInfo);
              var result = [];
              try {
                  for (var seq_1 = __values(seq), seq_1_1 = seq_1.next(); !seq_1_1.done; seq_1_1 = seq_1.next()) {
                      var item = seq_1_1.value;
                      if (executeFunction(fn, [item], contextStack, debugInfo)) {
                          result.push(item);
                      }
                      else {
                          break;
                      }
                  }
              }
              catch (e_1_1) { e_1 = { error: e_1_1 }; }
              finally {
                  try {
                      if (seq_1_1 && !seq_1_1.done && (_c = seq_1.return)) _c.call(seq_1);
                  }
                  finally { if (e_1) throw e_1.error; }
              }
              return string.is(seq) ? result.join("") : result;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      drop: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), n = _b[0], input = _b[1];
              number.assert(n, debugInfo);
              var num = Math.max(Math.ceil(n), 0);
              sequence.assert(input, debugInfo);
              return input.slice(num);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'drop-last': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), n = _b[0], array = _b[1];
              sequence.assert(array, debugInfo);
              number.assert(n, debugInfo);
              var num = Math.max(Math.ceil(n), 0);
              var from = array.length - num;
              return array.slice(0, from);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'drop-while': {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a, 2), fn = _c[0], seq = _c[1];
              var executeFunction = _b.executeFunction;
              sequence.assert(seq, debugInfo);
              litsFunction.assert(fn, debugInfo);
              if (Array.isArray(seq)) {
                  var from_1 = seq.findIndex(function (elem) { return !executeFunction(fn, [elem], contextStack, debugInfo); });
                  return seq.slice(from_1);
              }
              var charArray = seq.split("");
              var from = charArray.findIndex(function (elem) { return !executeFunction(fn, [elem], contextStack, debugInfo); });
              return charArray.slice(from).join("");
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      unshift: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a), seq = _b[0], values = _b.slice(1);
              sequence.assert(seq, debugInfo);
              if (string.is(seq)) {
                  charArray.assert(values, debugInfo);
                  return __spreadArray(__spreadArray([], __read(values), false), [seq], false).join("");
              }
              var copy = __spreadArray([], __read(seq), false);
              copy.unshift.apply(copy, __spreadArray([], __read(values), false));
              return copy;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
      'random-sample!': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), prob = _b[0], seq = _b[1];
              number.assert(prob, debugInfo, { finite: true });
              sequence.assert(seq, debugInfo);
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
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), seq = _b[0];
              sequence.assert(seq, debugInfo);
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
      'shuffle!': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), input = _b[0];
              sequence.assert(input, debugInfo);
              var array = string.is(input) ? __spreadArray([], __read(input.split("")), false) : __spreadArray([], __read(input), false);
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
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), input = _b[0];
              sequence.assert(input, debugInfo);
              if (Array.isArray(input)) {
                  return Array.from(new Set(input));
              }
              return Array.from(new Set(input.split(""))).join("");
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      remove: {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a, 2), fn = _c[0], input = _c[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, debugInfo);
              sequence.assert(input, debugInfo);
              if (Array.isArray(input)) {
                  return input.filter(function (elem) { return !executeFunction(fn, [elem], contextStack, debugInfo); });
              }
              return input
                  .split("")
                  .filter(function (elem) { return !executeFunction(fn, [elem], contextStack, debugInfo); })
                  .join("");
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'remove-at': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), index = _b[0], input = _b[1];
              number.assert(index, debugInfo);
              sequence.assert(input, debugInfo);
              var intIndex = Math.ceil(index);
              if (intIndex < 0 || intIndex >= input.length) {
                  return input;
              }
              if (Array.isArray(input)) {
                  var copy = __spreadArray([], __read(input), false);
                  copy.splice(index, 1);
                  return copy;
              }
              return "".concat(input.substring(0, index)).concat(input.substring(index + 1));
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'split-at': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), pos = _b[0], seq = _b[1];
              number.assert(pos, debugInfo, { finite: true });
              var intPos = toNonNegativeInteger(pos);
              sequence.assert(seq, debugInfo);
              return [seq.slice(0, intPos), seq.slice(intPos)];
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'split-with': {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a, 2), fn = _c[0], seq = _c[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, debugInfo);
              sequence.assert(seq, debugInfo);
              var seqIsArray = Array.isArray(seq);
              var arr = seqIsArray ? seq : seq.split("");
              var index = arr.findIndex(function (elem) { return !executeFunction(fn, [elem], contextStack, debugInfo); });
              if (index === -1) {
                  return [seq, seqIsArray ? [] : ""];
              }
              return [seq.slice(0, index), seq.slice(index)];
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      frequencies: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), seq = _b[0];
              sequence.assert(seq, debugInfo);
              var arr = string.is(seq) ? seq.split("") : seq;
              return arr.reduce(function (result, val) {
                  string.assert(val, debugInfo);
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
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a, 2), fn = _c[0], seq = _c[1];
              var executeFunction = _b.executeFunction;
              any.assert(fn, debugInfo);
              sequence.assert(seq, debugInfo);
              var arr = Array.isArray(seq) ? seq : seq.split("");
              return arr.reduce(function (result, val) {
                  var key = executeFunction(fn, [val], contextStack, debugInfo);
                  string.assert(key, debugInfo);
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
          evaluate: function (params, debugInfo) {
              var len = params.length;
              var n = toNonNegativeInteger(number.as(params[0], debugInfo));
              var seq = len === 2
                  ? sequence.as(params[1], debugInfo)
                  : len === 3
                      ? sequence.as(params[2], debugInfo)
                      : sequence.as(params[3], debugInfo);
              var step = len >= 3 ? toNonNegativeInteger(number.as(params[1], debugInfo)) : n;
              var pad = len === 4 ? (params[2] === null ? [] : array.as(params[2], debugInfo)) : undefined;
              return partition(n, step, seq, pad, debugInfo);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 4 }, node); },
      },
      'partition-all': {
          evaluate: function (params, debugInfo) {
              var len = params.length;
              var n = toNonNegativeInteger(number.as(params[0], debugInfo));
              var seq = len === 2 ? sequence.as(params[1], debugInfo) : sequence.as(params[2], debugInfo);
              var step = len >= 3 ? toNonNegativeInteger(number.as(params[1], debugInfo)) : n;
              return partition(n, step, seq, [], debugInfo);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'partition-by': {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a, 2), fn = _c[0], seq = _c[1];
              var executeFunction = _b.executeFunction;
              litsFunction.assert(fn, debugInfo);
              sequence.assert(seq, debugInfo);
              var isStringSeq = string.is(seq);
              var oldValue = undefined;
              var result = (isStringSeq ? seq.split("") : seq).reduce(function (result, elem) {
                  var value = executeFunction(fn, [elem], contextStack, debugInfo);
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
  function partition(n, step, seq, pad, debugInfo) {
      number.assert(step, debugInfo, { positive: true });
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
          evaluate: function (params, debugInfo) {
              var _a = __read(params, 3), first = _a[0], second = _a[1], third = _a[2];
              var from;
              var to;
              var step;
              number.assert(first, debugInfo, { finite: true });
              if (params.length === 1) {
                  from = 0;
                  to = first;
                  step = to >= 0 ? 1 : -1;
              }
              else if (params.length === 2) {
                  number.assert(second, debugInfo, { finite: true });
                  from = first;
                  to = second;
                  step = to >= from ? 1 : -1;
              }
              else {
                  number.assert(second, debugInfo, { finite: true });
                  number.assert(third, debugInfo, { finite: true });
                  from = first;
                  to = second;
                  step = third;
                  if (to > from) {
                      number.assert(step, debugInfo, { positive: true });
                  }
                  else if (to < from) {
                      number.assert(step, debugInfo, { negative: true });
                  }
                  else {
                      number.assert(step, debugInfo, { nonZero: true });
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
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), count = _b[0], value = _b[1];
              number.assert(count, debugInfo, { integer: true, nonNegative: true });
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
              var _b = __read(_a, 1), seq = _b[0];
              if (!array.is(seq)) {
                  return [];
              }
              return seq.flat(Number.POSITIVE_INFINITY);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      mapcat: {
          evaluate: function (params, debugInfo, contextStack, helpers) {
              params.slice(1).forEach(function (arr) {
                  array.assert(arr, debugInfo);
              });
              var mapResult = evaluateMap(params, debugInfo, contextStack, helpers);
              array.assert(mapResult, debugInfo);
              return mapResult.flat(1);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
  };

  var mathNormalExpression = {
      inc: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              number.assert(first, debugInfo);
              return first + 1;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      dec: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              number.assert(first, debugInfo);
              return first - 1;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      '+': {
          evaluate: function (params, debugInfo) {
              return params.reduce(function (result, param) {
                  number.assert(param, debugInfo);
                  return result + param;
              }, 0);
          },
      },
      '*': {
          evaluate: function (params, debugInfo) {
              return params.reduce(function (result, param) {
                  number.assert(param, debugInfo);
                  return result * param;
              }, 1);
          },
      },
      '/': {
          evaluate: function (params, debugInfo) {
              if (params.length === 0) {
                  return 1;
              }
              var _a = __read(params), first = _a[0], rest = _a.slice(1);
              number.assert(first, debugInfo);
              if (rest.length === 0) {
                  number.assert(first, debugInfo);
                  return 1 / first;
              }
              return rest.reduce(function (result, param) {
                  number.assert(param, debugInfo);
                  return result / param;
              }, first);
          },
      },
      '-': {
          evaluate: function (params, debugInfo) {
              if (params.length === 0) {
                  return 0;
              }
              var _a = __read(params), first = _a[0], rest = _a.slice(1);
              number.assert(first, debugInfo);
              if (rest.length === 0) {
                  return -first;
              }
              return rest.reduce(function (result, param) {
                  number.assert(param, debugInfo);
                  return result - param;
              }, first);
          },
      },
      quot: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), dividend = _b[0], divisor = _b[1];
              number.assert(dividend, debugInfo);
              number.assert(divisor, debugInfo);
              var quotient = Math.trunc(dividend / divisor);
              return quotient;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      mod: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), dividend = _b[0], divisor = _b[1];
              number.assert(dividend, debugInfo);
              number.assert(divisor, debugInfo);
              var quotient = Math.floor(dividend / divisor);
              return dividend - divisor * quotient;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      rem: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), dividend = _b[0], divisor = _b[1];
              number.assert(dividend, debugInfo);
              number.assert(divisor, debugInfo);
              var quotient = Math.trunc(dividend / divisor);
              return dividend - divisor * quotient;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      sqrt: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              number.assert(first, debugInfo);
              return Math.sqrt(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      cbrt: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              number.assert(first, debugInfo);
              return Math.cbrt(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      pow: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), first = _b[0], second = _b[1];
              number.assert(first, debugInfo);
              number.assert(second, debugInfo);
              return Math.pow(first, second);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      round: {
          evaluate: function (params, debugInfo) {
              var _a = __read(params, 2), value = _a[0], decimals = _a[1];
              number.assert(value, debugInfo);
              if (params.length === 1 || decimals === 0) {
                  return Math.round(value);
              }
              number.assert(decimals, debugInfo, { integer: true, nonNegative: true });
              var factor = Math.pow(10, decimals);
              return Math.round(value * factor) / factor;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
      trunc: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              number.assert(first, debugInfo);
              return Math.trunc(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      floor: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              number.assert(first, debugInfo);
              return Math.floor(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      ceil: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              number.assert(first, debugInfo);
              return Math.ceil(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'rand!': {
          evaluate: function (parameters, debugInfo) {
              var num = number.as(parameters.length === 1 ? parameters[0] : 1, debugInfo);
              return Math.random() * num;
          },
          validate: function (node) { return assertNumberOfParams({ min: 0, max: 1 }, node); },
      },
      'rand-int!': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              number.assert(first, debugInfo);
              return Math.floor(Math.random() * Math.abs(first)) * Math.sign(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      min: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a), first = _b[0], rest = _b.slice(1);
              number.assert(first, debugInfo);
              if (rest.length === 0) {
                  return first;
              }
              return rest.reduce(function (min, value) {
                  number.assert(value, debugInfo);
                  return Math.min(min, value);
              }, first);
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      max: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a), first = _b[0], rest = _b.slice(1);
              number.assert(first, debugInfo);
              if (rest.length === 0) {
                  return first;
              }
              return rest.reduce(function (min, value) {
                  number.assert(value, debugInfo);
                  return Math.max(min, value);
              }, first);
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      abs: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.abs(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      sign: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
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
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.exp(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      log: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.log(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      log2: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.log2(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      log10: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.log10(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      sin: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.sin(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      asin: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.asin(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      sinh: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.sinh(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      asinh: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.asinh(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      cos: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.cos(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      acos: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.acos(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      cosh: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.cosh(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      acosh: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.acosh(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      tan: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.tan(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      atan: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.atan(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      tanh: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.tanh(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      atanh: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Math.atanh(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
  };

  var version = "1.0.0";

  var uuidTemplate = "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx";
  var xyRegexp = /[xy]/g;
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
              var e_1, _b;
              var _c = __read(_a), first = _c[0], rest = _c.slice(1);
              try {
                  for (var rest_1 = __values(rest), rest_1_1 = rest_1.next(); !rest_1_1.done; rest_1_1 = rest_1.next()) {
                      var param = rest_1_1.value;
                      if (param !== first) {
                          return false;
                      }
                  }
              }
              catch (e_1_1) { e_1 = { error: e_1_1 }; }
              finally {
                  try {
                      if (rest_1_1 && !rest_1_1.done && (_b = rest_1.return)) _b.call(rest_1);
                  }
                  finally { if (e_1) throw e_1.error; }
              }
              return true;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      'equal?': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), a = _b[0], b = _b[1];
              return deepEqual(any.as(a, debugInfo), any.as(b, debugInfo), debugInfo);
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      '>': {
          evaluate: function (_a) {
              var e_2, _b;
              var _c = __read(_a), first = _c[0], rest = _c.slice(1);
              var currentValue = first;
              try {
                  for (var rest_2 = __values(rest), rest_2_1 = rest_2.next(); !rest_2_1.done; rest_2_1 = rest_2.next()) {
                      var param = rest_2_1.value;
                      if (compare(currentValue, param) <= 0) {
                          return false;
                      }
                      currentValue = param;
                  }
              }
              catch (e_2_1) { e_2 = { error: e_2_1 }; }
              finally {
                  try {
                      if (rest_2_1 && !rest_2_1.done && (_b = rest_2.return)) _b.call(rest_2);
                  }
                  finally { if (e_2) throw e_2.error; }
              }
              return true;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      '<': {
          evaluate: function (_a) {
              var e_3, _b;
              var _c = __read(_a), first = _c[0], rest = _c.slice(1);
              var currentValue = first;
              try {
                  for (var rest_3 = __values(rest), rest_3_1 = rest_3.next(); !rest_3_1.done; rest_3_1 = rest_3.next()) {
                      var param = rest_3_1.value;
                      if (compare(currentValue, param) >= 0) {
                          return false;
                      }
                      currentValue = param;
                  }
              }
              catch (e_3_1) { e_3 = { error: e_3_1 }; }
              finally {
                  try {
                      if (rest_3_1 && !rest_3_1.done && (_b = rest_3.return)) _b.call(rest_3);
                  }
                  finally { if (e_3) throw e_3.error; }
              }
              return true;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      '>=': {
          evaluate: function (_a) {
              var e_4, _b;
              var _c = __read(_a), first = _c[0], rest = _c.slice(1);
              var currentValue = first;
              try {
                  for (var rest_4 = __values(rest), rest_4_1 = rest_4.next(); !rest_4_1.done; rest_4_1 = rest_4.next()) {
                      var param = rest_4_1.value;
                      if (compare(currentValue, param) < 0) {
                          return false;
                      }
                      currentValue = param;
                  }
              }
              catch (e_4_1) { e_4 = { error: e_4_1 }; }
              finally {
                  try {
                      if (rest_4_1 && !rest_4_1.done && (_b = rest_4.return)) _b.call(rest_4);
                  }
                  finally { if (e_4) throw e_4.error; }
              }
              return true;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      '<=': {
          evaluate: function (_a) {
              var e_5, _b;
              var _c = __read(_a), first = _c[0], rest = _c.slice(1);
              var currentValue = first;
              try {
                  for (var rest_5 = __values(rest), rest_5_1 = rest_5.next(); !rest_5_1.done; rest_5_1 = rest_5.next()) {
                      var param = rest_5_1.value;
                      if (compare(currentValue, param) > 0) {
                          return false;
                      }
                      currentValue = param;
                  }
              }
              catch (e_5_1) { e_5 = { error: e_5_1 }; }
              finally {
                  try {
                      if (rest_5_1 && !rest_5_1.done && (_b = rest_5.return)) _b.call(rest_5);
                  }
                  finally { if (e_5) throw e_5.error; }
              }
              return true;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      not: {
          evaluate: function (_a) {
              var _b = __read(_a, 1), first = _b[0];
              return !first;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'inst-ms!': {
          evaluate: function () {
              return Date.now();
          },
          validate: function (node) { return assertNumberOfParams(0, node); },
      },
      'inst-ms->iso-date-time': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), ms = _b[0];
              number.assert(ms, debugInfo);
              return new Date(ms).toISOString();
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'iso-date-time->inst-ms': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), dateTime = _b[0];
              string.assert(dateTime, debugInfo);
              var ms = new Date(dateTime).valueOf();
              number.assert(ms, debugInfo, { finite: true });
              return ms;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'write!': {
          evaluate: function (params, debugInfo) {
              // eslint-disable-next-line no-console
              console.log.apply(console, __spreadArray([], __read(params), false));
              if (params.length > 0) {
                  return any.as(params[params.length - 1], debugInfo);
              }
              return null;
          },
      },
      'debug!': {
          evaluate: function (params, debugInfo, contextStack) {
              if (params.length === 0) {
                  // eslint-disable-next-line no-console
                  console.warn("*** LITS DEBUG ***\n".concat(contextStackToString(contextStack), "\n"));
                  return null;
              }
              // eslint-disable-next-line no-console
              console.warn("*** LITS DEBUG ***\n".concat(JSON.stringify(params[0], null, 2), "\n"));
              return any.as(params[0], debugInfo);
          },
          validate: function (node) { return assertNumberOfParams({ max: 1 }, node); },
      },
      boolean: {
          evaluate: function (_a) {
              var _b = __read(_a, 1), value = _b[0];
              return !!value;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      compare: {
          evaluate: function (_a) {
              var _b = __read(_a, 2), a = _b[0], b = _b[1];
              return compare(a, b);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'uuid!': {
          evaluate: function () {
              return uuidTemplate.replace(xyRegexp, function (character) {
                  var randomNbr = Math.floor(Math.random() * 16);
                  var newValue = character === "x" ? randomNbr : (randomNbr & 0x3) | 0x8;
                  return newValue.toString(16);
              });
          },
          validate: function (node) { return assertNumberOfParams(0, node); },
      },
      'lits-version!': {
          evaluate: function () {
              return version;
          },
          validate: function (node) { return assertNumberOfParams(0, node); },
      },
  };
  function contextStackToString(contextStack) {
      return contextStack.stack.reduce(function (result, context, index) {
          return "".concat(result, "Context ").concat(index).concat(context === contextStack.globalContext ? " - Global context" : "", "\n").concat(contextToString(context), "\n");
      }, "");
  }
  function contextToString(context) {
      if (Object.keys(context).length === 0) {
          return "  <empty>\n";
      }
      var maxKeyLength = Math.max.apply(Math, __spreadArray([], __read(Object.keys(context).map(function (key) { return key.length; })), false));
      return Object.entries(context).reduce(function (result, entry) {
          var key = "".concat(entry[0]).padEnd(maxKeyLength + 2, " ");
          return "".concat(result, "  ").concat(key).concat(valueToString(entry[1]), "\n");
      }, "");
  }
  function valueToString(contextEntry) {
      var value = contextEntry.value;
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      var name = value.name;
      if (litsFunction.is(value)) {
          if (name) {
              return "<".concat(value.type, " function ").concat(name, ">");
          }
          else {
              return "<".concat(value.type, " function \u03BB>");
          }
      }
      return JSON.stringify(contextEntry.value);
  }

  var assertNormalExpression = {
      assert: {
          evaluate: function (params, debugInfo) {
              var value = params[0];
              var message = params.length === 2 ? params[1] : "".concat(value);
              string.assert(message, debugInfo);
              if (!value) {
                  throw new AssertionError(message, debugInfo);
              }
              return any.as(value, debugInfo);
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
      'assert=': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
              message = typeof message === "string" && message ? " \"".concat(message, "\"") : "";
              if (first !== second) {
                  throw new AssertionError("Expected ".concat(first, " to be ").concat(second, ".").concat(message), debugInfo);
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'assert-not=': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
              message = typeof message === "string" && message ? " \"".concat(message, "\"") : "";
              if (first === second) {
                  throw new AssertionError("Expected ".concat(first, " not to be ").concat(second, ".").concat(message), debugInfo);
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'assert-equal': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
              message = typeof message === "string" && message ? " \"".concat(message, "\"") : "";
              if (!deepEqual(any.as(first, debugInfo), any.as(second, debugInfo), debugInfo)) {
                  throw new AssertionError("Expected\n".concat(JSON.stringify(first, null, 2), "\nto deep equal\n").concat(JSON.stringify(second, null, 2), ".").concat(message), debugInfo);
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'assert-not-equal': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
              message = typeof message === "string" && message ? " \"".concat(message, "\"") : "";
              if (deepEqual(any.as(first, debugInfo), any.as(second, debugInfo), debugInfo)) {
                  throw new AssertionError("Expected ".concat(JSON.stringify(first), " not to deep equal ").concat(JSON.stringify(second), ".").concat(message), debugInfo);
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'assert>': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
              message = typeof message === "string" && message ? " \"".concat(message, "\"") : "";
              if (compare(first, second) <= 0) {
                  throw new AssertionError("Expected ".concat(first, " to be grater than ").concat(second, ".").concat(message), debugInfo);
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'assert>=': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
              message = typeof message === "string" && message ? " \"".concat(message, "\"") : "";
              if (compare(first, second) < 0) {
                  throw new AssertionError("Expected ".concat(first, " to be grater than or equal to ").concat(second, ".").concat(message), debugInfo);
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'assert<': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
              message = typeof message === "string" && message ? " \"".concat(message, "\"") : "";
              if (compare(first, second) >= 0) {
                  throw new AssertionError("Expected ".concat(first, " to be less than ").concat(second, ".").concat(message), debugInfo);
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'assert<=': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
              message = typeof message === "string" && message ? " \"".concat(message, "\"") : "";
              if (compare(first, second) > 0) {
                  throw new AssertionError("Expected ".concat(first, " to be less than or equal to ").concat(second, ".").concat(message), debugInfo);
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'assert-true': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), first = _b[0], message = _b[1];
              message = typeof message === "string" && message ? " \"".concat(message, "\"") : "";
              if (first !== true) {
                  throw new AssertionError("Expected ".concat(first, " to be true.").concat(message), debugInfo);
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
      'assert-false': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), first = _b[0], message = _b[1];
              message = typeof message === "string" && message ? " \"".concat(message, "\"") : "";
              if (first !== false) {
                  throw new AssertionError("Expected ".concat(first, " to be false.").concat(message), debugInfo);
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
      'assert-truthy': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), first = _b[0], message = _b[1];
              message = typeof message === "string" && message ? " \"".concat(message, "\"") : "";
              if (!first) {
                  throw new AssertionError("Expected ".concat(first, " to be truthy.").concat(message), debugInfo);
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
      'assert-falsy': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), first = _b[0], message = _b[1];
              message = typeof message === "string" && message ? " \"".concat(message, "\"") : "";
              if (first) {
                  throw new AssertionError("Expected ".concat(first, " to be falsy.").concat(message), debugInfo);
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
      'assert-nil': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), first = _b[0], message = _b[1];
              message = typeof message === "string" && message ? " \"".concat(message, "\"") : "";
              if (first !== null) {
                  throw new AssertionError("Expected ".concat(first, " to be nil.").concat(message), debugInfo);
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
      'assert-throws': {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a, 2), func = _c[0], message = _c[1];
              var executeFunction = _b.executeFunction;
              message = typeof message === "string" && message ? " \"".concat(message, "\"") : "";
              litsFunction.assert(func, debugInfo);
              try {
                  executeFunction(func, [], contextStack, debugInfo);
              }
              catch (_d) {
                  return null;
              }
              throw new AssertionError("Expected function to throw.".concat(message), debugInfo);
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
      'assert-throws-error': {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a, 3), func = _c[0], throwMessage = _c[1], message = _c[2];
              var executeFunction = _b.executeFunction;
              message = typeof message === "string" && message ? " \"".concat(message, "\"") : "";
              string.assert(throwMessage, debugInfo);
              litsFunction.assert(func, debugInfo);
              try {
                  executeFunction(func, [], contextStack, debugInfo);
              }
              catch (error) {
                  var errorMessage = error.shortMessage;
                  if (errorMessage !== throwMessage) {
                      throw new AssertionError("Expected function to throw \"".concat(throwMessage, "\", but thrown \"").concat(errorMessage, "\".").concat(message), debugInfo);
                  }
                  return null;
              }
              throw new AssertionError("Expected function to throw \"".concat(throwMessage, "\".").concat(message), debugInfo);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'assert-not-throws': {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a, 2), func = _c[0], message = _c[1];
              var executeFunction = _b.executeFunction;
              message = typeof message === "string" && message ? " \"".concat(message, "\"") : "";
              litsFunction.assert(func, debugInfo);
              try {
                  executeFunction(func, [], contextStack, debugInfo);
              }
              catch (_d) {
                  throw new AssertionError("Expected function not to throw.".concat(message), debugInfo);
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
  };

  var objectNormalExpression = {
      object: {
          evaluate: function (params, debugInfo) {
              var result = {};
              for (var i = 0; i < params.length; i += 2) {
                  var key = params[i];
                  var value = params[i + 1];
                  string.assert(key, debugInfo);
                  result[key] = value;
              }
              return result;
          },
          validate: function (node) { return assertEventNumberOfParams(node); },
      },
      keys: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              object.assert(first, debugInfo);
              return Object.keys(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      vals: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              object.assert(first, debugInfo);
              return Object.values(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      entries: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              object.assert(first, debugInfo);
              return Object.entries(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      find: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), obj = _b[0], key = _b[1];
              object.assert(obj, debugInfo);
              string.assert(key, debugInfo);
              if (collHasKey(obj, key)) {
                  return [key, obj[key]];
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      dissoc: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), obj = _b[0], key = _b[1];
              object.assert(obj, debugInfo);
              string.assert(key, debugInfo);
              var newObj = __assign({}, obj);
              delete newObj[key];
              return newObj;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      merge: {
          evaluate: function (params, debugInfo) {
              if (params.length === 0) {
                  return null;
              }
              var _a = __read(params), first = _a[0], rest = _a.slice(1);
              object.assert(first, debugInfo);
              return rest.reduce(function (result, obj) {
                  object.assert(obj, debugInfo);
                  return __assign(__assign({}, result), obj);
              }, __assign({}, first));
          },
          validate: function (node) { return assertNumberOfParams({ min: 0 }, node); },
      },
      'merge-with': {
          evaluate: function (params, debugInfo, contextStack, _a) {
              var executeFunction = _a.executeFunction;
              var _b = __read(params), fn = _b[0], first = _b[1], rest = _b.slice(2);
              litsFunction.assert(fn, debugInfo);
              if (params.length === 1) {
                  return null;
              }
              object.assert(first, debugInfo);
              return rest.reduce(function (result, obj) {
                  object.assert(obj, debugInfo);
                  Object.entries(obj).forEach(function (entry) {
                      var key = string.as(entry[0], debugInfo);
                      var val = toAny(entry[1]);
                      if (collHasKey(result, key)) {
                          result[key] = executeFunction(fn, [result[key], val], contextStack, debugInfo);
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
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), keys = _b[0], values = _b[1];
              stringArray.assert(keys, debugInfo);
              array.assert(values, debugInfo);
              var length = Math.min(keys.length, values.length);
              var result = {};
              for (var i = 0; i < length; i += 1) {
                  var key = string.as(keys[i], debugInfo);
                  result[key] = toAny(values[i]);
              }
              return result;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      'select-keys': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), obj = _b[0], keys = _b[1];
              stringArray.assert(keys, debugInfo);
              object.assert(obj, debugInfo);
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
              var _b = __read(_a, 1), first = _b[0];
              return litsFunction.is(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'string?': {
          evaluate: function (_a) {
              var _b = __read(_a, 1), first = _b[0];
              return typeof first === "string";
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'number?': {
          evaluate: function (_a) {
              var _b = __read(_a, 1), first = _b[0];
              return typeof first === "number";
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'integer?': {
          evaluate: function (_a) {
              var _b = __read(_a, 1), first = _b[0];
              return typeof first === "number" && number.is(first, { integer: true });
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'boolean?': {
          evaluate: function (_a) {
              var _b = __read(_a, 1), first = _b[0];
              return typeof first === "boolean";
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'nil?': {
          evaluate: function (_a) {
              var _b = __read(_a, 1), first = _b[0];
              return first === null || first === undefined;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'zero?': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              number.assert(first, debugInfo, { finite: true });
              return first === 0;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'pos?': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              number.assert(first, debugInfo, { finite: true });
              return first > 0;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'neg?': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              number.assert(first, debugInfo, { finite: true });
              return first < 0;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'even?': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              number.assert(first, debugInfo, { finite: true });
              return first % 2 === 0;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'odd?': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), first = _b[0];
              number.assert(first, debugInfo, { finite: true });
              return number.is(first, { integer: true }) && first % 2 !== 0;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'array?': {
          evaluate: function (_a) {
              var _b = __read(_a, 1), first = _b[0];
              return array.is(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'coll?': {
          evaluate: function (_a) {
              var _b = __read(_a, 1), first = _b[0];
              return collection.is(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'seq?': {
          evaluate: function (_a) {
              var _b = __read(_a, 1), first = _b[0];
              return sequence.is(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'object?': {
          evaluate: function (_a) {
              var _b = __read(_a, 1), first = _b[0];
              return object.is(first);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'regexp?': {
          evaluate: function (_a) {
              var _b = __read(_a, 1), value = _b[0];
              return isRegularExpression(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'finite?': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Number.isFinite(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'nan?': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return Number.isNaN(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'positive-infinity?': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return value === Number.POSITIVE_INFINITY;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'negative-infinity?': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              number.assert(value, debugInfo);
              return value === Number.NEGATIVE_INFINITY;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'true?': {
          evaluate: function (_a) {
              var _b = __read(_a, 1), value = _b[0];
              return value === true;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'false?': {
          evaluate: function (_a) {
              var _b = __read(_a, 1), value = _b[0];
              return value === false;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'empty?': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), coll = _b[0];
              collection.assert(coll, debugInfo);
              if (string.is(coll)) {
                  return coll.length === 0;
              }
              if (Array.isArray(coll)) {
                  return coll.length === 0;
              }
              return Object.keys(coll).length === 0;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'not-empty?': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), coll = _b[0];
              collection.assert(coll, debugInfo);
              if (string.is(coll)) {
                  return coll.length > 0;
              }
              if (Array.isArray(coll)) {
                  return coll.length > 0;
              }
              return Object.keys(coll).length > 0;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
  };

  var regexpNormalExpression = {
      regexp: {
          evaluate: function (_a, debugInfo) {
              var _b;
              var _c = __read(_a, 2), sourceArg = _c[0], flagsArg = _c[1];
              string.assert(sourceArg, debugInfo);
              var source = sourceArg || "(?:)";
              var flags = string.is(flagsArg) ? flagsArg : "";
              return _b = {},
                  _b[REGEXP_SYMBOL] = true,
                  _b.debugInfo = debugInfo,
                  _b.source = source,
                  _b.flags = flags,
                  _b;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
      match: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), regexp = _b[0], text = _b[1];
              regularExpression.assert(regexp, debugInfo);
              string.assert(text, debugInfo);
              var regExp = new RegExp(regexp.source, regexp.flags);
              var match = regExp.exec(text);
              if (match) {
                  return __spreadArray([], __read(match), false);
              }
              return null;
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      replace: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 3), str = _b[0], regexp = _b[1], value = _b[2];
              string.assert(str, debugInfo);
              regularExpression.assert(regexp, debugInfo);
              string.assert(value, debugInfo);
              var regExp = new RegExp(regexp.source, regexp.flags);
              return str.replace(regExp, value);
          },
          validate: function (node) { return assertNumberOfParams(3, node); },
      },
  };

  var stringNormalExpression = {
      subs: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 3), first = _b[0], second = _b[1], third = _b[2];
              string.assert(first, debugInfo);
              number.assert(second, debugInfo, { integer: true, nonNegative: true });
              if (third === undefined) {
                  return first.substring(second);
              }
              number.assert(third, debugInfo, { gte: second });
              return first.substring(second, third);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'string-repeat': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), str = _b[0], count = _b[1];
              string.assert(str, debugInfo);
              number.assert(count, debugInfo, { integer: true, nonNegative: true });
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
                              : "".concat(param);
                  return result + paramStr;
              }, "");
          },
      },
      number: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), str = _b[0];
              string.assert(str, debugInfo);
              var number = Number(str);
              if (Number.isNaN(number)) {
                  throw new LitsError("Could not convert '".concat(str, "' to a number."), debugInfo);
              }
              return number;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'number-to-string': {
          evaluate: function (params, debugInfo) {
              var _a = __read(params, 2), num = _a[0], base = _a[1];
              number.assert(num, debugInfo, { finite: true });
              if (params.length === 1) {
                  return "".concat(num);
              }
              else {
                  number.assert(base, debugInfo, { finite: true });
                  if (base !== 2 && base !== 8 && base !== 10 && base !== 16) {
                      throw new LitsError("Expected \"number-to-string\" base argument to be 2, 8, 10 or 16, got: ".concat(base), debugInfo);
                  }
                  if (base === 10) {
                      return "".concat(num);
                  }
                  number.assert(num, debugInfo, { integer: true, nonNegative: true });
                  return Number(num).toString(base);
              }
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      },
      'from-char-code': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), num = _b[0];
              number.assert(num, debugInfo, { finite: true });
              var int = toNonNegativeInteger(num);
              try {
                  return String.fromCodePoint(int);
              }
              catch (error) {
                  throw new LitsError(error, debugInfo);
              }
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'to-char-code': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), str = _b[0];
              string.assert(str, debugInfo, { nonEmpty: true });
              return asValue(str.codePointAt(0), debugInfo);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'lower-case': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), str = _b[0];
              string.assert(str, debugInfo);
              return str.toLowerCase();
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'upper-case': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), str = _b[0];
              string.assert(str, debugInfo);
              return str.toUpperCase();
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      trim: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), str = _b[0];
              string.assert(str, debugInfo);
              return str.trim();
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'trim-left': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), str = _b[0];
              string.assert(str, debugInfo);
              return str.replace(/^\s+/, "");
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'trim-right': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), str = _b[0];
              string.assert(str, debugInfo);
              return str.replace(/\s+$/, "");
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      join: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 2), stringList = _b[0], delimiter = _b[1];
              array.assert(stringList, debugInfo);
              stringList.forEach(function (str) { return string.assert(str, debugInfo); });
              string.assert(delimiter, debugInfo);
              return stringList.join(delimiter);
          },
          validate: function (node) { return assertNumberOfParams(2, node); },
      },
      split: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 3), str = _b[0], stringOrRegExpValue = _b[1], limit = _b[2];
              string.assert(str, debugInfo);
              stringOrRegExp.assert(stringOrRegExpValue, debugInfo);
              if (limit !== undefined) {
                  number.assert(limit, debugInfo, { integer: true, nonNegative: true });
              }
              var delimiter = typeof stringOrRegExpValue === "string"
                  ? stringOrRegExpValue
                  : new RegExp(stringOrRegExpValue.source, stringOrRegExpValue.flags);
              return str.split(delimiter, limit);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'pad-left': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 3), str = _b[0], length = _b[1], padString = _b[2];
              string.assert(str, debugInfo);
              number.assert(length, debugInfo, { integer: true });
              if (padString !== undefined) {
                  string.assert(padString, debugInfo);
              }
              return str.padStart(length, padString);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      'pad-right': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 3), str = _b[0], length = _b[1], padString = _b[2];
              string.assert(str, debugInfo);
              number.assert(length, debugInfo, { integer: true });
              if (padString !== undefined) {
                  string.assert(padString, debugInfo);
              }
              return str.padEnd(length, padString);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
      },
      template: {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a), templateString = _b[0], placeholders = _b.slice(1);
              string.assert(templateString, debugInfo);
              array.assert(placeholders, debugInfo);
              var templateStrings = templateString.split("||||");
              if (templateStrings.length <= 1) {
                  return applyPlaceholders(templateStrings[0], placeholders, debugInfo);
              }
              else {
                  // Pluralisation
                  var count = placeholders[0];
                  number.assert(count, debugInfo, { integer: true, nonNegative: true });
                  var stringPlaceholders = __spreadArray(["".concat(count)], __read(placeholders.slice(1)), false);
                  if (templateStrings.length === 2) {
                      // Exactly two valiants.
                      // First variant (singular) for count = 1, Second variant (plural) for count = 0 or count > 1
                      var placehoder = templateStrings[count === 1 ? 0 : 1];
                      return applyPlaceholders(placehoder, stringPlaceholders, debugInfo);
                  }
                  else {
                      // More than two variant:
                      // Use count as index
                      // If count >= number of variants, use last variant
                      var placehoder = templateStrings[Math.min(count, templateStrings.length - 1)];
                      return applyPlaceholders(placehoder, stringPlaceholders, debugInfo);
                  }
              }
          },
          validate: function (node) { return assertNumberOfParams({ min: 1, max: 10 }, node); },
      },
      'encode-base64': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              string.assert(value, debugInfo);
              return btoa(encodeURIComponent(value).replace(/%([0-9A-F]{2})/g, function (_match, p1) {
                  return String.fromCharCode(parseInt(p1, 16));
              }));
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'decode-base64': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              string.assert(value, debugInfo);
              try {
                  return decodeURIComponent(Array.prototype.map
                      .call(atob(value), function (c) {
                      return "%" + ("00" + c.charCodeAt(0).toString(16)).slice(-2);
                  })
                      .join(""));
              }
              catch (error) {
                  throw new LitsError(error, debugInfo);
              }
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'encode-uri-component': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              string.assert(value, debugInfo);
              return encodeURIComponent(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'decode-uri-component': {
          evaluate: function (_a, debugInfo) {
              var _b = __read(_a, 1), value = _b[0];
              string.assert(value, debugInfo);
              try {
                  return decodeURIComponent(value);
              }
              catch (error) {
                  throw new LitsError(error, debugInfo);
              }
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
  };
  var doubleDollarRegexp = /\$\$/g;
  function applyPlaceholders(templateString, placeholders, debugInfo) {
      for (var i = 0; i < 9; i += 1) {
          // Matches $1, $2, ..., $9
          // Does not match $$1
          // But does match $$$1, (since the two first '$' will later be raplaced with a single '$'
          var re = new RegExp("(\\$\\$|[^$]|^)\\$".concat(i + 1), "g");
          if (re.test(templateString)) {
              var placeHolder = stringOrNumber.as(placeholders[i], debugInfo);
              templateString = templateString.replace(re, "$1".concat(placeHolder));
          }
      }
      templateString = templateString.replace(doubleDollarRegexp, "$");
      return templateString;
  }

  var functionalNormalExpression = {
      apply: {
          evaluate: function (_a, debugInfo, contextStack, _b) {
              var _c = __read(_a), func = _c[0], params = _c.slice(1);
              var executeFunction = _b.executeFunction;
              litsFunction.assert(func, debugInfo);
              var paramsLength = params.length;
              var last = params[paramsLength - 1];
              array.assert(last, debugInfo);
              var applyArray = __spreadArray(__spreadArray([], __read(params.slice(0, -1)), false), __read(last), false);
              return executeFunction(func, applyArray, contextStack, debugInfo);
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
      identity: {
          evaluate: function (_a) {
              var _b = __read(_a, 1), value = _b[0];
              return toAny(value);
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      partial: {
          evaluate: function (_a, debugInfo) {
              var _b;
              var _c = __read(_a), fn = _c[0], params = _c.slice(1);
              return _b = {},
                  _b[FUNCTION_SYMBOL] = true,
                  _b.debugInfo = debugInfo,
                  _b.type = "partial",
                  _b.fn = toAny(fn),
                  _b.params = params,
                  _b;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      comp: {
          evaluate: function (fns, debugInfo) {
              var _a;
              if (fns.length > 1) {
                  var last = fns[fns.length - 1];
                  if (array.is(last)) {
                      fns = __spreadArray(__spreadArray([], __read(fns.slice(0, -1)), false), __read(last), false);
                  }
              }
              return _a = {},
                  _a[FUNCTION_SYMBOL] = true,
                  _a.debugInfo = debugInfo,
                  _a.type = "comp",
                  _a.fns = fns,
                  _a;
          },
      },
      constantly: {
          evaluate: function (_a, debugInfo) {
              var _b;
              var _c = __read(_a, 1), value = _c[0];
              return _b = {},
                  _b[FUNCTION_SYMBOL] = true,
                  _b.debugInfo = debugInfo,
                  _b.type = "constantly",
                  _b.value = toAny(value),
                  _b;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      juxt: {
          evaluate: function (fns, debugInfo) {
              var _a;
              return _a = {},
                  _a[FUNCTION_SYMBOL] = true,
                  _a.debugInfo = debugInfo,
                  _a.type = "juxt",
                  _a.fns = fns,
                  _a;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      complement: {
          evaluate: function (_a, debugInfo) {
              var _b;
              var _c = __read(_a, 1), fn = _c[0];
              return _b = {},
                  _b[FUNCTION_SYMBOL] = true,
                  _b.debugInfo = debugInfo,
                  _b.type = "complement",
                  _b.fn = toAny(fn),
                  _b;
          },
          validate: function (node) { return assertNumberOfParams(1, node); },
      },
      'every-pred': {
          evaluate: function (fns, debugInfo) {
              var _a;
              return _a = {},
                  _a[FUNCTION_SYMBOL] = true,
                  _a.debugInfo = debugInfo,
                  _a.type = "every-pred",
                  _a.fns = fns,
                  _a;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      'some-pred': {
          evaluate: function (fns, debugInfo) {
              var _a;
              return _a = {},
                  _a[FUNCTION_SYMBOL] = true,
                  _a.debugInfo = debugInfo,
                  _a.type = "some-pred",
                  _a.fns = fns,
                  _a;
          },
          validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
      },
      fnil: {
          evaluate: function (_a, debugInfo) {
              var _b;
              var _c = __read(_a), fn = _c[0], params = _c.slice(1);
              return _b = {},
                  _b[FUNCTION_SYMBOL] = true,
                  _b.debugInfo = debugInfo,
                  _b.type = "fnil",
                  _b.fn = toAny(fn),
                  _b.params = params,
                  _b;
          },
          validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
      },
  };

  var normalExpressions = __assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign({}, bitwiseNormalExpression), collectionNormalExpression), arrayNormalExpression), sequenceNormalExpression), mathNormalExpression), miscNormalExpression), assertNormalExpression), objectNormalExpression), predicatesNormalExpression), regexpNormalExpression), stringNormalExpression), functionalNormalExpression);

  var commentSpecialExpression = {
      parse: function (tokens, position, _a) {
          var _b;
          var parseToken = _a.parseToken;
          var tkn = token.as(tokens[position], "EOF");
          var node = {
              type: "SpecialExpression",
              name: "comment",
              params: [],
              token: tkn.debugInfo ? tkn : undefined,
          };
          while (!token.is(tkn, { type: "paren", value: ")" })) {
              var bodyNode = void 0;
              _b = __read(parseToken(tokens, position), 2), position = _b[0], bodyNode = _b[1];
              node.params.push(bodyNode);
              tkn = token.as(tokens[position], "EOF");
          }
          return [position + 1, node];
      },
      evaluate: function () { return null; },
      analyze: function () { return ({ undefinedSymbols: new Set() }); },
  };

  var declaredSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = __read(parseTokens(tokens, position), 2), newPosition = _b[0], params = _b[1];
          var node = {
              type: "SpecialExpression",
              name: "declared?",
              params: params,
              token: firstToken.debugInfo ? firstToken : undefined,
          };
          return [newPosition + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var _b;
          var lookUp = _a.lookUp;
          var _c = __read(node.params, 1), astNode = _c[0];
          nameNode.assert(astNode, (_b = node.token) === null || _b === void 0 ? void 0 : _b.debugInfo);
          var lookUpResult = lookUp(astNode, contextStack);
          return !!(lookUpResult.builtinFunction || lookUpResult.contextEntry || lookUpResult.specialExpression);
      },
      validate: function (node) { return assertNumberOfParams(1, node); },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          return analyzeAst(node.params, contextStack, builtin);
      },
  };

  var qqSpecialExpression = {
      parse: function (tokens, position, _a) {
          var parseTokens = _a.parseTokens;
          var firstToken = token.as(tokens[position], "EOF");
          var _b = __read(parseTokens(tokens, position), 2), newPosition = _b[0], params = _b[1];
          var node = {
              type: "SpecialExpression",
              name: "??",
              params: params,
              token: firstToken.debugInfo ? firstToken : undefined,
          };
          return [newPosition + 1, node];
      },
      evaluate: function (node, contextStack, _a) {
          var _b;
          var lookUp = _a.lookUp, evaluateAstNode = _a.evaluateAstNode;
          var _c = __read(node.params, 2), firstNode = _c[0], secondNode = _c[1];
          if (nameNode.is(firstNode)) {
              var lookUpResult = lookUp(firstNode, contextStack);
              if (!(lookUpResult.builtinFunction || lookUpResult.contextEntry || lookUpResult.specialExpression)) {
                  return secondNode ? evaluateAstNode(secondNode, contextStack) : null;
              }
          }
          any.assert(firstNode, (_b = node.token) === null || _b === void 0 ? void 0 : _b.debugInfo);
          var firstResult = evaluateAstNode(firstNode, contextStack);
          return firstResult ? firstResult : secondNode ? evaluateAstNode(secondNode, contextStack) : firstResult;
      },
      validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
      analyze: function (node, contextStack, _a) {
          var analyzeAst = _a.analyzeAst, builtin = _a.builtin;
          return analyzeAst(node.params, contextStack, builtin);
      },
  };

  var specialExpressions = {
      and: andSpecialExpression,
      comment: commentSpecialExpression,
      cond: condSpecialExpression,
      def: defSpecialExpression,
      defn: defnSpecialExpression,
      defns: defnsSpecialExpression,
      defs: defsSpecialExpression,
      do: doSpecialExpression,
      doseq: doseqSpecialExpression,
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
      'declared?': declaredSpecialExpression,
      '??': qqSpecialExpression,
  };
  Object.keys(specialExpressions).forEach(function (key) {
      /* istanbul ignore next */
      if (normalExpressions[key]) {
          throw Error("Expression ".concat(key, " is defined as both a normal expression and a special expression"));
      }
  });
  var builtin = {
      normalExpressions: normalExpressions,
      specialExpressions: specialExpressions,
  };
  var normalExpressionKeys = Object.keys(normalExpressions);
  var specialExpressionKeys = Object.keys(specialExpressions);

  function findOverloadFunction(overloads, nbrOfParams, debugInfo) {
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
          throw new LitsError("Unexpected number of arguments, got ".concat(valueToString$1(nbrOfParams), "."), debugInfo);
      }
      return overloadFunction;
  }
  var functionExecutors = {
      'user-defined': function (fn, params, debugInfo, contextStack, _a) {
          var e_1, _b;
          var evaluateAstNode = _a.evaluateAstNode;
          for (;;) {
              var overloadFunction = findOverloadFunction(fn.overloads, params.length, debugInfo);
              var args = overloadFunction.arguments;
              var nbrOfMandatoryArgs = args.mandatoryArguments.length;
              var newContext = __assign({}, overloadFunction.functionContext);
              var length_1 = Math.max(params.length, args.mandatoryArguments.length);
              var rest = [];
              for (var i = 0; i < length_1; i += 1) {
                  if (i < nbrOfMandatoryArgs) {
                      var param = toAny(params[i]);
                      var key = string.as(args.mandatoryArguments[i], debugInfo);
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
                  try {
                      for (var _c = (e_1 = void 0, __values(overloadFunction.body)), _d = _c.next(); !_d.done; _d = _c.next()) {
                          var node = _d.value;
                          result = evaluateAstNode(node, contextStack.withContext(newContext));
                      }
                  }
                  catch (e_1_1) { e_1 = { error: e_1_1 }; }
                  finally {
                      try {
                          if (_d && !_d.done && (_b = _c.return)) _b.call(_c);
                      }
                      finally { if (e_1) throw e_1.error; }
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
      partial: function (fn, params, debugInfo, contextStack, _a) {
          var executeFunction = _a.executeFunction;
          return executeFunction(fn.fn, __spreadArray(__spreadArray([], __read(fn.params), false), __read(params), false), contextStack, debugInfo);
      },
      comp: function (fn, params, debugInfo, contextStack, _a) {
          var executeFunction = _a.executeFunction;
          var fns = fn.fns;
          if (fns.length === 0) {
              if (params.length !== 1) {
                  throw new LitsError("(comp) expects one argument, got ".concat(valueToString$1(params.length), "."), debugInfo);
              }
              return any.as(params[0], debugInfo);
          }
          return any.as(fns.reduceRight(function (result, fn) {
              return [executeFunction(toAny(fn), result, contextStack, debugInfo)];
          }, params)[0], debugInfo);
      },
      constantly: function (fn) {
          return fn.value;
      },
      juxt: function (fn, params, debugInfo, contextStack, _a) {
          var executeFunction = _a.executeFunction;
          return fn.fns.map(function (fn) { return executeFunction(toAny(fn), params, contextStack, debugInfo); });
      },
      complement: function (fn, params, debugInfo, contextStack, _a) {
          var executeFunction = _a.executeFunction;
          return !executeFunction(fn.fn, params, contextStack, debugInfo);
      },
      'every-pred': function (fn, params, debugInfo, contextStack, _a) {
          var e_2, _b, e_3, _c;
          var executeFunction = _a.executeFunction;
          try {
              for (var _d = __values(fn.fns), _e = _d.next(); !_e.done; _e = _d.next()) {
                  var f = _e.value;
                  try {
                      for (var params_1 = (e_3 = void 0, __values(params)), params_1_1 = params_1.next(); !params_1_1.done; params_1_1 = params_1.next()) {
                          var param = params_1_1.value;
                          var result = executeFunction(toAny(f), [param], contextStack, debugInfo);
                          if (!result) {
                              return false;
                          }
                      }
                  }
                  catch (e_3_1) { e_3 = { error: e_3_1 }; }
                  finally {
                      try {
                          if (params_1_1 && !params_1_1.done && (_c = params_1.return)) _c.call(params_1);
                      }
                      finally { if (e_3) throw e_3.error; }
                  }
              }
          }
          catch (e_2_1) { e_2 = { error: e_2_1 }; }
          finally {
              try {
                  if (_e && !_e.done && (_b = _d.return)) _b.call(_d);
              }
              finally { if (e_2) throw e_2.error; }
          }
          return true;
      },
      'some-pred': function (fn, params, debugInfo, contextStack, _a) {
          var e_4, _b, e_5, _c;
          var executeFunction = _a.executeFunction;
          try {
              for (var _d = __values(fn.fns), _e = _d.next(); !_e.done; _e = _d.next()) {
                  var f = _e.value;
                  try {
                      for (var params_2 = (e_5 = void 0, __values(params)), params_2_1 = params_2.next(); !params_2_1.done; params_2_1 = params_2.next()) {
                          var param = params_2_1.value;
                          var result = executeFunction(toAny(f), [param], contextStack, debugInfo);
                          if (result) {
                              return true;
                          }
                      }
                  }
                  catch (e_5_1) { e_5 = { error: e_5_1 }; }
                  finally {
                      try {
                          if (params_2_1 && !params_2_1.done && (_c = params_2.return)) _c.call(params_2);
                      }
                      finally { if (e_5) throw e_5.error; }
                  }
              }
          }
          catch (e_4_1) { e_4 = { error: e_4_1 }; }
          finally {
              try {
                  if (_e && !_e.done && (_b = _d.return)) _b.call(_d);
              }
              finally { if (e_4) throw e_4.error; }
          }
          return false;
      },
      fnil: function (fn, params, debugInfo, contextStack, _a) {
          var executeFunction = _a.executeFunction;
          var fniledParams = params.map(function (param, index) { return (param === null ? toAny(fn.params[index]) : param); });
          return executeFunction(toAny(fn.fn), fniledParams, contextStack, debugInfo);
      },
      builtin: function (fn, params, debugInfo, contextStack, _a) {
          var executeFunction = _a.executeFunction;
          var normalExpression = asValue(normalExpressions[fn.name], debugInfo);
          return normalExpression.evaluate(params, debugInfo, contextStack, { executeFunction: executeFunction });
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
          return new ContextStackImpl(__spreadArray([context], __read(this.stack), false), this.stack.length - this.numberOfImportedContexts);
      };
      return ContextStackImpl;
  }());
  function evaluate(ast, contextStack) {
      var e_1, _a;
      var result = null;
      try {
          for (var _b = __values(ast.body), _c = _b.next(); !_c.done; _c = _b.next()) {
              var node = _c.value;
              result = evaluateAstNode(node, contextStack);
          }
      }
      catch (e_1_1) { e_1 = { error: e_1_1 }; }
      finally {
          try {
              if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
          }
          finally { if (e_1) throw e_1.error; }
      }
      return result;
  }
  var evaluateAstNode = function (node, contextStack) {
      var _a;
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
              throw new LitsError("".concat(node.type, "-node cannot be evaluated"), (_a = node.token) === null || _a === void 0 ? void 0 : _a.debugInfo);
      }
  };
  function evaluateNumber(node) {
      return node.value;
  }
  function evaluateString(node) {
      return node.value;
  }
  function evaluateReservedName(node) {
      var _a;
      return asValue(reservedNamesRecord[node.value], (_a = node.token) === null || _a === void 0 ? void 0 : _a.debugInfo).value;
  }
  function evaluateName(node, contextStack) {
      var _a;
      var lookUpResult = lookUp(node, contextStack);
      if (lookUpResult.contextEntry) {
          return lookUpResult.contextEntry.value;
      }
      else if (lookUpResult.builtinFunction) {
          return lookUpResult.builtinFunction;
      }
      throw new UndefinedSymbolError(node.value, (_a = node.token) === null || _a === void 0 ? void 0 : _a.debugInfo);
  }
  function lookUp(node, contextStack) {
      var e_2, _a, _b;
      var _c;
      var value = node.value;
      var debugInfo = (_c = node.token) === null || _c === void 0 ? void 0 : _c.debugInfo;
      try {
          for (var _d = __values(contextStack.stack), _e = _d.next(); !_e.done; _e = _d.next()) {
              var context = _e.value;
              var variable = context[value];
              if (variable) {
                  return {
                      builtinFunction: null,
                      contextEntry: variable,
                      specialExpression: null,
                  };
              }
          }
      }
      catch (e_2_1) { e_2 = { error: e_2_1 }; }
      finally {
          try {
              if (_e && !_e.done && (_a = _d.return)) _a.call(_d);
          }
          finally { if (e_2) throw e_2.error; }
      }
      if (builtin.normalExpressions[value]) {
          var builtinFunction = (_b = {},
              _b[FUNCTION_SYMBOL] = true,
              _b.debugInfo = debugInfo,
              _b.type = "builtin",
              _b.name = value,
              _b);
          return {
              builtinFunction: builtinFunction,
              contextEntry: null,
              specialExpression: null,
          };
      }
      if (builtin.specialExpressions[value]) {
          return {
              specialExpression: true,
              builtinFunction: null,
              contextEntry: null,
          };
      }
      return {
          specialExpression: null,
          builtinFunction: null,
          contextEntry: null,
      };
  }
  function evaluateNormalExpression(node, contextStack) {
      var e_3, _a;
      var _b, _c;
      var params = node.params.map(function (paramNode) { return evaluateAstNode(paramNode, contextStack); });
      var debugInfo = (_b = node.token) === null || _b === void 0 ? void 0 : _b.debugInfo;
      if (normalExpressionNodeWithName.is(node)) {
          try {
              for (var _d = __values(contextStack.stack), _e = _d.next(); !_e.done; _e = _d.next()) {
                  var context = _e.value;
                  var fn = (_c = context[node.name]) === null || _c === void 0 ? void 0 : _c.value;
                  if (fn === undefined) {
                      continue;
                  }
                  return executeFunction(fn, params, contextStack, debugInfo);
              }
          }
          catch (e_3_1) { e_3 = { error: e_3_1 }; }
          finally {
              try {
                  if (_e && !_e.done && (_a = _d.return)) _a.call(_d);
              }
              finally { if (e_3) throw e_3.error; }
          }
          return evaluateBuiltinNormalExpression(node, params, contextStack);
      }
      else {
          var fn = evaluateAstNode(node.expression, contextStack);
          return executeFunction(fn, params, contextStack, debugInfo);
      }
  }
  var executeFunction = function (fn, params, contextStack, debugInfo) {
      if (litsFunction.is(fn)) {
          return functionExecutors[fn.type](fn, params, debugInfo, contextStack, { evaluateAstNode: evaluateAstNode, executeFunction: executeFunction });
      }
      if (Array.isArray(fn)) {
          return evaluateArrayAsFunction(fn, params, debugInfo);
      }
      if (object.is(fn)) {
          return evalueateObjectAsFunction(fn, params, debugInfo);
      }
      if (string.is(fn)) {
          return evaluateStringAsFunction(fn, params, debugInfo);
      }
      if (number.is(fn)) {
          return evaluateNumberAsFunction(fn, params, debugInfo);
      }
      throw new NotAFunctionError(fn, debugInfo);
  };
  function evaluateBuiltinNormalExpression(node, params, contextStack) {
      var _a, _b;
      var normalExpression = builtin.normalExpressions[node.name];
      if (!normalExpression) {
          throw new UndefinedSymbolError(node.name, (_a = node.token) === null || _a === void 0 ? void 0 : _a.debugInfo);
      }
      return normalExpression.evaluate(params, (_b = node.token) === null || _b === void 0 ? void 0 : _b.debugInfo, contextStack, { executeFunction: executeFunction });
  }
  function evaluateSpecialExpression(node, contextStack) {
      var _a;
      var specialExpression = asValue(builtin.specialExpressions[node.name], (_a = node.token) === null || _a === void 0 ? void 0 : _a.debugInfo);
      return specialExpression.evaluate(node, contextStack, { evaluateAstNode: evaluateAstNode, builtin: builtin, lookUp: lookUp });
  }
  function evalueateObjectAsFunction(fn, params, debugInfo) {
      if (params.length !== 1) {
          throw new LitsError("Object as function requires one string parameter.", debugInfo);
      }
      var key = params[0];
      string.assert(key, debugInfo);
      return toAny(fn[key]);
  }
  function evaluateArrayAsFunction(fn, params, debugInfo) {
      if (params.length !== 1) {
          throw new LitsError("Array as function requires one non negative integer parameter.", debugInfo);
      }
      var index = params[0];
      number.assert(index, debugInfo, { integer: true, nonNegative: true });
      return toAny(fn[index]);
  }
  function evaluateStringAsFunction(fn, params, debugInfo) {
      if (params.length !== 1) {
          throw new LitsError("String as function requires one Obj parameter.", debugInfo);
      }
      var param = toAny(params[0]);
      if (object.is(param)) {
          return toAny(param[fn]);
      }
      if (number.is(param, { integer: true })) {
          return toAny(fn[param]);
      }
      throw new LitsError("string as function expects Obj or integer parameter, got ".concat(valueToString$1(param)), debugInfo);
  }
  function evaluateNumberAsFunction(fn, params, debugInfo) {
      number.assert(fn, debugInfo, { integer: true });
      if (params.length !== 1) {
          throw new LitsError("Number as function requires one Arr parameter.", debugInfo);
      }
      var param = params[0];
      sequence.assert(param, debugInfo);
      return toAny(param[fn]);
  }

  var analyzeAst = function (astNode, contextStack, builtin) {
      var e_1, _a;
      var astNodes = Array.isArray(astNode) ? astNode : [astNode];
      var analyzeResult = {
          undefinedSymbols: new Set(),
      };
      try {
          for (var astNodes_1 = __values(astNodes), astNodes_1_1 = astNodes_1.next(); !astNodes_1_1.done; astNodes_1_1 = astNodes_1.next()) {
              var subNode = astNodes_1_1.value;
              var result = analyzeAstNode(subNode, contextStack, builtin);
              result.undefinedSymbols.forEach(function (symbol) { return analyzeResult.undefinedSymbols.add(symbol); });
          }
      }
      catch (e_1_1) { e_1 = { error: e_1_1 }; }
      finally {
          try {
              if (astNodes_1_1 && !astNodes_1_1.done && (_a = astNodes_1.return)) _a.call(astNodes_1);
          }
          finally { if (e_1) throw e_1.error; }
      }
      return analyzeResult;
  };
  function analyzeAstNode(astNode, contextStack, builtin) {
      var e_2, _a;
      var _b;
      var emptySet = new Set();
      switch (astNode.type) {
          case "Name": {
              var lookUpResult = lookUp(astNode, contextStack);
              if (!lookUpResult.builtinFunction && !lookUpResult.contextEntry && !lookUpResult.specialExpression) {
                  return { undefinedSymbols: new Set([{ symbol: astNode.value, token: astNode.token }]) };
              }
              return { undefinedSymbols: emptySet };
          }
          case "String":
          case "Number":
          case "Modifier":
          case "ReservedName":
              return { undefinedSymbols: emptySet };
          case "NormalExpression": {
              var undefinedSymbols_1 = new Set();
              var expression = astNode.expression, name_1 = astNode.name, token = astNode.token;
              if (typeof name_1 === "string") {
                  var lookUpResult = lookUp({ type: "Name", value: name_1, token: token }, contextStack);
                  if (lookUpResult.builtinFunction === null &&
                      lookUpResult.contextEntry === null &&
                      lookUpResult.specialExpression === null) {
                      undefinedSymbols_1.add({ symbol: name_1, token: astNode.token });
                  }
              }
              if (expression) {
                  switch (expression.type) {
                      case "String":
                      case "Number":
                          break;
                      case "NormalExpression":
                      case "SpecialExpression": {
                          var subResult = analyzeAstNode(expression, contextStack, builtin);
                          subResult.undefinedSymbols.forEach(function (symbol) { return undefinedSymbols_1.add(symbol); });
                          break;
                      }
                  }
              }
              try {
                  for (var _c = __values(astNode.params), _d = _c.next(); !_d.done; _d = _c.next()) {
                      var subNode = _d.value;
                      var subNodeResult = analyzeAst(subNode, contextStack, builtin);
                      subNodeResult.undefinedSymbols.forEach(function (symbol) { return undefinedSymbols_1.add(symbol); });
                  }
              }
              catch (e_2_1) { e_2 = { error: e_2_1 }; }
              finally {
                  try {
                      if (_d && !_d.done && (_a = _c.return)) _a.call(_c);
                  }
                  finally { if (e_2) throw e_2.error; }
              }
              return { undefinedSymbols: undefinedSymbols_1 };
          }
          case "SpecialExpression": {
              var specialExpression = asValue(builtin.specialExpressions[astNode.name], (_b = astNode.token) === null || _b === void 0 ? void 0 : _b.debugInfo);
              var result = specialExpression.analyze(astNode, contextStack, {
                  analyzeAst: analyzeAst,
                  builtin: builtin,
              });
              return result;
          }
      }
  }

  var parseNumber = function (tokens, position) {
      var tkn = token.as(tokens[position], "EOF");
      return [position + 1, { type: "Number", value: Number(tkn.value), token: tkn.debugInfo ? tkn : undefined }];
  };
  var parseString = function (tokens, position) {
      var tkn = token.as(tokens[position], "EOF");
      return [position + 1, { type: "String", value: tkn.value, token: tkn.debugInfo ? tkn : undefined }];
  };
  var parseName = function (tokens, position) {
      var tkn = token.as(tokens[position], "EOF");
      return [position + 1, { type: "Name", value: tkn.value, token: tkn.debugInfo ? tkn : undefined }];
  };
  var parseReservedName = function (tokens, position) {
      var tkn = token.as(tokens[position], "EOF");
      return [
          position + 1,
          { type: "ReservedName", value: tkn.value, token: tkn.debugInfo ? tkn : undefined },
      ];
  };
  var parseTokens = function (tokens, position) {
      var _a;
      var tkn = token.as(tokens[position], "EOF");
      var astNodes = [];
      var astNode;
      while (!(tkn.type === "paren" && (tkn.value === ")" || tkn.value === "]"))) {
          _a = __read(parseToken(tokens, position), 2), position = _a[0], astNode = _a[1];
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
          _a = __read(parseToken(tokens, position), 2), position = _a[0], param = _a[1];
          params.push(param);
          tkn = token.as(tokens[position], "EOF");
      }
      position = position + 1;
      var node = {
          type: "NormalExpression",
          name: "array",
          params: params,
          token: firstToken.debugInfo ? firstToken : undefined,
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
          _a = __read(parseToken(tokens, position), 2), position = _a[0], param = _a[1];
          params.push(param);
          tkn = token.as(tokens[position], "EOF");
      }
      position = position + 1;
      var node = {
          type: "NormalExpression",
          name: "object",
          params: params,
          token: firstToken.debugInfo ? firstToken : undefined,
      };
      assertEventNumberOfParams(node);
      return [position, node];
  };
  var parseRegexpShorthand = function (tokens, position) {
      var tkn = token.as(tokens[position], "EOF");
      var stringNode = {
          type: "String",
          value: tkn.value,
          token: tkn.debugInfo ? tkn : undefined,
      };
      assertValue(tkn.options, tkn.debugInfo);
      var optionsNode = {
          type: "String",
          value: "".concat(tkn.options.g ? "g" : "").concat(tkn.options.i ? "i" : ""),
          token: tkn.debugInfo ? tkn : undefined,
      };
      var node = {
          type: "NormalExpression",
          name: "regexp",
          params: [stringNode, optionsNode],
          token: tkn.debugInfo ? tkn : undefined,
      };
      return [position + 1, node];
  };
  var placeholderRegexp = /^%([1-9][0-9]?$)/;
  var parseFnShorthand = function (tokens, position) {
      var firstToken = token.as(tokens[position], "EOF");
      position += 1;
      var _a = __read(parseExpression(tokens, position), 2), newPosition = _a[0], expressionNode = _a[1];
      var arity = 0;
      for (var pos = position + 1; pos < newPosition - 1; pos += 1) {
          var tkn = token.as(tokens[pos], "EOF");
          if (tkn.type === "name") {
              var match = placeholderRegexp.exec(tkn.value);
              if (match) {
                  arity = Math.max(arity, Number(match[1]));
                  if (arity > 20) {
                      throw new LitsError("Can't specify more than 20 arguments", firstToken.debugInfo);
                  }
              }
          }
          if (tkn.type === "fnShorthand") {
              throw new LitsError("Nested shortcut functions are not allowed", firstToken.debugInfo);
          }
      }
      var mandatoryArguments = [];
      for (var i = 1; i <= arity; i += 1) {
          mandatoryArguments.push("%".concat(i));
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
                  body: [expressionNode],
                  arity: args.mandatoryArguments.length,
              },
          ],
          token: firstToken.debugInfo ? firstToken : undefined,
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
          return [position + 1, { type: "Modifier", value: value, token: tkn.debugInfo ? tkn : undefined }];
      }
      else {
          throw new LitsError("Expected name or modifier token, got ".concat(valueToString$1(tkn), "."), tkn.debugInfo);
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
          _a = __read(parseBinding(tokens, position), 2), position = _a[0], binding = _a[1];
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
      _a = __read(parseToken(tokens, position), 2), position = _a[0], value = _a[1];
      var node = {
          type: "Binding",
          name: name,
          value: value,
          token: firstToken.debugInfo ? firstToken : undefined,
      };
      return [position, node];
  };
  var parseNormalExpression = function (tokens, position) {
      var _a;
      var _b, _c;
      var _d = __read(parseToken(tokens, position), 2), newPosition = _d[0], fnNode = _d[1];
      var params;
      _a = __read(parseTokens(tokens, newPosition), 2), position = _a[0], params = _a[1];
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
      nameNode.assert(fnNode, (_b = fnNode.token) === null || _b === void 0 ? void 0 : _b.debugInfo);
      var node = {
          type: "NormalExpression",
          name: fnNode.value,
          params: params,
          token: fnNode.token,
      };
      var builtinExpression = builtin.normalExpressions[node.name];
      if (builtinExpression) {
          (_c = builtinExpression.validate) === null || _c === void 0 ? void 0 : _c.call(builtinExpression, node);
      }
      return [position, node];
  };
  var parseSpecialExpression = function (tokens, position) {
      var _a = token.as(tokens[position], "EOF"), expressionName = _a.value, debugInfo = _a.debugInfo;
      position += 1;
      var _b = asValue(builtin.specialExpressions[expressionName], debugInfo), parse = _b.parse, validate = _b.validate;
      var _c = __read(parse(tokens, position, {
          parseExpression: parseExpression,
          parseTokens: parseTokens,
          parseToken: parseToken,
          parseBinding: parseBinding,
          parseBindings: parseBindings,
          parseArgument: parseArgument,
      }), 2), positionAfterParse = _c[0], node = _c[1];
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
          throw new LitsError("Unrecognized token: ".concat(tkn.type, " value=").concat(tkn.value), tkn.debugInfo);
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
          _a = __read(parseToken(tokens, position), 2), position = _a[0], node = _a[1];
          ast.body.push(node);
      }
      return ast;
  }

  var NO_MATCH = [0, undefined];
  // A name (function or variable) can contain a lot of different characters
  var nameRegExp = /[@%0-9a-zA-ZàáâãăäāåæćčçèéêĕëēìíîĭïðłñòóôõöőøšùúûüűýÿþÀÁÂÃĂÄĀÅÆĆČÇÈÉÊĔËĒÌÍÎĬÏÐŁÑÒÓÔÕÖŐØŠÙÚÛÜŰÝÞß_^?=!$%<>+*/-]/;
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
  var tokenizeLeftParen = function (input, position, debugInfo) {
      return tokenizeCharacter("paren", "(", input, position, debugInfo);
  };
  var tokenizeRightParen = function (input, position, debugInfo) {
      return tokenizeCharacter("paren", ")", input, position, debugInfo);
  };
  var tokenizeLeftBracket = function (input, position, debugInfo) {
      return tokenizeCharacter("paren", "[", input, position, debugInfo);
  };
  var tokenizeRightBracket = function (input, position, debugInfo) {
      return tokenizeCharacter("paren", "]", input, position, debugInfo);
  };
  var tokenizeLeftCurly = function (input, position, debugInfo) {
      return tokenizeCharacter("paren", "{", input, position, debugInfo);
  };
  var tokenizeRightCurly = function (input, position, debugInfo) {
      return tokenizeCharacter("paren", "}", input, position, debugInfo);
  };
  var tokenizeString = function (input, position, debugInfo) {
      if (input[position] !== "\"") {
          return NO_MATCH;
      }
      var value = "";
      var length = 1;
      var char = input[position + length];
      var escape = false;
      while (char !== "\"" || escape) {
          if (char === undefined) {
              throw new LitsError("Unclosed string at position ".concat(position, "."), debugInfo);
          }
          length += 1;
          if (escape) {
              escape = false;
              if (char === "\"" || char === "\\") {
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
      return [length + 1, { type: "string", value: value, debugInfo: debugInfo }];
  };
  var tokenizeSymbolString = function (input, position, debugInfo) {
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
      return [length, { type: "string", value: value, debugInfo: debugInfo }];
  };
  var tokenizeRegexpShorthand = function (input, position, debugInfo) {
      var _a;
      if (input[position] !== "#") {
          return NO_MATCH;
      }
      var _b = __read(tokenizeString(input, position + 1, debugInfo), 2), stringLength = _b[0], token = _b[1];
      if (!token) {
          return NO_MATCH;
      }
      position += stringLength + 1;
      var length = stringLength + 1;
      var options = {};
      while (input[position] === "g" || input[position] === "i") {
          if (input[position] === "g") {
              if (options.g) {
                  throw new LitsError("Duplicated regexp option \"".concat(input[position], "\" at position ").concat(position, "."), debugInfo);
              }
              length += 1;
              options.g = true;
          }
          else {
              if (options.i) {
                  throw new LitsError("Duplicated regexp option \"".concat(input[position], "\" at position ").concat(position, "."), debugInfo);
              }
              length += 1;
              options.i = true;
          }
          position += 1;
      }
      if (nameRegExp.test((_a = input[position]) !== null && _a !== void 0 ? _a : "")) {
          throw new LitsError("Unexpected regexp option \"".concat(input[position], "\" at position ").concat(position, "."), debugInfo);
      }
      return [
          length,
          {
              type: "regexpShorthand",
              value: token.value,
              options: options,
              debugInfo: debugInfo,
          },
      ];
  };
  var tokenizeFnShorthand = function (input, position, debugInfo) {
      if (input.slice(position, position + 2) !== "#(") {
          return NO_MATCH;
      }
      return [
          1,
          {
              type: "fnShorthand",
              value: "#",
              debugInfo: debugInfo,
          },
      ];
  };
  var endOfNumberRegExp = /\s|[)\]},]/;
  var decimalNumberRegExp = /[0-9]/;
  var octalNumberRegExp = /[0-7]/;
  var hexNumberRegExp = /[0-9a-fA-F]/;
  var binaryNumberRegExp = /[0-1]/;
  var firstCharRegExp = /[0-9.-]/;
  var tokenizeNumber = function (input, position, debugInfo) {
      var type = "decimal";
      var firstChar = input[position];
      if (!firstCharRegExp.test(firstChar)) {
          return NO_MATCH;
      }
      var hasDecimals = firstChar === ".";
      var i;
      for (i = position + 1; i < input.length; i += 1) {
          var char = string.as(input[i], debugInfo, { char: true });
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
      return [length, { type: "number", value: value, debugInfo: debugInfo }];
  };
  var tokenizeReservedName = function (input, position, debugInfo) {
      var e_1, _a;
      try {
          for (var _b = __values(Object.entries(reservedNamesRecord)), _c = _b.next(); !_c.done; _c = _b.next()) {
              var _d = __read(_c.value, 2), reservedName = _d[0], forbidden = _d[1].forbidden;
              var length_2 = reservedName.length;
              var nextChar = input[position + length_2];
              if (nextChar && nameRegExp.test(nextChar)) {
                  continue;
              }
              var name_1 = input.substr(position, length_2);
              if (name_1 === reservedName) {
                  if (forbidden) {
                      throw new LitsError("".concat(name_1, " is forbidden!"), debugInfo);
                  }
                  return [length_2, { type: "reservedName", value: reservedName, debugInfo: debugInfo }];
              }
          }
      }
      catch (e_1_1) { e_1 = { error: e_1_1 }; }
      finally {
          try {
              if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
          }
          finally { if (e_1) throw e_1.error; }
      }
      return NO_MATCH;
  };
  var tokenizeName = function (input, position, debugInfo) {
      return tokenizePattern("name", nameRegExp, input, position, debugInfo);
  };
  var tokenizeModifier = function (input, position, debugInfo) {
      var e_2, _a;
      var modifiers = ["&", "&let", "&when", "&while"];
      try {
          for (var modifiers_1 = __values(modifiers), modifiers_1_1 = modifiers_1.next(); !modifiers_1_1.done; modifiers_1_1 = modifiers_1.next()) {
              var modifier = modifiers_1_1.value;
              var length_3 = modifier.length;
              var charAfterModifier = input[position + length_3];
              if (input.substr(position, length_3) === modifier && (!charAfterModifier || !nameRegExp.test(charAfterModifier))) {
                  var value = modifier;
                  return [length_3, { type: "modifier", value: value, debugInfo: debugInfo }];
              }
          }
      }
      catch (e_2_1) { e_2 = { error: e_2_1 }; }
      finally {
          try {
              if (modifiers_1_1 && !modifiers_1_1.done && (_a = modifiers_1.return)) _a.call(modifiers_1);
          }
          finally { if (e_2) throw e_2.error; }
      }
      return NO_MATCH;
  };
  function tokenizeCharacter(type, value, input, position, debugInfo) {
      if (value === input[position]) {
          return [1, { type: type, value: value, debugInfo: debugInfo }];
      }
      else {
          return NO_MATCH;
      }
  }
  function tokenizePattern(type, pattern, input, position, debugInfo) {
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
      return [length, { type: type, value: value, debugInfo: debugInfo }];
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
  function getSourceCodeLine(input, lineNbr) {
      return input.split(/\r\n|\r|\n/)[lineNbr];
  }
  function createDebugInfo(input, position, getLocation) {
      var lines = input.substr(0, position + 1).split(/\r\n|\r|\n/);
      var lastLine = lines[lines.length - 1];
      var code = getSourceCodeLine(input, lines.length - 1);
      var line = lines.length;
      var column = lastLine.length;
      return {
          code: code,
          line: line,
          column: column,
          getLocation: getLocation,
      };
  }
  function tokenize(input, params) {
      var e_1, _a;
      var tokens = [];
      var position = 0;
      var tokenized = false;
      while (position < input.length) {
          tokenized = false;
          // Loop through all tokenizer until one matches
          var debugInfo = params.debug
              ? createDebugInfo(input, position, params.getLocation)
              : undefined;
          try {
              for (var tokenizers_1 = (e_1 = void 0, __values(tokenizers)), tokenizers_1_1 = tokenizers_1.next(); !tokenizers_1_1.done; tokenizers_1_1 = tokenizers_1.next()) {
                  var tokenize_1 = tokenizers_1_1.value;
                  var _b = __read(tokenize_1(input, position, debugInfo), 2), nbrOfCharacters = _b[0], token = _b[1];
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
          }
          catch (e_1_1) { e_1 = { error: e_1_1 }; }
          finally {
              try {
                  if (tokenizers_1_1 && !tokenizers_1_1.done && (_a = tokenizers_1.return)) _a.call(tokenizers_1);
              }
              finally { if (e_1) throw e_1.error; }
          }
          if (!tokenized) {
              throw new LitsError("Unrecognized character '".concat(input[position], "'."), debugInfo);
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
          this.maxSize = maxSize === null ? null : toNonNegativeInteger(maxSize);
          if (typeof this.maxSize === "number" && this.maxSize < 1) {
              throw Error("1 is the minimum maxSize, got ".concat(valueToString$1(maxSize)));
          }
      }
      Cache.prototype.getContent = function () {
          return Object.entries(this.cache).reduce(function (result, _a) {
              var _b = __read(_a, 2), key = _b[0], entry = _b[1];
              result[key] = entry.value;
              return result;
          }, {});
      };
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
              throw Error("AstCache - key already present: ".concat(key));
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
          while (this.maxSize !== null && this.size > this.maxSize) {
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
          var e_1, _a;
          if (config === void 0) { config = {}; }
          var _b, _c, _d;
          this.debug = (_b = config.debug) !== null && _b !== void 0 ? _b : false;
          this.astCacheSize = (_c = config.astCacheSize) !== null && _c !== void 0 ? _c : null;
          if (this.astCacheSize !== 0) {
              this.astCache = new Cache(this.astCacheSize);
              var initialCache = (_d = config.initialCache) !== null && _d !== void 0 ? _d : {};
              try {
                  for (var _e = __values(Object.keys(initialCache)), _f = _e.next(); !_f.done; _f = _e.next()) {
                      var cacheEntry = _f.value;
                      this.astCache.set(cacheEntry, initialCache[cacheEntry]);
                  }
              }
              catch (e_1_1) { e_1 = { error: e_1_1 }; }
              finally {
                  try {
                      if (_f && !_f.done && (_a = _e.return)) _a.call(_e);
                  }
                  finally { if (e_1) throw e_1.error; }
              }
          }
          else {
              this.astCache = null;
          }
      }
      Lits.prototype.getRuntimeInfo = function () {
          return {
              astCacheSize: this.astCacheSize,
              astCache: this.astCache,
              debug: this.debug,
          };
      };
      Lits.prototype.run = function (program, params) {
          if (params === void 0) { params = {}; }
          var ast = this.generateAst(program, params.getLocation);
          var result = this.evaluate(ast, params);
          return result;
      };
      Lits.prototype.context = function (program, params) {
          if (params === void 0) { params = {}; }
          var contextStack = createContextStackFromParams(params);
          var ast = this.generateAst(program, params.getLocation);
          evaluate(ast, contextStack);
          return contextStack.globalContext;
      };
      Lits.prototype.analyze = function (program) {
          var params = {};
          var contextStack = createContextStackFromParams(params);
          var ast = this.generateAst(program, params.getLocation);
          return analyzeAst(ast.body, contextStack, builtin);
      };
      Lits.prototype.tokenize = function (program, getLocation) {
          return tokenize(program, { debug: this.debug, getLocation: getLocation });
      };
      Lits.prototype.parse = function (tokens) {
          return parse(tokens);
      };
      Lits.prototype.evaluate = function (ast, params) {
          var contextStack = createContextStackFromParams(params);
          return evaluate(ast, contextStack);
      };
      Lits.prototype.apply = function (fn, fnParams, params) {
          var _a;
          if (params === void 0) { params = {}; }
          var fnName = "FN_2eb7b316-471c-5bfa-90cb-d3dfd9164a59";
          var paramsString = fnParams
              .map(function (_, index) {
              return "".concat(fnName, "_").concat(index);
          })
              .join(" ");
          var program = "(".concat(fnName, " ").concat(paramsString, ")");
          var ast = this.generateAst(program, params.getLocation);
          var globals = fnParams.reduce(function (result, param, index) {
              result["".concat(fnName, "_").concat(index)] = param;
              return result;
          }, (_a = {}, _a[fnName] = fn, _a));
          params.globals = __assign(__assign({}, params.globals), globals);
          return this.evaluate(ast, params);
      };
      Lits.prototype.generateAst = function (untrimmedProgram, getLocation) {
          var _a;
          var program = untrimmedProgram.trim();
          if (this.astCache) {
              var cachedAst = this.astCache.get(program);
              if (cachedAst) {
                  return cachedAst;
              }
          }
          var tokens = this.tokenize(program, getLocation);
          var ast = this.parse(tokens);
          (_a = this.astCache) === null || _a === void 0 ? void 0 : _a.set(program, ast);
          return ast;
      };
      return Lits;
  }());
  function createContextStackFromParams(params) {
      var _a, _b;
      var globalContext = (_a = params.globalContext) !== null && _a !== void 0 ? _a : {};
      Object.assign(globalContext, createContextFromValues(params.globals));
      var contextStack = createContextStack(__spreadArray([globalContext], __read(((_b = params.contexts) !== null && _b !== void 0 ? _b : [])), false));
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
