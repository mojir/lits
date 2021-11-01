var Lits = (function (exports) {
    'use strict';

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
        function LitsError(message, meta) {
            var _this = _super.call(this, message + " " + meta) || this;
            _this.line = meta === "EOF" ? null : meta.line;
            _this.column = meta === "EOF" ? null : meta.column;
            Object.setPrototypeOf(_this, LitsError.prototype);
            _this.name = "LitsError";
            return _this;
        }
        return LitsError;
    }(Error));
    var UserDefinedError = /** @class */ (function (_super) {
        __extends(UserDefinedError, _super);
        function UserDefinedError(message, meta) {
            var _this = _super.call(this, message + " " + meta) || this;
            _this.line = meta === "EOF" ? null : meta.line;
            _this.column = meta === "EOF" ? null : meta.column;
            Object.setPrototypeOf(_this, UserDefinedError.prototype);
            _this.name = "UserDefinedError";
            return _this;
        }
        return UserDefinedError;
    }(Error));
    var AssertionError = /** @class */ (function (_super) {
        __extends(AssertionError, _super);
        function AssertionError(message, meta) {
            var _this = _super.call(this, message + " " + meta) || this;
            _this.line = meta === "EOF" ? null : meta.line;
            _this.column = meta === "EOF" ? null : meta.column;
            Object.setPrototypeOf(_this, AssertionError.prototype);
            _this.name = "AssertionError";
            return _this;
        }
        return AssertionError;
    }(Error));
    var UnexpectedTokenError = /** @class */ (function (_super) {
        __extends(UnexpectedTokenError, _super);
        function UnexpectedTokenError(expectedToken, actualToken) {
            var _this = _super.call(this, "Expected a \"" + expectedToken + "\" token, got Token[" + actualToken.type + ":\"" + actualToken.value + "\"] " + actualToken.meta) || this;
            _this.line = actualToken.meta === "EOF" ? null : actualToken.meta.line;
            _this.column = actualToken.meta === "EOF" ? null : actualToken.meta.column;
            Object.setPrototypeOf(_this, UnexpectedTokenError.prototype);
            _this.name = "UnexpectedTokenError";
            return _this;
        }
        return UnexpectedTokenError;
    }(Error));
    var UnexpectedNodeTypeError = /** @class */ (function (_super) {
        __extends(UnexpectedNodeTypeError, _super);
        function UnexpectedNodeTypeError(expectedNodeType, actualNode, meta) {
            var _this = _super.call(this, "Expected a " + expectedNodeType + " node, got " + (actualNode ? "a " + actualNode.type + " node" : "undefined") + " " + meta) || this;
            _this.line = meta === "EOF" ? null : meta.line;
            _this.column = meta === "EOF" ? null : meta.column;
            Object.setPrototypeOf(_this, UnexpectedNodeTypeError.prototype);
            _this.name = "UnexpectedNodeTypeError";
            return _this;
        }
        return UnexpectedNodeTypeError;
    }(Error));

    var FUNCTION_SYMBOL = Symbol("function");

    function asAstNode(node, meta) {
        if (node === undefined) {
            throw new LitsError("Expected an AST node, got undefined", meta);
        }
        return node;
    }
    function asNameNode(node, meta) {
        if (node === undefined || node.type !== "Name") {
            throw new UnexpectedNodeTypeError("Name", node, meta);
        }
        return node;
    }
    function assertNameNode(node, meta) {
        if (node === undefined || node.type !== "Name") {
            throw new UnexpectedNodeTypeError("Name", node, meta);
        }
    }
    function asAny(value, meta) {
        if (value === undefined) {
            throw new LitsError("Unexpected end of input", meta);
        }
        return value;
    }
    function asNotUndefined(value, meta) {
        if (meta === void 0) { meta = "EOF"; }
        if (value === undefined) {
            throw new LitsError("Unexpected nil", meta);
        }
        return value;
    }
    function assertNotUndefined(value, meta) {
        if (meta === void 0) { meta = "EOF"; }
        if (value === undefined) {
            throw new LitsError("Unexpected nil", meta);
        }
    }
    function assertFiniteNumber(value, meta) {
        if (typeof value !== "number" || !isFinite(value)) {
            throw new LitsError("Expected number, got: " + value + " type=\"" + typeof value + "\"", meta);
        }
    }
    function assertPositiveNumber(value, meta) {
        assertFiniteNumber(value, meta);
        if (value <= 0) {
            throw new LitsError("Expected positive number, got " + value, meta);
        }
    }
    function assertNegativeNumber(value, meta) {
        assertFiniteNumber(value, meta);
        if (value >= 0) {
            throw new LitsError("Expected negative number, got " + value, meta);
        }
    }
    function assertNonNegativeNumber(value, meta) {
        assertFiniteNumber(value, meta);
        if (value < 0) {
            throw new LitsError("Expected non negative number, got " + value, meta);
        }
    }
    function assertNonNegativeInteger(value, meta) {
        assertNonNegativeNumber(value, meta);
        assertInteger(value, meta);
    }
    function assertInteger(value, meta) {
        assertFiniteNumber(value, meta);
        if (!Number.isInteger(value)) {
            throw new LitsError("Expected integer, got " + value, meta);
        }
    }
    function assertNumberGte(value, x, meta) {
        assertFiniteNumber(value, meta);
        if (value < x) {
            throw new LitsError("Expected parameter (" + value + ") to be a number equal or grater than " + x, meta);
        }
    }
    function assertNumberLte(value, x, meta) {
        assertFiniteNumber(value, meta);
        if (value > x) {
            throw new LitsError("Expected parameter (" + value + ") to be a number equal or less than " + x, meta);
        }
    }
    function isString(value) {
        return typeof value === "string";
    }
    function assertString(value, meta) {
        if (!isString(value)) {
            throw new LitsError("Expected string, got: " + value + " type=\"" + typeof value + "\"", meta);
        }
    }
    function assertStringOrRegExp(value, meta) {
        if (!(value instanceof RegExp || typeof value === "string")) {
            throw new LitsError("Expected RegExp or string, got: " + value + " type=\"" + typeof value + "\"", meta);
        }
    }
    function assertNonEmptyString(value, meta) {
        assertString(value, meta);
        if (value.length === 0) {
            throw new LitsError("Expected non empty string, got: " + value + " type=\"" + typeof value + "\"", meta);
        }
    }
    function isChar(value) {
        return isString(value) && value.length === 1;
    }
    function assertChar(value, meta) {
        if (!isChar(value)) {
            throw new LitsError("Expected char, got: " + value + " type=\"" + typeof value + "\"", meta);
        }
    }
    function asChar(value, meta) {
        assertChar(value, meta);
        return value;
    }
    function isStringOrNumber(value) {
        return typeof value === "string" || typeof value === "number";
    }
    function assertStringOrNumber(value, meta) {
        if (!isStringOrNumber(value)) {
            throw new LitsError("Expected string or number, got: " + value + " type=\"" + typeof value + "\"", meta);
        }
    }
    function asStringOrNumber(value, meta) {
        assertStringOrNumber(value, meta);
        return value;
    }
    function asNonEmptyString(value, meta) {
        if (typeof value !== "string" || value.length === 0) {
            throw new LitsError("Expected non empty string, got: " + value + " type=\"" + typeof value + "\"", meta);
        }
        return value;
    }
    function isRegExp(value) {
        return value instanceof RegExp;
    }
    function assertRegExp(value, meta) {
        if (!(value instanceof RegExp)) {
            throw new LitsError("Expected RegExp, got: " + value + " type=\"" + typeof value + "\"", meta);
        }
    }
    function assertNumberNotZero(value, meta) {
        assertFiniteNumber(value, meta);
        if (value === 0) {
            throw new LitsError("Expected non zero value", meta);
        }
    }
    function assertLength(count, node) {
        var length = node.params.length;
        if (typeof count === "number") {
            if (length !== count) {
                throw new LitsError("Wrong number of arguments to \"" + node.name + "\", expected " + count + ", got " + length, node.token.meta);
            }
        }
        else {
            var min = count.min, max = count.max;
            if (min === undefined && max === undefined) {
                throw new LitsError("Min or max must be specified", node.token.meta);
            }
            if (typeof min === "number" && length < min) {
                throw new LitsError("Wrong number of arguments to \"" + node.name + "\", expected at least " + min + ", got " + length, node.token.meta);
            }
            if (typeof max === "number" && length > max) {
                throw new LitsError("Wrong number of arguments to \"" + node.name + "\", expected at most " + max + ", got " + length, node.token.meta);
            }
        }
    }
    function assertLengthEven(node) {
        var length = node.params.length;
        if (length % 2 !== 0) {
            throw new LitsError("Wrong number of arguments, expected an even number, got " + length, node.token.meta);
        }
    }
    function isLitsFunction(func) {
        if (func === null || typeof func !== "object") {
            return false;
        }
        return !!func[FUNCTION_SYMBOL];
    }
    function assertLitsFunction(func, meta) {
        if (!isLitsFunction(func)) {
            throw new LitsError("Expected lits function, got " + JSON.stringify(func), meta);
        }
    }
    function assertStringArray(value, meta) {
        if (!Array.isArray(value) || value.some(function (v) { return typeof v !== "string"; })) {
            throw new LitsError("Expected an array of strings, got " + value, meta);
        }
    }
    function assertCharArray(arr, meta) {
        if (!Array.isArray(arr) || arr.some(function (v) { return typeof v !== "string" || v.length !== 1; })) {
            throw new LitsError("Expected an array of chars, got " + arr, meta);
        }
    }
    function isExpressionNode(node) {
        return (node.type === "NormalExpression" ||
            node.type === "SpecialExpression" ||
            node.type === "Number" ||
            node.type === "String");
    }
    function assertNumber(value, meta) {
        if (!isNumber(value)) {
            throw new LitsError("Expected a number, got: " + value + " type=\"" + typeof value + "\"", meta);
        }
    }
    function asNumber(value, meta) {
        assertNumber(value, meta);
        return value;
    }
    function assertArr(value, meta) {
        if (!isArr(value)) {
            throw new LitsError("Expected Arr, got: " + value + " type=\"" + typeof value + "\"", meta);
        }
    }
    function asArr(value, meta) {
        assertArr(value, meta);
        return value;
    }
    function isAny(value) {
        return value !== undefined;
    }
    function assertAny(value, meta) {
        if (!isAny(value)) {
            throw new LitsError("Expected Any, got: " + value + " type=\"" + typeof value + "\"", meta);
        }
    }
    function isSeq(value) {
        return Array.isArray(value) || isString(value);
    }
    function assertSeq(value, meta) {
        if (!isSeq(value)) {
            throw new LitsError("Expected string or array, got: " + value + " type=\"" + typeof value + "\"", meta);
        }
    }
    function asSeq(value, meta) {
        if (!isSeq(value)) {
            throw new LitsError("Expected string or array, got: " + value + " type=\"" + typeof value + "\"", meta);
        }
        return value;
    }
    function assertObj(value, meta) {
        if (!isObj(value)) {
            throw new LitsError("Expected object, got: " + value + " type=\"" + typeof value + "\"", meta);
        }
    }
    function isObj(value) {
        return !(value === null ||
            typeof value !== "object" ||
            Array.isArray(value) ||
            value instanceof RegExp ||
            isLitsFunction(value));
    }
    function isArr(value) {
        return Array.isArray(value);
    }
    function isColl(value) {
        return isSeq(value) || isObj(value);
    }
    function assertColl(value, meta) {
        if (!isColl(value)) {
            throw new LitsError("Expected collection, got: " + value + " type=\"" + typeof value + "\"", meta);
        }
    }
    function asColl(value, meta) {
        if (!isColl(value)) {
            throw new LitsError("Expected collection, got: " + value + " type=\"" + typeof value + "\"", meta);
        }
        return value;
    }
    function isNumber(value) {
        return typeof value === "number";
    }
    function isInteger(value) {
        return Number.isInteger(value);
    }
    function collHasKey(coll, key) {
        if (!isColl(coll)) {
            return false;
        }
        if (isString(coll) || isArr(coll)) {
            if (!isInteger(key)) {
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
        else if (isArr(value)) {
            return "array";
        }
        else if (isObj(value)) {
            return "object";
        }
        else if (isRegExp(value)) {
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
    function isNormalExpressionNodeName(node) {
        return typeof node.name === "string";
    }
    function deepEqual(a, b, meta) {
        if (a === b) {
            return true;
        }
        if (typeof a === "number" && typeof b === "number") {
            return Math.abs(a - b) < Number.EPSILON;
        }
        if (isArr(a) && isArr(b)) {
            if (a.length !== b.length) {
                return false;
            }
            for (var i = 0; i < a.length; i += 1) {
                if (!deepEqual(asAny(a[i], meta), asAny(b[i], meta), meta)) {
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
                var key = asNotUndefined(aKeys[i], meta);
                if (!deepEqual(toAny(aObj[key]), toAny(bObj[key]), meta)) {
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
    function assertMax(value, maxNumber, meta) {
        if (value > maxNumber) {
            throw new LitsError("Expected number less than or equal to " + maxNumber + "'", meta);
        }
    }
    function toAny(value) {
        return (value !== null && value !== void 0 ? value : null);
    }
    function clone(value) {
        if (isObj(value)) {
            return Object.entries(value).reduce(function (result, entry) {
                var key = entry[0], val = entry[1];
                result[key] = clone(val);
                return result;
            }, {});
        }
        if (isArr(value)) {
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

    var andSpecialExpression = {
        parse: function (tokens, position, _a) {
            var parseTokens = _a.parseTokens;
            var firstToken = asNotUndefined(tokens[position]);
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
        var token = asNotUndefined(tokens[position]);
        while (!(token.type === "paren" && token.value === ")")) {
            var test_1 = void 0;
            _a = parseToken(tokens, position), position = _a[0], test_1 = _a[1];
            var form = void 0;
            _b = parseToken(tokens, position), position = _b[0], form = _b[1];
            conditions.push({ test: test_1, form: form });
            token = asNotUndefined(tokens[position]);
        }
        return [position, conditions];
    }
    var condSpecialExpression = {
        parse: function (tokens, position, _a) {
            var _b;
            var parseToken = _a.parseToken;
            var firstToken = asNotUndefined(tokens[position]);
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

    function assertNameNotDefined(name, contextStack, builtin, meta) {
        if (typeof name !== "string") {
            return;
        }
        if (builtin.specialExpressions[name]) {
            throw new LitsError("Cannot define variable " + name + ", it's a special expression", meta);
        }
        if (builtin.normalExpressions[name]) {
            throw new LitsError("Cannot define variable " + name + ", it's a builtin function", meta);
        }
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        if (reservedNamesRecord[name]) {
            throw new LitsError("Cannot define variable " + name + ", it's a reserved name", meta);
        }
        if (contextStack.globalContext[name]) {
            throw new LitsError("Name already defined \"" + name + "\"", meta);
        }
    }

    function createParser(expressionName) {
        return function (tokens, position, parsers) {
            var _a, _b;
            var firstToken = asNotUndefined(tokens[position]);
            var parseToken = parsers.parseToken;
            var functionName = undefined;
            if (expressionName === "defn" || expressionName === "defns") {
                _a = parseToken(tokens, position), position = _a[0], functionName = _a[1];
                if (expressionName === "defn" && functionName.type !== "Name") {
                    throw new UnexpectedNodeTypeError("Name", functionName, functionName.token.meta);
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
        var meta = node.token.meta;
        if (expressionName === "defn") {
            var name_1 = node.functionName.value;
            assertString(name_1, meta);
            return name_1;
        }
        if (expressionName === "defns") {
            var name_2 = evaluateAstNode(node.functionName, contextStack);
            assertString(name_2, meta);
            return name_2;
        }
        return undefined;
    }
    function createEvaluator(expressionName) {
        return function (node, contextStack, _a) {
            var _b;
            var evaluateAstNode = _a.evaluateAstNode, builtin = _a.builtin;
            var name = getFunctionName(expressionName, node, contextStack, evaluateAstNode);
            assertNameNotDefined(name, contextStack, builtin, node.token.meta);
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
        var parseToken = _a.parseToken;
        var token = asNotUndefined(tokens[position]);
        var body = [];
        while (!(token.type === "paren" && (token.value === ")" || token.value === "]"))) {
            var _b = parseToken(tokens, position), newPosition = _b[0], bodyNode = _b[1];
            body.push(bodyNode);
            position = newPosition;
            token = asNotUndefined(tokens[position]);
        }
        if (body.length === 0) {
            throw new LitsError("Missing body in function", token.meta);
        }
        position += 1;
        return [position, body];
    }
    function parseFunctionOverloades(tokens, position, parsers) {
        var _a, _b, _c, _d;
        var token = asNotUndefined(tokens[position]);
        if (token.type === "paren" && token.value === "(") {
            var functionOverloades = [];
            while (!(token.type === "paren" && token.value === ")")) {
                position += 1;
                token = asNotUndefined(tokens[position]);
                var functionArguments = void 0;
                _a = parseFunctionArguments(tokens, position, parsers), position = _a[0], functionArguments = _a[1];
                var arity = functionArguments.restArgument
                    ? { min: functionArguments.mandatoryArguments.length }
                    : functionArguments.mandatoryArguments.length;
                if (!arityOk(functionOverloades, arity)) {
                    throw new LitsError("All overloaded functions must have different arity", token.meta);
                }
                var functionBody = void 0;
                _b = parseFunctionBody(tokens, position, parsers), position = _b[0], functionBody = _b[1];
                functionOverloades.push({
                    arguments: functionArguments,
                    body: functionBody,
                    arity: arity,
                });
                token = asNotUndefined(tokens[position]);
            }
            return [position + 1, functionOverloades];
        }
        else if (token.type === "paren" && token.value === "[") {
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
            throw new UnexpectedTokenError("[ or (", token);
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
        var token = asNotUndefined(tokens[position]);
        position += 1;
        token = asNotUndefined(tokens[position]);
        while (!(token.type === "paren" && token.value === "]")) {
            if (state === "let") {
                _a = parseBindings(tokens, position), position = _a[0], bindings = _a[1];
                break;
            }
            else {
                var _b = parseArgument(tokens, position), newPosition = _b[0], node = _b[1];
                position = newPosition;
                token = asNotUndefined(tokens[position]);
                if (node.type === "Modifier") {
                    switch (node.value) {
                        case "&":
                            if (state === "rest") {
                                throw new LitsError("& can only appear once", token.meta);
                            }
                            state = "rest";
                            break;
                        case "&let":
                            if (state === "rest" && !restArgument) {
                                throw new LitsError("No rest argument was spcified", token.meta);
                            }
                            state = "let";
                            break;
                        default:
                            throw new LitsError("Illegal modifier: " + node.value, token.meta);
                    }
                }
                else {
                    if (argNames[node.name]) {
                        throw new LitsError("Duplicate argument \"" + node.name + "\"", token.meta);
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
                                throw new LitsError("Can only specify one rest argument", token.meta);
                            }
                            restArgument = node.name;
                            break;
                    }
                }
            }
        }
        if (state === "rest" && restArgument === undefined) {
            throw new LitsError("Missing rest argument name", token.meta);
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
            var firstToken = asNotUndefined(tokens[position]);
            var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
            assertNameNode(params[0], firstToken.meta);
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
            var meta = node.token.meta;
            var name = asNameNode(node.params[0], meta).value;
            assertNameNotDefined(name, contextStack, builtin, meta);
            var value = evaluateAstNode(asAstNode(node.params[1], meta), contextStack);
            contextStack.globalContext[name] = { value: value };
            return value;
        },
        validate: function (node) { return assertLength(2, node); },
    };

    var defsSpecialExpression = {
        parse: function (tokens, position, _a) {
            var parseTokens = _a.parseTokens;
            var firstToken = asNotUndefined(tokens[position]);
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
            var meta = node.token.meta;
            var name = evaluateAstNode(asAstNode(node.params[0], meta), contextStack);
            assertString(name, meta);
            assertNameNotDefined(name, contextStack, builtin, node.token.meta);
            var value = evaluateAstNode(asAstNode(node.params[1], meta), contextStack);
            contextStack.globalContext[name] = { value: value };
            return value;
        },
        validate: function (node) { return assertLength(2, node); },
    };

    var doSpecialExpression = {
        parse: function (tokens, position, _a) {
            var parseToken = _a.parseToken;
            var node = {
                type: "SpecialExpression",
                name: "do",
                params: [],
                token: asNotUndefined(tokens[position]),
            };
            var token = asNotUndefined(tokens[position]);
            while (!(token.type === "paren" && token.value === ")")) {
                var _b = parseToken(tokens, position), newPosition = _b[0], bodyNode = _b[1];
                node.params.push(bodyNode);
                position = newPosition;
                token = asNotUndefined(tokens[position]);
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
        var token = asNotUndefined(tokens[position]);
        while (token.type === "modifier") {
            switch (token.value) {
                case "&let":
                    if (loopBinding.letBindings) {
                        throw new LitsError("Only one &let modifier allowed", token.meta);
                    }
                    _c = parseBindings(tokens, position + 1), position = _c[0], loopBinding.letBindings = _c[1];
                    loopBinding.modifiers.push("&let");
                    break;
                case "&when":
                    if (loopBinding.whenNode) {
                        throw new LitsError("Only one &when modifier allowed", token.meta);
                    }
                    _d = parseToken(tokens, position + 1), position = _d[0], loopBinding.whenNode = _d[1];
                    loopBinding.modifiers.push("&when");
                    break;
                case "&while":
                    if (loopBinding.whileNode) {
                        throw new LitsError("Only one &while modifier allowed", token.meta);
                    }
                    _e = parseToken(tokens, position + 1), position = _e[0], loopBinding.whileNode = _e[1];
                    loopBinding.modifiers.push("&while");
                    break;
                default:
                    throw new LitsError("Illegal modifier: " + token.value, token.meta);
            }
            token = asNotUndefined(tokens[position]);
        }
        return [position, loopBinding];
    }
    function addToContext(bindings, context, contextStack, evaluateAstNode, meta) {
        for (var _i = 0, bindings_1 = bindings; _i < bindings_1.length; _i++) {
            var binding = bindings_1[_i];
            if (context[binding.name]) {
                throw new LitsError("Variable already defined: " + binding.name, meta);
            }
            context[binding.name] = { value: evaluateAstNode(binding.value, contextStack) };
        }
    }
    function parseLoopBindings(tokens, position, parsers) {
        var _a;
        var token = asNotUndefined(tokens[position]);
        if (!(token.type === "paren" && token.value === "[")) {
            throw new UnexpectedTokenError("[", token);
        }
        position += 1;
        var loopBindings = [];
        token = asNotUndefined(tokens[position]);
        while (!(token.type === "paren" && token.value === "]")) {
            var loopBinding = void 0;
            _a = parseLoopBinding(tokens, position, parsers), position = _a[0], loopBinding = _a[1];
            loopBindings.push(loopBinding);
            token = asNotUndefined(tokens[position]);
        }
        return [position + 1, loopBindings];
    }
    var forSpecialExpression = {
        parse: function (tokens, position, parsers) {
            var _a, _b;
            var firstToken = asNotUndefined(tokens[position]);
            var parseToken = parsers.parseToken;
            var loopBindings;
            _a = parseLoopBindings(tokens, position, parsers), position = _a[0], loopBindings = _a[1];
            var expression;
            _b = parseToken(tokens, position), position = _b[0], expression = _b[1];
            var token = asNotUndefined(tokens[position]);
            if (!(token.type === "paren" && token.value === ")")) {
                throw new UnexpectedTokenError(")", token);
            }
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
            var meta = node.token.meta;
            var loopBindings = node.loopBindings, params = node.params;
            var expression = asNotUndefined(params[0]);
            var result = [];
            var bindingIndices = loopBindings.map(function () { return 0; });
            var abort = false;
            while (!abort) {
                var context = {};
                var newContextStack = contextStack.withContext(context);
                var skip = false;
                bindingsLoop: for (var bindingIndex = 0; bindingIndex < loopBindings.length; bindingIndex += 1) {
                    var _b = asNotUndefined(loopBindings[bindingIndex]), binding = _b.binding, letBindings = _b.letBindings, whenNode = _b.whenNode, whileNode = _b.whileNode, modifiers = _b.modifiers;
                    var coll = asColl(evaluateAstNode(binding.value, newContextStack), meta);
                    var seq = isSeq(coll) ? coll : Object.entries(coll);
                    if (seq.length === 0) {
                        skip = true;
                        abort = true;
                        break;
                    }
                    var index = asNotUndefined(bindingIndices[bindingIndex]);
                    if (index >= seq.length) {
                        skip = true;
                        if (bindingIndex === 0) {
                            abort = true;
                            break;
                        }
                        bindingIndices[bindingIndex] = 0;
                        bindingIndices[bindingIndex - 1] = asNotUndefined(bindingIndices[bindingIndex - 1]) + 1;
                        break;
                    }
                    if (context[binding.name]) {
                        throw new LitsError("Variable already defined: " + binding.name, meta);
                    }
                    context[binding.name] = {
                        value: asAny(seq[index], meta),
                    };
                    for (var _i = 0, modifiers_1 = modifiers; _i < modifiers_1.length; _i++) {
                        var modifier = modifiers_1[_i];
                        switch (modifier) {
                            case "&let":
                                addToContext(asNotUndefined(letBindings), context, newContextStack, evaluateAstNode, meta);
                                break;
                            case "&when":
                                if (!evaluateAstNode(asNotUndefined(whenNode), newContextStack)) {
                                    bindingIndices[bindingIndex] = asNotUndefined(bindingIndices[bindingIndex]) + 1;
                                    skip = true;
                                    break bindingsLoop;
                                }
                                break;
                            case "&while":
                                if (!evaluateAstNode(asNotUndefined(whileNode), newContextStack)) {
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
            var firstToken = asNotUndefined(tokens[position]);
            var bindings;
            _b = parseBindings(tokens, position), position = _b[0], bindings = _b[1];
            if (bindings.length !== 1) {
                throw new LitsError("Expected exactly one binding, got " + bindings.length, firstToken.meta);
            }
            var params;
            _c = parseTokens(tokens, position), position = _c[0], params = _c[1];
            var node = {
                type: "SpecialExpression",
                name: "if-let",
                binding: asNotUndefined(bindings[0]),
                params: params,
                token: firstToken,
            };
            return [position + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var evaluateAstNode = _a.evaluateAstNode;
            var locals = {};
            var bindingValue = evaluateAstNode(node.binding.value, contextStack);
            if (bindingValue) {
                locals[node.binding.name] = { value: bindingValue };
                var newContextStack = contextStack.withContext(locals);
                var thenForm = asNotUndefined(node.params[0]);
                return evaluateAstNode(thenForm, newContextStack);
            }
            if (node.params.length === 2) {
                var elseForm = asNotUndefined(node.params[1]);
                return evaluateAstNode(elseForm, contextStack);
            }
            return null;
        },
        validate: function (node) { return assertLength({ min: 1, max: 2 }, node); },
    };

    var ifNotSpecialExpression = {
        parse: function (tokens, position, _a) {
            var parseTokens = _a.parseTokens;
            var firstToken = asNotUndefined(tokens[position]);
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
            var meta = node.token.meta;
            var _b = node.params, conditionNode = _b[0], trueNode = _b[1], falseNode = _b[2];
            if (!evaluateAstNode(asAstNode(conditionNode, meta), contextStack)) {
                return evaluateAstNode(asAstNode(trueNode, meta), contextStack);
            }
            else {
                if (node.params.length === 3) {
                    return evaluateAstNode(asAstNode(falseNode, meta), contextStack);
                }
                else {
                    return null;
                }
            }
        },
        validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
    };

    var ifSpecialExpression = {
        parse: function (tokens, position, _a) {
            var parseTokens = _a.parseTokens;
            var firstToken = asNotUndefined(tokens[position]);
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
            var meta = node.token.meta;
            var _b = node.params, conditionNode = _b[0], trueNode = _b[1], falseNode = _b[2];
            if (evaluateAstNode(asAstNode(conditionNode, meta), contextStack)) {
                return evaluateAstNode(asAstNode(trueNode, meta), contextStack);
            }
            else {
                if (node.params.length === 3) {
                    return evaluateAstNode(asAstNode(falseNode, meta), contextStack);
                }
                else {
                    return null;
                }
            }
        },
        validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
    };

    var letSpecialExpression = {
        parse: function (tokens, position, _a) {
            var _b, _c;
            var parseBindings = _a.parseBindings, parseTokens = _a.parseTokens;
            var firstToken = asNotUndefined(tokens[position]);
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
            var firstToken = asNotUndefined(tokens[position]);
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
            var meta = node.token.meta;
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
                            throw new LitsError("recur expected " + node.bindings.length + " parameters, got " + params_1.length, meta);
                        }
                        node.bindings.forEach(function (binding, index) {
                            asNotUndefined(bindingContext[binding.name]).value = asAny(params_1[index], meta);
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
            var firstToken = asNotUndefined(tokens[position]);
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
            var firstToken = asNotUndefined(tokens[position]);
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
            var firstToken = asNotUndefined(tokens[position]);
            var _b = parseToken(tokens, position), newPosition = _b[0], messageNode = _b[1];
            position = newPosition;
            var token = asNotUndefined(tokens[position]);
            if (!(token.type === "paren" && token.value === ")")) {
                throw new UnexpectedTokenError(")", token);
            }
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
            var message = asNonEmptyString(evaluateAstNode(node.messageNode, contextStack), node.token.meta);
            throw new UserDefinedError(message, node.token.meta);
        },
    };

    var timeSpecialExpression = {
        parse: function (tokens, position, _a) {
            var parseToken = _a.parseToken;
            var firstToken = asNotUndefined(tokens[position]);
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
            var astNode = node.params[0];
            assertNotUndefined(astNode);
            var startTime = Date.now();
            var result = evaluateAstNode(astNode, contextStack);
            var totalTime = Date.now() - startTime;
            // eslint-disable-next-line no-console
            console.log("Elapsed time: " + totalTime + " ms");
            return result;
        },
        validate: function (node) { return assertLength(1, node); },
    };

    var trySpecialExpression = {
        parse: function (tokens, position, _a) {
            var _b, _c, _d;
            var parseToken = _a.parseToken;
            var firstToken = asNotUndefined(tokens[position]);
            var tryExpression;
            _b = parseToken(tokens, position), position = _b[0], tryExpression = _b[1];
            var token = asNotUndefined(tokens[position]);
            if (!(token.type === "paren" && token.value === "(")) {
                throw new UnexpectedTokenError("(", token);
            }
            position += 1;
            token = asNotUndefined(tokens[position]);
            if (!(token.type === "paren" && token.value === "(")) {
                throw new UnexpectedTokenError("(", token);
            }
            position += 1;
            var error;
            _c = parseToken(tokens, position), position = _c[0], error = _c[1];
            if (error.type !== "Name") {
                throw new UnexpectedNodeTypeError("Name", error, error.token.meta);
            }
            token = asNotUndefined(tokens[position]);
            if (!(token.type === "paren" && token.value === ")")) {
                throw new UnexpectedTokenError(")", token);
            }
            position += 1;
            var catchExpression;
            _d = parseToken(tokens, position), position = _d[0], catchExpression = _d[1];
            token = asNotUndefined(tokens[position]);
            if (!(token.type === "paren" && token.value === ")")) {
                throw new UnexpectedTokenError(")", token);
            }
            position += 1;
            token = asNotUndefined(tokens[position]);
            if (!(token.type === "paren" && token.value === ")")) {
                throw new UnexpectedTokenError(")", token);
            }
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
                var newContext = (_b = {}, _b[node.error.value] = { value: asNotUndefined(error) }, _b);
                return evaluateAstNode(node.catchExpression, contextStack.withContext(newContext));
            }
        },
    };

    var whenFirstSpecialExpression = {
        parse: function (tokens, position, _a) {
            var _b, _c;
            var parseBindings = _a.parseBindings, parseTokens = _a.parseTokens;
            var firstToken = asNotUndefined(tokens[position]);
            var bindings;
            _b = parseBindings(tokens, position), position = _b[0], bindings = _b[1];
            if (bindings.length !== 1) {
                throw new LitsError("Expected exactly one binding, got " + bindings.length, firstToken.meta);
            }
            var params;
            _c = parseTokens(tokens, position), position = _c[0], params = _c[1];
            var node = {
                type: "SpecialExpression",
                name: "when-first",
                binding: asNotUndefined(bindings[0]),
                params: params,
                token: firstToken,
            };
            return [position + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var evaluateAstNode = _a.evaluateAstNode;
            var locals = {};
            var evaluatedBindingForm = evaluateAstNode(node.binding.value, contextStack);
            if (!isSeq(evaluatedBindingForm)) {
                throw new LitsError("Expected undefined or a sequence, got " + evaluatedBindingForm, node.token.meta);
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
        validate: function (node) { return assertLength({ min: 0 }, node); },
    };

    var whenLetSpecialExpression = {
        parse: function (tokens, position, _a) {
            var _b, _c;
            var parseBindings = _a.parseBindings, parseTokens = _a.parseTokens;
            var firstToken = asNotUndefined(tokens[position]);
            var bindings;
            _b = parseBindings(tokens, position), position = _b[0], bindings = _b[1];
            if (bindings.length !== 1) {
                throw new LitsError("Expected exactly one binding, got " + bindings.length, firstToken.meta);
            }
            var params;
            _c = parseTokens(tokens, position), position = _c[0], params = _c[1];
            var node = {
                type: "SpecialExpression",
                name: "when-let",
                binding: asNotUndefined(bindings[0]),
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
        validate: function (node) { return assertLength({ min: 0 }, node); },
    };

    var whenNotSpecialExpression = {
        parse: function (tokens, position, _a) {
            var parseTokens = _a.parseTokens;
            var firstToken = asNotUndefined(tokens[position]);
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
            assertNotUndefined(whenExpression);
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
        validate: function (node) { return assertLength({ min: 1 }, node); },
    };

    var whenSpecialExpression = {
        parse: function (tokens, position, _a) {
            var parseTokens = _a.parseTokens;
            var firstToken = asNotUndefined(tokens[position]);
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
            assertNotUndefined(whenExpression);
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
        validate: function (node) { return assertLength({ min: 1 }, node); },
    };

    var bitwiseNormalExpression = {
        'bit-shift-left': {
            evaluate: function (_a, meta) {
                var number = _a[0], count = _a[1];
                assertInteger(number, meta);
                assertNonNegativeInteger(count, meta);
                return number << count;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'bit-shift-right': {
            evaluate: function (_a, meta) {
                var number = _a[0], count = _a[1];
                assertInteger(number, meta);
                assertNonNegativeInteger(count, meta);
                return number >> count;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'bit-not': {
            evaluate: function (_a, meta) {
                var number = _a[0];
                assertInteger(number, meta);
                return ~number;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'bit-and': {
            evaluate: function (_a, meta) {
                var first = _a[0], rest = _a.slice(1);
                assertInteger(first, meta);
                return rest.reduce(function (result, value) {
                    assertInteger(value, meta);
                    return result & value;
                }, first);
            },
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
        'bit-and-not': {
            evaluate: function (_a, meta) {
                var first = _a[0], rest = _a.slice(1);
                assertInteger(first, meta);
                return rest.reduce(function (result, value) {
                    assertInteger(value, meta);
                    return result & ~value;
                }, first);
            },
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
        'bit-or': {
            evaluate: function (_a, meta) {
                var first = _a[0], rest = _a.slice(1);
                assertInteger(first, meta);
                return rest.reduce(function (result, value) {
                    assertInteger(value, meta);
                    return result | value;
                }, first);
            },
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
        'bit-xor': {
            evaluate: function (_a, meta) {
                var first = _a[0], rest = _a.slice(1);
                assertInteger(first, meta);
                return rest.reduce(function (result, value) {
                    assertInteger(value, meta);
                    return result ^ value;
                }, first);
            },
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
        'bit-flip': {
            evaluate: function (_a, meta) {
                var number = _a[0], index = _a[1];
                assertInteger(number, meta);
                assertNonNegativeInteger(index, meta);
                var mask = 1 << index;
                return (number ^= mask);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'bit-set': {
            evaluate: function (_a, meta) {
                var number = _a[0], index = _a[1];
                assertInteger(number, meta);
                assertNonNegativeInteger(index, meta);
                var mask = 1 << index;
                return (number |= mask);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'bit-clear': {
            evaluate: function (_a, meta) {
                var number = _a[0], index = _a[1];
                assertInteger(number, meta);
                assertNonNegativeInteger(index, meta);
                var mask = 1 << index;
                return (number &= ~mask);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'bit-test': {
            evaluate: function (_a, meta) {
                var number = _a[0], index = _a[1];
                assertInteger(number, meta);
                assertNonNegativeInteger(index, meta);
                var mask = 1 << index;
                return !!(number & mask);
            },
            validate: function (node) { return assertLength(2, node); },
        },
    };

    function cloneAndGetMeta(originalColl, keys, meta) {
        var coll = cloneColl(originalColl);
        var butLastKeys = keys.slice(0, keys.length - 1);
        var innerCollMeta = butLastKeys.reduce(function (result, key) {
            var resultColl = result.coll;
            var newResultColl;
            if (isArr(resultColl)) {
                assertNumber(key, meta);
                // eslint-disable-next-line @typescript-eslint/no-explicit-any
                newResultColl = asColl(resultColl[key], meta);
            }
            else {
                assertObj(resultColl, meta);
                assertString(key, meta);
                if (!collHasKey(result.coll, key)) {
                    resultColl[key] = {};
                }
                newResultColl = asColl(resultColl[key], meta);
            }
            return { coll: newResultColl, parent: resultColl };
        }, { coll: coll, parent: {} });
        return { coll: coll, innerCollMeta: innerCollMeta };
    }
    function get(coll, key, meta) {
        if (isArr(coll)) {
            assertInteger(key, meta);
            if (key < coll.length) {
                return toAny(coll[key]);
            }
        }
        else if (isObj(coll)) {
            assertString(key, meta);
            if (collHasKey(coll, key)) {
                return toAny(coll[key]);
            }
        }
        else {
            assertInteger(key, meta);
            if (key < coll.length) {
                return toAny(coll[key]);
            }
        }
        return undefined;
    }
    function update(coll, key, fn, params, meta, contextStack, executeFunction) {
        if (isObj(coll)) {
            assertString(key, meta);
            var result = __assign({}, coll);
            result[key] = executeFunction(fn, __spreadArray([result[key]], params), meta, contextStack);
            return result;
        }
        else {
            assertNumber(key, meta);
            var intKey_1 = toNonNegativeInteger(key);
            assertMax(intKey_1, coll.length, meta);
            if (Array.isArray(coll)) {
                var result = coll.map(function (elem, index) {
                    if (intKey_1 === index) {
                        return executeFunction(fn, __spreadArray([elem], params), meta, contextStack);
                    }
                    return elem;
                });
                if (intKey_1 === coll.length) {
                    result[intKey_1] = executeFunction(fn, __spreadArray([undefined], params), meta, contextStack);
                }
                return result;
            }
            else {
                var result = coll.split("").map(function (elem, index) {
                    if (intKey_1 === index) {
                        return asChar(executeFunction(fn, __spreadArray([elem], params), meta, contextStack), meta);
                    }
                    return elem;
                });
                if (intKey_1 === coll.length) {
                    result[intKey_1] = asChar(executeFunction(fn, __spreadArray([undefined], params), meta, contextStack), meta);
                }
                return result.join("");
            }
        }
    }
    function assoc(coll, key, value, meta) {
        assertColl(coll, meta);
        assertStringOrNumber(key, meta);
        if (Array.isArray(coll) || typeof coll === "string") {
            assertInteger(key, meta);
            assertNumberGte(key, 0, meta);
            assertNumberLte(key, coll.length, meta);
            if (typeof coll === "string") {
                assertChar(value, meta);
                return "" + coll.slice(0, key) + value + coll.slice(key + 1);
            }
            var copy_1 = __spreadArray([], coll);
            copy_1[key] = value;
            return copy_1;
        }
        assertString(key, meta);
        var copy = __assign({}, coll);
        copy[key] = value;
        return copy;
    }
    var collectionNormalExpression = {
        get: {
            evaluate: function (params, meta) {
                var coll = params[0], key = params[1];
                var defaultValue = toAny(params[2]);
                assertColl(coll, meta);
                assertStringOrNumber(key, meta);
                var result = get(coll, key, meta);
                return result === undefined ? defaultValue : result;
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        'get-in': {
            evaluate: function (params, meta) {
                var coll = params[0];
                var keys = params[1];
                var defaultValue = toAny(params[2]);
                assertColl(coll, meta);
                assertArr(keys, meta);
                for (var _i = 0, keys_1 = keys; _i < keys_1.length; _i++) {
                    var key = keys_1[_i];
                    assertStringOrNumber(key, meta);
                    if (isColl(coll)) {
                        coll = get(coll, key, meta);
                    }
                    else {
                        return defaultValue;
                    }
                }
                return isAny(coll) ? coll : defaultValue;
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        count: {
            evaluate: function (_a, meta) {
                var coll = _a[0];
                if (typeof coll === "string") {
                    return coll.length;
                }
                assertColl(coll, meta);
                if (Array.isArray(coll)) {
                    return coll.length;
                }
                return Object.keys(coll).length;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'contains?': {
            evaluate: function (_a, meta) {
                var coll = _a[0], key = _a[1];
                assertColl(coll, meta);
                assertStringOrNumber(key, meta);
                if (isSeq(coll)) {
                    if (!Number.isInteger(key)) {
                        return false;
                    }
                    assertInteger(key, meta);
                    return key >= 0 && key < coll.length;
                }
                return !!Object.getOwnPropertyDescriptor(coll, key);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'has?': {
            evaluate: function (_a, meta) {
                var coll = _a[0], value = _a[1];
                assertColl(coll, meta);
                if (isArr(coll)) {
                    return coll.includes(value);
                }
                if (isString(coll)) {
                    return isString(value) ? coll.split("").includes(value) : false;
                }
                return Object.values(coll).includes(value);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'has-some?': {
            evaluate: function (_a, meta) {
                var coll = _a[0], seq = _a[1];
                assertColl(coll, meta);
                assertSeq(seq, meta);
                if (isArr(coll)) {
                    for (var _i = 0, seq_1 = seq; _i < seq_1.length; _i++) {
                        var value = seq_1[_i];
                        if (coll.includes(value)) {
                            return true;
                        }
                    }
                    return false;
                }
                if (isString(coll)) {
                    for (var _b = 0, seq_2 = seq; _b < seq_2.length; _b++) {
                        var value = seq_2[_b];
                        if (isChar(value) ? coll.split("").includes(value) : false) {
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
            validate: function (node) { return assertLength(2, node); },
        },
        'has-every?': {
            evaluate: function (_a, meta) {
                var coll = _a[0], seq = _a[1];
                assertColl(coll, meta);
                assertSeq(seq, meta);
                if (isArr(coll)) {
                    for (var _i = 0, seq_4 = seq; _i < seq_4.length; _i++) {
                        var value = seq_4[_i];
                        if (!coll.includes(value)) {
                            return false;
                        }
                    }
                    return true;
                }
                if (isString(coll)) {
                    for (var _b = 0, seq_5 = seq; _b < seq_5.length; _b++) {
                        var value = seq_5[_b];
                        if (!isChar(value) || !coll.split("").includes(value)) {
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
            validate: function (node) { return assertLength(2, node); },
        },
        assoc: {
            evaluate: function (_a, meta) {
                var coll = _a[0], key = _a[1], value = _a[2];
                assertColl(coll, meta);
                assertStringOrNumber(key, meta);
                assertAny(value, meta);
                return assoc(coll, key, value, meta);
            },
            validate: function (node) { return assertLength(3, node); },
        },
        'assoc-in': {
            evaluate: function (_a, meta) {
                var originalColl = _a[0], keys = _a[1], value = _a[2];
                assertColl(originalColl, meta);
                assertArr(keys, meta);
                assertAny(value, meta);
                if (keys.length === 1) {
                    assertStringOrNumber(keys[0], meta);
                    return assoc(originalColl, keys[0], value, meta);
                }
                var _b = cloneAndGetMeta(originalColl, keys, meta), coll = _b.coll, innerCollMeta = _b.innerCollMeta;
                var lastKey = asStringOrNumber(keys[keys.length - 1], meta);
                var parentKey = asStringOrNumber(keys[keys.length - 2], meta);
                if (isArr(innerCollMeta.parent)) {
                    assertNumber(parentKey, meta);
                    innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value, meta);
                }
                else {
                    assertString(parentKey, meta);
                    innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value, meta);
                }
                return coll;
            },
            validate: function (node) { return assertLength(3, node); },
        },
        update: {
            evaluate: function (_a, meta, contextStack, _b) {
                var coll = _a[0], key = _a[1], fn = _a[2], params = _a.slice(3);
                var executeFunction = _b.executeFunction;
                assertColl(coll, meta);
                assertStringOrNumber(key, meta);
                assertLitsFunction(fn, meta);
                return update(coll, key, fn, params, meta, contextStack, executeFunction);
            },
            validate: function (node) { return assertLength({ min: 3 }, node); },
        },
        'update-in': {
            evaluate: function (_a, meta, contextStack, _b) {
                var originalColl = _a[0], keys = _a[1], fn = _a[2], params = _a.slice(3);
                var executeFunction = _b.executeFunction;
                assertColl(originalColl, meta);
                assertArr(keys, meta);
                assertLitsFunction(fn, meta);
                if (keys.length === 1) {
                    assertStringOrNumber(keys[0], meta);
                    return update(originalColl, keys[0], fn, params, meta, contextStack, executeFunction);
                }
                var _c = cloneAndGetMeta(originalColl, keys, meta), coll = _c.coll, innerCollMeta = _c.innerCollMeta;
                var lastKey = asStringOrNumber(keys[keys.length - 1], meta);
                var parentKey = asStringOrNumber(keys[keys.length - 2], meta);
                if (isArr(innerCollMeta.parent)) {
                    assertNumber(parentKey, meta);
                    innerCollMeta.parent[parentKey] = update(innerCollMeta.coll, lastKey, fn, params, meta, contextStack, executeFunction);
                }
                else {
                    assertString(parentKey, meta);
                    innerCollMeta.parent[parentKey] = update(innerCollMeta.coll, lastKey, fn, params, meta, contextStack, executeFunction);
                }
                return coll;
            },
            validate: function (node) { return assertLength({ min: 3 }, node); },
        },
        concat: {
            evaluate: function (params, meta) {
                assertColl(params[0], meta);
                if (isArr(params[0])) {
                    return params.reduce(function (result, arr) {
                        assertArr(arr, meta);
                        return result.concat(arr);
                    }, []);
                }
                else if (isString(params[0])) {
                    return params.reduce(function (result, s) {
                        assertString(s, meta);
                        return "" + result + s;
                    }, "");
                }
                else {
                    return params.reduce(function (result, obj) {
                        assertObj(obj, meta);
                        return Object.assign(result, obj);
                    }, {});
                }
            },
            validate: function (node) { return assertLength({ min: 1 }, node); },
        },
        'empty?': {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertColl(first, meta);
                if (isString(first)) {
                    return first.length === 0;
                }
                if (Array.isArray(first)) {
                    return first.length === 0;
                }
                return Object.keys(first).length === 0;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'every?': {
            evaluate: function (_a, meta, contextStack, _b) {
                var fn = _a[0], coll = _a[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, meta);
                assertColl(coll, meta);
                if (Array.isArray(coll)) {
                    return coll.every(function (elem) { return executeFunction(fn, [elem], meta, contextStack); });
                }
                if (isString(coll)) {
                    return coll.split("").every(function (elem) { return executeFunction(fn, [elem], meta, contextStack); });
                }
                return Object.entries(coll).every(function (elem) { return executeFunction(fn, [elem], meta, contextStack); });
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'any?': {
            evaluate: function (_a, meta, contextStack, _b) {
                var fn = _a[0], coll = _a[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, meta);
                assertColl(coll, meta);
                if (Array.isArray(coll)) {
                    return coll.some(function (elem) { return executeFunction(fn, [elem], meta, contextStack); });
                }
                if (isString(coll)) {
                    return coll.split("").some(function (elem) { return executeFunction(fn, [elem], meta, contextStack); });
                }
                return Object.entries(coll).some(function (elem) { return executeFunction(fn, [elem], meta, contextStack); });
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'not-any?': {
            evaluate: function (_a, meta, contextStack, _b) {
                var fn = _a[0], coll = _a[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, meta);
                assertColl(coll, meta);
                if (Array.isArray(coll)) {
                    return !coll.some(function (elem) { return executeFunction(fn, [elem], meta, contextStack); });
                }
                if (isString(coll)) {
                    return !coll.split("").some(function (elem) { return executeFunction(fn, [elem], meta, contextStack); });
                }
                return !Object.entries(coll).some(function (elem) { return executeFunction(fn, [elem], meta, contextStack); });
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'not-every?': {
            evaluate: function (_a, meta, contextStack, _b) {
                var fn = _a[0], coll = _a[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, meta);
                assertColl(coll, meta);
                if (Array.isArray(coll)) {
                    return !coll.every(function (elem) { return executeFunction(fn, [elem], meta, contextStack); });
                }
                if (isString(coll)) {
                    return !coll.split("").every(function (elem) { return executeFunction(fn, [elem], meta, contextStack); });
                }
                return !Object.entries(coll).every(function (elem) { return executeFunction(fn, [elem], meta, contextStack); });
            },
            validate: function (node) { return assertLength(2, node); },
        },
    };

    var evaluateMap = function (params, meta, contextStack, _a) {
        var executeFunction = _a.executeFunction;
        var fn = params[0], firstList = params[1];
        assertLitsFunction(fn, meta);
        assertSeq(firstList, meta);
        var isStringSeq = isString(firstList);
        var length = firstList.length;
        if (params.length === 2) {
            if (isArr(firstList)) {
                return firstList.map(function (elem) { return executeFunction(fn, [elem], meta, contextStack); });
            }
            else {
                return firstList
                    .split("")
                    .map(function (elem) {
                    var newVal = executeFunction(fn, [elem], meta, contextStack);
                    assertChar(newVal, meta);
                    return newVal;
                })
                    .join("");
            }
        }
        else {
            params.slice(2).forEach(function (collParam) {
                if (isStringSeq) {
                    assertString(collParam, meta);
                }
                else {
                    assertArr(collParam, meta);
                }
                if (length !== collParam.length) {
                    throw new LitsError("All arguments to \"map\" must have the same length", meta);
                }
            });
            if (isStringSeq) {
                var result = "";
                var _loop_1 = function (i) {
                    var fnParams = params.slice(1).map(function (l) { return l[i]; });
                    var newValue = executeFunction(fn, fnParams, meta, contextStack);
                    assertChar(newValue, meta);
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
                    result.push(executeFunction(fn, fnParams, meta, contextStack));
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
            evaluate: function (_a, meta) {
                var elem = _a[0], seq = _a[1];
                assertAny(elem, meta);
                assertSeq(seq, meta);
                if (Array.isArray(seq)) {
                    return __spreadArray([elem], seq);
                }
                assertChar(elem, meta);
                return "" + elem + seq;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        nth: {
            evaluate: function (_a, meta) {
                var seq = _a[0], i = _a[1];
                assertSeq(seq, meta);
                assertInteger(i, meta);
                return toAny(seq[i]);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        filter: {
            evaluate: function (_a, meta, contextStack, _b) {
                var fn = _a[0], seq = _a[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, meta);
                assertSeq(seq, meta);
                if (Array.isArray(seq)) {
                    return seq.filter(function (elem) { return executeFunction(fn, [elem], meta, contextStack); });
                }
                return seq
                    .split("")
                    .filter(function (elem) { return executeFunction(fn, [elem], meta, contextStack); })
                    .join("");
            },
            validate: function (node) { return assertLength(2, node); },
        },
        first: {
            evaluate: function (_a, meta) {
                var array = _a[0];
                assertSeq(array, meta);
                return toAny(array[0]);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        last: {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertSeq(first, meta);
                return toAny(first[first.length - 1]);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        map: {
            evaluate: evaluateMap,
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
        pop: {
            evaluate: function (_a, meta) {
                var seq = _a[0];
                assertSeq(seq, meta);
                if (isString(seq)) {
                    return seq.substr(0, seq.length - 1);
                }
                var copy = __spreadArray([], seq);
                copy.pop();
                return copy;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        position: {
            evaluate: function (_a, meta, contextStack, _b) {
                var fn = _a[0], seq = _a[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, meta);
                assertSeq(seq, meta);
                if (isString(seq)) {
                    var index = seq.split("").findIndex(function (elem) { return executeFunction(fn, [elem], meta, contextStack); });
                    return index !== -1 ? index : null;
                }
                else {
                    var index = seq.findIndex(function (elem) { return executeFunction(fn, [elem], meta, contextStack); });
                    return index !== -1 ? index : null;
                }
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'index-of': {
            evaluate: function (_a, meta) {
                var seq = _a[0], value = _a[1];
                assertAny(value, meta);
                assertSeq(seq, meta);
                if (isString(seq)) {
                    assertString(value, meta);
                    var index = seq.indexOf(value);
                    return index !== -1 ? index : null;
                }
                else {
                    var index = seq.indexOf(value);
                    return index !== -1 ? index : null;
                }
            },
            validate: function (node) { return assertLength(2, node); },
        },
        push: {
            evaluate: function (_a, meta) {
                var seq = _a[0], values = _a.slice(1);
                assertSeq(seq, meta);
                if (isString(seq)) {
                    assertCharArray(values, meta);
                    return __spreadArray([seq], values).join("");
                }
                else {
                    return __spreadArray(__spreadArray([], seq, true), values);
                }
            },
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
        reduce: {
            evaluate: function (params, meta, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var fn = params[0];
                assertLitsFunction(fn, meta);
                if (params.length === 2) {
                    var arr = params[1];
                    assertSeq(arr, meta);
                    if (arr.length === 0) {
                        return executeFunction(fn, [], meta, contextStack);
                    }
                    else if (arr.length === 1) {
                        return toAny(arr[0]);
                    }
                    if (isString(arr)) {
                        var chars = arr.split("");
                        return chars.slice(1).reduce(function (result, elem) {
                            var val = executeFunction(fn, [result, elem], meta, contextStack);
                            return val;
                        }, asAny(chars[0], meta));
                    }
                    else {
                        return arr.slice(1).reduce(function (result, elem) {
                            return executeFunction(fn, [result, elem], meta, contextStack);
                        }, toAny(arr[0]));
                    }
                }
                else {
                    var val = params[1], seq = params[2];
                    assertAny(val, meta);
                    assertSeq(seq, meta);
                    if (isString(seq)) {
                        assertString(val, meta);
                        if (seq.length === 0) {
                            return val;
                        }
                        return seq.split("").reduce(function (result, elem) {
                            var newVal = executeFunction(fn, [result, elem], meta, contextStack);
                            return newVal;
                        }, val);
                    }
                    else {
                        if (seq.length === 0) {
                            return val;
                        }
                        return seq.reduce(function (result, elem) {
                            return executeFunction(fn, [result, elem], meta, contextStack);
                        }, val);
                    }
                }
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        'reduce-right': {
            evaluate: function (params, meta, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var fn = params[0];
                assertLitsFunction(fn, meta);
                if (params.length === 2) {
                    var seq = params[1];
                    assertSeq(seq, meta);
                    if (seq.length === 0) {
                        return executeFunction(fn, [], meta, contextStack);
                    }
                    else if (seq.length === 1) {
                        return toAny(seq[0]);
                    }
                    if (isString(seq)) {
                        var chars = seq.split("");
                        return chars.slice(0, chars.length - 1).reduceRight(function (result, elem) {
                            var newVal = executeFunction(fn, [result, elem], meta, contextStack);
                            assertString(newVal, meta);
                            return newVal;
                        }, chars[chars.length - 1]);
                    }
                    else {
                        return seq.slice(0, seq.length - 1).reduceRight(function (result, elem) {
                            return executeFunction(fn, [result, elem], meta, contextStack);
                        }, asAny(seq[seq.length - 1], meta));
                    }
                }
                else {
                    var val = params[1], seq = params[2];
                    assertAny(val, meta);
                    assertSeq(seq, meta);
                    if (isString(seq)) {
                        if (seq.length === 0) {
                            return val;
                        }
                        return seq.split("").reduceRight(function (result, elem) {
                            var newVal = executeFunction(fn, [result, elem], meta, contextStack);
                            return newVal;
                        }, val);
                    }
                    else {
                        if (seq.length === 0) {
                            return val;
                        }
                        return seq.reduceRight(function (result, elem) {
                            return executeFunction(fn, [result, elem], meta, contextStack);
                        }, val);
                    }
                }
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        rest: {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertSeq(first, meta);
                if (Array.isArray(first)) {
                    if (first.length <= 1) {
                        return [];
                    }
                    return first.slice(1);
                }
                return first.substr(1);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        nthrest: {
            evaluate: function (_a, meta) {
                var seq = _a[0], count = _a[1];
                assertSeq(seq, meta);
                assertFiniteNumber(count, meta);
                var integerCount = Math.max(Math.ceil(count), 0);
                if (Array.isArray(seq)) {
                    return seq.slice(integerCount);
                }
                return seq.substr(integerCount);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        next: {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertSeq(first, meta);
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
            validate: function (node) { return assertLength(1, node); },
        },
        nthnext: {
            evaluate: function (_a, meta) {
                var seq = _a[0], count = _a[1];
                assertSeq(seq, meta);
                assertFiniteNumber(count, meta);
                var integerCount = Math.max(Math.ceil(count), 0);
                if (seq.length <= count) {
                    return null;
                }
                if (Array.isArray(seq)) {
                    return seq.slice(integerCount);
                }
                return seq.substr(integerCount);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        reverse: {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertSeq(first, meta);
                if (Array.isArray(first)) {
                    return __spreadArray([], first).reverse();
                }
                return first.split("").reverse().join("");
            },
            validate: function (node) { return assertLength(1, node); },
        },
        second: {
            evaluate: function (_a, meta) {
                var array = _a[0];
                assertSeq(array, meta);
                return toAny(array[1]);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        shift: {
            evaluate: function (_a, meta) {
                var seq = _a[0];
                assertSeq(seq, meta);
                if (isString(seq)) {
                    return seq.substr(1);
                }
                var copy = __spreadArray([], seq);
                copy.shift();
                return copy;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        slice: {
            evaluate: function (params, meta) {
                var seq = params[0], from = params[1], to = params[2];
                assertSeq(seq, meta);
                if (params.length === 1) {
                    return seq;
                }
                assertInteger(from, meta);
                if (params.length === 2) {
                    return seq.slice(from);
                }
                assertInteger(to, meta);
                return seq.slice(from, to);
            },
            validate: function (node) { return assertLength({ min: 1, max: 3 }, node); },
        },
        some: {
            evaluate: function (_a, meta, contextStack, _b) {
                var _c;
                var fn = _a[0], seq = _a[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, meta);
                assertSeq(seq, meta);
                if (seq.length === 0) {
                    return null;
                }
                if (isString(seq)) {
                    return (_c = seq.split("").find(function (elem) { return executeFunction(fn, [elem], meta, contextStack); })) !== null && _c !== void 0 ? _c : null;
                }
                return toAny(seq.find(function (elem) { return executeFunction(fn, [elem], meta, contextStack); }));
            },
            validate: function (node) { return assertLength(2, node); },
        },
        sort: {
            evaluate: function (params, meta, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var defaultComparer = params.length === 1;
                var seq = defaultComparer ? params[0] : params[1];
                var comparer = defaultComparer ? null : params[0];
                assertSeq(seq, meta);
                if (isString(seq)) {
                    var result_1 = seq.split("");
                    if (defaultComparer) {
                        result_1.sort(compare);
                    }
                    else {
                        assertLitsFunction(comparer, meta);
                        result_1.sort(function (a, b) {
                            var compareValue = executeFunction(comparer, [a, b], meta, contextStack);
                            assertFiniteNumber(compareValue, meta);
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
                        assertLitsFunction(comparer, meta);
                        var compareValue = executeFunction(comparer, [a, b], meta, contextStack);
                        assertFiniteNumber(compareValue, meta);
                        return compareValue;
                    });
                }
                return result;
            },
            validate: function (node) { return assertLength({ min: 1, max: 2 }, node); },
        },
        'sort-by': {
            evaluate: function (params, meta, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var defaultComparer = params.length === 2;
                var keyfn = asAny(params[0], meta);
                var comparer = defaultComparer ? null : params[1];
                var seq = asSeq(defaultComparer ? params[1] : params[2], meta);
                if (isString(seq)) {
                    var result_2 = seq.split("");
                    if (defaultComparer) {
                        result_2.sort(function (a, b) {
                            var aKey = executeFunction(keyfn, [a], meta, contextStack);
                            var bKey = executeFunction(keyfn, [b], meta, contextStack);
                            return compare(aKey, bKey);
                        });
                    }
                    else {
                        assertLitsFunction(comparer, meta);
                        result_2.sort(function (a, b) {
                            var aKey = executeFunction(keyfn, [a], meta, contextStack);
                            var bKey = executeFunction(keyfn, [b], meta, contextStack);
                            var compareValue = executeFunction(comparer, [aKey, bKey], meta, contextStack);
                            assertFiniteNumber(compareValue, meta);
                            return compareValue;
                        });
                    }
                    return result_2.join("");
                }
                var result = __spreadArray([], seq);
                if (defaultComparer) {
                    result.sort(function (a, b) {
                        var aKey = executeFunction(keyfn, [a], meta, contextStack);
                        var bKey = executeFunction(keyfn, [b], meta, contextStack);
                        return compare(aKey, bKey);
                    });
                }
                else {
                    assertLitsFunction(comparer, meta);
                    result.sort(function (a, b) {
                        var aKey = executeFunction(keyfn, [a], meta, contextStack);
                        var bKey = executeFunction(keyfn, [b], meta, contextStack);
                        var compareValue = executeFunction(comparer, [aKey, bKey], meta, contextStack);
                        assertFiniteNumber(compareValue, meta);
                        return compareValue;
                    });
                }
                return result;
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        take: {
            evaluate: function (_a, meta) {
                var n = _a[0], input = _a[1];
                assertNumber(n, meta);
                assertSeq(input, meta);
                var number = Math.max(Math.ceil(n), 0);
                return input.slice(0, number);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'take-last': {
            evaluate: function (_a, meta) {
                var n = _a[0], array = _a[1];
                assertSeq(array, meta);
                assertNumber(n, meta);
                var number = Math.max(Math.ceil(n), 0);
                var from = array.length - number;
                return array.slice(from);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'take-while': {
            evaluate: function (_a, meta, contextStack, _b) {
                var fn = _a[0], seq = _a[1];
                var executeFunction = _b.executeFunction;
                assertSeq(seq, meta);
                assertLitsFunction(fn, meta);
                var result = [];
                for (var _i = 0, seq_1 = seq; _i < seq_1.length; _i++) {
                    var item = seq_1[_i];
                    if (executeFunction(fn, [item], meta, contextStack)) {
                        result.push(item);
                    }
                    else {
                        break;
                    }
                }
                return isString(seq) ? result.join("") : result;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        drop: {
            evaluate: function (_a, meta) {
                var n = _a[0], input = _a[1];
                assertNumber(n, meta);
                var number = Math.max(Math.ceil(n), 0);
                assertSeq(input, meta);
                return input.slice(number);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'drop-last': {
            evaluate: function (_a, meta) {
                var n = _a[0], array = _a[1];
                assertSeq(array, meta);
                assertNumber(n, meta);
                var number = Math.max(Math.ceil(n), 0);
                var from = array.length - number;
                return array.slice(0, from);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'drop-while': {
            evaluate: function (_a, meta, contextStack, _b) {
                var fn = _a[0], seq = _a[1];
                var executeFunction = _b.executeFunction;
                assertSeq(seq, meta);
                assertLitsFunction(fn, meta);
                if (Array.isArray(seq)) {
                    var from_1 = seq.findIndex(function (elem) { return !executeFunction(fn, [elem], meta, contextStack); });
                    return seq.slice(from_1);
                }
                var charArray = seq.split("");
                var from = charArray.findIndex(function (elem) { return !executeFunction(fn, [elem], meta, contextStack); });
                return charArray.slice(from).join("");
            },
            validate: function (node) { return assertLength(2, node); },
        },
        unshift: {
            evaluate: function (_a, meta) {
                var seq = _a[0], values = _a.slice(1);
                assertSeq(seq, meta);
                if (isString(seq)) {
                    assertCharArray(values, meta);
                    return __spreadArray(__spreadArray([], values, true), [seq]).join("");
                }
                var copy = __spreadArray([], seq);
                copy.unshift.apply(copy, values);
                return copy;
            },
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
        'random-sample!': {
            evaluate: function (_a, meta) {
                var prob = _a[0], seq = _a[1];
                assertFiniteNumber(prob, meta);
                assertSeq(seq, meta);
                if (isString(seq)) {
                    return seq
                        .split("")
                        .filter(function () { return Math.random() < prob; })
                        .join("");
                }
                else {
                    return seq.filter(function () { return Math.random() < prob; });
                }
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'rand-nth!': {
            evaluate: function (_a, meta) {
                var seq = _a[0];
                assertSeq(seq, meta);
                if (seq.length === 0) {
                    return null;
                }
                var index = Math.floor(Math.random() * seq.length);
                if (isString(seq)) {
                    return toAny(seq.split("")[index]);
                }
                return toAny(seq[index]);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        shuffle: {
            evaluate: function (_a, meta) {
                var input = _a[0];
                assertSeq(input, meta);
                var array = isString(input) ? __spreadArray([], input.split("")) : __spreadArray([], input);
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
                return isString(input) ? array.join("") : array;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        distinct: {
            evaluate: function (_a, meta) {
                var input = _a[0];
                assertSeq(input, meta);
                if (Array.isArray(input)) {
                    return Array.from(new Set(input));
                }
                return Array.from(new Set(input.split(""))).join("");
            },
            validate: function (node) { return assertLength(1, node); },
        },
        remove: {
            evaluate: function (_a, meta, contextStack, _b) {
                var fn = _a[0], input = _a[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, meta);
                assertSeq(input, meta);
                if (Array.isArray(input)) {
                    return input.filter(function (elem) { return !executeFunction(fn, [elem], meta, contextStack); });
                }
                return input
                    .split("")
                    .filter(function (elem) { return !executeFunction(fn, [elem], meta, contextStack); })
                    .join("");
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'remove-at': {
            evaluate: function (_a, meta) {
                var index = _a[0], input = _a[1];
                assertNumber(index, meta);
                assertSeq(input, meta);
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
            validate: function (node) { return assertLength(2, node); },
        },
        'split-at': {
            evaluate: function (_a, meta) {
                var pos = _a[0], seq = _a[1];
                assertFiniteNumber(pos, meta);
                var intPos = toNonNegativeInteger(pos);
                assertSeq(seq, meta);
                return [seq.slice(0, intPos), seq.slice(intPos)];
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'split-with': {
            evaluate: function (_a, meta, contextStack, _b) {
                var fn = _a[0], seq = _a[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, meta);
                assertSeq(seq, meta);
                var seqIsArray = Array.isArray(seq);
                var arr = seqIsArray ? seq : seq.split("");
                var index = arr.findIndex(function (elem) { return !executeFunction(fn, [elem], meta, contextStack); });
                if (index === -1) {
                    return [seq, seqIsArray ? [] : ""];
                }
                return [seq.slice(0, index), seq.slice(index)];
            },
            validate: function (node) { return assertLength(2, node); },
        },
        frequencies: {
            evaluate: function (_a, meta) {
                var seq = _a[0];
                assertSeq(seq, meta);
                var arr = isString(seq) ? seq.split("") : seq;
                return arr.reduce(function (result, val) {
                    assertString(val, meta);
                    if (collHasKey(result, val)) {
                        result[val] = result[val] + 1;
                    }
                    else {
                        result[val] = 1;
                    }
                    return result;
                }, {});
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'group-by': {
            evaluate: function (_a, meta, contextStack, _b) {
                var fn = _a[0], seq = _a[1];
                var executeFunction = _b.executeFunction;
                assertAny(fn, meta);
                assertSeq(seq, meta);
                var arr = Array.isArray(seq) ? seq : seq.split("");
                return arr.reduce(function (result, val) {
                    var key = executeFunction(fn, [val], meta, contextStack);
                    assertString(key, meta);
                    if (!collHasKey(result, key)) {
                        result[key] = [];
                    }
                    result[key].push(val);
                    return result;
                }, {});
            },
            validate: function (node) { return assertLength(2, node); },
        },
        partition: {
            evaluate: function (params, meta) {
                var len = params.length;
                var n = toNonNegativeInteger(asNumber(params[0], meta));
                var seq = len === 2 ? asSeq(params[1], meta) : len === 3 ? asSeq(params[2], meta) : asSeq(params[3], meta);
                var step = len >= 3 ? toNonNegativeInteger(asNumber(params[1], meta)) : n;
                var pad = len === 4 ? (params[2] === null ? [] : asArr(params[2], meta)) : undefined;
                return partition(n, step, seq, pad, meta);
            },
            validate: function (node) { return assertLength({ min: 2, max: 4 }, node); },
        },
        'partition-all': {
            evaluate: function (params, meta) {
                var len = params.length;
                var n = toNonNegativeInteger(asNumber(params[0], meta));
                var seq = len === 2 ? asSeq(params[1], meta) : asSeq(params[2], meta);
                var step = len >= 3 ? toNonNegativeInteger(asNumber(params[1], meta)) : n;
                return partition(n, step, seq, [], meta);
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        'partition-by': {
            evaluate: function (_a, meta, contextStack, _b) {
                var fn = _a[0], seq = _a[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, meta);
                assertSeq(seq, meta);
                var isStringSeq = isString(seq);
                var oldValue = undefined;
                var result = (isStringSeq ? seq.split("") : seq).reduce(function (result, elem) {
                    var value = executeFunction(fn, [elem], meta, contextStack);
                    if (value !== oldValue) {
                        result.push([]);
                        oldValue = value;
                    }
                    result[result.length - 1].push(elem);
                    return result;
                }, []);
                return isStringSeq ? result.map(function (elem) { return elem.join(""); }) : result;
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
    };
    function partition(n, step, seq, pad, meta) {
        assertPositiveNumber(step, meta);
        var isStringSeq = isString(seq);
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
            evaluate: function (params, meta) {
                var first = params[0], second = params[1], third = params[2];
                var from;
                var to;
                var step;
                assertFiniteNumber(first, meta);
                if (params.length === 1) {
                    from = 0;
                    to = first;
                    step = to >= 0 ? 1 : -1;
                }
                else if (params.length === 2) {
                    assertFiniteNumber(second, meta);
                    from = first;
                    to = second;
                    step = to >= from ? 1 : -1;
                }
                else {
                    assertFiniteNumber(second, meta);
                    assertFiniteNumber(third, meta);
                    from = first;
                    to = second;
                    step = third;
                    if (to > from) {
                        assertPositiveNumber(step, meta);
                    }
                    else if (to < from) {
                        assertNegativeNumber(step, meta);
                    }
                    else {
                        assertNumberNotZero(step, meta);
                    }
                }
                var result = [];
                for (var i = from; step < 0 ? i > to : i < to; i += step) {
                    result.push(i);
                }
                return result;
            },
            validate: function (node) { return assertLength({ min: 1, max: 3 }, node); },
        },
        repeat: {
            evaluate: function (_a, meta) {
                var count = _a[0], value = _a[1];
                assertNonNegativeInteger(count, meta);
                var result = [];
                for (var i = 0; i < count; i += 1) {
                    result.push(value);
                }
                return result;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        flatten: {
            evaluate: function (_a) {
                var seq = _a[0];
                if (!isArr(seq)) {
                    return [];
                }
                return seq.flat(Number.POSITIVE_INFINITY);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        mapcat: {
            evaluate: function (params, meta, contextStack, helpers) {
                params.slice(1).forEach(function (arr) {
                    assertArr(arr, meta);
                });
                var mapResult = evaluateMap(params, meta, contextStack, helpers);
                assertArr(mapResult, meta);
                return mapResult.flat(1);
            },
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
    };

    var mathNormalExpression = {
        inc: {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertNumber(first, meta);
                return first + 1;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        dec: {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertNumber(first, meta);
                return first - 1;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        '+': {
            evaluate: function (params, meta) {
                return params.reduce(function (result, param) {
                    assertNumber(param, meta);
                    return result + param;
                }, 0);
            },
        },
        '*': {
            evaluate: function (params, meta) {
                return params.reduce(function (result, param) {
                    assertNumber(param, meta);
                    return result * param;
                }, 1);
            },
        },
        '/': {
            evaluate: function (params, meta) {
                if (params.length === 0) {
                    return 1;
                }
                var first = params[0], rest = params.slice(1);
                assertNumber(first, meta);
                if (rest.length === 0) {
                    assertNumber(first, meta);
                    return 1 / first;
                }
                return rest.reduce(function (result, param) {
                    assertNumber(param, meta);
                    return result / param;
                }, first);
            },
        },
        '-': {
            evaluate: function (_a, meta) {
                var first = _a[0], rest = _a.slice(1);
                if (!first) {
                    return 0;
                }
                assertNumber(first, meta);
                if (rest.length === 0) {
                    return -first;
                }
                return rest.reduce(function (result, param) {
                    assertNumber(param, meta);
                    return result - param;
                }, first);
            },
        },
        quot: {
            evaluate: function (_a, meta) {
                var dividend = _a[0], divisor = _a[1];
                assertNumber(dividend, meta);
                assertNumber(divisor, meta);
                var quotient = Math.trunc(dividend / divisor);
                return quotient;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        mod: {
            evaluate: function (_a, meta) {
                var dividend = _a[0], divisor = _a[1];
                assertNumber(dividend, meta);
                assertNumber(divisor, meta);
                var quotient = Math.floor(dividend / divisor);
                return dividend - divisor * quotient;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        rem: {
            evaluate: function (_a, meta) {
                var dividend = _a[0], divisor = _a[1];
                assertNumber(dividend, meta);
                assertNumber(divisor, meta);
                var quotient = Math.trunc(dividend / divisor);
                return dividend - divisor * quotient;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        sqrt: {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertNumber(first, meta);
                return Math.sqrt(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        cbrt: {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertNumber(first, meta);
                return Math.cbrt(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        pow: {
            evaluate: function (_a, meta) {
                var first = _a[0], second = _a[1];
                assertNumber(first, meta);
                assertNumber(second, meta);
                return Math.pow(first, second);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        round: {
            evaluate: function (params, meta) {
                var value = params[0], decimals = params[1];
                assertNumber(value, meta);
                if (params.length === 1 || decimals === 0) {
                    return Math.round(value);
                }
                assertNonNegativeInteger(decimals, meta);
                var factor = Math.pow(10, decimals);
                return Math.round(value * factor) / factor;
            },
            validate: function (node) { return assertLength({ min: 1, max: 2 }, node); },
        },
        trunc: {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertNumber(first, meta);
                return Math.trunc(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        floor: {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertNumber(first, meta);
                return Math.floor(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        ceil: {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertNumber(first, meta);
                return Math.ceil(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'rand!': {
            evaluate: function (parameters, meta) {
                var number = parameters.length === 1 ? parameters[0] : 1;
                assertNumber(number, meta);
                return Math.random() * number;
            },
            validate: function (node) { return assertLength({ min: 0, max: 1 }, node); },
        },
        'rand-int!': {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertNumber(first, meta);
                return Math.floor(Math.random() * Math.abs(first)) * Math.sign(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        min: {
            evaluate: function (_a, meta) {
                var first = _a[0], rest = _a.slice(1);
                assertNumber(first, meta);
                if (rest.length === 0) {
                    return first;
                }
                return rest.reduce(function (min, value) {
                    assertNumber(value, meta);
                    return Math.min(min, value);
                }, first);
            },
            validate: function (node) { return assertLength({ min: 1 }, node); },
        },
        max: {
            evaluate: function (_a, meta) {
                var first = _a[0], rest = _a.slice(1);
                assertNumber(first, meta);
                if (rest.length === 0) {
                    return first;
                }
                return rest.reduce(function (min, value) {
                    assertNumber(value, meta);
                    return Math.max(min, value);
                }, first);
            },
            validate: function (node) { return assertLength({ min: 1 }, node); },
        },
        abs: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.abs(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        sign: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.sign(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'max-safe-integer': {
            evaluate: function () {
                return Number.MAX_SAFE_INTEGER;
            },
            validate: function (node) { return assertLength(0, node); },
        },
        'min-safe-integer': {
            evaluate: function () {
                return Number.MIN_SAFE_INTEGER;
            },
            validate: function (node) { return assertLength(0, node); },
        },
        'max-value': {
            evaluate: function () {
                return Number.MAX_VALUE;
            },
            validate: function (node) { return assertLength(0, node); },
        },
        'min-value': {
            evaluate: function () {
                return Number.MIN_VALUE;
            },
            validate: function (node) { return assertLength(0, node); },
        },
        epsilon: {
            evaluate: function () {
                return Number.EPSILON;
            },
            validate: function (node) { return assertLength(0, node); },
        },
        'positive-infinity': {
            evaluate: function () {
                return Number.POSITIVE_INFINITY;
            },
            validate: function (node) { return assertLength(0, node); },
        },
        'negative-infinity': {
            evaluate: function () {
                return Number.NEGATIVE_INFINITY;
            },
            validate: function (node) { return assertLength(0, node); },
        },
        nan: {
            evaluate: function () {
                return Number.NaN;
            },
            validate: function (node) { return assertLength(0, node); },
        },
        e: {
            evaluate: function () {
                return Math.E;
            },
            validate: function (node) { return assertLength(0, node); },
        },
        pi: {
            evaluate: function () {
                return Math.PI;
            },
            validate: function (node) { return assertLength(0, node); },
        },
        exp: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.exp(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        log: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.log(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        log2: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.log2(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        log10: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.log10(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        sin: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.sin(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        asin: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.asin(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        sinh: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.sinh(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        asinh: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.asinh(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        cos: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.cos(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        acos: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.acos(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        cosh: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.cosh(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        acosh: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.acosh(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        tan: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.tan(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        atan: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.atan(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        tanh: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.tanh(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        atanh: {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Math.atanh(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
    };

    var version = "1.0.0-alpha.5";

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
            validate: function (node) { return assertLength({ min: 1 }, node); },
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
            validate: function (node) { return assertLength({ min: 1 }, node); },
        },
        'equal?': {
            evaluate: function (_a, meta) {
                var a = _a[0], b = _a[1];
                return deepEqual(asAny(a, meta), asAny(b, meta), meta);
            },
            validate: function (node) { return assertLength({ min: 1 }, node); },
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
            validate: function (node) { return assertLength({ min: 1 }, node); },
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
            validate: function (node) { return assertLength({ min: 1 }, node); },
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
            validate: function (node) { return assertLength({ min: 1 }, node); },
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
            validate: function (node) { return assertLength({ min: 1 }, node); },
        },
        not: {
            evaluate: function (_a) {
                var first = _a[0];
                return !first;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'inst-ms': {
            evaluate: function () {
                return Date.now();
            },
            validate: function (node) { return assertLength(0, node); },
        },
        'write!': {
            evaluate: function (params, meta) {
                // eslint-disable-next-line no-console
                console.log.apply(console, params);
                if (params.length > 0) {
                    return asAny(params[params.length - 1], meta);
                }
                return null;
            },
        },
        'debug!': {
            evaluate: function (params, meta, contextStack) {
                if (params.length === 0) {
                    // eslint-disable-next-line no-console
                    console.warn("*** LITS DEBUG ***\n" + contextStackToString(contextStack) + "\n");
                    return null;
                }
                // eslint-disable-next-line no-console
                console.warn("*** LITS DEBUG ***\n" + JSON.stringify(params[0], null, 2) + "\n");
                return asAny(params[0], meta);
            },
            validate: function (node) { return assertLength({ max: 1 }, node); },
        },
        boolean: {
            evaluate: function (_a) {
                var value = _a[0];
                return !!value;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        compare: {
            evaluate: function (_a) {
                var a = _a[0], b = _a[1];
                return compare(a, b);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        assert: {
            evaluate: function (params, meta) {
                var value = params[0];
                var message = params.length === 2 ? params[1] : "" + value;
                assertString(message, meta);
                if (!value) {
                    throw new AssertionError(message, meta);
                }
                return asAny(value, meta);
            },
            validate: function (node) { return assertLength({ min: 1, max: 2 }, node); },
        },
        'lits-version': {
            evaluate: function () {
                return version;
            },
            validate: function (node) { return assertLength(0, node); },
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
        if (isLitsFunction(value)) {
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
            evaluate: function (params, meta) {
                var result = {};
                for (var i = 0; i < params.length; i += 2) {
                    var key = params[i];
                    var value = params[i + 1];
                    assertString(key, meta);
                    result[key] = value;
                }
                return result;
            },
            validate: function (node) { return assertLengthEven(node); },
        },
        keys: {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertObj(first, meta);
                return Object.keys(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        vals: {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertObj(first, meta);
                return Object.values(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        entries: {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertObj(first, meta);
                return Object.entries(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        find: {
            evaluate: function (_a, meta) {
                var obj = _a[0], key = _a[1];
                assertObj(obj, meta);
                assertString(key, meta);
                if (collHasKey(obj, key)) {
                    return [key, obj[key]];
                }
                return null;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        dissoc: {
            evaluate: function (_a, meta) {
                var obj = _a[0], key = _a[1];
                assertObj(obj, meta);
                assertString(key, meta);
                var result = toAny(obj[key]);
                delete obj[key];
                return result;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        merge: {
            evaluate: function (params, meta) {
                if (params.length === 0) {
                    return null;
                }
                var first = params[0], rest = params.slice(1);
                assertObj(first, meta);
                return rest.reduce(function (result, obj) {
                    assertObj(obj, meta);
                    return __assign(__assign({}, result), obj);
                }, __assign({}, first));
            },
            validate: function (node) { return assertLength({ min: 0 }, node); },
        },
        'merge-with': {
            evaluate: function (params, meta, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var fn = params[0], first = params[1], rest = params.slice(2);
                assertLitsFunction(fn, meta);
                if (params.length === 1) {
                    return null;
                }
                assertObj(first, meta);
                return rest.reduce(function (result, obj) {
                    assertObj(obj, meta);
                    Object.entries(obj).forEach(function (entry) {
                        var key = asNotUndefined(entry[0], meta);
                        var val = toAny(entry[1]);
                        if (collHasKey(result, key)) {
                            result[key] = executeFunction(fn, [result[key], val], meta, contextStack);
                        }
                        else {
                            result[key] = val;
                        }
                    });
                    return result;
                }, __assign({}, first));
            },
            validate: function (node) { return assertLength({ min: 1 }, node); },
        },
        zipmap: {
            evaluate: function (_a, meta) {
                var keys = _a[0], values = _a[1];
                assertStringArray(keys, meta);
                assertArr(values, meta);
                var length = Math.min(keys.length, values.length);
                var result = {};
                for (var i = 0; i < length; i += 1) {
                    var key = asNotUndefined(keys[i]);
                    result[key] = toAny(values[i]);
                }
                return result;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'select-keys': {
            evaluate: function (_a, meta) {
                var obj = _a[0], keys = _a[1];
                assertStringArray(keys, meta);
                assertObj(obj, meta);
                return keys.reduce(function (result, key) {
                    if (collHasKey(obj, key)) {
                        result[key] = toAny(obj[key]);
                    }
                    return result;
                }, {});
            },
            validate: function (node) { return assertLength(2, node); },
        },
    };

    var predicatesNormalExpression = {
        'function?': {
            evaluate: function (_a) {
                var first = _a[0];
                return isLitsFunction(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'string?': {
            evaluate: function (_a) {
                var first = _a[0];
                return typeof first === "string";
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'number?': {
            evaluate: function (_a) {
                var first = _a[0];
                return typeof first === "number";
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'integer?': {
            evaluate: function (_a) {
                var first = _a[0];
                return typeof first === "number" && Number.isInteger(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'boolean?': {
            evaluate: function (_a) {
                var first = _a[0];
                return typeof first === "boolean";
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'nil?': {
            evaluate: function (_a) {
                var first = _a[0];
                return first === null || first === undefined;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'zero?': {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertFiniteNumber(first, meta);
                return first === 0;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'pos?': {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertFiniteNumber(first, meta);
                return first > 0;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'neg?': {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertFiniteNumber(first, meta);
                return first < 0;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'even?': {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertFiniteNumber(first, meta);
                return first % 2 === 0;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'odd?': {
            evaluate: function (_a, meta) {
                var first = _a[0];
                assertFiniteNumber(first, meta);
                return Number.isInteger(first) && first % 2 !== 0;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'array?': {
            evaluate: function (_a) {
                var first = _a[0];
                return isArr(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'coll?': {
            evaluate: function (_a) {
                var first = _a[0];
                return isColl(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'seq?': {
            evaluate: function (_a) {
                var first = _a[0];
                return isSeq(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'object?': {
            evaluate: function (_a) {
                var first = _a[0];
                return isObj(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'regexp?': {
            evaluate: function (_a) {
                var first = _a[0];
                return first !== null && !Array.isArray(first) && typeof first === "object" && first instanceof RegExp;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'finite?': {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Number.isFinite(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'nan?': {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return Number.isNaN(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'positive-infinity?': {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return value === Number.POSITIVE_INFINITY;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'negative-infinity?': {
            evaluate: function (_a, meta) {
                var value = _a[0];
                assertNumber(value, meta);
                return value === Number.NEGATIVE_INFINITY;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'true?': {
            evaluate: function (_a) {
                var value = _a[0];
                return value === true;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'false?': {
            evaluate: function (_a) {
                var value = _a[0];
                return value === false;
            },
            validate: function (node) { return assertLength(1, node); },
        },
    };

    var regexpNormalExpression = {
        regexp: {
            evaluate: function (params, meta) {
                var first = params[0], second = params[1];
                assertString(first, meta);
                if (params.length === 1) {
                    return new RegExp(first);
                }
                assertString(second, meta);
                return new RegExp(first, second);
            },
            validate: function (node) { return assertLength({ min: 1, max: 2 }, node); },
        },
        match: {
            evaluate: function (_a, meta) {
                var first = _a[0], second = _a[1];
                assertRegExp(first, meta);
                assertString(second, meta);
                var match = first.exec(second);
                if (match) {
                    return __spreadArray([], match);
                }
                return null;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        replace: {
            evaluate: function (_a, meta) {
                var string = _a[0], regexp = _a[1], value = _a[2];
                assertString(string, meta);
                assertRegExp(regexp, meta);
                assertString(value, meta);
                return string.replace(regexp, value);
            },
            validate: function (node) { return assertLength(3, node); },
        },
    };

    var stringNormalExpression = {
        subs: {
            evaluate: function (_a, meta) {
                var first = _a[0], second = _a[1], third = _a[2];
                assertString(first, meta);
                assertNonNegativeInteger(second, meta);
                if (third === undefined) {
                    return first.substring(second);
                }
                assertNumberGte(third, second, meta);
                return first.substring(second, third);
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        'string-repeat': {
            evaluate: function (_a, meta) {
                var string = _a[0], count = _a[1];
                assertString(string, meta);
                assertNonNegativeInteger(count, meta);
                return string.repeat(count);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        str: {
            evaluate: function (params) {
                return params.reduce(function (result, param) {
                    var paramStr = param === undefined || param === null
                        ? ""
                        : isObj(param)
                            ? JSON.stringify(param)
                            : Array.isArray(param)
                                ? JSON.stringify(param)
                                : "" + param;
                    return result + paramStr;
                }, "");
            },
        },
        number: {
            evaluate: function (_a, meta) {
                var str = _a[0];
                assertString(str, meta);
                var number = Number(str);
                if (Number.isNaN(number)) {
                    throw new LitsError("Could not convert '" + str + "' to a number", meta);
                }
                return number;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'number-to-string': {
            evaluate: function (params, meta) {
                var number = params[0], base = params[1];
                assertFiniteNumber(number, meta);
                if (params.length === 1) {
                    return "" + number;
                }
                else {
                    assertFiniteNumber(base, meta);
                    if (base !== 2 && base !== 8 && base !== 10 && base !== 16) {
                        throw new LitsError("Expected \"number-to-string\" base argument to be 2, 8, 10 or 16, got: " + base, meta);
                    }
                    if (base === 10) {
                        return "" + number;
                    }
                    assertNonNegativeInteger(number, meta);
                    return Number(number).toString(base);
                }
            },
            validate: function (node) { return assertLength({ min: 1, max: 2 }, node); },
        },
        'from-char-code': {
            evaluate: function (_a, meta) {
                var number = _a[0];
                assertFiniteNumber(number, meta);
                var int = toNonNegativeInteger(number);
                return String.fromCodePoint(int);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'to-char-code': {
            evaluate: function (_a, meta) {
                var str = _a[0];
                assertNonEmptyString(str, meta);
                return asNotUndefined(str.codePointAt(0));
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'lower-case': {
            evaluate: function (_a, meta) {
                var str = _a[0];
                assertString(str, meta);
                return str.toLowerCase();
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'upper-case': {
            evaluate: function (_a, meta) {
                var str = _a[0];
                assertString(str, meta);
                return str.toUpperCase();
            },
            validate: function (node) { return assertLength(1, node); },
        },
        trim: {
            evaluate: function (_a, meta) {
                var str = _a[0];
                assertString(str, meta);
                return str.trim();
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'trim-left': {
            evaluate: function (_a, meta) {
                var str = _a[0];
                assertString(str, meta);
                return str.replace(/^\s+/, "");
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'trim-right': {
            evaluate: function (_a, meta) {
                var str = _a[0];
                assertString(str, meta);
                return str.replace(/\s+$/, "");
            },
            validate: function (node) { return assertLength(1, node); },
        },
        join: {
            evaluate: function (_a, meta) {
                var stringList = _a[0], delimiter = _a[1];
                assertArr(stringList, meta);
                stringList.forEach(function (str) { return assertString(str, meta); });
                assertString(delimiter, meta);
                return stringList.join(delimiter);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        split: {
            evaluate: function (_a, meta) {
                var str = _a[0], delimiter = _a[1], limit = _a[2];
                assertString(str, meta);
                assertStringOrRegExp(delimiter, meta);
                if (limit !== undefined) {
                    assertNonNegativeInteger(limit, meta);
                }
                return str.split(delimiter, limit);
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        'pad-left': {
            evaluate: function (_a, meta) {
                var str = _a[0], length = _a[1], padString = _a[2];
                assertString(str, meta);
                assertInteger(length, meta);
                if (padString !== undefined) {
                    assertString(padString, meta);
                }
                return str.padStart(length, padString);
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        'pad-right': {
            evaluate: function (_a, meta) {
                var str = _a[0], length = _a[1], padString = _a[2];
                assertString(str, meta);
                assertInteger(length, meta);
                if (padString !== undefined) {
                    assertString(padString, meta);
                }
                return str.padEnd(length, padString);
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        template: {
            evaluate: function (_a, meta) {
                var templateString = _a[0], placeholders = _a.slice(1);
                assertString(templateString, meta);
                var templateStrings = templateString.split("||||");
                if (templateStrings.length === 1) {
                    assertStringArray(placeholders, meta);
                    return applyPlaceholders(templateStrings[0], placeholders, meta);
                }
                else if (templateStrings.length === 2) {
                    var firstPlaceholder = placeholders[0];
                    assertNonNegativeInteger(firstPlaceholder, meta);
                    var stringPlaceholders = __spreadArray(["" + firstPlaceholder], placeholders.slice(1));
                    if (firstPlaceholder === 1) {
                        return applyPlaceholders(templateStrings[0], stringPlaceholders, meta);
                    }
                    else {
                        return applyPlaceholders(templateStrings[1], stringPlaceholders, meta);
                    }
                }
                else {
                    throw new LitsError("Invalid template string, only one \"||||\" separator allowed", meta);
                }
            },
            validate: function (node) { return assertLength({ min: 1, max: 10 }, node); },
        },
    };
    var doubleDollarRegexp = /\$\$/g;
    function applyPlaceholders(templateString, placeholders, meta) {
        for (var i = 0; i < 9; i += 1) {
            var re = new RegExp("(?<=^|[^$]|\\$\\$)\\$" + (i + 1), "g");
            if (re.test(templateString)) {
                var placeholder = placeholders[i];
                assertString(placeholder, meta);
                templateString = templateString.replace(re, placeholder);
            }
        }
        return templateString.replace(doubleDollarRegexp, "$");
    }

    var functionalNormalExpression = {
        apply: {
            evaluate: function (_a, meta, contextStack, _b) {
                var func = _a[0], params = _a.slice(1);
                var executeFunction = _b.executeFunction;
                assertLitsFunction(func, meta);
                var paramsLength = params.length;
                var last = params[paramsLength - 1];
                assertArr(last, meta);
                var applyArray = __spreadArray(__spreadArray([], params.slice(0, -1), true), last);
                return executeFunction(func, applyArray, meta, contextStack);
            },
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
        identity: {
            evaluate: function (_a) {
                var value = _a[0];
                return toAny(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        partial: {
            evaluate: function (_a) {
                var _b;
                var fn = _a[0], params = _a.slice(1);
                return _b = {},
                    _b[FUNCTION_SYMBOL] = true,
                    _b.type = "partial",
                    _b.fn = toAny(fn),
                    _b.params = params,
                    _b;
            },
            validate: function (node) { return assertLength({ min: 1 }, node); },
        },
        comp: {
            evaluate: function (fns) {
                var _a;
                if (fns.length > 1) {
                    var last = fns[fns.length - 1];
                    if (isArr(last)) {
                        fns = __spreadArray(__spreadArray([], fns.slice(0, -1), true), last);
                    }
                }
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.type = "comp",
                    _a.fns = fns,
                    _a;
            },
        },
        constantly: {
            evaluate: function (_a) {
                var _b;
                var value = _a[0];
                return _b = {},
                    _b[FUNCTION_SYMBOL] = true,
                    _b.type = "constantly",
                    _b.value = toAny(value),
                    _b;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        juxt: {
            evaluate: function (fns) {
                var _a;
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.type = "juxt",
                    _a.fns = fns,
                    _a;
            },
            validate: function (node) { return assertLength({ min: 1 }, node); },
        },
        complement: {
            evaluate: function (_a) {
                var _b;
                var fn = _a[0];
                return _b = {},
                    _b[FUNCTION_SYMBOL] = true,
                    _b.type = "complement",
                    _b.fn = toAny(fn),
                    _b;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'every-pred': {
            evaluate: function (fns) {
                var _a;
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.type = "every-pred",
                    _a.fns = fns,
                    _a;
            },
            validate: function (node) { return assertLength({ min: 1 }, node); },
        },
        'some-pred': {
            evaluate: function (fns) {
                var _a;
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.type = "some-pred",
                    _a.fns = fns,
                    _a;
            },
            validate: function (node) { return assertLength({ min: 1 }, node); },
        },
        fnil: {
            evaluate: function (_a) {
                var _b;
                var fn = _a[0], params = _a.slice(1);
                return _b = {},
                    _b[FUNCTION_SYMBOL] = true,
                    _b.type = "fnil",
                    _b.fn = toAny(fn),
                    _b.params = params,
                    _b;
            },
            validate: function (node) { return assertLength({ min: 2 }, node); },
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

    function findOverloadFunction(overloads, nbrOfParams, meta) {
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
            throw new LitsError("Unexpected number of arguments, got " + nbrOfParams, meta);
        }
        return overloadFunction;
    }
    var functionExecutors = {
        'user-defined': function (fn, params, meta, contextStack, _a) {
            var evaluateAstNode = _a.evaluateAstNode;
            for (;;) {
                var overloadFunction = findOverloadFunction(fn.overloads, params.length, meta);
                var args = overloadFunction.arguments;
                var nbrOfMandatoryArgs = args.mandatoryArguments.length;
                var newContext = __assign({}, overloadFunction.functionContext);
                var length_1 = Math.max(params.length, args.mandatoryArguments.length);
                var rest = [];
                for (var i = 0; i < length_1; i += 1) {
                    if (i < nbrOfMandatoryArgs) {
                        var param = toAny(params[i]);
                        var key = asNotUndefined(args.mandatoryArguments[i], meta);
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
        partial: function (fn, params, meta, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return executeFunction(fn.fn, __spreadArray(__spreadArray([], fn.params, true), params), meta, contextStack);
        },
        comp: function (fn, params, meta, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var fns = fn.fns;
            if (fns.length === 0) {
                if (params.length !== 1) {
                    throw new LitsError("(comp) expects one argument, got " + params.length, meta);
                }
                return asAny(params[0], meta);
            }
            return asAny(fns.reduceRight(function (result, fn) {
                return [executeFunction(toAny(fn), result, meta, contextStack)];
            }, params)[0], meta);
        },
        constantly: function (fn) {
            return fn.value;
        },
        juxt: function (fn, params, meta, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return fn.fns.map(function (fn) { return executeFunction(toAny(fn), params, meta, contextStack); });
        },
        complement: function (fn, params, meta, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return !executeFunction(fn.fn, params, meta, contextStack);
        },
        'every-pred': function (fn, params, meta, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            for (var _i = 0, _b = fn.fns; _i < _b.length; _i++) {
                var f = _b[_i];
                for (var _c = 0, params_1 = params; _c < params_1.length; _c++) {
                    var param = params_1[_c];
                    var result = executeFunction(toAny(f), [param], meta, contextStack);
                    if (!result) {
                        return false;
                    }
                }
            }
            return true;
        },
        'some-pred': function (fn, params, meta, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            for (var _i = 0, _b = fn.fns; _i < _b.length; _i++) {
                var f = _b[_i];
                for (var _c = 0, params_2 = params; _c < params_2.length; _c++) {
                    var param = params_2[_c];
                    var result = executeFunction(toAny(f), [param], meta, contextStack);
                    if (result) {
                        return true;
                    }
                }
            }
            return false;
        },
        fnil: function (fn, params, meta, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var fniledParams = params.map(function (param, index) { return (param === null ? toAny(fn.params[index]) : param); });
            return executeFunction(toAny(fn.fn), fniledParams, meta, contextStack);
        },
        builtin: function (fn, params, meta, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var normalExpression = asNotUndefined(normalExpressions[fn.name]);
            return normalExpression.evaluate(params, meta, contextStack, { executeFunction: executeFunction });
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
                throw new LitsError(node.type + "-node cannot be evaluated", node.token.meta);
        }
    };
    function evaluateNumber(node) {
        return node.value;
    }
    function evaluateString(node) {
        return node.value;
    }
    function evaluateReservedName(node) {
        return asNotUndefined(reservedNamesRecord[node.value]).value;
    }
    function evaluateName(node, contextStack) {
        var _a;
        var value = node.value, meta = node.token.meta;
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
                _a.type = "builtin",
                _a.name = value,
                _a);
            return builtinFunction;
        }
        throw new LitsError("Undefined identifier " + value, meta);
    }
    function evaluateNormalExpression(node, contextStack) {
        var _a;
        var params = node.params.map(function (paramNode) { return evaluateAstNode(paramNode, contextStack); });
        if (isNormalExpressionNodeName(node)) {
            for (var _i = 0, _b = contextStack.stack; _i < _b.length; _i++) {
                var context = _b[_i];
                var fn = (_a = context[node.name]) === null || _a === void 0 ? void 0 : _a.value;
                if (fn === undefined) {
                    continue;
                }
                try {
                    return executeFunction(fn, params, node.token.meta, contextStack);
                }
                catch (_c) {
                    continue;
                }
            }
            return evaluateBuiltinNormalExpression(node, params, contextStack);
        }
        else {
            var fn = evaluateAstNode(node.expression, contextStack);
            return executeFunction(fn, params, node.token.meta, contextStack);
        }
    }
    var executeFunction = function (fn, params, meta, contextStack) {
        if (isLitsFunction(fn)) {
            return functionExecutors[fn.type](fn, params, meta, contextStack, { evaluateAstNode: evaluateAstNode, executeFunction: executeFunction });
        }
        if (Array.isArray(fn)) {
            return evaluateArrayAsFunction(fn, params, meta);
        }
        if (isObj(fn)) {
            return evalueateObjectAsFunction(fn, params, meta);
        }
        if (isString(fn)) {
            return evaluateStringAsFunction(fn, params, meta);
        }
        if (isNumber(fn)) {
            return evaluateNumberAsFunction(fn, params, meta);
        }
        throw new LitsError("Expected function, got " + fn, meta);
    };
    function evaluateBuiltinNormalExpression(node, params, contextStack) {
        var normalExpressionEvaluator = asNotUndefined(builtin.normalExpressions[node.name]).evaluate;
        return normalExpressionEvaluator(params, node.token.meta, contextStack, { executeFunction: executeFunction });
    }
    function evaluateSpecialExpression(node, contextStack) {
        var specialExpressionEvaluator = asNotUndefined(builtin.specialExpressions[node.name]).evaluate;
        return specialExpressionEvaluator(node, contextStack, { evaluateAstNode: evaluateAstNode, builtin: builtin });
    }
    function evalueateObjectAsFunction(fn, params, meta) {
        if (params.length !== 1) {
            throw new LitsError("Object as function requires one string parameter", meta);
        }
        var key = params[0];
        assertString(key, meta);
        return toAny(fn[key]);
    }
    function evaluateArrayAsFunction(fn, params, meta) {
        if (params.length !== 1) {
            throw new LitsError("Array as function requires one non negative integer parameter", meta);
        }
        var index = params[0];
        assertNonNegativeInteger(index, meta);
        return toAny(fn[index]);
    }
    function evaluateStringAsFunction(fn, params, meta) {
        if (params.length !== 1) {
            throw new LitsError("String as function requires one Obj parameter", meta);
        }
        var param = toAny(params[0]);
        if (isObj(param)) {
            return toAny(param[fn]);
        }
        if (isInteger(param)) {
            return toAny(fn[param]);
        }
        throw new LitsError("string as function expects Obj or integer parameter, got " + param, meta);
    }
    function evaluateNumberAsFunction(fn, params, meta) {
        assertInteger(fn, meta);
        if (params.length !== 1) {
            throw new LitsError("String as function requires one Arr parameter", meta);
        }
        var param = params[0];
        assertSeq(param, meta);
        return toAny(param[fn]);
    }

    var parseNumber = function (tokens, position) {
        var token = asNotUndefined(tokens[position]);
        return [position + 1, { type: "Number", value: Number(token.value), token: token }];
    };
    var parseString = function (tokens, position) {
        var token = asNotUndefined(tokens[position]);
        return [position + 1, { type: "String", value: token.value, token: token }];
    };
    var parseName = function (tokens, position) {
        var token = asNotUndefined(tokens[position]);
        return [position + 1, { type: "Name", value: token.value, token: token }];
    };
    var parseReservedName = function (tokens, position) {
        var token = asNotUndefined(tokens[position]);
        return [position + 1, { type: "ReservedName", value: token.value, token: token }];
    };
    var parseTokens = function (tokens, position) {
        var _a;
        var token = asNotUndefined(tokens[position]);
        var astNodes = [];
        var astNode;
        while (!(token.type === "paren" && (token.value === ")" || token.value === "]"))) {
            _a = parseToken(tokens, position), position = _a[0], astNode = _a[1];
            astNodes.push(astNode);
            token = asNotUndefined(tokens[position]);
        }
        return [position, astNodes];
    };
    var parseExpression = function (tokens, position) {
        position += 1; // Skip parenthesis
        var token = asNotUndefined(tokens[position]);
        if (token.type === "name" && builtin.specialExpressions[token.value]) {
            return parseSpecialExpression(tokens, position);
        }
        return parseNormalExpression(tokens, position);
    };
    var parseArrayLitteral = function (tokens, position) {
        var _a;
        var firstToken = asNotUndefined(tokens[position]);
        position = position + 1;
        var token = asNotUndefined(tokens[position]);
        var params = [];
        var param;
        while (!(token.type === "paren" && token.value === "]")) {
            _a = parseToken(tokens, position), position = _a[0], param = _a[1];
            params.push(param);
            token = asNotUndefined(tokens[position]);
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
        var firstToken = asNotUndefined(tokens[position]);
        position = position + 1;
        var token = asNotUndefined(tokens[position]);
        var params = [];
        var param;
        while (!(token.type === "paren" && token.value === "}")) {
            _a = parseToken(tokens, position), position = _a[0], param = _a[1];
            params.push(param);
            token = asNotUndefined(tokens[position]);
        }
        position = position + 1;
        var node = {
            type: "NormalExpression",
            name: "object",
            params: params,
            token: firstToken,
        };
        assertLengthEven(node);
        return [position, node];
    };
    var parseRegexpShorthand = function (tokens, position) {
        var token = asNotUndefined(tokens[position]);
        var stringNode = {
            type: "String",
            value: token.value,
            token: token,
        };
        assertNotUndefined(token.options);
        var optionsNode = {
            type: "String",
            value: "" + (token.options.g ? "g" : "") + (token.options.i ? "i" : ""),
            token: token,
        };
        var node = {
            type: "NormalExpression",
            name: "regexp",
            params: [stringNode, optionsNode],
            token: token,
        };
        return [position + 1, node];
    };
    var placeholderRegexp = /^%([1-9][0-9]?$)/;
    var parseFnShorthand = function (tokens, position) {
        var firstToken = asNotUndefined(tokens[position]);
        position += 2;
        var _a = parseNormalExpression(tokens, position), newPosition = _a[0], normalExpressionNode = _a[1];
        var arity = 0;
        for (var pos = position + 1; pos < newPosition - 1; pos += 1) {
            var token = asNotUndefined(tokens[pos]);
            if (token.type === "name") {
                var match = placeholderRegexp.exec(token.value);
                if (match) {
                    arity = Math.max(arity, Number(match[1]));
                    if (arity > 20) {
                        throw new LitsError("Can't specify more than 20 arguments", firstToken.meta);
                    }
                }
            }
            if (token.type === "fnShorthand") {
                throw new LitsError("Nested shortcut functions are not allowed", firstToken.meta);
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
        var token = asNotUndefined(tokens[position]);
        if (token.type === "name") {
            return [position + 1, { type: "Argument", name: token.value, token: token }];
        }
        else if (token.type === "modifier") {
            var value = token.value;
            return [position + 1, { type: "Modifier", value: value, token: token }];
        }
        else {
            throw new UnexpectedTokenError("), name or modifier", token);
        }
    };
    var parseBindings = function (tokens, position) {
        var _a;
        var token = asNotUndefined(tokens[position]);
        if (!(token.type === "paren" && token.value === "[")) {
            throw new UnexpectedTokenError("[", token);
        }
        position += 1;
        token = asNotUndefined(tokens[position]);
        var bindings = [];
        var binding;
        while (!(token.type === "paren" && token.value === "]")) {
            _a = parseBinding(tokens, position), position = _a[0], binding = _a[1];
            bindings.push(binding);
            token = asNotUndefined(tokens[position]);
        }
        position += 1;
        return [position, bindings];
    };
    var parseBinding = function (tokens, position) {
        var _a;
        var firstToken = asNotUndefined(tokens[position]);
        if (firstToken.type !== "name") {
            throw new LitsError("Expected name node in binding, got " + firstToken.type + " value=" + firstToken.value, firstToken.meta);
        }
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
        if (isExpressionNode(fnNode)) {
            var node_1 = {
                type: "NormalExpression",
                expression: fnNode,
                params: params,
                token: fnNode.token,
            };
            return [position, node_1];
        }
        assertNameNode(fnNode, fnNode.token.meta);
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
        var expressionName = asNotUndefined(tokens[position]).value;
        position += 1;
        var _a = asNotUndefined(builtin.specialExpressions[expressionName]), parse = _a.parse, validate = _a.validate;
        var _b = parse(tokens, position, {
            parseExpression: parseExpression,
            parseTokens: parseTokens,
            parseToken: parseToken,
            parseBinding: parseBinding,
            parseBindings: parseBindings,
            parseArgument: parseArgument,
        }), positionAfterParse = _b[0], node = _b[1];
        validate === null || validate === void 0 ? void 0 : validate(node);
        return [positionAfterParse, node];
    };
    var parseToken = function (tokens, position) {
        var token = asNotUndefined(tokens[position]);
        var nodeDescriptor = undefined;
        switch (token.type) {
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
                if (token.value === "(") {
                    nodeDescriptor = parseExpression(tokens, position);
                }
                else if (token.value === "[") {
                    nodeDescriptor = parseArrayLitteral(tokens, position);
                }
                else if (token.value === "{") {
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
            throw new LitsError("Unrecognized token: " + token.type + " value=" + token.value, token.meta);
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
    var tokenizeLeftParen = function (input, position, meta) {
        return tokenizeCharacter("paren", "(", input, position, meta);
    };
    var tokenizeRightParen = function (input, position, meta) {
        return tokenizeCharacter("paren", ")", input, position, meta);
    };
    var tokenizeLeftBracket = function (input, position, meta) {
        return tokenizeCharacter("paren", "[", input, position, meta);
    };
    var tokenizeRightBracket = function (input, position, meta) {
        return tokenizeCharacter("paren", "]", input, position, meta);
    };
    var tokenizeLeftCurly = function (input, position, meta) {
        return tokenizeCharacter("paren", "{", input, position, meta);
    };
    var tokenizeRightCurly = function (input, position, meta) {
        return tokenizeCharacter("paren", "}", input, position, meta);
    };
    var tokenizeString = function (input, position, meta) {
        if (input[position] !== "'") {
            return NO_MATCH;
        }
        var value = "";
        var length = 1;
        var char = input[position + length];
        var escape = false;
        while (char !== "'" || escape) {
            if (char === undefined) {
                throw new LitsError("Unclosed string at position " + position, meta);
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
        return [length + 1, { type: "string", value: value, meta: meta }];
    };
    var tokenizeSymbolString = function (input, position, meta) {
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
        return [length, { type: "string", value: value, meta: meta }];
    };
    var tokenizeRegexpShorthand = function (input, position, meta) {
        var _a;
        if (input[position] !== "#") {
            return NO_MATCH;
        }
        var _b = tokenizeString(input, position + 1, meta), stringLength = _b[0], token = _b[1];
        if (!token) {
            return NO_MATCH;
        }
        position += stringLength + 1;
        var length = stringLength + 1;
        var options = {};
        while (input[position] === "g" || input[position] === "i") {
            if (input[position] === "g") {
                if (options.g) {
                    throw new LitsError("Duplicated regexp option \"" + input[position] + "\" at position " + position, meta);
                }
                length += 1;
                options.g = true;
            }
            else {
                if (options.i) {
                    throw new LitsError("Duplicated regexp option \"" + input[position] + "\" at position " + position, meta);
                }
                length += 1;
                options.i = true;
            }
            position += 1;
        }
        if (nameRegExp.test((_a = input[position]) !== null && _a !== void 0 ? _a : "")) {
            throw new LitsError("Unexpected regexp option \"" + input[position] + "\" at position " + position, meta);
        }
        return [
            length,
            {
                type: "regexpShorthand",
                value: token.value,
                options: options,
                meta: meta,
            },
        ];
    };
    var tokenizeFnShorthand = function (input, position, meta) {
        if (input.slice(position, position + 2) !== "#(") {
            return NO_MATCH;
        }
        return [
            1,
            {
                type: "fnShorthand",
                value: "#",
                meta: meta,
            },
        ];
    };
    var endOfNumberRegExp = /\s|[)\]},]/;
    var decimalNumberRegExp = /[0-9]/;
    var octalNumberRegExp = /[0-7]/;
    var hexNumberRegExp = /[0-9a-fA-F]/;
    var binaryNumberRegExp = /[0-1]/;
    var firstCharRegExp = /[0-9.-]/;
    var tokenizeNumber = function (input, position, meta) {
        var type = "decimal";
        var firstChar = input[position];
        if (firstChar === undefined) {
            return NO_MATCH;
        }
        var hasDecimals = firstChar === ".";
        if (!firstCharRegExp.test(firstChar)) {
            return NO_MATCH;
        }
        var i;
        for (i = position + 1; i < input.length; i += 1) {
            var char = asNotUndefined(input[i], meta);
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
        return [length, { type: "number", value: value, meta: meta }];
    };
    var tokenizeReservedName = function (input, position, meta) {
        for (var _i = 0, _a = Object.keys(reservedNamesRecord); _i < _a.length; _i++) {
            var reservedName = _a[_i];
            var length_2 = reservedName.length;
            var nextChar = input[position + length_2];
            if (nextChar && nameRegExp.test(nextChar)) {
                continue;
            }
            if (input.substr(position, length_2) === reservedName) {
                return [length_2, { type: "reservedName", value: reservedName, meta: meta }];
            }
        }
        return NO_MATCH;
    };
    var tokenizeName = function (input, position, meta) {
        return tokenizePattern("name", nameRegExp, input, position, meta);
    };
    var tokenizeModifier = function (input, position, meta) {
        var modifiers = ["&", "&let", "&when", "&while"];
        for (var _i = 0, modifiers_1 = modifiers; _i < modifiers_1.length; _i++) {
            var modifier = modifiers_1[_i];
            var length_3 = modifier.length;
            var charAfterModifier = input[position + length_3];
            if (input.substr(position, length_3) === modifier && (!charAfterModifier || !nameRegExp.test(charAfterModifier))) {
                var value = modifier;
                return [length_3, { type: "modifier", value: value, meta: meta }];
            }
        }
        return NO_MATCH;
    };
    function tokenizeCharacter(type, value, input, position, meta) {
        if (value === input[position]) {
            return [1, { type: type, value: value, meta: meta }];
        }
        else {
            return NO_MATCH;
        }
    }
    function tokenizePattern(type, pattern, input, position, meta) {
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
        return [length, { type: type, value: value, meta: meta }];
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
        function TokenMetaImpl(line, column) {
            this.line = line;
            this.column = column;
        }
        TokenMetaImpl.prototype.toString = function () {
            return "(" + this.line + ":" + this.column + ")";
        };
        return TokenMetaImpl;
    }());
    function calculateMeta(input, position) {
        var lines = input.substr(0, position).split(/\r\n|\r|\n/);
        return new TokenMetaImpl(lines.length, lines[lines.length - 1].length);
    }
    function tokenize(input) {
        var tokens = [];
        var position = 0;
        var tokenized = false;
        while (position < input.length) {
            tokenized = false;
            // Loop through all tokenizer until one matches
            var meta = calculateMeta(input, position);
            for (var _i = 0, tokenizers_1 = tokenizers; _i < tokenizers_1.length; _i++) {
                var tokenize_1 = tokenizers_1[_i];
                var _a = tokenize_1(input, position, meta), nbrOfCharacters = _a[0], token = _a[1];
                // tokenizer matched
                if (nbrOfCharacters > 0) {
                    tokenized = true;
                    position += nbrOfCharacters;
                    if (token) {
                        tokens.push(token);
                        break;
                    }
                }
            }
            if (!tokenized) {
                throw new LitsError("Unrecognized character", calculateMeta(input, position));
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
                throw Error("1 is the minimum maxSize, got " + maxSize);
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
            var firstEntry = asNotUndefined(this.firstEntry);
            delete this.cache[firstEntry.key];
            this._size -= 1;
            this.firstEntry = firstEntry.nextEntry;
        };
        return Cache;
    }());

    var Lits = /** @class */ (function () {
        function Lits(config) {
            if (config === void 0) { config = {}; }
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
            return tokenize(program);
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
