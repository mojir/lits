var Lispish = (function (exports) {
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
    var UserDefinedError = /** @class */ (function (_super) {
        __extends(UserDefinedError, _super);
        function UserDefinedError(message) {
            var _this = _super.call(this, message) || this;
            Object.setPrototypeOf(_this, UserDefinedError.prototype);
            _this.name = "UserDefinedError";
            return _this;
        }
        return UserDefinedError;
    }(Error));
    var AssertionError = /** @class */ (function (_super) {
        __extends(AssertionError, _super);
        function AssertionError(message) {
            var _this = _super.call(this, message) || this;
            Object.setPrototypeOf(_this, AssertionError.prototype);
            _this.name = "AssertionError";
            return _this;
        }
        return AssertionError;
    }(Error));
    var UnexpectedTokenError = /** @class */ (function (_super) {
        __extends(UnexpectedTokenError, _super);
        function UnexpectedTokenError(expectedToken, actualToken) {
            var _this = _super.call(this, "Expected a \"" + expectedToken + "\" token, got Token[" + actualToken.type + ":\"" + actualToken.value + "\"]") || this;
            Object.setPrototypeOf(_this, UnexpectedTokenError.prototype);
            _this.name = "UnexpectedTokenError";
            return _this;
        }
        return UnexpectedTokenError;
    }(Error));
    var UnexpectedNodeTypeError = /** @class */ (function (_super) {
        __extends(UnexpectedNodeTypeError, _super);
        function UnexpectedNodeTypeError(expectedNodeType, actualNode) {
            var _this = _super.call(this, "Expected a " + expectedNodeType + " node, got " + (actualNode ? "a " + actualNode.type + " node" : "undefined")) || this;
            Object.setPrototypeOf(_this, UnexpectedNodeTypeError.prototype);
            _this.name = "UnexpectedNodeTypeError";
            return _this;
        }
        return UnexpectedNodeTypeError;
    }(Error));

    var FUNCTION_SYMBOL = Symbol("function");

    function asAstNode(node) {
        if (node === undefined) {
            throw Error("Expected an AST node, got undefined");
        }
        return node;
    }
    function asNameNode(node) {
        if (node === undefined || node.type !== "Name") {
            throw new UnexpectedNodeTypeError("Name", node);
        }
        return node;
    }
    function assertNameNode(node) {
        if (node === undefined || node.type !== "Name") {
            throw new UnexpectedNodeTypeError("Name", node);
        }
    }
    function asAny(value, message) {
        if (message === void 0) { message = "Unexpected end of input"; }
        if (value === undefined) {
            throw Error(message);
        }
        return value;
    }
    function asNotUndefined(value) {
        if (value === undefined) {
            throw Error("Unexpected nil");
        }
        return value;
    }
    function assertNotUndefined(value) {
        if (value === undefined) {
            throw Error("Unexpected nil");
        }
    }
    function assertFiniteNumber(value) {
        if (typeof value !== "number" || !isFinite(value)) {
            throw TypeError("Expected number, got: " + value + " type=\"" + typeof value + "\"");
        }
    }
    function assertPositiveNumber(value) {
        assertFiniteNumber(value);
        if (value <= 0) {
            throw TypeError("Expected positive number, got " + value);
        }
    }
    function assertNegativeNumber(value) {
        assertFiniteNumber(value);
        if (value >= 0) {
            throw TypeError("Expected negative number, got " + value);
        }
    }
    function assertNonNegativeNumber(value) {
        assertFiniteNumber(value);
        if (value < 0) {
            throw TypeError("Expected non negative number, got " + value);
        }
    }
    function assertNonNegativeInteger(value) {
        assertNonNegativeNumber(value);
        assertInteger(value);
    }
    function assertInteger(value) {
        assertFiniteNumber(value);
        if (!Number.isInteger(value)) {
            throw TypeError("Expected integer, got " + value);
        }
    }
    function assertNumberGte(value, x) {
        assertFiniteNumber(value);
        if (value < x) {
            throw TypeError("Expected parameter (" + value + ") to be a number equal or grater than " + x);
        }
    }
    function assertNumberLte(value, x) {
        assertFiniteNumber(value);
        if (value > x) {
            throw TypeError("Expected parameter (" + value + ") to be a number equal or less than " + x);
        }
    }
    function isString(value) {
        return typeof value === "string";
    }
    function assertString(value) {
        if (!isString(value)) {
            throw TypeError("Expected string, got: " + value + " type=\"" + typeof value + "\"");
        }
    }
    function assertStringOrRegExp(value) {
        if (!(value instanceof RegExp || typeof value === "string")) {
            throw TypeError("Expected RegExp or string, got: " + value + " type=\"" + typeof value + "\"");
        }
    }
    function asString(value) {
        if (!isString(value)) {
            throw TypeError("Expected string, got: " + value + " type=\"" + typeof value + "\"");
        }
        return value;
    }
    function assertNonEmptyString(value) {
        assertString(value);
        if (value.length === 0) {
            throw TypeError("Expected non empty string, got: " + value + " type=\"" + typeof value + "\"");
        }
    }
    function isChar(value) {
        return isString(value) && value.length === 1;
    }
    function assertChar(value) {
        if (!isChar(value)) {
            throw TypeError("Expected char, got: " + value + " type=\"" + typeof value + "\"");
        }
    }
    function asChar(value) {
        assertChar(value);
        return value;
    }
    function isStringOrNumber(value) {
        return typeof value === "string" || typeof value === "number";
    }
    function assertStringOrNumber(value) {
        if (!isStringOrNumber(value)) {
            throw TypeError("Expected string or number, got: " + value + " type=\"" + typeof value + "\"");
        }
    }
    function asStringOrNumber(value) {
        assertStringOrNumber(value);
        return value;
    }
    function asNonEmptyString(value) {
        if (typeof value !== "string" || value.length === 0) {
            throw TypeError("Expected non empty string, got: " + value + " type=\"" + typeof value + "\"");
        }
        return value;
    }
    function isRegExp(value) {
        return value instanceof RegExp;
    }
    function assertRegExp(value) {
        if (!(value instanceof RegExp)) {
            throw TypeError("Expected RegExp, got: " + value + " type=\"" + typeof value + "\"");
        }
    }
    function assertObjectOrArray(value) {
        if ((value === null ||
            typeof value !== "object" ||
            Array.isArray(value) ||
            value instanceof RegExp ||
            isLispishFunction(value)) &&
            !Array.isArray(value)) {
            throw TypeError("Expected object or array, got: " + value + " type=\"" + typeof value + "\"");
        }
    }
    function assertNumberNotZero(value) {
        assertFiniteNumber(value);
        if (value === 0) {
            throw TypeError("Expected non zero value");
        }
    }
    function assertLength(count, node) {
        var length = node.params.length;
        if (typeof count === "number") {
            if (length !== count) {
                throw Error("Wrong number of arguments to \"" + node.name + "\", expected " + count + ", got " + length);
            }
        }
        else {
            var min = count.min, max = count.max;
            if (min === undefined && max === undefined) {
                throw Error("Min or max must be specified");
            }
            if (typeof min === "number" && length < min) {
                throw Error("Wrong number of arguments to \"" + node.name + "\", expected at least " + min + ", got " + length);
            }
            if (typeof max === "number" && length > max) {
                throw Error("Wrong number of arguments to \"" + node.name + "\", expected at most " + max + ", got " + length);
            }
        }
    }
    function assertLengthEven(node) {
        var length = node.params.length;
        if (length % 2 !== 0) {
            throw Error("Wrong number of arguments, expected an even number, got " + length);
        }
    }
    function isLispishFunction(func) {
        if (func === null || typeof func !== "object") {
            return false;
        }
        return !!func[FUNCTION_SYMBOL];
    }
    function assertLispishFunction(func) {
        if (!isLispishFunction(func)) {
            throw Error("Expected lispish function, got " + JSON.stringify(func));
        }
    }
    function assertStringArray(value) {
        if (!Array.isArray(value) || value.some(function (v) { return typeof v !== "string"; })) {
            throw Error("Expected an array of strings, got " + value);
        }
    }
    function assertCharArray(arr) {
        if (!Array.isArray(arr) || arr.some(function (v) { return typeof v !== "string" || v.length !== 1; })) {
            throw Error("Expected an array of chars, got " + arr);
        }
    }
    function isExpressionNode(node) {
        return (node.type === "NormalExpression" ||
            node.type === "SpecialExpression" ||
            node.type === "Number" ||
            node.type === "String");
    }
    function assertNumber(value) {
        if (!isNumber(value)) {
            throw TypeError("Expected a number, got: " + value + " type=\"" + typeof value + "\"");
        }
    }
    function asNumber(value) {
        assertNumber(value);
        return value;
    }
    function assertArr(value) {
        if (!isArr(value)) {
            throw TypeError("Expected Arr, got: " + value + " type=\"" + typeof value + "\"");
        }
    }
    function asArr(value) {
        assertArr(value);
        return value;
    }
    function isAny(value) {
        return value !== undefined;
    }
    function assertAny(value) {
        if (!isAny(value)) {
            throw TypeError("Expected Any, got: " + value + " type=\"" + typeof value + "\"");
        }
    }
    function isSeq(value) {
        return Array.isArray(value) || isString(value);
    }
    function assertSeq(value) {
        if (!isSeq(value)) {
            throw TypeError("Expected string or array, got: " + value + " type=\"" + typeof value + "\"");
        }
    }
    function asSeq(value) {
        if (!isSeq(value)) {
            throw TypeError("Expected string or array, got: " + value + " type=\"" + typeof value + "\"");
        }
        return value;
    }
    function assertObj(value) {
        if (!isObj(value)) {
            throw TypeError("Expected object, got: " + value + " type=\"" + typeof value + "\"");
        }
    }
    function isObj(value) {
        return !(value === null ||
            typeof value !== "object" ||
            Array.isArray(value) ||
            value instanceof RegExp ||
            isLispishFunction(value));
    }
    function isArr(value) {
        return Array.isArray(value);
    }
    function isColl(value) {
        return isSeq(value) || isObj(value);
    }
    function assertColl(value) {
        if (!isColl(value)) {
            throw TypeError("Expected collection, got: " + value + " type=\"" + typeof value + "\"");
        }
    }
    function asColl(value) {
        if (!isColl(value)) {
            throw TypeError("Expected collection, got: " + value + " type=\"" + typeof value + "\"");
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
    function deepEqual(a, b) {
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
                if (!deepEqual(asAny(a[i]), asAny(b[i]))) {
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
                var key = asString(aKeys[i]);
                if (!deepEqual(toAny(aObj[key]), toAny(bObj[key]))) {
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
    function assertMax(value, maxNumber) {
        if (value > maxNumber) {
            throw Error("Expected number less than or equal to " + maxNumber + "'");
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

    var andSpecialExpression = {
        parse: function (tokens, position, _a) {
            var parseTokens = _a.parseTokens;
            var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
            return [
                newPosition + 1,
                {
                    type: "SpecialExpression",
                    name: "and",
                    params: params,
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
            var conditions;
            _b = parseConditions(tokens, position, parseToken), position = _b[0], conditions = _b[1];
            return [
                position + 1,
                {
                    type: "SpecialExpression",
                    name: "cond",
                    conditions: conditions,
                    params: [],
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

    function assertNameNotDefined(name, contextStack, builtin) {
        if (typeof name !== "string") {
            return;
        }
        if (builtin.specialExpressions[name]) {
            throw Error("Cannot define variable " + name + ", it's a special expression");
        }
        if (builtin.normalExpressions[name]) {
            throw Error("Cannot define variable " + name + ", it's a builtin function");
        }
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        if (reservedNamesRecord[name]) {
            throw Error("Cannot define variable " + name + ", it's a reserved name");
        }
        var globalContext = asNotUndefined(contextStack[contextStack.length - 2]);
        if (globalContext[name]) {
            throw Error("Name already defined \"" + name + "\"");
        }
    }

    function createParser(expressionName) {
        return function (tokens, position, _a) {
            var _b;
            var parseToken = _a.parseToken, parseArgument = _a.parseArgument, parseBindings = _a.parseBindings;
            var functionName = undefined;
            if (expressionName === "defn" || expressionName === "defns") {
                _b = parseToken(tokens, position), position = _b[0], functionName = _b[1];
                if (expressionName === "defn" && functionName.type !== "Name") {
                    throw new UnexpectedNodeTypeError("Name", functionName);
                }
            }
            var _c = parseFunctionArguments(tokens, position, parseArgument, parseBindings), nextPosition = _c[0], functionArguments = _c[1];
            position = nextPosition;
            var token = asNotUndefined(tokens[position]);
            var body = [];
            while (!(token.type === "paren" && token.value === ")")) {
                var _d = parseToken(tokens, position), newPosition = _d[0], bodyNode = _d[1];
                body.push(bodyNode);
                position = newPosition;
                token = asNotUndefined(tokens[position]);
            }
            if (body.length === 0) {
                throw Error("Missing body in special expression \"defn\"");
            }
            position += 1;
            if (expressionName === "defn" || expressionName === "defns") {
                return [
                    position,
                    {
                        type: "SpecialExpression",
                        name: expressionName,
                        functionName: functionName,
                        params: [],
                        arguments: functionArguments,
                        body: body,
                    },
                ];
            }
            return [
                position,
                {
                    type: "SpecialExpression",
                    name: expressionName,
                    params: [],
                    arguments: functionArguments,
                    body: body,
                },
            ];
        };
    }
    function getFunctionName(expressionName, node, contextStack, evaluateAstNode) {
        if (expressionName === "defn") {
            var name_1 = node.functionName.value;
            assertString(name_1);
            return name_1;
        }
        if (expressionName === "defns") {
            var name_2 = evaluateAstNode(node.functionName, contextStack);
            assertString(name_2);
            return name_2;
        }
        return undefined;
    }
    function createEvaluator(expressionName) {
        return function (node, contextStack, _a) {
            var _b;
            var evaluateAstNode = _a.evaluateAstNode, builtin = _a.builtin;
            var name = getFunctionName(expressionName, node, contextStack, evaluateAstNode);
            assertNameNotDefined(name, contextStack, builtin);
            var functionContext = {};
            for (var _i = 0, _c = node.arguments.bindings; _i < _c.length; _i++) {
                var binding = _c[_i];
                var bindingValueNode = binding.value;
                var bindingValue = evaluateAstNode(bindingValueNode, contextStack);
                functionContext[binding.name] = { value: bindingValue };
            }
            var optionalArguments = node.arguments.optionalArguments.map(function (optArg) {
                var name = optArg.name;
                var defaultValue = optArg.defaultValue;
                if (defaultValue) {
                    return {
                        name: name,
                        defaultValue: evaluateAstNode(defaultValue, contextStack),
                    };
                }
                return { name: name };
            });
            var lispishFunction = (_b = {},
                _b[FUNCTION_SYMBOL] = true,
                _b.type = "user-defined",
                _b.name = name,
                _b.arguments = {
                    mandatoryArguments: node.arguments.mandatoryArguments,
                    restArgument: node.arguments.restArgument,
                    optionalArguments: optionalArguments,
                },
                _b.body = node.body,
                _b.functionContext = functionContext,
                _b);
            if (expressionName === "fn") {
                return lispishFunction;
            }
            var globalContext = asNotUndefined(contextStack[contextStack.length - 2]);
            globalContext[name] = { value: lispishFunction };
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
    function parseFunctionArguments(tokens, position, parseArgument, parseBindings) {
        var _a;
        var bindings = [];
        var restArgument = undefined;
        var mandatoryArguments = [];
        var optionalArguments = [];
        var argNames = {};
        var state = "mandatory";
        var token = asNotUndefined(tokens[position]);
        if (!(token.type === "paren" && token.value === "[")) {
            throw new UnexpectedTokenError("[", token);
        }
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
                        case "&opt":
                            if (state === "rest") {
                                throw Error("&opt cannot appear after &rest");
                            }
                            if (state === "optional") {
                                throw Error("&opt can only appear once");
                            }
                            state = "optional";
                            break;
                        case "&rest":
                            if (state === "rest") {
                                throw Error("&rest can only appear once");
                            }
                            if (state === "optional" && optionalArguments.length === 0) {
                                throw Error("No optional arguments where spcified");
                            }
                            state = "rest";
                            break;
                        case "&let":
                            if (state === "optional" && optionalArguments.length === 0) {
                                throw Error("No optional arguments where spcified");
                            }
                            if (state === "rest" && !restArgument) {
                                throw Error("No rest argument was spcified");
                            }
                            state = "let";
                            break;
                        default:
                            throw Error("Illegal modifier: " + node.value);
                    }
                }
                else {
                    if (argNames[node.name]) {
                        throw Error("Duplicate argument \"" + node.name + "\"");
                    }
                    else {
                        argNames[node.name] = true;
                    }
                    if (Object.getOwnPropertyDescriptor(node, "defaultValue")) {
                        if (state !== "optional") {
                            throw Error("Cannot specify default value if not an optional argument");
                        }
                        optionalArguments.push({
                            name: node.name,
                            defaultValue: node.defaultValue,
                        });
                    }
                    else {
                        switch (state) {
                            case "mandatory":
                                mandatoryArguments.push(node.name);
                                break;
                            case "optional":
                                optionalArguments.push({
                                    name: node.name,
                                    defaultValue: undefined,
                                });
                                break;
                            case "rest":
                                if (restArgument !== undefined) {
                                    throw Error("Can only specify one rest argument");
                                }
                                restArgument = node.name;
                                break;
                        }
                    }
                }
            }
        }
        if (state === "rest" && restArgument === undefined) {
            throw Error("Missing rest argument name");
        }
        if (state === "optional" && optionalArguments.length === 0) {
            throw Error("No optional arguments where spcified");
        }
        position += 1;
        var args = {
            mandatoryArguments: mandatoryArguments,
            optionalArguments: optionalArguments,
            restArgument: restArgument,
            bindings: bindings,
        };
        return [position, args];
    }

    var defSpecialExpression = {
        parse: function (tokens, position, _a) {
            var parseTokens = _a.parseTokens;
            var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
            assertNameNode(params[0]);
            return [
                newPosition + 1,
                {
                    type: "SpecialExpression",
                    name: "def",
                    params: params,
                },
            ];
        },
        evaluate: function (node, contextStack, _a) {
            var evaluateAstNode = _a.evaluateAstNode, builtin = _a.builtin;
            var name = asNameNode(node.params[0]).value;
            assertNameNotDefined(name, contextStack, builtin);
            var value = evaluateAstNode(asAstNode(node.params[1]), contextStack);
            var context = asNotUndefined(contextStack[contextStack.length - 2]);
            context[name] = { value: value };
            return value;
        },
        validate: function (node) { return assertLength(2, node); },
    };

    var defsSpecialExpression = {
        parse: function (tokens, position, _a) {
            var parseTokens = _a.parseTokens;
            var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
            return [
                newPosition + 1,
                {
                    type: "SpecialExpression",
                    name: "defs",
                    params: params,
                },
            ];
        },
        evaluate: function (node, contextStack, _a) {
            var evaluateAstNode = _a.evaluateAstNode, builtin = _a.builtin;
            var name = evaluateAstNode(asAstNode(node.params[0]), contextStack);
            assertString(name);
            assertNameNotDefined(name, contextStack, builtin);
            var value = evaluateAstNode(asAstNode(node.params[1]), contextStack);
            var context = asNotUndefined(contextStack[contextStack.length - 2]);
            context[name] = { value: value };
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
            var newContextStack = __spreadArray([newContext], contextStack);
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
                        throw Error("Only one &let modifier allowed");
                    }
                    _c = parseBindings(tokens, position + 1), position = _c[0], loopBinding.letBindings = _c[1];
                    loopBinding.modifiers.push("&let");
                    break;
                case "&when":
                    if (loopBinding.whenNode) {
                        throw Error("Only one &when modifier allowed");
                    }
                    _d = parseToken(tokens, position + 1), position = _d[0], loopBinding.whenNode = _d[1];
                    loopBinding.modifiers.push("&when");
                    break;
                case "&while":
                    if (loopBinding.whileNode) {
                        throw Error("Only one &while modifier allowed");
                    }
                    _e = parseToken(tokens, position + 1), position = _e[0], loopBinding.whileNode = _e[1];
                    loopBinding.modifiers.push("&while");
                    break;
                default:
                    throw Error("Illegal modifier: " + token.value);
            }
            token = asNotUndefined(tokens[position]);
        }
        return [position, loopBinding];
    }
    function addToContext(bindings, context, contextStack, evaluateAstNode) {
        for (var _i = 0, bindings_1 = bindings; _i < bindings_1.length; _i++) {
            var binding = bindings_1[_i];
            if (context[binding.name]) {
                throw Error("Variable already defined: " + binding.name);
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
            };
            return [position + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var evaluateAstNode = _a.evaluateAstNode;
            var loopBindings = node.loopBindings, params = node.params;
            var expression = asNotUndefined(params[0]);
            var result = [];
            var bindingIndices = loopBindings.map(function () { return 0; });
            var abort = false;
            while (!abort) {
                var context = {};
                var newContextStack = __spreadArray([context], contextStack);
                var skip = false;
                bindingsLoop: for (var bindingIndex = 0; bindingIndex < loopBindings.length; bindingIndex += 1) {
                    var _b = asNotUndefined(loopBindings[bindingIndex]), binding = _b.binding, letBindings = _b.letBindings, whenNode = _b.whenNode, whileNode = _b.whileNode, modifiers = _b.modifiers;
                    var coll = asColl(evaluateAstNode(binding.value, newContextStack));
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
                        throw Error("Variable already defined: " + binding.name);
                    }
                    context[binding.name] = {
                        value: asAny(seq[index]),
                    };
                    for (var _i = 0, modifiers_1 = modifiers; _i < modifiers_1.length; _i++) {
                        var modifier = modifiers_1[_i];
                        switch (modifier) {
                            case "&let":
                                addToContext(asNotUndefined(letBindings), context, newContextStack, evaluateAstNode);
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
            var bindings;
            _b = parseBindings(tokens, position), position = _b[0], bindings = _b[1];
            if (bindings.length !== 1) {
                throw Error("Expected exactly one binding, got " + bindings.length);
            }
            var params;
            _c = parseTokens(tokens, position), position = _c[0], params = _c[1];
            var node = {
                type: "SpecialExpression",
                name: "if-let",
                binding: asNotUndefined(bindings[0]),
                params: params,
            };
            return [position + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var evaluateAstNode = _a.evaluateAstNode;
            var locals = {};
            var bindingValue = evaluateAstNode(node.binding.value, contextStack);
            if (bindingValue) {
                locals[node.binding.name] = { value: bindingValue };
                var newContextStack = __spreadArray([locals], contextStack);
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
            var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
            return [
                newPosition + 1,
                {
                    type: "SpecialExpression",
                    name: "if-not",
                    params: params,
                },
            ];
        },
        evaluate: function (node, contextStack, _a) {
            var evaluateAstNode = _a.evaluateAstNode;
            var _b = node.params, conditionNode = _b[0], trueNode = _b[1], falseNode = _b[2];
            if (!evaluateAstNode(asAstNode(conditionNode), contextStack)) {
                return evaluateAstNode(asAstNode(trueNode), contextStack);
            }
            else {
                if (node.params.length === 3) {
                    return evaluateAstNode(asAstNode(falseNode), contextStack);
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
            var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
            return [
                newPosition + 1,
                {
                    type: "SpecialExpression",
                    name: "if",
                    params: params,
                },
            ];
        },
        evaluate: function (node, contextStack, _a) {
            var evaluateAstNode = _a.evaluateAstNode;
            var _b = node.params, conditionNode = _b[0], trueNode = _b[1], falseNode = _b[2];
            if (evaluateAstNode(asAstNode(conditionNode), contextStack)) {
                return evaluateAstNode(asAstNode(trueNode), contextStack);
            }
            else {
                if (node.params.length === 3) {
                    return evaluateAstNode(asAstNode(falseNode), contextStack);
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
            var bindings;
            _b = parseBindings(tokens, position), position = _b[0], bindings = _b[1];
            var params;
            _c = parseTokens(tokens, position), position = _c[0], params = _c[1];
            var node = {
                type: "SpecialExpression",
                name: "let",
                params: params,
                bindings: bindings,
            };
            return [position + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var evaluateAstNode = _a.evaluateAstNode;
            var locals = {};
            var newContextStack = __spreadArray([locals], contextStack);
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
            var bindings;
            _b = parseBindings(tokens, position), position = _b[0], bindings = _b[1];
            var params;
            _c = parseTokens(tokens, position), position = _c[0], params = _c[1];
            var node = {
                type: "SpecialExpression",
                name: "loop",
                params: params,
                bindings: bindings,
            };
            return [position + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var evaluateAstNode = _a.evaluateAstNode;
            var bindingContext = node.bindings.reduce(function (result, binding) {
                result[binding.name] = { value: evaluateAstNode(binding.value, contextStack) };
                return result;
            }, {});
            var newContextStack = __spreadArray([bindingContext], contextStack);
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
                            throw Error("recur expected " + node.bindings.length + " parameters, got " + params_1.length);
                        }
                        node.bindings.forEach(function (binding, index) {
                            asNotUndefined(bindingContext[binding.name]).value = asAny(params_1[index]);
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
            var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
            return [
                newPosition + 1,
                {
                    type: "SpecialExpression",
                    name: "or",
                    params: params,
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
            var params;
            _b = parseTokens(tokens, position), position = _b[0], params = _b[1];
            var node = {
                type: "SpecialExpression",
                name: "recur",
                params: params,
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
            };
            return [position, node];
        },
        evaluate: function (node, contextStack, _a) {
            var evaluateAstNode = _a.evaluateAstNode;
            var message = asNonEmptyString(evaluateAstNode(node.messageNode, contextStack));
            throw new UserDefinedError(message);
        },
    };

    var timeSpecialExpression = {
        parse: function (tokens, position, _a) {
            var parseToken = _a.parseToken;
            var _b = parseToken(tokens, position), newPosition = _b[0], astNode = _b[1];
            var node = {
                type: "SpecialExpression",
                name: "time!",
                params: [astNode],
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
                throw new UnexpectedNodeTypeError("Name", error);
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
                return evaluateAstNode(node.catchExpression, __spreadArray([newContext], contextStack));
            }
        },
    };

    var whenFirstSpecialExpression = {
        parse: function (tokens, position, _a) {
            var _b, _c;
            var parseBindings = _a.parseBindings, parseTokens = _a.parseTokens;
            var bindings;
            _b = parseBindings(tokens, position), position = _b[0], bindings = _b[1];
            if (bindings.length !== 1) {
                throw Error("Expected exactly one binding, got " + bindings.length);
            }
            var params;
            _c = parseTokens(tokens, position), position = _c[0], params = _c[1];
            var node = {
                type: "SpecialExpression",
                name: "when-first",
                binding: asNotUndefined(bindings[0]),
                params: params,
            };
            return [position + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var evaluateAstNode = _a.evaluateAstNode;
            var locals = {};
            var evaluatedBindingForm = evaluateAstNode(node.binding.value, contextStack);
            if (!isSeq(evaluatedBindingForm)) {
                throw Error("Expected undefined or a sequence, got " + evaluatedBindingForm);
            }
            if (evaluatedBindingForm.length === 0) {
                return null;
            }
            var bindingValue = toAny(evaluatedBindingForm[0]);
            locals[node.binding.name] = { value: bindingValue };
            var newContextStack = __spreadArray([locals], contextStack);
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
            var bindings;
            _b = parseBindings(tokens, position), position = _b[0], bindings = _b[1];
            if (bindings.length !== 1) {
                throw Error("Expected exactly one binding, got " + bindings.length);
            }
            var params;
            _c = parseTokens(tokens, position), position = _c[0], params = _c[1];
            var node = {
                type: "SpecialExpression",
                name: "when-let",
                binding: asNotUndefined(bindings[0]),
                params: params,
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
            var newContextStack = __spreadArray([locals], contextStack);
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
            var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
            var node = {
                type: "SpecialExpression",
                name: "when-not",
                params: params,
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
            var _b = parseTokens(tokens, position), newPosition = _b[0], params = _b[1];
            var node = {
                type: "SpecialExpression",
                name: "when",
                params: params,
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
            evaluate: function (_a) {
                var number = _a[0], count = _a[1];
                assertInteger(number);
                assertNonNegativeInteger(count);
                return number << count;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'bit-shift-right': {
            evaluate: function (_a) {
                var number = _a[0], count = _a[1];
                assertInteger(number);
                assertNonNegativeInteger(count);
                return number >> count;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'bit-not': {
            evaluate: function (_a) {
                var number = _a[0];
                assertInteger(number);
                return ~number;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'bit-and': {
            evaluate: function (_a) {
                var first = _a[0], rest = _a.slice(1);
                assertInteger(first);
                return rest.reduce(function (result, value) {
                    assertInteger(value);
                    return result & value;
                }, first);
            },
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
        'bit-and-not': {
            evaluate: function (_a) {
                var first = _a[0], rest = _a.slice(1);
                assertInteger(first);
                return rest.reduce(function (result, value) {
                    assertInteger(value);
                    return result & ~value;
                }, first);
            },
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
        'bit-or': {
            evaluate: function (_a) {
                var first = _a[0], rest = _a.slice(1);
                assertInteger(first);
                return rest.reduce(function (result, value) {
                    assertInteger(value);
                    return result | value;
                }, first);
            },
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
        'bit-xor': {
            evaluate: function (_a) {
                var first = _a[0], rest = _a.slice(1);
                assertInteger(first);
                return rest.reduce(function (result, value) {
                    assertInteger(value);
                    return result ^ value;
                }, first);
            },
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
        'bit-flip': {
            evaluate: function (_a) {
                var number = _a[0], index = _a[1];
                assertInteger(number);
                assertNonNegativeInteger(index);
                var mask = 1 << index;
                return (number ^= mask);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'bit-set': {
            evaluate: function (_a) {
                var number = _a[0], index = _a[1];
                assertInteger(number);
                assertNonNegativeInteger(index);
                var mask = 1 << index;
                return (number |= mask);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'bit-clear': {
            evaluate: function (_a) {
                var number = _a[0], index = _a[1];
                assertInteger(number);
                assertNonNegativeInteger(index);
                var mask = 1 << index;
                return (number &= ~mask);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'bit-test': {
            evaluate: function (_a) {
                var number = _a[0], index = _a[1];
                assertInteger(number);
                assertNonNegativeInteger(index);
                var mask = 1 << index;
                return !!(number & mask);
            },
            validate: function (node) { return assertLength(2, node); },
        },
    };

    function cloneAndGetMeta(originalColl, keys) {
        var coll = cloneColl(originalColl);
        var butLastKeys = keys.slice(0, keys.length - 1);
        var innerCollMeta = butLastKeys.reduce(function (result, key) {
            var resultColl = result.coll;
            var newResultColl;
            if (isArr(resultColl)) {
                assertNumber(key);
                // eslint-disable-next-line @typescript-eslint/no-explicit-any
                newResultColl = asColl(resultColl[key]);
            }
            else {
                assertObj(resultColl);
                assertString(key);
                if (!collHasKey(result.coll, key)) {
                    resultColl[key] = {};
                }
                newResultColl = asColl(resultColl[key]);
            }
            return { coll: newResultColl, parent: resultColl };
        }, { coll: coll, parent: {} });
        return { coll: coll, innerCollMeta: innerCollMeta };
    }
    function get(coll, key) {
        if (isArr(coll)) {
            assertInteger(key);
            if (key < coll.length) {
                return toAny(coll[key]);
            }
        }
        else if (isObj(coll)) {
            assertString(key);
            if (collHasKey(coll, key)) {
                return toAny(coll[key]);
            }
        }
        else {
            assertInteger(key);
            if (key < coll.length) {
                return toAny(coll[key]);
            }
        }
        return undefined;
    }
    function update(coll, key, fn, params, contextStack, executeFunction) {
        if (isObj(coll)) {
            assertString(key);
            var result = __assign({}, coll);
            result[key] = executeFunction(fn, __spreadArray([result[key]], params), contextStack);
            return result;
        }
        else {
            assertNumber(key);
            var intKey_1 = toNonNegativeInteger(key);
            assertMax(intKey_1, coll.length);
            if (Array.isArray(coll)) {
                var result = coll.map(function (elem, index) {
                    if (intKey_1 === index) {
                        return executeFunction(fn, __spreadArray([elem], params), contextStack);
                    }
                    return elem;
                });
                if (intKey_1 === coll.length) {
                    result[intKey_1] = executeFunction(fn, __spreadArray([undefined], params), contextStack);
                }
                return result;
            }
            else {
                var result = coll.split("").map(function (elem, index) {
                    if (intKey_1 === index) {
                        return asChar(executeFunction(fn, __spreadArray([elem], params), contextStack));
                    }
                    return elem;
                });
                if (intKey_1 === coll.length) {
                    result[intKey_1] = asChar(executeFunction(fn, __spreadArray([undefined], params), contextStack));
                }
                return result.join("");
            }
        }
    }
    function assoc(coll, key, value) {
        assertColl(coll);
        assertStringOrNumber(key);
        if (Array.isArray(coll) || typeof coll === "string") {
            assertInteger(key);
            assertNumberGte(key, 0);
            assertNumberLte(key, coll.length);
            if (typeof coll === "string") {
                assertChar(value);
                return "" + coll.slice(0, key) + value + coll.slice(key + 1);
            }
            var copy_1 = __spreadArray([], coll);
            copy_1[key] = value;
            return copy_1;
        }
        assertString(key);
        var copy = __assign({}, coll);
        copy[key] = value;
        return copy;
    }
    var collectionNormalExpression = {
        get: {
            evaluate: function (params) {
                var coll = params[0], key = params[1];
                var defaultValue = toAny(params[2]);
                assertColl(coll);
                assertStringOrNumber(key);
                var result = get(coll, key);
                return result === undefined ? defaultValue : result;
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        'get-in': {
            evaluate: function (params) {
                var coll = params[0];
                var keys = params[1];
                var defaultValue = toAny(params[2]);
                assertColl(coll);
                assertArr(keys);
                for (var _i = 0, keys_1 = keys; _i < keys_1.length; _i++) {
                    var key = keys_1[_i];
                    assertStringOrNumber(key);
                    if (isColl(coll)) {
                        coll = get(coll, key);
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
            evaluate: function (_a) {
                var coll = _a[0];
                if (typeof coll === "string") {
                    return coll.length;
                }
                assertColl(coll);
                if (Array.isArray(coll)) {
                    return coll.length;
                }
                return Object.keys(coll).length;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'contains?': {
            evaluate: function (_a) {
                var coll = _a[0], key = _a[1];
                assertColl(coll);
                assertStringOrNumber(key);
                if (isSeq(coll)) {
                    if (!Number.isInteger(key)) {
                        return false;
                    }
                    assertInteger(key);
                    return key >= 0 && key < coll.length;
                }
                return !!Object.getOwnPropertyDescriptor(coll, key);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'has?': {
            evaluate: function (_a) {
                var coll = _a[0], value = _a[1];
                assertColl(coll);
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
            evaluate: function (_a) {
                var coll = _a[0], seq = _a[1];
                assertColl(coll);
                assertSeq(seq);
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
            evaluate: function (_a) {
                var coll = _a[0], seq = _a[1];
                assertColl(coll);
                assertSeq(seq);
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
            evaluate: function (_a) {
                var coll = _a[0], key = _a[1], value = _a[2];
                assertColl(coll);
                assertStringOrNumber(key);
                assertAny(value);
                return assoc(coll, key, value);
            },
            validate: function (node) { return assertLength(3, node); },
        },
        'assoc-in': {
            evaluate: function (_a) {
                var originalColl = _a[0], keys = _a[1], value = _a[2];
                assertColl(originalColl);
                assertArr(keys);
                assertAny(value);
                if (keys.length === 1) {
                    assertStringOrNumber(keys[0]);
                    return assoc(originalColl, keys[0], value);
                }
                var _b = cloneAndGetMeta(originalColl, keys), coll = _b.coll, innerCollMeta = _b.innerCollMeta;
                var lastKey = asStringOrNumber(keys[keys.length - 1]);
                var parentKey = asStringOrNumber(keys[keys.length - 2]);
                if (isArr(innerCollMeta.parent)) {
                    assertNumber(parentKey);
                    innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value);
                }
                else {
                    assertString(parentKey);
                    innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value);
                }
                return coll;
            },
            validate: function (node) { return assertLength(3, node); },
        },
        update: {
            evaluate: function (_a, contextStack, _b) {
                var coll = _a[0], key = _a[1], fn = _a[2], params = _a.slice(3);
                var executeFunction = _b.executeFunction;
                assertColl(coll);
                assertStringOrNumber(key);
                assertLispishFunction(fn);
                return update(coll, key, fn, params, contextStack, executeFunction);
            },
            validate: function (node) { return assertLength({ min: 3 }, node); },
        },
        'update-in': {
            evaluate: function (_a, contextStack, _b) {
                var originalColl = _a[0], keys = _a[1], fn = _a[2], params = _a.slice(3);
                var executeFunction = _b.executeFunction;
                assertColl(originalColl);
                assertArr(keys);
                assertLispishFunction(fn);
                if (keys.length === 1) {
                    assertStringOrNumber(keys[0]);
                    return update(originalColl, keys[0], fn, params, contextStack, executeFunction);
                }
                var _c = cloneAndGetMeta(originalColl, keys), coll = _c.coll, innerCollMeta = _c.innerCollMeta;
                var lastKey = asStringOrNumber(keys[keys.length - 1]);
                var parentKey = asStringOrNumber(keys[keys.length - 2]);
                if (isArr(innerCollMeta.parent)) {
                    assertNumber(parentKey);
                    innerCollMeta.parent[parentKey] = update(innerCollMeta.coll, lastKey, fn, params, contextStack, executeFunction);
                }
                else {
                    assertString(parentKey);
                    innerCollMeta.parent[parentKey] = update(innerCollMeta.coll, lastKey, fn, params, contextStack, executeFunction);
                }
                return coll;
            },
            validate: function (node) { return assertLength({ min: 3 }, node); },
        },
        concat: {
            evaluate: function (params) {
                assertColl(params[0]);
                if (isArr(params[0])) {
                    return params.reduce(function (result, arr) {
                        assertArr(arr);
                        return result.concat(arr);
                    }, []);
                }
                else if (isString(params[0])) {
                    return params.reduce(function (result, s) {
                        assertString(s);
                        return "" + result + s;
                    }, "");
                }
                else {
                    return params.reduce(function (result, obj) {
                        assertObj(obj);
                        return Object.assign(result, obj);
                    }, {});
                }
            },
            validate: function (node) { return assertLength({ min: 1 }, node); },
        },
        'empty?': {
            evaluate: function (_a) {
                var first = _a[0];
                assertColl(first);
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
            evaluate: function (_a, contextStack, _b) {
                var fn = _a[0], coll = _a[1];
                var executeFunction = _b.executeFunction;
                assertLispishFunction(fn);
                assertColl(coll);
                if (Array.isArray(coll)) {
                    return coll.every(function (elem) { return executeFunction(fn, [elem], contextStack); });
                }
                if (isString(coll)) {
                    return coll.split("").every(function (elem) { return executeFunction(fn, [elem], contextStack); });
                }
                return Object.entries(coll).every(function (elem) { return executeFunction(fn, [elem], contextStack); });
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'any?': {
            evaluate: function (_a, contextStack, _b) {
                var fn = _a[0], coll = _a[1];
                var executeFunction = _b.executeFunction;
                assertLispishFunction(fn);
                assertColl(coll);
                if (Array.isArray(coll)) {
                    return coll.some(function (elem) { return executeFunction(fn, [elem], contextStack); });
                }
                if (isString(coll)) {
                    return coll.split("").some(function (elem) { return executeFunction(fn, [elem], contextStack); });
                }
                return Object.entries(coll).some(function (elem) { return executeFunction(fn, [elem], contextStack); });
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'not-any?': {
            evaluate: function (_a, contextStack, _b) {
                var fn = _a[0], coll = _a[1];
                var executeFunction = _b.executeFunction;
                assertLispishFunction(fn);
                assertColl(coll);
                if (Array.isArray(coll)) {
                    return !coll.some(function (elem) { return executeFunction(fn, [elem], contextStack); });
                }
                if (isString(coll)) {
                    return !coll.split("").some(function (elem) { return executeFunction(fn, [elem], contextStack); });
                }
                return !Object.entries(coll).some(function (elem) { return executeFunction(fn, [elem], contextStack); });
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'not-every?': {
            evaluate: function (_a, contextStack, _b) {
                var fn = _a[0], coll = _a[1];
                var executeFunction = _b.executeFunction;
                assertLispishFunction(fn);
                assertColl(coll);
                if (Array.isArray(coll)) {
                    return !coll.every(function (elem) { return executeFunction(fn, [elem], contextStack); });
                }
                if (isString(coll)) {
                    return !coll.split("").every(function (elem) { return executeFunction(fn, [elem], contextStack); });
                }
                return !Object.entries(coll).every(function (elem) { return executeFunction(fn, [elem], contextStack); });
            },
            validate: function (node) { return assertLength(2, node); },
        },
    };

    var evaluateMap = function (params, contextStack, _a) {
        var executeFunction = _a.executeFunction;
        var fn = params[0], firstList = params[1];
        assertLispishFunction(fn);
        assertSeq(firstList);
        var isStringSeq = isString(firstList);
        var length = firstList.length;
        if (params.length === 2) {
            if (isArr(firstList)) {
                return firstList.map(function (elem) { return executeFunction(fn, [elem], contextStack); });
            }
            else {
                return firstList
                    .split("")
                    .map(function (elem) {
                    var newVal = executeFunction(fn, [elem], contextStack);
                    assertChar(newVal);
                    return newVal;
                })
                    .join("");
            }
        }
        else {
            params.slice(2).forEach(function (collParam) {
                if (isStringSeq) {
                    assertString(collParam);
                }
                else {
                    assertArr(collParam);
                }
                if (length !== collParam.length) {
                    throw Error("All arguments to \"map\" must have the same length");
                }
            });
            if (isStringSeq) {
                var result = "";
                var _loop_1 = function (i) {
                    var fnParams = params.slice(1).map(function (l) { return l[i]; });
                    var newValue = executeFunction(fn, fnParams, contextStack);
                    assertChar(newValue);
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
                    result.push(executeFunction(fn, fnParams, contextStack));
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
            evaluate: function (_a) {
                var elem = _a[0], seq = _a[1];
                assertAny(elem);
                assertSeq(seq);
                if (Array.isArray(seq)) {
                    return __spreadArray([elem], seq);
                }
                assertChar(elem);
                return "" + elem + seq;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        nth: {
            evaluate: function (_a) {
                var seq = _a[0], i = _a[1];
                assertSeq(seq);
                assertInteger(i);
                return toAny(seq[i]);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        filter: {
            evaluate: function (_a, contextStack, _b) {
                var fn = _a[0], seq = _a[1];
                var executeFunction = _b.executeFunction;
                assertLispishFunction(fn);
                assertSeq(seq);
                if (Array.isArray(seq)) {
                    return seq.filter(function (elem) { return executeFunction(fn, [elem], contextStack); });
                }
                return seq
                    .split("")
                    .filter(function (elem) { return executeFunction(fn, [elem], contextStack); })
                    .join("");
            },
            validate: function (node) { return assertLength(2, node); },
        },
        first: {
            evaluate: function (_a) {
                var array = _a[0];
                assertSeq(array);
                return toAny(array[0]);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        last: {
            evaluate: function (_a) {
                var first = _a[0];
                assertSeq(first);
                return toAny(first[first.length - 1]);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        map: {
            evaluate: evaluateMap,
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
        pop: {
            evaluate: function (_a) {
                var seq = _a[0];
                assertSeq(seq);
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
            evaluate: function (_a, contextStack, _b) {
                var fn = _a[0], seq = _a[1];
                var executeFunction = _b.executeFunction;
                assertLispishFunction(fn);
                assertSeq(seq);
                if (isString(seq)) {
                    var index = seq.split("").findIndex(function (elem) { return executeFunction(fn, [elem], contextStack); });
                    return index !== -1 ? index : null;
                }
                else {
                    var index = seq.findIndex(function (elem) { return executeFunction(fn, [elem], contextStack); });
                    return index !== -1 ? index : null;
                }
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'index-of': {
            evaluate: function (_a) {
                var seq = _a[0], value = _a[1];
                assertAny(value);
                assertSeq(seq);
                if (isString(seq)) {
                    assertString(value);
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
            evaluate: function (_a) {
                var seq = _a[0], values = _a.slice(1);
                assertSeq(seq);
                if (isString(seq)) {
                    assertCharArray(values);
                    return __spreadArray([seq], values).join("");
                }
                else {
                    return __spreadArray(__spreadArray([], seq, true), values);
                }
            },
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
        reduce: {
            evaluate: function (params, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var fn = params[0];
                assertLispishFunction(fn);
                if (params.length === 2) {
                    var arr = params[1];
                    assertSeq(arr);
                    if (arr.length === 0) {
                        return executeFunction(fn, [], contextStack);
                    }
                    else if (arr.length === 1) {
                        return toAny(arr[0]);
                    }
                    if (isString(arr)) {
                        var chars = arr.split("");
                        return chars.slice(1).reduce(function (result, elem) {
                            var val = executeFunction(fn, [result, elem], contextStack);
                            return val;
                        }, asAny(chars[0]));
                    }
                    else {
                        return arr.slice(1).reduce(function (result, elem) {
                            return executeFunction(fn, [result, elem], contextStack);
                        }, toAny(arr[0]));
                    }
                }
                else {
                    var val = params[1], seq = params[2];
                    assertAny(val);
                    assertSeq(seq);
                    if (isString(seq)) {
                        assertString(val);
                        if (seq.length === 0) {
                            return val;
                        }
                        return seq.split("").reduce(function (result, elem) {
                            var newVal = executeFunction(fn, [result, elem], contextStack);
                            return newVal;
                        }, val);
                    }
                    else {
                        if (seq.length === 0) {
                            return val;
                        }
                        return seq.reduce(function (result, elem) {
                            return executeFunction(fn, [result, elem], contextStack);
                        }, val);
                    }
                }
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        'reduce-right': {
            evaluate: function (params, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var fn = params[0];
                assertLispishFunction(fn);
                if (params.length === 2) {
                    var seq = params[1];
                    assertSeq(seq);
                    if (seq.length === 0) {
                        return executeFunction(fn, [], contextStack);
                    }
                    else if (seq.length === 1) {
                        return toAny(seq[0]);
                    }
                    if (isString(seq)) {
                        var chars = seq.split("");
                        return chars.slice(0, chars.length - 1).reduceRight(function (result, elem) {
                            var newVal = executeFunction(fn, [result, elem], contextStack);
                            assertString(newVal);
                            return newVal;
                        }, chars[chars.length - 1]);
                    }
                    else {
                        return seq.slice(0, seq.length - 1).reduceRight(function (result, elem) {
                            return executeFunction(fn, [result, elem], contextStack);
                        }, asAny(seq[seq.length - 1]));
                    }
                }
                else {
                    var val = params[1], seq = params[2];
                    assertAny(val);
                    assertSeq(seq);
                    if (isString(seq)) {
                        if (seq.length === 0) {
                            return val;
                        }
                        return seq.split("").reduceRight(function (result, elem) {
                            var newVal = executeFunction(fn, [result, elem], contextStack);
                            return newVal;
                        }, val);
                    }
                    else {
                        if (seq.length === 0) {
                            return val;
                        }
                        return seq.reduceRight(function (result, elem) {
                            return executeFunction(fn, [result, elem], contextStack);
                        }, val);
                    }
                }
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        rest: {
            evaluate: function (_a) {
                var first = _a[0];
                assertSeq(first);
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
            evaluate: function (_a) {
                var seq = _a[0], count = _a[1];
                assertSeq(seq);
                assertFiniteNumber(count);
                var integerCount = Math.max(Math.ceil(count), 0);
                if (Array.isArray(seq)) {
                    return seq.slice(integerCount);
                }
                return seq.substr(integerCount);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        next: {
            evaluate: function (_a) {
                var first = _a[0];
                assertSeq(first);
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
            evaluate: function (_a) {
                var seq = _a[0], count = _a[1];
                assertSeq(seq);
                assertFiniteNumber(count);
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
            evaluate: function (_a) {
                var first = _a[0];
                assertSeq(first);
                if (Array.isArray(first)) {
                    return __spreadArray([], first).reverse();
                }
                return first.split("").reverse().join("");
            },
            validate: function (node) { return assertLength(1, node); },
        },
        second: {
            evaluate: function (_a) {
                var array = _a[0];
                assertSeq(array);
                return toAny(array[1]);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        shift: {
            evaluate: function (_a) {
                var seq = _a[0];
                assertSeq(seq);
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
            evaluate: function (params) {
                var seq = params[0], from = params[1], to = params[2];
                assertSeq(seq);
                if (params.length === 1) {
                    return seq;
                }
                assertInteger(from);
                if (params.length === 2) {
                    return seq.slice(from);
                }
                assertInteger(to);
                return seq.slice(from, to);
            },
            validate: function (node) { return assertLength({ min: 1, max: 3 }, node); },
        },
        some: {
            evaluate: function (_a, contextStack, _b) {
                var _c;
                var fn = _a[0], seq = _a[1];
                var executeFunction = _b.executeFunction;
                assertLispishFunction(fn);
                assertSeq(seq);
                if (seq.length === 0) {
                    return null;
                }
                if (isString(seq)) {
                    return (_c = seq.split("").find(function (elem) { return executeFunction(fn, [elem], contextStack); })) !== null && _c !== void 0 ? _c : null;
                }
                return toAny(seq.find(function (elem) { return executeFunction(fn, [elem], contextStack); }));
            },
            validate: function (node) { return assertLength(2, node); },
        },
        sort: {
            evaluate: function (params, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var defaultComparer = params.length === 1;
                var seq = defaultComparer ? params[0] : params[1];
                var comparer = defaultComparer ? null : params[0];
                assertSeq(seq);
                if (isString(seq)) {
                    var result_1 = seq.split("");
                    if (defaultComparer) {
                        result_1.sort(compare);
                    }
                    else {
                        assertLispishFunction(comparer);
                        result_1.sort(function (a, b) {
                            var compareValue = executeFunction(comparer, [a, b], contextStack);
                            assertFiniteNumber(compareValue);
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
                        assertLispishFunction(comparer);
                        var compareValue = executeFunction(comparer, [a, b], contextStack);
                        assertFiniteNumber(compareValue);
                        return compareValue;
                    });
                }
                return result;
            },
            validate: function (node) { return assertLength({ min: 1, max: 2 }, node); },
        },
        'sort-by': {
            evaluate: function (params, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var defaultComparer = params.length === 2;
                var keyfn = asAny(params[0]);
                var comparer = defaultComparer ? null : params[1];
                var seq = asSeq(defaultComparer ? params[1] : params[2]);
                if (isString(seq)) {
                    var result_2 = seq.split("");
                    if (defaultComparer) {
                        result_2.sort(function (a, b) {
                            var aKey = executeFunction(keyfn, [a], contextStack);
                            var bKey = executeFunction(keyfn, [b], contextStack);
                            return compare(aKey, bKey);
                        });
                    }
                    else {
                        assertLispishFunction(comparer);
                        result_2.sort(function (a, b) {
                            var aKey = executeFunction(keyfn, [a], contextStack);
                            var bKey = executeFunction(keyfn, [b], contextStack);
                            var compareValue = executeFunction(comparer, [aKey, bKey], contextStack);
                            assertFiniteNumber(compareValue);
                            return compareValue;
                        });
                    }
                    return result_2.join("");
                }
                var result = __spreadArray([], seq);
                if (defaultComparer) {
                    result.sort(function (a, b) {
                        var aKey = executeFunction(keyfn, [a], contextStack);
                        var bKey = executeFunction(keyfn, [b], contextStack);
                        return compare(aKey, bKey);
                    });
                }
                else {
                    assertLispishFunction(comparer);
                    result.sort(function (a, b) {
                        var aKey = executeFunction(keyfn, [a], contextStack);
                        var bKey = executeFunction(keyfn, [b], contextStack);
                        var compareValue = executeFunction(comparer, [aKey, bKey], contextStack);
                        assertFiniteNumber(compareValue);
                        return compareValue;
                    });
                }
                return result;
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        take: {
            evaluate: function (_a) {
                var n = _a[0], input = _a[1];
                assertNumber(n);
                assertSeq(input);
                var number = Math.max(Math.ceil(n), 0);
                return input.slice(0, number);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'take-last': {
            evaluate: function (_a) {
                var n = _a[0], array = _a[1];
                assertSeq(array);
                assertNumber(n);
                var number = Math.max(Math.ceil(n), 0);
                var from = array.length - number;
                return array.slice(from);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'take-while': {
            evaluate: function (_a, contextStack, _b) {
                var fn = _a[0], seq = _a[1];
                var executeFunction = _b.executeFunction;
                assertSeq(seq);
                assertLispishFunction(fn);
                var result = [];
                for (var _i = 0, seq_1 = seq; _i < seq_1.length; _i++) {
                    var item = seq_1[_i];
                    if (executeFunction(fn, [item], contextStack)) {
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
            evaluate: function (_a) {
                var n = _a[0], input = _a[1];
                assertNumber(n);
                var number = Math.max(Math.ceil(n), 0);
                assertSeq(input);
                return input.slice(number);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'drop-last': {
            evaluate: function (_a) {
                var n = _a[0], array = _a[1];
                assertSeq(array);
                assertNumber(n);
                var number = Math.max(Math.ceil(n), 0);
                var from = array.length - number;
                return array.slice(0, from);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'drop-while': {
            evaluate: function (_a, contextStack, _b) {
                var fn = _a[0], seq = _a[1];
                var executeFunction = _b.executeFunction;
                assertSeq(seq);
                assertLispishFunction(fn);
                if (Array.isArray(seq)) {
                    var from_1 = seq.findIndex(function (elem) { return !executeFunction(fn, [elem], contextStack); });
                    return seq.slice(from_1);
                }
                var charArray = seq.split("");
                var from = charArray.findIndex(function (elem) { return !executeFunction(fn, [elem], contextStack); });
                return charArray.slice(from).join("");
            },
            validate: function (node) { return assertLength(2, node); },
        },
        unshift: {
            evaluate: function (_a) {
                var seq = _a[0], values = _a.slice(1);
                assertSeq(seq);
                if (isString(seq)) {
                    assertCharArray(values);
                    return __spreadArray(__spreadArray([], values, true), [seq]).join("");
                }
                var copy = __spreadArray([], seq);
                copy.unshift.apply(copy, values);
                return copy;
            },
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
        'random-sample!': {
            evaluate: function (_a) {
                var prob = _a[0], seq = _a[1];
                assertFiniteNumber(prob);
                assertSeq(seq);
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
            evaluate: function (_a) {
                var seq = _a[0];
                assertSeq(seq);
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
            evaluate: function (_a) {
                var input = _a[0];
                assertSeq(input);
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
            evaluate: function (_a) {
                var input = _a[0];
                assertSeq(input);
                if (Array.isArray(input)) {
                    return Array.from(new Set(input));
                }
                return Array.from(new Set(input.split(""))).join("");
            },
            validate: function (node) { return assertLength(1, node); },
        },
        remove: {
            evaluate: function (_a, contextStack, _b) {
                var fn = _a[0], input = _a[1];
                var executeFunction = _b.executeFunction;
                assertLispishFunction(fn);
                assertSeq(input);
                if (Array.isArray(input)) {
                    return input.filter(function (elem) { return !executeFunction(fn, [elem], contextStack); });
                }
                return input
                    .split("")
                    .filter(function (elem) { return !executeFunction(fn, [elem], contextStack); })
                    .join("");
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'remove-at': {
            evaluate: function (_a) {
                var index = _a[0], input = _a[1];
                assertNumber(index);
                assertSeq(input);
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
            evaluate: function (_a) {
                var pos = _a[0], seq = _a[1];
                assertFiniteNumber(pos);
                var intPos = toNonNegativeInteger(pos);
                assertSeq(seq);
                return [seq.slice(0, intPos), seq.slice(intPos)];
            },
            validate: function (node) { return assertLength(2, node); },
        },
        'split-with': {
            evaluate: function (_a, contextStack, _b) {
                var fn = _a[0], seq = _a[1];
                var executeFunction = _b.executeFunction;
                assertLispishFunction(fn);
                assertSeq(seq);
                var seqIsArray = Array.isArray(seq);
                var arr = seqIsArray ? seq : seq.split("");
                var index = arr.findIndex(function (elem) { return !executeFunction(fn, [elem], contextStack); });
                if (index === -1) {
                    return [seq, seqIsArray ? [] : ""];
                }
                return [seq.slice(0, index), seq.slice(index)];
            },
            validate: function (node) { return assertLength(2, node); },
        },
        frequencies: {
            evaluate: function (_a) {
                var seq = _a[0];
                assertSeq(seq);
                var arr = isString(seq) ? seq.split("") : seq;
                return arr.reduce(function (result, val) {
                    assertString(val);
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
            evaluate: function (_a, contextStack, _b) {
                var fn = _a[0], seq = _a[1];
                var executeFunction = _b.executeFunction;
                assertAny(fn);
                assertSeq(seq);
                var arr = Array.isArray(seq) ? seq : seq.split("");
                return arr.reduce(function (result, val) {
                    var key = executeFunction(fn, [val], contextStack);
                    assertString(key);
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
            evaluate: function (params) {
                var len = params.length;
                var n = toNonNegativeInteger(asNumber(params[0]));
                var seq = len === 2 ? asSeq(params[1]) : len === 3 ? asSeq(params[2]) : asSeq(params[3]);
                var step = len >= 3 ? toNonNegativeInteger(asNumber(params[1])) : n;
                var pad = len === 4 ? (params[2] === null ? [] : asArr(params[2])) : undefined;
                return partition(n, step, seq, pad);
            },
            validate: function (node) { return assertLength({ min: 2, max: 4 }, node); },
        },
        'partition-all': {
            evaluate: function (params) {
                var len = params.length;
                var n = toNonNegativeInteger(asNumber(params[0]));
                var seq = len === 2 ? asSeq(params[1]) : asSeq(params[2]);
                var step = len >= 3 ? toNonNegativeInteger(asNumber(params[1])) : n;
                return partition(n, step, seq, []);
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        'partition-by': {
            evaluate: function (_a, contextStack, _b) {
                var fn = _a[0], seq = _a[1];
                var executeFunction = _b.executeFunction;
                assertLispishFunction(fn);
                assertSeq(seq);
                var isStringSeq = isString(seq);
                var oldValue = undefined;
                var result = (isStringSeq ? seq.split("") : seq).reduce(function (result, elem) {
                    var value = executeFunction(fn, [elem], contextStack);
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
    function partition(n, step, seq, pad) {
        assertPositiveNumber(step);
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
            evaluate: function (params) {
                var first = params[0], second = params[1], third = params[2];
                var from;
                var to;
                var step;
                assertFiniteNumber(first);
                if (params.length === 1) {
                    from = 0;
                    to = first;
                    step = to >= 0 ? 1 : -1;
                }
                else if (params.length === 2) {
                    assertFiniteNumber(second);
                    from = first;
                    to = second;
                    step = to >= from ? 1 : -1;
                }
                else {
                    assertFiniteNumber(second);
                    assertFiniteNumber(third);
                    from = first;
                    to = second;
                    step = third;
                    if (to > from) {
                        assertPositiveNumber(step);
                    }
                    else if (to < from) {
                        assertNegativeNumber(step);
                    }
                    else {
                        assertNumberNotZero(step);
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
            evaluate: function (_a) {
                var count = _a[0], value = _a[1];
                assertNonNegativeInteger(count);
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
            evaluate: function (params, contextStack, helpers) {
                params.slice(1).forEach(function (arr) {
                    assertArr(arr);
                });
                var mapResult = evaluateMap(params, contextStack, helpers);
                assertArr(mapResult);
                return mapResult.flat(1);
            },
            validate: function (node) { return assertLength({ min: 2 }, node); },
        },
    };

    var mathNormalExpression = {
        inc: {
            evaluate: function (_a) {
                var first = _a[0];
                assertNumber(first);
                return first + 1;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        dec: {
            evaluate: function (_a) {
                var first = _a[0];
                assertNumber(first);
                return first - 1;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        '+': {
            evaluate: function (params) {
                return params.reduce(function (result, param) {
                    assertNumber(param);
                    return result + param;
                }, 0);
            },
        },
        '*': {
            evaluate: function (params) {
                return params.reduce(function (result, param) {
                    assertNumber(param);
                    return result * param;
                }, 1);
            },
        },
        '/': {
            evaluate: function (params) {
                if (params.length === 0) {
                    return 1;
                }
                var first = params[0], rest = params.slice(1);
                assertNumber(first);
                if (rest.length === 0) {
                    assertNumber(first);
                    return 1 / first;
                }
                return rest.reduce(function (result, param) {
                    assertNumber(param);
                    return result / param;
                }, first);
            },
        },
        '-': {
            evaluate: function (_a) {
                var first = _a[0], rest = _a.slice(1);
                if (!first) {
                    return 0;
                }
                assertNumber(first);
                if (rest.length === 0) {
                    return -first;
                }
                return rest.reduce(function (result, param) {
                    assertNumber(param);
                    return result - param;
                }, first);
            },
        },
        quot: {
            evaluate: function (_a) {
                var dividend = _a[0], divisor = _a[1];
                assertNumber(dividend);
                assertNumber(divisor);
                var quotient = Math.trunc(dividend / divisor);
                return quotient;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        mod: {
            evaluate: function (_a) {
                var dividend = _a[0], divisor = _a[1];
                assertNumber(dividend);
                assertNumber(divisor);
                var quotient = Math.floor(dividend / divisor);
                return dividend - divisor * quotient;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        rem: {
            evaluate: function (_a) {
                var dividend = _a[0], divisor = _a[1];
                assertNumber(dividend);
                assertNumber(divisor);
                var quotient = Math.trunc(dividend / divisor);
                return dividend - divisor * quotient;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        sqrt: {
            evaluate: function (_a) {
                var first = _a[0];
                assertNumber(first);
                return Math.sqrt(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        cbrt: {
            evaluate: function (_a) {
                var first = _a[0];
                assertNumber(first);
                return Math.cbrt(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        pow: {
            evaluate: function (_a) {
                var first = _a[0], second = _a[1];
                assertNumber(first);
                assertNumber(second);
                return Math.pow(first, second);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        round: {
            evaluate: function (params) {
                var value = params[0], decimals = params[1];
                assertNumber(value);
                if (params.length === 1 || decimals === 0) {
                    return Math.round(value);
                }
                assertNonNegativeInteger(decimals);
                var factor = Math.pow(10, decimals);
                return Math.round(value * factor) / factor;
            },
            validate: function (node) { return assertLength({ min: 1, max: 2 }, node); },
        },
        trunc: {
            evaluate: function (_a) {
                var first = _a[0];
                assertNumber(first);
                return Math.trunc(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        floor: {
            evaluate: function (_a) {
                var first = _a[0];
                assertNumber(first);
                return Math.floor(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        ceil: {
            evaluate: function (_a) {
                var first = _a[0];
                assertNumber(first);
                return Math.ceil(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'rand!': {
            evaluate: function (parameters) {
                var number = parameters.length === 1 ? parameters[0] : 1;
                assertNumber(number);
                return Math.random() * number;
            },
            validate: function (node) { return assertLength({ min: 0, max: 1 }, node); },
        },
        'rand-int!': {
            evaluate: function (_a) {
                var first = _a[0];
                assertNumber(first);
                return Math.floor(Math.random() * Math.abs(first)) * Math.sign(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        min: {
            evaluate: function (_a) {
                var first = _a[0], rest = _a.slice(1);
                assertNumber(first);
                if (rest.length === 0) {
                    return first;
                }
                return rest.reduce(function (min, value) {
                    assertNumber(value);
                    return Math.min(min, value);
                }, first);
            },
            validate: function (node) { return assertLength({ min: 1 }, node); },
        },
        max: {
            evaluate: function (_a) {
                var first = _a[0], rest = _a.slice(1);
                assertNumber(first);
                if (rest.length === 0) {
                    return first;
                }
                return rest.reduce(function (min, value) {
                    assertNumber(value);
                    return Math.max(min, value);
                }, first);
            },
            validate: function (node) { return assertLength({ min: 1 }, node); },
        },
        abs: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.abs(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        sign: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
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
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.exp(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        log: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.log(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        log2: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.log2(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        log10: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.log10(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        sin: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.sin(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        asin: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.asin(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        sinh: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.sinh(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        asinh: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.asinh(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        cos: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.cos(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        acos: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.acos(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        cosh: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.cosh(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        acosh: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.acosh(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        tan: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.tan(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        atan: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.atan(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        tanh: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.tanh(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        atanh: {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Math.atanh(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
    };

    var delimiterRegExp = /[[.]/;
    function getPath(obj, path) {
        var destructedPath = destructPath(path);
        for (var _i = 0, destructedPath_1 = destructedPath; _i < destructedPath_1.length; _i++) {
            var part = destructedPath_1[_i];
            try {
                // eslint-disable-next-line @typescript-eslint/no-explicit-any
                obj = toAny(obj[part]);
            }
            catch (_a) {
                return null;
            }
        }
        return obj;
    }
    function destructPath(path) {
        if (!path) {
            return [];
        }
        var match = delimiterRegExp.exec(path);
        if (!match) {
            return [path];
        }
        if (match.index > 0) {
            return __spreadArray([path.substring(0, match.index)], destructPath(path.substring(match.index)));
        }
        if (path[0] === ".") {
            if (path.length < 2) {
                throw Error("Ill formed path: " + path);
            }
            return destructPath(path.substring(1));
        }
        var _a = parseBracketNotation(path), length = _a[0], value = _a[1];
        if (path.length > length && path[length] !== "." && path[length] !== "[") {
            throw Error("Ill formed path: " + path);
        }
        return __spreadArray([value], destructPath(path.substring(length)));
    }
    var singleQuoteBracketStringRegExp = /^\[\s*'(.*)'\s*\]/;
    var doubleQuoteBracketStringRegExp = /^\[\s*"(.*)"\s*\]/;
    var numberBracketStringRegExp = /^\[\s*(\d+)\s*\]/;
    function parseBracketNotation(path) {
        var stringMatch = singleQuoteBracketStringRegExp.exec(path) || doubleQuoteBracketStringRegExp.exec(path);
        if (stringMatch) {
            var length_1 = stringMatch[0].length;
            var value = stringMatch[1];
            return [length_1, value];
        }
        var numberMatch = numberBracketStringRegExp.exec(path);
        if (numberMatch) {
            var length_2 = numberMatch[0].length;
            var value = Number(numberMatch[1]);
            return [length_2, value];
        }
        throw Error("Ill formed path: " + path);
    }

    var version = "1.0.0-alpha.7";

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
            evaluate: function (_a) {
                var a = _a[0], b = _a[1];
                return deepEqual(asAny(a), asAny(b));
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
        'get-path': {
            evaluate: function (_a) {
                var first = _a[0], second = _a[1];
                assertObjectOrArray(first);
                assertString(second);
                return getPath(first, second);
            },
            validate: function (node) { return assertLength(2, node); },
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
            evaluate: function (params) {
                // eslint-disable-next-line no-console
                console.log.apply(console, params);
                if (params.length > 0) {
                    return asAny(params[params.length - 1]);
                }
                return null;
            },
        },
        'debug!': {
            evaluate: function (params, contextStack) {
                if (params.length === 0) {
                    // eslint-disable-next-line no-console
                    console.warn("*** LISPISH DEBUG ***\n" + contextstackToString(contextStack) + "\n");
                    return null;
                }
                // eslint-disable-next-line no-console
                console.warn("*** LISPISH DEBUG ***\n" + JSON.stringify(params[0], null, 2) + "\n");
                return asAny(params[0]);
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
            evaluate: function (params) {
                var value = params[0];
                var message = params.length === 2 ? params[1] : "" + value;
                assertString(message);
                if (!value) {
                    throw new AssertionError(message);
                }
                return asAny(value);
            },
            validate: function (node) { return assertLength({ min: 1, max: 2 }, node); },
        },
        'lispish-version': {
            evaluate: function () {
                return version;
            },
            validate: function (node) { return assertLength(0, node); },
        },
    };
    function contextstackToString(contextStack) {
        return __spreadArray([], contextStack).reverse().reduce(function (result, context, index) {
            return result + "Context " + index + (index === 0 ? " - Import context" : index === 1 ? " - Global context" : "") + "\n" + contextToString(context) + "\n";
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
        if (isLispishFunction(value)) {
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
            evaluate: function (params) {
                var result = {};
                for (var i = 0; i < params.length; i += 2) {
                    var key = params[i];
                    var value = params[i + 1];
                    assertString(key);
                    result[key] = value;
                }
                return result;
            },
            validate: function (node) { return assertLengthEven(node); },
        },
        keys: {
            evaluate: function (_a) {
                var first = _a[0];
                assertObj(first);
                return Object.keys(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        vals: {
            evaluate: function (_a) {
                var first = _a[0];
                assertObj(first);
                return Object.values(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        entries: {
            evaluate: function (_a) {
                var first = _a[0];
                assertObj(first);
                return Object.entries(first);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        find: {
            evaluate: function (_a) {
                var obj = _a[0], key = _a[1];
                assertObj(obj);
                assertString(key);
                if (collHasKey(obj, key)) {
                    return [key, obj[key]];
                }
                return null;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        dissoc: {
            evaluate: function (_a) {
                var obj = _a[0], key = _a[1];
                assertObj(obj);
                assertString(key);
                var result = toAny(obj[key]);
                delete obj[key];
                return result;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        merge: {
            evaluate: function (params) {
                if (params.length === 0) {
                    return null;
                }
                var first = params[0], rest = params.slice(1);
                assertObj(first);
                return rest.reduce(function (result, obj) {
                    assertObj(obj);
                    return __assign(__assign({}, result), obj);
                }, __assign({}, first));
            },
            validate: function (node) { return assertLength({ min: 0 }, node); },
        },
        'merge-with': {
            evaluate: function (params, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var fn = params[0], first = params[1], rest = params.slice(2);
                assertLispishFunction(fn);
                if (params.length === 1) {
                    return null;
                }
                assertObj(first);
                return rest.reduce(function (result, obj) {
                    assertObj(obj);
                    Object.entries(obj).forEach(function (entry) {
                        var key = asString(entry[0]);
                        var val = toAny(entry[1]);
                        if (collHasKey(result, key)) {
                            result[key] = executeFunction(fn, [result[key], val], contextStack);
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
            evaluate: function (_a) {
                var keys = _a[0], values = _a[1];
                assertStringArray(keys);
                assertArr(values);
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
            evaluate: function (_a) {
                var obj = _a[0], keys = _a[1];
                assertStringArray(keys);
                assertObj(obj);
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
                return isLispishFunction(first);
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
            evaluate: function (_a) {
                var first = _a[0];
                assertFiniteNumber(first);
                return first === 0;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'pos?': {
            evaluate: function (_a) {
                var first = _a[0];
                assertFiniteNumber(first);
                return first > 0;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'neg?': {
            evaluate: function (_a) {
                var first = _a[0];
                assertFiniteNumber(first);
                return first < 0;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'even?': {
            evaluate: function (_a) {
                var first = _a[0];
                assertFiniteNumber(first);
                return first % 2 === 0;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'odd?': {
            evaluate: function (_a) {
                var first = _a[0];
                assertFiniteNumber(first);
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
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Number.isFinite(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'nan?': {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return Number.isNaN(value);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'positive-infinity?': {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
                return value === Number.POSITIVE_INFINITY;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'negative-infinity?': {
            evaluate: function (_a) {
                var value = _a[0];
                assertNumber(value);
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
            evaluate: function (params) {
                var first = params[0], second = params[1];
                assertString(first);
                if (params.length === 1) {
                    return new RegExp(first);
                }
                assertString(second);
                return new RegExp(first, second);
            },
            validate: function (node) { return assertLength({ min: 1, max: 2 }, node); },
        },
        match: {
            evaluate: function (_a) {
                var first = _a[0], second = _a[1];
                assertRegExp(first);
                assertString(second);
                var match = first.exec(second);
                if (match) {
                    return __spreadArray([], match);
                }
                return null;
            },
            validate: function (node) { return assertLength(2, node); },
        },
        replace: {
            evaluate: function (_a) {
                var string = _a[0], regexp = _a[1], value = _a[2];
                assertString(string);
                assertRegExp(regexp);
                assertString(value);
                return string.replace(regexp, value);
            },
            validate: function (node) { return assertLength(3, node); },
        },
    };

    var stringNormalExpression = {
        subs: {
            evaluate: function (_a) {
                var first = _a[0], second = _a[1], third = _a[2];
                assertString(first);
                assertNonNegativeInteger(second);
                if (third === undefined) {
                    return first.substring(second);
                }
                assertNumberGte(third, second);
                return first.substring(second, third);
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        'string-repeat': {
            evaluate: function (_a) {
                var string = _a[0], count = _a[1];
                assertString(string);
                assertNonNegativeInteger(count);
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
            evaluate: function (_a) {
                var str = _a[0];
                assertString(str);
                var number = Number(str);
                if (Number.isNaN(number)) {
                    throw Error("Could not convert '" + str + "' to a number");
                }
                return number;
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'number-to-string': {
            evaluate: function (params) {
                var number = params[0], base = params[1];
                assertFiniteNumber(number);
                if (params.length === 1) {
                    return "" + number;
                }
                else {
                    assertFiniteNumber(base);
                    if (base !== 2 && base !== 8 && base !== 10 && base !== 16) {
                        throw Error("Expected \"number-to-string\" base argument to be 2, 8, 10 or 16, got: " + base);
                    }
                    if (base === 10) {
                        return "" + number;
                    }
                    assertNonNegativeInteger(number);
                    return Number(number).toString(base);
                }
            },
            validate: function (node) { return assertLength({ min: 1, max: 2 }, node); },
        },
        'from-char-code': {
            evaluate: function (_a) {
                var number = _a[0];
                assertFiniteNumber(number);
                var int = toNonNegativeInteger(number);
                return String.fromCodePoint(int);
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'to-char-code': {
            evaluate: function (_a) {
                var str = _a[0];
                assertNonEmptyString(str);
                return asNotUndefined(str.codePointAt(0));
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'lower-case': {
            evaluate: function (_a) {
                var str = _a[0];
                assertString(str);
                return str.toLowerCase();
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'upper-case': {
            evaluate: function (_a) {
                var str = _a[0];
                assertString(str);
                return str.toUpperCase();
            },
            validate: function (node) { return assertLength(1, node); },
        },
        trim: {
            evaluate: function (_a) {
                var str = _a[0];
                assertString(str);
                return str.trim();
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'trim-left': {
            evaluate: function (_a) {
                var str = _a[0];
                assertString(str);
                return str.replace(/^\s+/, "");
            },
            validate: function (node) { return assertLength(1, node); },
        },
        'trim-right': {
            evaluate: function (_a) {
                var str = _a[0];
                assertString(str);
                return str.replace(/\s+$/, "");
            },
            validate: function (node) { return assertLength(1, node); },
        },
        join: {
            evaluate: function (_a) {
                var stringList = _a[0], delimiter = _a[1];
                assertArr(stringList);
                stringList.forEach(function (str) { return assertString(str); });
                assertString(delimiter);
                return stringList.join(delimiter);
            },
            validate: function (node) { return assertLength(2, node); },
        },
        split: {
            evaluate: function (_a) {
                var str = _a[0], delimiter = _a[1], limit = _a[2];
                assertString(str);
                assertStringOrRegExp(delimiter);
                if (limit !== undefined) {
                    assertNonNegativeInteger(limit);
                }
                return str.split(delimiter, limit);
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        'pad-left': {
            evaluate: function (_a) {
                var str = _a[0], length = _a[1], padString = _a[2];
                assertString(str);
                assertInteger(length);
                if (padString !== undefined) {
                    assertString(padString);
                }
                return str.padStart(length, padString);
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        'pad-right': {
            evaluate: function (_a) {
                var str = _a[0], length = _a[1], padString = _a[2];
                assertString(str);
                assertInteger(length);
                if (padString !== undefined) {
                    assertString(padString);
                }
                return str.padEnd(length, padString);
            },
            validate: function (node) { return assertLength({ min: 2, max: 3 }, node); },
        },
        template: {
            evaluate: function (_a) {
                var templateString = _a[0], placeholders = _a.slice(1);
                assertString(templateString);
                var templateStrings = templateString.split("||||");
                if (templateStrings.length === 1) {
                    assertStringArray(placeholders);
                    return applyPlaceholders(templateStrings[0], placeholders);
                }
                else if (templateStrings.length === 2) {
                    var firstPlaceholder = placeholders[0];
                    assertNonNegativeInteger(firstPlaceholder);
                    var stringPlaceholders = __spreadArray(["" + firstPlaceholder], placeholders.slice(1));
                    if (firstPlaceholder === 1) {
                        return applyPlaceholders(templateStrings[0], stringPlaceholders);
                    }
                    else {
                        return applyPlaceholders(templateStrings[1], stringPlaceholders);
                    }
                }
                else {
                    throw Error("Invalid template string, only one \"||||\" separator allowed");
                }
            },
            validate: function (node) { return assertLength({ min: 1, max: 10 }, node); },
        },
    };
    var doubleDollarRegexp = /\$\$/g;
    function applyPlaceholders(templateString, placeholders) {
        for (var i = 0; i < 9; i += 1) {
            var re = new RegExp("(?<=^|[^$]|\\$\\$)\\$" + (i + 1), "g");
            if (re.test(templateString)) {
                var placeholder = placeholders[i];
                assertString(placeholder);
                templateString = templateString.replace(re, placeholder);
            }
        }
        return templateString.replace(doubleDollarRegexp, "$");
    }

    var functionalNormalExpression = {
        apply: {
            evaluate: function (_a, contextStack, _b) {
                var func = _a[0], params = _a.slice(1);
                var executeFunction = _b.executeFunction;
                assertLispishFunction(func);
                var paramsLength = params.length;
                var last = params[paramsLength - 1];
                assertArr(last);
                var applyArray = __spreadArray(__spreadArray([], params.slice(0, -1), true), last);
                return executeFunction(func, applyArray, contextStack);
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

    var functionExecutors = {
        'user-defined': function (fn, params, contextStack, _a) {
            var _b, _c, _d;
            var evaluateAstNode = _a.evaluateAstNode;
            var args = fn.arguments;
            var nbrOfMandatoryArgs = args.mandatoryArguments.length;
            var nbrOfOptionalArgs = args.optionalArguments.length;
            var maxNbrOfParameters = args.restArgument ? null : nbrOfMandatoryArgs + nbrOfOptionalArgs;
            for (;;) {
                var newContext = __assign({}, fn.functionContext);
                if (params.length < args.mandatoryArguments.length) {
                    throw Error("Function " + ((_b = fn.name) !== null && _b !== void 0 ? _b : "(fn)") + " requires at least " + args.mandatoryArguments.length + " arguments. Got " + params.length);
                }
                if (maxNbrOfParameters !== null && params.length > maxNbrOfParameters) {
                    throw Error("Function \"" + ((_c = fn.name) !== null && _c !== void 0 ? _c : "\u03BB") + "\" requires at most " + maxNbrOfParameters + " arguments. Got " + params.length);
                }
                var length_1 = Math.max(params.length, args.mandatoryArguments.length + args.optionalArguments.length);
                var rest = [];
                for (var i = 0; i < length_1; i += 1) {
                    if (i < nbrOfMandatoryArgs) {
                        var param = toAny(params[i]);
                        var key = asString(args.mandatoryArguments[i]);
                        newContext[key] = { value: param };
                    }
                    else if (i < nbrOfMandatoryArgs + nbrOfOptionalArgs) {
                        var arg = asNotUndefined(args.optionalArguments[i - nbrOfMandatoryArgs]);
                        var param = i < params.length ? toAny(params[i]) : (_d = arg.defaultValue) !== null && _d !== void 0 ? _d : null;
                        var key = arg.name;
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
                    for (var _i = 0, _e = fn.body; _i < _e.length; _i++) {
                        var node = _e[_i];
                        result = evaluateAstNode(node, __spreadArray([newContext], contextStack, true));
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
        partial: function (fn, params, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return executeFunction(fn.fn, __spreadArray(__spreadArray([], fn.params, true), params), contextStack);
        },
        comp: function (fn, params, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var fns = fn.fns;
            if (fns.length === 0) {
                if (params.length !== 1) {
                    throw Error("(comp) expects one argument, got " + params.length);
                }
                return asAny(params[0]);
            }
            return asAny(fns.reduceRight(function (result, fn) {
                return [executeFunction(toAny(fn), result, contextStack)];
            }, params)[0]);
        },
        constantly: function (fn) {
            return fn.value;
        },
        juxt: function (fn, params, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return fn.fns.map(function (fn) { return executeFunction(toAny(fn), params, contextStack); });
        },
        complement: function (fn, params, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return !executeFunction(fn.fn, params, contextStack);
        },
        'every-pred': function (fn, params, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            for (var _i = 0, _b = fn.fns; _i < _b.length; _i++) {
                var f = _b[_i];
                for (var _c = 0, params_1 = params; _c < params_1.length; _c++) {
                    var param = params_1[_c];
                    var result = executeFunction(toAny(f), [param], contextStack);
                    if (!result) {
                        return false;
                    }
                }
            }
            return true;
        },
        'some-pred': function (fn, params, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            for (var _i = 0, _b = fn.fns; _i < _b.length; _i++) {
                var f = _b[_i];
                for (var _c = 0, params_2 = params; _c < params_2.length; _c++) {
                    var param = params_2[_c];
                    var result = executeFunction(toAny(f), [param], contextStack);
                    if (result) {
                        return true;
                    }
                }
            }
            return false;
        },
        fnil: function (fn, params, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var fniledParams = params.map(function (param, index) { return (param === null ? toAny(fn.params[index]) : param); });
            return executeFunction(toAny(fn.fn), fniledParams, contextStack);
        },
        builtin: function (fn, params, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var normalExpression = asNotUndefined(normalExpressions[fn.name]);
            return normalExpression.evaluate(params, contextStack, { executeFunction: executeFunction });
        },
    };

    function evaluate(ast, globalScope, importScope) {
        // First element is the global context. E.g. def will assign to this if no local variable is available
        // Second element is the context sent in from outside (this should never be mutated)
        var contextStack = [globalScope, importScope];
        var result;
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
                throw Error(node.type + "-node cannot be evaluated");
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
    function evaluateName(_a, contextStack) {
        var _b;
        var value = _a.value;
        for (var _i = 0, contextStack_1 = contextStack; _i < contextStack_1.length; _i++) {
            var context = contextStack_1[_i];
            var variable = context[value];
            if (variable) {
                return variable.value;
            }
        }
        if (builtin.normalExpressions[value]) {
            var builtinFunction = (_b = {},
                _b[FUNCTION_SYMBOL] = true,
                _b.type = "builtin",
                _b.name = value,
                _b);
            return builtinFunction;
        }
        throw Error("Undefined identifier " + value);
    }
    function evaluateNormalExpression(node, contextStack) {
        var _a;
        var params = node.params.map(function (paramNode) { return evaluateAstNode(paramNode, contextStack); });
        if (isNormalExpressionNodeName(node)) {
            for (var _i = 0, contextStack_2 = contextStack; _i < contextStack_2.length; _i++) {
                var context = contextStack_2[_i];
                var fn = (_a = context[node.name]) === null || _a === void 0 ? void 0 : _a.value;
                if (fn === undefined) {
                    continue;
                }
                try {
                    return executeFunction(fn, params, contextStack);
                }
                catch (_b) {
                    continue;
                }
            }
            return evaluateBuiltinNormalExpression(node, params, contextStack);
        }
        else {
            var fn = evaluateAstNode(node.expression, contextStack);
            return executeFunction(fn, params, contextStack);
        }
    }
    var executeFunction = function (fn, params, contextStack) {
        if (isLispishFunction(fn)) {
            return functionExecutors[fn.type](fn, params, contextStack, { evaluateAstNode: evaluateAstNode, executeFunction: executeFunction });
        }
        if (Array.isArray(fn)) {
            return evaluateArrayAsFunction(fn, params);
        }
        if (isObj(fn)) {
            return evalueateObjectAsFunction(fn, params);
        }
        if (isString(fn)) {
            return evaluateStringAsFunction(fn, params);
        }
        if (isNumber(fn)) {
            return evaluateNumberAsFunction(fn, params);
        }
        throw Error("Expected function, got " + fn);
    };
    function evaluateBuiltinNormalExpression(node, params, contextStack) {
        var normalExpressionEvaluator = asNotUndefined(builtin.normalExpressions[node.name]).evaluate;
        return normalExpressionEvaluator(params, contextStack, { executeFunction: executeFunction });
    }
    function evaluateSpecialExpression(node, contextStack) {
        var specialExpressionEvaluator = asNotUndefined(builtin.specialExpressions[node.name]).evaluate;
        return specialExpressionEvaluator(node, contextStack, { evaluateAstNode: evaluateAstNode, builtin: builtin });
    }
    function evalueateObjectAsFunction(fn, params) {
        if (params.length !== 1) {
            throw Error("Object as function requires one string parameter");
        }
        var key = params[0];
        assertString(key);
        return toAny(fn[key]);
    }
    function evaluateArrayAsFunction(fn, params) {
        if (params.length !== 1) {
            throw Error("Array as function requires one non negative integer parameter");
        }
        var index = params[0];
        assertNonNegativeInteger(index);
        return toAny(fn[index]);
    }
    function evaluateStringAsFunction(fn, params) {
        if (params.length !== 1) {
            throw Error("String as function requires one Obj parameter");
        }
        var param = toAny(params[0]);
        if (isObj(param)) {
            return toAny(param[fn]);
        }
        if (isInteger(param)) {
            return toAny(fn[param]);
        }
        throw Error("string as function expects Obj or integer parameter, got " + param);
    }
    function evaluateNumberAsFunction(fn, params) {
        assertInteger(fn);
        if (params.length !== 1) {
            throw Error("String as function requires one Arr parameter");
        }
        var param = params[0];
        assertSeq(param);
        return toAny(param[fn]);
    }

    var parseNumber = function (tokens, position) {
        var token = asNotUndefined(tokens[position]);
        return [position + 1, { type: "Number", value: Number(token.value) }];
    };
    var parseString = function (tokens, position) {
        var token = asNotUndefined(tokens[position]);
        return [position + 1, { type: "String", value: token.value }];
    };
    var parseName = function (tokens, position) {
        var token = asNotUndefined(tokens[position]);
        return [position + 1, { type: "Name", value: token.value }];
    };
    var parseReservedName = function (tokens, position) {
        var token = asNotUndefined(tokens[position]);
        return [position + 1, { type: "ReservedName", value: token.value }];
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
        };
        return [position, node];
    };
    var parseObjectLitteral = function (tokens, position) {
        var _a;
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
        };
        assertLengthEven(node);
        return [position, node];
    };
    var parseRegexpShorthand = function (tokens, position) {
        var token = asNotUndefined(tokens[position]);
        var stringNode = {
            type: "String",
            value: token.value,
        };
        assertNotUndefined(token.options);
        var optionsNode = {
            type: "String",
            value: "" + (token.options.g ? "g" : "") + (token.options.i ? "i" : ""),
        };
        var node = {
            type: "NormalExpression",
            name: "regexp",
            params: [stringNode, optionsNode],
        };
        return [position + 1, node];
    };
    var placeholderRegexp = /^%([1-9][0-9]?$)/;
    var parseFnShorthand = function (tokens, position) {
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
                        throw Error("Can't specify more than 20 arguments");
                    }
                }
            }
            if (token.type === "fnShorthand") {
                throw Error("Nested shortcut functions are not allowed");
            }
        }
        var mandatoryArguments = [];
        for (var i = 1; i <= arity; i += 1) {
            mandatoryArguments.push("%" + i);
        }
        var args = {
            bindings: [],
            mandatoryArguments: mandatoryArguments,
            optionalArguments: [],
        };
        var node = {
            type: "SpecialExpression",
            name: "fn",
            params: [],
            arguments: args,
            body: [normalExpressionNode],
        };
        return [newPosition, node];
    };
    var parseArgument = function (tokens, position) {
        var token = asNotUndefined(tokens[position]);
        if (token.type === "name") {
            return [position + 1, { type: "Argument", name: token.value }];
        }
        else if (token.type === "paren" && token.value === "(") {
            position += 1;
            token = asNotUndefined(tokens[position]);
            if (token.type !== "name") {
                throw new UnexpectedTokenError("name", token);
            }
            var name_1 = token.value;
            position += 1;
            var _a = parseToken(tokens, position), newPosition = _a[0], defaultValue = _a[1];
            token = asNotUndefined(tokens[newPosition]);
            if (!(token.type === "paren" && token.value === ")")) {
                throw new UnexpectedTokenError(")", token);
            }
            return [newPosition + 1, { type: "Argument", name: name_1, defaultValue: defaultValue }];
        }
        else if (token.type === "modifier") {
            var value = token.value;
            return [position + 1, { type: "Modifier", value: value }];
        }
        else {
            throw new UnexpectedTokenError("\"(\", name or modifier", token);
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
        var token = asNotUndefined(tokens[position]);
        if (token.type !== "name") {
            throw Error("Expected name node in binding, got " + token.type + " value=" + token.value);
        }
        var name = token.value;
        position += 1;
        token = asNotUndefined(tokens[position]);
        var value;
        _a = parseToken(tokens, position), position = _a[0], value = _a[1];
        var node = {
            type: "Binding",
            name: name,
            value: value,
        };
        return [position, node];
    };
    var parseNormalExpression = function (tokens, position) {
        var _a;
        var _b;
        //  let fnNode: AstNode
        var _c = parseToken(tokens, position), newPosition = _c[0], fnNode = _c[1];
        var params;
        _a = parseTokens(tokens, newPosition), position = _a[0], params = _a[1];
        position += 1;
        if (isExpressionNode(fnNode)) {
            var node_1 = {
                type: "NormalExpression",
                expression: fnNode,
                params: params,
            };
            return [position, node_1];
        }
        assertNameNode(fnNode);
        var node = {
            type: "NormalExpression",
            name: fnNode.value,
            params: params,
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
            throw SyntaxError("Unrecognized token: " + token.type + " value=" + token.value);
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
    var nameRegExp = /[@%0-9a-zA-Z_^?=!$%<>.+*/-]/;
    var whitespaceRegExp = /\s|,/;
    var skipWhiteSpace = function (input, current) { var _a; return whitespaceRegExp.test((_a = input[current]) !== null && _a !== void 0 ? _a : "") ? [1, undefined] : NO_MATCH; };
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
    var tokenizeLeftParen = function (input, position) {
        return tokenizeCharacter("paren", "(", input, position);
    };
    var tokenizeRightParen = function (input, position) {
        return tokenizeCharacter("paren", ")", input, position);
    };
    var tokenizeLeftBracket = function (input, position) {
        return tokenizeCharacter("paren", "[", input, position);
    };
    var tokenizeRightBracket = function (input, position) {
        return tokenizeCharacter("paren", "]", input, position);
    };
    var tokenizeLeftCurly = function (input, position) {
        return tokenizeCharacter("paren", "{", input, position);
    };
    var tokenizeRightCurly = function (input, position) {
        return tokenizeCharacter("paren", "}", input, position);
    };
    var tokenizeString = function (input, position) {
        if (input[position] !== "\"") {
            return NO_MATCH;
        }
        var value = "";
        var length = 1;
        var char = input[position + length];
        var escape = false;
        while (char !== "\"" || escape) {
            if (char === undefined) {
                throw new SyntaxError("Unclosed string at position " + position);
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
        return [length + 1, { type: "string", value: value }];
    };
    var tokenizeSymbolString = function (input, position) {
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
        return [length, { type: "string", value: value }];
    };
    var tokenizeRegexpShorthand = function (input, position) {
        var _a;
        if (input[position] !== "#") {
            return NO_MATCH;
        }
        var _b = tokenizeString(input, position + 1), stringLength = _b[0], token = _b[1];
        if (!token) {
            return NO_MATCH;
        }
        position += stringLength + 1;
        var length = stringLength + 1;
        var options = {};
        while (input[position] === "g" || input[position] === "i") {
            if (input[position] === "g") {
                if (options.g) {
                    throw new SyntaxError("Duplicated regexp option \"" + input[position] + "\" at position " + position);
                }
                length += 1;
                options.g = true;
            }
            else {
                if (options.i) {
                    throw new SyntaxError("Duplicated regexp option \"" + input[position] + "\" at position " + position);
                }
                length += 1;
                options.i = true;
            }
            position += 1;
        }
        if (nameRegExp.test((_a = input[position]) !== null && _a !== void 0 ? _a : "")) {
            throw new SyntaxError("Unexpected regexp option \"" + input[position] + "\" at position " + position);
        }
        return [
            length,
            {
                type: "regexpShorthand",
                value: token.value,
                options: options,
            },
        ];
    };
    var tokenizeFnShorthand = function (input, position) {
        if (input.slice(position, position + 2) !== "#(") {
            return NO_MATCH;
        }
        return [
            1,
            {
                type: "fnShorthand",
                value: "#",
            },
        ];
    };
    var endOfNumberRegExp = /\s|[)\]},]/;
    var decimalNumberRegExp = /[0-9]/;
    var octalNumberRegExp = /[0-7]/;
    var hexNumberRegExp = /[0-9a-fA-F]/;
    var binaryNumberRegExp = /[0-1]/;
    var firstCharRegExp = /[0-9.-]/;
    var tokenizeNumber = function (input, position) {
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
            var char = asNotUndefined(input[i]);
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
        return [length, { type: "number", value: value }];
    };
    function tokenizeReservedName(input, position) {
        for (var _i = 0, _a = Object.keys(reservedNamesRecord); _i < _a.length; _i++) {
            var reservedName = _a[_i];
            var length_2 = reservedName.length;
            var nextChar = input[position + length_2];
            if (nextChar && nameRegExp.test(nextChar)) {
                continue;
            }
            if (input.substr(position, length_2) === reservedName) {
                return [length_2, { type: "reservedName", value: reservedName }];
            }
        }
        return NO_MATCH;
    }
    var tokenizeName = function (input, position) {
        return tokenizePattern("name", nameRegExp, input, position);
    };
    var tokenizeModifier = function (input, position) {
        var modifiers = ["&rest", "&opt", "&let", "&when", "&while"];
        for (var _i = 0, modifiers_1 = modifiers; _i < modifiers_1.length; _i++) {
            var modifier = modifiers_1[_i];
            var length_3 = modifier.length;
            if (input.substr(position, length_3) === modifier) {
                var value = modifier;
                return [length_3, { type: "modifier", value: value }];
            }
        }
        return NO_MATCH;
    };
    function tokenizeCharacter(type, value, input, position) {
        if (value === input[position]) {
            return [1, { type: type, value: value }];
        }
        else {
            return NO_MATCH;
        }
    }
    function tokenizePattern(type, pattern, input, position) {
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
        return [length, { type: type, value: value }];
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
    function tokenize(input) {
        var tokens = [];
        var position = 0;
        var tokenized = false;
        while (position < input.length) {
            tokenized = false;
            // Loop through all tokenizer until one matches
            for (var _i = 0, tokenizers_1 = tokenizers; _i < tokenizers_1.length; _i++) {
                var tokenize_1 = tokenizers_1[_i];
                var _a = tokenize_1(input, position), nbrOfCharacters = _a[0], token = _a[1];
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
                throw new SyntaxError("Unrecognized character at position " + position + ": '" + input[position] + "'");
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

    var Lispish = /** @class */ (function () {
        function Lispish(config) {
            if (config === void 0) { config = {}; }
            this.importScope = {};
            if (config.astCacheSize && config.astCacheSize > 0) {
                this.astCache = new Cache(config.astCacheSize);
            }
            else {
                this.astCache = null;
            }
        }
        Lispish.prototype.run = function (program, params) {
            var ast = this.generateAst(program);
            var result = this.evaluate(ast, params);
            return result;
        };
        Lispish.prototype.import = function (program, params) {
            if (params === void 0) { params = {}; }
            var context = getContextFromParams(params);
            var tokens = this.tokenize(program);
            var ast = this.parse(tokens);
            var scope = {};
            evaluate(ast, scope, context);
            var importKeys = Object.keys(this.importScope);
            for (var _i = 0, _a = Object.keys(scope); _i < _a.length; _i++) {
                var key = _a[_i];
                if (importKeys.includes(key)) {
                    throw Error("Import faild, imported function/variable already exists: \"" + key + "\"");
                }
                assertNameNotDefined(key, [{}, {}], builtin);
            }
            Object.assign(this.importScope, scope);
        };
        Lispish.prototype.tokenize = function (program) {
            return tokenize(program);
        };
        Lispish.prototype.parse = function (tokens) {
            return parse(tokens);
        };
        Lispish.prototype.evaluate = function (ast, params) {
            if (params === void 0) { params = {}; }
            var context = getContextFromParams(params);
            return evaluate(ast, context, this.importScope);
        };
        Lispish.prototype.generateAst = function (program) {
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
        return Lispish;
    }());
    function getContextFromParams(params) {
        var context = params.globalContext || {};
        if (params.vars) {
            Object.entries(params.vars).forEach(function (_a) {
                var key = _a[0], value = _a[1];
                context[key] = { value: toAny(value) };
            });
        }
        return context;
    }

    exports.Lispish = Lispish;
    exports.isLispishFunction = isLispishFunction;
    exports.normalExpressionKeys = normalExpressionKeys;
    exports.reservedNames = reservedNames;
    exports.specialExpressionKeys = specialExpressionKeys;

    Object.defineProperty(exports, '__esModule', { value: true });

    return exports;

})({});
//# sourceMappingURL=lispish.iife.js.map
