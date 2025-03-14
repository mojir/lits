var Playground = (function (exports) {
    'use strict';

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
    /* global Reflect, Promise, SuppressedError, Symbol */

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

    typeof SuppressedError === "function" ? SuppressedError : function (error, suppressed, message) {
        var e = new Error(message);
        return e.name = "SuppressedError", e.error = error, e.suppressed = suppressed, e;
    };

    var StateHistory = /** @class */ (function () {
        function StateHistory(initial, listener) {
            this.history = [];
            this.lastStatus = { canUndo: false, canRedo: false };
            this.history.push(initial);
            this.index = 0;
            this.listener = listener;
        }
        Object.defineProperty(StateHistory.prototype, "canUndo", {
            get: function () {
                return this.index > 0;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(StateHistory.prototype, "canRedo", {
            get: function () {
                return this.index < this.history.length - 1;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(StateHistory.prototype, "current", {
            get: function () {
                return this.history[this.index];
            },
            enumerable: false,
            configurable: true
        });
        StateHistory.prototype.push = function (entry) {
            if (entry.text !== this.current.text) {
                this.history.splice(this.index + 1);
                this.history.push(entry);
                this.index = this.history.length - 1;
                this.notify();
            }
            else {
                this.replace(entry);
            }
        };
        StateHistory.prototype.replace = function (entry) {
            this.current.text = entry.text;
            this.current.selectionStart = entry.selectionStart;
            this.current.selectionEnd = entry.selectionEnd;
            this.notify();
        };
        StateHistory.prototype.undo = function () {
            if (!this.canUndo)
                throw new Error('Cannot undo');
            this.index -= 1;
            this.notify();
            return this.history[this.index];
        };
        StateHistory.prototype.redo = function () {
            if (!this.canRedo)
                throw new Error('Cannot redo');
            this.index += 1;
            this.notify();
            return this.current;
        };
        StateHistory.prototype.peek = function () {
            return this.current;
        };
        StateHistory.prototype.reset = function (initialState) {
            this.history = [initialState];
            this.index = 0;
            this.notify();
        };
        StateHistory.prototype.notify = function () {
            var _this = this;
            var status = { canUndo: this.canUndo, canRedo: this.canRedo };
            if (status.canUndo !== this.lastStatus.canUndo || status.canRedo !== this.lastStatus.canRedo) {
                this.lastStatus = status;
                setTimeout(function () { return _this.listener(status); }, 0);
            }
        };
        return StateHistory;
    }());

    var defaultState = {
        'playground-height': 350,
        'resize-divider-1-percent': 20,
        'resize-divider-2-percent': 60,
        'context': '',
        'context-scroll-top': 0,
        'context-selection-start': 0,
        'context-selection-end': 0,
        'lits-code': '',
        'lits-code-scroll-top': 0,
        'lits-code-selection-start': 0,
        'lits-code-selection-end': 0,
        'output': '',
        'output-scroll-top': 0,
        'new-context-name': '',
        'new-context-value': '',
        'debug': false,
        'focused-panel': null,
    };
    var contextHistoryListener;
    var litsCodeHistoryListener;
    var state = __assign({}, defaultState);
    Object.keys(defaultState).forEach(function (key) {
        var value = localStorage.getItem(getStorageKey(key));
        state[key] = typeof value === 'string' ? JSON.parse(value) : defaultState[key];
    });
    var contextHistory = new StateHistory(createContextHistoryEntry(), function (status) {
        contextHistoryListener === null || contextHistoryListener === void 0 ? void 0 : contextHistoryListener(status);
    });
    var litsCodeHistory = new StateHistory(createLitsCodeHistoryEntry(), function (status) {
        litsCodeHistoryListener === null || litsCodeHistoryListener === void 0 ? void 0 : litsCodeHistoryListener(status);
    });
    function createContextHistoryEntry() {
        return {
            text: state.context,
            selectionStart: state['context-selection-start'],
            selectionEnd: state['context-selection-end'],
        };
    }
    function createLitsCodeHistoryEntry() {
        return {
            text: state['lits-code'],
            selectionStart: state['lits-code-selection-start'],
            selectionEnd: state['lits-code-selection-end'],
        };
    }
    function pushHistory() {
        contextHistory.push(createContextHistoryEntry());
        litsCodeHistory.push(createLitsCodeHistoryEntry());
    }
    function setContextHistoryListener(listener) {
        contextHistoryListener = listener;
    }
    function setLitsCodeHistoryListener(listener) {
        litsCodeHistoryListener = listener;
    }
    function saveState(newState, pushToHistory) {
        if (pushToHistory === void 0) { pushToHistory = true; }
        Object.entries(newState).forEach(function (entry) {
            var key = entry[0];
            var value = entry[1];
            setState(key, value);
            localStorage.setItem(getStorageKey(key), JSON.stringify(value));
        });
        if (pushToHistory) {
            pushHistory();
        }
    }
    function setState(key, value) {
        state[key] = value;
    }
    function clearAllStates() {
        localStorage.clear();
        Object.assign(state, defaultState);
        litsCodeHistory.reset(createLitsCodeHistoryEntry());
        contextHistory.reset(createContextHistoryEntry());
    }
    function clearState() {
        var keys = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            keys[_i] = arguments[_i];
        }
        keys.forEach(function (key) {
            localStorage.removeItem(getStorageKey(key));
            state[key] = defaultState[key];
        });
        pushHistory();
    }
    function getState(key) {
        return state[key];
    }
    function encodeState() {
        var sharedState = {
            'lits-code': state['lits-code'],
            'context': state.context,
        };
        return btoa(encodeURIComponent(JSON.stringify(sharedState)));
    }
    function applyEncodedState(encodedState) {
        try {
            saveState(JSON.parse(decodeURIComponent(atob(encodedState))), true);
            return true;
        }
        catch (error) {
            return false;
        }
    }
    function undoContext() {
        try {
            var historyEntry = contextHistory.undo();
            saveState({
                'context': historyEntry.text,
                'context-selection-start': historyEntry.selectionStart,
                'context-selection-end': historyEntry.selectionEnd,
            }, false);
            return true;
        }
        catch (_a) {
            return false;
        }
    }
    function redoContext() {
        try {
            var historyEntry = contextHistory.redo();
            saveState({
                'context': historyEntry.text,
                'context-selection-start': historyEntry.selectionStart,
                'context-selection-end': historyEntry.selectionEnd,
            }, false);
            return true;
        }
        catch (_a) {
            return false;
        }
    }
    function undoLitsCode() {
        try {
            var historyEntry = litsCodeHistory.undo();
            saveState({
                'lits-code': historyEntry.text,
                'lits-code-selection-start': historyEntry.selectionStart,
                'lits-code-selection-end': historyEntry.selectionEnd,
            }, false);
            return true;
        }
        catch (_a) {
            return false;
        }
    }
    function redoLitsCode() {
        try {
            var historyEntry = litsCodeHistory.redo();
            saveState({
                'lits-code': historyEntry.text,
                'lits-code-selection-start': historyEntry.selectionStart,
                'lits-code-selection-end': historyEntry.selectionEnd,
            }, false);
            return true;
        }
        catch (_a) {
            return false;
        }
    }
    function getStorageKey(key) {
        return "playground-".concat(key);
    }

    function isNotNull(value) {
        return value !== null && value !== undefined;
    }
    function assertNotNull(value) {
        if (!isNotNull(value))
            throw new Error('Value is null or undefined');
    }
    function asNotNull(value) {
        assertNotNull(value);
        return value;
    }
    function throttle(func) {
        var openForBusiness = true;
        return function () {
            var args = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                args[_i] = arguments[_i];
            }
            if (openForBusiness) {
                requestAnimationFrame(function () { return openForBusiness = true; });
                openForBusiness = false;
                func.apply(this, args);
            }
        };
    }
    function isMac() {
        return navigator.platform.includes('Mac');
    }

    var ctrlKeyTimer = 0;
    var ctrlKeyStarted = null;
    var selectedIndex = null;
    var onCloseCallback = null;
    var previouslyFocusedPanel = getState('focused-panel');
    var searchInput = asNotNull(document.getElementById('search-input'));
    var searchResult = asNotNull(document.getElementById('search-result'));
    var noSearchResult = asNotNull(document.getElementById('no-search-result'));
    var searchOverlay = asNotNull(document.getElementById('search-dialog-overlay'));
    var searchDialog = asNotNull(document.getElementById('search-dialog'));
    searchInput.addEventListener('input', onSearch);
    searchOverlay.addEventListener('click', closeSearch);
    searchDialog.addEventListener('click', function (event) {
        searchInput.focus();
        event.stopPropagation();
    });
    function onClose(callback) {
        onCloseCallback = callback;
    }
    function handleKeyDown(event) {
        var _a;
        if (event.key === 'Control')
            handleCtrlKey();
        if (isOpen()) {
            switch (event.key) {
                case 'Escape':
                    event.preventDefault();
                    if (selectedIndex !== null && selectedIndex > 0)
                        resetSelection();
                    else if (((_a = event.target) === null || _a === void 0 ? void 0 : _a.closest('#search-input')) && searchInput.value)
                        clearSearch();
                    else
                        closeSearch();
                    break;
                case 'ArrowDown':
                    event.preventDefault();
                    selectNext();
                    break;
                case 'ArrowUp':
                    event.preventDefault();
                    selectPrevious();
                    break;
                case 'PageDown':
                    event.preventDefault();
                    selectPageDown();
                    break;
                case 'PageUp':
                    event.preventDefault();
                    selectPageUp();
                    break;
                case 'Home':
                    event.preventDefault();
                    selectFirst();
                    break;
                case 'End':
                    event.preventDefault();
                    selectLast();
                    break;
                case 'Enter':
                    event.preventDefault();
                    if (typeof selectedIndex === 'number') {
                        var x = searchResult.children[selectedIndex];
                        x === null || x === void 0 ? void 0 : x.click();
                    }
                    break;
                case 'k':
                case 'K':
                    if (event.ctrlKey || event.metaKey) {
                        event.preventDefault();
                        openSearch();
                    }
                    break;
                case 'F3':
                    event.preventDefault();
                    openSearch();
                    break;
            }
            return 'stop';
        }
        else {
            switch (event.key) {
                case 'k':
                case 'K':
                    if (event.ctrlKey || event.metaKey) {
                        event.preventDefault();
                        openSearch();
                    }
                    break;
                case 'F3':
                    event.preventDefault();
                    openSearch();
                    break;
            }
        }
    }
    function handleCtrlKey() {
        if (ctrlKeyStarted === null) {
            ctrlKeyStarted = Date.now();
            ctrlKeyTimer = window.setTimeout(resetCtrlKey, 400);
        }
        else {
            resetCtrlKey();
            if (isOpen())
                closeSearch();
            else
                openSearch();
        }
    }
    function resetCtrlKey() {
        window.clearTimeout(ctrlKeyTimer);
        ctrlKeyStarted = null;
        ctrlKeyTimer = 0;
    }
    function openSearch() {
        previouslyFocusedPanel = getState('focused-panel');
        searchOverlay.style.display = 'block';
        if (searchResult.children.length === 0)
            updateSearchResult(searchInput.value);
        searchInput.focus();
    }
    function closeSearch() {
        if (searchOverlay.style.display === 'block') {
            searchOverlay.style.display = 'none';
            saveState({ 'focused-panel': previouslyFocusedPanel });
            onCloseCallback === null || onCloseCallback === void 0 ? void 0 : onCloseCallback();
        }
    }
    function isOpen() {
        return searchOverlay.style.display === 'block';
    }
    function clearSearch() {
        searchInput.value = '';
        updateSearchResult('');
    }
    function resetSelection() {
        if (searchInput.value)
            selectedIndex = 0;
        else
            selectedIndex = null;
        updateSelection();
        searchResult.scrollTo(0, 0);
    }
    function updateSelection() {
        Array.from(searchResult.getElementsByClassName('selected'))
            .forEach(function (el) { return el.classList.remove('selected'); });
        if (selectedIndex !== null) {
            var count = searchResult.children.length;
            if (count) {
                if (selectedIndex >= count)
                    selectedIndex = count - 1;
                else if (selectedIndex < 0)
                    selectedIndex = 0;
                var element = searchResult.children[selectedIndex];
                element.classList.add('selected');
                element.scrollIntoView({ block: 'center' });
            }
            else {
                selectedIndex = null;
            }
        }
    }
    function selectPrevious() {
        if (selectedIndex !== null)
            selectedIndex -= 1;
        updateSelection();
    }
    function selectNext() {
        if (selectedIndex !== null)
            selectedIndex += 1;
        else
            selectedIndex = 0;
        updateSelection();
    }
    function selectPageUp() {
        if (selectedIndex !== null)
            selectedIndex -= 10;
        updateSelection();
    }
    function selectPageDown() {
        if (selectedIndex !== null)
            selectedIndex += 10;
        else
            selectedIndex = 10;
        updateSelection();
    }
    function selectFirst() {
        selectedIndex = 0;
        updateSelection();
    }
    function selectLast() {
        selectedIndex = searchResult.children.length - 1;
        updateSelection();
    }
    function onSearch(event) {
        var _a;
        var target = event.target;
        var searchString = (_a = target === null || target === void 0 ? void 0 : target.value) !== null && _a !== void 0 ? _a : '';
        updateSearchResult(searchString);
    }
    function updateSearchResult(searchString) {
        searchResult.style.display = 'none';
        noSearchResult.style.display = 'none';
        searchResult.innerHTML = '';
        // eslint-disable-next-line ts/no-unsafe-member-access
        var searchResults = window.Playground.allSearchResultEntries.filter(function (entry) { return entry.search.toLowerCase().includes(searchString.toLowerCase()); });
        if (searchResults.length === 0) {
            noSearchResult.style.display = 'flex';
        }
        else {
            searchResult.style.display = 'flex';
            searchResult.innerHTML = searchResults.map(function (entry) { return entry.html; }).join('');
        }
        resetSelection();
    }
    var Search = {
        openSearch: openSearch,
        closeSearch: closeSearch,
        clearSearch: clearSearch,
        handleKeyDown: handleKeyDown,
        onClose: onClose,
    };

    var AstNodeType;
    (function (AstNodeType) {
        AstNodeType[AstNodeType["Number"] = 201] = "Number";
        AstNodeType[AstNodeType["String"] = 202] = "String";
        AstNodeType[AstNodeType["NormalExpression"] = 203] = "NormalExpression";
        AstNodeType[AstNodeType["SpecialExpression"] = 204] = "SpecialExpression";
        AstNodeType[AstNodeType["Symbol"] = 205] = "Symbol";
        AstNodeType[AstNodeType["Modifier"] = 206] = "Modifier";
        AstNodeType[AstNodeType["ReservedSymbol"] = 207] = "ReservedSymbol";
        AstNodeType[AstNodeType["Binding"] = 208] = "Binding";
        AstNodeType[AstNodeType["Argument"] = 209] = "Argument";
        AstNodeType[AstNodeType["Partial"] = 210] = "Partial";
        AstNodeType[AstNodeType["Comment"] = 211] = "Comment";
    })(AstNodeType || (AstNodeType = {}));
    var astNodeTypeName = new Map([
        [AstNodeType.Number, 'Number'],
        [AstNodeType.String, 'String'],
        [AstNodeType.NormalExpression, 'NormalExpression'],
        [AstNodeType.SpecialExpression, 'SpecialExpression'],
        [AstNodeType.Symbol, 'Name'],
        [AstNodeType.Modifier, 'Modifier'],
        [AstNodeType.ReservedSymbol, 'ReservedName'],
        [AstNodeType.Binding, 'Binding'],
        [AstNodeType.Argument, 'Argument'],
        [AstNodeType.Partial, 'Partial'],
    ]);
    function isAstNodeType(type) {
        return typeof type === 'number' && astNodeTypeName.has(type);
    }
    var FunctionType;
    (function (FunctionType) {
        FunctionType[FunctionType["UserDefined"] = 301] = "UserDefined";
        FunctionType[FunctionType["Partial"] = 302] = "Partial";
        FunctionType[FunctionType["Comp"] = 303] = "Comp";
        FunctionType[FunctionType["Constantly"] = 304] = "Constantly";
        FunctionType[FunctionType["Juxt"] = 305] = "Juxt";
        FunctionType[FunctionType["Complement"] = 306] = "Complement";
        FunctionType[FunctionType["EveryPred"] = 307] = "EveryPred";
        FunctionType[FunctionType["SomePred"] = 308] = "SomePred";
        FunctionType[FunctionType["Fnull"] = 309] = "Fnull";
        FunctionType[FunctionType["Builtin"] = 310] = "Builtin";
        FunctionType[FunctionType["NativeJsFunction"] = 399] = "NativeJsFunction";
    })(FunctionType || (FunctionType = {}));
    var functionTypeName = new Map([
        [FunctionType.UserDefined, 'UserDefined'],
        [FunctionType.Partial, 'Partial'],
        [FunctionType.Comp, 'Comp'],
        [FunctionType.Constantly, 'Constantly'],
        [FunctionType.Juxt, 'Juxt'],
        [FunctionType.Complement, 'Complement'],
        [FunctionType.EveryPred, 'EveryPred'],
        [FunctionType.SomePred, 'SomePred'],
        [FunctionType.Fnull, 'Fnull'],
        [FunctionType.Builtin, 'Builtin'],
    ]);
    function isFunctionType(type) {
        return typeof type === 'number' && functionTypeName.has(type);
    }

    function getCodeMarker(sourceCodeInfo) {
        if (!sourceCodeInfo.position || !sourceCodeInfo.code)
            return '';
        var leftPadding = sourceCodeInfo.position.column - 1;
        var rightPadding = sourceCodeInfo.code.length - leftPadding - 1;
        return "".concat(' '.repeat(Math.max(leftPadding, 0)), "^").concat(' '.repeat(Math.max(rightPadding, 0)));
    }

    function getLitsErrorMessage(message, sourceCodeInfo) {
        var filePathLine = (sourceCodeInfo === null || sourceCodeInfo === void 0 ? void 0 : sourceCodeInfo.filePath) ? "\n".concat(sourceCodeInfo.filePath) : '';
        var codeLine = (sourceCodeInfo === null || sourceCodeInfo === void 0 ? void 0 : sourceCodeInfo.code) ? "\n".concat(sourceCodeInfo.code) : '';
        var codeMarker = sourceCodeInfo && codeLine ? "\n".concat(getCodeMarker(sourceCodeInfo)) : '';
        return "".concat(message).concat(filePathLine).concat(codeLine).concat(codeMarker);
    }
    var RecurSignal = /** @class */ (function (_super) {
        __extends(RecurSignal, _super);
        function RecurSignal(params) {
            var _this = _super.call(this, "recur, params: ".concat(params)) || this;
            Object.setPrototypeOf(_this, RecurSignal.prototype);
            _this.name = 'RecurSignal';
            _this.params = params;
            return _this;
        }
        return RecurSignal;
    }(Error));
    var LitsError = /** @class */ (function (_super) {
        __extends(LitsError, _super);
        function LitsError(message, sourceCodeInfo) {
            var _this = this;
            if (message instanceof Error)
                message = "".concat(message.name).concat(message.message);
            _this = _super.call(this, getLitsErrorMessage(message, sourceCodeInfo)) || this;
            _this.shortMessage = message;
            _this.sourceCodeInfo = sourceCodeInfo;
            Object.setPrototypeOf(_this, LitsError.prototype);
            _this.name = 'LitsError';
            return _this;
        }
        LitsError.prototype.getCodeMarker = function () {
            return this.sourceCodeInfo && getCodeMarker(this.sourceCodeInfo);
        };
        return LitsError;
    }(Error));
    var NotAFunctionError = /** @class */ (function (_super) {
        __extends(NotAFunctionError, _super);
        function NotAFunctionError(fn, sourceCodeInfo) {
            var _this = this;
            var message = "Expected function, got ".concat(fn, ".");
            _this = _super.call(this, message, sourceCodeInfo) || this;
            Object.setPrototypeOf(_this, NotAFunctionError.prototype);
            _this.name = 'NotAFunctionError';
            return _this;
        }
        return NotAFunctionError;
    }(LitsError));
    var UserDefinedError = /** @class */ (function (_super) {
        __extends(UserDefinedError, _super);
        function UserDefinedError(message, sourceCodeInfo) {
            var _this = _super.call(this, message, sourceCodeInfo) || this;
            _this.userMessage = message;
            Object.setPrototypeOf(_this, UserDefinedError.prototype);
            _this.name = 'UserDefinedError';
            return _this;
        }
        return UserDefinedError;
    }(LitsError));
    var AssertionError = /** @class */ (function (_super) {
        __extends(AssertionError, _super);
        function AssertionError(message, sourceCodeInfo) {
            var _this = _super.call(this, message, sourceCodeInfo) || this;
            Object.setPrototypeOf(_this, AssertionError.prototype);
            _this.name = 'AssertionError';
            return _this;
        }
        return AssertionError;
    }(LitsError));
    var UndefinedSymbolError = /** @class */ (function (_super) {
        __extends(UndefinedSymbolError, _super);
        function UndefinedSymbolError(symbolName, sourceCodeInfo) {
            var _this = this;
            var message = "Undefined symbol '".concat(symbolName, "'.");
            _this = _super.call(this, message, sourceCodeInfo) || this;
            _this.symbol = symbolName;
            Object.setPrototypeOf(_this, UndefinedSymbolError.prototype);
            _this.name = 'UndefinedSymbolError';
            return _this;
        }
        return UndefinedSymbolError;
    }(LitsError));

    function getSourceCodeInfo(anyValue, sourceCodeInfo) {
        var _a;
        // eslint-disable-next-line ts/no-unsafe-return, ts/no-unsafe-member-access
        return (_a = anyValue === null || anyValue === void 0 ? void 0 : anyValue.sourceCodeInfo) !== null && _a !== void 0 ? _a : sourceCodeInfo;
    }

    var binaryOperators = [
        '**', // exponentiation
        '*', // multiplication
        '/', // division
        '%', // remainder
        '+', // addition
        '-', // subtraction
        '<<', // left shift
        '>>', // signed right shift
        '>>>', // unsigned right shift
        '++', // string concatenation
        '<', // less than
        '<=', // less than or equal
        '≤', // less than or equal
        '>', // greater than
        '>=', // greater than or equal
        '≥', // greater than or equal
        '=', // equal
        '!=', // not equal
        '≠', // not equal
        '&', // bitwise AND
        '^', // bitwise XOR
        '|', // bitwise OR
        '&&', // logical AND
        '||', // logical OR
        '??', // nullish coalescing
    ];
    var otherOperators = [
        '->', // lambda
        '...', // rest
        '.', // property accessor
        ',', // item separator
        ':=', // property assignment
        ';', // statement terminator
    ];
    var symbolicOperators = __spreadArray(__spreadArray([], __read(binaryOperators), false), __read(otherOperators), false);
    var nonFunctionOperators = [
        '??',
        '&&',
        '||',
        'comment',
        'cond',
        'def',
        'defined?',
        'defn',
        'do',
        'doseq',
        'fn',
        'if',
        'let',
        'loop',
        'recur',
        'throw',
        'try',
        'unless',
        'while',
    ];
    var nonFunctionOperatorSet = new Set(nonFunctionOperators);
    function isFunctionOperator(operator) {
        return !nonFunctionOperatorSet.has(operator);
    }
    var binaryOperatorSet = new Set(binaryOperators);
    function isBinaryOperator(operator) {
        return binaryOperatorSet.has(operator);
    }
    var symbolicOperatorSet = new Set(symbolicOperators);
    function isSymbolicOperator(operator) {
        return symbolicOperatorSet.has(operator);
    }

    var tokenTypes = [
        'LBrace',
        'LBracket',
        'RBrace',
        'RBracket',
        'LParen',
        'RParen',
        'BasePrefixedNumber',
        'MultiLineComment',
        'Number',
        'Operator',
        'RegexpShorthand',
        'ReservedSymbol',
        'SingleLineComment',
        'String',
        'Symbol',
        'Whitespace',
    ];
    function isTokenDebugData(tokenDebugData) {
        return (typeof tokenDebugData === 'object'
            && tokenDebugData !== null
            && 'sourceCodeInfo' in tokenDebugData);
    }
    function getTokenDebugData(token) {
        var debugData = token === null || token === void 0 ? void 0 : token.at(-1);
        return isTokenDebugData(debugData) ? debugData : undefined;
    }
    function hasTokenDebugData(token) {
        return isTokenDebugData(token === null || token === void 0 ? void 0 : token.at(-1));
    }
    function addTokenDebugData(token, debugData) {
        if (isTokenDebugData(token.at(-1))) {
            throw new Error("Token already has debug data: ".concat(token));
        }
        token.push(debugData);
    }
    function isTokenType(type) {
        return typeof type === 'string' && tokenTypes.includes(type);
    }
    function isToken$1(token) {
        return !!token;
    }
    function assertToken(token) {
        if (!isToken$1(token)) {
            throw new LitsError("Expected token, got ".concat(token), undefined);
        }
    }
    function asToken(token) {
        assertToken(token);
        return token;
    }
    function isSymbolToken(token, symbolName) {
        if ((token === null || token === void 0 ? void 0 : token[0]) !== 'Symbol') {
            return false;
        }
        if (symbolName && token[1] !== symbolName) {
            return false;
        }
        return true;
    }
    function assertSymbolToken(token, symbolName) {
        if (!isSymbolToken(token, symbolName)) {
            throwUnexpectedToken('Symbol', undefined, token);
        }
    }
    function asSymbolToken(token, symbolName) {
        assertSymbolToken(token, symbolName);
        return token;
    }
    function isReservedSymbolToken(token, symbolName) {
        if ((token === null || token === void 0 ? void 0 : token[0]) !== 'ReservedSymbol') {
            return false;
        }
        if (symbolName && token[1] !== symbolName) {
            return false;
        }
        return true;
    }
    function assertReservedSymbolToken(token, symbolName) {
        if (!isReservedSymbolToken(token, symbolName)) {
            throwUnexpectedToken('ReservedSymbol', symbolName, token);
        }
    }
    function isA_CommentToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'SingleLineComment';
    }
    function isMultiLineCommentToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'MultiLineComment';
    }
    function isOperatorToken(token, operatorName) {
        if ((token === null || token === void 0 ? void 0 : token[0]) !== 'Operator') {
            return false;
        }
        if (operatorName && token[1] !== operatorName) {
            return false;
        }
        return true;
    }
    function assertOperatorToken(token, operatorName) {
        if (!isOperatorToken(token, operatorName)) {
            if (operatorName) {
                throw new LitsError("Unexpected token: ".concat(token, ", expected operator ").concat(operatorName), undefined);
            }
            throwUnexpectedToken('Operator', operatorName, token);
        }
    }
    function isWhitespaceToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'Whitespace';
    }
    function isNumberToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'Number';
    }
    function isBasePrefixedNumberToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'BasePrefixedNumber';
    }
    function isLParenToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'LParen';
    }
    function isRParenToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'RParen';
    }
    function assertRParenToken(token) {
        if (!isRParenToken(token)) {
            throwUnexpectedToken('RParen', undefined, token);
        }
    }
    function isLBracketToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'LBracket';
    }
    function assertLBracketToken(token) {
        if (!isLBracketToken(token)) {
            throwUnexpectedToken('LBracket', undefined, token);
        }
    }
    function asLBracketToken(token) {
        assertLBracketToken(token);
        return token;
    }
    function isRBracketToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'RBracket';
    }
    function assertRBracketToken(token) {
        if (!isRBracketToken(token)) {
            throwUnexpectedToken('RBracket', undefined, token);
        }
    }
    function isLBraceToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'LBrace';
    }
    function assertLBraceToken(token) {
        if (!isLBraceToken(token)) {
            throwUnexpectedToken('LBrace', undefined, token);
        }
    }
    function asLBraceToken(token) {
        assertLBraceToken(token);
        return token;
    }
    function isRBraceToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'RBrace';
    }
    function assertRBraceToken(token) {
        if (!isRBraceToken(token)) {
            throwUnexpectedToken('RBrace', undefined, token);
        }
    }
    function isStringToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'String';
    }
    function assertStringToken(token) {
        if (!isStringToken(token)) {
            throwUnexpectedToken('String', undefined, token);
        }
    }
    function asStringToken(token) {
        assertStringToken(token);
        return token;
    }
    function isRegexpShorthandToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'RegexpShorthand';
    }
    function assertRegexpShorthandToken(token) {
        if (!isRegexpShorthandToken(token)) {
            throwUnexpectedToken('RegexpShorthand', undefined, token);
        }
    }
    function asRegexpShorthandToken(token) {
        assertRegexpShorthandToken(token);
        return token;
    }
    function isA_BinaryOperatorToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'Operator' && isBinaryOperator(token[1]);
    }
    function throwUnexpectedToken(expected, expectedValue, actual) {
        if (actual === undefined) {
            throw new LitsError("Unexpected end of input, expected ".concat(expected).concat(expectedValue ? " '".concat(expectedValue, "'") : ''), undefined);
        }
        var actualOutput = "".concat(actual[0]).concat(actual[1] ? " '".concat(actual[1], "'") : '');
        throw new LitsError("Unexpected token: ".concat(actualOutput, ", expected ").concat(expected).concat(expectedValue ? " '".concat(expectedValue, "'") : ''), getSourceCodeInfo(actual));
    }

    var FUNCTION_SYMBOL = '^^fn^^';
    var REGEXP_SYMBOL = '^^re^^';

    function isLitsFunction$1(func) {
        if (!isUnknownRecord$1(func))
            return false;
        return !!func[FUNCTION_SYMBOL] && isFunctionType(func.t);
    }
    function isUnknownRecord$1(value) {
        return typeof value === 'object' && value !== null;
    }
    function isToken(value) {
        return Array.isArray(value) && value.length >= 1 && value.length <= 3 && typeof value[0] === 'string' && isTokenType(value[0]);
    }
    function isAstNode$1(value) {
        return isUnknownRecord$1(value) && isAstNodeType(value.t);
    }
    function valueToString(value) {
        if (isLitsFunction$1(value))
            // eslint-disable-next-line ts/no-unsafe-member-access
            return "<function ".concat(value.name || '\u03BB', ">");
        if (isToken(value))
            return "".concat(value[0], "-token").concat(value[1] ? "\"".concat(value[1], "\"") : '');
        if (isAstNode$1(value))
            return "".concat(astNodeTypeName.get(value.t), "-node");
        if (value === null)
            return 'null';
        if (typeof value === 'object' && value instanceof RegExp)
            return "".concat(value);
        if (typeof value === 'object' && value instanceof Error)
            return value.toString();
        return JSON.stringify(value);
    }

    function getAssertionError(typeName, value, sourceCodeInfo) {
        return new LitsError("Expected ".concat(typeName, ", got ").concat(valueToString(value), "."), getSourceCodeInfo(value, sourceCodeInfo));
    }

    function assertNumberOfParams(count, node) {
        var _a, _b;
        assertCount({
            count: count,
            length: node.p.length,
            name: (_a = node.n) !== null && _a !== void 0 ? _a : 'expression',
            sourceCodeInfo: (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo,
        });
    }
    function isNonUndefined(value) {
        return value !== undefined;
    }
    function asNonUndefined(value, sourceCodeInfo) {
        assertNonUndefined(value, sourceCodeInfo);
        return value;
    }
    function assertNonUndefined(value, sourceCodeInfo) {
        if (!isNonUndefined(value))
            throw new LitsError('Unexpected undefined', getSourceCodeInfo(value, sourceCodeInfo));
    }
    function isUnknownRecord(value) {
        return value !== null && typeof value === 'object' && !Array.isArray(value);
    }
    function assertUnknownRecord(value, sourceCodeInfo) {
        if (!isUnknownRecord(value)) {
            throw new LitsError("Expected ".concat('UnknownRecord', ", got ").concat(valueToString(value), "."), getSourceCodeInfo(value, sourceCodeInfo));
        }
    }
    function asUnknownRecord(value, sourceCodeInfo) {
        assertUnknownRecord(value, sourceCodeInfo);
        return value;
    }
    function assertCount(_a) {
        var count = _a.count, length = _a.length, name = _a.name, sourceCodeInfo = _a.sourceCodeInfo;
        if (typeof count === 'number') {
            if (length !== count) {
                throw new LitsError("Wrong number of arguments to \"".concat(name, "\", expected ").concat(count, ", got ").concat(valueToString(length), "."), sourceCodeInfo);
            }
        }
        else {
            var min = count.min, max = count.max, even = count.even, odd = count.odd;
            if (even) {
                if (length % 2 !== 0) {
                    throw new LitsError("Wrong number of arguments to \"".concat(name, "\",, expected an even number, got ").concat(valueToString(length), "."), sourceCodeInfo);
                }
            }
            if (odd) {
                if (length % 2 !== 1) {
                    throw new LitsError("Wrong number of arguments to \"".concat(name, "\",, expected an odd number, got ").concat(valueToString(length), "."), sourceCodeInfo);
                }
            }
            if (typeof min === 'number' && length < min) {
                throw new LitsError("Wrong number of arguments to \"".concat(name, "\", expected at least ").concat(min, ", got ").concat(valueToString(length), "."), sourceCodeInfo);
            }
            if (typeof max === 'number' && length > max) {
                throw new LitsError("Wrong number of arguments to \"".concat(name, "\", expected at most ").concat(max, ", got ").concat(valueToString(length), "."), sourceCodeInfo);
            }
        }
    }

    function isLitsFunction(value) {
        if (value === null || typeof value !== 'object')
            return false;
        return !!value[FUNCTION_SYMBOL];
    }
    function assertLitsFunction(value, sourceCodeInfo) {
        if (!isLitsFunction(value))
            throw getAssertionError('LitsFunction', value, sourceCodeInfo);
    }
    function isBuiltinFunction(value) {
        return isUnknownRecord(value) && value.t === FunctionType.Builtin;
    }

    function isAny(value) {
        // TODO weak test
        return value !== undefined;
    }
    function asAny(value, sourceCodeInfo) {
        assertAny(value, sourceCodeInfo);
        return value;
    }
    function assertAny(value, sourceCodeInfo) {
        if (!isAny(value))
            throw getAssertionError('not undefined', value, sourceCodeInfo);
    }
    function isSeq(value) {
        return Array.isArray(value) || typeof value === 'string';
    }
    function asSeq(value, sourceCodeInfo) {
        assertSeq(value, sourceCodeInfo);
        return value;
    }
    function assertSeq(value, sourceCodeInfo) {
        if (!isSeq(value))
            throw getAssertionError('string or array', value, sourceCodeInfo);
    }
    function isObj(value) {
        return !(value === null
            || typeof value !== 'object'
            || Array.isArray(value)
            || value instanceof RegExp
            || isLitsFunction(value)
            || isRegularExpression(value));
    }
    function assertObj(value, sourceCodeInfo) {
        if (!isObj(value))
            throw getAssertionError('object', value, sourceCodeInfo);
    }
    function isColl(value) {
        return isSeq(value) || isObj(value);
    }
    function asColl(value, sourceCodeInfo) {
        assertColl(value, sourceCodeInfo);
        return value;
    }
    function assertColl(value, sourceCodeInfo) {
        if (!isColl(value))
            throw getAssertionError('string, array or object', value, sourceCodeInfo);
    }
    function isRegularExpression(regexp) {
        if (regexp === null || typeof regexp !== 'object')
            return false;
        return !!regexp[REGEXP_SYMBOL];
    }
    function assertRegularExpression(value, sourceCodeInfo) {
        if (!isRegularExpression(value))
            throw getAssertionError('RegularExpression', value, sourceCodeInfo);
    }
    function isStringOrRegularExpression(value) {
        return isRegularExpression(value) || typeof value === 'string';
    }
    function assertStringOrRegularExpression(value, sourceCodeInfo) {
        if (!isStringOrRegularExpression(value))
            throw getAssertionError('string or RegularExpression', value, sourceCodeInfo);
    }

    function stringifyValue(value, html) {
        var _a;
        var gt = '>';
        var lt = '<';
        if (isLitsFunction(value)) {
            if (value.t === FunctionType.Builtin)
                return "".concat(lt, "builtin function ").concat(value.n).concat(gt);
            else
                return "".concat(lt, "function ").concat((_a = value.n) !== null && _a !== void 0 ? _a : '\u03BB').concat(gt);
        }
        if (value === null)
            return 'null';
        if (typeof value === 'object' && value instanceof Error)
            return value.toString();
        if (typeof value === 'object' && value instanceof RegExp)
            return "".concat(value);
        if (value === Number.POSITIVE_INFINITY)
            return "".concat(Number.POSITIVE_INFINITY);
        if (value === Number.NEGATIVE_INFINITY)
            return "".concat(Number.NEGATIVE_INFINITY);
        if (typeof value === 'number' && Number.isNaN(value))
            return 'NaN';
        if (isRegularExpression(value))
            return "/".concat(value.s, "/").concat(value.f);
        if (typeof value === 'string')
            return "\"".concat(value, "\"");
        return JSON.stringify(value, null, 2);
    }

    function getRangeString(options) {
        var hasUpperAndLowerBound = (typeof options.gt === 'number' || typeof options.gte === 'number')
            && (typeof options.lt === 'number' || typeof options.lte === 'number');
        if (hasUpperAndLowerBound) {
            return "".concat(typeof options.gt === 'number' ? "".concat(options.gt, " < n ") : "".concat(options.gte, " <= n ")).concat(typeof options.lt === 'number' ? "< ".concat(options.lt) : "<= ".concat(options.lte));
        }
        else if (typeof options.gt === 'number' || typeof options.gte === 'number') {
            return "".concat(typeof options.gt === 'number' ? "n > ".concat(options.gt) : "n >= ".concat(options.gte));
        }
        else if (typeof options.lt === 'number' || typeof options.lte === 'number') {
            return "".concat(typeof options.lt === 'number' ? "n < ".concat(options.lt) : "n <= ".concat(options.lte));
        }
        else {
            return '';
        }
    }
    function getSignString(options) {
        return options.positive
            ? 'positive'
            : options.negative
                ? 'negative'
                : options.nonNegative
                    ? 'non negative'
                    : options.nonPositive
                        ? 'non positive'
                        : options.nonZero
                            ? 'non zero'
                            : '';
    }
    function getNumberTypeName(options) {
        if (options.zero)
            return 'zero';
        var sign = getSignString(options);
        var numberType = options.integer ? 'integer' : 'number';
        var finite = options.finite ? 'finite' : '';
        var range = getRangeString(options);
        return [sign, finite, numberType, range].filter(function (x) { return !!x; }).join(' ');
    }
    function isNumber(value, options) {
        if (options === void 0) { options = {}; }
        if (typeof value !== 'number')
            return false;
        if (options.integer && !Number.isInteger(value))
            return false;
        if (options.finite && !Number.isFinite(value))
            return false;
        if (options.zero && value !== 0)
            return false;
        if (options.nonZero && value === 0)
            return false;
        if (options.positive && value <= 0)
            return false;
        if (options.negative && value >= 0)
            return false;
        if (options.nonPositive && value > 0)
            return false;
        if (options.nonNegative && value < 0)
            return false;
        if (typeof options.gt === 'number' && value <= options.gt)
            return false;
        if (typeof options.gte === 'number' && value < options.gte)
            return false;
        if (typeof options.lt === 'number' && value >= options.lt)
            return false;
        if (typeof options.lte === 'number' && value > options.lte)
            return false;
        return true;
    }
    function assertNumber(value, sourceCodeInfo, options) {
        if (options === void 0) { options = {}; }
        if (!isNumber(value, options)) {
            throw new LitsError("Expected ".concat(getNumberTypeName(options), ", got ").concat(valueToString(value), "."), getSourceCodeInfo(value, sourceCodeInfo));
        }
    }
    function asNumber(value, sourceCodeInfo, options) {
        if (options === void 0) { options = {}; }
        assertNumber(value, sourceCodeInfo, options);
        return value;
    }

    var bitwiseNormalExpression = {
        '<<': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], count = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true });
                return num << count;
            },
            paramCount: 2,
        },
        '>>': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], count = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true });
                return num >> count;
            },
            paramCount: 2,
        },
        '>>>': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], count = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true });
                return num >>> count;
            },
            paramCount: 2,
        },
        '~': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), num = _b[0];
                assertNumber(num, sourceCodeInfo, { integer: true });
                return ~num;
            },
            paramCount: 1,
        },
        '&': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), first = _b[0], rest = _b.slice(1);
                assertNumber(first, sourceCodeInfo, { integer: true });
                return rest.reduce(function (result, value) {
                    assertNumber(value, sourceCodeInfo, { integer: true });
                    return result & value;
                }, first);
            },
            paramCount: { min: 2 },
        },
        'bit-and-not': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), first = _b[0], rest = _b.slice(1);
                assertNumber(first, sourceCodeInfo, { integer: true });
                return rest.reduce(function (result, value) {
                    assertNumber(value, sourceCodeInfo, { integer: true });
                    return result & ~value;
                }, first);
            },
            paramCount: { min: 2 },
        },
        '|': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), first = _b[0], rest = _b.slice(1);
                assertNumber(first, sourceCodeInfo, { integer: true });
                return rest.reduce(function (result, value) {
                    assertNumber(value, sourceCodeInfo, { integer: true });
                    return result | value;
                }, first);
            },
            paramCount: { min: 2 },
        },
        '^': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), first = _b[0], rest = _b.slice(1);
                assertNumber(first, sourceCodeInfo, { integer: true });
                return rest.reduce(function (result, value) {
                    assertNumber(value, sourceCodeInfo, { integer: true });
                    return result ^ value;
                }, first);
            },
            paramCount: { min: 2 },
        },
        'bit-flip': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], index = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true });
                var mask = 1 << index;
                return (num ^= mask);
            },
            paramCount: 2,
        },
        'bit-set': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], index = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true });
                var mask = 1 << index;
                return (num |= mask);
            },
            paramCount: 2,
        },
        'bit-clear': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], index = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true });
                var mask = 1 << index;
                return (num &= ~mask);
            },
            paramCount: 2,
        },
        'bit-test': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], index = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true });
                var mask = 1 << index;
                return !!(num & mask);
            },
            paramCount: 2,
        },
    };

    function isString(value, options) {
        if (options === void 0) { options = {}; }
        if (typeof value !== 'string')
            return false;
        if (options.nonEmpty && value.length === 0)
            return false;
        if (options.char && value.length !== 1)
            return false;
        return true;
    }
    function assertString(value, sourceCodeInfo, options) {
        if (options === void 0) { options = {}; }
        if (!isString(value, options)) {
            throw getAssertionError("".concat(options.nonEmpty ? 'non empty string' : options.char ? 'character' : 'string'), value, sourceCodeInfo);
        }
    }
    function asString(value, sourceCodeInfo, options) {
        if (options === void 0) { options = {}; }
        assertString(value, sourceCodeInfo, options);
        return value;
    }
    function isStringOrNumber(value) {
        return typeof value === 'string' || typeof value === 'number';
    }
    function asStringOrNumber(value, sourceCodeInfo) {
        assertStringOrNumber(value, sourceCodeInfo);
        return value;
    }
    function assertStringOrNumber(value, sourceCodeInfo) {
        if (!isStringOrNumber(value))
            throw getAssertionError('string or number', value, sourceCodeInfo);
    }

    function collHasKey(coll, key) {
        if (!isColl(coll))
            return false;
        if (typeof coll === 'string' || Array.isArray(coll)) {
            if (!isNumber(key, { integer: true }))
                return false;
            return key >= 0 && key < coll.length;
        }
        return !!Object.getOwnPropertyDescriptor(coll, key);
    }
    function compare(a, b, sourceCodeInfo) {
        assertStringOrNumber(a, sourceCodeInfo);
        assertStringOrNumber(b, sourceCodeInfo);
        if (typeof a === 'string' && typeof b === 'string') {
            return a < b ? -1 : a > b ? 1 : 0;
        }
        if (typeof a === 'number' && typeof b === 'number') {
            return Math.sign((a) - (b));
        }
        throw new LitsError("Cannot compare values of different types: ".concat(typeof a, " and ").concat(typeof b), sourceCodeInfo);
    }
    function deepEqual(a, b, sourceCodeInfo) {
        if (a === b)
            return true;
        if (typeof a === 'number' && typeof b === 'number')
            return Math.abs(a - b) < Number.EPSILON;
        if (Array.isArray(a) && Array.isArray(b)) {
            if (a.length !== b.length)
                return false;
            for (var i = 0; i < a.length; i += 1) {
                if (!deepEqual(asAny(a[i], sourceCodeInfo), asAny(b[i], sourceCodeInfo), sourceCodeInfo))
                    return false;
            }
            return true;
        }
        if (isRegularExpression(a) && isRegularExpression(b))
            return a.s === b.s && a.f === b.f;
        if (isUnknownRecord(a) && isUnknownRecord(b)) {
            var aKeys = Object.keys(a);
            var bKeys = Object.keys(b);
            if (aKeys.length !== bKeys.length)
                return false;
            for (var i = 0; i < aKeys.length; i += 1) {
                var key = asString(aKeys[i], sourceCodeInfo);
                if (!deepEqual(toAny(a[key]), toAny(b[key]), sourceCodeInfo))
                    return false;
            }
            return true;
        }
        return false;
    }
    function toNonNegativeInteger(num) {
        return Math.max(0, Math.ceil(num));
    }
    function toAny(value) {
        return (value !== null && value !== void 0 ? value : null);
    }
    function clone(value) {
        if (isObj(value)) {
            return Object.entries(value).reduce(function (result, entry) {
                var _a = __read(entry, 2), key = _a[0], val = _a[1];
                result[key] = clone(val);
                return result;
            }, {});
        }
        if (Array.isArray(value))
            // eslint-disable-next-line ts/no-unsafe-return
            return value.map(function (item) { return clone(item); });
        return value;
    }
    function cloneColl(value) {
        return clone(value);
    }
    function arrayToPairs(arr) {
        var pairs = [];
        for (var i = 0; i < arr.length; i += 2)
            pairs.push([arr[i], arr[i + 1]]);
        return pairs;
    }
    function joinSets() {
        var e_1, _a;
        var results = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            results[_i] = arguments[_i];
        }
        var result = new Set();
        try {
            for (var results_1 = __values(results), results_1_1 = results_1.next(); !results_1_1.done; results_1_1 = results_1.next()) {
                var symbols = results_1_1.value;
                symbols.forEach(function (symbol) { return result.add(symbol); });
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
    function addToSet(target, source) {
        source.forEach(function (symbol) { return target.add(symbol); });
    }

    // isArray not needed, use Array.isArary
    function asArray(value, sourceCodeInfo) {
        assertArray(value, sourceCodeInfo);
        return value;
    }
    function assertArray(value, sourceCodeInfo) {
        if (!Array.isArray(value))
            throw getAssertionError('array', value, sourceCodeInfo);
    }
    function isStringArray(value) {
        return Array.isArray(value) && value.every(function (v) { return typeof v === 'string'; });
    }
    function assertStringArray(value, sourceCodeInfo) {
        if (!isStringArray(value))
            throw getAssertionError('array of strings', value, sourceCodeInfo);
    }
    function isCharArray(value) {
        return Array.isArray(value) && value.every(function (v) { return typeof v === 'string' && v.length === 1; });
    }
    function assertCharArray(value, sourceCodeInfo) {
        if (!isCharArray(value))
            throw getAssertionError('array of strings', value, sourceCodeInfo);
    }

    function cloneAndGetMeta(originalColl, keys, sourceCodeInfo) {
        var coll = cloneColl(originalColl);
        var butLastKeys = keys.slice(0, keys.length - 1);
        var innerCollMeta = butLastKeys.reduce(function (result, key) {
            var resultColl = result.coll;
            var newResultColl;
            if (Array.isArray(resultColl)) {
                assertNumber(key, sourceCodeInfo);
                newResultColl = asColl(resultColl[key], sourceCodeInfo);
            }
            else {
                assertObj(resultColl, sourceCodeInfo);
                assertString(key, sourceCodeInfo);
                if (!collHasKey(result.coll, key))
                    resultColl[key] = {};
                newResultColl = asColl(resultColl[key], sourceCodeInfo);
            }
            return { coll: newResultColl, parent: resultColl };
        }, { coll: coll, parent: {} });
        return { coll: coll, innerCollMeta: innerCollMeta };
    }
    function get(coll, key) {
        if (isObj(coll)) {
            if (typeof key === 'string' && collHasKey(coll, key))
                return toAny(coll[key]);
        }
        else {
            if (isNumber(key, { nonNegative: true, integer: true }) && key >= 0 && key < coll.length)
                return toAny(coll[key]);
        }
        return undefined;
    }
    function update(coll, key, fn, params, contextStack, executeFunction, sourceCodeInfo) {
        if (isObj(coll)) {
            assertString(key, sourceCodeInfo);
            var result = __assign({}, coll);
            result[key] = executeFunction(fn, __spreadArray([result[key]], __read(params), false), contextStack, sourceCodeInfo);
            return result;
        }
        else {
            assertNumber(key, sourceCodeInfo);
            var intKey_1 = toNonNegativeInteger(key);
            assertNumber(intKey_1, sourceCodeInfo, { lte: coll.length });
            if (Array.isArray(coll)) {
                var result = coll.map(function (elem, index) {
                    if (intKey_1 === index)
                        return executeFunction(fn, __spreadArray([elem], __read(params), false), contextStack, sourceCodeInfo);
                    return elem;
                });
                if (intKey_1 === coll.length)
                    result[intKey_1] = executeFunction(fn, __spreadArray([undefined], __read(params), false), contextStack, sourceCodeInfo);
                return result;
            }
            else {
                var result = coll.split('').map(function (elem, index) {
                    if (intKey_1 === index) {
                        return asString(executeFunction(fn, __spreadArray([elem], __read(params), false), contextStack, sourceCodeInfo), sourceCodeInfo, {
                            char: true,
                        });
                    }
                    return elem;
                });
                if (intKey_1 === coll.length) {
                    result[intKey_1] = asString(executeFunction(fn, __spreadArray([undefined], __read(params), false), contextStack, sourceCodeInfo), sourceCodeInfo, {
                        char: true,
                    });
                }
                return result.join('');
            }
        }
    }
    function assoc(coll, key, value, sourceCodeInfo) {
        assertColl(coll, sourceCodeInfo);
        assertStringOrNumber(key, sourceCodeInfo);
        if (Array.isArray(coll) || typeof coll === 'string') {
            assertNumber(key, sourceCodeInfo, { integer: true });
            assertNumber(key, sourceCodeInfo, { gte: 0 });
            assertNumber(key, sourceCodeInfo, { lte: coll.length });
            if (typeof coll === 'string') {
                assertString(value, sourceCodeInfo, { char: true });
                return "".concat(coll.slice(0, key)).concat(value).concat(coll.slice(key + 1));
            }
            var copy_1 = __spreadArray([], __read(coll), false);
            copy_1[key] = value;
            return copy_1;
        }
        assertString(key, sourceCodeInfo);
        var copy = __assign({}, coll);
        copy[key] = value;
        return copy;
    }
    var collectionNormalExpression = {
        'get': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(params, 2), coll = _a[0], key = _a[1];
                var defaultValue = toAny(params[2]);
                assertStringOrNumber(key, sourceCodeInfo);
                if (coll === null)
                    return defaultValue;
                assertColl(coll, sourceCodeInfo);
                var result = get(coll, key);
                return result === undefined ? defaultValue : result;
            },
            paramCount: { min: 2, max: 3 },
        },
        'get-in': {
            evaluate: function (params, sourceCodeInfo) {
                var e_1, _a;
                var _b;
                var coll = toAny(params[0]);
                var keys = (_b = params[1]) !== null && _b !== void 0 ? _b : []; // null behaves as empty array
                var defaultValue = toAny(params[2]);
                assertArray(keys, sourceCodeInfo);
                try {
                    for (var keys_1 = __values(keys), keys_1_1 = keys_1.next(); !keys_1_1.done; keys_1_1 = keys_1.next()) {
                        var key = keys_1_1.value;
                        assertStringOrNumber(key, sourceCodeInfo);
                        if (isColl(coll)) {
                            var nextValue = get(coll, key);
                            if (nextValue !== undefined)
                                coll = nextValue;
                            else
                                return defaultValue;
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
            paramCount: { min: 2, max: 3 },
        },
        'count': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), coll = _b[0];
                if (coll === null)
                    return 0;
                if (typeof coll === 'string')
                    return coll.length;
                assertColl(coll, sourceCodeInfo);
                if (Array.isArray(coll))
                    return coll.length;
                return Object.keys(coll).length;
            },
            paramCount: 1,
        },
        'contains?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), coll = _b[0], key = _b[1];
                if (coll === null)
                    return false;
                assertColl(coll, sourceCodeInfo);
                if (isString(coll)) {
                    assertString(key, sourceCodeInfo);
                    return coll.includes(key);
                }
                if (isSeq(coll)) {
                    assertAny(key, sourceCodeInfo);
                    return !!coll.find(function (elem) { return deepEqual(asAny(elem), key, sourceCodeInfo); });
                }
                assertString(key, sourceCodeInfo);
                return key in coll;
            },
            paramCount: 2,
        },
        'assoc': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), coll = _b[0], key = _b[1], value = _b[2];
                assertColl(coll, sourceCodeInfo);
                assertStringOrNumber(key, sourceCodeInfo);
                assertAny(value, sourceCodeInfo);
                return assoc(coll, key, value, sourceCodeInfo);
            },
            paramCount: 3,
        },
        'assoc-in': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), originalColl = _b[0], keys = _b[1], value = _b[2];
                assertColl(originalColl, sourceCodeInfo);
                assertArray(keys, sourceCodeInfo);
                assertAny(value, sourceCodeInfo);
                if (keys.length === 1) {
                    assertStringOrNumber(keys[0], sourceCodeInfo);
                    return assoc(originalColl, keys[0], value, sourceCodeInfo);
                }
                var _c = cloneAndGetMeta(originalColl, keys, sourceCodeInfo), coll = _c.coll, innerCollMeta = _c.innerCollMeta;
                var lastKey = asStringOrNumber(keys[keys.length - 1], sourceCodeInfo);
                var parentKey = asStringOrNumber(keys[keys.length - 2], sourceCodeInfo);
                if (Array.isArray(innerCollMeta.parent)) {
                    assertNumber(parentKey, sourceCodeInfo);
                    innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value, sourceCodeInfo);
                }
                else {
                    assertString(parentKey, sourceCodeInfo);
                    innerCollMeta.parent[parentKey] = assoc(innerCollMeta.coll, lastKey, value, sourceCodeInfo);
                }
                return coll;
            },
            paramCount: 3,
        },
        'update': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a), coll = _c[0], key = _c[1], fn = _c[2], params = _c.slice(3);
                var executeFunction = _b.executeFunction;
                assertColl(coll, sourceCodeInfo);
                assertStringOrNumber(key, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                return update(coll, key, fn, params, contextStack, executeFunction, sourceCodeInfo);
            },
            paramCount: { min: 3 },
        },
        'update-in': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a), originalColl = _c[0], keys = _c[1], fn = _c[2], params = _c.slice(3);
                var executeFunction = _b.executeFunction;
                assertColl(originalColl, sourceCodeInfo);
                assertArray(keys, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                if (keys.length === 1) {
                    assertStringOrNumber(keys[0], sourceCodeInfo);
                    return update(originalColl, keys[0], fn, params, contextStack, executeFunction, sourceCodeInfo);
                }
                var _d = cloneAndGetMeta(originalColl, keys, sourceCodeInfo), coll = _d.coll, innerCollMeta = _d.innerCollMeta;
                var lastKey = asStringOrNumber(keys[keys.length - 1], sourceCodeInfo);
                var parentKey = asStringOrNumber(keys[keys.length - 2], sourceCodeInfo);
                if (Array.isArray(innerCollMeta.parent)) {
                    assertNumber(parentKey, sourceCodeInfo);
                    innerCollMeta.parent[parentKey] = update(innerCollMeta.coll, lastKey, fn, params, contextStack, executeFunction, sourceCodeInfo);
                }
                else {
                    assertString(parentKey, sourceCodeInfo);
                    innerCollMeta.parent[parentKey] = update(innerCollMeta.coll, lastKey, fn, params, contextStack, executeFunction, sourceCodeInfo);
                }
                return coll;
            },
            paramCount: { min: 3 },
        },
        '++': {
            evaluate: function (params, sourceCodeInfo) {
                if (!isNumber(params[0])) {
                    assertColl(params[0], sourceCodeInfo);
                }
                if (Array.isArray(params[0])) {
                    return params.reduce(function (result, arr) {
                        assertArray(arr, sourceCodeInfo);
                        return result.concat(arr);
                    }, []);
                }
                else if (isStringOrNumber(params[0])) {
                    return params.reduce(function (result, s) {
                        assertStringOrNumber(s, sourceCodeInfo);
                        return "".concat(result).concat(s);
                    }, '');
                }
                else {
                    return params.reduce(function (result, obj) {
                        assertObj(obj, sourceCodeInfo);
                        return Object.assign(result, obj);
                    }, {});
                }
            },
            paramCount: { min: 1 },
            aliases: ['concat'],
        },
        'not-empty': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), coll = _b[0];
                if (coll === null)
                    return null;
                assertColl(coll, sourceCodeInfo);
                if (typeof coll === 'string')
                    return coll.length > 0 ? coll : null;
                if (Array.isArray(coll))
                    return coll.length > 0 ? coll : null;
                return Object.keys(coll).length > 0 ? coll : null;
            },
            paramCount: 1,
        },
        'every?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), coll = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertColl(coll, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                if (Array.isArray(coll))
                    return coll.every(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                if (typeof coll === 'string')
                    return coll.split('').every(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                return Object.entries(coll).every(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
            },
            paramCount: 2,
        },
        'any?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), coll = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, sourceCodeInfo);
                assertColl(coll, sourceCodeInfo);
                if (Array.isArray(coll))
                    return coll.some(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                if (typeof coll === 'string')
                    return coll.split('').some(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                return Object.entries(coll).some(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
            },
            paramCount: 2,
        },
        'not-any?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), coll = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, sourceCodeInfo);
                assertColl(coll, sourceCodeInfo);
                if (Array.isArray(coll))
                    return !coll.some(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                if (typeof coll === 'string')
                    return !coll.split('').some(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                return !Object.entries(coll).some(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
            },
            paramCount: 2,
        },
        'not-every?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), coll = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, sourceCodeInfo);
                assertColl(coll, sourceCodeInfo);
                if (Array.isArray(coll))
                    return !coll.every(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                if (typeof coll === 'string')
                    return !coll.split('').every(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                return !Object.entries(coll).every(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
            },
            paramCount: 2,
        },
    };

    var arrayNormalExpression = {
        array: {
            evaluate: function (params) { return params; },
            paramCount: {},
        },
        range: {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(params, 3), first = _a[0], second = _a[1], third = _a[2];
                var from;
                var to;
                var step;
                assertNumber(first, sourceCodeInfo, { finite: true });
                if (params.length === 1) {
                    from = 0;
                    to = first;
                    step = to >= 0 ? 1 : -1;
                }
                else if (params.length === 2) {
                    assertNumber(second, sourceCodeInfo, { finite: true });
                    from = first;
                    to = second;
                    step = to >= from ? 1 : -1;
                }
                else {
                    assertNumber(second, sourceCodeInfo, { finite: true });
                    assertNumber(third, sourceCodeInfo, { finite: true });
                    from = first;
                    to = second;
                    step = third;
                    if (to > from)
                        assertNumber(step, sourceCodeInfo, { positive: true });
                    else if (to < from)
                        assertNumber(step, sourceCodeInfo, { negative: true });
                    else
                        assertNumber(step, sourceCodeInfo, { nonZero: true });
                }
                var result = [];
                for (var i = from; step < 0 ? i > to : i < to; i += step)
                    result.push(i);
                return result;
            },
            paramCount: { min: 1, max: 3 },
        },
        repeat: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), value = _b[0], count = _b[1];
                assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true });
                var result = [];
                for (var i = 0; i < count; i += 1)
                    result.push(value);
                return result;
            },
            paramCount: 2,
        },
        flatten: {
            evaluate: function (_a) {
                var _b = __read(_a, 1), seq = _b[0];
                if (!Array.isArray(seq))
                    return [];
                return seq.flat(Number.POSITIVE_INFINITY);
            },
            paramCount: 1,
        },
        mapcat: {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), arr = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertArray(arr, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                return arr.map(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); }).flat(1);
            },
            paramCount: 2,
        },
    };

    var sequenceNormalExpression = {
        'nth': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(params, 2), seq = _a[0], i = _a[1];
                var defaultValue = toAny(params[2]);
                assertNumber(i, sourceCodeInfo, { integer: true });
                if (seq === null)
                    return defaultValue;
                assertSeq(seq, sourceCodeInfo);
                return i >= 0 && i < seq.length ? toAny(seq[i]) : defaultValue;
            },
            paramCount: { min: 2, max: 3 },
        },
        'filter': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), seq = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertSeq(seq, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                if (Array.isArray(seq))
                    return seq.filter(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                return seq
                    .split('')
                    .filter(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); })
                    .join('');
            },
            paramCount: 2,
        },
        'first': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), array = _b[0];
                if (array === null)
                    return null;
                assertSeq(array, sourceCodeInfo);
                return toAny(array[0]);
            },
            paramCount: 1,
        },
        'last': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), array = _b[0];
                if (array === null)
                    return null;
                assertSeq(array, sourceCodeInfo);
                return toAny(array[array.length - 1]);
            },
            paramCount: 1,
        },
        'map': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), seq = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertSeq(seq, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                if (Array.isArray(seq)) {
                    return seq.map(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                }
                else {
                    return seq
                        .split('')
                        .map(function (elem) {
                        var newVal = executeFunction(fn, [elem], contextStack, sourceCodeInfo);
                        assertString(newVal, sourceCodeInfo, { char: true });
                        return newVal;
                    })
                        .join('');
                }
            },
            paramCount: 2,
        },
        'pop': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), seq = _b[0];
                assertSeq(seq, sourceCodeInfo);
                if (typeof seq === 'string') {
                    return seq.substring(0, seq.length - 1);
                }
                return seq.slice(0, seq.length - 1);
            },
            paramCount: 1,
        },
        'position': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), seq = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, sourceCodeInfo);
                if (seq === null)
                    return null;
                assertSeq(seq, sourceCodeInfo);
                if (typeof seq === 'string') {
                    var index = seq.split('').findIndex(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                    return index !== -1 ? index : null;
                }
                else {
                    var index = seq.findIndex(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                    return index !== -1 ? index : null;
                }
            },
            paramCount: 2,
        },
        'index-of': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), seq = _b[0], value = _b[1];
                assertAny(value, sourceCodeInfo);
                if (seq === null)
                    return null;
                assertSeq(seq, sourceCodeInfo);
                if (typeof seq === 'string') {
                    assertString(value, sourceCodeInfo);
                    var index = seq.indexOf(value);
                    return index !== -1 ? index : null;
                }
                else {
                    var index = seq.findIndex(function (item) { return deepEqual(asAny(item, sourceCodeInfo), value); }, sourceCodeInfo);
                    return index !== -1 ? index : null;
                }
            },
            paramCount: 2,
        },
        'last-index-of': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), seq = _b[0], value = _b[1];
                assertAny(value, sourceCodeInfo);
                if (seq === null)
                    return null;
                assertSeq(seq, sourceCodeInfo);
                if (typeof seq === 'string') {
                    assertString(value, sourceCodeInfo);
                    var index = seq.lastIndexOf(value);
                    return index !== -1 ? index : null;
                }
                else {
                    var index = seq.findLastIndex(function (item) { return deepEqual(asAny(item, sourceCodeInfo), value); }, sourceCodeInfo);
                    return index !== -1 ? index : null;
                }
            },
            paramCount: 2,
        },
        'push': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), seq = _b[0], values = _b.slice(1);
                assertSeq(seq, sourceCodeInfo);
                if (typeof seq === 'string') {
                    assertCharArray(values, sourceCodeInfo);
                    return __spreadArray([seq], __read(values), false).join('');
                }
                else {
                    return __spreadArray(__spreadArray([], __read(seq), false), __read(values), false);
                }
            },
            paramCount: { min: 2 },
        },
        'reductions': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var _b = __read(params, 2), seq = _b[0], fn = _b[1];
                assertSeq(seq, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                if (params.length === 2) {
                    if (seq.length === 0)
                        return [executeFunction(fn, [], contextStack, sourceCodeInfo)];
                    else if (seq.length === 1)
                        return [toAny(seq[0])];
                    if (typeof seq === 'string') {
                        var chars = seq.split('');
                        var resultArray_1 = [asAny(chars[0], sourceCodeInfo)];
                        chars.slice(1).reduce(function (result, elem) {
                            var newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                            resultArray_1.push(newVal);
                            return newVal;
                        }, asAny(chars[0], sourceCodeInfo));
                        return resultArray_1;
                    }
                    else {
                        var resultArray_2 = [toAny(seq[0])];
                        seq.slice(1).reduce(function (result, elem) {
                            var newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                            resultArray_2.push(newVal);
                            return newVal;
                        }, toAny(seq[0]));
                        return resultArray_2;
                    }
                }
                else {
                    var val = params[2];
                    assertAny(val, sourceCodeInfo);
                    if (typeof seq === 'string') {
                        assertString(val, sourceCodeInfo);
                        if (seq.length === 0)
                            return [val];
                        var resultArray_3 = [val];
                        seq.split('').reduce(function (result, elem) {
                            var newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                            resultArray_3.push(newVal);
                            return newVal;
                        }, val);
                        return resultArray_3;
                    }
                    else {
                        if (seq.length === 0)
                            return [val];
                        var resultArray_4 = [val];
                        seq.reduce(function (result, elem) {
                            var newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                            resultArray_4.push(newVal);
                            return newVal;
                        }, val);
                        return resultArray_4;
                    }
                }
            },
            paramCount: { min: 2, max: 3 },
        },
        'reduce': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var _b = __read(params, 2), seq = _b[0], fn = _b[1];
                assertSeq(seq, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                if (params.length === 2) {
                    if (seq.length === 0)
                        return executeFunction(fn, [], contextStack, sourceCodeInfo);
                    else if (seq.length === 1)
                        return toAny(seq[0]);
                    if (typeof seq === 'string') {
                        var chars = seq.split('');
                        return chars.slice(1).reduce(function (result, elem) {
                            var val = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                            return val;
                        }, asAny(chars[0], sourceCodeInfo));
                    }
                    else {
                        return seq.slice(1).reduce(function (result, elem) {
                            return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                        }, toAny(seq[0]));
                    }
                }
                else {
                    var val = params[2];
                    assertAny(val, sourceCodeInfo);
                    if (typeof seq === 'string') {
                        assertString(val, sourceCodeInfo);
                        if (seq.length === 0)
                            return val;
                        return seq.split('').reduce(function (result, elem) {
                            var newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                            return newVal;
                        }, val);
                    }
                    else {
                        if (seq.length === 0)
                            return val;
                        return seq.reduce(function (result, elem) {
                            return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                        }, val);
                    }
                }
            },
            paramCount: { min: 2, max: 3 },
        },
        'reduce-right': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var _b = __read(params, 2), seq = _b[0], fn = _b[1];
                assertSeq(seq, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                if (params.length === 2) {
                    if (seq.length === 0)
                        return executeFunction(fn, [], contextStack, sourceCodeInfo);
                    else if (seq.length === 1)
                        return toAny(seq[0]);
                    if (typeof seq === 'string') {
                        var chars = seq.split('');
                        return chars.slice(0, chars.length - 1).reduceRight(function (result, elem) {
                            var newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                            assertString(newVal, sourceCodeInfo);
                            return newVal;
                        }, chars[chars.length - 1]);
                    }
                    else {
                        return seq.slice(0, seq.length - 1).reduceRight(function (result, elem) {
                            return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                        }, asAny(seq[seq.length - 1], sourceCodeInfo));
                    }
                }
                else {
                    var val = params[2];
                    assertAny(val, sourceCodeInfo);
                    assertSeq(seq, sourceCodeInfo);
                    if (typeof seq === 'string') {
                        if (seq.length === 0)
                            return val;
                        return seq.split('').reduceRight(function (result, elem) {
                            var newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                            return newVal;
                        }, val);
                    }
                    else {
                        if (seq.length === 0)
                            return val;
                        return seq.reduceRight(function (result, elem) {
                            return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                        }, val);
                    }
                }
            },
            paramCount: { min: 2, max: 3 },
        },
        'rest': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), seq = _b[0];
                assertSeq(seq, sourceCodeInfo);
                if (Array.isArray(seq)) {
                    if (seq.length <= 1)
                        return [];
                    return seq.slice(1);
                }
                return seq.substring(1);
            },
            paramCount: 1,
        },
        'next': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), seq = _b[0];
                assertSeq(seq, sourceCodeInfo);
                if (Array.isArray(seq)) {
                    if (seq.length <= 1)
                        return null;
                    return seq.slice(1);
                }
                if (seq.length <= 1)
                    return null;
                return seq.substring(1);
            },
            paramCount: 1,
        },
        'reverse': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), seq = _b[0];
                if (seq === null)
                    return null;
                assertSeq(seq, sourceCodeInfo);
                if (Array.isArray(seq))
                    return __spreadArray([], __read(seq), false).reverse();
                return seq.split('').reverse().join('');
            },
            paramCount: 1,
        },
        'second': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), seq = _b[0];
                if (seq === null)
                    return null;
                assertSeq(seq, sourceCodeInfo);
                return toAny(seq[1]);
            },
            paramCount: 1,
        },
        'shift': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), seq = _b[0];
                assertSeq(seq, sourceCodeInfo);
                if (typeof seq === 'string')
                    return seq.substring(1);
                var copy = __spreadArray([], __read(seq), false);
                copy.shift();
                return copy;
            },
            paramCount: 1,
        },
        'slice': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(params, 3), seq = _a[0], from = _a[1], to = _a[2];
                assertSeq(seq, sourceCodeInfo);
                assertNumber(from, sourceCodeInfo, { integer: true });
                if (params.length === 2)
                    return seq.slice(from);
                assertNumber(to, sourceCodeInfo, { integer: true });
                return seq.slice(from, to);
            },
            paramCount: { min: 2, max: 3 },
        },
        'splice': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(params), seq = _a[0], start = _a[1], deleteCount = _a[2], rest = _a.slice(3);
                assertSeq(seq, sourceCodeInfo);
                assertNumber(start, sourceCodeInfo, { integer: true });
                assertNumber(deleteCount, sourceCodeInfo, { integer: true, nonNegative: true });
                var from = start < 0 ? seq.length + start : start;
                if (Array.isArray(seq)) {
                    return __spreadArray(__spreadArray(__spreadArray([], __read(seq.slice(0, from)), false), __read(rest), false), __read(seq.slice(from + deleteCount)), false);
                }
                rest.forEach(function (elem) { return assertString(elem, sourceCodeInfo); });
                return "".concat(seq.substring(0, from)).concat(rest.join('')).concat(seq.substring(from + deleteCount));
            },
            paramCount: { min: 3 },
        },
        'some': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c;
                var _d = __read(_a, 2), seq = _d[0], fn = _d[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, sourceCodeInfo);
                if (seq === null)
                    return null;
                assertSeq(seq, sourceCodeInfo);
                if (seq.length === 0)
                    return null;
                if (typeof seq === 'string')
                    return (_c = seq.split('').find(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); })) !== null && _c !== void 0 ? _c : null;
                return toAny(seq.find(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); }));
            },
            paramCount: 2,
        },
        'sort': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var _b = __read(params, 1), seq = _b[0];
                var defaultComparer = params.length === 1;
                var comparer = defaultComparer ? null : params[1];
                assertSeq(seq, sourceCodeInfo);
                if (typeof seq === 'string') {
                    var result_1 = seq.split('');
                    if (defaultComparer) {
                        result_1.sort(function (a, b) { return compare(a, b, sourceCodeInfo); });
                    }
                    else {
                        assertLitsFunction(comparer, sourceCodeInfo);
                        result_1.sort(function (a, b) {
                            var compareValue = executeFunction(comparer, [a, b], contextStack, sourceCodeInfo);
                            assertNumber(compareValue, sourceCodeInfo, { finite: true });
                            return compareValue;
                        });
                    }
                    return result_1.join('');
                }
                var result = __spreadArray([], __read(seq), false);
                if (defaultComparer) {
                    result.sort(function (a, b) {
                        assertStringOrNumber(a, sourceCodeInfo);
                        assertStringOrNumber(b, sourceCodeInfo);
                        return compare(a, b, sourceCodeInfo);
                    });
                }
                else {
                    result.sort(function (a, b) {
                        assertLitsFunction(comparer, sourceCodeInfo);
                        var compareValue = executeFunction(comparer, [a, b], contextStack, sourceCodeInfo);
                        assertNumber(compareValue, sourceCodeInfo, { finite: true });
                        return compareValue;
                    });
                }
                return result;
            },
            paramCount: { min: 1, max: 2 },
        },
        'sort-by': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var _b = __read(params, 2), seq = _b[0], keyfn = _b[1];
                var defaultComparer = params.length === 2;
                assertSeq(seq, sourceCodeInfo);
                assertAny(keyfn, sourceCodeInfo);
                var comparer = defaultComparer ? null : params[2];
                if (typeof seq === 'string') {
                    var result_2 = seq.split('');
                    if (defaultComparer) {
                        result_2.sort(function (a, b) {
                            var aKey = executeFunction(keyfn, [a], contextStack, sourceCodeInfo);
                            assertStringOrNumber(aKey, sourceCodeInfo);
                            var bKey = executeFunction(keyfn, [b], contextStack, sourceCodeInfo);
                            assertStringOrNumber(bKey, sourceCodeInfo);
                            return compare(aKey, bKey, sourceCodeInfo);
                        });
                    }
                    else {
                        assertLitsFunction(comparer, sourceCodeInfo);
                        result_2.sort(function (a, b) {
                            var aKey = executeFunction(keyfn, [a], contextStack, sourceCodeInfo);
                            var bKey = executeFunction(keyfn, [b], contextStack, sourceCodeInfo);
                            var compareValue = executeFunction(comparer, [aKey, bKey], contextStack, sourceCodeInfo);
                            assertNumber(compareValue, sourceCodeInfo, { finite: true });
                            return compareValue;
                        });
                    }
                    return result_2.join('');
                }
                var result = __spreadArray([], __read(seq), false);
                if (defaultComparer) {
                    result.sort(function (a, b) {
                        var aKey = executeFunction(keyfn, [a], contextStack, sourceCodeInfo);
                        assertStringOrNumber(aKey, sourceCodeInfo);
                        var bKey = executeFunction(keyfn, [b], contextStack, sourceCodeInfo);
                        assertStringOrNumber(bKey, sourceCodeInfo);
                        return compare(aKey, bKey, sourceCodeInfo);
                    });
                }
                else {
                    assertLitsFunction(comparer, sourceCodeInfo);
                    result.sort(function (a, b) {
                        var aKey = executeFunction(keyfn, [a], contextStack, sourceCodeInfo);
                        var bKey = executeFunction(keyfn, [b], contextStack, sourceCodeInfo);
                        var compareValue = executeFunction(comparer, [aKey, bKey], contextStack, sourceCodeInfo);
                        assertNumber(compareValue, sourceCodeInfo, { finite: true });
                        return compareValue;
                    });
                }
                return result;
            },
            paramCount: { min: 2, max: 3 },
        },
        'take': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), input = _b[0], n = _b[1];
                assertNumber(n, sourceCodeInfo);
                assertSeq(input, sourceCodeInfo);
                var num = Math.max(Math.ceil(n), 0);
                return input.slice(0, num);
            },
            paramCount: 2,
        },
        'take-last': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), array = _b[0], n = _b[1];
                assertSeq(array, sourceCodeInfo);
                assertNumber(n, sourceCodeInfo);
                var num = Math.max(Math.ceil(n), 0);
                var from = array.length - num;
                return array.slice(from);
            },
            paramCount: 2,
        },
        'take-while': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_1, _c;
                var _d = __read(_a, 2), seq = _d[0], fn = _d[1];
                var executeFunction = _b.executeFunction;
                assertSeq(seq, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                var result = [];
                try {
                    for (var seq_1 = __values(seq), seq_1_1 = seq_1.next(); !seq_1_1.done; seq_1_1 = seq_1.next()) {
                        var item = seq_1_1.value;
                        if (executeFunction(fn, [item], contextStack, sourceCodeInfo))
                            result.push(item);
                        else
                            break;
                    }
                }
                catch (e_1_1) { e_1 = { error: e_1_1 }; }
                finally {
                    try {
                        if (seq_1_1 && !seq_1_1.done && (_c = seq_1.return)) _c.call(seq_1);
                    }
                    finally { if (e_1) throw e_1.error; }
                }
                return typeof seq === 'string' ? result.join('') : result;
            },
            paramCount: 2,
        },
        'drop': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), input = _b[0], n = _b[1];
                assertNumber(n, sourceCodeInfo);
                var num = Math.max(Math.ceil(n), 0);
                assertSeq(input, sourceCodeInfo);
                return input.slice(num);
            },
            paramCount: 2,
        },
        'drop-last': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), array = _b[0], n = _b[1];
                assertSeq(array, sourceCodeInfo);
                assertNumber(n, sourceCodeInfo);
                var num = Math.max(Math.ceil(n), 0);
                var from = array.length - num;
                return array.slice(0, from);
            },
            paramCount: 2,
        },
        'drop-while': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), seq = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertSeq(seq, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                if (Array.isArray(seq)) {
                    var from_1 = seq.findIndex(function (elem) { return !executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                    return seq.slice(from_1);
                }
                var charArray = seq.split('');
                var from = charArray.findIndex(function (elem) { return !executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                return charArray.slice(from).join('');
            },
            paramCount: 2,
        },
        'unshift': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), seq = _b[0], values = _b.slice(1);
                assertSeq(seq, sourceCodeInfo);
                if (typeof seq === 'string') {
                    assertCharArray(values, sourceCodeInfo);
                    return __spreadArray(__spreadArray([], __read(values), false), [seq], false).join('');
                }
                var copy = __spreadArray([], __read(seq), false);
                copy.unshift.apply(copy, __spreadArray([], __read(values), false));
                return copy;
            },
            paramCount: { min: 2 },
        },
        'distinct': {
            evaluate: function (_a, sourceCodeInfo) {
                var e_2, _b;
                var _c = __read(_a, 1), input = _c[0];
                assertSeq(input, sourceCodeInfo);
                if (Array.isArray(input)) {
                    var result = [];
                    var _loop_1 = function (item) {
                        assertAny(item, sourceCodeInfo);
                        if (!result.some(function (existingItem) { return deepEqual(existingItem, item, sourceCodeInfo); })) {
                            result.push(item);
                        }
                    };
                    try {
                        for (var input_1 = __values(input), input_1_1 = input_1.next(); !input_1_1.done; input_1_1 = input_1.next()) {
                            var item = input_1_1.value;
                            _loop_1(item);
                        }
                    }
                    catch (e_2_1) { e_2 = { error: e_2_1 }; }
                    finally {
                        try {
                            if (input_1_1 && !input_1_1.done && (_b = input_1.return)) _b.call(input_1);
                        }
                        finally { if (e_2) throw e_2.error; }
                    }
                    return result;
                }
                return Array.from(new Set(input.split(''))).join('');
            },
            paramCount: 1,
        },
        'remove': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), input = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, sourceCodeInfo);
                assertSeq(input, sourceCodeInfo);
                if (Array.isArray(input))
                    return input.filter(function (elem) { return !executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                return input
                    .split('')
                    .filter(function (elem) { return !executeFunction(fn, [elem], contextStack, sourceCodeInfo); })
                    .join('');
            },
            paramCount: 2,
        },
        'remove-at': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), input = _b[0], index = _b[1];
                assertNumber(index, sourceCodeInfo, { integer: true });
                assertSeq(input, sourceCodeInfo);
                var at = index < 0 ? input.length + index : index;
                if (at < 0 || at >= input.length)
                    return input;
                if (Array.isArray(input)) {
                    return input.filter(function (_, i) { return i !== at; });
                }
                return "".concat(input.substring(0, at)).concat(input.substring(at + 1));
            },
            paramCount: 2,
        },
        'split-at': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), seq = _b[0], pos = _b[1];
                assertNumber(pos, sourceCodeInfo, { integer: true });
                assertSeq(seq, sourceCodeInfo);
                var at = pos < 0 ? seq.length + pos : pos;
                return [seq.slice(0, at), seq.slice(at)];
            },
            paramCount: 2,
        },
        'split-with': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), seq = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, sourceCodeInfo);
                assertSeq(seq, sourceCodeInfo);
                var seqIsArray = Array.isArray(seq);
                var arr = seqIsArray ? seq : seq.split('');
                var index = arr.findIndex(function (elem) { return !executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                if (index === -1)
                    return [seq, seqIsArray ? [] : ''];
                return [seq.slice(0, index), seq.slice(index)];
            },
            paramCount: 2,
        },
        'frequencies': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), seq = _b[0];
                assertSeq(seq, sourceCodeInfo);
                var arr = typeof seq === 'string' ? seq.split('') : seq;
                return arr.reduce(function (result, val) {
                    assertString(val, sourceCodeInfo);
                    if (collHasKey(result, val))
                        result[val] = result[val] + 1;
                    else
                        result[val] = 1;
                    return result;
                }, {});
            },
            paramCount: 1,
        },
        'group-by': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), seq = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertAny(fn, sourceCodeInfo);
                assertSeq(seq, sourceCodeInfo);
                var arr = Array.isArray(seq) ? seq : seq.split('');
                return arr.reduce(function (result, val) {
                    var key = executeFunction(fn, [val], contextStack, sourceCodeInfo);
                    assertString(key, sourceCodeInfo);
                    if (!collHasKey(result, key))
                        result[key] = [];
                    result[key].push(val);
                    return result;
                }, {});
            },
            paramCount: 2,
        },
        'partition': {
            evaluate: function (params, sourceCodeInfo) {
                var seq = asSeq(params[0], sourceCodeInfo);
                var n = toNonNegativeInteger(asNumber(params[1], sourceCodeInfo));
                var step = params.length >= 3 ? toNonNegativeInteger(asNumber(params[2], sourceCodeInfo)) : n;
                var pad = params.length === 4
                    ? params[3] === null ? [] : asArray(params[3], sourceCodeInfo)
                    : undefined;
                return partition(n, step, seq, pad, sourceCodeInfo);
            },
            paramCount: { min: 2, max: 4 },
        },
        'partition-all': {
            evaluate: function (params, sourceCodeInfo) {
                var seq = asSeq(params[0], sourceCodeInfo);
                var n = toNonNegativeInteger(asNumber(params[1], sourceCodeInfo));
                var step = params.length === 3 ? toNonNegativeInteger(asNumber(params[2], sourceCodeInfo)) : n;
                return partition(n, step, seq, [], sourceCodeInfo);
            },
            paramCount: { min: 2, max: 3 },
        },
        'partition-by': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), seq = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, sourceCodeInfo);
                assertSeq(seq, sourceCodeInfo);
                var isStringSeq = typeof seq === 'string';
                var oldValue;
                var result = (isStringSeq ? seq.split('') : seq).reduce(function (acc, elem) {
                    var value = executeFunction(fn, [elem], contextStack, sourceCodeInfo);
                    if (value !== oldValue) {
                        acc.push([]);
                        oldValue = value;
                    }
                    acc[acc.length - 1].push(elem);
                    return acc;
                }, []);
                return isStringSeq ? result.map(function (elem) { return elem.join(''); }) : result;
            },
            paramCount: 2,
        },
        'ends-with?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), str = _b[0], search = _b[1];
                assertSeq(str, sourceCodeInfo);
                if (typeof str === 'string') {
                    assertString(search, sourceCodeInfo);
                    return str.endsWith(search);
                }
                return deepEqual(asAny(str.at(-1), sourceCodeInfo), asAny(search, sourceCodeInfo), sourceCodeInfo);
            },
            paramCount: 2,
        },
        'starts-with?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), seq = _b[0], search = _b[1];
                assertSeq(seq, sourceCodeInfo);
                if (typeof seq === 'string') {
                    assertString(search, sourceCodeInfo);
                    return seq.startsWith(search);
                }
                return deepEqual(asAny(seq[0], sourceCodeInfo), asAny(search, sourceCodeInfo), sourceCodeInfo);
            },
            paramCount: 2,
        },
        'interleave': {
            evaluate: function (_a, sourceCodeInfo) {
                var e_3, _b;
                var _c = __read(_a), seqs = _c.slice(0);
                var isStringSeq = typeof seqs[0] === 'string';
                var seqsArr = isStringSeq
                    ? seqs.map(function (seq) {
                        assertString(seq, sourceCodeInfo);
                        if (typeof seq === 'string')
                            return seq.split('');
                        return seq;
                    })
                    : seqs.map(function (seq) {
                        assertArray(seq, sourceCodeInfo);
                        return seq;
                    });
                var maxLength = Math.min.apply(Math, __spreadArray([], __read(seqsArr.map(function (seq) { return seq.length; })), false));
                var result = [];
                for (var i = 0; i < maxLength; i += 1) {
                    try {
                        for (var seqsArr_1 = (e_3 = void 0, __values(seqsArr)), seqsArr_1_1 = seqsArr_1.next(); !seqsArr_1_1.done; seqsArr_1_1 = seqsArr_1.next()) {
                            var seq = seqsArr_1_1.value;
                            if (i < seq.length)
                                result.push(seq[i]);
                        }
                    }
                    catch (e_3_1) { e_3 = { error: e_3_1 }; }
                    finally {
                        try {
                            if (seqsArr_1_1 && !seqsArr_1_1.done && (_b = seqsArr_1.return)) _b.call(seqsArr_1);
                        }
                        finally { if (e_3) throw e_3.error; }
                    }
                }
                return isStringSeq ? result.join('') : result;
            },
            paramCount: { min: 1 },
        },
        'interpose': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), seq = _b[0], separator = _b[1];
                assertSeq(seq, sourceCodeInfo);
                if (typeof seq === 'string') {
                    assertString(separator, sourceCodeInfo);
                    return seq.split('').join(separator);
                }
                if (seq.length === 0)
                    return [];
                var result = [];
                for (var i = 0; i < seq.length - 1; i += 1) {
                    result.push(seq[i], separator);
                }
                result.push(seq[seq.length - 1]);
                return result;
            },
            paramCount: 2,
        },
    };
    function partition(n, step, seq, pad, sourceCodeInfo) {
        assertNumber(step, sourceCodeInfo, { positive: true });
        var isStringSeq = typeof seq === 'string';
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
                    if (padIndex >= pad.length)
                        break;
                    innerArr.push(pad[padIndex]);
                }
                else {
                    innerArr.push(seq[i]);
                }
            }
            result.push(innerArr);
            start += step;
        }
        return isStringSeq ? result.map(function (x) { return x.join(''); }) : result;
    }

    var mathNormalExpression = {
        'inc': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo);
                return first + 1;
            },
            paramCount: 1,
        },
        'dec': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo);
                return first - 1;
            },
            paramCount: 1,
        },
        '+': {
            evaluate: function (params, sourceCodeInfo) {
                return params.reduce(function (result, param) {
                    assertNumber(param, sourceCodeInfo);
                    return result + param;
                }, 0);
            },
            paramCount: {},
        },
        '*': {
            evaluate: function (params, sourceCodeInfo) {
                return params.reduce(function (result, param) {
                    assertNumber(param, sourceCodeInfo);
                    return result * param;
                }, 1);
            },
            paramCount: {},
        },
        '/': {
            evaluate: function (params, sourceCodeInfo) {
                if (params.length === 0)
                    return 1;
                var _a = __read(params), first = _a[0], rest = _a.slice(1);
                assertNumber(first, sourceCodeInfo);
                if (rest.length === 0) {
                    assertNumber(first, sourceCodeInfo);
                    return 1 / first;
                }
                return rest.reduce(function (result, param) {
                    assertNumber(param, sourceCodeInfo);
                    return result / param;
                }, first);
            },
            paramCount: {},
        },
        '-': {
            evaluate: function (params, sourceCodeInfo) {
                if (params.length === 0)
                    return 0;
                var _a = __read(params), first = _a[0], rest = _a.slice(1);
                assertNumber(first, sourceCodeInfo);
                if (rest.length === 0)
                    return -first;
                return rest.reduce(function (result, param) {
                    assertNumber(param, sourceCodeInfo);
                    return result - param;
                }, first);
            },
            paramCount: {},
        },
        'quot': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), dividend = _b[0], divisor = _b[1];
                assertNumber(dividend, sourceCodeInfo);
                assertNumber(divisor, sourceCodeInfo);
                var quotient = Math.trunc(dividend / divisor);
                return quotient;
            },
            paramCount: 2,
        },
        'mod': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), dividend = _b[0], divisor = _b[1];
                assertNumber(dividend, sourceCodeInfo);
                assertNumber(divisor, sourceCodeInfo);
                var quotient = Math.floor(dividend / divisor);
                return dividend - divisor * quotient;
            },
            paramCount: 2,
        },
        '%': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), dividend = _b[0], divisor = _b[1];
                assertNumber(dividend, sourceCodeInfo);
                assertNumber(divisor, sourceCodeInfo);
                var quotient = Math.trunc(dividend / divisor);
                return dividend - divisor * quotient;
            },
            paramCount: 2,
        },
        '√': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo);
                return Math.sqrt(first);
            },
            paramCount: 1,
            aliases: ['sqrt'],
        },
        '∛': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo);
                return Math.cbrt(first);
            },
            paramCount: 1,
            aliases: ['cbrt'],
        },
        '**': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], second = _b[1];
                assertNumber(first, sourceCodeInfo);
                assertNumber(second, sourceCodeInfo);
                return Math.pow(first, second);
            },
            paramCount: 2,
        },
        'round': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(params, 2), value = _a[0], decimals = _a[1];
                assertNumber(value, sourceCodeInfo);
                if (params.length === 1 || decimals === 0)
                    return Math.round(value);
                assertNumber(decimals, sourceCodeInfo, { integer: true, nonNegative: true });
                var factor = Math.pow(10, decimals);
                return Math.round(value * factor) / factor;
            },
            paramCount: { min: 1, max: 2 },
        },
        'trunc': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo);
                return Math.trunc(first);
            },
            paramCount: 1,
        },
        'floor': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo);
                return Math.floor(first);
            },
            paramCount: 1,
        },
        'ceil': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo);
                return Math.ceil(first);
            },
            paramCount: 1,
        },
        'min': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), first = _b[0], rest = _b.slice(1);
                assertNumber(first, sourceCodeInfo);
                if (rest.length === 0)
                    return first;
                return rest.reduce(function (min, value) {
                    assertNumber(value, sourceCodeInfo);
                    return Math.min(min, value);
                }, first);
            },
            paramCount: { min: 1 },
        },
        'max': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), first = _b[0], rest = _b.slice(1);
                assertNumber(first, sourceCodeInfo);
                if (rest.length === 0)
                    return first;
                return rest.reduce(function (min, value) {
                    assertNumber(value, sourceCodeInfo);
                    return Math.max(min, value);
                }, first);
            },
            paramCount: { min: 1 },
        },
        'abs': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.abs(value);
            },
            paramCount: 1,
        },
        'sign': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.sign(value);
            },
            paramCount: 1,
        },
        'log': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.log(value);
            },
            paramCount: 1,
        },
        'log2': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.log2(value);
            },
            paramCount: 1,
        },
        'log10': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.log10(value);
            },
            paramCount: 1,
        },
        'sin': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.sin(value);
            },
            paramCount: 1,
        },
        'asin': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.asin(value);
            },
            paramCount: 1,
        },
        'sinh': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.sinh(value);
            },
            paramCount: 1,
        },
        'asinh': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.asinh(value);
            },
            paramCount: 1,
        },
        'cos': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.cos(value);
            },
            paramCount: 1,
        },
        'acos': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.acos(value);
            },
            paramCount: 1,
        },
        'cosh': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.cosh(value);
            },
            paramCount: 1,
        },
        'acosh': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.acosh(value);
            },
            paramCount: 1,
        },
        'tan': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.tan(value);
            },
            paramCount: 1,
        },
        'atan': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.atan(value);
            },
            paramCount: 1,
        },
        'tanh': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.tanh(value);
            },
            paramCount: 1,
        },
        'atanh': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.atanh(value);
            },
            paramCount: 1,
        },
    };

    function isEqual(_a, sourceCodeInfo) {
        var e_1, _b;
        var _c = __read(_a), first = _c[0], rest = _c.slice(1);
        var firstAny = asAny(first, sourceCodeInfo);
        try {
            for (var rest_1 = __values(rest), rest_1_1 = rest_1.next(); !rest_1_1.done; rest_1_1 = rest_1.next()) {
                var param = rest_1_1.value;
                if (!deepEqual(firstAny, asAny(param, sourceCodeInfo), sourceCodeInfo))
                    return false;
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
    }
    function isIdentical(_a) {
        var e_2, _b;
        var _c = __read(_a), first = _c[0], rest = _c.slice(1);
        try {
            for (var rest_2 = __values(rest), rest_2_1 = rest_2.next(); !rest_2_1.done; rest_2_1 = rest_2.next()) {
                var param = rest_2_1.value;
                if (param !== first)
                    return false;
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
    }
    var miscNormalExpression = {
        '=': {
            evaluate: function (params, sourceCodeInfo) {
                return isEqual(params, sourceCodeInfo);
            },
            paramCount: { min: 1 },
        },
        '≠': {
            evaluate: function (params, sourceCodeInfo) {
                return !isEqual(params, sourceCodeInfo);
            },
            paramCount: { min: 1 },
            aliases: ['!='],
        },
        'identical?': {
            evaluate: function (params) {
                return isIdentical(params);
            },
            paramCount: { min: 1 },
        },
        '>': {
            evaluate: function (_a, sourceCodeInfo) {
                var e_3, _b;
                var _c = __read(_a), first = _c[0], rest = _c.slice(1);
                var currentValue = asStringOrNumber(first);
                try {
                    for (var rest_3 = __values(rest), rest_3_1 = rest_3.next(); !rest_3_1.done; rest_3_1 = rest_3.next()) {
                        var param = rest_3_1.value;
                        if (compare(currentValue, asStringOrNumber(param), sourceCodeInfo) <= 0)
                            return false;
                        currentValue = asStringOrNumber(param);
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
            paramCount: { min: 1 },
        },
        '<': {
            evaluate: function (_a, sourceCodeInfo) {
                var e_4, _b;
                var _c = __read(_a), first = _c[0], rest = _c.slice(1);
                var currentValue = asStringOrNumber(first);
                try {
                    for (var rest_4 = __values(rest), rest_4_1 = rest_4.next(); !rest_4_1.done; rest_4_1 = rest_4.next()) {
                        var param = rest_4_1.value;
                        if (compare(currentValue, asStringOrNumber(param), sourceCodeInfo) >= 0)
                            return false;
                        currentValue = asStringOrNumber(param);
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
            paramCount: { min: 1 },
        },
        '≥': {
            evaluate: function (_a, sourceCodeInfo) {
                var e_5, _b;
                var _c = __read(_a), first = _c[0], rest = _c.slice(1);
                var currentValue = asStringOrNumber(first);
                try {
                    for (var rest_5 = __values(rest), rest_5_1 = rest_5.next(); !rest_5_1.done; rest_5_1 = rest_5.next()) {
                        var param = rest_5_1.value;
                        if (compare(currentValue, asStringOrNumber(param), sourceCodeInfo) < 0)
                            return false;
                        currentValue = asStringOrNumber(param);
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
            paramCount: { min: 1 },
            aliases: ['>='],
        },
        '≤': {
            evaluate: function (_a, sourceCodeInfo) {
                var e_6, _b;
                var _c = __read(_a), first = _c[0], rest = _c.slice(1);
                var currentValue = asStringOrNumber(first);
                try {
                    for (var rest_6 = __values(rest), rest_6_1 = rest_6.next(); !rest_6_1.done; rest_6_1 = rest_6.next()) {
                        var param = rest_6_1.value;
                        if (compare(currentValue, asStringOrNumber(param), sourceCodeInfo) > 0)
                            return false;
                        currentValue = asStringOrNumber(param);
                    }
                }
                catch (e_6_1) { e_6 = { error: e_6_1 }; }
                finally {
                    try {
                        if (rest_6_1 && !rest_6_1.done && (_b = rest_6.return)) _b.call(rest_6);
                    }
                    finally { if (e_6) throw e_6.error; }
                }
                return true;
            },
            paramCount: { min: 1 },
            aliases: ['<='],
        },
        '!': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return !first;
            },
            paramCount: 1,
        },
        'epoch->iso-date': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), ms = _b[0];
                assertNumber(ms, sourceCodeInfo);
                return new Date(ms).toISOString();
            },
            paramCount: 1,
        },
        'iso-date->epoch': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), dateTime = _b[0];
                assertString(dateTime, sourceCodeInfo);
                var ms = new Date(dateTime).valueOf();
                assertNumber(ms, sourceCodeInfo, { finite: true });
                return ms;
            },
            paramCount: 1,
        },
        'write!': {
            evaluate: function (params, sourceCodeInfo) {
                // eslint-disable-next-line no-console
                console.log.apply(console, __spreadArray([], __read(params), false));
                if (params.length > 0)
                    return asAny(params[params.length - 1], sourceCodeInfo);
                return null;
            },
            paramCount: {},
        },
        'boolean': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), value = _b[0];
                return !!value;
            },
            paramCount: 1,
        },
        'compare': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), a = _b[0], b = _b[1];
                assertStringOrNumber(a, sourceCodeInfo);
                assertStringOrNumber(b, sourceCodeInfo);
                return compare(a, b, sourceCodeInfo);
            },
            paramCount: 2,
        },
        'json-parse': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertString(first, sourceCodeInfo);
                // eslint-disable-next-line ts/no-unsafe-return
                return JSON.parse(first);
            },
            paramCount: 1,
        },
        'json-stringify': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], second = _b[1];
                assertAny(first, sourceCodeInfo);
                if (second === undefined)
                    return JSON.stringify(first);
                assertNumber(second, sourceCodeInfo);
                return JSON.stringify(first, null, second);
            },
            paramCount: { min: 1, max: 2 },
        },
    };

    var assertNormalExpression = {
        'assert': {
            evaluate: function (params, sourceCodeInfo) {
                var value = params[0];
                var message = params.length === 2 ? params[1] : "".concat(value);
                assertString(message, sourceCodeInfo);
                if (!value)
                    throw new AssertionError(message, sourceCodeInfo);
                return asAny(value, sourceCodeInfo);
            },
            paramCount: { min: 1, max: 2 },
        },
        'assert=': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (!deepEqual(asAny(first, sourceCodeInfo), asAny(second, sourceCodeInfo), sourceCodeInfo)) {
                    throw new AssertionError("Expected ".concat(JSON.stringify(first, null, 2), " to deep equal ").concat(JSON.stringify(second, null, 2), ".").concat(message), sourceCodeInfo);
                }
                return null;
            },
            paramCount: { min: 2, max: 3 },
        },
        'assert!=': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (deepEqual(asAny(first, sourceCodeInfo), asAny(second, sourceCodeInfo), sourceCodeInfo)) {
                    throw new AssertionError("Expected ".concat(JSON.stringify(first), " not to deep equal ").concat(JSON.stringify(second), ".").concat(message), sourceCodeInfo);
                }
                return null;
            },
            paramCount: { min: 2, max: 3 },
        },
        'assert-gt': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                assertStringOrNumber(first, sourceCodeInfo);
                assertStringOrNumber(second, sourceCodeInfo);
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (compare(first, second, sourceCodeInfo) <= 0)
                    throw new AssertionError("Expected ".concat(first, " to be grater than ").concat(second, ".").concat(message), sourceCodeInfo);
                return null;
            },
            paramCount: { min: 2, max: 3 },
        },
        'assert-gte': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                assertStringOrNumber(first, sourceCodeInfo);
                assertStringOrNumber(second, sourceCodeInfo);
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (compare(first, second, sourceCodeInfo) < 0)
                    throw new AssertionError("Expected ".concat(first, " to be grater than or equal to ").concat(second, ".").concat(message), sourceCodeInfo);
                return null;
            },
            paramCount: { min: 2, max: 3 },
        },
        'assert-lt': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                assertStringOrNumber(first, sourceCodeInfo);
                assertStringOrNumber(second, sourceCodeInfo);
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (compare(first, second, sourceCodeInfo) >= 0)
                    throw new AssertionError("Expected ".concat(first, " to be less than ").concat(second, ".").concat(message), sourceCodeInfo);
                return null;
            },
            paramCount: { min: 2, max: 3 },
        },
        'assert-lte': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                assertStringOrNumber(first, sourceCodeInfo);
                assertStringOrNumber(second, sourceCodeInfo);
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (compare(first, second, sourceCodeInfo) > 0)
                    throw new AssertionError("Expected ".concat(first, " to be less than or equal to ").concat(second, ".").concat(message), sourceCodeInfo);
                return null;
            },
            paramCount: { min: 2, max: 3 },
        },
        'assert-true': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (first !== true)
                    throw new AssertionError("Expected ".concat(first, " to be true.").concat(message), sourceCodeInfo);
                return null;
            },
            paramCount: { min: 1, max: 2 },
        },
        'assert-false': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (first !== false)
                    throw new AssertionError("Expected ".concat(first, " to be false.").concat(message), sourceCodeInfo);
                return null;
            },
            paramCount: { min: 1, max: 2 },
        },
        'assert-truthy': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (!first)
                    throw new AssertionError("Expected ".concat(first, " to be truthy.").concat(message), sourceCodeInfo);
                return null;
            },
            paramCount: { min: 1, max: 2 },
        },
        'assert-falsy': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (first)
                    throw new AssertionError("Expected ".concat(first, " to be falsy.").concat(message), sourceCodeInfo);
                return null;
            },
            paramCount: { min: 1, max: 2 },
        },
        'assert-null': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (first !== null)
                    throw new AssertionError("Expected ".concat(first, " to be null.").concat(message), sourceCodeInfo);
                return null;
            },
            paramCount: { min: 1, max: 2 },
        },
        'assert-throws': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), func = _c[0], message = _c[1];
                var executeFunction = _b.executeFunction;
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                assertLitsFunction(func, sourceCodeInfo);
                try {
                    executeFunction(func, [], contextStack, sourceCodeInfo);
                }
                catch (_d) {
                    return null;
                }
                throw new AssertionError("Expected function to throw.".concat(message), sourceCodeInfo);
            },
            paramCount: { min: 1, max: 2 },
        },
        'assert-throws-error': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 3), func = _c[0], throwMessage = _c[1], message = _c[2];
                var executeFunction = _b.executeFunction;
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                assertString(throwMessage, sourceCodeInfo);
                assertLitsFunction(func, sourceCodeInfo);
                try {
                    executeFunction(func, [], contextStack, sourceCodeInfo);
                }
                catch (error) {
                    var errorMessage = error.shortMessage;
                    if (errorMessage !== throwMessage) {
                        throw new AssertionError("Expected function to throw \"".concat(throwMessage, "\", but thrown \"").concat(errorMessage, "\".").concat(message), sourceCodeInfo);
                    }
                    return null;
                }
                throw new AssertionError("Expected function to throw \"".concat(throwMessage, "\".").concat(message), sourceCodeInfo);
            },
            paramCount: { min: 2, max: 3 },
        },
        'assert-not-throws': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), func = _c[0], message = _c[1];
                var executeFunction = _b.executeFunction;
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                assertLitsFunction(func, sourceCodeInfo);
                try {
                    executeFunction(func, [], contextStack, sourceCodeInfo);
                }
                catch (_d) {
                    throw new AssertionError("Expected function not to throw.".concat(message), sourceCodeInfo);
                }
                return null;
            },
            paramCount: { min: 1, max: 2 },
        },
    };

    var objectNormalExpression = {
        'object': {
            evaluate: function (params, sourceCodeInfo) {
                var result = {};
                for (var i = 0; i < params.length; i += 2) {
                    var key = params[i];
                    var value = params[i + 1];
                    assertString(key, sourceCodeInfo);
                    result[key] = value;
                }
                return result;
            },
            paramCount: { even: true },
        },
        'keys': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), obj = _b[0];
                assertObj(obj, sourceCodeInfo);
                return Object.keys(obj);
            },
            paramCount: 1,
        },
        'vals': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), obj = _b[0];
                assertObj(obj, sourceCodeInfo);
                return Object.values(obj);
            },
            paramCount: 1,
        },
        'entries': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), obj = _b[0];
                assertObj(obj, sourceCodeInfo);
                return Object.entries(obj);
            },
            paramCount: 1,
        },
        'find': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), obj = _b[0], key = _b[1];
                assertObj(obj, sourceCodeInfo);
                assertString(key, sourceCodeInfo);
                if (collHasKey(obj, key))
                    return [key, obj[key]];
                return null;
            },
            paramCount: 2,
        },
        'dissoc': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), obj = _b[0], key = _b[1];
                assertObj(obj, sourceCodeInfo);
                assertString(key, sourceCodeInfo);
                var newObj = __assign({}, obj);
                delete newObj[key];
                return newObj;
            },
            paramCount: 2,
        },
        'merge': {
            evaluate: function (params, sourceCodeInfo) {
                if (params.length === 0)
                    return null;
                var _a = __read(params), first = _a[0], rest = _a.slice(1);
                assertObj(first, sourceCodeInfo);
                return rest.reduce(function (result, obj) {
                    assertObj(obj, sourceCodeInfo);
                    return __assign(__assign({}, result), obj);
                }, __assign({}, first));
            },
            paramCount: { min: 0 },
        },
        'merge-with': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var first = params[0];
                var fn = params.at(-1);
                var rest = params.slice(1, -1);
                assertObj(first, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                return rest.reduce(function (result, obj) {
                    assertObj(obj, sourceCodeInfo);
                    Object.entries(obj).forEach(function (entry) {
                        var key = asString(entry[0], sourceCodeInfo);
                        var val = toAny(entry[1]);
                        if (collHasKey(result, key))
                            result[key] = executeFunction(fn, [result[key], val], contextStack, sourceCodeInfo);
                        else
                            result[key] = val;
                    });
                    return result;
                }, __assign({}, first));
            },
            paramCount: { min: 2 },
        },
        'zipmap': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), keys = _b[0], values = _b[1];
                assertStringArray(keys, sourceCodeInfo);
                assertArray(values, sourceCodeInfo);
                var length = Math.min(keys.length, values.length);
                var result = {};
                for (var i = 0; i < length; i += 1) {
                    var key = asString(keys[i], sourceCodeInfo);
                    result[key] = toAny(values[i]);
                }
                return result;
            },
            paramCount: 2,
        },
        'select-keys': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), obj = _b[0], keys = _b[1];
                assertStringArray(keys, sourceCodeInfo);
                assertObj(obj, sourceCodeInfo);
                return keys.reduce(function (result, key) {
                    if (collHasKey(obj, key))
                        result[key] = toAny(obj[key]);
                    return result;
                }, {});
            },
            paramCount: 2,
        },
    };

    var predicatesNormalExpression = {
        'function?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return isLitsFunction(first);
            },
            paramCount: 1,
        },
        'string?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return typeof first === 'string';
            },
            paramCount: 1,
        },
        'number?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return typeof first === 'number';
            },
            paramCount: 1,
        },
        'integer?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return typeof first === 'number' && isNumber(first, { integer: true });
            },
            paramCount: 1,
        },
        'boolean?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return typeof first === 'boolean';
            },
            paramCount: 1,
        },
        'null?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return first === null || first === undefined;
            },
            paramCount: 1,
        },
        'zero?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo, { finite: true });
                return first === 0;
            },
            paramCount: 1,
        },
        'pos?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo, { finite: true });
                return first > 0;
            },
            paramCount: 1,
        },
        'neg?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo, { finite: true });
                return first < 0;
            },
            paramCount: 1,
        },
        'even?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo, { finite: true });
                return first % 2 === 0;
            },
            paramCount: 1,
        },
        'odd?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo, { finite: true });
                return isNumber(first, { integer: true }) && first % 2 !== 0;
            },
            paramCount: 1,
        },
        'array?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return Array.isArray(first);
            },
            paramCount: 1,
        },
        'coll?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return isColl(first);
            },
            paramCount: 1,
        },
        'seq?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return isSeq(first);
            },
            paramCount: 1,
        },
        'object?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return isObj(first);
            },
            paramCount: 1,
        },
        'regexp?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), value = _b[0];
                return isRegularExpression(value);
            },
            paramCount: 1,
        },
        'finite?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Number.isFinite(value);
            },
            paramCount: 1,
        },
        'nan?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Number.isNaN(value);
            },
            paramCount: 1,
        },
        'positive-infinity?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return value === Number.POSITIVE_INFINITY;
            },
            paramCount: 1,
        },
        'negative-infinity?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return value === Number.NEGATIVE_INFINITY;
            },
            paramCount: 1,
        },
        'true?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), value = _b[0];
                return value === true;
            },
            paramCount: 1,
        },
        'false?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), value = _b[0];
                return value === false;
            },
            paramCount: 1,
        },
        'empty?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), coll = _b[0];
                if (coll === null)
                    return true;
                assertColl(coll, sourceCodeInfo);
                if (typeof coll === 'string')
                    return coll.length === 0;
                if (Array.isArray(coll))
                    return coll.length === 0;
                return Object.keys(coll).length === 0;
            },
            paramCount: 1,
        },
        'not-empty?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), coll = _b[0];
                if (coll === null)
                    return false;
                assertColl(coll, sourceCodeInfo);
                if (typeof coll === 'string')
                    return coll.length > 0;
                if (Array.isArray(coll))
                    return coll.length > 0;
                return Object.keys(coll).length > 0;
            },
            paramCount: 1,
        },
    };

    var regexpNormalExpression = {
        'regexp': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b;
                var _c = __read(_a, 2), sourceArg = _c[0], flagsArg = _c[1];
                assertString(sourceArg, sourceCodeInfo);
                var source = sourceArg || '(?:)';
                var flags = typeof flagsArg === 'string' ? flagsArg : '';
                return _b = {},
                    _b[REGEXP_SYMBOL] = true,
                    _b.sourceCodeInfo = sourceCodeInfo,
                    _b.s = source,
                    _b.f = flags,
                    _b;
            },
            paramCount: { min: 1, max: 2 },
        },
        'match': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), text = _b[0], regexp = _b[1];
                assertRegularExpression(regexp, sourceCodeInfo);
                if (!isString(text))
                    return null;
                var regExp = new RegExp(regexp.s, regexp.f);
                var match = regExp.exec(text);
                if (match)
                    return __spreadArray([], __read(match), false);
                return null;
            },
            paramCount: 2,
        },
        'replace': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), str = _b[0], regexp = _b[1], value = _b[2];
                assertString(str, sourceCodeInfo);
                assertStringOrRegularExpression(regexp, sourceCodeInfo);
                assertString(value, sourceCodeInfo);
                var matcher = isRegularExpression(regexp) ? new RegExp(regexp.s, "".concat(regexp.f)) : regexp;
                return str.replace(matcher, value);
            },
            paramCount: 3,
        },
        'replace-all': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), str = _b[0], regexp = _b[1], value = _b[2];
                assertString(str, sourceCodeInfo);
                assertStringOrRegularExpression(regexp, sourceCodeInfo);
                assertString(value, sourceCodeInfo);
                var matcher = isRegularExpression(regexp) ? new RegExp(regexp.s, "".concat(regexp.f.includes('g') ? regexp.f : "".concat(regexp.f, "g"))) : regexp;
                return str.replaceAll(matcher, value);
            },
            paramCount: 3,
        },
    };

    var blankRegexp = /^\s*$/;
    var stringNormalExpression = {
        'string-repeat': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), str = _b[0], count = _b[1];
                assertString(str, sourceCodeInfo);
                assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true });
                return str.repeat(count);
            },
            paramCount: 2,
        },
        'str': {
            evaluate: function (params) {
                return params.reduce(function (result, param) {
                    var paramStr = param === undefined || param === null
                        ? ''
                        : isObj(param)
                            ? JSON.stringify(param)
                            : Array.isArray(param)
                                ? JSON.stringify(param)
                                : "".concat(param);
                    return result + paramStr;
                }, '');
            },
            paramCount: {},
        },
        'number': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                var number = Number(str);
                if (Number.isNaN(number))
                    throw new LitsError("Could not convert '".concat(str, "' to a number."), sourceCodeInfo);
                return number;
            },
            paramCount: 1,
        },
        'from-char-code': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), num = _b[0];
                assertNumber(num, sourceCodeInfo, { finite: true });
                var int = toNonNegativeInteger(num);
                try {
                    return String.fromCodePoint(int);
                }
                catch (error) {
                    throw new LitsError(error, sourceCodeInfo);
                }
            },
            paramCount: 1,
        },
        'to-char-code': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo, { nonEmpty: true });
                return asNonUndefined(str.codePointAt(0), sourceCodeInfo);
            },
            paramCount: 1,
        },
        'lower-case': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.toLowerCase();
            },
            paramCount: 1,
        },
        'upper-case': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.toUpperCase();
            },
            paramCount: 1,
        },
        'trim': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.trim();
            },
            paramCount: 1,
        },
        'trim-left': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.replace(/^\s+/, '');
            },
            paramCount: 1,
        },
        'trim-right': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.replace(/\s+$/, '');
            },
            paramCount: 1,
        },
        'join': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), stringList = _b[0], delimiter = _b[1];
                assertArray(stringList, sourceCodeInfo);
                stringList.forEach(function (str) { return assertStringOrNumber(str, sourceCodeInfo); });
                assertString(delimiter, sourceCodeInfo);
                return stringList.join(delimiter);
            },
            paramCount: 2,
        },
        'split': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), str = _b[0], stringOrRegExpValue = _b[1], limit = _b[2];
                assertString(str, sourceCodeInfo);
                assertStringOrRegularExpression(stringOrRegExpValue, sourceCodeInfo);
                if (limit !== undefined)
                    assertNumber(limit, sourceCodeInfo, { integer: true, nonNegative: true });
                var delimiter = typeof stringOrRegExpValue === 'string'
                    ? stringOrRegExpValue
                    : new RegExp(stringOrRegExpValue.s, stringOrRegExpValue.f);
                return str.split(delimiter, limit);
            },
            paramCount: { min: 2, max: 3 },
        },
        'split-lines': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.split((/\r\n|\n|\r/)).filter(function (line) { return line !== ''; });
            },
            paramCount: 1,
        },
        'pad-left': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), str = _b[0], length = _b[1], padString = _b[2];
                assertString(str, sourceCodeInfo);
                assertNumber(length, sourceCodeInfo, { integer: true });
                if (padString !== undefined)
                    assertString(padString, sourceCodeInfo);
                return str.padStart(length, padString);
            },
            paramCount: { min: 2, max: 3 },
        },
        'pad-right': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), str = _b[0], length = _b[1], padString = _b[2];
                assertString(str, sourceCodeInfo);
                assertNumber(length, sourceCodeInfo, { integer: true });
                if (padString !== undefined)
                    assertString(padString, sourceCodeInfo);
                return str.padEnd(length, padString);
            },
            paramCount: { min: 2, max: 3 },
        },
        'template': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), templateString = _b[0], placeholders = _b.slice(1);
                assertString(templateString, sourceCodeInfo);
                assertArray(placeholders, sourceCodeInfo);
                var templateStrings = templateString.split('||||');
                if (templateStrings.length <= 1) {
                    return applyPlaceholders(templateStrings[0], placeholders, sourceCodeInfo);
                }
                else {
                    // Pluralisation
                    var count = placeholders[0];
                    assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true });
                    var stringPlaceholders = __spreadArray(["".concat(count)], __read(placeholders.slice(1)), false);
                    if (templateStrings.length === 2) {
                        // Exactly two valiants.
                        // First variant (singular) for count = 1, Second variant (plural) for count = 0 or count > 1
                        var placehoder = templateStrings[count === 1 ? 0 : 1];
                        return applyPlaceholders(placehoder, stringPlaceholders, sourceCodeInfo);
                    }
                    else {
                        // More than two variant:
                        // Use count as index
                        // If count >= number of variants, use last variant
                        var placehoder = templateStrings[Math.min(count, templateStrings.length - 1)];
                        return applyPlaceholders(placehoder, stringPlaceholders, sourceCodeInfo);
                    }
                }
            },
            paramCount: { min: 1, max: 10 },
        },
        'encode-base64': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertString(value, sourceCodeInfo);
                return btoa(encodeURIComponent(value).replace(/%([0-9A-F]{2})/g, function (_match, p1) {
                    // eslint-disable-next-line ts/no-unsafe-argument
                    return String.fromCharCode(Number.parseInt(p1, 16));
                }));
            },
            paramCount: 1,
        },
        'decode-base64': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertString(value, sourceCodeInfo);
                try {
                    return decodeURIComponent(Array.prototype.map
                        .call(atob(value), function (c) {
                        // eslint-disable-next-line ts/no-unsafe-call, ts/no-unsafe-member-access
                        return "%".concat(("00".concat(c.charCodeAt(0).toString(16))).slice(-2));
                    })
                        .join(''));
                }
                catch (error) {
                    throw new LitsError(error, sourceCodeInfo);
                }
            },
            paramCount: 1,
        },
        'encode-uri-component': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertString(value, sourceCodeInfo);
                return encodeURIComponent(value);
            },
            paramCount: 1,
        },
        'decode-uri-component': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertString(value, sourceCodeInfo);
                try {
                    return decodeURIComponent(value);
                }
                catch (error) {
                    throw new LitsError(error, sourceCodeInfo);
                }
            },
            paramCount: 1,
        },
        'blank?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                if (value === null) {
                    return true;
                }
                assertString(value, sourceCodeInfo);
                return blankRegexp.test(value);
            },
            paramCount: 1,
        },
        'capitalize': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.charAt(0).toUpperCase() + str.slice(1).toLowerCase();
            },
            paramCount: 1,
        },
    };
    var doubleDollarRegexp = /\$\$/g;
    function applyPlaceholders(templateString, placeholders, sourceCodeInfo) {
        for (var i = 0; i < 9; i += 1) {
            // Matches $1, $2, ..., $9
            // Does not match $$1
            // But does match $$$1, (since the two first '$' will later be raplaced with a single '$'
            var re = new RegExp("(\\$\\$|[^$]|^)\\$".concat(i + 1), 'g');
            if (re.test(templateString)) {
                var placeHolder = asStringOrNumber(placeholders[i], sourceCodeInfo);
                templateString = templateString.replace(re, "$1".concat(placeHolder));
            }
        }
        templateString = templateString.replace(doubleDollarRegexp, '$');
        return templateString;
    }

    var functionalNormalExpression = {
        'apply': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a), func = _c[0], params = _c.slice(1);
                var executeFunction = _b.executeFunction;
                assertLitsFunction(func, sourceCodeInfo);
                var paramsLength = params.length;
                var last = params[paramsLength - 1];
                assertArray(last, sourceCodeInfo);
                var applyArray = __spreadArray(__spreadArray([], __read(params.slice(0, -1)), false), __read(last), false);
                return executeFunction(func, applyArray, contextStack, sourceCodeInfo);
            },
            paramCount: { min: 2 },
        },
        'identity': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), value = _b[0];
                return toAny(value);
            },
            paramCount: 1,
        },
        'partial': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b;
                var _c = __read(_a), fn = _c[0], params = _c.slice(1);
                return _b = {},
                    _b[FUNCTION_SYMBOL] = true,
                    _b.sourceCodeInfo = sourceCodeInfo,
                    _b.t = FunctionType.Partial,
                    _b.f = toAny(fn),
                    _b.p = params,
                    _b;
            },
            paramCount: { min: 1 },
        },
        'comp': {
            evaluate: function (fns, sourceCodeInfo) {
                var _a;
                if (fns.length > 1) {
                    var last = fns[fns.length - 1];
                    if (Array.isArray(last))
                        // eslint-disable-next-line ts/no-unsafe-assignment
                        fns = __spreadArray(__spreadArray([], __read(fns.slice(0, -1)), false), __read(last), false);
                }
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.sourceCodeInfo = sourceCodeInfo,
                    _a.t = FunctionType.Comp,
                    _a.f = fns,
                    _a;
            },
            paramCount: {},
        },
        'constantly': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b;
                var _c = __read(_a, 1), value = _c[0];
                return _b = {},
                    _b[FUNCTION_SYMBOL] = true,
                    _b.sourceCodeInfo = sourceCodeInfo,
                    _b.t = FunctionType.Constantly,
                    _b.v = toAny(value),
                    _b;
            },
            paramCount: 1,
        },
        'juxt': {
            evaluate: function (fns, sourceCodeInfo) {
                var _a;
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.sourceCodeInfo = sourceCodeInfo,
                    _a.t = FunctionType.Juxt,
                    _a.f = fns,
                    _a;
            },
            paramCount: { min: 1 },
        },
        'complement': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b;
                var _c = __read(_a, 1), fn = _c[0];
                return _b = {},
                    _b[FUNCTION_SYMBOL] = true,
                    _b.sourceCodeInfo = sourceCodeInfo,
                    _b.t = FunctionType.Complement,
                    _b.f = toAny(fn),
                    _b;
            },
            paramCount: 1,
        },
        'every-pred': {
            evaluate: function (fns, sourceCodeInfo) {
                var _a;
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.sourceCodeInfo = sourceCodeInfo,
                    _a.t = FunctionType.EveryPred,
                    _a.f = fns,
                    _a;
            },
            paramCount: { min: 1 },
        },
        'some-pred': {
            evaluate: function (fns, sourceCodeInfo) {
                var _a;
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.sourceCodeInfo = sourceCodeInfo,
                    _a.t = FunctionType.SomePred,
                    _a.f = fns,
                    _a;
            },
            paramCount: { min: 1 },
        },
        'fnull': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b;
                var _c = __read(_a), fn = _c[0], params = _c.slice(1);
                return _b = {},
                    _b[FUNCTION_SYMBOL] = true,
                    _b.sourceCodeInfo = sourceCodeInfo,
                    _b.t = FunctionType.Fnull,
                    _b.f = toAny(fn),
                    _b.p = params,
                    _b;
            },
            paramCount: { min: 2 },
        },
    };

    var expressions = __assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign({}, bitwiseNormalExpression), collectionNormalExpression), arrayNormalExpression), sequenceNormalExpression), mathNormalExpression), miscNormalExpression), assertNormalExpression), objectNormalExpression), predicatesNormalExpression), regexpNormalExpression), stringNormalExpression), functionalNormalExpression);
    var aliases = {};
    Object.values(expressions).forEach(function (normalExpression) {
        var _a;
        (_a = normalExpression.aliases) === null || _a === void 0 ? void 0 : _a.forEach(function (alias) {
            aliases[alias] = normalExpression;
        });
    });
    var normalExpressions = __assign(__assign({}, expressions), aliases);

    var andSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var evaluateAstNode = _a.evaluateAstNode;
            var value = true;
            try {
                for (var _c = __values(node.p), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var param = _d.value;
                    value = evaluateAstNode(param, contextStack);
                    if (!value)
                        break;
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
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            return getUndefinedSymbols(node.p, contextStack, builtin);
        },
    };

    var condSpecialExpression = {
        paramCount: { even: true },
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var evaluateAstNode = _a.evaluateAstNode;
            try {
                for (var _c = __values(arrayToPairs(node.p)), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var _e = __read(_d.value, 2), test = _e[0], form = _e[1];
                    var value = evaluateAstNode(test, contextStack);
                    if (!value)
                        continue;
                    return evaluateAstNode(form, contextStack);
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
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            return getUndefinedSymbols(node.p, contextStack, builtin);
        },
    };

    var switchSpecialExpression = {
        paramCount: { odd: true },
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var evaluateAstNode = _a.evaluateAstNode;
            var switchValue = evaluateAstNode(node.p[0], contextStack);
            try {
                for (var _c = __values(arrayToPairs(node.p.slice(1))), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var _e = __read(_d.value, 2), test = _e[0], form = _e[1];
                    var value = evaluateAstNode(test, contextStack);
                    if (value === switchValue) {
                        return evaluateAstNode(form, contextStack);
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
            return null;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            return getUndefinedSymbols(node.p, contextStack, builtin);
        },
    };

    var declaredSpecialExpression = {
        paramCount: 1,
        evaluate: function (node, contextStack) {
            var lookUpResult = contextStack.lookUp(node.p[0]);
            return lookUpResult !== null;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            return getUndefinedSymbols(node.p, contextStack, builtin);
        },
    };

    function isAstNode(value) {
        if (value === null || typeof value !== 'object')
            return false;
        if (!isAstNodeType(value.t))
            return false;
        return true;
    }
    function asAstNode(value, sourceCodeInfo) {
        assertAstNode(value, sourceCodeInfo);
        return value;
    }
    function assertAstNode(value, sourceCodeInfo) {
        if (!isAstNode(value))
            throw getAssertionError('AstNode', value, sourceCodeInfo);
    }
    function isSymbolNode(value) {
        if (!isAstNode(value))
            return false;
        return value.t === AstNodeType.Symbol;
    }
    function asSymbolNode(value, sourceCodeInfo) {
        assertSymbolNode(value, sourceCodeInfo);
        return value;
    }
    function assertSymbolNode(value, sourceCodeInfo) {
        if (!isSymbolNode(value))
            throw getAssertionError('SymbolNode', value, sourceCodeInfo);
    }
    function isNormalExpressionNodeWithName(value) {
        if (!isAstNode(value))
            return false;
        return value.t === AstNodeType.NormalExpression && typeof value.n === 'string';
    }

    var nonNumberReservedSymbolRecord = {
        true: true,
        false: false,
        null: null,
        then: null,
        else: null,
        end: null,
        case: null,
        each: null,
        in: null,
        when: null,
        while: null,
        function: null,
        export: null,
    };
    var phi = (1 + Math.sqrt(5)) / 2;
    var numberReservedSymbolRecord = {
        'E': Math.E,
        '-E': -Math.E,
        'ε': Math.E,
        '-ε': -Math.E,
        'PI': Math.PI,
        '-PI': -Math.PI,
        'π': Math.PI,
        '-π': -Math.PI,
        'PHI': phi,
        '-PHI': -phi,
        'φ': phi,
        '-φ': -phi,
        'POSITIVE_INFINITY': Number.POSITIVE_INFINITY,
        '∞': Number.POSITIVE_INFINITY,
        'NEGATIVE_INFINITY': Number.NEGATIVE_INFINITY,
        '-∞': Number.NEGATIVE_INFINITY,
        'MAX_SAFE_INTEGER': Number.MAX_SAFE_INTEGER,
        'MIN_SAFE_INTEGER': Number.MIN_SAFE_INTEGER,
        'MAX_VALUE': Number.MAX_VALUE,
        'MIN_VALUE': Number.MIN_VALUE,
        'DELTA': Number.EPSILON, // TODO use DELTA instead of DELTA δ
        '-DELTA': -Number.EPSILON,
        'δ': Number.EPSILON, // TODO use DELTA instead of DELTA δ
        '-δ': -Number.EPSILON,
        'NaN': Number.NaN,
    };
    var algebraicReservedSymbolRecord = __assign(__assign({}, nonNumberReservedSymbolRecord), numberReservedSymbolRecord);
    var validReservedSymbolRecord = __assign(__assign({}, nonNumberReservedSymbolRecord), numberReservedSymbolRecord);
    function isReservedSymbol(symbol) {
        return symbol in validReservedSymbolRecord;
    }
    function isNumberReservedSymbol(symbol) {
        return symbol in numberReservedSymbolRecord;
    }

    function assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo) {
        if (typeof name !== 'string')
            return;
        if (builtin.specialExpressions[name])
            throw new LitsError("Cannot define variable ".concat(name, ", it's a special expression."), sourceCodeInfo);
        if (builtin.normalExpressions[name])
            throw new LitsError("Cannot define variable ".concat(name, ", it's a builtin function."), sourceCodeInfo);
        if (isReservedSymbol(name))
            throw new LitsError("Cannot define variable ".concat(name, ", it's a reserved name."), sourceCodeInfo);
        if (contextStack.globalContext[name])
            throw new LitsError("Name already defined \"".concat(name, "\"."), sourceCodeInfo);
    }

    var defSpecialExpression = {
        paramCount: 2,
        evaluate: function (node, contextStack, _a) {
            var _b;
            var evaluateAstNode = _a.evaluateAstNode, builtin = _a.builtin;
            var sourceCodeInfo = (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo;
            var name = node.p[0].v;
            assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo);
            contextStack.exportValue(name, evaluateAstNode(node.p[1], contextStack));
            return null;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var _b;
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            var sourceCodeInfo = (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo;
            var subNode = asAstNode(node.p[1]);
            var result = getUndefinedSymbols([subNode], contextStack, builtin);
            var name = asSymbolNode(node.p[0]).v;
            assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo);
            contextStack.exportValue(name, true);
            return result;
        },
    };

    var doSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var evaluateAstNode = _a.evaluateAstNode;
            var newContext = {};
            var newContextStack = contextStack.create(newContext);
            var result = null;
            try {
                for (var _c = __values(node.p), _d = _c.next(); !_d.done; _d = _c.next()) {
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
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            return getUndefinedSymbols(node.p, contextStack.create({}), builtin);
        },
    };

    var functionSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var _b;
            var _c, _d;
            var builtin = _a.builtin, evaluateAstNode = _a.evaluateAstNode;
            var name = node.f.v;
            assertNameNotDefined(name, contextStack, builtin, (_c = getTokenDebugData(node.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
            var evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode);
            var litsFunction = (_b = {},
                _b[FUNCTION_SYMBOL] = true,
                _b.sourceCodeInfo = (_d = getTokenDebugData(node.token)) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo,
                _b.t = FunctionType.UserDefined,
                _b.n = name,
                _b.o = evaluatedFunctionOverloades,
                _b);
            contextStack.addValue(name, litsFunction);
            return null;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var _b;
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            contextStack.exportValue(node.f.v, true);
            var newContext = (_b = {}, _b[node.f.v] = { value: true }, _b);
            return addOverloadsUnresolvedSymbols(node.o, contextStack, getUndefinedSymbols, builtin, newContext);
        },
    };
    var defnSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var _b;
            var _c, _d;
            var builtin = _a.builtin, evaluateAstNode = _a.evaluateAstNode;
            var name = node.f.v;
            assertNameNotDefined(name, contextStack, builtin, (_c = getTokenDebugData(node.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
            var evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode);
            var litsFunction = (_b = {},
                _b[FUNCTION_SYMBOL] = true,
                _b.sourceCodeInfo = (_d = getTokenDebugData(node.token)) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo,
                _b.t = FunctionType.UserDefined,
                _b.n = name,
                _b.o = evaluatedFunctionOverloades,
                _b);
            contextStack.exportValue(name, litsFunction);
            return null;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var _b;
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            contextStack.exportValue(node.f.v, true);
            var newContext = (_b = {}, _b[node.f.v] = { value: true }, _b);
            return addOverloadsUnresolvedSymbols(node.o, contextStack, getUndefinedSymbols, builtin, newContext);
        },
    };
    var fnSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var _b;
            var _c;
            var evaluateAstNode = _a.evaluateAstNode;
            var evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode);
            var litsFunction = (_b = {},
                _b[FUNCTION_SYMBOL] = true,
                _b.sourceCodeInfo = (_c = getTokenDebugData(node.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo,
                _b.t = FunctionType.UserDefined,
                _b.n = undefined,
                _b.o = evaluatedFunctionOverloades,
                _b);
            return litsFunction;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            return addOverloadsUnresolvedSymbols(node.o, contextStack, getUndefinedSymbols, builtin);
        },
    };
    function evaluateFunctionOverloades(node, contextStack, evaluateAstNode) {
        var e_1, _a, e_2, _b;
        var evaluatedFunctionOverloades = [];
        try {
            for (var _c = __values(node.o), _d = _c.next(); !_d.done; _d = _c.next()) {
                var functionOverload = _d.value;
                var functionContext = {};
                try {
                    for (var _e = (e_2 = void 0, __values(functionOverload.as.b)), _f = _e.next(); !_f.done; _f = _e.next()) {
                        var binding = _f.value;
                        var bindingValueNode = binding.v;
                        var bindingValue = evaluateAstNode(bindingValueNode, contextStack);
                        functionContext[binding.n] = { value: bindingValue };
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
                    as: {
                        mandatoryArguments: functionOverload.as.m,
                        restArgument: functionOverload.as.r,
                    },
                    a: functionOverload.a,
                    b: functionOverload.b,
                    f: functionContext,
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
    function addOverloadsUnresolvedSymbols(overloads, contextStack, getUndefinedSymbols, builtin, functionNameContext) {
        var e_3, _a;
        var result = new Set();
        var contextStackWithFunctionName = functionNameContext ? contextStack.create(functionNameContext) : contextStack;
        var _loop_1 = function (overload) {
            var newContext = {};
            overload.as.b.forEach(function (binding) {
                var bindingResult = getUndefinedSymbols([binding.v], contextStack, builtin);
                addToSet(result, bindingResult);
                newContext[binding.n] = { value: true };
            });
            overload.as.m.forEach(function (arg) {
                newContext[arg] = { value: true };
            });
            if (typeof overload.as.r === 'string')
                newContext[overload.as.r] = { value: true };
            var newContextStack = contextStackWithFunctionName.create(newContext);
            var overloadResult = getUndefinedSymbols(overload.b, newContextStack, builtin);
            addToSet(result, overloadResult);
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

    var ifSpecialExpression = {
        paramCount: { min: 2, max: 3 },
        evaluate: function (node, contextStack, _a) {
            var _b;
            var evaluateAstNode = _a.evaluateAstNode;
            var sourceCodeInfo = (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo;
            var _c = __read(node.p, 3), conditionNode = _c[0], trueNode = _c[1], falseNode = _c[2];
            if (evaluateAstNode(asAstNode(conditionNode, sourceCodeInfo), contextStack)) {
                return evaluateAstNode(asAstNode(trueNode, sourceCodeInfo), contextStack);
            }
            else {
                if (node.p.length === 3)
                    return evaluateAstNode(asAstNode(falseNode, sourceCodeInfo), contextStack);
                else
                    return null;
            }
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            return getUndefinedSymbols(node.p, contextStack, builtin);
        },
    };

    var unlessSpecialExpression = {
        paramCount: { min: 2, max: 3 },
        evaluate: function (node, contextStack, _a) {
            var _b;
            var evaluateAstNode = _a.evaluateAstNode;
            var sourceCodeInfo = (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo;
            var _c = __read(node.p, 3), conditionNode = _c[0], trueNode = _c[1], falseNode = _c[2];
            if (!evaluateAstNode(asAstNode(conditionNode, sourceCodeInfo), contextStack)) {
                return evaluateAstNode(asAstNode(trueNode, sourceCodeInfo), contextStack);
            }
            else {
                if (node.p.length === 3)
                    return evaluateAstNode(asAstNode(falseNode, sourceCodeInfo), contextStack);
                else
                    return null;
            }
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            return getUndefinedSymbols(node.p, contextStack, builtin);
        },
    };

    var letSpecialExpression = {
        paramCount: 0,
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var evaluateAstNode = _a.evaluateAstNode;
            try {
                for (var _c = __values(node.bs), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var binding = _d.value;
                    var bindingValueNode = binding.v;
                    var bindingValue = evaluateAstNode(bindingValueNode, contextStack);
                    contextStack.addValue(binding.n, bindingValue);
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
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            var newContext = node.bs
                .map(function (binding) { return binding.n; })
                .reduce(function (context, name) {
                context[name] = { value: true };
                return context;
            }, {});
            var bindingResults = node.bs.map(function (bindingNode) {
                var valueNode = bindingNode.v;
                var bindingsResult = getUndefinedSymbols([valueNode], contextStack, builtin);
                contextStack.addValue(bindingNode.n, { value: true });
                return bindingsResult;
            });
            var paramsResult = getUndefinedSymbols(node.p, contextStack.create(newContext), builtin);
            return joinSets.apply(void 0, __spreadArray(__spreadArray([], __read(bindingResults), false), [paramsResult], false));
        },
    };

    var loopSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var _b;
            var evaluateAstNode = _a.evaluateAstNode;
            var sourceCodeInfo = (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo;
            var bindingContext = node.bs.reduce(function (result, binding) {
                result[binding.n] = { value: evaluateAstNode(binding.v, contextStack) };
                return result;
            }, {});
            var newContextStack = contextStack.create(bindingContext);
            var _loop_1 = function () {
                var e_1, _c;
                var result = null;
                try {
                    try {
                        for (var _d = (e_1 = void 0, __values(node.p)), _e = _d.next(); !_e.done; _e = _d.next()) {
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
                        if (params_1.length !== node.bs.length) {
                            throw new LitsError("recur expected ".concat(node.bs.length, " parameters, got ").concat(valueToString(params_1.length)), sourceCodeInfo);
                        }
                        node.bs.forEach(function (binding, index) {
                            asNonUndefined(bindingContext[binding.n], sourceCodeInfo).value = asAny(params_1[index], sourceCodeInfo);
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
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            var newContext = node.bs
                .map(function (binding) { return binding.n; })
                .reduce(function (context, name) {
                context[name] = { value: true };
                return context;
            }, {});
            var bindingValueNodes = node.bs.map(function (binding) { return binding.v; });
            var bindingsResult = getUndefinedSymbols(bindingValueNodes, contextStack, builtin);
            var paramsResult = getUndefinedSymbols(node.p, contextStack.create(newContext), builtin);
            return joinSets(bindingsResult, paramsResult);
        },
    };

    function addToContext(bindings, context, contextStack, evaluateAstNode, sourceCodeInfo) {
        var e_1, _a;
        try {
            for (var bindings_1 = __values(bindings), bindings_1_1 = bindings_1.next(); !bindings_1_1.done; bindings_1_1 = bindings_1.next()) {
                var binding = bindings_1_1.value;
                if (context[binding.n])
                    throw new LitsError("Variable already defined: ".concat(binding.n, "."), sourceCodeInfo);
                context[binding.n] = { value: evaluateAstNode(binding.v, contextStack) };
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
    function evaluateLoop(returnResult, node, contextStack, evaluateAstNode) {
        var e_2, _a;
        var _b;
        var sourceCodeInfo = (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo;
        var _c = node, loopBindings = _c.l, params = _c.p;
        var result = [];
        var bindingIndices = loopBindings.map(function () { return 0; });
        var abort = false;
        while (!abort) {
            var context = {};
            var newContextStack = contextStack.create(context);
            var skip = false;
            bindingsLoop: for (var bindingIndex = 0; bindingIndex < loopBindings.length; bindingIndex += 1) {
                var _d = asNonUndefined(loopBindings[bindingIndex], sourceCodeInfo), binding = _d.b, letBindings = _d.l, whenNode = _d.wn, whileNode = _d.we, modifiers = _d.m;
                var coll = asColl(evaluateAstNode(binding.v, newContextStack), sourceCodeInfo);
                var seq = isSeq(coll) ? coll : Object.entries(coll);
                if (seq.length === 0) {
                    skip = true;
                    abort = true;
                    break;
                }
                var index = asNonUndefined(bindingIndices[bindingIndex], sourceCodeInfo);
                if (index >= seq.length) {
                    skip = true;
                    if (bindingIndex === 0) {
                        abort = true;
                        break;
                    }
                    bindingIndices[bindingIndex] = 0;
                    bindingIndices[bindingIndex - 1] = asNonUndefined(bindingIndices[bindingIndex - 1], sourceCodeInfo) + 1;
                    break;
                }
                if (context[binding.n])
                    throw new LitsError("Variable already defined: ".concat(binding.n, "."), sourceCodeInfo);
                context[binding.n] = {
                    value: asAny(seq[index], sourceCodeInfo),
                };
                try {
                    for (var modifiers_1 = (e_2 = void 0, __values(modifiers)), modifiers_1_1 = modifiers_1.next(); !modifiers_1_1.done; modifiers_1_1 = modifiers_1.next()) {
                        var modifier = modifiers_1_1.value;
                        switch (modifier) {
                            case '&let':
                                addToContext(asNonUndefined(letBindings, sourceCodeInfo), context, newContextStack, evaluateAstNode, sourceCodeInfo);
                                break;
                            case '&when':
                                if (!evaluateAstNode(asAstNode(whenNode, sourceCodeInfo), newContextStack)) {
                                    bindingIndices[bindingIndex] = asNonUndefined(bindingIndices[bindingIndex], sourceCodeInfo) + 1;
                                    skip = true;
                                    break bindingsLoop;
                                }
                                break;
                            case '&while':
                                if (!evaluateAstNode(asAstNode(whileNode, sourceCodeInfo), newContextStack)) {
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
                var value = evaluateAstNode(params[0], newContextStack);
                if (returnResult)
                    result.push(value);
                if (bindingIndices.length > 0)
                    bindingIndices[bindingIndices.length - 1] += 1;
            }
        }
        return returnResult ? result : null;
    }
    function analyze$1(node, contextStack, getUndefinedSymbols, builtin) {
        var result = new Set();
        var newContext = {};
        var loopBindings = node.l;
        loopBindings.forEach(function (loopBinding) {
            var binding = loopBinding.b, letBindings = loopBinding.l, whenNode = loopBinding.wn, whileNode = loopBinding.we;
            getUndefinedSymbols([binding.v], contextStack.create(newContext), builtin).forEach(function (symbol) {
                return result.add(symbol);
            });
            newContext[binding.n] = { value: true };
            if (letBindings) {
                letBindings.forEach(function (letBinding) {
                    getUndefinedSymbols([letBinding.v], contextStack.create(newContext), builtin).forEach(function (symbol) {
                        return result.add(symbol);
                    });
                    newContext[letBinding.n] = { value: true };
                });
            }
            if (whenNode) {
                getUndefinedSymbols([whenNode], contextStack.create(newContext), builtin).forEach(function (symbol) {
                    return result.add(symbol);
                });
            }
            if (whileNode) {
                getUndefinedSymbols([whileNode], contextStack.create(newContext), builtin).forEach(function (symbol) {
                    return result.add(symbol);
                });
            }
        });
        getUndefinedSymbols(node.p, contextStack.create(newContext), builtin).forEach(function (symbol) {
            return result.add(symbol);
        });
        return result;
    }
    var forSpecialExpression = {
        paramCount: 1,
        evaluate: function (node, contextStack, helpers) { return evaluateLoop(true, node, contextStack, helpers.evaluateAstNode); },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            return analyze$1(node, contextStack, getUndefinedSymbols, builtin);
        },
    };
    var doseqSpecialExpression = {
        paramCount: 1,
        evaluate: function (node, contextStack, helpers) {
            evaluateLoop(false, node, contextStack, helpers.evaluateAstNode);
            return null;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            return analyze$1(node, contextStack, getUndefinedSymbols, builtin);
        },
    };

    var orSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var evaluateAstNode = _a.evaluateAstNode;
            var value = false;
            try {
                for (var _c = __values(node.p), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var param = _d.value;
                    value = evaluateAstNode(param, contextStack);
                    if (value)
                        break;
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
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            return getUndefinedSymbols(node.p, contextStack, builtin);
        },
    };

    var qqSpecialExpression = {
        paramCount: { min: 1, max: 2 },
        evaluate: function (node, contextStack, _a) {
            var _b;
            var evaluateAstNode = _a.evaluateAstNode;
            var _c = __read(node.p, 2), firstNode = _c[0], secondNode = _c[1];
            if (isSymbolNode(firstNode)) {
                if (contextStack.lookUp(firstNode) === null)
                    return secondNode ? evaluateAstNode(secondNode, contextStack) : null;
            }
            assertAny(firstNode, (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
            var firstResult = evaluateAstNode(firstNode, contextStack);
            return firstResult !== null && firstResult !== void 0 ? firstResult : (secondNode ? evaluateAstNode(secondNode, contextStack) : null);
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            return getUndefinedSymbols(node.p, contextStack, builtin);
        },
    };

    var recurSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var evaluateAstNode = _a.evaluateAstNode;
            var params = node.p.map(function (paramNode) { return evaluateAstNode(paramNode, contextStack); });
            throw new RecurSignal(params);
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            return getUndefinedSymbols(node.p, contextStack, builtin);
        },
    };

    var throwSpecialExpression = {
        paramCount: 1,
        evaluate: function (node, contextStack, _a) {
            var _b, _c;
            var evaluateAstNode = _a.evaluateAstNode;
            var message = asString(evaluateAstNode(node.p[0], contextStack), (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo, {
                nonEmpty: true,
            });
            throw new UserDefinedError(message, (_c = getTokenDebugData(node.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            return getUndefinedSymbols(node.p, contextStack, builtin);
        },
    };

    var trySpecialExpression = {
        paramCount: 1,
        evaluate: function (node, contextStack, _a) {
            var _b;
            var _c;
            var evaluateAstNode = _a.evaluateAstNode;
            var tryExpressions = node.p, catchExpression = node.ce, errorNode = node.e;
            try {
                return evaluateAstNode(tryExpressions[0], contextStack);
            }
            catch (error) {
                var newContext = errorNode
                    ? (_b = {},
                        _b[errorNode.v] = { value: asAny(error, (_c = getTokenDebugData(node.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo) },
                        _b) : {};
                return evaluateAstNode(catchExpression, contextStack.create(newContext));
            }
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var _b;
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin;
            var tryExpressions = node.p, catchExpression = node.ce, errorNode = node.e;
            var tryResult = getUndefinedSymbols(tryExpressions, contextStack, builtin);
            var newContext = errorNode
                ? (_b = {},
                    _b[errorNode.v] = { value: true },
                    _b) : {};
            var catchResult = getUndefinedSymbols([catchExpression], contextStack.create(newContext), builtin);
            return joinSets(tryResult, catchResult);
        },
    };

    var specialExpressions = {
        '&&': andSpecialExpression,
        'cond': condSpecialExpression,
        'switch': switchSpecialExpression,
        'def': defSpecialExpression,
        'defn': defnSpecialExpression,
        'function': functionSpecialExpression,
        'do': doSpecialExpression,
        'doseq': doseqSpecialExpression,
        'for': forSpecialExpression,
        'fn': fnSpecialExpression,
        'if': ifSpecialExpression,
        'unless': unlessSpecialExpression,
        'let': letSpecialExpression,
        'loop': loopSpecialExpression,
        '||': orSpecialExpression,
        'recur': recurSpecialExpression,
        'throw': throwSpecialExpression,
        'try': trySpecialExpression,
        'defined?': declaredSpecialExpression,
        '??': qqSpecialExpression,
    };
    var builtin = {
        normalExpressions: normalExpressions,
        specialExpressions: specialExpressions,
    };
    var normalExpressionKeys = Object.keys(normalExpressions);
    var specialExpressionKeys = Object.keys(specialExpressions);

    var _a;
    function findOverloadFunction(overloads, nbrOfParams, sourceCodeInfo) {
        var overloadFunction = overloads.find(function (overload) {
            var arity = overload.a;
            if (typeof arity === 'number')
                return arity === nbrOfParams;
            else
                return arity.min <= nbrOfParams;
        });
        if (!overloadFunction)
            throw new LitsError("Unexpected number of arguments, got ".concat(valueToString(nbrOfParams), "."), sourceCodeInfo);
        return overloadFunction;
    }
    var functionExecutors = (_a = {},
        _a[FunctionType.NativeJsFunction] = function (fn, params, sourceCodeInfo) {
            var _a;
            try {
                // eslint-disable-next-line ts/no-unsafe-assignment
                var clonedParams = JSON.parse(JSON.stringify(params));
                // eslint-disable-next-line ts/no-unsafe-argument
                return toAny((_a = fn.f).fn.apply(_a, __spreadArray([], __read(clonedParams), false)));
            }
            catch (error) {
                var message = typeof error === 'string'
                    ? error
                    : isUnknownRecord(error) && typeof error.message === 'string'
                        ? error.message
                        : '<no message>';
                throw new LitsError("Native function throwed: \"".concat(message, "\""), sourceCodeInfo);
            }
        },
        _a[FunctionType.UserDefined] = function (fn, params, sourceCodeInfo, contextStack, _a) {
            var e_1, _b;
            var evaluateAstNode = _a.evaluateAstNode;
            for (;;) {
                var overloadFunction = findOverloadFunction(fn.o, params.length, sourceCodeInfo);
                var args = overloadFunction.as;
                var nbrOfMandatoryArgs = args.mandatoryArguments.length;
                var newContext = __assign({}, overloadFunction.f);
                var length_1 = Math.max(params.length, args.mandatoryArguments.length);
                var rest = [];
                for (var i = 0; i < length_1; i += 1) {
                    if (i < nbrOfMandatoryArgs) {
                        var param = toAny(params[i]);
                        var key = asString(args.mandatoryArguments[i], sourceCodeInfo);
                        newContext[key] = { value: param };
                    }
                    else {
                        rest.push(toAny(params[i]));
                    }
                }
                if (args.restArgument)
                    newContext[args.restArgument] = { value: rest };
                try {
                    var result = null;
                    var newContextStack = contextStack.create(newContext, fn.x);
                    try {
                        for (var _c = (e_1 = void 0, __values(overloadFunction.b)), _d = _c.next(); !_d.done; _d = _c.next()) {
                            var node = _d.value;
                            result = evaluateAstNode(node, newContextStack);
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
        _a[FunctionType.Partial] = function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return executeFunction(fn.f, __spreadArray(__spreadArray([], __read(fn.p), false), __read(params), false), contextStack, sourceCodeInfo);
        },
        _a[FunctionType.Comp] = function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var f = fn.f;
            if (f.length === 0) {
                if (params.length !== 1)
                    throw new LitsError("(comp) expects one argument, got ".concat(valueToString(params.length), "."), sourceCodeInfo);
                return asAny(params[0], sourceCodeInfo);
            }
            return asAny(f.reduceRight(function (result, fun) {
                return [executeFunction(toAny(fun), result, contextStack, sourceCodeInfo)];
            }, params)[0], sourceCodeInfo);
        },
        _a[FunctionType.Constantly] = function (fn) {
            return fn.v;
        },
        _a[FunctionType.Juxt] = function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return fn.f.map(function (fun) { return executeFunction(toAny(fun), params, contextStack, sourceCodeInfo); });
        },
        _a[FunctionType.Complement] = function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return !executeFunction(fn.f, params, contextStack, sourceCodeInfo);
        },
        _a[FunctionType.EveryPred] = function (fn, params, sourceCodeInfo, contextStack, _a) {
            var e_2, _b, e_3, _c;
            var executeFunction = _a.executeFunction;
            try {
                for (var _d = __values(fn.f), _e = _d.next(); !_e.done; _e = _d.next()) {
                    var f = _e.value;
                    try {
                        for (var params_1 = (e_3 = void 0, __values(params)), params_1_1 = params_1.next(); !params_1_1.done; params_1_1 = params_1.next()) {
                            var param = params_1_1.value;
                            var result = executeFunction(toAny(f), [param], contextStack, sourceCodeInfo);
                            if (!result)
                                return false;
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
        _a[FunctionType.SomePred] = function (fn, params, sourceCodeInfo, contextStack, _a) {
            var e_4, _b, e_5, _c;
            var executeFunction = _a.executeFunction;
            try {
                for (var _d = __values(fn.f), _e = _d.next(); !_e.done; _e = _d.next()) {
                    var f = _e.value;
                    try {
                        for (var params_2 = (e_5 = void 0, __values(params)), params_2_1 = params_2.next(); !params_2_1.done; params_2_1 = params_2.next()) {
                            var param = params_2_1.value;
                            var result = executeFunction(toAny(f), [param], contextStack, sourceCodeInfo);
                            if (result)
                                return true;
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
        _a[FunctionType.Fnull] = function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var fnulledParams = params.map(function (param, index) { return (param === null ? toAny(fn.p[index]) : param); });
            return executeFunction(toAny(fn.f), fnulledParams, contextStack, sourceCodeInfo);
        },
        _a[FunctionType.Builtin] = function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var normalExpression = asNonUndefined(normalExpressions[fn.n], sourceCodeInfo);
            return normalExpression.evaluate(params, sourceCodeInfo, contextStack, { executeFunction: executeFunction });
        },
        _a);

    function evaluate(ast, contextStack) {
        var e_1, _a;
        var result = null;
        try {
            for (var _b = __values(ast.b), _c = _b.next(); !_c.done; _c = _b.next()) {
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
    function evaluateAstNode(node, contextStack) {
        var _a;
        switch (node.t) {
            case AstNodeType.Number:
                return evaluateNumber(node);
            case AstNodeType.String:
                return evaluateString(node);
            case AstNodeType.Symbol:
                return contextStack.evaluateName(node);
            case AstNodeType.ReservedSymbol:
                return evaluateReservedName(node);
            case AstNodeType.NormalExpression:
                return evaluateNormalExpression(node, contextStack);
            case AstNodeType.SpecialExpression:
                return evaluateSpecialExpression(node, contextStack);
            default:
                throw new LitsError("".concat(node.t, "-node cannot be evaluated"), (_a = getTokenDebugData(node.token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
        }
    }
    function evaluateNumber(node) {
        return node.v;
    }
    function evaluateString(node) {
        return node.v;
    }
    function evaluateReservedName(node) {
        var _a;
        var reservedName = node.v;
        var value = algebraicReservedSymbolRecord[reservedName];
        return asNonUndefined(value, (_a = getTokenDebugData(node.token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
    }
    function evaluateNormalExpression(node, contextStack) {
        var _a;
        var params = node.p.map(function (paramNode) { return evaluateAstNode(paramNode, contextStack); });
        var sourceCodeInfo = (_a = getTokenDebugData(node.token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo;
        if (isNormalExpressionNodeWithName(node)) {
            var value = contextStack.getValue(node.n);
            if (value !== undefined)
                return executeFunction(asAny(value), params, contextStack, sourceCodeInfo);
            return evaluateBuiltinNormalExpression(node, params, contextStack);
        }
        else {
            var fn = params[0];
            return executeFunction(fn, params.slice(1), contextStack, sourceCodeInfo);
        }
    }
    function executeFunction(fn, params, contextStack, sourceCodeInfo) {
        if (isLitsFunction(fn))
            return functionExecutors[fn.t](fn, params, sourceCodeInfo, contextStack, { evaluateAstNode: evaluateAstNode, executeFunction: executeFunction });
        if (Array.isArray(fn))
            return evaluateArrayAsFunction(fn, params, sourceCodeInfo);
        if (isObj(fn))
            return evalueateObjectAsFunction(fn, params, sourceCodeInfo);
        if (typeof fn === 'string')
            return evaluateStringAsFunction(fn, params, sourceCodeInfo);
        if (isNumber(fn))
            return evaluateNumberAsFunction(fn, params, sourceCodeInfo);
        throw new NotAFunctionError(fn, sourceCodeInfo);
    }
    function evaluateBuiltinNormalExpression(node, params, contextStack) {
        var _a, _b;
        var normalExpression = builtin.normalExpressions[node.n];
        if (!normalExpression)
            throw new UndefinedSymbolError(node.n, (_a = getTokenDebugData(node.token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
        return normalExpression.evaluate(params, (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo, contextStack, { executeFunction: executeFunction });
    }
    function evaluateSpecialExpression(node, contextStack) {
        var _a;
        var specialExpression = asNonUndefined(builtin.specialExpressions[node.n], (_a = getTokenDebugData(node.token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
        // eslint-disable-next-line ts/no-unsafe-argument
        return specialExpression.evaluate(node, contextStack, { evaluateAstNode: evaluateAstNode, builtin: builtin });
    }
    function evalueateObjectAsFunction(fn, params, sourceCodeInfo) {
        if (params.length !== 1)
            throw new LitsError('Object as function requires one string parameter.', sourceCodeInfo);
        var key = params[0];
        assertString(key, sourceCodeInfo);
        return toAny(fn[key]);
    }
    function evaluateArrayAsFunction(fn, params, sourceCodeInfo) {
        if (params.length !== 1)
            throw new LitsError('Array as function requires one non negative integer parameter.', sourceCodeInfo);
        var index = params[0];
        assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true });
        return toAny(fn[index]);
    }
    function evaluateStringAsFunction(fn, params, sourceCodeInfo) {
        if (params.length !== 1)
            throw new LitsError('String as function requires one Obj parameter.', sourceCodeInfo);
        var param = toAny(params[0]);
        if (isObj(param))
            return toAny((param)[fn]);
        if (isNumber(param, { integer: true }))
            return toAny(fn[param]);
        throw new LitsError("string as function expects Obj or integer parameter, got ".concat(valueToString(param)), sourceCodeInfo);
    }
    function evaluateNumberAsFunction(fn, params, sourceCodeInfo) {
        assertNumber(fn, sourceCodeInfo, { integer: true });
        if (params.length !== 1)
            throw new LitsError('Number as function requires one Arr parameter.', sourceCodeInfo);
        var param = params[0];
        assertSeq(param, sourceCodeInfo);
        return toAny(param[fn]);
    }

    var getUndefinedSymbols = function (ast, contextStack, builtin) {
        var e_1, _a;
        var _b;
        var astNodes = Array.isArray(ast)
            ? ast
            : [{
                    t: AstNodeType.SpecialExpression,
                    n: 'do',
                    p: ast.b,
                    token: undefined,
                }];
        var unresolvedSymbols = new Set();
        try {
            for (var astNodes_1 = __values(astNodes), astNodes_1_1 = astNodes_1.next(); !astNodes_1_1.done; astNodes_1_1 = astNodes_1.next()) {
                var subNode = astNodes_1_1.value;
                (_b = findUnresolvedSymbolsInAstNode(subNode, contextStack, builtin)) === null || _b === void 0 ? void 0 : _b.forEach(function (symbol) { return unresolvedSymbols.add(symbol); });
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (astNodes_1_1 && !astNodes_1_1.done && (_a = astNodes_1.return)) _a.call(astNodes_1);
            }
            finally { if (e_1) throw e_1.error; }
        }
        return unresolvedSymbols;
    };
    function findUnresolvedSymbolsInAstNode(astNode, contextStack, builtin) {
        var e_2, _a;
        var _b, _c;
        switch (astNode.t) {
            case AstNodeType.Symbol: {
                var lookUpResult = contextStack.lookUp(astNode);
                if (lookUpResult === null)
                    return new Set([astNode.v]);
                return null;
            }
            case AstNodeType.String:
            case AstNodeType.Number:
            case AstNodeType.Modifier:
            case AstNodeType.ReservedSymbol:
            case AstNodeType.Comment:
                return null;
            case AstNodeType.NormalExpression: {
                var unresolvedSymbols_1 = new Set();
                var name_1 = astNode.n, debug = astNode.token;
                if (typeof name_1 === 'string') {
                    var lookUpResult = contextStack.lookUp({ t: AstNodeType.Symbol, v: name_1, token: debug, p: [], n: undefined });
                    if (lookUpResult === null)
                        unresolvedSymbols_1.add(name_1);
                }
                try {
                    for (var _d = __values(astNode.p), _e = _d.next(); !_e.done; _e = _d.next()) {
                        var subNode = _e.value;
                        (_b = findUnresolvedSymbolsInAstNode(subNode, contextStack, builtin)) === null || _b === void 0 ? void 0 : _b.forEach(function (symbol) { return unresolvedSymbols_1.add(symbol); });
                    }
                }
                catch (e_2_1) { e_2 = { error: e_2_1 }; }
                finally {
                    try {
                        if (_e && !_e.done && (_a = _d.return)) _a.call(_d);
                    }
                    finally { if (e_2) throw e_2.error; }
                }
                return unresolvedSymbols_1;
            }
            case AstNodeType.SpecialExpression: {
                var specialExpression = asNonUndefined(builtin.specialExpressions[astNode.n], (_c = getTokenDebugData(astNode.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
                // eslint-disable-next-line ts/no-unsafe-argument
                return specialExpression.getUndefinedSymbols(astNode, contextStack, {
                    getUndefinedSymbols: getUndefinedSymbols,
                    builtin: builtin,
                    evaluateAstNode: evaluateAstNode,
                });
            }
        }
    }

    function isContextEntry(value) {
        return isUnknownRecord(value) && value.value !== undefined;
    }

    var ContextStackImpl = /** @class */ (function () {
        function ContextStackImpl(_a) {
            var contexts = _a.contexts, hostValues = _a.values, lazyHostValues = _a.lazyValues, nativeJsFunctions = _a.nativeJsFunctions;
            this.globalContext = asNonUndefined(contexts[0]);
            this.contexts = contexts;
            this.values = hostValues;
            this.lazyValues = lazyHostValues;
            this.nativeJsFunctions = nativeJsFunctions;
        }
        ContextStackImpl.prototype.create = function (context, extraData) {
            var globalContext = this.globalContext;
            var contextStack = new ContextStackImpl({
                contexts: __spreadArray([context], __read(this.contexts), false),
                values: this.values,
                lazyValues: extraData ? __assign(__assign({}, this.lazyValues), extraData) : this.lazyValues,
                nativeJsFunctions: this.nativeJsFunctions,
            });
            contextStack.globalContext = globalContext;
            return contextStack;
        };
        ContextStackImpl.prototype.exportValue = function (name, value) {
            if (this.globalContext[name]) {
                throw new Error("Cannot redefine exported value \"".concat(name, "\""));
            }
            if (specialExpressionKeys.includes(name)) {
                throw new Error("Cannot shadow special expression \"".concat(name, "\""));
            }
            if (normalExpressionKeys.includes(name)) {
                throw new Error("Cannot shadow builtin function \"".concat(name, "\""));
            }
            this.addValue(name, value);
            this.globalContext[name] = { value: value };
        };
        ContextStackImpl.prototype.addValue = function (name, value) {
            var currentContext = this.contexts[0];
            if (!currentContext) {
                throw new Error('No context to add value to');
            }
            if (currentContext[name]) {
                throw new Error("Cannot redefine value \"".concat(name, "\""));
            }
            if (specialExpressionKeys.includes(name)) {
                throw new Error("Cannot shadow special expression \"".concat(name, "\""));
            }
            if (normalExpressionKeys.includes(name)) {
                throw new Error("Cannot shadow builtin function \"".concat(name, "\""));
            }
            currentContext[name] = { value: toAny(value) };
        };
        ContextStackImpl.prototype.clone = function () {
            // eslint-disable-next-line ts/no-unsafe-argument
            return new ContextStackImpl(JSON.parse(JSON.stringify({
                contexts: this.contexts,
                values: this.values,
                lazyValues: this.lazyValues,
                nativeJsFunctions: this.nativeJsFunctions,
            })));
        };
        ContextStackImpl.prototype.getValue = function (name) {
            var e_1, _a;
            var _b, _c, _d;
            try {
                for (var _e = __values(this.contexts), _f = _e.next(); !_f.done; _f = _e.next()) {
                    var context = _f.value;
                    var contextEntry = context[name];
                    if (contextEntry)
                        return contextEntry.value;
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (_f && !_f.done && (_a = _e.return)) _a.call(_e);
                }
                finally { if (e_1) throw e_1.error; }
            }
            var lazyHostValue = (_b = this.lazyValues) === null || _b === void 0 ? void 0 : _b[name];
            if (lazyHostValue)
                return lazyHostValue.read();
            var nativeJsFunction = (_c = this.nativeJsFunctions) === null || _c === void 0 ? void 0 : _c[name];
            if (nativeJsFunction)
                return nativeJsFunction;
            return (_d = this.values) === null || _d === void 0 ? void 0 : _d[name];
        };
        ContextStackImpl.prototype.lookUp = function (node) {
            var e_2, _a, _b;
            var _c, _d, _e, _f;
            var value = node.v;
            var sourceCodeInfo = (_c = getTokenDebugData(node.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo;
            try {
                for (var _g = __values(this.contexts), _h = _g.next(); !_h.done; _h = _g.next()) {
                    var context = _h.value;
                    var contextEntry = context[value];
                    if (contextEntry)
                        return contextEntry;
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (_h && !_h.done && (_a = _g.return)) _a.call(_g);
                }
                finally { if (e_2) throw e_2.error; }
            }
            var lazyHostValue = (_d = this.lazyValues) === null || _d === void 0 ? void 0 : _d[value];
            if (lazyHostValue !== undefined) {
                return {
                    value: toAny(lazyHostValue.read()),
                };
            }
            var hostValue = (_e = this.values) === null || _e === void 0 ? void 0 : _e[value];
            if (hostValue !== undefined) {
                return {
                    value: toAny(hostValue),
                };
            }
            if (builtin.normalExpressions[value]) {
                var builtinFunction = (_b = {},
                    _b[FUNCTION_SYMBOL] = true,
                    _b.sourceCodeInfo = sourceCodeInfo,
                    _b.t = FunctionType.Builtin,
                    _b.n = value,
                    _b);
                return builtinFunction;
            }
            if (builtin.specialExpressions[value])
                return 'specialExpression';
            var nativeJsFunction = (_f = this.nativeJsFunctions) === null || _f === void 0 ? void 0 : _f[value];
            if (nativeJsFunction) {
                return {
                    value: nativeJsFunction,
                };
            }
            return null;
        };
        ContextStackImpl.prototype.evaluateName = function (node) {
            var _a;
            var lookUpResult = this.lookUp(node);
            if (isContextEntry(lookUpResult))
                return lookUpResult.value;
            else if (isBuiltinFunction(lookUpResult))
                return lookUpResult;
            throw new UndefinedSymbolError(node.v, (_a = getTokenDebugData(node.token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
        };
        return ContextStackImpl;
    }());
    function createContextStack(params) {
        var _a;
        if (params === void 0) { params = {}; }
        var globalContext = (_a = params.globalContext) !== null && _a !== void 0 ? _a : {};
        // Contexts are checked from left to right
        var contexts = params.contexts ? __spreadArray([globalContext], __read(params.contexts), false) : [globalContext];
        var contextStack = new ContextStackImpl({
            contexts: contexts,
            values: params.values,
            lazyValues: params.lazyValues,
            nativeJsFunctions: params.jsFunctions
                && Object.entries(params.jsFunctions).reduce(function (acc, _a) {
                    var _b;
                    var _c = __read(_a, 2), name = _c[0], jsFunction = _c[1];
                    if (specialExpressionKeys.includes(name)) {
                        console.warn("Cannot shadow special expression \"".concat(name, "\", ignoring."));
                        return acc;
                    }
                    if (normalExpressionKeys.includes(name)) {
                        console.warn("Cannot shadow builtin function \"".concat(name, "\", ignoring."));
                        return acc;
                    }
                    acc[name] = (_b = {
                            t: FunctionType.NativeJsFunction,
                            f: jsFunction,
                            n: name
                        },
                        _b[FUNCTION_SYMBOL] = true,
                        _b);
                    return acc;
                }, {}),
        });
        return contextStack.create({});
    }

    function minifyTokenStream(tokenStream, _a) {
        var removeWhiteSpace = _a.removeWhiteSpace;
        var tokens = tokenStream.tokens
            .filter(function (token) {
            if (isA_CommentToken(token)
                || isMultiLineCommentToken(token)
                || (removeWhiteSpace && isWhitespaceToken(token))) {
                return false;
            }
            return true;
        });
        return __assign(__assign({}, tokenStream), { tokens: tokens });
    }

    function parseSymbol(tokenStream, parseState) {
        var _a;
        var tkn = asToken(tokenStream.tokens[parseState.position++]);
        if (!isSymbolToken(tkn)) {
            throw new LitsError("Expected symbol token, got ".concat(tkn[0]), (_a = getTokenDebugData(tkn)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
        }
        if (tkn[1][0] !== '\'') {
            return {
                t: AstNodeType.Symbol,
                v: tkn[1],
                p: [],
                n: undefined,
                token: getTokenDebugData(tkn) && tkn,
            };
        }
        else {
            var value = tkn[1].substring(1, tkn[1].length - 1)
                .replace(/(\\{2})|(\\')|\\(.)/g, function (_, backslash, singleQuote, normalChar) {
                if (backslash) {
                    return '\\';
                }
                if (singleQuote) {
                    return '\'';
                }
                return "\\".concat(normalChar);
            });
            return {
                t: AstNodeType.Symbol,
                v: value,
                p: [],
                n: undefined,
                token: getTokenDebugData(tkn) && tkn,
            };
        }
    }
    function parseReservedSymbol(tokenStream, parseState) {
        var _a;
        var tkn = asToken(tokenStream.tokens[parseState.position++]);
        if (!isReservedSymbolToken(tkn)) {
            throw new LitsError("Expected symbol token, got ".concat(tkn[0]), (_a = getTokenDebugData(tkn)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
        }
        if (isReservedSymbolToken(tkn)) {
            var symbol = tkn[1];
            if (isNumberReservedSymbol(symbol)) {
                return {
                    t: AstNodeType.Number,
                    v: numberReservedSymbolRecord[symbol],
                    p: [],
                    n: undefined,
                    token: getTokenDebugData(tkn) && tkn,
                };
            }
        }
        return {
            t: AstNodeType.ReservedSymbol,
            v: tkn[1],
            p: [],
            n: undefined,
            token: getTokenDebugData(tkn) && tkn,
        };
    }
    function parseNumber(tokenStream, parseState) {
        var _a;
        var tkn = tokenStream.tokens[parseState.position++];
        if (!isBasePrefixedNumberToken(tkn) && !isNumberToken(tkn)) {
            throw new LitsError("Expected number token, got ".concat(tkn), (_a = getTokenDebugData(tkn)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
        }
        var value = tkn[1];
        var negative = value[0] === '-';
        var numberString = (negative ? value.substring(1) : value).replace(/_/g, '');
        return {
            t: AstNodeType.Number,
            v: negative ? -Number(numberString) : Number(numberString),
            p: [],
            n: undefined,
            token: getTokenDebugData(tkn) && tkn,
        };
    }
    function parseString(tokenStream, parseState) {
        var tkn = asStringToken(tokenStream.tokens[parseState.position++]);
        var value = tkn[1].substring(1, tkn[1].length - 1)
            .replace(/(\\{2})|(\\")|(\\n)|(\\t)|(\\r)|(\\b)|(\\f)|\\(.)/g, function (_, backslash, doubleQuote, newline, tab, carriageReturn, backspace, formFeed, normalChar) {
            // If it's a double escape (\\x), return \x
            if (backslash) {
                return '\\';
            }
            // If it's a special character (\n, \t, \r, \b, \f), return the special character
            else if (newline) {
                return '\n';
            }
            else if (tab) {
                return '\t';
            }
            else if (carriageReturn) {
                return '\r';
            }
            else if (backspace) {
                return '\b';
            }
            else if (formFeed) {
                return '\f';
            }
            else if (doubleQuote) {
                return '"';
            }
            return normalChar;
        });
        return {
            t: AstNodeType.String,
            v: value,
            p: [],
            n: undefined,
            token: getTokenDebugData(tkn) && tkn,
        };
    }
    function parseRegexpShorthand(tokenStream, parseState) {
        var tkn = asRegexpShorthandToken(tokenStream.tokens[parseState.position++]);
        var endStringPosition = tkn[1].lastIndexOf('"');
        var regexpString = tkn[1].substring(2, endStringPosition);
        var optionsString = tkn[1].substring(endStringPosition + 1);
        var stringNode = {
            t: AstNodeType.String,
            v: regexpString,
            p: [],
            n: undefined,
            token: getTokenDebugData(tkn) && tkn,
        };
        var optionsNode = {
            t: AstNodeType.String,
            v: optionsString,
            p: [],
            n: undefined,
            token: getTokenDebugData(tkn) && tkn,
        };
        var node = {
            t: AstNodeType.NormalExpression,
            n: 'regexp',
            p: [stringNode, optionsNode],
            token: getTokenDebugData(tkn) && tkn,
        };
        return node;
    }

    var exponentiationPrecedence = 10;
    var binaryFunctionalOperatorPrecedence = 1;
    var placeholderRegexp = /^\$([1-9]\d?)?$/;
    function getPrecedence(operatorSign) {
        switch (operatorSign) {
            case '**': // exponentiation
                return exponentiationPrecedence;
            case '*': // multiplication
            case '/': // division
            case '%': // remainder
                return 9;
            case '+': // addition
            case '-': // subtraction
                return 8;
            case '<<': // left shift
            case '>>': // signed right shift
            case '>>>': // unsigned right shift
                return 7;
            case '++': // string concatenation
                return 6;
            case '<': // less than
            case '<=': // less than or equal
            case '≤': // less than or equal
            case '>': // greater than
            case '>=': // greater than or equal
            case '≥': // greater than or equal
                return 5;
            case '=': // equal
            case '!=': // not equal
            case '≠': // not equal
                return 4;
            case '&': // bitwise AND
            case '^': // bitwise XOR
            case '|': // bitwise OR
                return 3;
            case '&&': // logical AND
            case '||': // logical OR
            case '??': // nullish coalescing
                return 2;
            // leave room for binaryFunctionalOperatorPrecedence = 1
            default:
                throw new Error("Unknown binary operator: ".concat(operatorSign));
        }
    }
    function createNamedNormalExpressionNode(name, params, token) {
        var node = {
            t: AstNodeType.NormalExpression,
            n: name,
            p: params,
            token: getTokenDebugData(token) && token,
        };
        var builtinExpression = builtin.normalExpressions[node.n];
        if (builtinExpression) {
            assertNumberOfParams(builtinExpression.paramCount, node);
        }
        return node;
    }
    function fromSymbolToStringNode(symbol) {
        return {
            t: AstNodeType.String,
            v: symbol.v,
            token: getTokenDebugData(symbol.token) && symbol.token,
            p: [],
            n: undefined,
        };
    }
    function createAccessorNode(left, right, token) {
        // Unnamed normal expression
        return {
            t: AstNodeType.NormalExpression,
            p: [left, right],
            n: undefined,
            token: getTokenDebugData(token) && token,
        };
    }
    function fromBinaryOperatorToAstNode(operator, left, right, token) {
        var _a, _b, _c;
        var operatorName = operator[1];
        switch (operatorName) {
            case '.':
                return createAccessorNode(left, fromSymbolToStringNode(asSymbolNode(right, (_a = getTokenDebugData(token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo)), token);
            case '**': // exponentiation
            case '*':
            case '/':
            case '%':
            case '+':
            case '-':
            case '<<':
            case '>>':
            case '>>>':
            case '++':
            case '<':
            case '<=':
            case '≤':
            case '>':
            case '>=':
            case '≥':
            case '=':
            case '!=':
            case '≠':
            case '&':
            case '^':
            case '|':
                return createNamedNormalExpressionNode(operatorName, [left, right], token);
            case '&&':
            case '||':
            case '??':
                return {
                    t: AstNodeType.SpecialExpression,
                    n: operatorName,
                    p: [left, right],
                    token: getTokenDebugData(token) && token,
                };
            /* v8 ignore next 8 */
            case ';':
            case ':=':
            case ',':
            case '->':
            case '...':
                throw new LitsError("Unknown binary operator: ".concat(operatorName), (_b = getTokenDebugData(token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
            default:
                throw new LitsError("Unknown binary operator: ".concat(operatorName), (_c = getTokenDebugData(token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
        }
    }
    var AlgebraicParser = /** @class */ (function () {
        function AlgebraicParser(tokenStream, parseState) {
            this.tokenStream = tokenStream;
            this.parseState = parseState;
        }
        AlgebraicParser.prototype.peek = function () {
            return this.tokenStream.tokens[this.parseState.position];
        };
        AlgebraicParser.prototype.peekAhead = function (count) {
            return this.tokenStream.tokens[this.parseState.position + count];
        };
        AlgebraicParser.prototype.advance = function () {
            this.parseState.position += 1;
        };
        AlgebraicParser.prototype.parse = function () {
            var _a;
            var nodes = [];
            while (!this.isAtEnd()) {
                nodes.push(this.parseExpression(0, true));
                if (isOperatorToken(this.peek(), ';')) {
                    this.advance();
                }
                else {
                    if (!this.isAtEnd()) {
                        throw new LitsError('Expected ;', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                    }
                }
            }
            return nodes;
        };
        AlgebraicParser.prototype.parseExpression = function (precedence, moduleScope) {
            var _a, _b;
            if (precedence === void 0) { precedence = 0; }
            if (moduleScope === void 0) { moduleScope = false; }
            var firstToken = this.peek();
            var left;
            if (isSymbolToken(firstToken)) {
                switch (firstToken[1]) {
                    case 'let':
                        return this.parseLet(firstToken);
                    case 'if':
                    case 'unless':
                        left = this.parseIfOrUnless(firstToken);
                        break;
                    case 'cond':
                        left = this.parseCond(firstToken);
                        break;
                    case 'switch':
                        left = this.parseSwitch(firstToken);
                        break;
                    case 'for':
                    case 'doseq':
                        left = this.parseForOrDoseq(firstToken);
                        break;
                    case 'do':
                        left = this.parseDo(firstToken);
                        break;
                    case 'loop':
                        left = this.parseLoop(firstToken);
                        break;
                    case 'try':
                        left = this.parseTry(firstToken);
                        break;
                }
            }
            else if (isReservedSymbolToken(firstToken, 'function')) {
                return this.parseFunction(firstToken);
            }
            else if (isReservedSymbolToken(firstToken, 'export')) {
                if (!moduleScope) {
                    throw new LitsError('export is only allowed in module scope', (_a = getTokenDebugData(firstToken)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                }
                return this.parseExport(firstToken);
            }
            left || (left = this.parseOperand());
            var operator = this.peek();
            while (!this.isAtExpressionEnd()) {
                if (isA_BinaryOperatorToken(operator)) {
                    var name_1 = operator[1];
                    var newPrecedece = getPrecedence(name_1);
                    if (newPrecedece <= precedence
                        // ** (exponentiation) is right associative
                        && !(newPrecedece === exponentiationPrecedence && precedence === exponentiationPrecedence)) {
                        break;
                    }
                    this.advance();
                    var right = this.parseExpression(newPrecedece);
                    var token = hasTokenDebugData(operator) ? operator : undefined;
                    left = fromBinaryOperatorToAstNode(operator, left, right, token);
                }
                else if (isSymbolToken(operator)) {
                    if (!isFunctionOperator(operator[1])) {
                        break;
                    }
                    var newPrecedece = binaryFunctionalOperatorPrecedence;
                    if (newPrecedece <= precedence) {
                        break;
                    }
                    this.advance();
                    var right = this.parseExpression(newPrecedece);
                    var token = hasTokenDebugData(operator) ? operator : undefined;
                    left = createNamedNormalExpressionNode(operator[1], [left, right], token);
                }
                else {
                    break;
                }
                operator = this.peek();
            }
            if (!left) {
                throw new LitsError('Expected operand', (_b = getTokenDebugData(this.peek())) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
            }
            return left;
        };
        AlgebraicParser.prototype.parseOperand = function () {
            var _a, _b;
            var operand = this.parseOperandPart();
            var token = this.peek();
            while (isOperatorToken(token, '.') || isLBracketToken(token) || isLParenToken(token)) {
                if (token[1] === '.') {
                    this.advance();
                    var symbolToken = this.peek();
                    if (!isSymbolToken(symbolToken)) {
                        throw new LitsError('Expected symbol', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                    }
                    var stringNode = {
                        t: AstNodeType.String,
                        v: symbolToken[1],
                        token: getTokenDebugData(symbolToken) && symbolToken,
                        p: [],
                        n: undefined,
                    };
                    operand = createAccessorNode(operand, stringNode, token);
                    this.advance();
                    token = this.peek();
                }
                else if (isLBracketToken(token)) {
                    this.advance();
                    var expression = this.parseExpression();
                    if (!isRBracketToken(this.peek())) {
                        throw new LitsError('Expected closing bracket', (_b = getTokenDebugData(this.peek())) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
                    }
                    operand = createAccessorNode(operand, expression, token);
                    this.advance();
                    token = this.peek();
                }
                else if (isLParenToken(token)) {
                    operand = this.parseFunctionCall(operand);
                    token = this.peek();
                }
            }
            return operand;
        };
        AlgebraicParser.prototype.parseOperandPart = function () {
            var _a, _b;
            var token = this.peek();
            // Parentheses
            if (isLParenToken(token)) {
                var positionBefore = this.parseState.position;
                var lamdaFunction = this.parseLambdaFunction();
                if (lamdaFunction) {
                    return lamdaFunction;
                }
                this.parseState.position = positionBefore;
                this.advance();
                var expression = this.parseExpression();
                if (!isRParenToken(this.peek())) {
                    throw new Error('Expected closing parenthesis');
                }
                this.advance();
                return expression;
            }
            // Unary operators
            else if (isOperatorToken(token)) {
                var operatorName = token[1];
                if (isBinaryOperator(operatorName)) {
                    this.advance();
                    return {
                        t: AstNodeType.Symbol,
                        v: operatorName,
                        token: getTokenDebugData(token) && token,
                        p: [],
                        n: undefined,
                    };
                }
                if (operatorName === '->') {
                    return this.parseShorthandLamdaFunction();
                }
                else {
                    throw new LitsError("Illegal operator: ".concat(operatorName), (_a = getTokenDebugData(token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                }
            }
            // Object litteral, e.g. {a=1, b=2}
            if (isLBraceToken(token)) {
                return this.parseObject();
            }
            // Array litteral, e.g. [1, 2]
            if (isLBracketToken(token)) {
                return this.parseArray();
            }
            var tokenType = token[0];
            switch (tokenType) {
                case 'Number':
                case 'BasePrefixedNumber':
                    return parseNumber(this.tokenStream, this.parseState);
                case 'String':
                    return parseString(this.tokenStream, this.parseState);
                case 'Symbol': {
                    var positionBefore = this.parseState.position;
                    var lamdaFunction = this.parseLambdaFunction();
                    if (lamdaFunction) {
                        return lamdaFunction;
                    }
                    this.parseState.position = positionBefore;
                    return parseSymbol(this.tokenStream, this.parseState);
                }
                case 'ReservedSymbol':
                    return parseReservedSymbol(this.tokenStream, this.parseState);
                case 'RegexpShorthand':
                    return parseRegexpShorthand(this.tokenStream, this.parseState);
                default:
                    throw new LitsError("Unknown token type: ".concat(tokenType), (_b = getTokenDebugData(token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
            }
        };
        AlgebraicParser.prototype.parseObject = function () {
            var _a, _b, _c;
            var firstToken = asLBraceToken(this.peek());
            this.advance();
            var params = [];
            while (!this.isAtEnd() && !isRBraceToken(this.peek())) {
                var key = this.parseOperand();
                if (key === null) {
                    throw new LitsError('Expected key', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                }
                if (key.t !== AstNodeType.Symbol && key.t !== AstNodeType.String) {
                    throw new LitsError('Expected key to be a symbol or a string', (_b = getTokenDebugData(this.peek())) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
                }
                params.push({
                    t: AstNodeType.String,
                    v: key.v,
                    token: getTokenDebugData(key.token) && key.token,
                    p: [],
                    n: undefined,
                });
                assertOperatorToken(this.peek(), ':=');
                this.advance();
                params.push(this.parseExpression());
                var nextToken = this.peek();
                if (!isOperatorToken(nextToken, ',') && !isRBraceToken(nextToken)) {
                    throw new LitsError('Expected comma or closing brace', (_c = getTokenDebugData(this.peek())) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
                }
                if (isOperatorToken(nextToken, ',')) {
                    this.advance();
                }
            }
            assertRBraceToken(this.peek());
            this.advance();
            return {
                t: AstNodeType.NormalExpression,
                n: 'object',
                p: params,
                token: getTokenDebugData(firstToken) && firstToken,
            };
        };
        AlgebraicParser.prototype.parseArray = function () {
            var _a;
            var firstToken = asLBracketToken(this.peek());
            this.advance();
            var params = [];
            while (!this.isAtEnd() && !isRBracketToken(this.peek())) {
                params.push(this.parseExpression());
                var nextToken = this.peek();
                if (!isOperatorToken(nextToken, ',') && !isRBracketToken(nextToken)) {
                    throw new LitsError('Expected comma or closing parenthesis', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                }
                if (isOperatorToken(nextToken, ',')) {
                    this.advance();
                }
            }
            assertRBracketToken(this.peek());
            this.advance();
            return {
                t: AstNodeType.NormalExpression,
                n: 'array',
                p: params,
                token: getTokenDebugData(firstToken) && firstToken,
            };
        };
        AlgebraicParser.prototype.parseFunctionCall = function (symbol) {
            var _a, _b;
            var isNamedFunction = symbol.t === AstNodeType.Symbol;
            this.advance();
            var params = [];
            while (!this.isAtEnd() && !isRParenToken(this.peek())) {
                params.push(this.parseExpression());
                var nextToken = this.peek();
                if (!isOperatorToken(nextToken, ',') && !isRParenToken(nextToken)) {
                    throw new LitsError('Expected comma or closing parenthesis', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                }
                if (isOperatorToken(nextToken, ',')) {
                    this.advance();
                }
            }
            if (!isRParenToken(this.peek())) {
                throw new LitsError('Expected closing parenthesis', (_b = getTokenDebugData(this.peek())) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
            }
            this.advance();
            if (isNamedFunction) {
                if (specialExpressionKeys.includes(symbol.v)) {
                    var name_2 = symbol.v;
                    switch (name_2) {
                        case '??':
                        case '&&':
                        case 'defined?':
                        case '||':
                        case 'recur':
                        case 'throw': {
                            var node = {
                                t: AstNodeType.SpecialExpression,
                                n: name_2,
                                p: params,
                                token: getTokenDebugData(symbol.token) && symbol.token,
                            };
                            assertNumberOfParams(builtin.specialExpressions[node.n].paramCount, node);
                            return node;
                        }
                        case 'fn':
                        case 'def':
                        case 'defn':
                            throw new Error("Special expression ".concat(name_2, " is not available in algebraic notation"));
                        default:
                            throw new Error("Unknown special expression: ".concat(name_2));
                    }
                }
                return createNamedNormalExpressionNode(symbol.v, params, symbol.token);
            }
            else {
                return {
                    t: AstNodeType.NormalExpression,
                    n: undefined,
                    p: __spreadArray([symbol], __read(params), false),
                    token: getTokenDebugData(symbol.token) && symbol.token,
                };
            }
        };
        AlgebraicParser.prototype.parseLambdaFunction = function () {
            var firstToken = this.peek();
            if (isLParenToken(firstToken)
                && isSymbolToken(this.peekAhead(1))
                && isOperatorToken(this.peekAhead(2), '->')) {
                return null;
            }
            try {
                var _a = this.parseFunctionArguments(), functionArguments = _a.functionArguments, arity = _a.arity;
                if (!isOperatorToken(this.peek(), '->')) {
                    return null;
                }
                this.advance();
                var body = this.parseExpression();
                return {
                    t: AstNodeType.SpecialExpression,
                    n: 'fn',
                    p: [],
                    o: [{
                            as: functionArguments,
                            b: [body],
                            a: arity,
                        }],
                    token: getTokenDebugData(firstToken) && firstToken,
                };
            }
            catch (_b) {
                return null;
            }
        };
        AlgebraicParser.prototype.parseFunctionArguments = function () {
            var _a, _b, _c, _d, _e;
            var firstToken = this.peek();
            if (isSymbolToken(firstToken)) {
                this.advance();
                return {
                    functionArguments: {
                        m: [firstToken[1]],
                        b: [],
                        r: undefined,
                    },
                    arity: 1,
                };
            }
            this.advance();
            var rest = false;
            var args = [];
            var restArg;
            while (!this.isAtEnd() && !isRParenToken(this.peek()) && !isSymbolToken(this.peek(), 'let')) {
                if (isOperatorToken(this.peek(), '...')) {
                    if (rest) {
                        throw new LitsError('Multiple spread operators in lambda function', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                    }
                    this.advance();
                    rest = true;
                }
                var symbolToken = this.peek();
                if (!isSymbolToken(symbolToken)) {
                    throw new LitsError('Expected symbol', (_b = getTokenDebugData(this.peek())) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
                }
                if (rest) {
                    restArg = symbolToken[1];
                }
                else {
                    args.push(symbolToken[1]);
                }
                this.advance();
                if (!isOperatorToken(this.peek(), ',') && !isRParenToken(this.peek()) && !isSymbolToken(this.peek(), 'let')) {
                    throw new LitsError('Expected comma or closing parenthesis', (_c = getTokenDebugData(this.peek())) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
                }
                if (isOperatorToken(this.peek(), ',')) {
                    this.advance();
                }
            }
            var arity = restArg !== undefined ? { min: args.length } : args.length;
            // let bindings, to be able to pass on values in the context down to the body
            // This is needed since lits is dynamically scoped
            // E.g.
            // x => y => x + y // would not work, x is not available in the second lambda
            // x => (y, let x = x) => x + y // would work, x is available in the second lambda
            var bindingNodess = [];
            var token = this.peek();
            while (isSymbolToken(token, 'let')) {
                var letNode = this.parseLet(token, true);
                bindingNodess.push(letNode.bs[0]);
                if (!isOperatorToken(this.peek(), ',') && !isRParenToken(this.peek())) {
                    throw new LitsError('Expected comma or closing parenthesis', (_d = getTokenDebugData(this.peek())) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
                }
                if (isOperatorToken(this.peek(), ',')) {
                    this.advance();
                }
                token = this.peek();
            }
            if (!isRParenToken(this.peek())) {
                throw new LitsError('Expected closing parenthesis', (_e = getTokenDebugData(this.peek())) === null || _e === void 0 ? void 0 : _e.sourceCodeInfo);
            }
            var functionArguments = {
                m: args,
                r: restArg,
                b: bindingNodess,
            };
            this.advance();
            return {
                functionArguments: functionArguments,
                arity: arity,
            };
        };
        AlgebraicParser.prototype.parseShorthandLamdaFunction = function () {
            var _a, _b, _c;
            var firstToken = this.peek();
            this.advance();
            var startPos = this.parseState.position;
            var exprNode = this.parseExpression();
            var endPos = this.parseState.position - 1;
            var arity = 0;
            var percent1 = 'NOT_SET'; // referring to argument bindings. % = NAKED, %1, %2, %3, etc = WITH_1
            for (var pos = startPos; pos <= endPos; pos += 1) {
                var tkn = this.tokenStream.tokens[pos];
                if (isSymbolToken(tkn)) {
                    var match = placeholderRegexp.exec(tkn[1]);
                    if (match) {
                        var number = (_a = match[1]) !== null && _a !== void 0 ? _a : '1';
                        if (number === '1') {
                            var mixedPercent1 = (!match[1] && percent1 === 'WITH_1') || (match[1] && percent1 === 'NAKED');
                            if (mixedPercent1)
                                throw new LitsError('Please make up your mind, either use $ or $1', (_b = getTokenDebugData(firstToken)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
                            percent1 = match[1] ? 'WITH_1' : 'NAKED';
                        }
                        arity = Math.max(arity, Number(number));
                        if (arity > 20)
                            throw new LitsError('Can\'t specify more than 20 arguments', (_c = getTokenDebugData(firstToken)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
                    }
                }
            }
            var mandatoryArguments = [];
            for (var i = 1; i <= arity; i += 1) {
                if (i === 1 && percent1 === 'NAKED')
                    mandatoryArguments.push('$');
                else
                    mandatoryArguments.push("$".concat(i));
            }
            var args = {
                b: [],
                m: mandatoryArguments,
            };
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'fn',
                p: [],
                o: [
                    {
                        as: args,
                        b: [exprNode],
                        a: args.m.length,
                    },
                ],
                token: getTokenDebugData(firstToken) && firstToken,
            };
            return node;
        };
        AlgebraicParser.prototype.parseLet = function (token, optionalSemicolon) {
            if (optionalSemicolon === void 0) { optionalSemicolon = false; }
            this.advance();
            var letSymbol = parseSymbol(this.tokenStream, this.parseState);
            assertOperatorToken(this.peek(), ':=');
            this.advance();
            var value = this.parseExpression();
            if (!optionalSemicolon) {
                assertOperatorToken(this.peek(), ';');
            }
            return {
                t: AstNodeType.SpecialExpression,
                n: 'let',
                p: [],
                bs: [{
                        t: AstNodeType.Binding,
                        n: letSymbol.v,
                        v: value,
                        p: [],
                        token: getTokenDebugData(token) && token,
                    }],
                token: getTokenDebugData(letSymbol.token) && letSymbol.token,
            };
        };
        AlgebraicParser.prototype.parseDo = function (token) {
            var _a;
            this.advance();
            var expressions = [];
            while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
                expressions.push(this.parseExpression());
                if (isOperatorToken(this.peek(), ';')) {
                    this.advance();
                }
                else if (!isReservedSymbolToken(this.peek(), 'end')) {
                    throw new LitsError('Expected ;', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                }
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            return {
                t: AstNodeType.SpecialExpression,
                n: 'do',
                p: expressions,
                token: getTokenDebugData(token) && token,
            };
        };
        AlgebraicParser.prototype.parseLoop = function (firstToken) {
            var _a, _b;
            this.advance();
            var bindingNodes = [];
            var token = this.peek();
            while (!this.isAtEnd() && !isSymbolToken(token, 'do')) {
                assertSymbolToken(token, 'let');
                this.advance();
                var symbol = parseSymbol(this.tokenStream, this.parseState);
                assertOperatorToken(this.peek(), ':=');
                this.advance();
                var value = this.parseExpression();
                bindingNodes.push({
                    t: AstNodeType.Binding,
                    n: symbol.v,
                    v: value,
                    p: [],
                    token: getTokenDebugData(symbol.token) && symbol.token,
                });
                if (isOperatorToken(this.peek(), ',')) {
                    this.advance();
                }
                token = this.peek();
            }
            if (bindingNodes.length === 0) {
                throw new LitsError('Expected binding', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
            }
            assertSymbolToken(token, 'do');
            this.advance();
            var params = [];
            while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
                params.push(this.parseExpression());
                if (isOperatorToken(this.peek(), ';')) {
                    this.advance();
                }
                else if (!isReservedSymbolToken(this.peek(), 'end')) {
                    throw new LitsError('Expected ;', (_b = getTokenDebugData(this.peek())) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
                }
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            return {
                t: AstNodeType.SpecialExpression,
                n: 'loop',
                p: params,
                bs: bindingNodes,
                token: getTokenDebugData(firstToken) && firstToken,
            };
        };
        AlgebraicParser.prototype.parseTry = function (token) {
            var _a, _b;
            this.advance();
            var tryExpressions = [];
            while (!this.isAtEnd() && !isSymbolToken(this.peek(), 'catch')) {
                tryExpressions.push(this.parseExpression());
                if (isOperatorToken(this.peek(), ';')) {
                    this.advance();
                }
                else if (!isSymbolToken(this.peek(), 'catch')) {
                    throw new LitsError('Expected ;', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                }
            }
            var tryExpression = tryExpressions.length === 1
                ? tryExpressions[0]
                : {
                    t: AstNodeType.SpecialExpression,
                    n: 'do',
                    p: tryExpressions,
                    token: getTokenDebugData(token) && token,
                };
            assertSymbolToken(this.peek(), 'catch');
            this.advance();
            var errorSymbol;
            if (isLParenToken(this.peek())) {
                this.advance();
                errorSymbol = parseSymbol(this.tokenStream, this.parseState);
                assertRParenToken(this.peek());
                this.advance();
            }
            var catchExpressions = [];
            while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
                catchExpressions.push(this.parseExpression());
                if (isOperatorToken(this.peek(), ';')) {
                    this.advance();
                }
                else if (!isReservedSymbolToken(this.peek(), 'end')) {
                    throw new LitsError('Expected ;', (_b = getTokenDebugData(this.peek())) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
                }
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            var catchExpression = catchExpressions.length === 1
                ? catchExpressions[0]
                : {
                    t: AstNodeType.SpecialExpression,
                    n: 'do',
                    p: catchExpressions,
                    token: getTokenDebugData(token) && token,
                };
            return {
                t: AstNodeType.SpecialExpression,
                n: 'try',
                p: [tryExpression],
                ce: catchExpression,
                e: errorSymbol,
                token: getTokenDebugData(token) && token,
            };
        };
        AlgebraicParser.prototype.parseForOrDoseq = function (firstToken) {
            var _a;
            var isDoseq = firstToken[1] === 'doseq';
            this.advance();
            var forLoopBindings = [];
            while (!this.isAtEnd() && !isSymbolToken(this.peek(), 'do')) {
                forLoopBindings.push(this.parseForLoopBinding());
            }
            assertSymbolToken(this.peek(), 'do');
            this.advance();
            var expressions = [];
            while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
                expressions.push(this.parseExpression());
                if (isOperatorToken(this.peek(), ';')) {
                    this.advance();
                }
                else if (!isReservedSymbolToken(this.peek(), 'end')) {
                    throw new LitsError('Expected ;', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                }
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            return {
                t: AstNodeType.SpecialExpression,
                n: isDoseq ? 'doseq' : 'for',
                p: expressions,
                token: getTokenDebugData(firstToken) && firstToken,
                l: forLoopBindings,
            };
        };
        AlgebraicParser.prototype.parseForLoopBinding = function () {
            var _a, _b, _c, _d, _e, _f, _g;
            assertReservedSymbolToken(this.peek(), 'each');
            this.advance();
            var bindingNode = this.parseBinding();
            // if (isSymbolToken(this.peek(), 'do') || isReservedSymbolToken(this.peek(), 'each')) {
            //   return {
            //     b: bindingNode,
            //     m: [],
            //   }
            // }
            var modifiers = [];
            var token = this.peek();
            if (!isSymbolToken(token, 'do') && !isReservedSymbolToken(this.peek(), 'each') && !isOperatorToken(token, ',')) {
                throw new LitsError('Expected do, each or comma', (_a = getTokenDebugData(token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
            }
            if (isOperatorToken(token, ',')) {
                this.advance();
                token = this.peek();
            }
            if (!isSymbolToken(token, 'let')
                && !isReservedSymbolToken(token, 'when')
                && !isReservedSymbolToken(token, 'while')
                && !isSymbolToken(token, 'do')
                && !isReservedSymbolToken(token, 'each')) {
                throw new LitsError('Expected symbol each, do, let, when or while', (_b = getTokenDebugData(token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
            }
            var letBindings;
            if (token[1] === 'let') {
                modifiers.push('&let');
                letBindings = [];
                while (isSymbolToken(token, 'let')) {
                    var letNode = this.parseLet(token, true);
                    letBindings.push(letNode.bs[0]);
                    token = this.peek();
                    if (!isSymbolToken(token, 'do') && !isReservedSymbolToken(this.peek(), 'each') && !isOperatorToken(token, ',')) {
                        throw new LitsError('Expected do, each or comma', (_c = getTokenDebugData(token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
                    }
                    if (isOperatorToken(token, ',')) {
                        this.advance();
                    }
                    token = this.peek();
                }
            }
            var whenNode;
            var whileNode;
            while (isReservedSymbolToken(token, 'when')
                || isReservedSymbolToken(token, 'while')) {
                this.advance();
                if (token[1] === 'when') {
                    if (modifiers.includes('&when')) {
                        throw new LitsError('Multiple when modifiers in for loop', (_d = getTokenDebugData(token)) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
                    }
                    modifiers.push('&when');
                    whenNode = this.parseExpression();
                }
                else {
                    if (modifiers.includes('&while')) {
                        throw new LitsError('Multiple while modifiers in for loop', (_e = getTokenDebugData(token)) === null || _e === void 0 ? void 0 : _e.sourceCodeInfo);
                    }
                    modifiers.push('&while');
                    whileNode = this.parseExpression();
                }
                token = this.peek();
                if (!isSymbolToken(token, 'do') && !isReservedSymbolToken(this.peek(), 'each') && !isOperatorToken(token, ',')) {
                    throw new LitsError('Expected do or comma', (_f = getTokenDebugData(token)) === null || _f === void 0 ? void 0 : _f.sourceCodeInfo);
                }
                if (isOperatorToken(token, ',')) {
                    this.advance();
                }
                token = this.peek();
            }
            if (!isSymbolToken(token, 'do') && !isReservedSymbolToken(this.peek(), 'each')) {
                throw new LitsError('Expected do or each', (_g = getTokenDebugData(token)) === null || _g === void 0 ? void 0 : _g.sourceCodeInfo);
            }
            return {
                b: bindingNode,
                m: modifiers,
                l: letBindings,
                wn: whenNode,
                we: whileNode,
            };
        };
        AlgebraicParser.prototype.parseBinding = function () {
            var firstToken = asSymbolToken(this.peek());
            var name = firstToken[1];
            this.advance();
            assertReservedSymbolToken(this.peek(), 'in');
            this.advance();
            var value = this.parseExpression();
            var node = {
                t: AstNodeType.Binding,
                n: name,
                v: value,
                p: [],
                token: getTokenDebugData(firstToken) && firstToken,
            };
            return node;
        };
        AlgebraicParser.prototype.parseIfOrUnless = function (token) {
            var _a, _b;
            var isUnless = token[1] === 'unless';
            this.advance();
            var condition = this.parseExpression();
            assertReservedSymbolToken(this.peek(), 'then');
            this.advance();
            var thenExpressions = [];
            while (!this.isAtEnd()
                && !isReservedSymbolToken(this.peek(), 'else')
                && !isReservedSymbolToken(this.peek(), 'end')) {
                thenExpressions.push(this.parseExpression());
                if (isOperatorToken(this.peek(), ';')) {
                    this.advance();
                }
                else if (!isReservedSymbolToken(this.peek(), 'else') && !isReservedSymbolToken(this.peek(), 'end')) {
                    throw new LitsError('Expected ;', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                }
            }
            var thenExpression = thenExpressions.length === 1
                ? thenExpressions[0]
                : {
                    t: AstNodeType.SpecialExpression,
                    n: 'do',
                    p: thenExpressions,
                    token: getTokenDebugData(token) && token,
                };
            var elseExpression;
            if (isReservedSymbolToken(this.peek(), 'else')) {
                this.advance();
                var elseExpressions = [];
                while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
                    elseExpressions.push(this.parseExpression());
                    if (isOperatorToken(this.peek(), ';')) {
                        this.advance();
                    }
                    else if (!isReservedSymbolToken(this.peek(), 'end')) {
                        throw new LitsError('Expected ;', (_b = getTokenDebugData(this.peek())) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
                    }
                }
                elseExpression = elseExpressions.length === 1
                    ? elseExpressions[0]
                    : {
                        t: AstNodeType.SpecialExpression,
                        n: 'do',
                        p: elseExpressions,
                        token: getTokenDebugData(token) && token,
                    };
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            var params = [condition, thenExpression];
            if (elseExpression) {
                params.push(elseExpression);
            }
            return {
                t: AstNodeType.SpecialExpression,
                n: isUnless ? 'unless' : 'if',
                p: params,
                token: getTokenDebugData(token) && token,
            };
        };
        AlgebraicParser.prototype.parseCond = function (token) {
            var _a;
            this.advance();
            var params = [];
            while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
                assertReservedSymbolToken(this.peek(), 'case');
                this.advance();
                params.push(this.parseExpression());
                assertReservedSymbolToken(this.peek(), 'then');
                this.advance();
                var expressions = [];
                while (!this.isAtEnd()
                    && !isReservedSymbolToken(this.peek(), 'case')
                    && !isReservedSymbolToken(this.peek(), 'end')) {
                    expressions.push(this.parseExpression());
                    if (isOperatorToken(this.peek(), ';')) {
                        this.advance();
                    }
                    else if (!isReservedSymbolToken(this.peek(), 'case') && !isReservedSymbolToken(this.peek(), 'end')) {
                        throw new LitsError('Expected ;', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                    }
                }
                params.push(expressions.length === 1
                    ? expressions[0]
                    : {
                        t: AstNodeType.SpecialExpression,
                        n: 'do',
                        p: expressions,
                        token: getTokenDebugData(token) && token,
                    });
                if (isReservedSymbolToken(this.peek(), 'end')) {
                    break;
                }
                assertReservedSymbolToken(this.peek(), 'case');
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            return {
                t: AstNodeType.SpecialExpression,
                n: 'cond',
                p: params,
                token: getTokenDebugData(token) && token,
            };
        };
        AlgebraicParser.prototype.parseSwitch = function (token) {
            var _a;
            this.advance();
            var params = [this.parseExpression()];
            while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
                assertReservedSymbolToken(this.peek(), 'case');
                this.advance();
                params.push(this.parseExpression());
                assertReservedSymbolToken(this.peek(), 'then');
                this.advance();
                var expressions = [];
                while (!this.isAtEnd()
                    && !isReservedSymbolToken(this.peek(), 'case')
                    && !isReservedSymbolToken(this.peek(), 'end')) {
                    expressions.push(this.parseExpression());
                    if (isOperatorToken(this.peek(), ';')) {
                        this.advance();
                    }
                    else if (!isReservedSymbolToken(this.peek(), 'case') && !isReservedSymbolToken(this.peek(), 'end')) {
                        throw new LitsError('Expected ;', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                    }
                }
                params.push(expressions.length === 1
                    ? expressions[0]
                    : {
                        t: AstNodeType.SpecialExpression,
                        n: 'do',
                        p: expressions,
                        token: getTokenDebugData(token) && token,
                    });
                if (isReservedSymbolToken(this.peek(), 'end')) {
                    break;
                }
                assertReservedSymbolToken(this.peek(), 'case');
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            return {
                t: AstNodeType.SpecialExpression,
                n: 'switch',
                p: params,
                token: getTokenDebugData(token) && token,
            };
        };
        AlgebraicParser.prototype.parseFunction = function (token) {
            var _a;
            this.advance();
            var symbol = parseSymbol(this.tokenStream, this.parseState);
            var _b = this.parseFunctionArguments(), functionArguments = _b.functionArguments, arity = _b.arity;
            var body = [];
            while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
                body.push(this.parseExpression());
                if (isOperatorToken(this.peek(), ';')) {
                    this.advance();
                }
                else if (!isReservedSymbolToken(this.peek(), 'end')) {
                    throw new LitsError('Expected ;', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                }
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            assertOperatorToken(this.peek(), ';');
            return {
                t: AstNodeType.SpecialExpression,
                n: 'function',
                f: symbol,
                p: [],
                o: [{
                        as: functionArguments,
                        b: body,
                        a: arity,
                    }],
                token: getTokenDebugData(token) && token,
            };
        };
        AlgebraicParser.prototype.isAtEnd = function () {
            return this.parseState.position >= this.tokenStream.tokens.length;
        };
        AlgebraicParser.prototype.isAtExpressionEnd = function () {
            if (this.isAtEnd()) {
                return true;
            }
            var token = this.peek();
            if (isOperatorToken(token)) {
                return [';', ','].includes(token[1]);
            }
            if (isSymbolToken(token)) {
                return ['catch'].includes(token[1]);
            }
            if (isReservedSymbolToken(token)) {
                return ['else', 'when', 'while', 'then', 'end', 'case'].includes(token[1]);
            }
            return false;
        };
        AlgebraicParser.prototype.parseExport = function (token) {
            var _a, _b;
            this.advance();
            if (isSymbolToken(this.peek(), 'let')) {
                this.advance();
                var symbol = parseSymbol(this.tokenStream, this.parseState);
                assertOperatorToken(this.peek(), ':=');
                this.advance();
                var value = this.parseExpression();
                assertOperatorToken(this.peek(), ';');
                return {
                    t: AstNodeType.SpecialExpression,
                    n: 'def',
                    p: [symbol, value],
                    token: getTokenDebugData(symbol.token) && symbol.token,
                };
            }
            else if (isReservedSymbolToken(this.peek(), 'function')) {
                this.advance();
                var symbol = parseSymbol(this.tokenStream, this.parseState);
                var _c = this.parseFunctionArguments(), functionArguments = _c.functionArguments, arity = _c.arity;
                var body = [];
                while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
                    body.push(this.parseExpression());
                    if (isOperatorToken(this.peek(), ';')) {
                        this.advance();
                    }
                    else if (!isReservedSymbolToken(this.peek(), 'end')) {
                        throw new LitsError('Expected ;', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                    }
                }
                assertReservedSymbolToken(this.peek(), 'end');
                this.advance();
                return {
                    t: AstNodeType.SpecialExpression,
                    n: 'defn',
                    f: symbol,
                    p: [],
                    o: [{
                            as: functionArguments,
                            b: body,
                            a: arity,
                        }],
                    token: getTokenDebugData(token) && token,
                };
            }
            else {
                throw new LitsError('Expected let or function', (_b = getTokenDebugData(this.peek())) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
            }
        };
        return AlgebraicParser;
    }());

    function parsePolishToken(_tokenStream, _parseState) {
        throw new Error('Should not be called');
    }

    function parse$1(tokenStream) {
        tokenStream = minifyTokenStream(tokenStream, { removeWhiteSpace: true });
        var ast = {
            b: [],
            hasDebugData: tokenStream.hasDebugData,
        };
        var parseState = {
            position: 0,
            parseToken: parseToken,
        };
        var algebraicParser = new AlgebraicParser(tokenStream, parseState);
        ast.b = algebraicParser.parse();
        return ast;
    }
    function parseToken(tokenStream, parseState) {
        return parsePolishToken();
    }

    var illegalSymbolCharacters = [
        '(',
        ')',
        '[',
        ']',
        '{',
        '}',
        '\'',
        '"',
        '`',
        ',',
        '.',
        ';',
        ' ',
        '\n',
        '\r',
        '\t',
    ];
    var illegalFirstSymbolCharacters = __spreadArray([
        '0',
        '1',
        '2',
        '3',
        '4',
        '5',
        '6',
        '7',
        '8',
        '9'
    ], __read(illegalSymbolCharacters), false);
    var illegalSymbolCharacterSet = new Set(illegalSymbolCharacters);
    var illegalFirstSymbolCharacterSet = new Set(illegalFirstSymbolCharacters);
    var whitespaceRegExp = /\s/;
    var NO_MATCH = [0];
    var tokenizeLParen = function (input, position) {
        return tokenizeToken('LParen', '(', input, position);
    };
    var tokenizeRParen = function (input, position) {
        return tokenizeToken('RParen', ')', input, position);
    };
    var tokenizeLBracket = function (input, position) {
        return tokenizeToken('LBracket', '[', input, position);
    };
    var tokenizeRBracket = function (input, position) {
        return tokenizeToken('RBracket', ']', input, position);
    };
    var tokenizeLBrace = function (input, position) {
        return tokenizeToken('LBrace', '{', input, position);
    };
    var tokenizeRBrace = function (input, position) {
        return tokenizeToken('RBrace', '}', input, position);
    };
    var tokenizeString = function (input, position) {
        if (input[position] !== '"')
            return NO_MATCH;
        var value = '"';
        var length = 1;
        var char = input[position + length];
        var escaping = false;
        while (char !== '"' || escaping) {
            if (char === undefined)
                throw new LitsError("Unclosed string at position ".concat(position, "."), undefined);
            length += 1;
            if (escaping) {
                escaping = false;
                value += char;
            }
            else {
                if (char === '\\') {
                    escaping = true;
                }
                value += char;
            }
            char = input[position + length];
        }
        value += '"'; // closing quote
        return [length + 1, ['String', value]];
    };
    var tokenizeRegexpShorthand = function (input, position) {
        if (input[position] !== '#')
            return NO_MATCH;
        var _a = __read(tokenizeString(input, position + 1), 2), stringLength = _a[0], token = _a[1];
        if (!token)
            return NO_MATCH;
        position += stringLength + 1;
        var length = stringLength + 1;
        var options = '';
        while (input[position] === 'g' || input[position] === 'i') {
            if (options.includes(input[position])) {
                throw new LitsError("Duplicated regexp option \"".concat(input[position], "\" at position ").concat(position, "."), undefined);
            }
            options += input[position];
            length += 1;
            position += 1;
        }
        return [length, ['RegexpShorthand', "#".concat(token[1]).concat(options)]];
    };
    function tokenizeToken(type, value, input, position) {
        if (value === input.slice(position, position + value.length))
            return [value.length, [type, value]];
        else
            return NO_MATCH;
    }
    var tokenizeWhitespace = function (input, position) {
        var char = input[position];
        if (!char || !whitespaceRegExp.test(char)) {
            return NO_MATCH;
        }
        var value = char;
        position += 1;
        char = input[position];
        while (char && whitespaceRegExp.test(char)) {
            value += char;
            position += 1;
            char = input[position];
        }
        return [value.length, ['Whitespace', value]];
    };
    var decimalNumberRegExp = /\d/;
    var octalNumberRegExp = /[0-7]/;
    var hexNumberRegExp = /[0-9a-f]/i;
    var binaryNumberRegExp = /[01]/;
    var tokenizeNumber = function (input, position) {
        var i;
        var negate = input[position] === '-';
        var plusPrefix = input[position] === '+';
        var start = negate || plusPrefix ? position + 1 : position;
        var hasDecimalPoint = false;
        var hasExponent = false;
        for (i = start; i < input.length; i += 1) {
            var char = input[i];
            if (char === '_') {
                if (!decimalNumberRegExp.test(input[i - 1]) || !decimalNumberRegExp.test(input[i + 1])) {
                    return NO_MATCH;
                }
            }
            else if (char === '.') {
                if (i === start || hasDecimalPoint || hasExponent) {
                    return NO_MATCH;
                }
                hasDecimalPoint = true;
            }
            else if (char === 'e' || char === 'E') {
                if (i === start || hasExponent) {
                    return NO_MATCH;
                }
                if (input[i - 1] === '.' || input[i - 1] === '+' || input[i - 1] === '-') {
                    return NO_MATCH;
                }
                if (input[i + 1] === '+' || input[i + 1] === '-') {
                    i += 1;
                }
                hasExponent = true;
            }
            else if (!decimalNumberRegExp.test(char)) {
                break;
            }
        }
        if ((negate || plusPrefix) && i === start) {
            return NO_MATCH;
        }
        var length = i - position;
        if (length === 0) {
            return NO_MATCH;
        }
        return [length, ['Number', input.substring(position, i)]];
    };
    var tokenizeBasePrefixedNumber = function (input, position) {
        if (input[position] !== '0') {
            return NO_MATCH;
        }
        var baseChar = input[position + 1];
        var type = baseChar === 'b' || baseChar === 'B'
            ? 'binary'
            : baseChar === 'o' || baseChar === 'O'
                ? 'octal'
                : baseChar === 'x' || baseChar === 'X'
                    ? 'hex'
                    : null;
        if (type === null) {
            return NO_MATCH;
        }
        var i;
        for (i = position + 2; i < input.length; i += 1) {
            var char = input[i];
            if (type === 'binary' && !binaryNumberRegExp.test(char)) {
                break;
            }
            if (type === 'octal' && !octalNumberRegExp.test(char)) {
                break;
            }
            if (type === 'hex' && !hexNumberRegExp.test(char)) {
                break;
            }
        }
        var length = i - position;
        if (length <= 2) {
            return NO_MATCH;
        }
        return [length, ['BasePrefixedNumber', input.substring(position, i)]];
    };
    var tokenizeSymbol = function (input, position) {
        var value = input[position];
        if (!value) {
            return NO_MATCH;
        }
        if (value === '\'') {
            var length_1 = 1;
            var char = input[position + length_1];
            var escaping = false;
            while (char !== '\'' || escaping) {
                if (char === undefined)
                    throw new LitsError("Unclosed string at position ".concat(position, "."), undefined);
                length_1 += 1;
                if (escaping) {
                    escaping = false;
                    value += char;
                }
                else {
                    if (char === '\\') {
                        escaping = true;
                    }
                    value += char;
                }
                char = input[position + length_1];
            }
            value += '\''; // closing quote
            return [length_1 + 1, ['Symbol', value]];
        }
        if (!illegalFirstSymbolCharacterSet.has(value)) {
            var initialPosition = position;
            position += 1;
            var char = input[position];
            while (char && !illegalSymbolCharacterSet.has(char)) {
                value += char;
                position += 1;
                char = input[position];
            }
            return [position - initialPosition, ['Symbol', value]];
        }
        return NO_MATCH;
    };
    var tokenizeReservedSymbolToken = function (input, position) {
        var symbolMeta = tokenizeSymbol(input, position);
        if (symbolMeta[0] === 0 || !symbolMeta[1]) {
            return NO_MATCH;
        }
        var symbolName = symbolMeta[1][1];
        symbolName = symbolName.startsWith('\'') ? symbolName.slice(1, symbolName.length - 1) : symbolName;
        var info = algebraicReservedSymbolRecord[symbolName];
        if (info === undefined) {
            return NO_MATCH;
        }
        return [symbolMeta[0], ['ReservedSymbol', symbolName]];
    };
    var tokenizeOperator = function (input, position) {
        var _a;
        var threeChars = input.slice(position, position + 3);
        if (position + 2 < input.length && isSymbolicOperator(threeChars)) {
            return [3, ['Operator', threeChars]];
        }
        var twoChars = input.slice(position, position + 2);
        if (position + 1 < input.length && isSymbolicOperator(twoChars)) {
            return [2, ['Operator', twoChars]];
        }
        var oneChar = (_a = input[position]) !== null && _a !== void 0 ? _a : '';
        if (isSymbolicOperator(oneChar)) {
            return [1, ['Operator', oneChar]];
        }
        return NO_MATCH;
    };
    var tokenizeMultiLineComment = function (input, position) {
        if (input[position] === '/' && input[position + 1] === '*') {
            var length_2 = 2;
            var value = '/*';
            while ((input[position + length_2] !== '*' || input[position + length_2 + 1] !== '/') && position + length_2 + 1 < input.length) {
                value += input[position + length_2];
                length_2 += 1;
            }
            if (position + length_2 + 1 >= input.length) {
                throw new LitsError('Comment not closed', undefined);
            }
            value += '*/';
            length_2 += 2;
            return [length_2, ['MultiLineComment', value]];
        }
        return NO_MATCH;
    };
    var tokenizeSingleLineComment = function (input, position) {
        if (input[position] === '/' && input[position + 1] === '/') {
            var length_3 = 2;
            var value = '//';
            while (input[position + length_3] !== '\n' && position + length_3 < input.length) {
                value += input[position + length_3];
                length_3 += 1;
            }
            return [length_3, ['SingleLineComment', value]];
        }
        return NO_MATCH;
    };
    // All tokenizers, order matters!
    var tokenizers = [
        tokenizeWhitespace,
        tokenizeMultiLineComment,
        tokenizeSingleLineComment,
        tokenizeReservedSymbolToken,
        tokenizeLParen,
        tokenizeRParen,
        tokenizeLBracket,
        tokenizeRBracket,
        tokenizeLBrace,
        tokenizeRBrace,
        tokenizeString,
        tokenizeRegexpShorthand,
        tokenizeBasePrefixedNumber,
        tokenizeNumber,
        tokenizeOperator,
        tokenizeSymbol,
    ];

    function tokenize$1(input, debug, filePath) {
        var position = 0;
        var tokenStream = {
            tokens: [],
            filePath: filePath,
            hasDebugData: debug,
        };
        while (position < input.length) {
            var tokenDescriptor = getCurrentToken(input, position);
            var debugData = debug
                ? {
                    sourceCodeInfo: createSourceCodeInfo(input, position, filePath),
                }
                : undefined;
            if (!tokenDescriptor) {
                throw new LitsError("Unrecognized character '".concat(input[position], "'."), debugData === null || debugData === void 0 ? void 0 : debugData.sourceCodeInfo);
            }
            var _a = __read(tokenDescriptor, 2), count = _a[0], token = _a[1];
            position += count;
            if (token) {
                if (debugData) {
                    addTokenDebugData(token, debugData);
                }
                tokenStream.tokens.push(token);
            }
        }
        return tokenStream;
    }
    function getSourceCodeLine(input, lineNbr) {
        return input.split(/\r\n|\r|\n/)[lineNbr];
    }
    function createSourceCodeInfo(input, position, filePath) {
        var lines = input.substring(0, position + 1).split(/\r\n|\r|\n/);
        var lastLine = lines[lines.length - 1];
        var code = getSourceCodeLine(input, lines.length - 1);
        var line = lines.length;
        var column = lastLine.length;
        return {
            code: code,
            position: {
                line: line,
                column: column,
            },
            filePath: filePath,
        };
    }
    function getCurrentToken(input, position) {
        var e_1, _a;
        var initialPosition = position;
        var tryNext = true;
        while (tryNext) {
            if (position >= input.length) {
                return [position - initialPosition, undefined];
            }
            tryNext = false;
            try {
                for (var tokenizers_1 = (e_1 = void 0, __values(tokenizers)), tokenizers_1_1 = tokenizers_1.next(); !tokenizers_1_1.done; tokenizers_1_1 = tokenizers_1.next()) {
                    var tokenizer = tokenizers_1_1.value;
                    var _b = __read(tokenizer(input, position), 2), nbrOfCharacters = _b[0], token = _b[1];
                    position += nbrOfCharacters;
                    if (nbrOfCharacters === 0) {
                        continue;
                    }
                    if (!token) {
                        tryNext = true;
                        break;
                    }
                    return [position - initialPosition, token];
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (tokenizers_1_1 && !tokenizers_1_1.done && (_a = tokenizers_1.return)) _a.call(tokenizers_1);
                }
                finally { if (e_1) throw e_1.error; }
            }
        }
        return null;
    }

    function transformSymbolTokens(tokenStram, transformer) {
        return __assign(__assign({}, tokenStram), { tokens: tokenStram.tokens.map(function (token) { return isSymbolToken(token)
                ? [token[0], transformer(token[1])]
                : token; }) });
    }

    function untokenize(tokenStream) {
        return tokenStream.tokens.reduce(function (acc, token) {
            return "".concat(acc).concat(token[1]);
        }, '');
    }

    var Cache = /** @class */ (function () {
        function Cache(maxSize) {
            this.cache = {};
            this.firstEntry = undefined;
            this.lastEntry = undefined;
            this._size = 0;
            this.maxSize = maxSize === null ? null : toNonNegativeInteger(maxSize);
            if (typeof this.maxSize === 'number' && this.maxSize < 1)
                throw new Error("1 is the minimum maxSize, got ".concat(valueToString(maxSize)));
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
            if (this.has(key))
                throw new Error("AstCache - key already present: ".concat(key));
            var newEntry = { value: value, nextEntry: undefined, key: key };
            this.cache[key] = newEntry;
            this._size += 1;
            if (this.lastEntry)
                this.lastEntry.nextEntry = newEntry;
            this.lastEntry = newEntry;
            if (!this.firstEntry)
                this.firstEntry = this.lastEntry;
            while (this.maxSize !== null && this.size > this.maxSize)
                this.dropFirstEntry();
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
            if (this.astCacheSize) {
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
            var ast = this.generateAst(program, params);
            return this.evaluate(ast, params);
        };
        Lits.prototype.context = function (program, params) {
            if (params === void 0) { params = {}; }
            var contextStack = createContextStack(params);
            var ast = this.generateAst(program, params);
            evaluate(ast, contextStack);
            return contextStack.globalContext;
        };
        Lits.prototype.getUndefinedSymbols = function (programOrAst, params) {
            if (params === void 0) { params = {}; }
            var ast = typeof programOrAst === 'string' ? this.generateAst(programOrAst, params) : programOrAst;
            var contextStack = createContextStack(params);
            return getUndefinedSymbols(ast, contextStack, builtin);
        };
        Lits.prototype.tokenize = function (program, tokenizeParams) {
            if (tokenizeParams === void 0) { tokenizeParams = {}; }
            var debug = this.debug;
            var tokenStream = tokenize$1(program, debug, tokenizeParams.filePath);
            return tokenizeParams.minify ? minifyTokenStream(tokenStream, { removeWhiteSpace: false }) : tokenStream;
        };
        Lits.prototype.parse = function (tokenStream) {
            return parse$1(tokenStream);
        };
        Lits.prototype.evaluate = function (ast, params) {
            var contextStack = createContextStack(params);
            return evaluate(ast, contextStack);
        };
        Lits.prototype.transformSymbols = function (tokenStream, transformer) {
            return transformSymbolTokens(tokenStream, transformer);
        };
        Lits.prototype.untokenize = function (tokenStream) {
            return untokenize(tokenStream);
        };
        Lits.prototype.apply = function (fn, fnParams, params) {
            var _a;
            if (params === void 0) { params = {}; }
            var fnName = 'FN_2eb7b316_471c_5bfa_90cb_d3dfd9164a59';
            var program = this.generateApplyFunctionCall(fnName, fnParams);
            var ast = this.generateAst(program, params);
            var hostValues = fnParams.reduce(function (result, param, index) {
                result["".concat(fnName, "_").concat(index)] = param;
                return result;
            }, (_a = {}, _a[fnName] = fn, _a));
            params.values = __assign(__assign({}, params.values), hostValues);
            return this.evaluate(ast, params);
        };
        Lits.prototype.generateApplyFunctionCall = function (fnName, fnParams) {
            var paramsString = fnParams
                .map(function (_, index) {
                return "".concat(fnName, "_").concat(index);
            })
                .join(', ');
            return "".concat(fnName, "(").concat(paramsString, ")");
        };
        Lits.prototype.generateAst = function (program, params) {
            var _a;
            if (this.astCache) {
                var cachedAst = this.astCache.get(program);
                if (cachedAst)
                    return cachedAst;
            }
            var tokenStream = this.tokenize(program, {
                filePath: params.filePath,
            });
            var ast = this.parse(tokenStream);
            (_a = this.astCache) === null || _a === void 0 ? void 0 : _a.set(program, ast);
            return ast;
        };
        return Lits;
    }());

    var getLits = (function () {
        var lits = new Lits({ debug: true });
        var litsNoDebug = new Lits({ debug: false });
        return function (forceDebug) { return forceDebug || getState('debug') ? lits : litsNoDebug; };
    })();
    var elements = {
        wrapper: document.getElementById('wrapper'),
        playground: document.getElementById('playground'),
        sidebar: document.getElementById('sidebar'),
        mainPanel: document.getElementById('main-panel'),
        contextPanel: document.getElementById('context-panel'),
        litsPanel: document.getElementById('lits-panel'),
        outputPanel: document.getElementById('output-panel'),
        moreMenu: document.getElementById('more-menu'),
        addContextMenu: document.getElementById('add-context-menu'),
        newContextName: document.getElementById('new-context-name'),
        newContextValue: document.getElementById('new-context-value'),
        newContextError: document.getElementById('new-context-error'),
        contextTextArea: document.getElementById('context-textarea'),
        outputResult: document.getElementById('output-result'),
        litsTextArea: document.getElementById('lits-textarea'),
        resizePlayground: document.getElementById('resize-playground'),
        resizeDevider1: document.getElementById('resize-divider-1'),
        resizeDevider2: document.getElementById('resize-divider-2'),
        toggleDebugMenuLabel: document.getElementById('toggle-debug-menu-label'),
        litsPanelDebugInfo: document.getElementById('lits-panel-debug-info'),
        contextUndoButton: document.getElementById('context-undo-button'),
        contextRedoButton: document.getElementById('context-redo-button'),
        litsCodeUndoButton: document.getElementById('lits-code-undo-button'),
        litsCodeRedoButton: document.getElementById('lits-code-redo-button'),
        contextTitle: document.getElementById('context-title'),
        litsCodeTitle: document.getElementById('lits-code-title'),
        litsCodeTitleString: document.getElementById('lits-code-title-string'),
    };
    var moveParams = null;
    var ignoreSelectionChange = false;
    function calculateDimensions() {
        return {
            windowHeight: window.innerHeight,
            windowWidth: window.innerWidth,
        };
    }
    function openMoreMenu() {
        elements.moreMenu.style.display = 'block';
    }
    function closeMoreMenu() {
        elements.moreMenu.style.display = 'none';
    }
    function openAddContextMenu() {
        elements.newContextName.value = getState('new-context-name');
        elements.newContextValue.value = getState('new-context-value');
        elements.addContextMenu.style.display = 'block';
        elements.newContextName.focus();
    }
    function closeAddContextMenu() {
        elements.addContextMenu.style.display = 'none';
        elements.newContextError.style.display = 'none';
        elements.newContextError.textContent = '';
        elements.newContextName.value = '';
        elements.newContextValue.value = '';
    }
    function share() {
        addOutputSeparator();
        appendOutput('Sharable link:', 'comment');
        var href = "".concat(location.origin).concat(location.pathname, "?state=").concat(encodeState());
        var a = document.createElement('a');
        a.textContent = href;
        a.className = 'share-link';
        a.href = href;
        addOutputElement(a);
    }
    function onDocumentClick(event) {
        var target = event.target;
        if (!(target === null || target === void 0 ? void 0 : target.closest('#more-menu')) && elements.moreMenu.style.display === 'block')
            closeMoreMenu();
        if (!(target === null || target === void 0 ? void 0 : target.closest('#add-context-menu')) && elements.addContextMenu.style.display === 'block')
            closeAddContextMenu();
    }
    var layout = throttle(function () {
        var _a = calculateDimensions(), windowWidth = _a.windowWidth, windowHeight = _a.windowHeight;
        var playgroundHeight = Math.min(getState('playground-height'), windowHeight);
        var contextPanelWidth = (windowWidth * getState('resize-divider-1-percent')) / 100;
        var outputPanelWidth = (windowWidth * (100 - getState('resize-divider-2-percent'))) / 100;
        var litsPanelWidth = windowWidth - contextPanelWidth - outputPanelWidth;
        elements.playground.style.height = "".concat(playgroundHeight, "px");
        elements.contextPanel.style.width = "".concat(contextPanelWidth, "px");
        elements.litsPanel.style.width = "".concat(litsPanelWidth, "px");
        elements.outputPanel.style.width = "".concat(outputPanelWidth, "px");
        elements.sidebar.style.bottom = "".concat(playgroundHeight, "px");
        elements.mainPanel.style.bottom = "".concat(playgroundHeight, "px");
        elements.wrapper.style.display = 'block';
    });
    var undoContextHistory = throttle(function () {
        ignoreSelectionChange = true;
        if (undoContext()) {
            applyState();
            focusContext();
        }
        setTimeout(function () { return ignoreSelectionChange = false; });
    });
    var redoContextHistory = throttle(function () {
        ignoreSelectionChange = true;
        if (redoContext()) {
            applyState();
            focusContext();
        }
        setTimeout(function () { return ignoreSelectionChange = false; });
    });
    var undoLitsCodeHistory = throttle(function () {
        ignoreSelectionChange = true;
        if (undoLitsCode()) {
            applyState();
            focusLitsCode();
        }
        setTimeout(function () { return ignoreSelectionChange = false; });
    });
    var redoLitsCodeHistory = throttle(function () {
        ignoreSelectionChange = true;
        if (redoLitsCode()) {
            applyState();
            focusLitsCode();
        }
        setTimeout(function () { return ignoreSelectionChange = false; });
    });
    function resetPlayground() {
        clearAllStates();
        resetContext();
        resetLitsCode();
        resetOutput();
        Search.closeSearch();
        Search.clearSearch();
        layout();
        updateCSS();
    }
    function resetContext() {
        elements.contextTextArea.value = '';
        clearState('context', 'context-scroll-top', 'context-selection-start', 'context-selection-end');
        focusContext();
    }
    function setContext(value, pushToHistory, scroll) {
        elements.contextTextArea.value = value;
        if (pushToHistory) {
            saveState({
                'context': value,
                'context-selection-start': elements.contextTextArea.selectionStart,
                'context-selection-end': elements.contextTextArea.selectionEnd,
            }, true);
        }
        else {
            saveState({ context: value }, false);
        }
        if (scroll === 'top')
            elements.contextTextArea.scrollTo(0, 0);
        else if (scroll === 'bottom')
            elements.contextTextArea.scrollTo({ top: elements.contextTextArea.scrollHeight, behavior: 'smooth' });
    }
    function getParsedContext() {
        try {
            return asUnknownRecord(JSON.parse(getState('context')));
        }
        catch (e) {
            return {};
        }
    }
    function addContextEntry() {
        var name = elements.newContextName.value;
        if (name === '') {
            elements.newContextError.textContent = 'Name is required';
            elements.newContextError.style.display = 'block';
            elements.newContextName.focus();
            return;
        }
        var value = elements.newContextValue.value;
        try {
            var parsedValue = JSON.parse(value);
            var context = getParsedContext();
            var values = Object.assign({}, context.values);
            values[name] = parsedValue;
            context.values = values;
            setContext(JSON.stringify(context, null, 2), true);
            closeAddContextMenu();
        }
        catch (e) {
            elements.newContextError.textContent = 'Invalid JSON';
            elements.newContextError.style.display = 'block';
            elements.newContextValue.focus();
        }
        clearState('new-context-name');
        clearState('new-context-value');
    }
    function addSampleContext() {
        var context = getParsedContext();
        var values = {
            'a-number': 42,
            'a-string': 'foo bar',
            'an-array': ['foo', 'bar', 1, 2, true, false, null],
            'an-object': {
                name: 'John Doe',
                age: 42,
                married: true,
                children: ['Alice', 'Bob'],
                address: {
                    street: '123 Main St',
                    city: 'Springfield',
                    state: 'IL',
                    zip: '62701',
                },
            },
        };
        var jsFunctions = {
            'prompt!': '(title) => prompt(title)',
        };
        context.values = Object.assign(values, context.values);
        context.jsFunctions = Object.assign(jsFunctions, context.jsFunctions);
        setContext(JSON.stringify(context, null, 2), true);
    }
    function resetLitsCode() {
        elements.litsTextArea.value = '';
        clearState('lits-code', 'lits-code-scroll-top', 'lits-code-selection-start', 'lits-code-selection-end');
        focusLitsCode();
    }
    function setLitsCode(value, pushToHistory, scroll) {
        elements.litsTextArea.value = value;
        if (pushToHistory) {
            saveState({
                'lits-code': value,
                'lits-code-selection-start': elements.litsTextArea.selectionStart,
                'lits-code-selection-end': elements.litsTextArea.selectionEnd,
            }, true);
        }
        else {
            saveState({ 'lits-code': value }, false);
        }
        if (scroll === 'top')
            elements.litsTextArea.scrollTo(0, 0);
        else if (scroll === 'bottom')
            elements.litsTextArea.scrollTo({ top: elements.litsTextArea.scrollHeight, behavior: 'smooth' });
    }
    function appendLitsCode(value) {
        var oldContent = getState('lits-code').trimEnd();
        var newContent = oldContent ? "".concat(oldContent, "\n\n").concat(value) : value.trim();
        setLitsCode(newContent, true, 'bottom');
    }
    function resetOutput() {
        elements.outputResult.innerHTML = '';
        clearState('output');
    }
    function hasOutput() {
        return getState('output').trim() !== '';
    }
    function setOutput(value, pushToHistory) {
        elements.outputResult.innerHTML = value;
        saveState({ output: value }, pushToHistory);
    }
    function appendOutput(output, className) {
        var outputElement = document.createElement('span');
        outputElement.className = className;
        outputElement.textContent = "".concat(output);
        addOutputElement(outputElement);
    }
    function addOutputSeparator() {
        if (hasOutput()) {
            var separator = document.createElement('div');
            separator.className = 'separator';
            addOutputElement(separator);
        }
    }
    function addOutputElement(element) {
        elements.outputResult.appendChild(element);
        elements.outputResult.scrollTop = elements.outputResult.scrollHeight;
        saveState({ output: elements.outputResult.innerHTML });
    }
    window.onload = function () {
        elements.contextUndoButton.classList.add('disabled');
        elements.contextRedoButton.classList.add('disabled');
        elements.litsCodeUndoButton.classList.add('disabled');
        elements.litsCodeRedoButton.classList.add('disabled');
        setContextHistoryListener(function (status) {
            if (status.canUndo) {
                elements.contextUndoButton.classList.remove('disabled');
            }
            else {
                elements.contextUndoButton.classList.add('disabled');
            }
            if (status.canRedo) {
                elements.contextRedoButton.classList.remove('disabled');
            }
            else {
                elements.contextRedoButton.classList.add('disabled');
            }
        });
        setLitsCodeHistoryListener(function (status) {
            if (status.canUndo) {
                elements.litsCodeUndoButton.classList.remove('disabled');
            }
            else {
                elements.litsCodeUndoButton.classList.add('disabled');
            }
            if (status.canRedo) {
                elements.litsCodeRedoButton.classList.remove('disabled');
            }
            else {
                elements.litsCodeRedoButton.classList.add('disabled');
            }
        });
        document.addEventListener('click', onDocumentClick, true);
        elements.resizePlayground.onmousedown = function (event) {
            moveParams = {
                id: 'playground',
                startMoveY: event.clientY,
                heightBeforeMove: getState('playground-height'),
            };
        };
        elements.resizeDevider1.onmousedown = function (event) {
            moveParams = {
                id: 'resize-divider-1',
                startMoveX: event.clientX,
                percentBeforeMove: getState('resize-divider-1-percent'),
            };
        };
        elements.resizeDevider2.onmousedown = function (event) {
            moveParams = {
                id: 'resize-divider-2',
                startMoveX: event.clientX,
                percentBeforeMove: getState('resize-divider-2-percent'),
            };
        };
        window.onresize = layout;
        window.onmouseup = function () {
            document.body.classList.remove('no-select');
            moveParams = null;
        };
        window.onmousemove = function (event) {
            var _a = calculateDimensions(), windowHeight = _a.windowHeight, windowWidth = _a.windowWidth;
            if (moveParams === null)
                return;
            document.body.classList.add('no-select');
            if (moveParams.id === 'playground') {
                var playgroundHeight = moveParams.heightBeforeMove + moveParams.startMoveY - event.clientY;
                if (playgroundHeight < 30)
                    playgroundHeight = 30;
                if (playgroundHeight > windowHeight)
                    playgroundHeight = windowHeight;
                saveState({ 'playground-height': playgroundHeight });
                layout();
            }
            else if (moveParams.id === 'resize-divider-1') {
                var resizeDivider1XPercent = moveParams.percentBeforeMove + ((event.clientX - moveParams.startMoveX) / windowWidth) * 100;
                if (resizeDivider1XPercent < 10)
                    resizeDivider1XPercent = 10;
                if (resizeDivider1XPercent > getState('resize-divider-2-percent') - 10)
                    resizeDivider1XPercent = getState('resize-divider-2-percent') - 10;
                saveState({ 'resize-divider-1-percent': resizeDivider1XPercent });
                layout();
            }
            else if (moveParams.id === 'resize-divider-2') {
                var resizeDivider2XPercent = moveParams.percentBeforeMove + ((event.clientX - moveParams.startMoveX) / windowWidth) * 100;
                if (resizeDivider2XPercent < getState('resize-divider-1-percent') + 10)
                    resizeDivider2XPercent = getState('resize-divider-1-percent') + 10;
                if (resizeDivider2XPercent > 90)
                    resizeDivider2XPercent = 90;
                saveState({ 'resize-divider-2-percent': resizeDivider2XPercent });
                layout();
            }
        };
        window.addEventListener('keydown', function (evt) {
            if (Search.handleKeyDown(evt))
                return;
            if (evt.ctrlKey) {
                switch (evt.key) {
                    case 'r':
                        evt.preventDefault();
                        run();
                        break;
                    case 'a':
                        evt.preventDefault();
                        analyze();
                        break;
                    case 't':
                        evt.preventDefault();
                        tokenize();
                        break;
                    case 'p':
                        evt.preventDefault();
                        parse();
                        break;
                    case 'f':
                        evt.preventDefault();
                        format();
                        break;
                    case 'd':
                        evt.preventDefault();
                        toggleDebug();
                        break;
                    case '1':
                        evt.preventDefault();
                        focusContext();
                        break;
                    case '2':
                        evt.preventDefault();
                        focusLitsCode();
                        break;
                }
            }
            if (evt.key === 'Escape') {
                closeMoreMenu();
                closeAddContextMenu();
                evt.preventDefault();
            }
            if (((isMac() && evt.metaKey) || (!isMac && evt.ctrlKey)) && !evt.shiftKey && evt.key === 'z') {
                evt.preventDefault();
                if (document.activeElement === elements.contextTextArea)
                    undoContextHistory();
                else if (document.activeElement === elements.litsTextArea)
                    undoLitsCodeHistory();
            }
            if (((isMac() && evt.metaKey) || (!isMac && evt.ctrlKey)) && evt.shiftKey && evt.key === 'z') {
                evt.preventDefault();
                if (document.activeElement === elements.contextTextArea)
                    redoContextHistory();
                else if (document.activeElement === elements.litsTextArea)
                    redoLitsCodeHistory();
            }
        });
        elements.contextTextArea.addEventListener('keydown', function (evt) {
            keydownHandler(evt, function () { return setContext(elements.contextTextArea.value, true); });
        });
        elements.contextTextArea.addEventListener('input', function () {
            setContext(elements.contextTextArea.value, true);
        });
        elements.contextTextArea.addEventListener('scroll', function () {
            saveState({ 'context-scroll-top': elements.contextTextArea.scrollTop });
        });
        elements.contextTextArea.addEventListener('selectionchange', function () {
            if (!ignoreSelectionChange) {
                saveState({
                    'context-selection-start': elements.contextTextArea.selectionStart,
                    'context-selection-end': elements.contextTextArea.selectionEnd,
                });
            }
        });
        elements.contextTextArea.addEventListener('focusin', function () {
            saveState({ 'focused-panel': 'context' });
            updateCSS();
        });
        elements.contextTextArea.addEventListener('focusout', function () {
            saveState({ 'focused-panel': null });
            updateCSS();
        });
        elements.litsTextArea.addEventListener('keydown', function (evt) {
            keydownHandler(evt, function () { return setLitsCode(elements.litsTextArea.value, true); });
        });
        elements.litsTextArea.addEventListener('input', function () {
            setLitsCode(elements.litsTextArea.value, true);
        });
        elements.litsTextArea.addEventListener('scroll', function () {
            saveState({ 'lits-code-scroll-top': elements.litsTextArea.scrollTop });
        });
        elements.litsTextArea.addEventListener('selectionchange', function () {
            if (!ignoreSelectionChange) {
                saveState({
                    'lits-code-selection-start': elements.litsTextArea.selectionStart,
                    'lits-code-selection-end': elements.litsTextArea.selectionEnd,
                });
            }
        });
        elements.litsTextArea.addEventListener('focusin', function () {
            saveState({ 'focused-panel': 'lits-code' });
            updateCSS();
        });
        elements.litsTextArea.addEventListener('focusout', function () {
            saveState({ 'focused-panel': null });
            updateCSS();
        });
        elements.outputResult.addEventListener('scroll', function () {
            saveState({ 'output-scroll-top': elements.outputResult.scrollTop });
        });
        elements.newContextName.addEventListener('input', function () {
            saveState({ 'new-context-name': elements.newContextName.value });
        });
        elements.newContextValue.addEventListener('input', function () {
            saveState({ 'new-context-value': elements.newContextValue.value });
        });
        applyState(true);
        var id = location.hash.substring(1) || 'index';
        showPage(id, 'instant', 'replace');
        Search.onClose(function () {
            applyState();
        });
    };
    function getDataFromUrl() {
        var urlParams = new URLSearchParams(window.location.search);
        var urlState = urlParams.get('state');
        if (urlState) {
            addOutputSeparator();
            if (applyEncodedState(urlState))
                appendOutput("Data parsed from url parameter state: ".concat(urlState), 'comment');
            else
                appendOutput("Invalid url parameter state: ".concat(urlState), 'error');
            urlParams.delete('state');
            history.replaceState(null, '', "".concat(location.pathname).concat(urlParams.toString() ? '?' : '').concat(urlParams.toString()));
        }
    }
    function keydownHandler(evt, onChange) {
        if (['Tab', 'Backspace', 'Enter', 'Delete'].includes(evt.key)) {
            var target = evt.target;
            var start = target.selectionStart;
            var end = target.selectionEnd;
            var indexOfReturn = target.value.lastIndexOf('\n', start - 1);
            var rowLength = start - indexOfReturn - 1;
            var onTabStop = rowLength % 2 === 0;
            switch (evt.key) {
                case 'Tab':
                    evt.preventDefault();
                    if (!evt.shiftKey) {
                        target.value = target.value.substring(0, start) + (onTabStop ? '  ' : ' ') + target.value.substring(end);
                        target.selectionStart = target.selectionEnd = start + (onTabStop ? 2 : 1);
                        onChange();
                    }
                    break;
                case 'Backspace':
                    if (onTabStop && start === end && target.value.substring(start - 2, start + 2) === '  ') {
                        evt.preventDefault();
                        target.value = target.value.substring(0, start - 2) + target.value.substring(end);
                        target.selectionStart = target.selectionEnd = start - 2;
                        onChange();
                    }
                    break;
                case 'Enter': {
                    evt.preventDefault();
                    // eslint-disable-next-line regexp/optimal-quantifier-concatenation
                    var spaceCount = target.value.substring(indexOfReturn + 1, start).replace(/^( *).*/, '$1').length;
                    target.value = "".concat(target.value.substring(0, start), "\n").concat(' '.repeat(spaceCount)).concat(target.value.substring(end));
                    target.selectionStart = target.selectionEnd = start + 1 + spaceCount;
                    onChange();
                    break;
                }
                case 'Delete':
                    if (onTabStop && start === end && target.value.substring(start, start + 2) === '  ') {
                        evt.preventDefault();
                        target.value = target.value.substring(0, start) + target.value.substring(end + 2);
                        target.selectionStart = target.selectionEnd = start;
                        onChange();
                    }
                    break;
            }
        }
    }
    window.addEventListener('popstate', function () {
        var id = location.hash.substring(1) || 'index';
        showPage(id, 'instant', 'none');
    });
    function truncateCode(code, count) {
        if (count === void 0) { count = 1000; }
        var oneLiner = getLits().tokenize(code, { minify: true }).tokens.map(function (t) { return t[0] === 'Whitespace' ? ' ' : t[1]; }).join('').trim();
        if (oneLiner.length <= count)
            return oneLiner;
        else
            return "".concat(oneLiner.substring(0, count - 3), "...");
    }
    function run() {
        addOutputSeparator();
        var selectedCode = getSelectedLitsCode();
        var code = selectedCode.code || getState('lits-code');
        var title = selectedCode.code ? 'Run selection' : 'Run';
        appendOutput("".concat(title, ": ").concat(truncateCode(code)), 'comment');
        var litsParams = getLitsParamsFromContext();
        var hijacker = hijackConsole();
        try {
            var result = getLits().run(code, litsParams);
            var content = stringifyValue(result, false);
            appendOutput(content, 'result');
        }
        catch (error) {
            appendOutput(error, 'error');
        }
        finally {
            hijacker.releaseConsole();
            focusLitsCode();
        }
    }
    function analyze() {
        addOutputSeparator();
        var selectedCode = getSelectedLitsCode();
        var code = selectedCode.code || getState('lits-code');
        var title = selectedCode.code ? 'Analyze selection' : 'Analyze';
        appendOutput("".concat(title, ": ").concat(truncateCode(code)), 'comment');
        var litsParams = getLitsParamsFromContext();
        var hijacker = hijackConsole();
        try {
            var result = getLits('debug').getUndefinedSymbols(code, litsParams);
            var unresolvedSymbols = __spreadArray([], __read(result), false).join(', ');
            var unresolvedSymbolsOutput = "Unresolved symbols: ".concat(unresolvedSymbols || '-');
            appendOutput("".concat(unresolvedSymbolsOutput), 'analyze');
        }
        catch (error) {
            appendOutput(error, 'error');
        }
        finally {
            hijacker.releaseConsole();
            focusLitsCode();
        }
    }
    function parse() {
        addOutputSeparator();
        var selectedCode = getSelectedLitsCode();
        var code = selectedCode.code || getState('lits-code');
        var title = selectedCode.code ? 'Parse selection' : 'Parse';
        appendOutput("".concat(title).concat(getState('debug') ? ' (debug):' : ':', " ").concat(truncateCode(code)), 'comment');
        var hijacker = hijackConsole();
        try {
            var tokens = getLits().tokenize(code);
            var result = getLits().parse(tokens);
            var content = JSON.stringify(result, null, 2);
            appendOutput(content, 'parse');
        }
        catch (error) {
            appendOutput(error, 'error');
        }
        finally {
            hijacker.releaseConsole();
            focusLitsCode();
        }
    }
    function tokenize() {
        addOutputSeparator();
        var selectedCode = getSelectedLitsCode();
        var code = selectedCode.code || getState('lits-code');
        var title = selectedCode.code ? 'Tokenize selection' : 'Tokenize';
        appendOutput("".concat(title).concat(getState('debug') ? ' (debug):' : ':', " ").concat(truncateCode(code)), 'comment');
        var hijacker = hijackConsole();
        try {
            var result = getLits().tokenize(code);
            var content = JSON.stringify(result, null, 2);
            appendOutput(content, 'tokenize');
        }
        catch (error) {
            appendOutput(error, 'error');
            return;
        }
        finally {
            hijacker.releaseConsole();
            focusLitsCode();
        }
    }
    function format() {
        addOutputSeparator();
        var selectedCode = getSelectedLitsCode();
        var code = selectedCode.code || getState('lits-code');
        var title = selectedCode.code ? 'Format selection' : 'Format';
        appendOutput("".concat(title, ": ").concat(truncateCode(code)), 'comment');
        setLitsCode(code, true);
        if (selectedCode.code) {
            saveState({
                'focused-panel': 'lits-code',
                'lits-code-selection-start': selectedCode.selectionStart,
                'lits-code-selection-end': selectedCode.selectionStart + code.length,
            });
        }
        else {
            saveState({
                'focused-panel': 'lits-code',
                'lits-code-selection-start': selectedCode.selectionStart,
                'lits-code-selection-end': selectedCode.selectionEnd,
            });
        }
        applyState();
    }
    function toggleDebug() {
        var debug = !getState('debug');
        saveState({ debug: debug });
        updateCSS();
        addOutputSeparator();
        appendOutput("Debug mode toggled ".concat(debug ? 'ON' : 'OFF'), 'comment');
        focusLitsCode();
    }
    function focusContext() {
        elements.contextTextArea.focus();
    }
    function focusLitsCode() {
        elements.litsTextArea.focus();
    }
    function getLitsParamsFromContext() {
        var _a, _b;
        var contextString = getState('context');
        try {
            var parsedContext = contextString.trim().length > 0
                ? JSON.parse(contextString)
                : {};
            var parsedJsFunctions = asUnknownRecord((_a = parsedContext.jsFunctions) !== null && _a !== void 0 ? _a : {});
            var values = asUnknownRecord((_b = parsedContext.values) !== null && _b !== void 0 ? _b : {});
            var jsFunctions = Object.entries(parsedJsFunctions).reduce(function (acc, _a) {
                var _b = __read(_a, 2), key = _b[0], value = _b[1];
                if (typeof value !== 'string') {
                    console.log(key, value);
                    throw new TypeError("Invalid jsFunction value. \"".concat(key, "\" should be a javascript function string"));
                }
                // eslint-disable-next-line no-eval
                var fn = eval(value);
                if (typeof fn !== 'function') {
                    throw new TypeError("Invalid jsFunction value. \"".concat(key, "\" should be a javascript function"));
                }
                acc[key] = {
                    fn: fn,
                };
                return acc;
            }, {});
            return {
                values: values,
                jsFunctions: jsFunctions,
            };
        }
        catch (err) {
            appendOutput("Error: ".concat(err.message, "\nCould not parse context:\n").concat(contextString), 'error');
            return {};
        }
    }
    function getSelectedLitsCode() {
        var selectionStart = getState('lits-code-selection-start');
        var selectionEnd = getState('lits-code-selection-end');
        return {
            leadingCode: elements.litsTextArea.value.substring(0, selectionStart),
            trailingCode: elements.litsTextArea.value.substring(selectionEnd),
            code: elements.litsTextArea.value.substring(selectionStart, selectionEnd),
            selectionStart: selectionStart,
            selectionEnd: selectionEnd,
        };
    }
    function applyState(scrollToTop) {
        if (scrollToTop === void 0) { scrollToTop = false; }
        var contextTextAreaSelectionStart = getState('context-selection-start');
        var contextTextAreaSelectionEnd = getState('context-selection-end');
        var litsTextAreaSelectionStart = getState('lits-code-selection-start');
        var litsTextAreaSelectionEnd = getState('lits-code-selection-end');
        setOutput(getState('output'), false);
        getDataFromUrl();
        setContext(getState('context'), false);
        elements.contextTextArea.selectionStart = contextTextAreaSelectionStart;
        elements.contextTextArea.selectionEnd = contextTextAreaSelectionEnd;
        setLitsCode(getState('lits-code'), false, scrollToTop ? 'top' : undefined);
        elements.litsTextArea.selectionStart = litsTextAreaSelectionStart;
        elements.litsTextArea.selectionEnd = litsTextAreaSelectionEnd;
        updateCSS();
        layout();
        setTimeout(function () {
            if (getState('focused-panel') === 'context')
                focusContext();
            else if (getState('focused-panel') === 'lits-code')
                focusLitsCode();
            elements.contextTextArea.scrollTop = getState('context-scroll-top');
            elements.litsTextArea.scrollTop = getState('lits-code-scroll-top');
            elements.outputResult.scrollTop = getState('output-scroll-top');
        }, 0);
    }
    function updateCSS() {
        var debug = getState('debug');
        elements.toggleDebugMenuLabel.textContent = debug ? 'Debug: ON' : 'Debug: OFF';
        elements.litsPanelDebugInfo.style.display = debug ? 'flex' : 'none';
        elements.litsCodeTitle.style.color = (getState('focused-panel') === 'lits-code') ? 'white' : '';
        elements.litsCodeTitleString.textContent = 'Lisp Code';
        elements.contextTitle.style.color = (getState('focused-panel') === 'context') ? 'white' : '';
    }
    function showPage(id, scroll, historyEvent) {
        if (historyEvent === void 0) { historyEvent = 'push'; }
        setTimeout(function () {
            inactivateAll();
            Search.closeSearch();
            var page = document.getElementById(id);
            var linkElementId = "".concat((!id || id === 'index') ? 'home-page' : id, "_link");
            var link = document.getElementById(linkElementId);
            if (!id || id === 'index' || id === 'example-page')
                elements.mainPanel.scrollTo({ top: 0 });
            if (!page) {
                showPage('index', scroll, 'replace');
                return;
            }
            page.classList.add('active-content');
            if (link) {
                link.classList.add('active-sidebar-entry');
                if (scroll !== 'none')
                    link.scrollIntoView({ block: 'center', behavior: scroll });
            }
            if (id === 'index')
                history.replaceState(null, 'Lits', window.location.pathname + window.location.search);
            else if (historyEvent === 'replace')
                history.replaceState(null, '', "#".concat(id));
            else if (historyEvent !== 'none')
                history.pushState(null, '', "#".concat(id));
        }, 0);
    }
    function inactivateAll() {
        var els = document.getElementsByClassName('active-content');
        while (els[0])
            els[0].classList.remove('active-content');
        els = document.getElementsByClassName('active-sidebar-entry');
        while (els[0])
            els[0].classList.remove('active-sidebar-entry');
    }
    function addToPlayground(name, encodedExample) {
        var example = decodeURIComponent(atob(encodedExample));
        appendLitsCode("// Example - ".concat(name, "\n\n").concat(example, ";\n"));
        saveState({ 'focused-panel': 'lits-code' });
        applyState();
    }
    function setPlayground(name, encodedExample) {
        var example = JSON.parse(decodeURIComponent(atob(encodedExample)));
        var context = example.context
            // eslint-disable-next-line ts/no-unsafe-return
            ? JSON.stringify(example.context, function (_k, v) { return (v === undefined ? null : v); }, 2)
            : '';
        setContext(context, true, 'top');
        var code = example.code ? example.code : '';
        var size = Math.max(name.length + 10, 40);
        var paddingLeft = Math.floor((size - name.length) / 2);
        var paddingRight = Math.ceil((size - name.length) / 2);
        setLitsCode("\n".concat("/*".concat('*'.repeat(size), "**"), "\n").concat(" *".concat(' '.repeat(paddingLeft)).concat(name).concat(' '.repeat(paddingRight), " *"), "\n").concat(" *".concat('*'.repeat(size), "**/"), "\n\n").concat(code, "\n").trimStart(), true, 'top');
        saveState({ 'focused-panel': 'lits-code' });
        applyState();
    }
    function hijackConsole() {
        var oldLog = console.log;
        console.log = function () {
            var args = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                args[_i] = arguments[_i];
            }
            var logRow = args.map(function (arg) { return stringifyValue(arg); }).join(' ');
            appendOutput(logRow, 'output');
        };
        var oldWarn = console.warn;
        console.warn = function () {
            var args = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                args[_i] = arguments[_i];
            }
            oldWarn.apply(console, args);
            appendOutput(args[0], 'warn');
        };
        var oldError = console.error;
        console.warn = function () {
            var args = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                args[_i] = arguments[_i];
            }
            oldError.apply(console, args);
            appendOutput(args[0], 'error');
        };
        return {
            releaseConsole: function () {
                console.log = oldLog;
                console.warn = oldWarn;
            },
        };
    }

    exports.Search = Search;
    exports.addContextEntry = addContextEntry;
    exports.addSampleContext = addSampleContext;
    exports.addToPlayground = addToPlayground;
    exports.analyze = analyze;
    exports.closeAddContextMenu = closeAddContextMenu;
    exports.closeMoreMenu = closeMoreMenu;
    exports.focusContext = focusContext;
    exports.focusLitsCode = focusLitsCode;
    exports.format = format;
    exports.openAddContextMenu = openAddContextMenu;
    exports.openMoreMenu = openMoreMenu;
    exports.parse = parse;
    exports.redoContextHistory = redoContextHistory;
    exports.redoLitsCodeHistory = redoLitsCodeHistory;
    exports.resetContext = resetContext;
    exports.resetLitsCode = resetLitsCode;
    exports.resetOutput = resetOutput;
    exports.resetPlayground = resetPlayground;
    exports.run = run;
    exports.setPlayground = setPlayground;
    exports.share = share;
    exports.showPage = showPage;
    exports.toggleDebug = toggleDebug;
    exports.tokenize = tokenize;
    exports.undoContextHistory = undoContextHistory;
    exports.undoLitsCodeHistory = undoLitsCodeHistory;

    return exports;

})({});
