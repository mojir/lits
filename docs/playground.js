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
        'algebraic': false,
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
        return btoa(JSON.stringify(sharedState));
    }
    function applyEncodedState(encodedState) {
        try {
            saveState(JSON.parse(atob(encodedState)), true);
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
        FunctionType[FunctionType["Fnil"] = 309] = "Fnil";
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
        [FunctionType.Fnil, 'Fnil'],
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
    function throwUnexpectedToken(expected, actual) {
        throw new LitsError("Unexpected token: ".concat(actual, ", expected ").concat(expected), undefined);
    }

    var commonSimpleTokenTypes = [
        'LBrace',
        'LBracket',
        'LParen',
        'RBrace',
        'RBracket',
        'RParen',
        'AlgNotation',
        'PolNotation',
        'EndNotation',
    ];
    var commomValueTokenTypes = [
        'String',
    ];
    function isLParenToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'LParen';
    }
    function assertLParenToken(token) {
        if (!isLParenToken(token)) {
            throwUnexpectedToken('LParen', token);
        }
    }
    function asLParenToken(token) {
        assertLParenToken(token);
        return token;
    }
    function isRParenToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'RParen';
    }
    function assertRParenToken(token) {
        if (!isRParenToken(token)) {
            throwUnexpectedToken('RParen', token);
        }
    }
    function isLBracketToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'LBracket';
    }
    function assertLBracketToken(token) {
        if (!isLBracketToken(token)) {
            throwUnexpectedToken('LBracket', token);
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
            throwUnexpectedToken('RBracket', token);
        }
    }
    function isLBraceToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'LBrace';
    }
    function assertLBraceToken(token) {
        if (!isLBraceToken(token)) {
            throwUnexpectedToken('LBrace', token);
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
            throwUnexpectedToken('RBrace', token);
        }
    }
    function isStringToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'String';
    }
    function assertStringToken(token) {
        if (!isStringToken(token)) {
            throwUnexpectedToken('String', token);
        }
    }
    function asStringToken(token) {
        assertStringToken(token);
        return token;
    }
    function isAlgebraicNotationToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'AlgNotation';
    }
    function isPolishNotationToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'PolNotation';
    }
    function isEndNotationToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'EndNotation';
    }
    function assertEndNotationToken(token) {
        if (!isEndNotationToken(token)) {
            throwUnexpectedToken('EndNotation', token);
        }
    }

    var algebraicSimpleTokenTypes = __spreadArray([], __read(commonSimpleTokenTypes), false);
    var algebraicOnlyValueTokenTypes = [
        'A_Whitespace',
        'A_Operator',
        'A_Symbol',
        'A_ReservedSymbol',
        'A_SingleLineComment',
        'A_MultiLineComment',
        'A_Number',
        'A_BasePrefixedNumber',
    ];
    var algebraicValueTokenTypes = __spreadArray(__spreadArray([], __read(commomValueTokenTypes), false), __read(algebraicOnlyValueTokenTypes), false);
    __spreadArray(__spreadArray([], __read(algebraicSimpleTokenTypes), false), __read(algebraicValueTokenTypes), false);
    var symbolicUnaryOperators = [
        '!', // logical NOT
        '~', // bitwise NOT
        '+', // addition
        '-', // subtraction
    ];
    var symbolicBinaryOperators = [
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
        '>', // greater than
        '>=', // greater than or equal
        '==', // equal
        '!=', // not equal
        '&', // bitwise AND
        '^', // bitwise XOR
        '|', // bitwise OR
        '&&', // logical AND
        '||', // logical OR
        '??', // nullish coalescing
    ];
    var otherSymbolicOperators = [
        '=>', // lambda
        '...', // rest
        '.', // property accessor
        ',', // item separator
        '=', // property assignment
        ';', // statement terminator
    ];
    var symbolicOperators = __spreadArray(__spreadArray(__spreadArray([], __read(symbolicUnaryOperators), false), __read(symbolicBinaryOperators), false), __read(otherSymbolicOperators), false);
    var nonFunctionOperators = [
        '??',
        '&&',
        'comment',
        'cond',
        'declared?',
        'if',
        'if_not',
        '||',
        'when',
        'when_not',
        'do',
        'throw',
        'let',
        'def',
        'defs',
        'if_let',
        'when_let',
        'when_first',
        'fn',
        'defn',
        'defns',
        'try',
        'recur',
        'loop',
        'doseq',
        'while',
    ];
    var nonFunctionOperatorSet = new Set(nonFunctionOperators);
    function isFunctionOperator(operator) {
        return !nonFunctionOperatorSet.has(operator);
    }
    var symbolicUnaryOperatorSet = new Set(symbolicUnaryOperators);
    function isSymbolicUnaryOperator(operator) {
        return symbolicUnaryOperatorSet.has(operator);
    }
    var symbolicBinaryOperatorSet = new Set(symbolicBinaryOperators);
    function isSymbolicBinaryOperator(operator) {
        return symbolicBinaryOperatorSet.has(operator);
    }
    var symbolicOperatorSet = new Set(symbolicOperators);
    function isSymbolicOperator(operator) {
        return symbolicOperatorSet.has(operator);
    }
    function isA_SymbolToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'A_Symbol';
    }
    function assertA_SymbolToken(token) {
        if (!isA_SymbolToken(token)) {
            throwUnexpectedToken('A_Symbol', token);
        }
    }
    function asA_SymbolToken(token) {
        assertA_SymbolToken(token);
        return token;
    }
    function isA_BinaryOperatorToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'A_Operator' && isSymbolicBinaryOperator(token[1]);
    }
    function isA_ReservedSymbolToken(token, symbolName) {
        if ((token === null || token === void 0 ? void 0 : token[0]) !== 'A_ReservedSymbol') {
            return false;
        }
        if (symbolName && token[1] !== symbolName) {
            return false;
        }
        return true;
    }
    function assertA_ReservedSymbolToken(token, symbolName) {
        if (!isA_ReservedSymbolToken(token, symbolName)) {
            throwUnexpectedToken('A_ReservedSymbol', token);
        }
    }
    function isA_CommentToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'A_SingleLineComment';
    }
    function isA_MultiLineCommentToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'A_MultiLineComment';
    }
    function isA_OperatorToken(token, operatorName) {
        if ((token === null || token === void 0 ? void 0 : token[0]) !== 'A_Operator') {
            return false;
        }
        if (operatorName && token[1] !== operatorName) {
            return false;
        }
        return true;
    }
    function assertA_OperatorToken(token, operatorName) {
        if (!isA_OperatorToken(token, operatorName)) {
            if (operatorName) {
                throw new LitsError("Unexpected token: ".concat(token, ", expected operator ").concat(operatorName), undefined);
            }
            throwUnexpectedToken('A_Operator', token);
        }
    }
    function isA_WhitespaceToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'A_Whitespace';
    }
    function isA_NumberToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'A_Number';
    }
    function isA_BasePrefixedNumberToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'A_BasePrefixedNumber';
    }

    var modifierNames = ['&rest', '&let', '&when', '&while'];
    var polishOnlySimpleTokenTypes = [
        'P_FnShorthand',
    ];
    var polishSimpleTokenTypes = __spreadArray(__spreadArray([], __read(commonSimpleTokenTypes), false), __read(polishOnlySimpleTokenTypes), false);
    var polishOnlyValueTokenTypes = [
        'P_Modifier',
        'P_StringShorthand',
        'P_Symbol',
        'P_ReservedSymbol',
        'P_RegexpShorthand',
        'P_CollectionAccessor',
        'P_Comment',
        'P_Whitespace',
        'P_Number',
    ];
    var polishValueTokenTypes = __spreadArray(__spreadArray([], __read(commomValueTokenTypes), false), __read(polishOnlyValueTokenTypes), false);
    __spreadArray(__spreadArray([], __read(polishSimpleTokenTypes), false), __read(polishValueTokenTypes), false);
    function isP_StringShorthandToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'P_StringShorthand';
    }
    function assertP_StringShorthandToken(token) {
        if (!isP_StringShorthandToken(token)) {
            throwUnexpectedToken('P_StringShorthand', token);
        }
    }
    function asP_StringShorthandToken(token) {
        assertP_StringShorthandToken(token);
        return token;
    }
    function isP_SymbolToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'P_Symbol';
    }
    function assertP_SymbolToken(token) {
        if (!isP_SymbolToken(token)) {
            throwUnexpectedToken('P_Symbol', token);
        }
    }
    function asP_SymbolToken(token) {
        assertP_SymbolToken(token);
        return token;
    }
    function isP_ReservedSymbolToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'P_ReservedSymbol';
    }
    function isP_ModifierToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'P_Modifier';
    }
    function isP_RegexpShorthandToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'P_RegexpShorthand';
    }
    function assertP_RegexpShorthandToken(token) {
        if (!isP_RegexpShorthandToken(token)) {
            throwUnexpectedToken('P_RegexpShorthand', token);
        }
    }
    function asP_RegexpShorthandToken(token) {
        assertP_RegexpShorthandToken(token);
        return token;
    }
    function isP_FnShorthandToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'P_FnShorthand';
    }
    function isP_CollectionAccessorToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'P_CollectionAccessor';
    }
    function assertP_CollectionAccessorToken(token) {
        if (!isP_CollectionAccessorToken(token)) {
            throwUnexpectedToken('P_CollectionAccessor', token);
        }
    }
    function asP_CollectionAccessorToken(token) {
        assertP_CollectionAccessorToken(token);
        return token;
    }
    function isP_CommentToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'P_Comment';
    }
    function assertP_CommentToken(token) {
        if (!isP_CommentToken(token)) {
            throwUnexpectedToken('P_Comment', token);
        }
    }
    function asP_CommentToken(token) {
        assertP_CommentToken(token);
        return token;
    }
    function isP_WhitespaceToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'P_Whitespace';
    }
    function isP_NumberToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'P_Number';
    }
    function assertP_NumberToken(token) {
        if (!isP_NumberToken(token)) {
            throwUnexpectedToken('P_Number', token);
        }
    }

    var simpleTokenTypes = __spreadArray(__spreadArray([], __read(commonSimpleTokenTypes), false), __read(polishOnlySimpleTokenTypes), false);
    var valueTokenTypes = __spreadArray(__spreadArray(__spreadArray([], __read(commomValueTokenTypes), false), __read(algebraicOnlyValueTokenTypes), false), __read(polishOnlyValueTokenTypes), false);
    var tokenTypes = __spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray([], __read(commonSimpleTokenTypes), false), __read(polishOnlySimpleTokenTypes), false), __read(commomValueTokenTypes), false), __read(algebraicOnlyValueTokenTypes), false), __read(polishOnlyValueTokenTypes), false);
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
    function isSimpleToken(token) {
        return isToken$1(token) && simpleTokenTypes.includes(token[0]);
    }
    function assertSimpleToken(token) {
        if (!isSimpleToken(token)) {
            throw new LitsError("Expected simple token, got ".concat(token), undefined);
        }
    }
    function isValueToken(token) {
        return isToken$1(token) && valueTokenTypes.includes(token[0]);
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
            return 'nil';
        if (typeof value === 'object' && value instanceof RegExp)
            return "".concat(value);
        if (typeof value === 'object' && value instanceof Error)
            return value.toString();
        return JSON.stringify(value);
    }

    function getSourceCodeInfo(anyValue, sourceCodeInfo) {
        var _a;
        // eslint-disable-next-line ts/no-unsafe-return, ts/no-unsafe-member-access
        return (_a = anyValue === null || anyValue === void 0 ? void 0 : anyValue.sourceCodeInfo) !== null && _a !== void 0 ? _a : sourceCodeInfo;
    }

    function getAssertionError(typeName, value, sourceCodeInfo) {
        return new LitsError("Expected ".concat(typeName, ", got ").concat(valueToString(value), "."), getSourceCodeInfo(value, sourceCodeInfo));
    }

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
    function isNormalExpressionNode(value) {
        if (!isAstNode(value))
            return false;
        return value.t === AstNodeType.NormalExpression;
    }
    function isNormalExpressionNodeWithName(value) {
        if (!isAstNode(value))
            return false;
        return value.t === AstNodeType.NormalExpression && typeof value.n === 'string';
    }
    function isExpressionNode(value) {
        if (!isAstNode(value))
            return false;
        return (value.t === AstNodeType.NormalExpression
            || value.t === AstNodeType.SpecialExpression
            || value.t === AstNodeType.Number
            || value.t === AstNodeType.String);
    }

    var specialExpressionCommentRemovers = {
        '&&': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
        },
        'comment': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
        },
        'cond': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
        },
        'declared?': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
        },
        'defn': function (_node, _removeOptions) { },
        'def': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
        },
        'defns': function (_node, _removeOptions) { },
        'defs': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
        },
        'do': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
        },
        'doseq': function (_node, _removeOptions) { },
        'fn': function (_node, _removeOptions) { },
        'for': function (_node, _removeOptions) { },
        'if_let': function (_node, _removeOptions) { },
        'if': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
        },
        'if_not': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
        },
        'let': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
            node.bs.forEach(function (bindingNode) {
                removeOptions.recursivelyRemoveCommentNodes(bindingNode.v);
            });
        },
        'loop': function (_node, _removeOptions) { },
        '||': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
        },
        '??': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
        },
        'recur': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
        },
        'throw': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
        },
        'try': function (_node, _removeOptions) { },
        'when_first': function (_node, _removeOptions) { },
        'when_let': function (_node, _removeOptions) { },
        'when': function (_node, _removeOptions) { },
        'when_not': function (_node, _removeOptions) { },
    };
    function removeCommentNodesFromSpecialExpression(node, removeOptions) {
        var uncommenter = specialExpressionCommentRemovers[node.n];
        return uncommenter === null || uncommenter === void 0 ? void 0 : uncommenter(node, removeOptions);
    }

    var removeOptions = {
        recursivelyRemoveCommentNodes: recursivelyRemoveCommentNodes,
        removeCommenNodesFromArray: removeCommenNodesFromArray,
    };
    function removeCommenNodes(ast) {
        removeCommenNodesFromArray(ast.b);
        ast.b.forEach(recursivelyRemoveCommentNodes);
    }
    function recursivelyRemoveCommentNodes(astNode) {
        if (isNormalExpressionNode(astNode)) {
            removeCommenNodesFromArray(astNode.p);
            astNode.p.forEach(recursivelyRemoveCommentNodes);
        }
        else if (astNode.t === AstNodeType.SpecialExpression) {
            removeCommentNodesFromSpecialExpression(astNode, removeOptions);
        }
    }
    function removeCommenNodesFromArray(astNodes) {
        var i = astNodes.findIndex(function (n) { return n.t === AstNodeType.Comment; });
        while (i >= 0) {
            astNodes.splice(i, 1);
            i = astNodes.findIndex(function (n) { return n.t === AstNodeType.Comment; });
        }
    }
    function withoutCommentNodes(astNodes) {
        return astNodes.filter(function (n) { return n.t !== AstNodeType.Comment; });
    }

    function assertEvenNumberOfParams(node) {
        var _a;
        var length = withoutCommentNodes(node.p).length;
        if (length % 2 !== 0) {
            throw new LitsError("Wrong number of arguments, expected an even number, got ".concat(valueToString(length), "."), (_a = getTokenDebugData(node.token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
        }
    }
    function assertNumberOfParams(count, node) {
        var _a, _b;
        assertCount({
            count: count,
            length: withoutCommentNodes(node.p).length,
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
            var min = count.min, max = count.max;
            if (min === undefined && max === undefined)
                throw new LitsError('Min or max must be specified.', sourceCodeInfo);
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
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        '>>': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], count = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true });
                return num >> count;
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        '>>>': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], count = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true });
                return num >>> count;
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        '~': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), num = _b[0];
                assertNumber(num, sourceCodeInfo, { integer: true });
                return ~num;
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
        },
        '&!': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), first = _b[0], rest = _b.slice(1);
                assertNumber(first, sourceCodeInfo, { integer: true });
                return rest.reduce(function (result, value) {
                    assertNumber(value, sourceCodeInfo, { integer: true });
                    return result & ~value;
                }, first);
            },
            validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
        },
        'bit_flip': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], index = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true });
                var mask = 1 << index;
                return (num ^= mask);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'bit_set': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], index = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true });
                var mask = 1 << index;
                return (num |= mask);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'bit_clear': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], index = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true });
                var mask = 1 << index;
                return (num &= ~mask);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'bit_test': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], index = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true });
                var mask = 1 << index;
                return !!(num & mask);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
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
        if (value === null)
            return 'null';
        else if (typeof value === 'boolean')
            return 'boolean';
        else if (typeof value === 'number')
            return 'number';
        else if (typeof value === 'string')
            return 'string';
        else if (Array.isArray(value))
            return 'array';
        else if (isObj(value))
            return 'object';
        else if (isRegularExpression(value))
            return 'regexp';
        else
            return 'unknown';
    }
    function compare(a, b) {
        var aType = getType(a);
        var bType = getType(b);
        if (aType !== bType)
            return Math.sign(sortOrderByType[aType] - sortOrderByType[bType]);
        switch (aType) {
            case 'null':
                return 0;
            case 'boolean':
                if (a === b)
                    return 0;
                return a === false ? -1 : 1;
            case 'number':
                return Math.sign(a - b);
            case 'string': {
                var aString = a;
                var bString = b;
                return aString < bString ? -1 : aString > bString ? 1 : 0;
            }
            case 'array': {
                var aArray = a;
                var bArray = b;
                if (aArray.length < bArray.length)
                    return -1;
                else if (aArray.length > bArray.length)
                    return 1;
                for (var i = 0; i < aArray.length; i += 1) {
                    var innerComp = compare(aArray[i], bArray[i]);
                    if (innerComp !== 0)
                        return innerComp;
                }
                return 0;
            }
            case 'object': {
                var aObj = a;
                var bObj = b;
                return Math.sign(Object.keys(aObj).length - Object.keys(bObj).length);
            }
            case 'regexp': {
                var aString = a.s;
                var bString = b.s;
                return aString < bString ? -1 : aString > bString ? 1 : 0;
            }
            case 'unknown':
                return 0;
        }
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
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'get_in': {
            evaluate: function (params, sourceCodeInfo) {
                var e_1, _a;
                var _b;
                var coll = toAny(params[0]);
                var keys = (_b = params[1]) !== null && _b !== void 0 ? _b : []; // nil behaves as empty array
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
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'contains?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), coll = _b[0], key = _b[1];
                assertStringOrNumber(key, sourceCodeInfo);
                if (coll === null)
                    return false;
                assertColl(coll, sourceCodeInfo);
                if (isSeq(coll)) {
                    if (!isNumber(key, { integer: true }))
                        return false;
                    assertNumber(key, sourceCodeInfo, { integer: true });
                    return key >= 0 && key < coll.length;
                }
                return !!Object.getOwnPropertyDescriptor(coll, key);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'has?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), coll = _b[0], value = _b[1];
                if (coll === null)
                    return false;
                assertColl(coll, sourceCodeInfo);
                if (Array.isArray(coll))
                    return coll.includes(value);
                if (typeof coll === 'string') {
                    if (typeof value === 'string') {
                        return coll.includes(value);
                    }
                    else if (typeof value === 'number') {
                        return coll.includes("".concat(value));
                    }
                    return false;
                }
                return Object.values(coll).includes(value);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'has_some?': {
            evaluate: function (_a, sourceCodeInfo) {
                var e_2, _b, e_3, _c, e_4, _d;
                var _e = __read(_a, 2), coll = _e[0], seq = _e[1];
                if (coll === null || seq === null)
                    return false;
                assertColl(coll, sourceCodeInfo);
                assertSeq(seq, sourceCodeInfo);
                if (Array.isArray(coll)) {
                    try {
                        for (var seq_1 = __values(seq), seq_1_1 = seq_1.next(); !seq_1_1.done; seq_1_1 = seq_1.next()) {
                            var value = seq_1_1.value;
                            if (coll.includes(value))
                                return true;
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
                if (typeof coll === 'string') {
                    try {
                        for (var seq_2 = __values(seq), seq_2_1 = seq_2.next(); !seq_2_1.done; seq_2_1 = seq_2.next()) {
                            var value = seq_2_1.value;
                            if (isString(value, { char: true }) ? coll.split('').includes(value) : false)
                                return true;
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
                        if (Object.values(coll).includes(value))
                            return true;
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
        'has_every?': {
            evaluate: function (_a, sourceCodeInfo) {
                var e_5, _b, e_6, _c, e_7, _d;
                var _e = __read(_a, 2), coll = _e[0], seq = _e[1];
                if (coll === null)
                    return false;
                assertColl(coll, sourceCodeInfo);
                if (seq === null)
                    return true;
                assertSeq(seq, sourceCodeInfo);
                if (Array.isArray(coll)) {
                    try {
                        for (var seq_4 = __values(seq), seq_4_1 = seq_4.next(); !seq_4_1.done; seq_4_1 = seq_4.next()) {
                            var value = seq_4_1.value;
                            if (!coll.includes(value))
                                return false;
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
                if (typeof coll === 'string') {
                    try {
                        for (var seq_5 = __values(seq), seq_5_1 = seq_5.next(); !seq_5_1.done; seq_5_1 = seq_5.next()) {
                            var value = seq_5_1.value;
                            if (!isString(value, { char: true }) || !coll.split('').includes(value))
                                return false;
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
                        if (!Object.values(coll).includes(value))
                            return false;
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
        'assoc': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), coll = _b[0], key = _b[1], value = _b[2];
                assertColl(coll, sourceCodeInfo);
                assertStringOrNumber(key, sourceCodeInfo);
                assertAny(value, sourceCodeInfo);
                return assoc(coll, key, value, sourceCodeInfo);
            },
            validate: function (node) { return assertNumberOfParams(3, node); },
        },
        'assoc_in': {
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
            validate: function (node) { return assertNumberOfParams(3, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 3 }, node); },
        },
        'update_in': {
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
            validate: function (node) { return assertNumberOfParams({ min: 3 }, node); },
        },
        'concat': {
            evaluate: function (params, sourceCodeInfo) {
                assertColl(params[0], sourceCodeInfo);
                if (Array.isArray(params[0])) {
                    return params.reduce(function (result, arr) {
                        assertArray(arr, sourceCodeInfo);
                        return result.concat(arr);
                    }, []);
                }
                else if (isString(params[0])) {
                    return params.reduce(function (result, s) {
                        assertString(s, sourceCodeInfo);
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
            validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
        },
        'not_empty': {
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
            validate: function (node) { return assertNumberOfParams(1, node); },
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
            validate: function (node) { return assertNumberOfParams(2, node); },
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
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'not_any?': {
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
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'not_every?': {
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
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
    };

    var arrayNormalExpression = {
        array: {
            evaluate: function (params) { return params; },
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
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 3 }, node); },
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
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        flatten: {
            evaluate: function (_a) {
                var _b = __read(_a, 1), seq = _b[0];
                if (!Array.isArray(seq))
                    return [];
                return seq.flat(Number.POSITIVE_INFINITY);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        mapcat: {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), arr = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertArray(arr, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                return arr.map(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); }).flat(1);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
    };

    var sequenceNormalExpression = {
        nth: {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(params, 2), seq = _a[0], i = _a[1];
                var defaultValue = toAny(params[2]);
                assertNumber(i, sourceCodeInfo, { integer: true });
                if (seq === null)
                    return defaultValue;
                assertSeq(seq, sourceCodeInfo);
                return i >= 0 && i < seq.length ? toAny(seq[i]) : defaultValue;
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        filter: {
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
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        first: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), array = _b[0];
                if (array === null)
                    return null;
                assertSeq(array, sourceCodeInfo);
                return toAny(array[0]);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        last: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), array = _b[0];
                if (array === null)
                    return null;
                assertSeq(array, sourceCodeInfo);
                return toAny(array[array.length - 1]);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        map: {
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
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        pop: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), seq = _b[0];
                assertSeq(seq, sourceCodeInfo);
                if (typeof seq === 'string') {
                    return seq.substring(0, seq.length - 1);
                }
                return seq.slice(0, seq.length - 1);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        position: {
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
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        index_of: {
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
                    var index = seq.indexOf(value);
                    return index !== -1 ? index : null;
                }
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        push: {
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
            validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
        },
        reductions: {
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
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        reduce: {
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
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        reduce_right: {
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
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        rest: {
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        nthrest: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), seq = _b[0], count = _b[1];
                assertSeq(seq, sourceCodeInfo);
                assertNumber(count, sourceCodeInfo, { finite: true });
                var integerCount = Math.max(Math.ceil(count), 0);
                if (Array.isArray(seq))
                    return seq.slice(integerCount);
                return seq.substring(integerCount);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        next: {
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        nthnext: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), seq = _b[0], count = _b[1];
                assertSeq(seq, sourceCodeInfo);
                assertNumber(count, sourceCodeInfo, { finite: true });
                var integerCount = Math.max(Math.ceil(count), 0);
                if (seq.length <= count)
                    return null;
                if (Array.isArray(seq))
                    return seq.slice(integerCount);
                return seq.substring(integerCount);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        reverse: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), seq = _b[0];
                if (seq === null)
                    return null;
                assertSeq(seq, sourceCodeInfo);
                if (Array.isArray(seq))
                    return __spreadArray([], __read(seq), false).reverse();
                return seq.split('').reverse().join('');
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        second: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), seq = _b[0];
                if (seq === null)
                    return null;
                assertSeq(seq, sourceCodeInfo);
                return toAny(seq[1]);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        shift: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), seq = _b[0];
                assertSeq(seq, sourceCodeInfo);
                if (typeof seq === 'string')
                    return seq.substring(1);
                var copy = __spreadArray([], __read(seq), false);
                copy.shift();
                return copy;
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        slice: {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(params, 3), seq = _a[0], from = _a[1], to = _a[2];
                assertSeq(seq, sourceCodeInfo);
                if (params.length === 1)
                    return seq;
                assertNumber(from, sourceCodeInfo, { integer: true });
                if (params.length === 2)
                    return seq.slice(from);
                assertNumber(to, sourceCodeInfo, { integer: true });
                return seq.slice(from, to);
            },
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 3 }, node); },
        },
        some: {
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
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        sort: {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var _b = __read(params, 1), seq = _b[0];
                var defaultComparer = params.length === 1;
                var comparer = defaultComparer ? null : params[1];
                assertSeq(seq, sourceCodeInfo);
                if (typeof seq === 'string') {
                    var result_1 = seq.split('');
                    if (defaultComparer) {
                        result_1.sort(compare);
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
                    result.sort(compare);
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
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
        },
        sort_by: {
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
                            var bKey = executeFunction(keyfn, [b], contextStack, sourceCodeInfo);
                            return compare(aKey, bKey);
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
                        var bKey = executeFunction(keyfn, [b], contextStack, sourceCodeInfo);
                        return compare(aKey, bKey);
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
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        take: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), input = _b[0], n = _b[1];
                assertNumber(n, sourceCodeInfo);
                assertSeq(input, sourceCodeInfo);
                var num = Math.max(Math.ceil(n), 0);
                return input.slice(0, num);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        take_last: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), array = _b[0], n = _b[1];
                assertSeq(array, sourceCodeInfo);
                assertNumber(n, sourceCodeInfo);
                var num = Math.max(Math.ceil(n), 0);
                var from = array.length - num;
                return array.slice(from);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        take_while: {
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
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        drop: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), input = _b[0], n = _b[1];
                assertNumber(n, sourceCodeInfo);
                var num = Math.max(Math.ceil(n), 0);
                assertSeq(input, sourceCodeInfo);
                return input.slice(num);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        drop_last: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), array = _b[0], n = _b[1];
                assertSeq(array, sourceCodeInfo);
                assertNumber(n, sourceCodeInfo);
                var num = Math.max(Math.ceil(n), 0);
                var from = array.length - num;
                return array.slice(0, from);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        drop_while: {
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
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        unshift: {
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
            validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
        },
        distinct: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), input = _b[0];
                assertSeq(input, sourceCodeInfo);
                if (Array.isArray(input))
                    return Array.from(new Set(input));
                return Array.from(new Set(input.split(''))).join('');
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        remove: {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), fn = _c[0], input = _c[1];
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
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        remove_at: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), index = _b[0], input = _b[1];
                assertNumber(index, sourceCodeInfo);
                assertSeq(input, sourceCodeInfo);
                var intIndex = Math.ceil(index);
                if (intIndex < 0 || intIndex >= input.length)
                    return input;
                if (Array.isArray(input)) {
                    var copy = __spreadArray([], __read(input), false);
                    copy.splice(index, 1);
                    return copy;
                }
                return "".concat(input.substring(0, index)).concat(input.substring(index + 1));
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        split_at: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), pos = _b[0], seq = _b[1];
                assertNumber(pos, sourceCodeInfo, { finite: true });
                var intPos = toNonNegativeInteger(pos);
                assertSeq(seq, sourceCodeInfo);
                return [seq.slice(0, intPos), seq.slice(intPos)];
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        split_with: {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), fn = _c[0], seq = _c[1];
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
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        frequencies: {
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        group_by: {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), fn = _c[0], seq = _c[1];
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
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        partition: {
            evaluate: function (params, sourceCodeInfo) {
                var len = params.length;
                var n = toNonNegativeInteger(asNumber(params[0], sourceCodeInfo));
                var seq = len === 2
                    ? asSeq(params[1], sourceCodeInfo)
                    : len === 3
                        ? asSeq(params[2], sourceCodeInfo)
                        : asSeq(params[3], sourceCodeInfo);
                var step = len >= 3 ? toNonNegativeInteger(asNumber(params[1], sourceCodeInfo)) : n;
                var pad = len === 4 ? (params[2] === null ? [] : asArray(params[2], sourceCodeInfo)) : undefined;
                return partition(n, step, seq, pad, sourceCodeInfo);
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 4 }, node); },
        },
        partition_all: {
            evaluate: function (params, sourceCodeInfo) {
                var len = params.length;
                var n = toNonNegativeInteger(asNumber(params[0], sourceCodeInfo));
                var seq = len === 2 ? asSeq(params[1], sourceCodeInfo) : asSeq(params[2], sourceCodeInfo);
                var step = len >= 3 ? toNonNegativeInteger(asNumber(params[1], sourceCodeInfo)) : n;
                return partition(n, step, seq, [], sourceCodeInfo);
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        partition_by: {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), fn = _c[0], seq = _c[1];
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
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'dec': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo);
                return first - 1;
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        '+': {
            evaluate: function (params, sourceCodeInfo) {
                return params.reduce(function (result, param) {
                    assertNumber(param, sourceCodeInfo);
                    return result + param;
                }, 0);
            },
        },
        '*': {
            evaluate: function (params, sourceCodeInfo) {
                return params.reduce(function (result, param) {
                    assertNumber(param, sourceCodeInfo);
                    return result * param;
                }, 1);
            },
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
        },
        'quot': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), dividend = _b[0], divisor = _b[1];
                assertNumber(dividend, sourceCodeInfo);
                assertNumber(divisor, sourceCodeInfo);
                var quotient = Math.trunc(dividend / divisor);
                return quotient;
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'mod': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), dividend = _b[0], divisor = _b[1];
                assertNumber(dividend, sourceCodeInfo);
                assertNumber(divisor, sourceCodeInfo);
                var quotient = Math.floor(dividend / divisor);
                return dividend - divisor * quotient;
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        '%': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), dividend = _b[0], divisor = _b[1];
                assertNumber(dividend, sourceCodeInfo);
                assertNumber(divisor, sourceCodeInfo);
                var quotient = Math.trunc(dividend / divisor);
                return dividend - divisor * quotient;
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'sqrt': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo);
                return Math.sqrt(first);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'cbrt': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo);
                return Math.cbrt(first);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        '**': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], second = _b[1];
                assertNumber(first, sourceCodeInfo);
                assertNumber(second, sourceCodeInfo);
                return Math.pow(first, second);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
        },
        'trunc': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo);
                return Math.trunc(first);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'floor': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo);
                return Math.floor(first);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'ceil': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo);
                return Math.ceil(first);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
        },
        'abs': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.abs(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'sign': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.sign(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'max_safe_integer': {
            evaluate: function () {
                return Number.MAX_SAFE_INTEGER;
            },
            validate: function (node) { return assertNumberOfParams(0, node); },
        },
        'min_safe_integer': {
            evaluate: function () {
                return Number.MIN_SAFE_INTEGER;
            },
            validate: function (node) { return assertNumberOfParams(0, node); },
        },
        'max_value': {
            evaluate: function () {
                return Number.MAX_VALUE;
            },
            validate: function (node) { return assertNumberOfParams(0, node); },
        },
        'min_value': {
            evaluate: function () {
                return Number.MIN_VALUE;
            },
            validate: function (node) { return assertNumberOfParams(0, node); },
        },
        'epsilon': {
            evaluate: function () {
                return Number.EPSILON;
            },
            validate: function (node) { return assertNumberOfParams(0, node); },
        },
        'positive_infinity': {
            evaluate: function () {
                return Number.POSITIVE_INFINITY;
            },
            validate: function (node) { return assertNumberOfParams(0, node); },
        },
        'negative_infinity': {
            evaluate: function () {
                return Number.NEGATIVE_INFINITY;
            },
            validate: function (node) { return assertNumberOfParams(0, node); },
        },
        'nan': {
            evaluate: function () {
                return Number.NaN;
            },
            validate: function (node) { return assertNumberOfParams(0, node); },
        },
        'e': {
            evaluate: function () {
                return Math.E;
            },
            validate: function (node) { return assertNumberOfParams(0, node); },
        },
        'pi': {
            evaluate: function () {
                return Math.PI;
            },
            validate: function (node) { return assertNumberOfParams(0, node); },
        },
        'exp': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.exp(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'log': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.log(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'log2': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.log2(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'log10': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.log10(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'sin': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.sin(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'asin': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.asin(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'sinh': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.sinh(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'asinh': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.asinh(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'cos': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.cos(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'acos': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.acos(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'cosh': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.cosh(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'acosh': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.acosh(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'tan': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.tan(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'atan': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.atan(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'tanh': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.tanh(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'atanh': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Math.atanh(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
    };

    var miscNormalExpression = {
        '!=': {
            evaluate: function (params) {
                for (var i = 0; i < params.length - 1; i += 1) {
                    for (var j = i + 1; j < params.length; j += 1) {
                        if (params[i] === params[j])
                            return false;
                    }
                }
                return true;
            },
            validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
        },
        '==': {
            evaluate: function (_a) {
                var e_1, _b;
                var _c = __read(_a), first = _c[0], rest = _c.slice(1);
                try {
                    for (var rest_1 = __values(rest), rest_1_1 = rest_1.next(); !rest_1_1.done; rest_1_1 = rest_1.next()) {
                        var param = rest_1_1.value;
                        if (param !== first)
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
            },
            validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
        },
        'equal?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), a = _b[0], b = _b[1];
                return deepEqual(asAny(a, sourceCodeInfo), asAny(b, sourceCodeInfo), sourceCodeInfo);
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
                        if (compare(currentValue, param) <= 0)
                            return false;
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
                        if (compare(currentValue, param) >= 0)
                            return false;
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
                        if (compare(currentValue, param) < 0)
                            return false;
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
                        if (compare(currentValue, param) > 0)
                            return false;
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
        '!': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return !first;
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'epoch>iso_date': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), ms = _b[0];
                assertNumber(ms, sourceCodeInfo);
                return new Date(ms).toISOString();
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'iso_date>epoch': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), dateTime = _b[0];
                assertString(dateTime, sourceCodeInfo);
                var ms = new Date(dateTime).valueOf();
                assertNumber(ms, sourceCodeInfo, { finite: true });
                return ms;
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'write!': {
            evaluate: function (params, sourceCodeInfo) {
                // eslint-disable-next-line no-console
                console.log.apply(console, __spreadArray([], __read(params), false));
                if (params.length > 0)
                    return asAny(params[params.length - 1], sourceCodeInfo);
                return null;
            },
        },
        'boolean': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), value = _b[0];
                return !!value;
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'compare': {
            evaluate: function (_a) {
                var _b = __read(_a, 2), a = _b[0], b = _b[1];
                return compare(a, b);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'json_parse': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertString(first, sourceCodeInfo);
                // eslint-disable-next-line ts/no-unsafe-return
                return JSON.parse(first);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'json_stringify': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], second = _b[1];
                assertAny(first, sourceCodeInfo);
                if (second === undefined)
                    return JSON.stringify(first);
                assertNumber(second, sourceCodeInfo);
                return JSON.stringify(first, null, second);
            },
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
        },
        'assert=': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (first !== second)
                    throw new AssertionError("Expected ".concat(first, " to be ").concat(second, ".").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'assert!=': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (first === second)
                    throw new AssertionError("Expected ".concat(first, " not to be ").concat(second, ".").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'assert_equal': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (!deepEqual(asAny(first, sourceCodeInfo), asAny(second, sourceCodeInfo), sourceCodeInfo)) {
                    throw new AssertionError("Expected\n".concat(JSON.stringify(first, null, 2), "\nto deep equal\n").concat(JSON.stringify(second, null, 2), ".").concat(message), sourceCodeInfo);
                }
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'assert_not_equal': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (deepEqual(asAny(first, sourceCodeInfo), asAny(second, sourceCodeInfo), sourceCodeInfo)) {
                    throw new AssertionError("Expected ".concat(JSON.stringify(first), " not to deep equal ").concat(JSON.stringify(second), ".").concat(message), sourceCodeInfo);
                }
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'assert_gt': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (compare(first, second) <= 0)
                    throw new AssertionError("Expected ".concat(first, " to be grater than ").concat(second, ".").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'assert_gte': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (compare(first, second) < 0)
                    throw new AssertionError("Expected ".concat(first, " to be grater than or equal to ").concat(second, ".").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'assert_lt': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (compare(first, second) >= 0)
                    throw new AssertionError("Expected ".concat(first, " to be less than ").concat(second, ".").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'assert_lte': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (compare(first, second) > 0)
                    throw new AssertionError("Expected ".concat(first, " to be less than or equal to ").concat(second, ".").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'assert_true': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (first !== true)
                    throw new AssertionError("Expected ".concat(first, " to be true.").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
        },
        'assert_false': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (first !== false)
                    throw new AssertionError("Expected ".concat(first, " to be false.").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
        },
        'assert_truthy': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (!first)
                    throw new AssertionError("Expected ".concat(first, " to be truthy.").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
        },
        'assert_falsy': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (first)
                    throw new AssertionError("Expected ".concat(first, " to be falsy.").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
        },
        'assert_null': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (first !== null)
                    throw new AssertionError("Expected ".concat(first, " to be nil.").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
        },
        'assert_throws': {
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
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
        },
        'assert_throws_error': {
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
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'assert_not_throws': {
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
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
        },
    };

    var objectNormalExpression = {
        object: {
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
            validate: function (node) { return assertEvenNumberOfParams(node); },
        },
        keys: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), obj = _b[0];
                assertObj(obj, sourceCodeInfo);
                return Object.keys(obj);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        vals: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), obj = _b[0];
                assertObj(obj, sourceCodeInfo);
                return Object.values(obj);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        entries: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), obj = _b[0];
                assertObj(obj, sourceCodeInfo);
                return Object.entries(obj);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        find: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), obj = _b[0], key = _b[1];
                assertObj(obj, sourceCodeInfo);
                assertString(key, sourceCodeInfo);
                if (collHasKey(obj, key))
                    return [key, obj[key]];
                return null;
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        dissoc: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), obj = _b[0], key = _b[1];
                assertObj(obj, sourceCodeInfo);
                assertString(key, sourceCodeInfo);
                var newObj = __assign({}, obj);
                delete newObj[key];
                return newObj;
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        merge: {
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
            validate: function (node) { return assertNumberOfParams({ min: 0 }, node); },
        },
        merge_with: {
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
            validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
        },
        zipmap: {
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
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        select_keys: {
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
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
    };

    var predicatesNormalExpression = {
        'function?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return isLitsFunction(first);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'string?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return typeof first === 'string';
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'number?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return typeof first === 'number';
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'integer?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return typeof first === 'number' && isNumber(first, { integer: true });
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'boolean?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return typeof first === 'boolean';
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
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo, { finite: true });
                return first === 0;
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'pos?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo, { finite: true });
                return first > 0;
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'neg?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo, { finite: true });
                return first < 0;
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'even?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo, { finite: true });
                return first % 2 === 0;
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'odd?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo, { finite: true });
                return isNumber(first, { integer: true }) && first % 2 !== 0;
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'array?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return Array.isArray(first);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'coll?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return isColl(first);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'seq?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return isSeq(first);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'object?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return isObj(first);
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
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Number.isFinite(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'nan?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Number.isNaN(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'positive_infinity?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return value === Number.POSITIVE_INFINITY;
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'negative_infinity?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'not_empty?': {
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
    };

    var regexpNormalExpression = {
        regexp: {
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
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
        },
        match: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), regexp = _b[0], text = _b[1];
                assertRegularExpression(regexp, sourceCodeInfo);
                if (!isString(text))
                    return null;
                var regExp = new RegExp(regexp.s, regexp.f);
                var match = regExp.exec(text);
                if (match)
                    return __spreadArray([], __read(match), false);
                return null;
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        replace: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), str = _b[0], regexp = _b[1], value = _b[2];
                assertString(str, sourceCodeInfo);
                assertRegularExpression(regexp, sourceCodeInfo);
                assertString(value, sourceCodeInfo);
                var regExp = new RegExp(regexp.s, regexp.f);
                return str.replace(regExp, value);
            },
            validate: function (node) { return assertNumberOfParams(3, node); },
        },
    };

    var stringNormalExpression = {
        'subs': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], third = _b[2];
                assertString(first, sourceCodeInfo);
                assertNumber(second, sourceCodeInfo, { integer: true, nonNegative: true });
                if (third === undefined)
                    return (first).substring(second);
                assertNumber(third, sourceCodeInfo, { gte: second });
                return (first).substring(second, third);
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'string_repeat': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), str = _b[0], count = _b[1];
                assertString(str, sourceCodeInfo);
                assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true });
                return str.repeat(count);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'from_char_code': {
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'to_char_code': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo, { nonEmpty: true });
                return asNonUndefined(str.codePointAt(0), sourceCodeInfo);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'lower_case': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.toLowerCase();
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'upper_case': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.toUpperCase();
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'trim': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.trim();
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'trim_left': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.replace(/^\s+/, '');
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'trim_right': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.replace(/\s+$/, '');
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        '++': {
            evaluate: function (params, sourceCodeInfo) {
                if (params.length === 0) {
                    return '';
                }
                var first = params[0];
                if (first !== null) {
                    assertStringOrNumber(first, sourceCodeInfo);
                }
                if (params.length === 1) {
                    return first === null ? '' : "".concat(first);
                }
                return params.slice(1).reduce(function (acc, str) {
                    if (str !== null) {
                        assertStringOrNumber(str, sourceCodeInfo);
                    }
                    if (str === null) {
                        return acc;
                    }
                    return "".concat(acc).concat(str);
                }, first === null ? '' : "".concat(first));
            },
            validate: function () { return undefined; },
        },
        'join': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), stringList = _b[0], delimiter = _b[1];
                assertArray(stringList, sourceCodeInfo);
                stringList.forEach(function (str) { return assertString(str, sourceCodeInfo); });
                assertString(delimiter, sourceCodeInfo);
                return stringList.join(delimiter);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'pad_left': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), str = _b[0], length = _b[1], padString = _b[2];
                assertString(str, sourceCodeInfo);
                assertNumber(length, sourceCodeInfo, { integer: true });
                if (padString !== undefined)
                    assertString(padString, sourceCodeInfo);
                return str.padStart(length, padString);
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'pad_right': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), str = _b[0], length = _b[1], padString = _b[2];
                assertString(str, sourceCodeInfo);
                assertNumber(length, sourceCodeInfo, { integer: true });
                if (padString !== undefined)
                    assertString(padString, sourceCodeInfo);
                return str.padEnd(length, padString);
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 10 }, node); },
        },
        'encode_base64': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertString(value, sourceCodeInfo);
                return btoa(encodeURIComponent(value).replace(/%([0-9A-F]{2})/g, function (_match, p1) {
                    // eslint-disable-next-line ts/no-unsafe-argument
                    return String.fromCharCode(Number.parseInt(p1, 16));
                }));
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'decode_base64': {
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'encode_uri_component': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertString(value, sourceCodeInfo);
                return encodeURIComponent(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'decode_uri_component': {
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
            validate: function (node) { return assertNumberOfParams(1, node); },
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
        apply: {
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
            validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
        },
        comp: {
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
        },
        constantly: {
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        juxt: {
            evaluate: function (fns, sourceCodeInfo) {
                var _a;
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.sourceCodeInfo = sourceCodeInfo,
                    _a.t = FunctionType.Juxt,
                    _a.f = fns,
                    _a;
            },
            validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
        },
        complement: {
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        every_pred: {
            evaluate: function (fns, sourceCodeInfo) {
                var _a;
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.sourceCodeInfo = sourceCodeInfo,
                    _a.t = FunctionType.EveryPred,
                    _a.f = fns,
                    _a;
            },
            validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
        },
        some_pred: {
            evaluate: function (fns, sourceCodeInfo) {
                var _a;
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.sourceCodeInfo = sourceCodeInfo,
                    _a.t = FunctionType.SomePred,
                    _a.f = fns,
                    _a;
            },
            validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
        },
        fnil: {
            evaluate: function (_a, sourceCodeInfo) {
                var _b;
                var _c = __read(_a), fn = _c[0], params = _c.slice(1);
                return _b = {},
                    _b[FUNCTION_SYMBOL] = true,
                    _b.sourceCodeInfo = sourceCodeInfo,
                    _b.t = FunctionType.Fnil,
                    _b.f = toAny(fn),
                    _b.p = params,
                    _b;
            },
            validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
        },
    };

    var normalExpressions = __assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign({}, bitwiseNormalExpression), collectionNormalExpression), arrayNormalExpression), sequenceNormalExpression), mathNormalExpression), miscNormalExpression), assertNormalExpression), objectNormalExpression), predicatesNormalExpression), regexpNormalExpression), stringNormalExpression), functionalNormalExpression);

    var andSpecialExpression = {
        polishParse: function (tokenStream, parseState, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var params = parseTokensUntilClosingBracket(tokenStream, parseState);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            var node = {
                t: AstNodeType.SpecialExpression,
                n: '&&',
                p: params,
                token: getTokenDebugData(firstToken) && firstToken,
            };
            return node;
        },
        validateParameterCount: function () { return undefined; },
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
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return findUnresolvedIdentifiers(node.p, contextStack, builtin);
        },
    };

    function getCommonPolishSpecialExpressionParser(name) {
        return function (tokenStream, parseState, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var params = parseTokensUntilClosingBracket(tokenStream, parseState);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            return {
                t: AstNodeType.SpecialExpression,
                n: name,
                p: params,
                token: getTokenDebugData(firstToken) && firstToken,
            };
        };
    }

    var commentSpecialExpression = {
        polishParse: getCommonPolishSpecialExpressionParser('comment'),
        validateParameterCount: function () { return undefined; },
        evaluate: function () { return null; },
        findUnresolvedIdentifiers: function () { return new Set(); },
    };

    var condSpecialExpression = {
        polishParse: getCommonPolishSpecialExpressionParser('cond'),
        validateParameterCount: function (node) { return assertEvenNumberOfParams(node); },
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
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return findUnresolvedIdentifiers(node.p, contextStack, builtin);
        },
    };

    var declaredSpecialExpression = {
        polishParse: getCommonPolishSpecialExpressionParser('declared?'),
        validateParameterCount: function (node) { return assertNumberOfParams(1, node); },
        evaluate: function (node, contextStack) {
            var lookUpResult = contextStack.lookUp(node.p[0]);
            return lookUpResult !== null;
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return findUnresolvedIdentifiers(node.p, contextStack, builtin);
        },
    };

    var polishReservedNamesRecord = {
        true: { value: true },
        false: { value: false },
        nil: { value: null },
        null: { value: null },
    };

    function assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo) {
        if (typeof name !== 'string')
            return;
        if (builtin.specialExpressions[name])
            throw new LitsError("Cannot define variable ".concat(name, ", it's a special expression."), sourceCodeInfo);
        if (builtin.normalExpressions[name])
            throw new LitsError("Cannot define variable ".concat(name, ", it's a builtin function."), sourceCodeInfo);
        // eslint-disable-next-line ts/no-unsafe-member-access
        if (polishReservedNamesRecord[name])
            throw new LitsError("Cannot define variable ".concat(name, ", it's a reserved name."), sourceCodeInfo);
        if (contextStack.globalContext[name])
            throw new LitsError("Name already defined \"".concat(name, "\"."), sourceCodeInfo);
    }

    var defSpecialExpression = {
        polishParse: function (tokenStream, parseState, firstToken, _a) {
            var _b;
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var params = parseTokensUntilClosingBracket(tokenStream, parseState);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'def',
                p: params,
                token: getTokenDebugData(firstToken) && firstToken,
            };
            assertSymbolNode(node.p[0], (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
            return node;
        },
        validateParameterCount: function (node) { return assertNumberOfParams(2, node); },
        evaluate: function (node, contextStack, _a) {
            var _b;
            var evaluateAstNode = _a.evaluateAstNode, builtin = _a.builtin;
            var sourceCodeInfo = (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo;
            var name = node.p[0].v;
            assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo);
            contextStack.globalContext[name] = {
                value: evaluateAstNode(node.p[1], contextStack),
            };
            return null;
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var _b;
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            var sourceCodeInfo = (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo;
            var subNode = asAstNode(node.p[1]);
            var result = findUnresolvedIdentifiers([subNode], contextStack, builtin);
            var name = asSymbolNode(node.p[0]).v;
            assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo);
            contextStack.globalContext[name] = { value: true };
            return result;
        },
    };

    var defsSpecialExpression = {
        polishParse: function (tokenStream, parseState, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var params = parseTokensUntilClosingBracket(tokenStream, parseState);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'defs',
                p: params,
                token: getTokenDebugData(firstToken) && firstToken,
            };
            return node;
        },
        validateParameterCount: function (node) { return assertNumberOfParams(2, node); },
        evaluate: function (node, contextStack, _a) {
            var _b, _c;
            var evaluateAstNode = _a.evaluateAstNode, builtin = _a.builtin;
            var sourceCodeInfo = (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo;
            var name = evaluateAstNode(node.p[0], contextStack);
            assertString(name, sourceCodeInfo);
            assertNameNotDefined(name, contextStack, builtin, (_c = getTokenDebugData(node.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
            contextStack.globalContext[name] = {
                value: evaluateAstNode(node.p[1], contextStack),
            };
            return null;
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var _b;
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin, evaluateAstNode = _a.evaluateAstNode;
            var sourceCodeInfo = (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo;
            var subNode = node.p[1];
            var result = findUnresolvedIdentifiers([subNode], contextStack, builtin);
            var name = evaluateAstNode(node.p[0], contextStack);
            assertString(name, sourceCodeInfo);
            assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo);
            contextStack.globalContext[name] = { value: true };
            return result;
        },
    };

    var doSpecialExpression = {
        polishParse: getCommonPolishSpecialExpressionParser('do'),
        validateParameterCount: function () { return undefined; },
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
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return findUnresolvedIdentifiers(node.p, contextStack, builtin);
        },
    };

    function joinAnalyzeResults() {
        var e_1, _a;
        var results = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            results[_i] = arguments[_i];
        }
        var result = new Set();
        try {
            for (var results_1 = __values(results), results_1_1 = results_1.next(); !results_1_1.done; results_1_1 = results_1.next()) {
                var identifier = results_1_1.value;
                identifier.forEach(function (symbol) { return result.add(symbol); });
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
        source.forEach(function (symbol) { return target.add(symbol); });
    }
    function combinate(arrays) {
        return arrays.reduce(function (acc, curr) {
            return acc.flatMap(function (a) { return curr.map(function (c) { return __spreadArray(__spreadArray([], __read(a), false), [c], false); }); });
        }, [[]]);
    }

    var defnSpecialExpression = {
        polishParse: function (tokenStream, parseState, firstToken, parsers) {
            var _a;
            var parseToken = parsers.parseToken;
            var functionName = parseToken(tokenStream, parseState);
            assertSymbolNode(functionName, (_a = getTokenDebugData(functionName.token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
            var functionOverloades = parseFunctionOverloades(tokenStream, parseState, parsers);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'defn',
                f: functionName,
                p: [],
                o: functionOverloades,
                token: getTokenDebugData(firstToken) && firstToken,
            };
            return node;
        },
        validateParameterCount: function () { return undefined; },
        evaluate: function (node, contextStack, _a) {
            var _b;
            var _c, _d;
            var builtin = _a.builtin, evaluateAstNode = _a.evaluateAstNode;
            var name = getFunctionName('defn', node, contextStack, evaluateAstNode);
            assertNameNotDefined(name, contextStack, builtin, (_c = getTokenDebugData(node.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
            var evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode);
            var litsFunction = (_b = {},
                _b[FUNCTION_SYMBOL] = true,
                _b.sourceCodeInfo = (_d = getTokenDebugData(node.token)) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo,
                _b.t = FunctionType.UserDefined,
                _b.n = name,
                _b.o = evaluatedFunctionOverloades,
                _b);
            contextStack.globalContext[name] = { value: litsFunction };
            return null;
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var _b;
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            contextStack.globalContext[node.f.v] = { value: true };
            var newContext = (_b = {}, _b[node.f.v] = { value: true }, _b);
            return addOverloadsUnresolvedIdentifiers(node.o, contextStack, findUnresolvedIdentifiers, builtin, newContext);
        },
    };
    var defnsSpecialExpression = {
        polishParse: function (tokenStream, parseState, firstToken, parsers) {
            var parseToken = parsers.parseToken;
            var functionName = parseToken(tokenStream, parseState);
            var functionOverloades = parseFunctionOverloades(tokenStream, parseState, parsers);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'defns',
                p: [],
                f: functionName,
                o: functionOverloades,
                token: getTokenDebugData(firstToken) && firstToken,
            };
            return node;
        },
        validateParameterCount: function () { return undefined; },
        evaluate: function (node, contextStack, _a) {
            var _b;
            var _c, _d;
            var builtin = _a.builtin, evaluateAstNode = _a.evaluateAstNode;
            var name = getFunctionName('defns', node, contextStack, evaluateAstNode);
            assertNameNotDefined(name, contextStack, builtin, (_c = getTokenDebugData(node.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
            var evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode);
            var litsFunction = (_b = {},
                _b[FUNCTION_SYMBOL] = true,
                _b.sourceCodeInfo = (_d = getTokenDebugData(node.token)) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo,
                _b.t = FunctionType.UserDefined,
                _b.n = name,
                _b.o = evaluatedFunctionOverloades,
                _b);
            contextStack.globalContext[name] = { value: litsFunction };
            return null;
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var _b;
            var _c;
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin, evaluateAstNode = _a.evaluateAstNode;
            var sourceCodeInfo = (_c = getTokenDebugData(node.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo;
            var name = evaluateAstNode(asAstNode(node.f, sourceCodeInfo), contextStack);
            assertString(name, sourceCodeInfo);
            assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo);
            contextStack.globalContext[name] = { value: true };
            var newContext = (_b = {}, _b[name] = { value: true }, _b);
            return addOverloadsUnresolvedIdentifiers(node.o, contextStack, findUnresolvedIdentifiers, builtin, newContext);
        },
    };
    var fnSpecialExpression = {
        polishParse: function (tokenStream, parseState, firstToken, parsers) {
            var functionOverloades = parseFunctionOverloades(tokenStream, parseState, parsers);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'fn',
                p: [],
                o: functionOverloades,
                token: getTokenDebugData(firstToken) && firstToken,
            };
            return node;
        },
        validateParameterCount: function () { return undefined; },
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
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return addOverloadsUnresolvedIdentifiers(node.o, contextStack, findUnresolvedIdentifiers, builtin);
        },
    };
    function getFunctionName(expressionName, node, contextStack, evaluateAstNode) {
        var _a;
        var sourceCodeInfo = (_a = getTokenDebugData(node.token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo;
        if (expressionName === 'defn')
            return (node.f).v;
        var name = evaluateAstNode(node.f, contextStack);
        return asString(name, sourceCodeInfo);
    }
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
    function addOverloadsUnresolvedIdentifiers(overloads, contextStack, findUnresolvedIdentifiers, builtin, functionNameContext) {
        var e_3, _a;
        var result = new Set();
        var contextStackWithFunctionName = functionNameContext ? contextStack.create(functionNameContext) : contextStack;
        var _loop_1 = function (overload) {
            var newContext = {};
            overload.as.b.forEach(function (binding) {
                var bindingResult = findUnresolvedIdentifiers([binding.v], contextStack, builtin);
                addAnalyzeResults(result, bindingResult);
                newContext[binding.n] = { value: true };
            });
            overload.as.m.forEach(function (arg) {
                newContext[arg] = { value: true };
            });
            if (typeof overload.as.r === 'string')
                newContext[overload.as.r] = { value: true };
            var newContextStack = contextStackWithFunctionName.create(newContext);
            var overloadResult = findUnresolvedIdentifiers(overload.b, newContextStack, builtin);
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
        if (typeof arity === 'number') {
            return overloadedFunctions.every(function (fun) {
                if (typeof fun.a === 'number')
                    return fun.a !== arity;
                return fun.a.min > arity;
            });
        }
        return overloadedFunctions.every(function (fun) {
            if (typeof fun.a === 'number')
                return fun.a < arity.min;
            return false;
        });
    }
    function parseFunctionBody(tokenStream, parseState, _a) {
        var _b;
        var parseToken = _a.parseToken;
        var tkn = asToken(tokenStream.tokens[parseState.position]);
        var body = [];
        while (!isRParenToken(tkn)) {
            body.push(parseToken(tokenStream, parseState));
            tkn = asToken(tokenStream.tokens[parseState.position]);
        }
        if (body.length === 0)
            throw new LitsError('Missing body in function', (_b = getTokenDebugData(tkn)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
        return body;
    }
    function parseFunctionOverloades(tokenStream, parseState, parsers) {
        var _a, _b, _c;
        var tkn = asToken(tokenStream.tokens[parseState.position]);
        if (isLParenToken(tkn)) {
            var functionOverloades = [];
            while (!isRParenToken(tkn)) {
                parseState.position++;
                tkn = asLBracketToken(tokenStream.tokens[parseState.position]);
                var functionArguments = parseFunctionArguments(tokenStream, parseState, parsers);
                var arity = functionArguments.r ? { min: functionArguments.m.length } : functionArguments.m.length;
                if (!arityOk(functionOverloades, arity))
                    throw new LitsError('All overloaded functions must have different arity', (_a = getTokenDebugData(tkn)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                var functionBody = parseFunctionBody(tokenStream, parseState, parsers);
                functionOverloades.push({
                    as: functionArguments,
                    b: functionBody,
                    a: arity,
                });
                tkn = asToken(tokenStream.tokens[++parseState.position]);
                if (!isRParenToken(tkn) && !isLParenToken(tkn))
                    throw new LitsError("Expected ( or ) token, got ".concat(valueToString(tkn), "."), (_b = getTokenDebugData(tkn)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
            }
            return functionOverloades;
        }
        else if (isLBracketToken(tkn)) {
            var functionArguments = parseFunctionArguments(tokenStream, parseState, parsers);
            var arity = functionArguments.r ? { min: functionArguments.m.length } : functionArguments.m.length;
            var functionBody = parseFunctionBody(tokenStream, parseState, parsers);
            return [
                {
                    as: functionArguments,
                    b: functionBody,
                    a: arity,
                },
            ];
        }
        else {
            throw new LitsError("Expected [ or ( token, got ".concat(valueToString(tkn)), (_c = getTokenDebugData(tkn)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
        }
    }
    function parseFunctionArguments(tokenStream, parseState, parsers) {
        var _a, _b, _c, _d, _e;
        var parseArgument = parsers.parseArgument, parseBindings = parsers.parseBindings;
        var bindings = [];
        var restArgument;
        var mandatoryArguments = [];
        var state = 'mandatory';
        var tkn = asToken(tokenStream.tokens[++parseState.position]);
        // let tkn = asToken(tokenStream.tokens[parseState.position])
        while (!isRBracketToken(tkn)) {
            if (state === 'let') {
                bindings = parseBindings(tokenStream, parseState);
                break;
            }
            else {
                var node = parseArgument(tokenStream, parseState);
                tkn = asToken(tokenStream.tokens[parseState.position]);
                if (node.t === AstNodeType.Modifier) {
                    switch (node.v) {
                        case '&rest':
                            if (state === 'rest')
                                throw new LitsError('& can only appear once', (_a = getTokenDebugData(tkn)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                            state = 'rest';
                            break;
                        case '&let':
                            if (state === 'rest' && !restArgument)
                                throw new LitsError('No rest argument was specified', (_b = getTokenDebugData(tkn)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
                            state = 'let';
                            break;
                        default:
                            throw new LitsError("Illegal modifier: ".concat(node.v), (_c = getTokenDebugData(tkn)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
                    }
                }
                else {
                    switch (state) {
                        case 'mandatory':
                            mandatoryArguments.push(node.n);
                            break;
                        case 'rest':
                            if (restArgument !== undefined)
                                throw new LitsError('Can only specify one rest argument', (_d = getTokenDebugData(tkn)) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
                            restArgument = node.n;
                            break;
                    }
                }
            }
        }
        parseState.position += 1;
        if (state === 'rest' && restArgument === undefined)
            throw new LitsError('Missing rest argument name', (_e = getTokenDebugData(tkn)) === null || _e === void 0 ? void 0 : _e.sourceCodeInfo);
        var args = {
            m: mandatoryArguments,
            r: restArgument,
            b: bindings,
        };
        return args;
    }

    var ifSpecialExpression = {
        polishParse: getCommonPolishSpecialExpressionParser('if'),
        validateParameterCount: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
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
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return findUnresolvedIdentifiers(node.p, contextStack, builtin);
        },
    };

    var ifLetSpecialExpression = {
        polishParse: function (tokenStream, parseState, firstToken, _a) {
            var _b, _c;
            var parseBindings = _a.parseBindings, parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var bindings = parseBindings(tokenStream, parseState);
            if (bindings.length !== 1) {
                throw new LitsError("Expected exactly one binding, got ".concat(valueToString(bindings.length)), (_b = getTokenDebugData(firstToken)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
            }
            var params = parseTokensUntilClosingBracket(tokenStream, parseState);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'if_let',
                b: asNonUndefined(bindings[0], (_c = getTokenDebugData(firstToken)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo),
                p: params,
                token: getTokenDebugData(firstToken) && firstToken,
            };
            return node;
        },
        validateParameterCount: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
        evaluate: function (node, contextStack, _a) {
            var _b;
            var evaluateAstNode = _a.evaluateAstNode;
            var sourceCodeInfo = (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo;
            var locals = {};
            var bindingValue = evaluateAstNode(node.b.v, contextStack);
            if (bindingValue) {
                locals[node.b.n] = { value: bindingValue };
                var newContextStack = contextStack.create(locals);
                var thenForm = asAstNode(node.p[0], sourceCodeInfo);
                return evaluateAstNode(thenForm, newContextStack);
            }
            if (node.p.length === 2) {
                var elseForm = asAstNode(node.p[1], sourceCodeInfo);
                return evaluateAstNode(elseForm, contextStack);
            }
            return null;
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var _b;
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            var newContext = (_b = {}, _b[node.b.n] = { value: true }, _b);
            var bindingResult = findUnresolvedIdentifiers([node.b.v], contextStack, builtin);
            var paramsResult = findUnresolvedIdentifiers(node.p, contextStack.create(newContext), builtin);
            return joinAnalyzeResults(bindingResult, paramsResult);
        },
    };

    var ifNotSpecialExpression = {
        polishParse: getCommonPolishSpecialExpressionParser('if_not'),
        validateParameterCount: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
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
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return findUnresolvedIdentifiers(node.p, contextStack, builtin);
        },
    };

    var letSpecialExpression = {
        polishParse: function (tokenStream, parseState, firstToken, _a) {
            var parseBindings = _a.parseBindings, parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var bindings = parseBindings(tokenStream, parseState);
            var params = parseTokensUntilClosingBracket(tokenStream, parseState);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'let',
                p: params,
                bs: bindings,
                token: getTokenDebugData(firstToken) && firstToken,
            };
            return node;
        },
        validateParameterCount: function () { return undefined; },
        evaluate: function (node, contextStack, _a) {
            var e_1, _b, e_2, _c;
            var evaluateAstNode = _a.evaluateAstNode;
            var locals = {};
            var newContextStack = contextStack.create(locals);
            try {
                for (var _d = __values(node.bs), _e = _d.next(); !_e.done; _e = _d.next()) {
                    var binding = _e.value;
                    var bindingValueNode = binding.v;
                    var bindingValue = evaluateAstNode(bindingValueNode, newContextStack);
                    locals[binding.n] = { value: bindingValue };
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
                for (var _f = __values(node.p), _g = _f.next(); !_g.done; _g = _f.next()) {
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
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            var newContext = node.bs
                .map(function (binding) { return binding.n; })
                .reduce(function (context, name) {
                context[name] = { value: true };
                return context;
            }, {});
            var bindingContext = {};
            var bindingResults = node.bs.map(function (bindingNode) {
                var valueNode = bindingNode.v;
                var bindingsResult = findUnresolvedIdentifiers([valueNode], contextStack.create(bindingContext), builtin);
                bindingContext[bindingNode.n] = { value: true };
                return bindingsResult;
            });
            var paramsResult = findUnresolvedIdentifiers(node.p, contextStack.create(newContext), builtin);
            return joinAnalyzeResults.apply(void 0, __spreadArray(__spreadArray([], __read(bindingResults), false), [paramsResult], false));
        },
    };

    var loopSpecialExpression = {
        polishParse: function (tokenStream, parseState, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket, parseBindings = _a.parseBindings;
            var bindings = parseBindings(tokenStream, parseState);
            var params = parseTokensUntilClosingBracket(tokenStream, parseState);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'loop',
                p: params,
                bs: bindings,
                token: getTokenDebugData(firstToken) && firstToken,
            };
            return node;
        },
        validateParameterCount: function () { return undefined; },
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
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            var newContext = node.bs
                .map(function (binding) { return binding.n; })
                .reduce(function (context, name) {
                context[name] = { value: true };
                return context;
            }, {});
            var bindingValueNodes = node.bs.map(function (binding) { return binding.v; });
            var bindingsResult = findUnresolvedIdentifiers(bindingValueNodes, contextStack, builtin);
            var paramsResult = findUnresolvedIdentifiers(node.p, contextStack.create(newContext), builtin);
            return joinAnalyzeResults(bindingsResult, paramsResult);
        },
    };

    function parseLoopBinding(tokenStream, parseState, _a) {
        var _b, _c, _d, _e, _f;
        var parseBinding = _a.parseBinding, parseBindings = _a.parseBindings, parseToken = _a.parseToken;
        var bindingNode = parseBinding(tokenStream, parseState);
        var loopBinding = {
            b: bindingNode,
            m: [],
        };
        var tkn = asToken(tokenStream.tokens[parseState.position]);
        while (isP_ModifierToken(tkn)) {
            var modifier = tkn[1];
            switch (modifier) {
                case '&let':
                    if (loopBinding.l) {
                        throw new LitsError('Only one &let modifier allowed', (_b = getTokenDebugData(tkn)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
                    }
                    parseState.position += 1;
                    loopBinding.l = parseBindings(tokenStream, parseState);
                    loopBinding.m.push('&let');
                    break;
                case '&when':
                    if (loopBinding.wn) {
                        throw new LitsError('Only one &when modifier allowed', (_c = getTokenDebugData(tkn)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
                    }
                    parseState.position += 1;
                    loopBinding.wn = parseToken(tokenStream, parseState);
                    loopBinding.m.push('&when');
                    break;
                case '&while':
                    if (loopBinding.we) {
                        throw new LitsError('Only one &while modifier allowed', (_d = getTokenDebugData(tkn)) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
                    }
                    parseState.position += 1;
                    loopBinding.we = parseToken(tokenStream, parseState);
                    loopBinding.m.push('&while');
                    break;
                case '&rest':
                    throw new LitsError("Illegal modifier: ".concat(modifier), (_e = getTokenDebugData(tkn)) === null || _e === void 0 ? void 0 : _e.sourceCodeInfo);
                default:
                    throw new LitsError("Illegal modifier: ".concat(modifier), (_f = getTokenDebugData(tkn)) === null || _f === void 0 ? void 0 : _f.sourceCodeInfo);
            }
            tkn = asToken(tokenStream.tokens[parseState.position]);
        }
        return loopBinding;
    }
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
    function parseLoopBindings(tokenStream, parseState, parsers) {
        assertLBracketToken(tokenStream.tokens[parseState.position++]);
        var loopBindings = [];
        var tkn = asToken(tokenStream.tokens[parseState.position]);
        while (!isRBracketToken(tkn)) {
            loopBindings.push(parseLoopBinding(tokenStream, parseState, parsers));
            tkn = asToken(tokenStream.tokens[parseState.position]);
        }
        parseState.position += 1;
        return loopBindings;
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
    function analyze$2(node, contextStack, findUnresolvedIdentifiers, builtin) {
        var result = new Set();
        var newContext = {};
        var loopBindings = node.l;
        loopBindings.forEach(function (loopBinding) {
            var binding = loopBinding.b, letBindings = loopBinding.l, whenNode = loopBinding.wn, whileNode = loopBinding.we;
            findUnresolvedIdentifiers([binding.v], contextStack.create(newContext), builtin).forEach(function (symbol) {
                return result.add(symbol);
            });
            newContext[binding.n] = { value: true };
            if (letBindings) {
                letBindings.forEach(function (letBinding) {
                    findUnresolvedIdentifiers([letBinding.v], contextStack.create(newContext), builtin).forEach(function (symbol) {
                        return result.add(symbol);
                    });
                    newContext[letBinding.n] = { value: true };
                });
            }
            if (whenNode) {
                findUnresolvedIdentifiers([whenNode], contextStack.create(newContext), builtin).forEach(function (symbol) {
                    return result.add(symbol);
                });
            }
            if (whileNode) {
                findUnresolvedIdentifiers([whileNode], contextStack.create(newContext), builtin).forEach(function (symbol) {
                    return result.add(symbol);
                });
            }
        });
        findUnresolvedIdentifiers(node.p, contextStack.create(newContext), builtin).forEach(function (symbol) {
            return result.add(symbol);
        });
        return result;
    }
    var forSpecialExpression = {
        polishParse: function (tokenStream, parseState, firstToken, parsers) {
            var parseTokensUntilClosingBracket = parsers.parseTokensUntilClosingBracket;
            var loopBindings = parseLoopBindings(tokenStream, parseState, parsers);
            var params = parseTokensUntilClosingBracket(tokenStream, parseState);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            var node = {
                n: 'for',
                t: AstNodeType.SpecialExpression,
                l: loopBindings,
                p: params,
                token: getTokenDebugData(firstToken) && firstToken,
            };
            return node;
        },
        validateParameterCount: function (node) { return assertNumberOfParams(1, node); },
        evaluate: function (node, contextStack, helpers) { return evaluateLoop(true, node, contextStack, helpers.evaluateAstNode); },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return analyze$2(node, contextStack, findUnresolvedIdentifiers, builtin);
        },
    };
    var doseqSpecialExpression = {
        polishParse: function (tokenStream, parseState, firstToken, parsers) {
            var parseTokensUntilClosingBracket = parsers.parseTokensUntilClosingBracket;
            var loopBindings = parseLoopBindings(tokenStream, parseState, parsers);
            var params = parseTokensUntilClosingBracket(tokenStream, parseState);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            var node = {
                n: 'doseq',
                t: AstNodeType.SpecialExpression,
                l: loopBindings,
                p: params,
                token: getTokenDebugData(firstToken) && firstToken,
            };
            return node;
        },
        validateParameterCount: function (node) { return assertNumberOfParams(1, node); },
        evaluate: function (node, contextStack, helpers) {
            evaluateLoop(false, node, contextStack, helpers.evaluateAstNode);
            return null;
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return analyze$2(node, contextStack, findUnresolvedIdentifiers, builtin);
        },
    };

    var orSpecialExpression = {
        polishParse: getCommonPolishSpecialExpressionParser('||'),
        validateParameterCount: function () { return undefined; },
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
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return findUnresolvedIdentifiers(node.p, contextStack, builtin);
        },
    };

    var qqSpecialExpression = {
        polishParse: getCommonPolishSpecialExpressionParser('??'),
        validateParameterCount: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
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
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return findUnresolvedIdentifiers(node.p, contextStack, builtin);
        },
    };

    var recurSpecialExpression = {
        polishParse: function (tokenStream, parseState, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var params = parseTokensUntilClosingBracket(tokenStream, parseState);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'recur',
                p: params,
                token: getTokenDebugData(firstToken) && firstToken,
            };
            return node;
        },
        validateParameterCount: function () { return undefined; },
        evaluate: function (node, contextStack, _a) {
            var evaluateAstNode = _a.evaluateAstNode;
            var params = node.p.map(function (paramNode) { return evaluateAstNode(paramNode, contextStack); });
            throw new RecurSignal(params);
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return findUnresolvedIdentifiers(node.p, contextStack, builtin);
        },
    };

    var throwSpecialExpression = {
        polishParse: getCommonPolishSpecialExpressionParser('throw'),
        validateParameterCount: function (node) { return assertNumberOfParams(1, node); },
        evaluate: function (node, contextStack, _a) {
            var _b, _c;
            var evaluateAstNode = _a.evaluateAstNode;
            var message = asString(evaluateAstNode(node.p[0], contextStack), (_b = getTokenDebugData(node.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo, {
                nonEmpty: true,
            });
            throw new UserDefinedError(message, (_c = getTokenDebugData(node.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return findUnresolvedIdentifiers(node.p, contextStack, builtin);
        },
    };

    var trySpecialExpression = {
        polishParse: function (tokenStream, parseState, firstToken, _a) {
            var _b, _c, _d;
            var parseToken = _a.parseToken;
            var tryExpression = parseToken(tokenStream, parseState);
            assertLParenToken(tokenStream.tokens[parseState.position++]);
            var catchNode = parseToken(tokenStream, parseState);
            assertSymbolNode(catchNode, (_b = getTokenDebugData(catchNode.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
            if (catchNode.v !== 'catch') {
                throw new LitsError("Expected 'catch', got '".concat(catchNode.v, "'."), getSourceCodeInfo(catchNode, (_c = getTokenDebugData(catchNode.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo));
            }
            var error = parseToken(tokenStream, parseState);
            assertSymbolNode(error, (_d = getTokenDebugData(error.token)) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
            var catchExpression = parseToken(tokenStream, parseState);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'try',
                p: [tryExpression],
                ce: catchExpression,
                e: error,
                token: getTokenDebugData(firstToken) && firstToken,
            };
            return node;
        },
        validateParameterCount: function (node) { return assertNumberOfParams(1, node); },
        evaluate: function (node, contextStack, _a) {
            var _b;
            var _c;
            var evaluateAstNode = _a.evaluateAstNode;
            var tryExpressions = node.p, catchExpression = node.ce, errorNode = node.e;
            try {
                return evaluateAstNode(tryExpressions[0], contextStack);
            }
            catch (error) {
                var newContext = (_b = {},
                    _b[errorNode.v] = { value: asAny(error, (_c = getTokenDebugData(node.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo) },
                    _b);
                return evaluateAstNode(catchExpression, contextStack.create(newContext));
            }
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var _b;
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            var tryExpressions = node.p, catchExpression = node.ce, errorNode = node.e;
            var tryResult = findUnresolvedIdentifiers(tryExpressions, contextStack, builtin);
            var newContext = (_b = {},
                _b[errorNode.v] = { value: true },
                _b);
            var catchResult = findUnresolvedIdentifiers([catchExpression], contextStack.create(newContext), builtin);
            return joinAnalyzeResults(tryResult, catchResult);
        },
    };

    var whenSpecialExpression = {
        polishParse: getCommonPolishSpecialExpressionParser('when'),
        validateParameterCount: function (node) { return assertNumberOfParams({ min: 1 }, node); },
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var _c;
            var evaluateAstNode = _a.evaluateAstNode;
            var _d = __read(node.p), whenExpression = _d[0], body = _d.slice(1);
            assertAstNode(whenExpression, (_c = getTokenDebugData(node.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
            if (!evaluateAstNode(whenExpression, contextStack))
                return null;
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
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return findUnresolvedIdentifiers(node.p, contextStack, builtin);
        },
    };

    var whenFirstSpecialExpression = {
        polishParse: function (tokenStream, parseState, firstToken, _a) {
            var _b, _c;
            var parseBindings = _a.parseBindings, parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var bindings = parseBindings(tokenStream, parseState);
            if (bindings.length !== 1) {
                throw new LitsError("Expected exactly one binding, got ".concat(valueToString(bindings.length)), (_b = getTokenDebugData(firstToken)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
            }
            var params = parseTokensUntilClosingBracket(tokenStream, parseState);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'when_first',
                b: asNonUndefined(bindings[0], (_c = getTokenDebugData(firstToken)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo),
                p: params,
                token: getTokenDebugData(firstToken) && firstToken,
            };
            return node;
        },
        validateParameterCount: function () { return undefined; },
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var _c;
            var evaluateAstNode = _a.evaluateAstNode;
            var locals = {};
            var binding = node.b;
            var evaluatedBindingForm = evaluateAstNode(binding.v, contextStack);
            if (!isSeq(evaluatedBindingForm)) {
                throw new LitsError("Expected undefined or a sequence, got ".concat(valueToString(evaluatedBindingForm)), (_c = getTokenDebugData(node.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
            }
            if (evaluatedBindingForm.length === 0)
                return null;
            var bindingValue = toAny(evaluatedBindingForm[0]);
            locals[binding.n] = { value: bindingValue };
            var newContextStack = contextStack.create(locals);
            var result = null;
            try {
                for (var _d = __values(node.p), _e = _d.next(); !_e.done; _e = _d.next()) {
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
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var _b;
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            var binding = node.b;
            var newContext = (_b = {}, _b[binding.n] = { value: true }, _b);
            var bindingResult = findUnresolvedIdentifiers([binding.v], contextStack, builtin);
            var paramsResult = findUnresolvedIdentifiers(node.p, contextStack.create(newContext), builtin);
            return joinAnalyzeResults(bindingResult, paramsResult);
        },
    };

    var whenLetSpecialExpression = {
        polishParse: function (tokenStream, parseState, firstToken, _a) {
            var _b, _c;
            var parseBindings = _a.parseBindings, parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var bindings = parseBindings(tokenStream, parseState);
            if (bindings.length !== 1) {
                throw new LitsError("Expected exactly one binding, got ".concat(valueToString(bindings.length)), (_b = getTokenDebugData(firstToken)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
            }
            var params = parseTokensUntilClosingBracket(tokenStream, parseState);
            assertRParenToken(tokenStream.tokens[parseState.position++]);
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'when_let',
                b: asNonUndefined(bindings[0], (_c = getTokenDebugData(firstToken)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo),
                p: params,
                token: getTokenDebugData(firstToken) && firstToken,
            };
            return node;
        },
        validateParameterCount: function () { return undefined; },
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var evaluateAstNode = _a.evaluateAstNode;
            var binding = node.b;
            var locals = {};
            var bindingValue = evaluateAstNode(binding.v, contextStack);
            if (!bindingValue)
                return null;
            locals[binding.n] = { value: bindingValue };
            var newContextStack = contextStack.create(locals);
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
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var _b;
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            var binding = node.b;
            var newContext = (_b = {}, _b[binding.n] = { value: true }, _b);
            var bindingResult = findUnresolvedIdentifiers([binding.v], contextStack, builtin);
            var paramsResult = findUnresolvedIdentifiers(node.p, contextStack.create(newContext), builtin);
            return joinAnalyzeResults(bindingResult, paramsResult);
        },
    };

    var whenNotSpecialExpression = {
        polishParse: getCommonPolishSpecialExpressionParser('when_not'),
        validateParameterCount: function (node) { return assertNumberOfParams({ min: 1 }, node); },
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var _c;
            var evaluateAstNode = _a.evaluateAstNode;
            var _d = __read(node.p), whenExpression = _d[0], body = _d.slice(1);
            assertAstNode(whenExpression, (_c = getTokenDebugData(node.token)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
            if (evaluateAstNode(whenExpression, contextStack))
                return null;
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
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return findUnresolvedIdentifiers(node.p, contextStack, builtin);
        },
    };

    var specialExpressions = {
        '&&': andSpecialExpression,
        'comment': commentSpecialExpression,
        'cond': condSpecialExpression,
        'def': defSpecialExpression,
        'defn': defnSpecialExpression,
        'defns': defnsSpecialExpression,
        'defs': defsSpecialExpression,
        'do': doSpecialExpression,
        'doseq': doseqSpecialExpression,
        'for': forSpecialExpression,
        'fn': fnSpecialExpression,
        'if': ifSpecialExpression,
        'if_let': ifLetSpecialExpression,
        'if_not': ifNotSpecialExpression,
        'let': letSpecialExpression,
        'loop': loopSpecialExpression,
        '||': orSpecialExpression,
        'recur': recurSpecialExpression,
        'throw': throwSpecialExpression,
        'try': trySpecialExpression,
        'when': whenSpecialExpression,
        'when_first': whenFirstSpecialExpression,
        'when_let': whenLetSpecialExpression,
        'when_not': whenNotSpecialExpression,
        'declared?': declaredSpecialExpression,
        '??': qqSpecialExpression,
    };
    Object.keys(specialExpressions).forEach(function (key) {
        /* v8 ignore next 2 */
        if (normalExpressions[key])
            throw new Error("Expression ".concat(key, " is defined as both a normal expression and a special expression"));
    });
    var builtin = {
        normalExpressions: normalExpressions,
        specialExpressions: specialExpressions,
    };
    var normalExpressionKeys = Object.keys(normalExpressions);
    var specialExpressionKeys = Object.keys(specialExpressions);

    function isContextEntry(value) {
        return isUnknownRecord(value) && value.value !== undefined;
    }

    var ContextStackImpl = /** @class */ (function () {
        function ContextStackImpl(_a) {
            var contexts = _a.contexts, hostValues = _a.values, lazyHostValues = _a.lazyValues, nativeJsFunctions = _a.nativeJsFunctions;
            this.contexts = contexts;
            this.globalContext = asNonUndefined(contexts[0]);
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
        return contextStack;
    }

    var _a$1;
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
    var functionExecutors = (_a$1 = {},
        _a$1[FunctionType.NativeJsFunction] = function (fn, params, sourceCodeInfo) {
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
        _a$1[FunctionType.UserDefined] = function (fn, params, sourceCodeInfo, contextStack, _a) {
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
        _a$1[FunctionType.Partial] = function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return executeFunction(fn.f, __spreadArray(__spreadArray([], __read(fn.p), false), __read(params), false), contextStack, sourceCodeInfo);
        },
        _a$1[FunctionType.Comp] = function (fn, params, sourceCodeInfo, contextStack, _a) {
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
        _a$1[FunctionType.Constantly] = function (fn) {
            return fn.v;
        },
        _a$1[FunctionType.Juxt] = function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return fn.f.map(function (fun) { return executeFunction(toAny(fun), params, contextStack, sourceCodeInfo); });
        },
        _a$1[FunctionType.Complement] = function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return !executeFunction(fn.f, params, contextStack, sourceCodeInfo);
        },
        _a$1[FunctionType.EveryPred] = function (fn, params, sourceCodeInfo, contextStack, _a) {
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
        _a$1[FunctionType.SomePred] = function (fn, params, sourceCodeInfo, contextStack, _a) {
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
        _a$1[FunctionType.Fnil] = function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var fniledParams = params.map(function (param, index) { return (param === null ? toAny(fn.p[index]) : param); });
            return executeFunction(toAny(fn.f), fniledParams, contextStack, sourceCodeInfo);
        },
        _a$1[FunctionType.Builtin] = function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var normalExpression = asNonUndefined(normalExpressions[fn.n], sourceCodeInfo);
            return normalExpression.evaluate(params, sourceCodeInfo, contextStack, { executeFunction: executeFunction });
        },
        _a$1);

    function evaluate(ast, contextStack) {
        var e_1, _a;
        var result = null;
        var safeAstNode = ast.hasDebugData ? JSON.parse(JSON.stringify(ast)) : ast;
        if (safeAstNode.hasDebugData)
            removeCommenNodes(safeAstNode);
        try {
            for (var _b = __values(safeAstNode.b), _c = _b.next(); !_c.done; _c = _b.next()) {
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
        return asNonUndefined(polishReservedNamesRecord[node.v], (_a = getTokenDebugData(node.token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo).value;
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

    var findUnresolvedIdentifiers = function (ast, contextStack, builtin) {
        var e_1, _a;
        var astNodes = Array.isArray(ast) ? ast : ast.b;
        var unresolvedIdentifiers = new Set();
        try {
            for (var astNodes_1 = __values(astNodes), astNodes_1_1 = astNodes_1.next(); !astNodes_1_1.done; astNodes_1_1 = astNodes_1.next()) {
                var subNode = astNodes_1_1.value;
                findUnresolvedIdentifiersInAstNode(subNode, contextStack, builtin)
                    .forEach(function (symbol) { return unresolvedIdentifiers.add(symbol); });
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (astNodes_1_1 && !astNodes_1_1.done && (_a = astNodes_1.return)) _a.call(astNodes_1);
            }
            finally { if (e_1) throw e_1.error; }
        }
        return unresolvedIdentifiers;
    };
    function findUnresolvedIdentifiersInAstNode(astNode, contextStack, builtin) {
        var e_2, _a;
        var _b;
        var emptySet = new Set();
        switch (astNode.t) {
            case AstNodeType.Symbol: {
                var lookUpResult = contextStack.lookUp(astNode);
                if (lookUpResult === null)
                    return new Set([{ symbol: astNode.v, token: astNode.token }]);
                return emptySet;
            }
            case AstNodeType.String:
            case AstNodeType.Number:
            case AstNodeType.Modifier:
            case AstNodeType.ReservedSymbol:
            case AstNodeType.Comment:
                return emptySet;
            case AstNodeType.NormalExpression: {
                var unresolvedIdentifiers_1 = new Set();
                var name_1 = astNode.n, debug = astNode.token;
                if (typeof name_1 === 'string') {
                    var lookUpResult = contextStack.lookUp({ t: AstNodeType.Symbol, v: name_1, token: debug, p: [], n: undefined });
                    if (lookUpResult === null)
                        unresolvedIdentifiers_1.add({ symbol: name_1, token: astNode.token });
                }
                try {
                    for (var _c = __values(astNode.p), _d = _c.next(); !_d.done; _d = _c.next()) {
                        var subNode = _d.value;
                        var innerUnresolvedIdentifiers = findUnresolvedIdentifiersInAstNode(subNode, contextStack, builtin);
                        innerUnresolvedIdentifiers.forEach(function (symbol) { return unresolvedIdentifiers_1.add(symbol); });
                    }
                }
                catch (e_2_1) { e_2 = { error: e_2_1 }; }
                finally {
                    try {
                        if (_d && !_d.done && (_a = _c.return)) _a.call(_c);
                    }
                    finally { if (e_2) throw e_2.error; }
                }
                return unresolvedIdentifiers_1;
            }
            case AstNodeType.SpecialExpression: {
                var specialExpression = asNonUndefined(builtin.specialExpressions[astNode.n], (_b = getTokenDebugData(astNode.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
                // eslint-disable-next-line ts/no-unsafe-argument
                var unresolvedIdentifiers = specialExpression.findUnresolvedIdentifiers(astNode, contextStack, {
                    findUnresolvedIdentifiers: findUnresolvedIdentifiers,
                    builtin: builtin,
                    evaluateAstNode: evaluateAstNode,
                });
                return unresolvedIdentifiers;
            }
        }
    }

    var calculateAndOutcomes = function (_a) {
        var astNode = _a.astNode, combinateAstNodes = _a.combinateAstNodes;
        return combinateAstNodes(astNode.p)
            .map(function (p) { return ({
            n: '&&',
            t: AstNodeType.SpecialExpression,
            p: p,
            token: astNode.token,
        }); });
    };

    var calculateCondOutcomes = function (_a) {
        var astNode = _a.astNode, nilNode = _a.nilNode, calculatePossibleAstNodes = _a.calculatePossibleAstNodes, isAstComputable = _a.isAstComputable;
        var testNodes = arrayToPairs(astNode.p).map(function (_a) {
            var _b = __read(_a, 1), t = _b[0];
            return t;
        });
        if (isAstComputable(testNodes)) {
            return combinate(arrayToPairs(astNode.p)
                // Create a list of ast nodes from the test and form of each condition
                .reduce(function (acc, _a) {
                var _b = __read(_a, 2), test = _b[0], form = _b[1];
                acc.push(calculatePossibleAstNodes(test), calculatePossibleAstNodes(form));
                return acc;
            }, []))
                // Create a new CondNode for each combination of test and form outcomes
                .map(function (conditionAsts) { return (__assign(__assign({}, astNode), { c: arrayToPairs(conditionAsts).map(function (_a) {
                    var _b = __read(_a, 2), t = _b[0], f = _b[1];
                    return ({ t: t, f: f });
                }) })); });
        }
        return __spreadArray(__spreadArray([], __read(arrayToPairs(astNode.p).flatMap(function (_a) {
            var _b = __read(_a, 2); _b[0]; var form = _b[1];
            return calculatePossibleAstNodes(form);
        })), false), [
            nilNode,
        ], false);
    };

    var trueNode = { t: AstNodeType.ReservedSymbol, v: 'true', token: undefined, p: [], n: undefined };
    var falseNode = { t: AstNodeType.ReservedSymbol, v: 'false', token: undefined, p: [], n: undefined };
    var calculateDeclaredOutcomes = function (_a) {
        var astNode = _a.astNode, isAstComputable = _a.isAstComputable;
        if (isAstComputable(astNode.p))
            return [trueNode];
        return [trueNode, falseNode];
    };

    var calculateDefOutcomes = function (_a) {
        var astNode = _a.astNode, calculatePossibleAstNodes = _a.calculatePossibleAstNodes, addGlobalIdentifier = _a.addGlobalIdentifier;
        var nameNode = asSymbolNode(astNode.p[0]);
        var valueNode = astNode.p[1];
        addGlobalIdentifier(nameNode.v);
        return calculatePossibleAstNodes(valueNode)
            .map(function (node) { return (__assign(__assign({}, astNode), { p: [nameNode, node] })); });
    };

    var calculateDefsOutcomes = function (_a) {
        var astNode = _a.astNode, combinateAstNodes = _a.combinateAstNodes;
        return combinateAstNodes(astNode.p)
            .map(function (p) { return (__assign(__assign({}, astNode), { p: p })); });
    };

    var calculateDoOutcomes = function (_a) {
        var astNode = _a.astNode, combinateAstNodes = _a.combinateAstNodes;
        return combinateAstNodes(astNode.p).map(function (p) { return (__assign(__assign({}, astNode), { p: p })); });
    };

    function calculateFunctionOverloadOutcomes(combinateAstNodes, functionOverloads) {
        return combinate(functionOverloads
            // For each overload, calculate the possible outcomes for each parameter
            .map(function (functionOverload) {
            var _a;
            return combinateAstNodes(functionOverload.b, [
                functionOverload.as.m,
                functionOverload.as.b.map(function (bindingNode) { return bindingNode.n; }),
                (_a = functionOverload.as.r) !== null && _a !== void 0 ? _a : [],
            ].flat())
                // For each combination of parameter outcomes, create a new overload
                .map(function (body) { return (__assign(__assign({}, functionOverload), { b: body })); });
        }));
    }
    var calculateDefnOutcomes = function (_a) {
        var astNode = _a.astNode, combinateAstNodes = _a.combinateAstNodes, addGlobalIdentifier = _a.addGlobalIdentifier;
        addGlobalIdentifier(astNode.f.v);
        // astNode.o is an array of overloads
        return calculateFunctionOverloadOutcomes(combinateAstNodes, astNode.o).map(function (functionOverloads) { return (__assign(__assign({}, astNode), { o: functionOverloads })); });
    };
    var calculateDefnsOutcomes = function (_a) {
        var astNode = _a.astNode, calculatePossibleAstNodes = _a.calculatePossibleAstNodes, combinateAstNodes = _a.combinateAstNodes;
        // astNode.o is an array of overloads
        return calculatePossibleAstNodes(astNode.f).flatMap(function (functionName) {
            return calculateFunctionOverloadOutcomes(combinateAstNodes, astNode.o).map(function (functionOverloads) { return (__assign(__assign({}, astNode), { f: functionName, o: functionOverloads })); });
        });
    };
    var calculateFnOutcomes = function (_a) {
        var astNode = _a.astNode, combinateAstNodes = _a.combinateAstNodes;
        // astNode.o is an array of overloads
        return calculateFunctionOverloadOutcomes(combinateAstNodes, astNode.o).map(function (functionOverloads) { return (__assign(__assign({}, astNode), { o: functionOverloads })); });
    };

    var calculateIfLetOutcomes = function (_a) {
        var _b;
        var astNode = _a.astNode, nilNode = _a.nilNode, calculatePossibleAstNodes = _a.calculatePossibleAstNodes, combinateAstNodes = _a.combinateAstNodes, isAstComputable = _a.isAstComputable;
        var bindingNode = astNode.b;
        var thenBranch = astNode.p[0];
        var elseBranch = (_b = astNode.p[1]) !== null && _b !== void 0 ? _b : nilNode;
        if (!isAstComputable(bindingNode.v)) {
            return __spreadArray(__spreadArray([], __read(calculatePossibleAstNodes(thenBranch)), false), __read(calculatePossibleAstNodes(elseBranch)), false);
        }
        var newIdentifier = bindingNode.n;
        return calculatePossibleAstNodes(bindingNode.v)
            .map(function (bindingValue) { return (__assign(__assign({}, bindingNode), { v: bindingValue })); })
            .flatMap(function (b) { return combinateAstNodes(astNode.p, [newIdentifier])
            .map(function (p) { return (__assign(__assign({}, astNode), { b: b, p: p })); }); });
    };

    var calculateIfNotOutcomes = function (_a) {
        var _b;
        var astNode = _a.astNode, nilNode = _a.nilNode, calculatePossibleAstNodes = _a.calculatePossibleAstNodes, combinateAstNodes = _a.combinateAstNodes, isAstComputable = _a.isAstComputable;
        var condition = astNode.p[0];
        var thenBranch = astNode.p[1];
        var elseBranch = (_b = astNode.p[2]) !== null && _b !== void 0 ? _b : nilNode;
        if (isAstComputable(condition)) {
            return combinateAstNodes(astNode.p)
                .map(function (p) { return ({
                n: 'if_not',
                t: astNode.t,
                p: p,
                token: astNode.token,
            }); });
        }
        return __spreadArray(__spreadArray([], __read(calculatePossibleAstNodes(thenBranch)), false), __read(calculatePossibleAstNodes(elseBranch)), false);
    };

    var calculateIfOutcomes = function (_a) {
        var _b;
        var astNode = _a.astNode, nilNode = _a.nilNode, calculatePossibleAstNodes = _a.calculatePossibleAstNodes, combinateAstNodes = _a.combinateAstNodes, isAstComputable = _a.isAstComputable;
        var condition = astNode.p[0];
        var thenBranch = astNode.p[1];
        var elseBranch = (_b = astNode.p[2]) !== null && _b !== void 0 ? _b : nilNode;
        if (isAstComputable(condition)) {
            return combinateAstNodes(astNode.p)
                .map(function (p) { return ({
                n: 'if',
                t: astNode.t,
                p: p,
                token: astNode.token,
            }); });
        }
        return __spreadArray(__spreadArray([], __read(calculatePossibleAstNodes(thenBranch)), false), __read(calculatePossibleAstNodes(elseBranch)), false);
    };

    var calculateLetOutcomes = function (_a) {
        var astNode = _a.astNode, calculatePossibleAstNodes = _a.calculatePossibleAstNodes, combinateAstNodes = _a.combinateAstNodes, isAstComputable = _a.isAstComputable;
        try {
            // check bindings, if any binding value cannot be calculated, convert the whole let to a do-expression
            if (!isAstComputable(astNode.bs.map(function (b) { return calculatePossibleAstNodes(b.v); })))
                throw new Error('Could not calculate binding value');
        }
        catch (_b) {
            var doNodes = combinateAstNodes(astNode.p)
                .map(function (p) {
                return {
                    n: 'do',
                    t: AstNodeType.SpecialExpression,
                    p: p,
                    token: astNode.token,
                };
            });
            return doNodes;
        }
        var newIdentifiers = astNode.bs.map(function (bindingNode) { return bindingNode.n; });
        var letNodes = combinate(astNode.bs.map(function (bindingNode) {
            return calculatePossibleAstNodes(bindingNode.v)
                .map(function (bindingValues) { return (__assign(__assign({}, bindingNode), { v: bindingValues })); });
        }))
            .flatMap(function (bindingNodes) { return combinate(astNode.p.map(function (p) { return calculatePossibleAstNodes(p, newIdentifiers); }))
            .map(function (p) {
            return {
                n: 'let',
                bs: bindingNodes,
                t: AstNodeType.SpecialExpression,
                p: p,
                token: astNode.token,
            };
        }); });
        return letNodes;
    };

    var calculateForOutcomes = function (_a) {
        var astNode = _a.astNode, calculatePossibleAstNodes = _a.calculatePossibleAstNodes;
        if (!isDeterministic(calculatePossibleAstNodes, astNode))
            throw new Error('Could not calculate for loop, not deterministic');
        return [astNode];
    };
    var calculateDoSeqOutcomes = function (_a) {
        var astNode = _a.astNode, calculatePossibleAstNodes = _a.calculatePossibleAstNodes;
        if (!isDeterministic(calculatePossibleAstNodes, astNode))
            throw new Error('Could not calculate doSeq node, not deterministic');
        return [astNode];
    };
    function isDeterministic(calculatePossibleAstNodes, astNode) {
        var e_1, _a;
        try {
            for (var _b = __values(astNode.l), _c = _b.next(); !_c.done; _c = _b.next()) {
                var _d = _c.value, b = _d.b, l = _d.l, wn = _d.wn, we = _d.we;
                if (l && l.some(function (_a) {
                    var v = _a.v;
                    return !astIsDeterministic(calculatePossibleAstNodes, v);
                }))
                    return false;
                if (!astIsDeterministic(calculatePossibleAstNodes, b.v))
                    return false;
                if (wn && !astIsDeterministic(calculatePossibleAstNodes, wn))
                    return false;
                if (we && !astIsDeterministic(calculatePossibleAstNodes, we))
                    return false;
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
            }
            finally { if (e_1) throw e_1.error; }
        }
        if (!astIsDeterministic(calculatePossibleAstNodes, astNode.p[0]))
            return false;
        return true;
    }
    function astIsDeterministic(calculatePossibleAstNodes, astNode) {
        return calculatePossibleAstNodes(astNode).length === 1;
    }

    var calculateOrOutcomes = function (_a) {
        var astNode = _a.astNode, combinateAstNodes = _a.combinateAstNodes;
        return combinateAstNodes(astNode.p)
            .map(function (p) { return ({
            n: '||',
            t: AstNodeType.SpecialExpression,
            p: p,
            token: astNode.token,
        }); });
    };

    var calculateQqOutcomes = function (_a) {
        var astNode = _a.astNode, combinateAstNodes = _a.combinateAstNodes, isAstComputable = _a.isAstComputable;
        if (!isAstComputable(astNode.p[0]))
            throw new Error('First argument of ?? not computable');
        return combinateAstNodes(astNode.p)
            .map(function (p) { return (__assign(__assign({}, astNode), { p: p })); });
    };

    var calculateThrowOutcomes = function (_a) {
        var astNode = _a.astNode, calculatePossibleAstNodes = _a.calculatePossibleAstNodes;
        return calculatePossibleAstNodes(astNode.p[0]).map(function (m) { return (__assign(__assign({}, astNode), { p: [m] })); });
    };

    var calculateTryOutcomes = function (_a) {
        var astNode = _a.astNode, calculatePossibleAstNodes = _a.calculatePossibleAstNodes;
        var _b = calculatePossibleAstNodes(astNode.p[0]).reduce(function (acc, node) {
            if (node.n === 'throw') {
                acc.throws.push(node.p[0]);
            }
            else {
                acc.vals.push(node);
            }
            return acc;
        }, { vals: [], throws: [] }), vals = _b.vals, throws = _b.throws;
        var catches = throws.flatMap(function (throwNode) {
            var letNode = {
                t: AstNodeType.SpecialExpression,
                n: 'let',
                bs: [{
                        t: AstNodeType.Binding,
                        n: astNode.e.v,
                        v: throwNode,
                        token: undefined,
                        p: [],
                    }],
                p: [astNode.ce],
                token: undefined,
            };
            return calculatePossibleAstNodes(letNode);
        });
        return __spreadArray(__spreadArray([], __read(vals), false), __read(catches), false);
    };

    var calculateWhenFirstOutcomes = function (_a) {
        var astNode = _a.astNode, calculatePossibleAstNodes = _a.calculatePossibleAstNodes, combinateAstNodes = _a.combinateAstNodes, isAstComputable = _a.isAstComputable;
        var bindingNode = astNode.b;
        if (!isAstComputable(bindingNode.v))
            throw new Error('Could not calculate binding value');
        var newIdentifier = bindingNode.n;
        return calculatePossibleAstNodes(bindingNode.v)
            .map(function (bindingValue) { return (__assign(__assign({}, bindingNode), { v: bindingValue })); })
            .flatMap(function (b) {
            return combinateAstNodes(astNode.p, [newIdentifier])
                .map(function (p) { return (__assign(__assign({}, astNode), { b: b, p: p })); });
        });
    };

    var calculateWhenLetOutcomes = function (_a) {
        var astNode = _a.astNode, nilNode = _a.nilNode, calculatePossibleAstNodes = _a.calculatePossibleAstNodes, combinateAstNodes = _a.combinateAstNodes, isAstComputable = _a.isAstComputable;
        var bindingNode = astNode.b;
        if (!isAstComputable(bindingNode.v)) {
            return __spreadArray(__spreadArray([], __read(combinateAstNodes(astNode.p)
                .map(function (p) { return ({
                n: 'do',
                t: astNode.t,
                p: p,
                token: astNode.token,
            }); })), false), [
                nilNode,
            ], false);
        }
        var newIdentifier = bindingNode.n;
        return calculatePossibleAstNodes(bindingNode.v)
            .map(function (bindingValue) { return (__assign(__assign({}, bindingNode), { v: bindingValue })); })
            .flatMap(function (b) {
            return combinateAstNodes(astNode.p, [newIdentifier])
                .map(function (p) { return (__assign(__assign({}, astNode), { b: b, p: p })); });
        });
    };

    var calculateWhenNotOutcomes = function (_a) {
        var astNode = _a.astNode, combinateAstNodes = _a.combinateAstNodes, nilNode = _a.nilNode, isAstComputable = _a.isAstComputable;
        var condition = astNode.p[0];
        if (isAstComputable(condition)) {
            return combinateAstNodes(astNode.p)
                .map(function (p) { return ({
                n: 'when_not',
                t: astNode.t,
                p: p,
                token: astNode.token,
            }); });
        }
        var body = astNode.p.slice(1);
        return __spreadArray(__spreadArray([], __read(combinateAstNodes(body)
            .map(function (p) { return ({
            n: 'do',
            t: astNode.t,
            p: p,
            token: astNode.token,
        }); })), false), [
            nilNode,
        ], false);
    };

    var calculateWhenOutcomes = function (_a) {
        var astNode = _a.astNode, combinateAstNodes = _a.combinateAstNodes, nilNode = _a.nilNode, isAstComputable = _a.isAstComputable;
        var condition = astNode.p[0];
        if (isAstComputable(condition)) {
            return combinateAstNodes(astNode.p)
                .map(function (p) { return ({
                n: 'when',
                t: astNode.t,
                p: p,
                token: astNode.token,
            }); });
        }
        var body = astNode.p.slice(1);
        return __spreadArray(__spreadArray([], __read(combinateAstNodes(body)
            .map(function (p) { return ({
            n: 'do',
            t: astNode.t,
            p: p,
            token: astNode.token,
        }); })), false), [
            nilNode,
        ], false);
    };

    var calculateRecurOutcomes = function (_a) {
        var astNode = _a.astNode, combinateAstNodes = _a.combinateAstNodes;
        return combinateAstNodes(astNode.p)
            .map(function (p) { return (__assign(__assign({}, astNode), { p: p })); });
    };

    var calculateCommentOutcomes = function (_a) {
        var nilNode = _a.nilNode;
        return [nilNode];
    };

    var calculateLoopOutcomes = function (_a) {
        var astNode = _a.astNode, combinateAstNodes = _a.combinateAstNodes;
        return combinateAstNodes(astNode.p, astNode.bs.map(function (bindingNode) { return bindingNode.n; }))
            .map(function (p) { return (__assign(__assign({}, astNode), { p: p })); });
    };

    var specialExpressionCalculator = {
        '&&': function (astNode, helperOptions) { return calculateAndOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'comment': function (astNode, helperOptions) { return calculateCommentOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'cond': function (astNode, helperOptions) { return calculateCondOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'declared?': function (astNode, helperOptions) { return calculateDeclaredOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'defn': function (astNode, helperOptions) { return calculateDefnOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'def': function (astNode, helperOptions) { return calculateDefOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'defns': function (astNode, helperOptions) { return calculateDefnsOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'defs': function (astNode, helperOptions) { return calculateDefsOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'do': function (astNode, helperOptions) { return calculateDoOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'doseq': function (astNode, helperOptions) { return calculateDoSeqOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'fn': function (astNode, helperOptions) { return calculateFnOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'for': function (astNode, helperOptions) { return calculateForOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'if_let': function (astNode, helperOptions) { return calculateIfLetOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'if': function (astNode, helperOptions) { return calculateIfOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'if_not': function (astNode, helperOptions) { return calculateIfNotOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'let': function (astNode, helperOptions) { return calculateLetOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'loop': function (astNode, helperOptions) { return calculateLoopOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        '||': function (astNode, helperOptions) { return calculateOrOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        '??': function (astNode, helperOptions) { return calculateQqOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'recur': function (astNode, helperOptions) { return calculateRecurOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'throw': function (astNode, helperOptions) { return calculateThrowOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'try': function (astNode, helperOptions) { return calculateTryOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'when_first': function (astNode, helperOptions) { return calculateWhenFirstOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'when_let': function (astNode, helperOptions) { return calculateWhenLetOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'when': function (astNode, helperOptions) { return calculateWhenOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'when_not': function (astNode, helperOptions) { return calculateWhenNotOutcomes(__assign({ astNode: astNode }, helperOptions)); },
    };

    function calculateOutcomes(contextStack, astNodes) {
        // First, we try to calculate outcomes for the whole astNodes array.
        // If that fails, we try to calculate outcomes for the array without the first element.
        // If that fails, we try to calculate outcomes for the array without the first two elements.
        // And so on.
        // This makes it possible to calculate outcomes for e.g.
        // (write! x) x
        // Problems occur for e.g.
        // (def x 1) (write! x) x
        // This should output [1], but since (write! x) fails to calculate outcomes, we get null.
        // Ok, but not optimal
        // The contract is that when an array is returned, it must be correct.
        // But returning null (indicating that the calculation failed) is always a way out.
        for (var i = 0; i < astNodes.length; i++) {
            var usingAstNode = astNodes.slice(i);
            var outcomes = calculateOutcomesInner(contextStack, usingAstNode);
            if (outcomes !== null)
                return outcomes;
        }
        return null;
    }
    function calculateOutcomesInner(contextStack, astNodes) {
        var e_1, _a;
        var possibleAsts = calculatePossibleAsts(contextStack.clone(), astNodes);
        if (possibleAsts === null)
            return null;
        var outcomes = [];
        try {
            for (var possibleAsts_1 = __values(possibleAsts), possibleAsts_1_1 = possibleAsts_1.next(); !possibleAsts_1_1.done; possibleAsts_1_1 = possibleAsts_1.next()) {
                var possibleAst = possibleAsts_1_1.value;
                var unresolvedIdentifiers = findUnresolvedIdentifiers(possibleAst, contextStack.clone(), builtin);
                if (unresolvedIdentifiers.size !== 0)
                    return null;
                var ast = {
                    b: possibleAst,
                    hasDebugData: true,
                };
                try {
                    outcomes.push(evaluate(ast, contextStack.clone()));
                }
                catch (e) {
                    outcomes.push(e);
                }
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (possibleAsts_1_1 && !possibleAsts_1_1.done && (_a = possibleAsts_1.return)) _a.call(possibleAsts_1);
            }
            finally { if (e_1) throw e_1.error; }
        }
        return outcomes;
    }
    function calculatePossibleAsts(contextStack, astNodes) {
        var possibleAsts;
        try {
            possibleAsts = combinate(astNodes.map(function (astNode) { return calculatePossibleAstNodes(contextStack, astNode); }));
        }
        catch (e) {
            return null;
        }
        return possibleAsts;
    }
    var nilNode = { t: AstNodeType.ReservedSymbol, v: 'nil', token: undefined, p: [], n: undefined };
    function calculatePossibleAstNodes(contextStack, astNode, newIndentifiers) {
        var newContext = newIndentifiers
            ? newIndentifiers.reduce(function (acc, identity) {
                acc[identity] = { value: null };
                return acc;
            }, {})
            : undefined;
        var newContextStack = newContext ? contextStack.create(newContext) : contextStack;
        if (astNode.t === AstNodeType.NormalExpression) {
            return combinate(astNode.p.map(function (n) { return calculatePossibleAstNodes(newContextStack, n); }))
                .map(function (p) { return (__assign(__assign({}, astNode), { p: p })); });
        }
        else if (astNode.t === AstNodeType.SpecialExpression) {
            var helperOptions = {
                nilNode: nilNode,
                calculatePossibleAstNodes: function (node, identifiers) { return calculatePossibleAstNodes(newContextStack.clone(), node, identifiers); },
                combinateAstNodes: function (nodes, identifiers) {
                    return combinate(nodes.map(function (node) { return calculatePossibleAstNodes(newContextStack.clone(), node, identifiers); }));
                },
                isAstComputable: function (node) {
                    return calculateOutcomesInner(newContextStack, Array.isArray(node) ? node.flat() : [node]) !== null;
                },
                addGlobalIdentifier: function (name) { return newContextStack.globalContext[name] = { value: null }; },
            };
            // eslint-disable-next-line ts/no-unsafe-argument
            return specialExpressionCalculator[astNode.n](astNode, helperOptions);
        }
        return [astNode];
    }

    function analyze$1(ast, params) {
        return {
            unresolvedIdentifiers: findUnresolvedIdentifiers(ast, createContextStack(params), builtin),
            outcomes: calculateOutcomes(createContextStack(params), ast.b),
        };
    }

    function parseSymbol(tokenStream, parseState) {
        var _a;
        var tkn = asToken(tokenStream.tokens[parseState.position++]);
        if (!isA_SymbolToken(tkn) && !isP_SymbolToken(tkn)) {
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
        if (!isA_ReservedSymbolToken(tkn) && !isP_ReservedSymbolToken(tkn)) {
            throw new LitsError("Expected symbol token, got ".concat(tkn[0]), (_a = getTokenDebugData(tkn)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
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
        if (isA_NumberToken(tkn)) {
            var numberString_1 = tkn[1];
            var periodToken = tokenStream.tokens[parseState.position];
            var decimalToken = tokenStream.tokens[parseState.position + 1];
            if (isA_OperatorToken(periodToken, '.') && isA_NumberToken(decimalToken)) {
                numberString_1 += ".".concat(decimalToken[1]);
                parseState.position += 2;
            }
            return {
                t: AstNodeType.Number,
                v: Number(numberString_1),
                p: [],
                n: undefined,
                token: getTokenDebugData(tkn) && tkn,
            };
        }
        if (!isP_NumberToken(tkn) && !isA_BasePrefixedNumberToken(tkn)) {
            throw new LitsError("Expected number token, got ".concat(tkn), (_a = getTokenDebugData(tkn)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
        }
        var value = tkn[1];
        var negative = value[0] === '-';
        var numberString = negative ? value.substring(1) : value;
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

    var exponentiationPrecedence = 10;
    var binaryFunctionalOperatorPrecedence = 1;
    var placeholderRegexp$1 = /^\$([1-9]\d?)?$/;
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
            case '>': // greater than
            case '>=': // greater than or equal
                return 5;
            case '==': // equal
            case '!=': // not equal
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
        var _a;
        var node = {
            t: AstNodeType.NormalExpression,
            n: name,
            p: params,
            token: getTokenDebugData(token) && token,
        };
        var builtinExpression = builtin.normalExpressions[node.n];
        if (builtinExpression) {
            (_a = builtinExpression.validate) === null || _a === void 0 ? void 0 : _a.call(builtinExpression, __assign(__assign({}, node), { p: withoutCommentNodes(node.p) }));
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
    function fromUnaryAlgebraicToAstNode(operator, operand) {
        var token = hasTokenDebugData(operator) ? operand.token : undefined;
        var operatorName = operator[1];
        switch (operatorName) {
            case '+':
            case '-':
            case '!':
            case '~':
                return createNamedNormalExpressionNode(operatorName, [operand], token);
            /* v8 ignore next 2 */
            default:
                throw new Error("Unknown operator: ".concat(operatorName));
        }
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
            case '>':
            case '>=':
            case '==':
            case '!=':
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
            case '!':
            case '~':
            case '=':
            case ',':
            case '=>':
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
        AlgebraicParser.prototype.advance = function () {
            this.parseState.position += 1;
        };
        AlgebraicParser.prototype.parse = function () {
            var nodes = [];
            while (!this.isAtEnd()) {
                nodes.push(this.parseExpression());
                if (!isA_OperatorToken(this.peek(), ';')) {
                    break;
                }
                this.advance();
            }
            return nodes;
        };
        AlgebraicParser.prototype.parseExpression = function (precedence) {
            var _a;
            if (precedence === void 0) { precedence = 0; }
            var firstToken = this.peek();
            if (isA_SymbolToken(firstToken) && firstToken[1] === 'if') {
                return this.parseIf(firstToken);
            }
            if (isA_SymbolToken(firstToken) && firstToken[1] === 'def') {
                return this.parseDef(firstToken);
            }
            if (isA_SymbolToken(firstToken) && firstToken[1] === 'defn') {
                return this.parseDefn(firstToken);
            }
            var left = this.parseOperand();
            var operator = this.peek();
            while (!this.isAtEnd()
                && !isA_OperatorToken(operator, ',')
                && !isA_OperatorToken(operator, ';')
                && !isRBracketToken(operator)
                && !isA_ReservedSymbolToken(operator, 'else')
                && !isA_ReservedSymbolToken(operator, 'then')
                && !isA_ReservedSymbolToken(operator, 'end')
                && !isRParenToken(operator)) {
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
                else if (isA_SymbolToken(operator)) {
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
                throw new LitsError('Expected operand', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
            }
            return left;
        };
        AlgebraicParser.prototype.parseOperand = function () {
            var _a, _b;
            var operand = this.parseOperandPart();
            var token = this.peek();
            while (isA_OperatorToken(token, '.') || isLBracketToken(token) || isLParenToken(token)) {
                if (token[1] === '.') {
                    this.advance();
                    var symbolToken = this.peek();
                    if (!isA_SymbolToken(symbolToken)) {
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
            else if (isA_OperatorToken(token)) {
                var operatorName = token[1];
                if (isSymbolicUnaryOperator(operatorName)) {
                    this.advance();
                    var operand = this.parseOperand();
                    if (operand === null) {
                        throw new LitsError('Expected operand', (_a = getTokenDebugData(token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                    }
                    return fromUnaryAlgebraicToAstNode(token, operand);
                }
                if (operatorName === '=>') {
                    return this.parseShorthandLamdaFunction();
                }
                else {
                    throw new Error("Unknown unary operator: ".concat(operatorName));
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
                case 'A_Number':
                case 'A_BasePrefixedNumber':
                    return parseNumber(this.tokenStream, this.parseState);
                case 'String':
                    return parseString(this.tokenStream, this.parseState);
                case 'A_Symbol': {
                    return parseSymbol(this.tokenStream, this.parseState);
                }
                case 'A_ReservedSymbol':
                    return parseReservedSymbol(this.tokenStream, this.parseState);
                case 'PolNotation': {
                    this.parseState.algebraic = false;
                    var astNodes = [];
                    this.advance();
                    do {
                        astNodes.push(this.parseState.parseToken(this.tokenStream, this.parseState));
                    } while (!isEndNotationToken(this.peek()));
                    this.advance();
                    this.parseState.algebraic = true;
                    if (astNodes.length === 1) {
                        return astNodes[0];
                    }
                    return {
                        t: AstNodeType.SpecialExpression,
                        n: 'do',
                        p: astNodes,
                        token: getTokenDebugData(token) && token,
                    };
                }
                case 'AlgNotation': {
                    this.advance();
                    var node = this.parseOperand();
                    assertEndNotationToken(this.peek());
                    this.advance();
                    return node;
                }
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
                assertA_OperatorToken(this.peek(), '=');
                this.advance();
                params.push(this.parseExpression());
                var nextToken = this.peek();
                if (!isA_OperatorToken(nextToken, ',') && !isRBraceToken(nextToken)) {
                    throw new LitsError('Expected comma or closing brace', (_c = getTokenDebugData(this.peek())) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
                }
                if (isA_OperatorToken(nextToken, ',')) {
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
                if (!isA_OperatorToken(nextToken, ',') && !isRBracketToken(nextToken)) {
                    throw new LitsError('Expected comma or closing parenthesis', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                }
                if (isA_OperatorToken(nextToken, ',')) {
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
            if (isNamedFunction && symbol.v === 'for') {
                return this.parseFor(symbol);
            }
            var params = [];
            while (!this.isAtEnd() && !isRParenToken(this.peek())) {
                params.push(this.parseExpression());
                var nextToken = this.peek();
                if (!isA_OperatorToken(nextToken, ',') && !isRParenToken(nextToken)) {
                    throw new LitsError('Expected comma or closing parenthesis', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                }
                if (isA_OperatorToken(nextToken, ',')) {
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
                        case 'comment':
                        case 'cond':
                        case 'declared?':
                        case 'if_not':
                        case '||':
                        case 'when':
                        case 'when_not':
                        case 'do':
                        case 'throw': {
                            var node = {
                                t: AstNodeType.SpecialExpression,
                                n: name_2,
                                p: params,
                                token: getTokenDebugData(symbol.token) && symbol.token,
                            };
                            builtin.specialExpressions[node.n].validateParameterCount(node);
                            return node;
                        }
                        case 'let':
                            return this.parseLet(symbol, params);
                        case 'defs':
                        case 'if_let':
                        case 'when_let':
                        case 'when_first':
                        case 'fn':
                        case 'defns':
                        case 'try':
                        case 'recur':
                        case 'loop':
                        case 'doseq':
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
            try {
                var _a = this.parseFunctionArguments(), functionArguments = _a.functionArguments, arity = _a.arity;
                if (!isA_OperatorToken(this.peek(), '=>')) {
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
            this.advance();
            var rest = false;
            var letBindingObject;
            var args = [];
            var restArg;
            while (!this.isAtEnd() && !isRParenToken(this.peek())) {
                if (letBindingObject) {
                    throw new LitsError('Expected right parentheses', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
                }
                if (isLBraceToken(this.peek())) {
                    letBindingObject = this.parseObject();
                }
                else {
                    if (isA_OperatorToken(this.peek(), '...')) {
                        if (rest) {
                            throw new LitsError('Multiple spread operators in lambda function', (_b = getTokenDebugData(this.peek())) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
                        }
                        this.advance();
                        rest = true;
                    }
                    var symbolToken = this.peek();
                    if (!isA_SymbolToken(symbolToken)) {
                        throw new LitsError('Expected symbol', (_c = getTokenDebugData(this.peek())) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
                    }
                    if (rest) {
                        restArg = symbolToken[1];
                    }
                    else {
                        args.push(symbolToken[1]);
                    }
                    this.advance();
                }
                if (!isA_OperatorToken(this.peek(), ',') && !isRParenToken(this.peek())) {
                    throw new LitsError('Expected comma or closing parenthesis', (_d = getTokenDebugData(this.peek())) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
                }
                if (isA_OperatorToken(this.peek(), ',')) {
                    this.advance();
                }
            }
            var arity = restArg !== undefined ? { min: args.length } : args.length;
            if (!isRParenToken(this.peek())) {
                throw new LitsError('Expected closing parenthesis', (_e = getTokenDebugData(this.peek())) === null || _e === void 0 ? void 0 : _e.sourceCodeInfo);
            }
            var letBindings = letBindingObject ? arrayToPairs(letBindingObject.p) : [];
            var functionArguments = {
                m: args,
                r: restArg,
                b: letBindings.map(function (pair) {
                    var key = pair[0];
                    var value = pair[1];
                    return {
                        t: AstNodeType.Binding,
                        n: key.v,
                        v: value,
                        p: [],
                        token: getTokenDebugData(key.token) && key.token,
                    };
                }),
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
                if (isA_SymbolToken(tkn)) {
                    var match = placeholderRegexp$1.exec(tkn[1]);
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
        AlgebraicParser.prototype.parseLet = function (letSymbol, params) {
            var _a, _b;
            if (params.length !== 2) {
                throw new LitsError('let expects two arguments', (_a = getTokenDebugData(letSymbol.token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
            }
            var letObject = params[0];
            if (letObject.t !== AstNodeType.NormalExpression || letObject.n !== 'object') {
                throw new LitsError('let expects an object as first argument', (_b = getTokenDebugData(letObject.token)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
            }
            var letBindings = arrayToPairs(letObject.p);
            var expression = params[1];
            return {
                t: AstNodeType.SpecialExpression,
                n: 'let',
                p: [expression],
                token: getTokenDebugData(letSymbol.token) && letSymbol.token,
                bs: letBindings.map(function (pair) {
                    var key = pair[0];
                    var value = pair[1];
                    return {
                        t: AstNodeType.Binding,
                        n: key.v,
                        v: value,
                        p: [],
                        token: getTokenDebugData(key.token) && key.token,
                    };
                }),
            };
        };
        AlgebraicParser.prototype.parseFor = function (forSymbol) {
            var forLoopBindings = [
                this.parseForLoopBinding(),
            ];
            var nextToken = this.peekAhead();
            while (isA_SymbolToken(nextToken) && nextToken[1] === 'of') {
                forLoopBindings.push(this.parseForLoopBinding());
                nextToken = this.peekAhead();
            }
            var expression = this.parseExpression();
            assertRParenToken(this.peek());
            this.advance();
            return {
                t: AstNodeType.SpecialExpression,
                n: 'for',
                p: [expression],
                token: getTokenDebugData(forSymbol.token) && forSymbol.token,
                l: forLoopBindings,
            };
        };
        // export interface LoopBindingNode {
        //   b: BindingNode // Binding
        //   m: Array<'&let' | '&when' | '&while'> // Modifiers
        //   l?: BindingNode[] // Let-Bindings
        //   wn?: AstNode // When Node
        //   we?: AstNode // While Node
        // }
        AlgebraicParser.prototype.parseForLoopBinding = function () {
            var _a;
            var bindingNode = this.parseBinding();
            if (isA_OperatorToken(this.peek(), ',')) {
                this.advance();
                return {
                    b: bindingNode,
                    m: [],
                };
            }
            var modifiers = [];
            var token = this.peek();
            if (!isA_SymbolToken(token)) {
                throw new LitsError('Expected symbol let, when or while', (_a = getTokenDebugData(token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
            }
            var letBindings;
            if (token[1] === 'let') {
                modifiers.push('&let');
                letBindings = [];
                this.advance();
                var letObject = this.parseObject();
                letBindings = arrayToPairs(letObject.p).map(function (pair) {
                    var key = pair[0];
                    var value = pair[1];
                    return {
                        t: AstNodeType.Binding,
                        n: key.v,
                        v: value,
                        p: [],
                        token: getTokenDebugData(key.token) && key.token,
                    };
                });
            }
            token = this.peek();
            var whenNode;
            var whileNode;
            while (isA_SymbolToken(token)
                && ((token[1] === 'when' && !modifiers.includes('&when'))
                    || (token[1] === 'while' && !modifiers.includes('&while')))) {
                this.advance();
                if (token[1] === 'when') {
                    modifiers.push('&when');
                    whenNode = this.parseExpression();
                }
                else {
                    modifiers.push('&while');
                    whileNode = this.parseExpression();
                }
                token = this.peek();
            }
            assertA_OperatorToken(token, ',');
            this.advance();
            return {
                b: bindingNode,
                m: modifiers,
                l: letBindings,
                wn: whenNode,
                we: whileNode,
            };
        };
        AlgebraicParser.prototype.parseBinding = function () {
            var _a;
            var firstToken = asA_SymbolToken(this.peek());
            var name = firstToken[1];
            this.advance();
            var ofSymbol = asA_SymbolToken(this.peek());
            if (ofSymbol[1] !== 'of') {
                throw new LitsError('Expected "of"', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
            }
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
        AlgebraicParser.prototype.parseIf = function (token) {
            this.advance();
            var condition = this.parseExpression();
            assertA_ReservedSymbolToken(this.peek(), 'then');
            this.advance();
            var thenExpressions = [];
            while (!this.isAtEnd()
                && !isA_ReservedSymbolToken(this.peek(), 'else')
                && !isA_ReservedSymbolToken(this.peek(), 'end')) {
                thenExpressions.push(this.parseExpression());
                if (isA_OperatorToken(this.peek(), ';')) {
                    this.advance();
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
            if (isA_ReservedSymbolToken(this.peek(), 'else')) {
                this.advance();
                var elseExpressions = [];
                while (!this.isAtEnd() && !isA_ReservedSymbolToken(this.peek(), 'end')) {
                    elseExpressions.push(this.parseExpression());
                    if (isA_OperatorToken(this.peek(), ';')) {
                        this.advance();
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
            assertA_ReservedSymbolToken(this.peek(), 'end');
            this.advance();
            var params = [condition, thenExpression];
            if (elseExpression) {
                params.push(elseExpression);
            }
            return {
                t: AstNodeType.SpecialExpression,
                n: 'if',
                p: params,
                token: getTokenDebugData(token) && token,
            };
        };
        AlgebraicParser.prototype.parseDef = function (token) {
            this.advance();
            var symbol = parseSymbol(this.tokenStream, this.parseState);
            assertA_OperatorToken(this.peek(), '=');
            this.advance();
            var value = this.parseExpression();
            return {
                t: AstNodeType.SpecialExpression,
                n: 'def',
                p: [symbol, value],
                token: getTokenDebugData(token) && token,
            };
        };
        AlgebraicParser.prototype.parseDefn = function (token) {
            var _a;
            this.advance();
            var symbol = parseSymbol(this.tokenStream, this.parseState);
            var _b = this.parseFunctionArguments(), functionArguments = _b.functionArguments, arity = _b.arity;
            assertLBraceToken(this.peek());
            this.advance();
            var body = [];
            while (!this.isAtEnd() && !isRBraceToken(this.peek())) {
                body.push(this.parseExpression());
                if (isA_OperatorToken(this.peek(), ';')) {
                    this.advance();
                }
            }
            if (!isRBraceToken(this.peek())) {
                throw new LitsError('Expected closing brace', (_a = getTokenDebugData(this.peek())) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
            }
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
        };
        AlgebraicParser.prototype.isAtEnd = function () {
            return this.parseState.position >= this.tokenStream.tokens.length;
        };
        AlgebraicParser.prototype.peek = function () {
            return this.tokenStream.tokens[this.parseState.position];
        };
        AlgebraicParser.prototype.peekAhead = function () {
            return this.tokenStream.tokens[this.parseState.position + 1];
        };
        return AlgebraicParser;
    }());

    function parseStringShorthand(tokenStream, parseState) {
        var tkn = asP_StringShorthandToken(tokenStream.tokens[parseState.position++]);
        var value = tkn[1].substring(1);
        return {
            t: AstNodeType.String,
            v: value,
            p: [],
            n: undefined,
            token: getTokenDebugData(tkn) && tkn,
        };
    }
    function parseComment(tokenStream, parseState) {
        var tkn = asP_CommentToken(tokenStream.tokens[parseState.position++]);
        return {
            t: AstNodeType.Comment,
            v: tkn[1],
            p: [],
            n: undefined,
            token: getTokenDebugData(tkn) && tkn,
        };
    }
    function parseTokensUntilClosingBracket(tokenStream, parseState) {
        var tkn = asToken(tokenStream.tokens[parseState.position]);
        var astNodes = [];
        while (!isRParenToken(tkn) && !isRBracketToken(tkn)) {
            astNodes.push(parseState.parseToken(tokenStream, parseState));
            tkn = asToken(tokenStream.tokens[parseState.position]);
        }
        return astNodes;
    }
    var parseExpression = function (tokenStream, parseState) {
        var tkn = asToken(tokenStream.tokens[parseState.position + 1]);
        if (isP_SymbolToken(tkn) && builtin.specialExpressions[tkn[1]])
            return parseSpecialExpression(tokenStream, parseState);
        return parseNormalExpression(tokenStream, parseState);
    };
    function parseArrayLitteral(tokenStream, parseState) {
        var firstToken = asToken(tokenStream.tokens[parseState.position++]);
        var tkn = asToken(tokenStream.tokens[parseState.position]);
        var params = [];
        while (!isRBracketToken(tkn)) {
            params.push(parseState.parseToken(tokenStream, parseState));
            tkn = asToken(tokenStream.tokens[parseState.position]);
        }
        parseState.position += 1;
        var node = {
            t: AstNodeType.NormalExpression,
            n: 'array',
            p: params,
            token: getTokenDebugData(firstToken) && firstToken,
        };
        return node;
    }
    function parseObjectLitteral(tokenStream, parseState) {
        var firstToken = asToken(tokenStream.tokens[parseState.position++]);
        var tkn = asToken(tokenStream.tokens[parseState.position]);
        var params = [];
        while (!isRBraceToken(tkn)) {
            params.push(parseState.parseToken(tokenStream, parseState));
            tkn = asToken(tokenStream.tokens[parseState.position]);
        }
        parseState.position += 1;
        var node = {
            t: AstNodeType.NormalExpression,
            n: 'object',
            p: params,
            token: getTokenDebugData(firstToken) && firstToken,
        };
        assertEvenNumberOfParams(node);
        return node;
    }
    function parseRegexpShorthand(tokenStream, parseState) {
        var tkn = asP_RegexpShorthandToken(tokenStream.tokens[parseState.position++]);
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
    var placeholderRegexp = /^%([1-9]\d?)?$/;
    function parseFnShorthand(tokenStream, parseState) {
        var _a, _b, _c, _d;
        var firstToken = asToken(tokenStream.tokens[parseState.position++]);
        var startPos = parseState.position + 1;
        var exprNode = parseExpression(tokenStream, parseState);
        var arity = 0;
        var percent1 = 'NOT_SET'; // referring to argument bindings. % = NAKED, %1, %2, %3, etc = WITH_1
        for (var pos = startPos; pos < parseState.position - 1; pos += 1) {
            var tkn = asToken(tokenStream.tokens[pos]);
            if (isP_SymbolToken(tkn)) {
                var match = placeholderRegexp.exec(tkn[1]);
                if (match) {
                    var number = (_a = match[1]) !== null && _a !== void 0 ? _a : '1';
                    if (number === '1') {
                        var mixedPercent1 = (!match[1] && percent1 === 'WITH_1') || (match[1] && percent1 === 'NAKED');
                        if (mixedPercent1)
                            throw new LitsError('Please make up your mind, either use % or %1', (_b = getTokenDebugData(firstToken)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
                        percent1 = match[1] ? 'WITH_1' : 'NAKED';
                    }
                    arity = Math.max(arity, Number(number));
                    if (arity > 20)
                        throw new LitsError('Can\'t specify more than 20 arguments', (_c = getTokenDebugData(firstToken)) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
                }
            }
            if (isP_FnShorthandToken(tkn))
                throw new LitsError('Nested shortcut functions are not allowed', (_d = getTokenDebugData(firstToken)) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
        }
        var mandatoryArguments = [];
        for (var i = 1; i <= arity; i += 1) {
            if (i === 1 && percent1 === 'NAKED')
                mandatoryArguments.push('%');
            else
                mandatoryArguments.push("%".concat(i));
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
    }
    var parseArgument = function (tokenStream, parseState) {
        var _a;
        var tkn = asToken(tokenStream.tokens[parseState.position++]);
        if (isP_SymbolToken(tkn)) {
            return {
                t: AstNodeType.Argument,
                n: tkn[1],
                p: [],
                token: getTokenDebugData(tkn) && tkn,
            };
        }
        else if (isP_ModifierToken(tkn)) {
            return {
                t: AstNodeType.Modifier,
                v: tkn[1],
                p: [],
                n: undefined,
                token: getTokenDebugData(tkn) && tkn,
            };
        }
        else {
            throw new LitsError("Expected name or modifier token, got ".concat(valueToString(tkn), "."), (_a = getTokenDebugData(tkn)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
        }
    };
    function parseBindings(tokenStream, parseState) {
        assertLBracketToken(tokenStream.tokens[parseState.position++]);
        var tkn = asToken(tokenStream.tokens[parseState.position]);
        var bindings = [];
        while (!isRBracketToken(tkn)) {
            bindings.push(parseBinding(tokenStream, parseState));
            tkn = asToken(tokenStream.tokens[parseState.position]);
        }
        parseState.position += 1;
        return bindings;
    }
    function parseBinding(tokenStream, parseState) {
        var firstToken = asP_SymbolToken(tokenStream.tokens[parseState.position]);
        var name = parseSymbol(tokenStream, parseState);
        var value = parseState.parseToken(tokenStream, parseState);
        var node = {
            t: AstNodeType.Binding,
            n: name.v,
            v: value,
            p: [],
            token: getTokenDebugData(firstToken) && firstToken,
        };
        return node;
    }
    function parseNormalExpression(tokenStream, parseState) {
        var _a, _b;
        var startBracketToken = tokenStream.hasDebugData ? asLParenToken(tokenStream.tokens[parseState.position]) : undefined;
        parseState.position += 1;
        var fnNode = parseState.parseToken(tokenStream, parseState);
        var params = parseTokensUntilClosingBracket(tokenStream, parseState);
        assertRParenToken(tokenStream.tokens[parseState.position++]);
        if (isExpressionNode(fnNode)) {
            var node_1 = {
                t: AstNodeType.NormalExpression,
                p: __spreadArray([fnNode], __read(params), false),
                n: undefined,
                token: startBracketToken,
            };
            return node_1;
        }
        assertSymbolNode(fnNode, (_a = getTokenDebugData(fnNode.token)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
        var node = {
            t: AstNodeType.NormalExpression,
            n: fnNode.v,
            p: params,
            token: startBracketToken,
        };
        var builtinExpression = builtin.normalExpressions[node.n];
        if (builtinExpression) {
            (_b = builtinExpression.validate) === null || _b === void 0 ? void 0 : _b.call(builtinExpression, __assign(__assign({}, node), { p: withoutCommentNodes(node.p) }));
        }
        return node;
    }
    function parseSpecialExpression(tokenStream, parseState) {
        var _a;
        var firstToken = asLParenToken(tokenStream.tokens[parseState.position++]);
        var nameToken = asP_SymbolToken(tokenStream.tokens[parseState.position++]);
        var expressionName = nameToken[1];
        var _b = asNonUndefined(builtin.specialExpressions[expressionName], (_a = getTokenDebugData(nameToken)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo), parse = _b.polishParse, validateParameterCount = _b.validateParameterCount;
        var node = parse(tokenStream, parseState, firstToken, {
            parseExpression: parseExpression,
            parseTokensUntilClosingBracket: parseTokensUntilClosingBracket,
            parseToken: parseState.parseToken,
            parseBinding: parseBinding,
            parseBindings: parseBindings,
            parseArgument: parseArgument,
        });
        validateParameterCount(node);
        return node;
    }
    function parsePolishToken(tokenStream, parseState) {
        var _a, _b;
        var tkn = asToken(tokenStream.tokens[parseState.position]);
        var tokenType = tkn[0];
        switch (tokenType) {
            case 'String':
                return parseString(tokenStream, parseState);
            case 'P_Number':
                return parseNumber(tokenStream, parseState);
            case 'P_StringShorthand':
                return parseStringShorthand(tokenStream, parseState);
            case 'P_Symbol':
                return parseSymbol(tokenStream, parseState);
            case 'P_ReservedSymbol':
                return parseReservedSymbol(tokenStream, parseState);
            case 'LParen':
                return parseExpression(tokenStream, parseState);
            case 'LBracket':
                return parseArrayLitteral(tokenStream, parseState);
            case 'LBrace':
                return parseObjectLitteral(tokenStream, parseState);
            case 'P_RegexpShorthand':
                return parseRegexpShorthand(tokenStream, parseState);
            case 'P_FnShorthand':
                return parseFnShorthand(tokenStream, parseState);
            case 'P_Comment':
                return parseComment(tokenStream, parseState);
            case 'AlgNotation': {
                parseState.position += 1;
                parseState.algebraic = true;
                var algebraicParser = new AlgebraicParser(tokenStream, parseState);
                var nodes = algebraicParser.parse();
                assertEndNotationToken(tokenStream.tokens[parseState.position++]);
                parseState.algebraic = false;
                if (nodes.length === 1) {
                    return nodes[0];
                }
                return {
                    t: AstNodeType.SpecialExpression,
                    n: 'do',
                    p: nodes,
                    token: nodes[0].token,
                };
            }
            case 'PolNotation': {
                var astNodes = [];
                parseState.position += 1;
                do {
                    astNodes.push(parsePolishToken(tokenStream, parseState));
                } while (!isEndNotationToken(asToken(tokenStream.tokens[parseState.position])));
                parseState.position += 1;
                if (astNodes.length === 1) {
                    return astNodes[0];
                }
                return {
                    t: AstNodeType.SpecialExpression,
                    n: 'do',
                    p: astNodes,
                    token: astNodes[0].token,
                };
            }
            case 'P_CollectionAccessor':
            case 'P_Modifier':
            case 'RParen':
            case 'RBracket':
            case 'RBrace':
            case 'P_Whitespace':
            case 'EndNotation':
                break;
            /* v8 ignore next 2 */
            default:
                throw new LitsError("Unrecognized token: ".concat(tokenType, " ").concat(tkn[1]), (_a = getTokenDebugData(tkn)) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
        }
        throw new LitsError("Unrecognized token: ".concat(tokenType).concat(tkn[1] ? " ".concat(tkn[1]) : ''), (_b = getTokenDebugData(tkn)) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
    }

    function parse$1(tokenStream) {
        var _a;
        var safeTokenStream = removeUnnecessaryTokens(tokenStream);
        var ast = {
            b: [],
            hasDebugData: safeTokenStream.hasDebugData,
        };
        var parseState = {
            position: 0,
            algebraic: (_a = safeTokenStream.algebraic) !== null && _a !== void 0 ? _a : false,
            parseToken: parseToken,
        };
        while (parseState.position < safeTokenStream.tokens.length) {
            ast.b.push(parseToken(safeTokenStream, parseState));
        }
        return ast;
    }
    function removeUnnecessaryTokens(tokenStream) {
        var tokens = tokenStream.tokens.filter(function (token) {
            if (isP_CommentToken(token)
                || isA_CommentToken(token)
                || isA_MultiLineCommentToken(token)
                || isA_WhitespaceToken(token)
                || isP_WhitespaceToken(token)) {
                return false;
            }
            return true;
        });
        return __assign(__assign({}, tokenStream), { tokens: tokens });
    }
    function parseToken(tokenStream, parseState) {
        if (parseState.algebraic) {
            var algebraicParser = new AlgebraicParser(tokenStream, parseState);
            var nodes = algebraicParser.parse();
            if (nodes.length === 1) {
                return nodes[0];
            }
            return {
                t: AstNodeType.SpecialExpression,
                n: 'do',
                p: nodes,
                token: nodes[0].token,
            };
        }
        return parsePolishToken(tokenStream, parseState);
    }

    var polishIdentifierCharacterClass = '[\\w@%^?=!$<>+*/:&\|~-]';
    var algebraicIdentifierCharacterClass = '[\\w$:!?]';
    var algebraicIdentifierFirstCharacterClass = '[a-zA-Z_$]';

    var NO_MATCH = [0];
    function isNoMatch(tokenDescriptor) {
        return tokenDescriptor[0] === 0;
    }
    var tokenizeLParen = function (input, position) {
        return tokenizeSimpleToken('LParen', '(', input, position);
    };
    var tokenizeRParen = function (input, position) {
        return tokenizeSimpleToken('RParen', ')', input, position);
    };
    var tokenizeLBracket = function (input, position) {
        return tokenizeSimpleToken('LBracket', '[', input, position);
    };
    var tokenizeRBracket = function (input, position) {
        return tokenizeSimpleToken('RBracket', ']', input, position);
    };
    var tokenizeLBrace = function (input, position) {
        return tokenizeSimpleToken('LBrace', '{', input, position);
    };
    var tokenizeRBrace = function (input, position) {
        return tokenizeSimpleToken('RBrace', '}', input, position);
    };
    var tokenizePolishNotation = function (input, position) {
        return tokenizeSimpleToken('PolNotation', '$`', input, position);
    };
    var tokenizeAlgebraicNotation = function (input, position) {
        return tokenizeSimpleToken('AlgNotation', '@`', input, position);
    };
    var tokenizeEndNotation = function (input, position) {
        return tokenizeSimpleToken('EndNotation', '`', input, position);
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
    function tokenizeSimpleToken(type, value, input, position) {
        if (value === input.slice(position, position + value.length))
            return [value.length, [type]];
        else
            return NO_MATCH;
    }
    var commonTokenizers = [
        tokenizePolishNotation,
        tokenizeAlgebraicNotation,
        tokenizeEndNotation,
        tokenizeLParen,
        tokenizeRParen,
        tokenizeLBracket,
        tokenizeRBracket,
        tokenizeLBrace,
        tokenizeRBrace,
        tokenizeString,
    ];

    var validAlgebraicReservedNamesRecord = {
        true: { value: true, forbidden: false },
        false: { value: false, forbidden: false },
        nil: { value: null, forbidden: false },
        null: { value: null, forbidden: false },
        then: { value: null, forbidden: false },
        else: { value: null, forbidden: false },
        end: { value: null, forbidden: false },
    };
    var forbiddenAlgebraicReservedNamesRecord = {
        if_let: { value: null, forbidden: true },
        when_let: { value: null, forbidden: true },
        when_first: { value: null, forbidden: true },
        fn: { value: null, forbidden: true },
        defns: { value: null, forbidden: true },
        try: { value: null, forbidden: true },
        recur: { value: null, forbidden: true },
        loop: { value: null, forbidden: true },
        doseq: { value: null, forbidden: true },
    };
    var algebraicReservedNamesRecord = __assign(__assign({}, validAlgebraicReservedNamesRecord), forbiddenAlgebraicReservedNamesRecord);

    var identifierRegExp = new RegExp(algebraicIdentifierCharacterClass);
    var identifierFirstCharacterRegExp = new RegExp(algebraicIdentifierFirstCharacterClass);
    var whitespaceRegExp$1 = /\s/;
    var tokenizeA_Whitespace = function (input, position) {
        var char = input[position];
        if (!char || !whitespaceRegExp$1.test(char)) {
            return NO_MATCH;
        }
        var value = char;
        position += 1;
        char = input[position];
        while (char && whitespaceRegExp$1.test(char)) {
            value += char;
            position += 1;
            char = input[position];
        }
        return [value.length, ['A_Whitespace', value]];
    };
    var decimalNumberRegExp$1 = /\d/;
    var octalNumberRegExp$1 = /[0-7]/;
    var hexNumberRegExp$1 = /[0-9a-f]/i;
    var binaryNumberRegExp$1 = /[01]/;
    var tokenizeA_Number = function (input, position) {
        var i;
        for (i = position; i < input.length; i += 1) {
            var char = input[i];
            if (!decimalNumberRegExp$1.test(char)) {
                break;
            }
        }
        var length = i - position;
        if (length === 0) {
            return NO_MATCH;
        }
        return [length, ['A_Number', input.substring(position, i)]];
    };
    var tokenizeA_BasePrefixedNumber = function (input, position) {
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
            if (type === 'binary' && !binaryNumberRegExp$1.test(char)) {
                break;
            }
            if (type === 'octal' && !octalNumberRegExp$1.test(char)) {
                break;
            }
            if (type === 'hex' && !hexNumberRegExp$1.test(char)) {
                break;
            }
        }
        var length = i - position;
        if (length <= 2) {
            return NO_MATCH;
        }
        return [length, ['A_BasePrefixedNumber', input.substring(position, i)]];
    };
    var tokenizeA_Symbol = function (input, position) {
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
            return [length_1 + 1, ['A_Symbol', value]];
        }
        if (identifierFirstCharacterRegExp.test(value)) {
            var initialPosition = position;
            position += 1;
            var char = input[position];
            while (char && identifierRegExp.test(char)) {
                value += char;
                position += 1;
                char = input[position];
            }
            return [position - initialPosition, ['A_Symbol', value]];
        }
        return NO_MATCH;
    };
    var tokenizeA_ReservedSymbolToken = function (input, position) {
        var symbolMeta = tokenizeA_Symbol(input, position);
        if (symbolMeta[0] === 0 || !symbolMeta[1]) {
            return NO_MATCH;
        }
        var symbolName = symbolMeta[1][1];
        symbolName = symbolName.startsWith('\'') ? symbolName.slice(1, symbolName.length - 1) : symbolName;
        var info = algebraicReservedNamesRecord[symbolName];
        if (!info) {
            return NO_MATCH;
        }
        if (info.forbidden) {
            throw new LitsError("".concat(symbolName, " is forbidden!"), undefined);
        }
        return [symbolMeta[0], ['A_ReservedSymbol', symbolName]];
    };
    var tokenizeA_Operator = function (input, position) {
        var _a;
        var threeChars = input.slice(position, position + 3);
        if (position + 2 < input.length && isSymbolicOperator(threeChars)) {
            return [3, ['A_Operator', threeChars]];
        }
        var twoChars = input.slice(position, position + 2);
        if (position + 1 < input.length && isSymbolicOperator(twoChars)) {
            return [2, ['A_Operator', twoChars]];
        }
        var oneChar = (_a = input[position]) !== null && _a !== void 0 ? _a : '';
        if (isSymbolicOperator(oneChar)) {
            return [1, ['A_Operator', oneChar]];
        }
        return NO_MATCH;
    };
    var tokenizeA_MultiLineComment = function (input, position) {
        if (input[position] === '/' && input[position + 1] === '*') {
            var length_2 = 2;
            var value = '/*';
            while (input[position + length_2] !== '*' && input[position + length_2 + 1] !== '/' && position + length_2 + 1 < input.length) {
                value += input[position + length_2];
                length_2 += 1;
            }
            if (position + length_2 + 1 >= input.length) {
                throw new LitsError('Comment not closed', undefined);
            }
            value += '*/';
            length_2 += 2;
            return [length_2, ['A_MultiLineComment', value]];
        }
        return NO_MATCH;
    };
    var tokenizeA_SingleLineComment = function (input, position) {
        if (input[position] === '/' && input[position + 1] === '/') {
            var length_3 = 2;
            var value = '//';
            while (input[position + length_3] !== '\n' && position + length_3 < input.length) {
                value += input[position + length_3];
                length_3 += 1;
            }
            return [length_3, ['A_SingleLineComment', value]];
        }
        return NO_MATCH;
    };
    // All tokenizers, order matters!
    var algebraicTokenizers = __spreadArray(__spreadArray([
        tokenizeA_Whitespace,
        tokenizeA_MultiLineComment,
        tokenizeA_SingleLineComment
    ], __read(commonTokenizers), false), [
        tokenizeA_BasePrefixedNumber,
        tokenizeA_Number,
        tokenizeA_Operator,
        tokenizeA_ReservedSymbolToken,
        tokenizeA_Symbol,
    ], false);

    var whitespaceRegExp = /\s|,/;
    var tokenizeP_Comment = function (input, position) {
        if (input[position] === ';') {
            var length_1 = 0;
            var value = '';
            while (input[position + length_1] !== '\n' && position + length_1 < input.length) {
                value += input[position + length_1];
                length_1 += 1;
            }
            return [length_1, ['P_Comment', value]];
        }
        return NO_MATCH;
    };
    var tokenizeP_Whitespace = function (input, position) {
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
        return [value.length, ['P_Whitespace', value]];
    };
    var endOfNumberRegExp = /[\s)\]},;#`]/;
    var decimalNumberRegExp = /\d/;
    var octalNumberRegExp = /[0-7]/;
    var hexNumberRegExp = /[0-9a-f]/i;
    var binaryNumberRegExp = /[01]/;
    var firstCharRegExp = /[0-9.-]/;
    var tokenizeP_Number = function (input, position) {
        var type = 'decimal';
        var firstChar = input[position];
        if (!firstCharRegExp.test(firstChar))
            return NO_MATCH;
        var hasDecimals = firstChar === '.';
        var i;
        for (i = position + 1; i < input.length; i += 1) {
            var char = input[i];
            if (endOfNumberRegExp.test(char))
                break;
            if (char === '.') {
                var nextChar = input[i + 1];
                if (typeof nextChar === 'string' && !decimalNumberRegExp.test(nextChar))
                    break;
            }
            if (i === position + 1 && firstChar === '0') {
                if (char === 'b' || char === 'B') {
                    type = 'binary';
                    continue;
                }
                if (char === 'o' || char === 'O') {
                    type = 'octal';
                    continue;
                }
                if (char === 'x' || char === 'X') {
                    type = 'hex';
                    continue;
                }
            }
            if (type === 'decimal' && hasDecimals) {
                if (!decimalNumberRegExp.test(char))
                    return NO_MATCH;
            }
            else if (type === 'binary') {
                if (!binaryNumberRegExp.test(char))
                    return NO_MATCH;
            }
            else if (type === 'octal') {
                if (!octalNumberRegExp.test(char))
                    return NO_MATCH;
            }
            else if (type === 'hex') {
                if (!hexNumberRegExp.test(char))
                    return NO_MATCH;
            }
            else {
                if (char === '.') {
                    hasDecimals = true;
                    continue;
                }
                if (!decimalNumberRegExp.test(char))
                    return NO_MATCH;
            }
        }
        var length = i - position;
        var value = input.substring(position, i);
        if ((type !== 'decimal' && length <= 2) || value === '.' || value === '-')
            return NO_MATCH;
        return [length, ['P_Number', value]];
    };
    var P_symbolRegExp = new RegExp(polishIdentifierCharacterClass);
    var tokenizeP_Symbol = function (input, position) {
        var value = input[position];
        if (!value) {
            return NO_MATCH;
        }
        if (value === '\'') {
            var length_2 = 1;
            var char = input[position + length_2];
            var escaping = false;
            while (char !== '\'' || escaping) {
                if (char === undefined)
                    throw new LitsError("Unclosed string at position ".concat(position, "."), undefined);
                length_2 += 1;
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
                char = input[position + length_2];
            }
            value += '\''; // closing quote
            return [length_2 + 1, ['P_Symbol', value]];
        }
        if (P_symbolRegExp.test(value)) {
            var initialPosition = position;
            position += 1;
            var char = input[position];
            while (char && P_symbolRegExp.test(char)) {
                value += char;
                position += 1;
                char = input[position];
            }
            return [position - initialPosition, ['P_Symbol', value]];
        }
        return NO_MATCH;
    };
    var tokenizeP_FnShorthand = function (input, position) {
        if (input.slice(position, position + 2) !== '#(')
            return NO_MATCH;
        return [1, ['P_FnShorthand']];
    };
    var tokenizeP_ReservedSymbol = function (input, position) {
        var e_1, _a;
        try {
            for (var _b = __values(Object.entries(polishReservedNamesRecord)), _c = _b.next(); !_c.done; _c = _b.next()) {
                var _d = __read(_c.value, 2), reservedName = _d[0], forbidden = _d[1].forbidden;
                var length_3 = reservedName.length;
                var nextChar = input[position + length_3];
                if (nextChar && P_symbolRegExp.test(nextChar)) {
                    continue;
                }
                var symbol = input.substring(position, position + length_3);
                if (symbol === reservedName) {
                    if (forbidden)
                        throw new LitsError("".concat(symbol, " is forbidden!"), undefined);
                    return [length_3, ['P_ReservedSymbol', reservedName]];
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
    var tokenizeP_StringShorthand = function (input, position) {
        if (input[position] !== ':')
            return NO_MATCH;
        var symbolDescription = tokenizeP_Symbol(input, position + 1);
        if (isNoMatch(symbolDescription)) {
            return symbolDescription;
        }
        var symbolToken = asP_SymbolToken(symbolDescription[1]);
        return [symbolDescription[0] + 1, ['P_StringShorthand', ":".concat(symbolToken[1])]];
    };
    var tokenizeP_Modifier = function (input, position) {
        var e_2, _a;
        try {
            for (var modifierNames_1 = __values(modifierNames), modifierNames_1_1 = modifierNames_1.next(); !modifierNames_1_1.done; modifierNames_1_1 = modifierNames_1.next()) {
                var modifierName = modifierNames_1_1.value;
                var length_4 = modifierName.length;
                var charAfterModifier = input[position + length_4];
                if (input.substring(position, position + length_4) === modifierName && (!charAfterModifier || !P_symbolRegExp.test(charAfterModifier))) {
                    var value = modifierName;
                    return [length_4, ['P_Modifier', value]];
                }
            }
        }
        catch (e_2_1) { e_2 = { error: e_2_1 }; }
        finally {
            try {
                if (modifierNames_1_1 && !modifierNames_1_1.done && (_a = modifierNames_1.return)) _a.call(modifierNames_1);
            }
            finally { if (e_2) throw e_2.error; }
        }
        return NO_MATCH;
    };
    var tokenizeP_CollectionAccessor = function (input, position) {
        var char = input[position];
        if (char !== '.' && char !== '#')
            return NO_MATCH;
        return [1, ['P_CollectionAccessor', char]];
    };
    var tokenizeP_RegexpShorthand = function (input, position) {
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
        return [length, ['P_RegexpShorthand', "#".concat(token[1]).concat(options)]];
    };
    // All tokenizers, order matters!
    var polishTokenizers = __spreadArray(__spreadArray([
        tokenizeP_Whitespace,
        tokenizeP_Comment
    ], __read(commonTokenizers), false), [
        tokenizeP_StringShorthand,
        tokenizeP_Number,
        tokenizeP_ReservedSymbol,
        tokenizeP_Modifier,
        tokenizeP_Symbol,
        tokenizeP_RegexpShorthand,
        tokenizeP_FnShorthand,
        tokenizeP_CollectionAccessor,
    ], false);

    var applyCollectionAccessors = function (tokenStream) {
        var dotTokenIndex = tokenStream.tokens.findIndex(isP_CollectionAccessorToken);
        while (dotTokenIndex >= 0) {
            applyCollectionAccessor(tokenStream, dotTokenIndex);
            dotTokenIndex = tokenStream.tokens.findIndex(isP_CollectionAccessorToken);
        }
        return tokenStream;
    };
    function applyCollectionAccessor(tokenStream, position) {
        var dotTkn = asP_CollectionAccessorToken(tokenStream.tokens[position]);
        var debugData = getTokenDebugData(dotTkn);
        var backPosition = getPositionBackwards(tokenStream, position, debugData === null || debugData === void 0 ? void 0 : debugData.sourceCodeInfo);
        checkForward(tokenStream, position, dotTkn, debugData === null || debugData === void 0 ? void 0 : debugData.sourceCodeInfo);
        tokenStream.tokens.splice(position, 1);
        tokenStream.tokens.splice(backPosition, 0, ['LParen']);
        var nextTkn = asToken(tokenStream.tokens[position + 1]);
        if (dotTkn[1] === '.') {
            assertP_SymbolToken(nextTkn);
            var token = ['P_StringShorthand', ":".concat(nextTkn[1])];
            tokenStream.tokens[position + 1] = token;
            var nextTkndebugData = getTokenDebugData(nextTkn);
            if (nextTkndebugData) {
                addTokenDebugData(token, nextTkndebugData);
            }
        }
        else {
            assertP_NumberToken(nextTkn);
            assertNumber(Number(nextTkn[1]), debugData === null || debugData === void 0 ? void 0 : debugData.sourceCodeInfo, { integer: true, nonNegative: true });
            tokenStream.tokens[position + 1] = ['P_Number', nextTkn[1]];
        }
        tokenStream.tokens.splice(position + 2, 0, ['RParen']);
    }
    function getPositionBackwards(tokenStream, position, sourceCodeInfo) {
        var bracketCount = null;
        if (position <= 0)
            throw new LitsError('Array accessor # must come after a sequence', sourceCodeInfo);
        var prevToken = asNonUndefined(tokenStream.tokens[position - 1]);
        var openBracket = null;
        var closeBracket = null;
        if (isRParenToken(prevToken)) {
            openBracket = 'LParen';
            closeBracket = 'RParen';
        }
        else if (isRBracketToken(prevToken)) {
            openBracket = 'LBracket';
            closeBracket = 'RBracket';
        }
        else if (isRBraceToken(prevToken)) {
            openBracket = 'LBrace';
            closeBracket = 'RBrace';
        }
        while (bracketCount !== 0) {
            bracketCount = bracketCount === null ? 0 : bracketCount;
            position -= 1;
            var tkn = asNonUndefined(tokenStream.tokens[position], sourceCodeInfo);
            if (tkn[0] === openBracket) {
                bracketCount += 1;
            }
            else if (tkn[0] === closeBracket) {
                bracketCount -= 1;
            }
        }
        if (openBracket === 'LParen' && position > 0) {
            var tokenBeforeBracket = asNonUndefined(tokenStream.tokens[position - 1]);
            if (isP_FnShorthandToken(tokenBeforeBracket))
                throw new LitsError('# or . must NOT be preceeded by shorthand lambda function', sourceCodeInfo);
        }
        return position;
    }
    function checkForward(tokenStream, position, dotTkn, sourceCodeInfo) {
        var tkn = tokenStream.tokens[position + 1];
        if (dotTkn[1] === '.' && !isP_SymbolToken(tkn)) {
            throw new LitsError('# as a collection accessor must be followed by an name', sourceCodeInfo);
        }
        if (dotTkn[1] === '#' && isNumber(tkn)) {
            throw new LitsError('# as a collection accessor must be followed by an integer', sourceCodeInfo);
        }
    }

    function getSugar() {
        return [applyCollectionAccessors];
    }

    function tokenize$1(input, params) {
        var debug = !!params.debug;
        var notationStack = [params.algebraic ? 'algebraic' : 'polish'];
        var position = 0;
        var tokenStream = {
            tokens: [],
            filePath: params.filePath,
            hasDebugData: debug,
            algebraic: !!params.algebraic,
        };
        while (position < input.length) {
            var tokenizers = notationStack.at(-1) === 'algebraic' ? algebraicTokenizers : polishTokenizers;
            var tokenDescriptor = getCurrentToken(input, position, tokenizers);
            var debugData = debug
                ? {
                    sourceCodeInfo: createSourceCodeInfo(input, position, params.filePath),
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
                if (isAlgebraicNotationToken(token)) {
                    notationStack.push('algebraic');
                }
                if (isPolishNotationToken(token)) {
                    notationStack.push('polish');
                }
                if (isEndNotationToken(token)) {
                    notationStack.pop();
                    if (notationStack.length < 1) {
                        throw new LitsError('Unexpected end directive `.', debugData === null || debugData === void 0 ? void 0 : debugData.sourceCodeInfo);
                    }
                }
            }
        }
        if (notationStack.length > 1) {
            throw new LitsError('Missing end directive `.', createSourceCodeInfo(input, position, params.filePath));
        }
        applySugar(tokenStream);
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
    function getCurrentToken(input, position, tokenizers) {
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
    function applySugar(tokenStream) {
        var sugar = getSugar();
        sugar.forEach(function (sugarFn) { return sugarFn(tokenStream); });
    }

    function transformTokens(tokenStram, transformer) {
        return __assign(__assign({}, tokenStram), { tokens: tokenStram.tokens.map(function (token) { return isA_SymbolToken(token) || isP_SymbolToken(token)
                ? [token[0], transformer(token[1])]
                : token; }) });
    }

    function untokenize(tokenStream) {
        return tokenStream.tokens.reduce(function (acc, token) {
            return "".concat(acc).concat(untokenizeToken(token));
        }, '');
    }
    function untokenizeToken(token) {
        if (isValueToken(token)) {
            return token[1];
        }
        assertSimpleToken(token);
        var tokenType = token[0];
        switch (tokenType) {
            case 'LParen': return '(';
            case 'RParen': return ')';
            case 'LBracket': return '[';
            case 'RBracket': return ']';
            case 'LBrace': return '{';
            case 'RBrace': return '}';
            case 'PolNotation': return '$`';
            case 'AlgNotation': return '@`';
            case 'EndNotation': return '`';
            case 'P_FnShorthand': return '#';
            /* v8 ignore next 2 */
            default:
                throw new Error("Unknown token type: ".concat(tokenType));
        }
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
            var _b, _c, _d, _e;
            this.debug = (_b = config.debug) !== null && _b !== void 0 ? _b : false;
            this.algebraic = (_c = config.algebraic) !== null && _c !== void 0 ? _c : false;
            this.astCacheSize = (_d = config.astCacheSize) !== null && _d !== void 0 ? _d : null;
            if (this.astCacheSize) {
                this.astCache = new Cache(this.astCacheSize);
                var initialCache = (_e = config.initialCache) !== null && _e !== void 0 ? _e : {};
                try {
                    for (var _f = __values(Object.keys(initialCache)), _g = _f.next(); !_g.done; _g = _f.next()) {
                        var cacheEntry = _g.value;
                        this.astCache.set(cacheEntry, initialCache[cacheEntry]);
                    }
                }
                catch (e_1_1) { e_1 = { error: e_1_1 }; }
                finally {
                    try {
                        if (_g && !_g.done && (_a = _f.return)) _a.call(_f);
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
        Lits.prototype.analyze = function (program, params) {
            if (params === void 0) { params = {}; }
            var ast = this.generateAst(program, params);
            return analyze$1(ast, params);
        };
        Lits.prototype.tokenize = function (program, tokenizeParams) {
            if (tokenizeParams === void 0) { tokenizeParams = {}; }
            var debug = this.debug;
            var algebraic = this.algebraic;
            return tokenize$1(program, __assign(__assign({}, tokenizeParams), { debug: debug, algebraic: algebraic }));
        };
        Lits.prototype.parse = function (tokenStream) {
            return parse$1(tokenStream);
        };
        Lits.prototype.evaluate = function (ast, params) {
            var contextStack = createContextStack(params);
            return evaluate(ast, contextStack);
        };
        Lits.prototype.transform = function (tokenStream, transformer) {
            return transformTokens(tokenStream, transformer);
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
                .join(this.algebraic ? ', ' : ' ');
            return this.algebraic ? "".concat(fnName, "(").concat(paramsString, ")") : "(".concat(fnName, " ").concat(paramsString, ")");
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

    var collectionReference = {
        'count': {
            title: 'count',
            category: 'Collection',
            linkName: 'count',
            returns: {
                type: 'number',
            },
            args: {
                coll: {
                    type: ['collection', 'string', 'null'],
                },
            },
            variants: [
                { argumentNames: ['coll'] },
            ],
            description: 'Returns number of elements in $coll.',
            examples: [
                '(count [1 2 3])',
                '(count [])',
                '(count (object :a 1))',
                '(count "")',
                '(count "Albert")',
                '(count nil)',
            ],
        },
        'get': {
            title: 'get',
            category: 'Collection',
            linkName: 'get',
            returns: {
                type: 'any',
            },
            args: {
                'coll': {
                    type: 'collection',
                },
                'key': {
                    type: ['string', 'integer'],
                },
                'not-found': {
                    type: 'any',
                    description: 'Default value to return if $key is not found.',
                },
            },
            variants: [
                { argumentNames: ['coll', 'key'] },
                { argumentNames: ['coll', 'key', 'not-found'] },
            ],
            description: 'Returns value in $coll mapped at \`key\`.',
            examples: [
                "\n(get\n  [1 2 3]\n  1)",
                "\n(get\n  []\n  1)",
                "\n(get\n  []\n  1\n  \"default\")",
                "\n(get\n  (object :a 1)\n  :a)",
                "\n(get\n  (object :a 1)\n  :b)",
                "\n(get\n  (object :a 1)\n  :b\n  \"default\")",
                "\n(get\n  nil\n  :a)",
                "\n(get\n  nil\n  :b\n  \"default\")",
            ],
        },
        'get_in': {
            title: 'get_in',
            category: 'Collection',
            linkName: 'get_in',
            returns: {
                type: 'any',
            },
            args: {
                'coll': {
                    type: 'collection',
                },
                'keys': {
                    type: 'array',
                },
                'not-found': {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['coll', 'keys'] },
                { argumentNames: ['coll', 'keys', 'not-found'] },
            ],
            description: 'Returns the value in a nested collection, where $keys is an array of keys. Returns $not-found if the key is not present. If $not-found is not set, `nil` is returned.',
            examples: [
                "\n(get_in\n  [[1 2 3] [4 {:a \"Kalle\"} 6]]\n  [1 1 :a 0])",
                "\n(get_in\n  [[1 2 3] [4 {:a \"Kalle\"} 6]]\n  [1 1 :b 0])",
                "\n(get_in\n  [[1 2 3] [4 {:a \"Kalle\"} 6]]\n  [1 1 :b 0]\n  \"Lisa\")",
            ],
        },
        'contains?': {
            title: 'contains?',
            category: 'Collection',
            linkName: 'contains-question',
            returns: {
                type: 'boolean',
            },
            args: {
                coll: {
                    type: ['collection', 'null'],
                },
                key: {
                    type: ['string', 'number'],
                },
            },
            variants: [
                { argumentNames: ['coll', 'key'] },
            ],
            description: 'Returns `true` if $coll contains $key, otherwise returns `false`. For strings, it checks if substring is included.',
            examples: [
                "\n(contains?\n  []\n  1)",
                "\n(contains?\n  [1]\n  1)",
                "\n(contains?\n  [1 2 3]\n  1)",
                "\n(contains?\n  {}\n  :a)",
                "\n(contains?\n  {:a 1 :b 2}\n  :a)",
            ],
        },
        'has?': {
            title: 'has?',
            category: 'Collection',
            linkName: 'has-question',
            clojureDocs: null,
            returns: {
                type: 'boolean',
            },
            args: {
                coll: {
                    type: ['collection', 'null'],
                },
                value: {
                    type: ['any'],
                    description: 'Only support primitive values: `number`, `string`, `boolean`, or `nil`.',
                },
            },
            variants: [
                { argumentNames: ['coll', 'value'] },
            ],
            description: 'Returns `true` if $coll has $value, otherwise returns `false`.',
            examples: [
                "\n(has?\n  [1 2 3]\n  1)",
                "\n(has?\n  [1 2 3]\n  0)",
                "\n(has?\n  {:a 1 :b 2}\n  1)",
                "\n(has?\n  {:a 1 :b 2}\n  0)",
                "\n(has?\n  \"Albert\"\n  :A)",
                "\n(has?\n  \"Albert\"\n  :a)",
                "\n(has?\n  nil\n  :a)",
            ],
        },
        'has_some?': {
            title: 'has_some?',
            category: 'Collection',
            linkName: 'has_some-question',
            clojureDocs: null,
            returns: {
                type: 'boolean',
            },
            args: {
                coll: {
                    type: 'collection',
                },
                values: {
                    type: 'any',
                    array: true,
                    description: 'Only support primitive values: `number`, `string`, `boolean`, or `nil`.',
                },
            },
            variants: [
                { argumentNames: ['coll', 'values'] },
            ],
            description: 'Returns `true` if $coll has any of the elements in $values, otherwise returns `false`.',
            examples: [
                "\n(has_some?\n  []\n  [])",
                "\n(has_some?\n  [1 2 3]\n  [])",
                "\n(has_some?\n  [1 2 3]\n  [0])",
                "\n  (has_some?\n  [1 2 3]\n  [0 1])",
                "\n(has_some?\n  (object :a 1 :b 2)\n  [0])",
                "\n(has_some?\n  (object :a 1 :b 2)\n  [0 1])",
                "\n(has_some?\n  \"Albert\"\n  \"xyz\")",
                "\n(has_some?\n  \"Albert\"\n  \"xyzl\")",
                "\n(has_some?\n  [:a :b :c :d]\n  \"xyz\")",
                "\n(has_some?\n  [:a :b :c :d]\n  \"xyzc\")",
                "\n(has_some?\n  nil\n  [1])",
                "\n(has_some?\n  nil\n  \"\")",
            ],
        },
        'has_every?': {
            title: 'has_every?',
            category: 'Collection',
            linkName: 'has_every-question',
            clojureDocs: null,
            returns: {
                type: 'boolean',
            },
            args: {
                coll: {
                    type: 'collection',
                },
                values: {
                    type: 'any',
                    array: true,
                    description: 'Only support primitive values: `number`, `string`, `boolean`, or `nil`.',
                },
            },
            variants: [
                { argumentNames: ['coll', 'values'] },
            ],
            description: 'Returns `true` if $coll has all of the elements in $values, otherwise returns `false`.',
            examples: [
                "\n(has_every?\n  []\n  [])",
                "\n(has_every?\n  [1 2 3]\n  [])",
                "\n(has_every?\n  [1 2 3]\n  [0 1])",
                "\n(has_every?\n  [1 2 3]\n  [1 2])",
                "\n(has_every?\n  (object :a 1 :b 2)\n  [0 1])",
                "\n(has_every?\n  (object :a 1 :b 2)\n  [1 2])",
                "\n(has_every?\n  \"Albert\"\n  \"xyz\")",
                "\n(has_every?\n  \"Albert\"\n  \"treblA\")",
                "\n(has_every?\n  [:a :b :c :d]\n  \"xyz\")",
                "\n(has_every?\n  [:a :b :c :d]\n  \"dcba\")",
                "\n(has_every?\n  nil\n  \"abc\")",
                "\n(has_every?\n  nil\n  [0, 1, nil])",
                "\n(has_every?\n  nil\n  nil)",
                "\n(has_every?\n  [1, 2, 3]\n  nil)",
            ],
        },
        'assoc': {
            title: 'assoc',
            category: 'Collection',
            linkName: 'assoc',
            returns: {
                type: 'collection',
            },
            args: {
                coll: {
                    type: 'collection',
                },
                key: {
                    type: ['string', 'number'],
                },
                value: {
                    type: 'any',
                },
                kvs: {
                    type: 'any',
                    description: 'Key-value pairs to associate.',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['coll', 'key', 'value'] },
                { argumentNames: ['coll', 'key', 'value', 'kvs'] },
            ],
            description: "\nAdd or replace the value of element $key to $value in $coll. Repeated for all key-value pairs in $kvs.  \nIf $coll is an 'array', $key must be `number` satisfying `0 <=` $key `<= length`.",
            examples: [
                "\n(assoc\n  [1 2 3]\n  1\n  \"Two\")",
                "\n(assoc\n  [1 2 3]\n  3\n  \"Four\")",
                "\n(assoc\n  {:a 1 :b 2}\n  :a\n  \"One\")",
                "\n(assoc\n  {:a 1 :b 2}\n  :c\n  \"Three\")",
                "\n(assoc\n  :Albert\n  6\n  :a)",
            ],
        },
        'assoc_in': {
            title: 'assoc_in',
            category: 'Collection',
            linkName: 'assoc_in',
            returns: {
                type: 'collection',
            },
            args: {
                coll: {
                    type: 'collection',
                },
                keys: {
                    type: ['number', 'string'],
                    array: true,
                },
                value: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['coll', 'keys', 'value'] },
            ],
            description: "\nAssociates a value in the nested collection $coll, where $keys is an array of keys and $value is the new value.\n\nIf any levels do not exist, objects will be created - and the corresponding keys must be of type string.",
            examples: [
                "\n(assoc_in\n  {}\n  [:a :b :c]\n  \"Albert\")",
                "\n(assoc_in\n  [1 2 [1 2 3]]\n  [2 1]\n  \"Albert\")",
                "\n(assoc_in\n  [1 2 {\"name\" \"albert\"}]\n  [2 \"name\" 0]\n  :A)",
            ],
        },
        'concat': {
            title: 'concat',
            category: 'Collection',
            linkName: 'concat',
            returns: {
                type: 'collection',
            },
            args: {
                coll: {
                    type: 'collection',
                },
                colls: {
                    type: 'collection',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['coll'] },
                { argumentNames: ['coll', 'colls'] },
            ],
            description: 'Concatenates collections into one collection.',
            examples: [
                '(concat :A :l :b :e :r :t)',
                '(concat [1 2] [3 4])',
                '(concat [] [3 4])',
                '(concat [1 2] [])',
                '(concat [1 2] [3 4] [5 6])',
                '(concat [])',
                '(concat {:a 1 :b 2} {:b 1 :c 2})',
                '(concat {} {:a 1})',
            ],
        },
        'not_empty': {
            title: 'not_empty',
            category: 'Collection',
            linkName: 'not_empty',
            returns: {
                type: 'boolean',
            },
            args: {
                coll: {
                    type: ['collection', 'null'],
                },
            },
            variants: [
                { argumentNames: ['coll'] },
            ],
            description: 'Returns `nil` if $coll is empty or `nil`, otherwise $coll.',
            examples: [
                '(not_empty [])',
                '(not_empty [1 2 3])',
                '(not_empty {})',
                '(not_empty {:a 2})',
                '(not_empty "")',
                '(not_empty "Albert")',
                '(not_empty nil)',
            ],
        },
        'every?': {
            title: 'every?',
            category: 'Collection',
            linkName: 'every-question',
            returns: {
                type: 'boolean',
            },
            args: {
                coll: {
                    type: 'collection',
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['coll', 'fn'] },
            ],
            description: 'Returns `true` if all entries in $coll pass the test implemented by $fn, otherwise returns `false`.',
            examples: [
                "\n(every?\n[\"Albert\" \"Mojir\" 160 [1 2]]\n  string?)",
                "\n(every?\n[50 100 150 200]\n  (fn [x] (> x 10)))",
                "\n(every?\n  []\n  number?)",
                "\n(every?\n  \"\"\n  number?)",
                "\n(every?\n  {}\n  number?)",
                "\n(every?\n  {:a 2 :b 4}\n  #(even? (second %)))",
                "\n(every?\n  {:a 2 :b 3}\n  #(even? (second %)))",
            ],
        },
        'not_every?': {
            title: 'not_every?',
            category: 'Collection',
            linkName: 'not_every-question',
            returns: {
                type: 'boolean',
            },
            args: {
                coll: {
                    type: 'collection',
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['coll', 'fn'] },
            ],
            description: 'Returns `true` if at least one element in $coll does not pass the test implemented by $fn, otherwise returns `false`.',
            examples: [
                "\n(not_every?\n  [\"Albert\" \"Mojir\" 160 [1 2]]\n  string?)",
                "\n(not_every?\n  [50 100 150 200]\n  (fn [x] (> x 10)))",
                "\n(not_every?\n  []\n  number?)",
                "\n(not_every?\n  \"\"\n  number?)",
                "\n(not_every?\n  {}\n  number?)",
                "\n(not_every?\n  {:a 2 :b 4}\n  #(even? (second %)))",
                "\n(not_every?\n  {:a 2 :b 3}\n  #(even? (second %)))",
            ],
        },
        'any?': {
            title: 'any?',
            category: 'Collection',
            linkName: 'any-question',
            returns: {
                type: 'boolean',
            },
            args: {
                coll: {
                    type: 'collection',
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['coll', 'fn'] },
            ],
            description: 'Returns `true` if any element in $coll pass the test implemented by $fn, otherwise returns `false`.',
            examples: [
                "\n(any?\n  [\"Albert\" \"Mojir\" 160 [1 2]]\n  string?)",
                "\n(any?\n  [50 100 150 200]\n  (fn [x] (> x 10)))",
                "\n(any?\n  []\n  number?)",
                "\n(any?\n  \"\"\n  number?)",
                "\n(any?\n  {}\n  number?)",
                "\n(any?\n  {:a 2 :b 3}\n  #(even? (second %)))",
                "\n(any?\n  {:a 1 :b 3}\n  #(even? (second %)))",
            ],
        },
        'not_any?': {
            title: 'not_any?',
            category: 'Collection',
            linkName: 'not_any-question',
            returns: {
                type: 'boolean',
            },
            args: {
                coll: {
                    type: 'collection',
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['coll', 'fn'] },
            ],
            description: 'Returns `false` if any element in $coll pass the test implemented by $fn, otherwise returns `true`.',
            examples: [
                "\n(not_any?\n  [\"Albert\" \"Mojir\" 160 [1 2]]\n  string?)",
                "\n(not_any?\n  [50 100 150 200]\n  (fn [x] (> x 10)))",
                "\n(not_any?\n  []\n  number?)",
                "\n(not_any?\n  \"\"\n  number?)",
                "\n(not_any?\n  {}\n  number?)",
                "\n(not_any?\n  {:a 2 :b 3}\n  #(even? (second %)))",
                "\n(not_any?\n  {:a 1 :b 3}\n  #(even? (second %)))",
            ],
        },
        'update': {
            title: 'update',
            category: 'Collection',
            linkName: 'update',
            returns: {
                type: 'collection',
            },
            args: {
                'coll': {
                    type: 'collection',
                },
                'key': {
                    type: ['string', 'number'],
                },
                'fn': {
                    type: 'function',
                },
                'fn-args': {
                    type: 'any',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['coll', 'value', 'fn'] },
                { argumentNames: ['coll', 'value', 'fn', 'fn-args'] },
            ],
            description: "\nUpdates a value in the $coll collection, where $key is a key. $fn is a function\nthat will take the old value and any supplied $fn-args and\nreturn the new value.\nIf the key does not exist, `nil` is passed as the old value.",
            examples: [
                "\n(def x {:a 1 :b 2})\n(update x :a inc)",
                "\n(def x {:a 1 :b 2})\n(update\n  x\n  :c\n  (fn [val]\n    (if (nil? val) 0 (inc val))))",
            ],
        },
        'update_in': {
            title: 'update_in',
            category: 'Collection',
            linkName: 'update_in',
            returns: {
                type: 'collection',
            },
            args: {
                'coll': {
                    type: 'collection',
                },
                'keys': {
                    type: 'array',
                },
                'fn': {
                    type: 'function',
                },
                'fn-args': {
                    type: 'any',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['coll', 'keys', 'fn'] },
                { argumentNames: ['coll', 'keys', 'fn', 'fn-args'] },
            ],
            description: "Updates a value in the $coll collection, where $keys is an array of\nkeys and $fn is a function that will take the old value and\nany supplied $fn-args and return the new value. If any levels do not exist,\nobjects will be created - and the corresponding keys must be of type string.",
            examples: [
                "\n(update_in\n  {:a [1 2 3]}\n  [:a 1]\n  (fn [val]\n    (when (nil? val) 0)))",
                "\n(update_in\n  {:a {:foo :bar}}\n  [:a :foo]\n  (fn [val]\n    (if (nil? val) \"?\" \"!\")))",
                "\n(update_in\n  {:a {:foo :bar}}\n  [:a :baz]\n  (fn [val]\n    (if (nil? val) \"?\" \"!\")))",
                "\n(update_in\n  {:a [1 2 3]}\n  [:a 1]\n  *\n  10\n  10\n  10)",
            ],
        },
    };

    var functionalReference = {
        apply: {
            title: 'apply',
            category: 'Functional',
            linkName: 'apply',
            returns: {
                type: 'any',
            },
            args: {
                fn: {
                    type: 'function',
                },
                args: {
                    type: 'array',
                },
            },
            variants: [
                { argumentNames: ['fn', 'args'] },
            ],
            description: 'Call supplied function $fn with specified arguments $args.',
            examples: [
                "\n(apply + [1 2 3])",
                "\n(apply\n  (fn [x y]\n    (sqrt\n      (+\n        (* x x)\n        (* y y))))\n  [3 4])",
            ],
        },
        identity: {
            title: 'identity',
            category: 'Functional',
            linkName: 'identity',
            returns: {
                type: 'any',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns $x.',
            examples: ['(identity 1)', '(identity "Albert")', '(identity {:a 1})', '(identity nil)'],
        },
        partial: {
            title: 'partial',
            category: 'Functional',
            linkName: 'partial',
            returns: {
                type: 'function',
            },
            args: {
                fn: {
                    type: 'function',
                },
                args: {
                    type: 'any',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['fn', 'args'] },
            ],
            description: "Takes a function $fn and a optional number arguments $args to $fn.\nIt returns a function that takes the additional additional arguments.\nWhen called, the returned function calls `(`$fn `...`$args` ...additional_arguments)`.",
            examples: [
                '(partial + 100)',
                "\n(def plusMany (partial + 100 1000))\n(plusMany 1 10)",
                "\n(def addHundred (partial + 100))\n(addHundred 10)",
            ],
        },
        comp: {
            title: 'comp',
            category: 'Functional',
            linkName: 'comp',
            returns: {
                type: 'function',
            },
            args: {
                fns: {
                    type: 'function',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['fns'] },
            ],
            description: "Takes a variable number of functions and returns a function that is the composition of those.\n\n  The returned function takes a variable number of arguments,\n  applies the rightmost function to the args,\n  the next function (right-to-left) to the result, etc.",
            examples: [
                "\n(def negative-quotient (comp - /))\n(negative-quotient 9 3)",
                "\n(\n  #((apply comp first (repeat %2 rest)) %1)\n  [1 2 3 4 5 6 7]\n  3)",
                "\n(def x {\"bar\" {\"foo\" 42}})\n((comp \"foo\" \"bar\") x)",
            ],
        },
        constantly: {
            title: 'constantly',
            category: 'Functional',
            linkName: 'constantly',
            returns: {
                type: 'function',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns a function that takes any number of arguments and always returns $x.',
            examples: [
                "\n(def always-true (constantly true))\n(always-true 9 3)",
                "\n(\n  #((apply constantly first (repeat %2 rest)) %1)\n  [1 2 3 4 5 6 7]\n  3)",
            ],
        },
        juxt: {
            title: 'juxt',
            category: 'Functional',
            linkName: 'juxt',
            returns: {
                type: 'function',
            },
            args: {
                fn: {
                    type: 'function',
                },
                fns: {
                    type: 'function',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['fn'] },
                { argumentNames: ['fn', 'fns'] },
            ],
            description: "Takes one or many function and returns a function that is the juxtaposition of those functions.  \nThe returned function takes a variable number of args,\nand returns a vector containing the result of applying each function to the args (left-to-right).",
            examples: [
                "\n(\n  (juxt + * min max)\n  3\n  4\n  6)",
                "\n(\n  (juxt :a :b)\n  {:a 1, :b 2, :c 3, :d 4})",
                "\n(apply\n  (juxt + * min max)\n  (range 1 11))",
            ],
        },
        complement: {
            title: 'complement',
            category: 'Functional',
            linkName: 'complement',
            returns: {
                type: 'function',
            },
            args: {
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['fn'] },
            ],
            description: 'Takes a function $fn and returns a new function that takes the same arguments as f, has the same effects, if any, and returns the opposite truth value.',
            examples: ['((complement >) 1 3)', '((complement <) 1 3)', '((complement +) 1 3)', '((complement +) 0 0)'],
        },
        every_pred: {
            title: 'every_pred',
            category: 'Functional',
            linkName: 'every_pred',
            returns: {
                type: 'function',
            },
            args: {
                fn: {
                    type: 'function',
                },
                fns: {
                    type: 'function',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['fn'] },
                { argumentNames: ['fn', 'fns'] },
            ],
            description: "\nTakes a number of predicates and returns a function that returns `true` if all predicates\nreturn a truthy value against all of its arguments, else it returns `false`.",
            examples: [
                "\n(\n  (every_pred string? #(> (count %1) 3))\n  \"Albert\"\n  \"Mojir\")",
                "\n(\n  (every_pred string? #(> (count %1) 3))\n  \"Albert\"\n  :M)",
                "\n(\n  (every_pred string? #(> (count %1) 3))\n  \"Albert\"\n  [1 2 3])",
            ],
        },
        some_pred: {
            title: 'some_pred',
            category: 'Functional',
            linkName: 'some_pred',
            clojureDocs: null,
            returns: {
                type: 'function',
            },
            args: {
                fn: {
                    type: 'function',
                },
                fns: {
                    type: 'function',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['fn'] },
                { argumentNames: ['fn', 'fns'] },
            ],
            description: 'Takes a number of `predicates` and returns a function that returns \`true\` if at least one of the `predicates` return a truthy \`true\` value against at least one of its arguments, else it returns `false`.',
            examples: [
                '((some_pred string? #(> (count %1) 3)) "Albert" "Mojir")',
                '((some_pred string? #(> (count %1) 3)) :A :M)',
                '((some_pred string? #(> (count %1) 3)) :A [1 2 3])',
                '((some_pred string? #(> (count %1) 3)) [1 2 3] [2])',
            ],
        },
        fnil: {
            title: 'fnil',
            category: 'Functional',
            linkName: 'fnil',
            returns: {
                type: 'function',
            },
            args: {
                fn: {
                    type: 'function',
                },
                arg: {
                    type: 'any',
                },
                args: {
                    type: 'any',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['fn', 'arg'] },
                { argumentNames: ['fn', 'arg', 'args'] },
            ],
            description: 'Takes a function $fn, and returns a function that calls $fn, replacing a nil argument to the corresponding argument.',
            examples: [
                '((fnil + 1 2) 0 0)',
                '((fnil + 1 2) nil 0)',
                '((fnil + 1 2) 0 nil)',
                '((fnil + 1 2) nil nil)',
                '((fnil + 1 2) nil nil 3 4)',
            ],
        },
    };

    var arrayReference = {
        array: {
            title: 'array',
            category: 'Array',
            linkName: 'array',
            clojureDocs: 'vector',
            returns: {
                type: 'any',
                array: true,
            },
            args: {
                values: {
                    type: 'any',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['values'] },
            ],
            description: 'Makes new array from $values.',
            examples: [
                '(array 1 2 3)',
                '(array (array nil false true))',
                '[]',
                '[1 2 3]',
                '[[nil false true]]',
                '[]',
                '([1 2 3] 1)',
                '([1 2 3 4 5 6 7 8 9] 3)',
            ],
        },
        range: {
            title: 'range',
            category: 'Array',
            linkName: 'range',
            returns: {
                type: 'number',
                array: true,
            },
            args: {
                start: {
                    type: 'number',
                },
                end: {
                    type: 'number',
                },
                step: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['end'] },
                { argumentNames: ['start', 'end'] },
                { argumentNames: ['start', 'end', 'step'] },
            ],
            description: "$range creates an array with a range of numbers from $start to $end (exclusive), by $step.\n\n$start defaults to 0.  \n$step defaults to 1.",
            examples: [
                '(range 4)',
                '(range 1 4)',
                '(range 0.4 4.9)',
                "\n(range\n  0.25 ;; start value\n  1    ;; end value (exclusive)\n  0.25 ;; step value\n)",
            ],
        },
        repeat: {
            title: 'repeat',
            category: 'Array',
            linkName: 'repeat',
            returns: {
                type: 'any',
                array: true,
            },
            args: {
                x: {
                    type: 'any',
                },
                n: {
                    type: 'integer',
                },
            },
            variants: [{
                    argumentNames: ['x', 'n'],
                }],
            description: 'Returns an array with $x repeated $n times.',
            examples: [
                '(repeat 10 3)',
                '(repeat 10 0)',
                '(repeat "Albert" 5)',
            ],
        },
        flatten: {
            title: 'flatten',
            category: 'Array',
            linkName: 'flatten',
            returns: {
                type: 'any',
                array: true,
            },
            args: {
                x: {
                    type: ['array', 'any'],
                    description: 'If $x is not an array, `[ ]` is returned.',
                },
            },
            variants: [{
                    argumentNames: ['x'],
                }],
            description: 'Takes a nested array $x and flattens it.',
            examples: [
                '(flatten [1 2 [3 4] 5])',
                "\n(let [foo :bar]\n  (flatten\n    [1\n     \" 2 A \"\n     [foo [4 [:ABC]]] 6]))",
                '(flatten 12)',
            ],
        },
        mapcat: {
            title: 'mapcat',
            category: 'Array',
            linkName: 'mapcat',
            returns: {
                type: 'collection',
            },
            args: {
                f: {
                    type: 'function',
                },
                colls: {
                    type: 'collection',
                    array: true,
                },
            },
            variants: [{
                    argumentNames: ['f', 'colls'],
                }],
            description: 'Returns the result of applying concat to the result of applying map to $f and $colls.',
            examples: [
                '(mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])',
                '(mapcat reverse [[3 2 1 0] [6 [5] 4] [9 8 7]])',
                '(defn foo [n] [(- n 1) n (+ n 1)]) (mapcat foo [1 2 3])',
                '(mapcat #(remove even? %1) [[1 2] [2 2] [2 3]])',
            ],
        },
    };

    var sequenceReference = {
        nth: {
            title: 'nth',
            category: 'Sequence',
            linkName: 'nth',
            returns: {
                type: 'any',
            },
            args: {
                'seq': {
                    type: ['sequence', 'null'],
                },
                'n': {
                    type: 'integer',
                },
                'not-found': {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['seq', 'n'] },
                { argumentNames: ['seq', 'n', 'not-found'] },
            ],
            description: 'Accesses element $n of $seq. Accessing out-of-bounds indices returns $not-found, if present, else `nil`.',
            examples: [
                '(nth [1 2 3] 1)',
                '(nth [1 2 3] 3)',
                '(nth [1 2 3] -1)',
                '(nth [1 2 3] 3 99)',
                '(nth "A string" 1)',
                '(nth "A string" 3)',
                '(nth "A string" -3)',
                '(nth "A string" 30 :X)',
                '(nth nil 1)',
                '(nth nil 1 "Default value")',
            ],
        },
        push: {
            title: 'push',
            category: 'Sequence',
            linkName: 'push',
            clojureDocs: null,
            returns: {
                type: 'sequence',
                array: true,
            },
            args: {
                seq: {
                    type: 'sequence',
                    array: true,
                },
                values: {
                    type: 'any',
                    rest: true,
                    description: 'At least one.',
                },
            },
            variants: [
                { argumentNames: ['seq', 'values'] },
            ],
            description: 'Returns copy of $seq with $values added to the end of it.',
            examples: [
                '(push [1 2 3] 4)',
                '(push [1 2 3] 4 5 6)',
                '(def l [1 2 3]) (push l 4) l',
            ],
        },
        pop: {
            title: 'pop',
            category: 'Sequence',
            linkName: 'pop',
            returns: {
                type: ['sequence', 'null'],
                array: true,
            },
            args: {
                seq: {
                    type: 'sequence',
                    array: true,
                },
            },
            variants: [
                { argumentNames: ['seq'] },
            ],
            description: 'Returns a copy of $seq with last element removed. If $seq is empty `nil` is returned.',
            examples: [
                '(pop [1 2 3])',
                '(pop [])',
            ],
        },
        unshift: {
            title: 'unshift',
            category: 'Sequence',
            linkName: 'unshift',
            clojureDocs: null,
            returns: {
                type: 'sequence',
                array: true,
            },
            args: {
                seq: {
                    type: 'sequence',
                    array: true,
                },
                values: {
                    type: 'any',
                    array: true,
                },
            },
            variants: [
                { argumentNames: ['seq', 'values'] },
            ],
            description: 'Returns copy of $seq with $values added to the beginning.',
            examples: [
                '(unshift [1 2 3] 4)',
                '(unshift [1 2 3] 4 5 6)',
                "\n(def l [1 2 3])\n(unshift l 4)\nl",
            ],
        },
        shift: {
            title: 'shift',
            category: 'Sequence',
            linkName: 'shift',
            clojureDocs: null,
            returns: {
                type: ['sequence', 'null'],
                array: true,
            },
            args: {
                seq: {
                    type: 'sequence',
                    array: true,
                },
            },
            variants: [
                { argumentNames: ['seq'] },
            ],
            description: 'Returns a copy of $seq with first element removed. If $seq is empty `nil` is returned.',
            examples: [
                '(shift [1 2 3])',
                '(shift [])',
            ],
        },
        slice: {
            title: 'slice',
            category: 'Sequence',
            linkName: 'slice',
            clojureDocs: null,
            returns: {
                type: 'any',
                array: true,
            },
            args: {
                seq: {
                    type: 'sequence',
                    array: true,
                },
                start: {
                    type: 'integer',
                    description: 'Defaults to `0`.',
                },
                end: {
                    type: 'integer',
                    description: 'Defaults lenght of sequence + 1.',
                },
            },
            variants: [
                { argumentNames: ['seq'] },
                { argumentNames: ['seq', 'start'] },
                { argumentNames: ['seq', 'start', 'end'] },
            ],
            description: 'Returns a copy of a portion of $seq from index $start (inclusive) to $end (exclusive).',
            examples: [
                '(slice [1 2 3 4 5] 2 4)',
                '(slice [1 2 3 4 5] 2)',
            ],
        },
        reductions: {
            title: 'reductions',
            category: 'Sequence',
            linkName: 'reductions',
            returns: {
                type: 'any',
                array: true,
            },
            args: {
                fn: {
                    type: 'function',
                },
                seq: {
                    type: 'sequence',
                    array: true,
                },
                start: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['fn', 'seq'] },
                { argumentNames: ['fn', 'start', 'seq'] },
            ],
            description: 'Returns an array of the intermediate values of the reduction (see `reduce`) of $seq by $fn.',
            examples: [
                '(reductions + [1 2 3])',
                '(reductions + 10 [1 2 3])',
                '(reductions + 0 [])',
                "\n(reductions\n  (fn [result value] (+ result (if (even? value) value 0)))\n  0\n  [1 2 3 4 5 6 7 8 9])",
            ],
        },
        reduce: {
            title: 'reduce',
            category: 'Sequence',
            linkName: 'reduce',
            returns: {
                type: 'any',
            },
            args: {
                fn: {
                    type: 'function',
                },
                seq: {
                    type: 'sequence',
                },
                start: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['fn', 'seq'] },
                { argumentNames: ['fn', 'start', 'seq'] },
            ],
            description: 'Runs $fn function on each element of the $seq, passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $seq is a single value.',
            examples: [
                '(reduce + [1 2 3])',
                '(reduce + 0 [1 2 3])',
                '(reduce + 0 [])',
                "\n(reduce\n  (fn [result value] (+ result (if (even? value) value 0)))\n  0\n  [1 2 3 4 5 6 7 8 9])",
            ],
        },
        reduce_right: {
            title: 'reduce_right',
            category: 'Sequence',
            linkName: 'reduce_right',
            clojureDocs: null,
            returns: {
                type: 'sequence',
            },
            args: {
                fn: {
                    type: 'function',
                },
                seq: {
                    type: 'sequence',
                },
                start: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['fn', 'seq'] },
                { argumentNames: ['fn', 'start', 'seq'] },
            ],
            description: 'Runs $fn function on each element of the $seq (starting from the last item), passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $seq is a single value.',
            examples: [
                '(reduce_right str [:A :B :C] "")',
            ],
        },
        map: {
            title: 'map',
            category: 'Sequence',
            linkName: 'map',
            returns: {
                type: 'any',
                array: true,
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['seq', 'fn'] },
            ],
            description: 'Creates a new array populated with the results of calling $fn on every elements in $seq.',
            examples: [
                '(map ["Albert" "Mojir" 42] str)',
                '(map [1 2 3] inc)',
            ],
        },
        filter: {
            title: 'filter',
            category: 'Sequence',
            linkName: 'filter',
            returns: {
                type: 'sequence',
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['seq', 'fn'] },
            ],
            description: 'Creates a new array with all elements that pass the test implemented by $fn.',
            examples: [
                "\n(filter\n  [\"Albert\" \"Mojir\" 160 [1 2]]\n  string?)",
                "\n(filter\n[5 10 15 20]\n  (fn [x] (> x 10)))",
            ],
        },
        position: {
            title: 'position',
            category: 'Sequence',
            linkName: 'position',
            clojureDocs: null,
            returns: {
                type: ['number', 'null'],
            },
            args: {
                seq: {
                    type: ['sequence', 'null'],
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['seq', 'fn'] },
            ],
            description: 'Returns the index of the first elements that passes the test implemented by $fn. If no element was found, `nil` is returned.',
            examples: [
                "\n(position\n  [\"Albert\" \"Mojir\" 160 [1 2]]\n  string?)",
                "\n(position\n  [5 10 15 20]\n  (fn [x] (> x 10)))",
                "\n(position\n  [5 10 15 20]\n  (fn [x] (> x 100)))",
                "\n(position\n  (fn [x] (> x 100))\n  nil)",
            ],
        },
        index_of: {
            title: 'index_of',
            category: 'Sequence',
            linkName: 'index_of',
            clojureDocs: null,
            returns: {
                type: ['number', 'null'],
            },
            args: {
                seq: {
                    type: ['sequence', 'null'],
                },
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['seq', 'x'] },
            ],
            description: 'Returns the index of $x in $seq. If element is not present in $seq `nil` is returned.',
            examples: [
                '(index_of ["Albert" "Mojir" 160 [1 2]] "Mojir")',
                '(index_of [5 10 15 20] 15)',
                '(index_of [5 10 15 20] 1)',
                '(index_of nil 1)',
            ],
        },
        some: {
            title: 'some',
            category: 'Sequence',
            linkName: 'some',
            returns: {
                type: 'any',
            },
            args: {
                seq: {
                    type: ['sequence', 'null'],
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['seq', 'fn'] },
            ],
            description: 'Returns the first element that passes the test implemented by $fn. I no element was found, `nil` is returned.',
            examples: [
                "\n(some\n  [\"Albert\" \"Mojir\" 160 [1 2]]\n  string?)",
                "\n(some\n  [5 10 15 20]\n  (fn [x] (> x 10)))",
                "\n(some\n  [1 2 3 4]\n  (fn [x] (> x 10)))",
                "\n(some\n  []\n  (fn [x] (> x 10)))",
                "\n(some\n  nil\n  (fn [x] (> x 10)))",
            ],
        },
        reverse: {
            title: 'reverse',
            category: 'Sequence',
            linkName: 'reverse',
            returns: {
                type: ['sequence', 'null'],
            },
            args: {
                seq: {
                    type: ['sequence', 'null'],
                },
            },
            variants: [
                { argumentNames: ['seq'] },
            ],
            description: 'If $seq is an array, creates a new array with the elements from $seq in reversed order. If $seq is a string, returns new reversed string.',
            examples: [
                '(reverse ["Albert" "Mojir" 160 [1 2]])',
                '(reverse [])',
                '(reverse "Albert")',
                '(reverse nil)',
            ],
        },
        first: {
            title: 'first',
            category: 'Sequence',
            linkName: 'first',
            returns: {
                type: 'any',
            },
            args: {
                seq: {
                    type: ['sequence', 'null'],
                },
            },
            variants: [
                { argumentNames: ['seq'] },
            ],
            description: 'Returns the first element of $seq. If $seq is empty or `nil`, `nil` is returned.',
            examples: [
                '(first ["Albert" "Mojir" 160 [1 2]])',
                '(first [])',
                '(first nil)',
            ],
        },
        second: {
            title: 'second',
            category: 'Sequence',
            linkName: 'second',
            returns: {
                type: 'any',
            },
            args: {
                seq: {
                    type: ['sequence', 'null'],
                },
            },
            variants: [
                { argumentNames: ['seq'] },
            ],
            description: 'Returns the second element of $seq. If $seq has less than two elements or is `nil`, `nil` is returned.',
            examples: [
                '(second ["Albert" "Mojir" 160 [1 2]])',
                '(second [1])',
                '(second [])',
                '(second nil)',
            ],
        },
        last: {
            title: 'last',
            category: 'Sequence',
            linkName: 'last',
            returns: {
                type: 'any',
            },
            args: {
                seq: {
                    type: ['sequence', 'null'],
                },
            },
            variants: [
                { argumentNames: ['seq'] },
            ],
            description: 'Returns the last element of $seq. If $seq is empty, `nil` is returned.',
            examples: [
                '(last ["Albert" "Mojir" 160 [1 2]])',
                '(last [1 2])',
                '(last [1])',
                '(last [])',
                '(last nil)',
            ],
        },
        rest: {
            title: 'rest',
            category: 'Sequence',
            linkName: 'rest',
            returns: {
                type: ['sequence', 'null'],
            },
            args: {
                seq: {
                    type: 'sequence',
                },
            },
            variants: [
                { argumentNames: ['seq'] },
            ],
            description: "If $seq is an array, returns a new array with all but the first element from $seq.\nIf $seq has less than two elements, an empty array is returned.\nFor string $seq returns all but the first characters in $seq.",
            examples: [
                '(rest ["Albert" "Mojir" 160 [1 2]])',
                '(rest ["Albert"])',
                '(rest [])',
                '(rest "Albert")',
                '(rest :A)',
                '(rest "")',
            ],
        },
        nthrest: {
            title: 'nthrest',
            category: 'Sequence',
            linkName: 'nthrest',
            returns: {
                type: 'any',
                array: true,
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                n: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['seq', 'n'] },
            ],
            description: 'If $seq is an array, returns a new array with all but the first $n elements from $seq. For string $seq returns all but the first $n characters in $seq.',
            examples: [
                '(nthrest ["Albert" "Mojir" 160 [1 2]] 2)',
                '(nthrest "Albert" 3)',
                '(nthrest "Albert" 10)',
                '(nthrest [] 0)',
                '(nthrest "" 0)',
            ],
        },
        next: {
            title: 'next',
            category: 'Sequence',
            linkName: 'next',
            returns: {
                type: ['sequence', 'null'],
            },
            args: {
                seq: {
                    type: 'sequence',
                },
            },
            variants: [
                { argumentNames: ['seq'] },
            ],
            description: 'If $seq is an array, returns a new array with all but the first element from $seq. If $seq has less than two elements, `nil` is returned. For string $seq returns all but the first characters in $seq. If length of string $seq is less than two, `nil` is returned.',
            examples: [
                '(next ["Albert" "Mojir" 160 [1 2]])',
                '(next ["Albert"])',
                '(next [])',
                '(next "Albert")',
                '(next :A)',
                '(next "")',
            ],
        },
        nthnext: {
            title: 'nthnext',
            category: 'Sequence',
            linkName: 'nthnext',
            returns: {
                type: ['sequence', 'null'],
                array: true,
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                n: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['seq', 'n'] },
            ],
            description: 'If $seq is an array, returns a new array with all but the first $n elements from $seq. If $seq has less or equal than $n elements, `nil` returned. For string $seq returns all but the first $n characters in $seq. If length of string $seq is less or equal than $n, `nil` is returned.',
            examples: [
                '(nthnext ["Albert" "Mojir" 160 [1 2]] 2)',
                '(nthnext "Albert" 3)',
                '(nthnext "Albert" 6)',
                '(nthnext [] 0)',
                '(nthnext "" 0)',
            ],
        },
        take: {
            title: 'take',
            category: 'Sequence',
            linkName: 'take',
            returns: {
                type: 'sequence',
            },
            args: {
                n: {
                    type: 'integer',
                },
                seq: {
                    type: 'sequence',
                },
            },
            variants: [
                { argumentNames: ['n', 'seq'] },
            ],
            description: 'Constructs a new array/string with the $n first elements from $seq.',
            examples: [
                '(take 3 [1 2 3 4 5])',
                '(take 0 [1 2 3 4 5])',
                '(take 2 "Albert")',
                '(take 50 "Albert")',
            ],
        },
        take_last: {
            title: 'take_last',
            category: 'Sequence',
            linkName: 'take_last',
            returns: {
                type: 'sequence',
            },
            args: {
                n: {
                    type: 'integer',
                },
                seq: {
                    type: 'sequence',
                },
            },
            variants: [
                { argumentNames: ['n', 'seq'] },
            ],
            description: 'Constructs a new array with the $n last elements from $seq.',
            examples: [
                '(take_last 3 [1 2 3 4 5])',
                '(take_last 0 [1 2 3 4 5])',
            ],
        },
        take_while: {
            title: 'take_while',
            category: 'Sequence',
            linkName: 'take_while',
            returns: {
                type: 'sequence',
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['seq', 'fn'] },
            ],
            description: 'Returns the members of $seq in order, stopping before the first one for which `predicate` returns a falsy value.',
            examples: [
                "\n(take_while\n  [1 2 3 2 1]\n  (fn [x] (< x 3)))",
                "\n(take_while\n  [1 2 3 2 1]\n  (fn [x] (> x 3)))",
            ],
        },
        drop: {
            title: 'drop',
            category: 'Sequence',
            linkName: 'drop',
            returns: {
                type: 'sequence',
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                n: {
                    type: 'integer',
                },
            },
            variants: [
                { argumentNames: ['seq', 'n'] },
            ],
            description: 'Constructs a new array/string with the $n first elements dropped from $seq.',
            examples: [
                '(drop [1 2 3 4 5] 3)',
                '(drop [1 2 3 4 5] 0)',
                '(drop "Albert" 2)',
                '(drop "Albert" 50)',
            ],
        },
        drop_last: {
            title: 'drop_last',
            category: 'Sequence',
            linkName: 'drop_last',
            returns: {
                type: 'sequence',
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                n: {
                    type: 'integer',
                },
            },
            variants: [
                { argumentNames: ['seq', 'n'] },
            ],
            description: 'Constructs a new array with the $n last elements dropped from $seq.',
            examples: [
                '(drop_last [1 2 3 4 5] 3)',
                '(drop_last [1 2 3 4 5] 0)',
            ],
        },
        drop_while: {
            title: 'drop_while',
            category: 'Sequence',
            linkName: 'drop_while',
            returns: {
                type: 'sequence',
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['seq', 'fn'] },
            ],
            description: 'Returns the members of $seq in order, skipping the fist elements for witch the `predicate` returns a truethy value.',
            examples: [
                "\n(drop_while\n  [1 2 3 2 1]\n  (fn [x] (< x 3)))",
                "\n(drop_while\n  [1 2 3 2 1]\n  (fn [x] (> x 3)))",
            ],
        },
        sort: {
            title: 'sort',
            category: 'Sequence',
            linkName: 'sort',
            returns: {
                type: 'any',
                array: true,
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['seq'] },
                { argumentNames: ['seq', 'fn'] },
            ],
            description: 'Returns a new sequence with the elements from $seq sorted according to $fn. If no $fn is supplied, builtin `compare` will be used.',
            examples: [
                '(sort [3 1 2])',
                "\n(sort\n  [3 1 2]\n  (fn [a b] (cond (< a b) -1 (> a b) 1 true -1)))",
                "\n(sort\n  [3 1 2]\n  (fn [a b] (cond (> a b) -1 (< a b) 1 true -1)))",
            ],
        },
        sort_by: {
            title: 'sort_by',
            category: 'Sequence',
            linkName: 'sort_by',
            returns: {
                type: 'any',
                array: true,
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                keyfn: {
                    type: 'function',
                },
                comp: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['seq', 'keyfn'] },
                { argumentNames: ['seq', 'keyfn', 'comp'] },
            ],
            description: 'Returns a sorted sequence of the items in $seq, where the sort order is determined by comparing `(keyfn item)`. If no $comp is supplied, uses builtin `compare`.',
            examples: [
                '(sort_by ["Albert" "Mojir" "Nina"] count)',
                '(sort_by "Albert" lower_case #(compare %2 %1))',
            ],
        },
        distinct: {
            title: 'distinct',
            category: 'Sequence',
            linkName: 'distinct',
            returns: {
                type: 'sequence',
            },
            args: {
                seq: {
                    type: 'sequence',
                },
            },
            variants: [
                { argumentNames: ['seq'] },
            ],
            description: 'Returns a copy of $seq with no duplicates.',
            examples: [
                '(distinct [1 2 3 1 3 5])',
                '(distinct "Albert Mojir")',
                '(distinct [])',
                '(distinct "")',
            ],
        },
        remove: {
            title: 'remove',
            category: 'Sequence',
            linkName: 'remove',
            returns: {
                type: 'sequence',
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['seq', 'fn'] },
            ],
            description: 'Returns a new sequence of items in $seq for witch ``(pred item)`` returns a falsy value.',
            examples: [
                '(remove [1 2 3 1 3 5] even?)',
                '(remove "Albert Mojir" #(has? "aoueiyAOUEIY" %1))',
            ],
        },
        remove_at: {
            title: 'remove_at',
            category: 'Sequence',
            linkName: 'remove_at',
            clojureDocs: null,
            returns: {
                type: 'sequence',
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                n: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['seq', 'n'] },
            ],
            description: 'Returns a new sequence of all items in $seq except item at position $n.',
            examples: [
                '(remove_at [1 2 3 1 3 5] 0)',
                '(remove_at [1 2 3 1 3 5] -1)',
                '(remove_at "Albert Mojir" 6)',
            ],
        },
        split_at: {
            title: 'split_at',
            category: 'Sequence',
            linkName: 'split_at',
            returns: {
                type: 'sequence',
                array: true,
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                n: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['seq', 'n'] },
            ],
            description: 'Returns a pair of sequence ``[(take pos input) (drop pos input)]``.',
            examples: [
                '(split_at [1 2 3 4 5] 2)',
                '(split_at "Albert" 2)',
            ],
        },
        split_with: {
            title: 'split_with',
            category: 'Sequence',
            linkName: 'split_with',
            returns: {
                type: 'sequence',
                array: true,
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['seq', 'fn'] },
            ],
            description: 'Returns a pair of sequences ``[(take_while fn input) (drop_while fn input)]``.',
            examples: [
                '(split_with [1 2 3 4 5] #(> %1 3))',
                '(split_with "Albert" #(<= %1 :Z))',
            ],
        },
        frequencies: {
            title: 'frequencies',
            category: 'Sequence',
            linkName: 'frequencies',
            returns: {
                type: 'object',
            },
            args: {
                seq: {
                    type: 'sequence',
                },
            },
            variants: [
                { argumentNames: ['seq'] },
            ],
            description: 'Returns an object from distinct items in $seq to the number of times they appear. Note that all items in $seq must be valid object keys i.e. strings.',
            examples: [
                '(frequencies ["Albert" "Mojir" "Nina" "Mojir"])',
                '(frequencies "Pneumonoultramicroscopicsilicovolcanoconiosis")',
            ],
        },
        group_by: {
            title: 'group_by',
            category: 'Sequence',
            linkName: 'group_by',
            returns: {
                type: 'object',
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['seq', 'fn'] },
            ],
            description: 'Returns an object of the elements of $seq keyed by the result of $fn on each element. The value at each key will be an array of the corresponding elements.',
            examples: [
                '(group_by [{"name" "Albert"} {"name" "Albert"} {"name" "Mojir"}] "name")',
                '(group_by "Albert Mojir" (fn [char] (if (has? "aoueiAOUEI" char) "vowel" "other")))',
            ],
        },
        partition: {
            title: 'partition',
            category: 'Sequence',
            linkName: 'partition',
            returns: {
                type: 'sequence',
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                n: {
                    type: 'number',
                },
                step: {
                    type: 'number',
                },
                pad: {
                    type: 'array',
                },
            },
            variants: [
                { argumentNames: ['seq', 'n'] },
                { argumentNames: ['seq', 'n', 'step'] },
                { argumentNames: ['seq', 'n', 'step', 'pad'] },
            ],
            description: 'Returns an array of sequences of $n items each, at offsets $step apart. If $step is not supplied, defaults to $n. If a $pad array is supplied, use its elements as necessary to complete last partition upto $n items. In case there are not enough padding elements, return a partition with less than $n items.',
            examples: [
                '(partition (range 20) 4)',
                '(partition (range 22) 4)',
                '(partition (range 20) 4 6)',
                '(partition (range 20) 4 3)',
                '(partition (range 20) 3 6 [:a])',
                '(partition (range 20) 4 6 [:a])',
                '(partition (range 20) 4 6 [:a :b :c :d])',
                '(partition [:a :b :c :d :e :f] 3 1)',
                '(partition [1 2 3 4] 10)',
                '(partition [1 2 3 4] 10 10)',
                '(partition [1 2 3 4] 10 10 [])',
                '(partition [1 2 3 4] 10 10 nil)',
                '(partition "superfragilistic" 5)',
                '(partition "superfragilistic" 5 5 nil)',
                '(def foo [5 6 7 8]) (partition foo 2 1 foo)',
            ],
        },
        partition_all: {
            title: 'partition_all',
            category: 'Sequence',
            linkName: 'partition_all',
            returns: {
                type: 'sequence',
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                n: {
                    type: 'number',
                },
                step: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['seq', 'n'] },
                { argumentNames: ['seq', 'n', 'step'] },
            ],
            description: 'Returns an array of sequences like partition, but may include partitions with fewer than n items at the end.',
            examples: [
                '(partition_all [0 1 2 3 4 5 6 7 8 9] 4)',
                '(partition [0 1 2 3 4 5 6 7 8 9] 4)',
                '(partition_all [0 1 2 3 4 5 6 7 8 9] 2 4)',
            ],
        },
        partition_by: {
            title: 'partition_by',
            category: 'Sequence',
            linkName: 'partition_by',
            returns: {
                type: 'sequence',
            },
            args: {
                seq: {
                    type: 'sequence',
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['seq', 'fn'] },
            ],
            description: 'Applies $fn to each value in $seq, splitting it each time $fn returns a new value. Returns an array of sequences.',
            examples: [
                '(partition_by [1 2 3 4 5] #(== 3 %1))',
                '(partition_by [1 1 1 2 2 3 3] odd?)',
                '(partition_by "Leeeeeerrroyyy" identity)',
            ],
        },
    };

    var mathReference = {
        '+': {
            title: '+',
            category: 'Math',
            linkName: '-plus',
            returns: {
                type: 'number',
            },
            args: {
                xs: {
                    type: 'number',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['xs'] },
            ],
            description: 'Computes sum of $xs.',
            examples: ['(+)', '(+ 1)', '(+ 2 4)', '(+ 1 2 3 4)', '(+ (+ 2 3) (+ 5 6))'],
        },
        '-': {
            title: '-',
            category: 'Math',
            linkName: '-minus',
            returns: {
                type: 'number',
            },
            args: {
                xs: {
                    type: 'number',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['xs'] },
            ],
            description: 'Computes difference between first value and sum of the rest. When called with only one argument, it does negation.',
            examples: ['(-)', '(- 1)', '(- 2 4)', '(- 4 3 2 1)'],
        },
        '*': {
            title: '*',
            category: 'Math',
            linkName: '-star',
            returns: {
                type: 'number',
            },
            args: {
                xs: {
                    type: 'number',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['xs'] },
            ],
            description: 'Computes product of $xs.',
            examples: ['(*)', '(* 2)', '(* 2 4)', '(* 1 2 3 4)'],
        },
        '/': {
            title: '/',
            category: 'Math',
            linkName: '-slash',
            clojureDocs: '_fs',
            returns: {
                type: 'number',
            },
            args: {
                xs: {
                    type: 'number',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['xs'] },
            ],
            description: 'Computes division or reciprocal. When called with one argument it computes reciprocal. When called with two or more arguments it does compute division of the first by the all remaining $xs.',
            examples: ['(/)', '(/ 2)', '(/ 2 4)', '(/ 4 3 2 1)'],
        },
        'mod': {
            title: 'mod',
            category: 'Math',
            linkName: 'mod',
            returns: {
                type: 'number',
            },
            args: {
                a: {
                    type: 'number',
                },
                b: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Modulus of `dividend` and `divisor`. Truncates toward negative infinity.',
            examples: ['(mod 5 3)', '(mod 5.2 3.1)', '(mod -5 3)', '(mod 5 -3)', '(mod -5 -3)'],
        },
        '%': {
            title: '%',
            category: 'Math',
            linkName: '-percent',
            clojureDocs: 'rem',
            returns: {
                type: 'number',
            },
            args: {
                a: {
                    type: 'number',
                },
                b: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Remainder of dividing `dividend` and `divisor`.',
            examples: ['(% 5 3)', '(% 5.2 3.1)', '(% -5 3)', '(% 5 -3)', '(% -5 -3)'],
        },
        'quot': {
            title: 'quot',
            category: 'Math',
            linkName: 'quot',
            returns: {
                type: 'number',
            },
            args: {
                a: {
                    type: 'number',
                },
                b: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Quotient of dividing `dividend` and `divisor`.',
            examples: ['(quot 5 3)', '(quot 5.2 3.1)', '(quot -5 3)', '(quot 5 -3)', '(quot -5 -3)'],
        },
        'inc': {
            title: 'inc',
            category: 'Math',
            linkName: 'inc',
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Adds one to $x.',
            examples: ['(inc 0)', '(inc 1)', '(inc 100.1)'],
        },
        'dec': {
            title: 'dec',
            category: 'Math',
            linkName: 'dec',
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Subtracts one from $x.',
            examples: ['(dec 0)', '(dec 1)', '(dec 100.1)'],
        },
        'sqrt': {
            title: 'sqrt',
            category: 'Math',
            linkName: 'sqrt',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Computes square root of $x.',
            examples: ['(sqrt 0)', '(sqrt 9)', '(sqrt 2)'],
        },
        'cbrt': {
            title: 'cbrt',
            category: 'Math',
            linkName: 'cbrt',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Computes cube root of $x.',
            examples: ['(cbrt 0)', '(cbrt 27)', '(cbrt 2)', '(cbrt 1)'],
        },
        '**': {
            title: '**',
            category: 'Math',
            linkName: '-star-star',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                a: {
                    type: 'number',
                },
                b: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Computes returns $a raised to the power of $b.',
            examples: ['(** 2 3)', '(** 2 0)', '(** 2 -3)', '(** -2 3)', '(** -2 -3)'],
        },
        'exp': {
            title: 'exp',
            category: 'Math',
            linkName: 'exp',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Computes `e` rasied to the power of $x.',
            examples: ['(exp 3)', '(exp 0)', '(exp -3)', '(exp 3)'],
        },
        'round': {
            title: 'round',
            category: 'Math',
            linkName: 'round',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
                decimals: {
                    type: 'integer',
                },
            },
            variants: [
                { argumentNames: ['x'] },
                { argumentNames: ['x', 'decimals'] },
            ],
            description: 'Returns rounded $x. If $decimals is provided it return a number with that many decimals.',
            examples: [
                '(round 2)',
                '(round 2.49)',
                '(round 2.5)',
                '(round -2.49)',
                '(round -2.5)',
                '(round -2.501)',
                '(round 1.23456789 4)',
            ],
        },
        'trunc': {
            title: 'trunc',
            category: 'Math',
            linkName: 'trunc',
            clojureDocs: null,
            returns: {
                type: 'integer',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the integer part of $x by removing any fractional digits.',
            examples: ['(trunc 2)', '(trunc 2.49)', '(trunc 2.5)', '(trunc -2.49)', '(trunc -2.5)', '(trunc -2.501)'],
        },
        'floor': {
            title: 'floor',
            category: 'Math',
            linkName: 'floor',
            clojureDocs: null,
            returns: {
                type: 'integer',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the largest `integer` less than or equal to $x.',
            examples: ['(floor 2)', '(floor 2.49)', '(floor 2.5)', '(floor -2.49)', '(floor -2.5)', '(floor -2.501)'],
        },
        'ceil': {
            title: 'ceil',
            category: 'Math',
            linkName: 'ceil',
            clojureDocs: null,
            returns: {
                type: 'integer',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the smallest `integer` larger than or equal to $x.',
            examples: ['(ceil 2)', '(ceil 2.49)', '(ceil 2.5)', '(ceil -2.49)', '(ceil -2.5)', '(ceil -2.501)'],
        },
        'min': {
            title: 'min',
            category: 'Math',
            linkName: 'min',
            returns: {
                type: 'number',
            },
            args: {
                xs: {
                    type: 'number',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['xs'] },
            ],
            description: 'Returns the smallest number of the arguments.',
            examples: ['(min 2 0 1)', '(min 2 -1 1)', '(min 2.5)'],
        },
        'max': {
            title: 'max',
            category: 'Math',
            linkName: 'max',
            returns: {
                type: 'number',
            },
            args: {
                xs: {
                    type: 'number',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['xs'] },
            ],
            description: 'Returns the largest number of the arguments.',
            examples: ['(max 2 0 1)', '(max 2 -1 1)', '(max 2.5)'],
        },
        'abs': {
            title: 'abs',
            category: 'Math',
            linkName: 'abs',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the absolute value of $x.',
            examples: ['(abs -2.3)', '(abs 0)', '(abs 2.5)'],
        },
        'sign': {
            title: 'sign',
            category: 'Math',
            linkName: 'sign',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `1` if $x `> 0`, `-1` if $x `< 0`, `0` if $x `= 0` or `-0` if $x `= -0`.',
            examples: ['(sign -2.3)', '(sign -0)', '(sign 0)', '(sign 12312)'],
        },
        'positive_infinity': {
            title: 'positive_infinity',
            category: 'Math',
            linkName: 'positive_infinity',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {},
            variants: [
                { argumentNames: [] },
            ],
            description: 'Returns a number representing positive positive_infinity.',
            examples: ['(positive_infinity)'],
        },
        'negative_infinity': {
            title: 'negative_infinity',
            category: 'Math',
            linkName: 'negative_infinity',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {},
            variants: [
                { argumentNames: [] },
            ],
            description: 'Returns a number representing negative infinity.',
            examples: ['(negative_infinity)'],
        },
        'max_safe_integer': {
            title: 'max_safe_integer',
            category: 'Math',
            linkName: 'max_safe_integer',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {},
            variants: [
                { argumentNames: [] },
            ],
            description: 'Returns a number representing the maximum safe integer.',
            examples: ['(max_safe_integer)'],
        },
        'min_safe_integer': {
            title: 'min_safe_integer',
            category: 'Math',
            linkName: 'min_safe_integer',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {},
            variants: [
                { argumentNames: [] },
            ],
            description: 'Returns a number representing the minimum safe integer.',
            examples: ['(min_safe_integer)'],
        },
        'max_value': {
            title: 'max_value',
            category: 'Math',
            linkName: 'max_value',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {},
            variants: [
                { argumentNames: [] },
            ],
            description: 'Returns a number representing the maximum numeric value.',
            examples: ['(max_value)'],
        },
        'min_value': {
            title: 'min_value',
            category: 'Math',
            linkName: 'min_value',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {},
            variants: [
                { argumentNames: [] },
            ],
            description: 'Returns a number representing the smallest positive numeric value.',
            examples: ['(min_value)'],
        },
        'epsilon': {
            title: 'epsilon',
            category: 'Math',
            linkName: 'epsilon',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {},
            variants: [
                { argumentNames: [] },
            ],
            description: 'Returns a number representing the difference between 1 and the smallest floating point number greater than 1.',
            examples: ['(epsilon)'],
        },
        'nan': {
            title: 'nan',
            category: 'Math',
            linkName: 'nan',
            returns: {
                type: 'number',
            },
            args: {},
            variants: [
                { argumentNames: [] },
            ],
            description: 'Returns a number representing Not-A-Number.',
            examples: ['(nan)'],
        },
        'e': {
            title: 'e',
            category: 'Math',
            linkName: 'e',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {},
            variants: [
                { argumentNames: [] },
            ],
            description: 'Returns Euler\'s number, the base of natural logarithms, e.',
            examples: ['(e)'],
        },
        'pi': {
            title: 'pi',
            category: 'Math',
            linkName: 'pi',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {},
            variants: [
                { argumentNames: [] },
            ],
            description: 'Returns Pi, the ratio of the circumference of a circle to its diameter.',
            examples: ['(pi)'],
        },
        'log': {
            title: 'log',
            category: 'Math',
            linkName: 'log',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the natural logarithm (base e) of $x.',
            examples: ['(log 0.01)', '(log (exp 12))', '(log 2.5)'],
        },
        'log2': {
            title: 'log2',
            category: 'Math',
            linkName: 'log2',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the base `2` logarithm of a number.',
            examples: ['(log2 0.01)', '(log2 (** 2 12))', '(log2 2.5)'],
        },
        'log10': {
            title: 'log10',
            category: 'Math',
            linkName: 'log10',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the `10` logarithm of a number.',
            examples: ['(log10 0.01)', '(log10 (** 10 12))', '(log10 2.5)'],
        },
        'sin': {
            title: 'sin',
            category: 'Math',
            linkName: 'sin',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the sine of $x. $x must be specified in radians.',
            examples: ['(sin 0)', '(sin 1)', '(sin (pi))', '(sin -0.5)'],
        },
        'cos': {
            title: 'cos',
            category: 'Math',
            linkName: 'cos',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the cosine of $x. $x must be specified in radians.',
            examples: ['(cos 0)', '(cos 1)', '(cos (pi))', '(cos -0.5)'],
        },
        'tan': {
            title: 'tan',
            category: 'Math',
            linkName: 'tan',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the tangent of $x. $x must be specified in radians.',
            examples: ['(tan 0)', '(tan 1)', '(tan (pi))', '(tan -0.5)'],
        },
        'asin': {
            title: 'asin',
            category: 'Math',
            linkName: 'asin',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the arcsine (in radians) of $x.',
            examples: ['(asin 0)', '(asin 1)', '(asin -0.5)'],
        },
        'acos': {
            title: 'acos',
            category: 'Math',
            linkName: 'acos',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the arccosine (in radians) of $x.',
            examples: ['(acos 0)', '(acos 1)', '(acos -0.5)'],
        },
        'atan': {
            title: 'atan',
            category: 'Math',
            linkName: 'atan',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the arctangent (in radians) of $x.',
            examples: ['(atan 0)', '(atan 1)', '(atan -0.5)'],
        },
        'sinh': {
            title: 'sinh',
            category: 'Math',
            linkName: 'sinh',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the hyperbolic sine of $x.',
            examples: ['(sinh 0)', '(sinh 1)', '(sinh -0.5)'],
        },
        'cosh': {
            title: 'cosh',
            category: 'Math',
            linkName: 'cosh',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the hyperbolic cosine of $x.',
            examples: ['(cosh 0)', '(cosh 1)', '(cosh -0.5)'],
        },
        'tanh': {
            title: 'tanh',
            category: 'Math',
            linkName: 'tanh',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the hyperbolic tangent of $x.',
            examples: ['(tanh 0)', '(tanh 1)', '(tanh -0.5)', '(tanh 50)'],
        },
        'asinh': {
            title: 'asinh',
            category: 'Math',
            linkName: 'asinh',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the hyperbolic arcsine of $x.',
            examples: ['(asinh 0)', '(asinh 0.9)', '(asinh -0.5)'],
        },
        'acosh': {
            title: 'acosh',
            category: 'Math',
            linkName: 'acosh',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the hyperbolic arccosine of $x.',
            examples: ['(acosh 1)', '(acosh 2)', '(acosh 100)'],
        },
        'atanh': {
            title: 'atanh',
            category: 'Math',
            linkName: 'atanh',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns the hyperbolic arctangent of $x.',
            examples: ['(atanh 0)', '(atanh 0.9)', '(atanh -0.5)'],
        },
    };

    var miscReference = {
        '!=': {
            title: '!=',
            category: 'Misc',
            clojureDocs: 'not=',
            linkName: '-exclamation-equal',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
                ys: {
                    type: 'any',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['x'] },
                { argumentNames: ['x', 'ys'] },
            ],
            description: 'Result is `true` if no two `values` are equal to each other, otherwise result is `false`. Note that only two argument version result is negation of `=` function, that is `(!= a b)` is same as `(! (== a b))`.',
            examples: ['(!= 3)', '(!= 3 2)', '(!= :3 3)', '(!= 3 3 2)', '(!= :3 :2 :1 :0)', '(!= 0 -0)'],
        },
        '==': {
            title: '==',
            category: 'Misc',
            linkName: '-equal-equal',
            clojureDocs: '=',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
                ys: {
                    type: 'any',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['x'] },
                { argumentNames: ['x', 'ys'] },
            ],
            description: 'Compares `values` according to \'equal\' predicate. Result is `true` if every specified value is equal to each other, otherwise result is `false`.',
            examples: ['(== 1 1)', '(== 1.01 1)', '(== :1 1)', '(== :2 :2 :2 :2)', '(== 2 2 1 2)'],
        },
        '<': {
            title: '<',
            category: 'Misc',
            linkName: '-lt',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'number',
                },
                ys: {
                    type: 'number',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['x'] },
                { argumentNames: ['x', 'ys'] },
            ],
            description: 'Returns `true` if the number $x and $ys are in increasing order, `false` otherwise.',
            examples: ['(< 0 1)', '(< 1 1.01)', '(< 1 1)', '(< 1 2 2 3)', '(< :a :b)', '(< [9] [1 2])'],
        },
        '>': {
            title: '>',
            category: 'Misc',
            linkName: '-gt',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'number',
                },
                ys: {
                    type: 'number',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['x'] },
                { argumentNames: ['x', 'ys'] },
            ],
            description: 'Returns `true` if the number $x and $ys are in decreasing order, `false` otherwise.',
            examples: ['(> 1 0)', '(> 1.01 1)', '(> 1 1)', '(> 4 3 2 1)', '(> 3 2 2 1)'],
        },
        '<=': {
            title: '<=',
            category: 'Misc',
            linkName: '-lte',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'number',
                },
                ys: {
                    type: 'number',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['x'] },
                { argumentNames: ['x', 'ys'] },
            ],
            description: 'Returns `true` if the number $x and $ys are in non decreasing order, `false` otherwise.',
            examples: ['(<= 0 1)', '(<= 1 1.01)', '(<= 1 1)', '(<= 1 2 3 4)', '(<= 1 2 2 3)'],
        },
        '>=': {
            title: '>=',
            category: 'Misc',
            linkName: '-gte',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'number',
                },
                ys: {
                    type: 'number',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['x'] },
                { argumentNames: ['x', 'ys'] },
            ],
            description: 'Returns `true` if the number $x and $ys are in non increasing order, `false` otherwise.',
            examples: ['(>= 1 0)', '(>= 1.01 1)', '(>= 1 1)', '(>= 4 3 2 1)', '(>= 3 2 2 1)'],
        },
        '!': {
            title: '!',
            category: 'Misc',
            linkName: '-exclamation',
            clojureDocs: 'not',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Computes logical negation. Note that any other $x than `false`, `0`, `nil` and `\'\'` is truthy.',
            examples: ['(! 3)', '(! true)', '(! "A string")', '(! 0)', '(! false)', '(! nil)', '(! "")'],
        },
        'write!': {
            title: 'write!',
            category: 'Misc',
            linkName: 'write-exclamation',
            clojureDocs: null,
            returns: {
                type: 'any',
            },
            args: {
                values: {
                    type: 'any',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['values'] },
            ],
            description: 'It logs the $values and then returns the last argument. If called with no arguments `nil` is returned.',
            examples: [
                '(write! "A string")',
                '(write! 100 "items")',
                '(write! (object :a 10))',
                '(write! [:a :b :c])',
                '(write! #"^start")',
                '(write! nil true false)',
            ],
        },
        'iso_date>epoch': {
            title: 'iso_date>epoch',
            category: 'Misc',
            linkName: 'iso_date-gtepoch',
            returns: {
                type: 'number',
            },
            args: {
                iso: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['iso'] },
            ],
            description: 'Returns milliseconds elapsed since the UNIX epoch to `iso`.',
            examples: [
                '(iso_date>epoch "2022-04-12T09:37:10.899Z")',
                '(iso_date>epoch "1980-01-01")',
            ],
        },
        'epoch>iso_date': {
            title: 'epoch>iso_date',
            category: 'Misc',
            linkName: 'epoch-gtiso_date',
            returns: {
                type: 'string',
            },
            args: {
                ms: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['ms'] },
            ],
            description: 'Returns IOS date time string from `ms` (milliseconds elapsed since the UNIX epoch).',
            examples: [
                '(epoch>iso_date 1649756230899)',
                '(epoch>iso_date 0)',
            ],
        },
        'boolean': {
            title: 'boolean',
            category: 'Misc',
            linkName: 'boolean',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Coerces $x to boolean.',
            examples: [
                '(boolean 0)',
                '(boolean 1)',
                '(boolean nil)',
                '(boolean "Albert")',
            ],
        },
        'compare': {
            title: 'compare',
            category: 'Misc',
            linkName: 'compare',
            returns: {
                type: 'number',
            },
            args: {
                a: {
                    type: 'any',
                },
                b: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Compares two values. Returns `-1` if $a < $b, `1` if $a > $b and `0` if $a and $b have the same sort order.',
            examples: [
                '(compare 0 1)',
                '(compare "Albert" "Mojir")',
                '(compare 1 :1)',
                '(compare [1 2 3] [2 3])',
                '(compare [1 2 3] [2 3 4])',
                '(compare {:a 1 :b 2} {:a 1})',
                '(compare {:a 1} [2 3])',
                '(compare + -)',
            ],
        },
        'equal?': {
            title: 'equal?',
            category: 'Misc',
            linkName: 'equal-question',
            clojureDocs: null,
            returns: {
                type: 'boolean',
            },
            args: {
                a: {
                    type: 'any',
                },
                b: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Returns true if $a and $b are structually equal.',
            examples: [
                '(equal? {:a 10 :b 20} {:b 20 :a 10})',
                '(equal? [1 true nil] [1 true nil])',
                '(equal? {:a 10 :b [1 2 {:b 20}]} {:b [1 2 {:b 20}] :a 10})',
                '(equal? {:a 10 :b [1 2 {:b 20}]} {:b [1 2 {:b 21}] :a 10})',
                '(== 0.3 (+ 0.1 0.2))',
                '(equal? 0.3 (+ 0.1 0.2))',
            ],
        },
        'json_parse': {
            title: 'json_parse',
            category: 'Misc',
            linkName: 'json_parse',
            clojureDocs: null,
            returns: {
                type: 'any',
            },
            args: {
                x: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `JSON.parse(`$x`)`.',
            examples: [
                '(json_parse "[1, 2, 3]")',
            ],
        },
        'json_stringify': {
            title: 'json_stringify',
            category: 'Misc',
            linkName: 'json_stringify',
            clojureDocs: null,
            returns: {
                type: 'string',
            },
            args: {
                x: {
                    type: 'any',
                },
                indent: {
                    type: 'integer',
                    description: 'Number of spaces to use for indentation.',
                },
            },
            variants: [
                { argumentNames: ['x'] },
                { argumentNames: ['x', 'indent'] },
            ],
            description: 'Returns `JSON.stringify(`$x`)`. If second argument is provided, returns `JSON.stringify(`$x`, null, `$indent`)`.',
            examples: [
                '(json_stringify [1, 2, 3])',
                '(json_stringify {:a {:b 10}} 2)',
            ],
        },
    };

    var assertReference = { 'assert': {
            title: 'assert',
            category: 'Assert',
            linkName: 'assert',
            returns: {
                type: 'any',
            },
            args: {
                value: {
                    type: 'any',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['value'] },
                { argumentNames: ['value', 'message'] },
            ],
            description: 'If $value is falsy it throws `AssertionError` with $message. If no $message is provided, message is set to $value.',
            examples: ['(assert 0 "Expected a positive value")'],
        }, 'assert=': {
            title: 'assert=',
            category: 'Assert',
            linkName: 'assert-equal',
            clojureDocs: null,
            returns: {
                type: 'null',
            },
            args: {
                a: {
                    type: 'any',
                },
                b: {
                    type: 'any',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'message'] },
            ],
            description: 'If $a is not the same as $b it throws `AssertionError`.',
            examples: [
                '(assert= 0 1 "Expected same values")',
                '(assert= 0 1)',
                '(assert= 1 1)',
            ],
        }, 'assert!=': {
            title: 'assert!=',
            category: 'Assert',
            linkName: 'assert-exclamation-equal',
            clojureDocs: null,
            returns: {
                type: 'null',
            },
            args: {
                a: {
                    type: 'any',
                },
                b: {
                    type: 'any',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'message'] },
            ],
            description: 'If $a is the same as $b it throws `AssertionError`.',
            examples: [
                '(assert!= 0 0 "Expected different values")',
                '(assert!= 0 0)',
                '(assert!= 0 1)',
            ],
        }, 'assert_equal': {
            title: 'assert_equal',
            category: 'Assert',
            linkName: 'assert_equal',
            clojureDocs: null,
            returns: {
                type: 'null',
            },
            args: {
                a: {
                    type: 'any',
                },
                b: {
                    type: 'any',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'message'] },
            ],
            description: 'If $a is not deep equal to $b it throws `AssertionError`.',
            examples: [
                '(assert_equal { :a 1 } { :a 2 } "Expected equal values")',
                '(assert_equal { :a 1 } { :a 2 })',
                '(assert_equal { :a 1 } { :a 1 })',
            ],
        }, 'assert_not_equal': {
            title: 'assert_not_equal',
            category: 'Assert',
            linkName: 'assert_not_equal',
            clojureDocs: null,
            returns: {
                type: 'null',
            },
            args: {
                a: {
                    type: 'any',
                },
                b: {
                    type: 'any',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'message'] },
            ],
            description: 'If $a is not deep equal to $b it throws `AssertionError`.',
            examples: [
                '(assert_not_equal { :a 2 } { :a 2 } "Expected different values")',
                '(assert_not_equal { :a 2 } { :a 2 })',
                '(assert_not_equal { :a 1 } { :a 2 })',
            ],
        }, 'assert_gt': {
            title: 'assert_gt',
            category: 'Assert',
            linkName: 'assert_gt',
            clojureDocs: null,
            returns: {
                type: 'null',
            },
            args: {
                a: {
                    type: 'any',
                },
                b: {
                    type: 'any',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'message'] },
            ],
            description: 'If $a is not greater than $b it throws `AssertionError`.',
            examples: [
                '(assert_gt 0 1 "Expected greater value")',
                '(assert_gt 0 0)',
                '(assert_gt 1 0)',
            ],
        }, 'assert_lt': {
            title: 'assert_lt',
            category: 'Assert',
            linkName: 'assert_lt',
            clojureDocs: null,
            returns: {
                type: 'null',
            },
            args: {
                a: {
                    type: 'any',
                },
                b: {
                    type: 'any',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'message'] },
            ],
            description: 'If $a is not less than $b it throws `AssertionError`.',
            examples: [
                '(assert_lte 1 0 "Expected smaller value value")',
                '(assert_lte 1 1)',
                '(assert_lte 0 1)',
            ],
        }, 'assert_gte': {
            title: 'assert_gte',
            category: 'Assert',
            linkName: 'assert_gte',
            clojureDocs: null,
            returns: {
                type: 'null',
            },
            args: {
                a: {
                    type: 'any',
                },
                b: {
                    type: 'any',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'message'] },
            ],
            description: 'If $a is less than $b it throws `AssertionError`.',
            examples: [
                '(assert_gte 0 1 "Expected greater value")',
                '(assert_gte 0 1)',
                '(assert_gte 1 1)',
            ],
        }, 'assert_lte': {
            title: 'assert_lte',
            category: 'Assert',
            linkName: 'assert_lte',
            clojureDocs: null,
            returns: {
                type: 'null',
            },
            args: {
                a: {
                    type: 'any',
                },
                b: {
                    type: 'any',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'message'] },
            ],
            description: 'If $a is grater than $b it throws `AssertionError`.',
            examples: [
                '(assert_lte 1 0 "Expected smaller value value")',
                '(assert_lte 1 0)',
                '(assert_lte 1 1)',
            ],
        }, 'assert_true': {
            title: 'assert_true',
            category: 'Assert',
            linkName: 'assert_true',
            clojureDocs: null,
            returns: {
                type: 'null',
            },
            args: {
                value: {
                    type: 'any',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['value'] },
                { argumentNames: ['value', 'message'] },
            ],
            description: 'If $value is not `true` it throws `AssertionError`.',
            examples: [
                '(assert_true false "Expected true")',
                '(assert_true false)',
                '(assert_true true)',
            ],
        }, 'assert_false': {
            title: 'assert_false',
            category: 'Assert',
            linkName: 'assert_false',
            clojureDocs: null,
            returns: {
                type: 'null',
            },
            args: {
                value: {
                    type: 'any',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['value'] },
                { argumentNames: ['value', 'message'] },
            ],
            description: 'If $value is not `false` it throws `AssertionError`.',
            examples: [
                '(assert_false true "Expected false")',
                '(assert_false true)',
                '(assert_false false)',
            ],
        }, 'assert_truthy': {
            title: 'assert_truthy',
            category: 'Assert',
            linkName: 'assert_truthy',
            clojureDocs: null,
            returns: {
                type: 'null',
            },
            args: {
                value: {
                    type: 'any',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['value'] },
                { argumentNames: ['value', 'message'] },
            ],
            description: 'If $value is not `truthy` it throws `AssertionError`.',
            examples: [
                '(assert_truthy false "Expected truthy")',
                '(assert_truthy false)',
                '(assert_truthy 0)',
                '(assert_truthy nil)',
                '(assert_truthy "")',
                '(assert_truthy true)',
                '(assert_truthy 1)',
                '(assert_truthy :x)',
                '(assert_truthy [])',
                '(assert_truthy {})',
            ],
        }, 'assert_falsy': {
            title: 'assert_falsy',
            category: 'Assert',
            linkName: 'assert_falsy',
            clojureDocs: null,
            returns: {
                type: 'null',
            },
            args: {
                value: {
                    type: 'any',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['value'] },
                { argumentNames: ['value', 'message'] },
            ],
            description: 'If $value is not `falsy` it throws `AssertionError`.',
            examples: [
                '(assert_falsy true "Expected falsy")',
                '(assert_falsy :x)',
                '(assert_falsy [])',
                '(assert_falsy {})',
                '(assert_falsy 1)',
                '(assert_falsy false)',
                '(assert_falsy 0)',
                '(assert_falsy nil)',
                '(assert_falsy "")',
            ],
        }, 'assert_null': {
            title: 'assert_null',
            category: 'Assert',
            linkName: 'assert_null',
            clojureDocs: null,
            returns: {
                type: 'null',
            },
            args: {
                value: {
                    type: 'any',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['value'] },
                { argumentNames: ['value', 'message'] },
            ],
            description: 'If $value is not `nil` it throws `AssertionError`.',
            examples: [
                '(assert_null nil)',
                '(assert_null true "Expected nil")',
                '(assert_null :x)',
                '(assert_null [])',
                '(assert_null {})',
                '(assert_null 1)',
                '(assert_null false)',
                '(assert_null 0)',
                '(assert_null "")',
            ],
        }, 'assert_throws': {
            title: 'assert_throws',
            category: 'Assert',
            linkName: 'assert_throws',
            clojureDocs: null,
            returns: {
                type: 'null',
            },
            args: {
                fn: {
                    type: 'function',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['fn'] },
                { argumentNames: ['fn', 'message'] },
            ],
            description: 'If $fn does not throw, it throws `AssertionError`.',
            examples: ['(assert_throws #(throw "Error"))', '(assert_throws #(identity "Error"))'],
        }, 'assert_throws_error': {
            title: 'assert_throws_error',
            category: 'Assert',
            linkName: 'assert_throws_error',
            clojureDocs: null,
            returns: {
                type: 'null',
            },
            args: {
                'fn': {
                    type: 'function',
                },
                'error-message': {
                    type: 'string',
                },
                'message': {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['value', 'error-message'] },
                { argumentNames: ['value', 'error-message', 'message'] },
            ],
            description: 'If $fn does not throw $error-message, it throws `AssertionError`.',
            examples: [
                '(assert_throws_error #(throw :Error) :Error)',
                '(assert_throws_error #(throw "Something else") :Error "Hej alla barn")',
                '(assert_throws_error #(identity :Error) :Error)',
            ],
        }, 'assert_not_throws': {
            title: 'assert_not_throws',
            category: 'Assert',
            linkName: 'assert_not_throws',
            clojureDocs: null,
            returns: {
                type: 'null',
            },
            args: {
                fn: {
                    type: 'function',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['fn'] },
                { argumentNames: ['fn', 'message'] },
            ],
            description: 'If $fn throws, it throws `AssertionError`.',
            examples: ['(assert_not_throws #(identity "Error"))', '(assert_not_throws #(throw "Error"))'],
        } };

    var objectReference = {
        dissoc: {
            title: 'dissoc',
            category: 'Object',
            linkName: 'dissoc',
            returns: {
                type: 'object',
            },
            args: {
                obj: {
                    type: 'object',
                },
                key: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['obj', 'key'] },
            ],
            description: 'Return shallow copy of $obj with $key deleted.',
            examples: [
                '(dissoc (object :x 10 :y 20) :x)',
                '(dissoc { :x 10 } :y)',
                "\n(def o { :a 5 }) (dissoc o :a)\no",
            ],
        },
        object: {
            title: 'object',
            category: 'Object',
            linkName: 'object',
            clojureDocs: null,
            returns: {
                type: 'object',
            },
            args: {
                kvps: {
                    type: 'any',
                    rest: true,
                    description: 'key - value pairs, where key is a string',
                },
            },
            variants: [
                { argumentNames: ['kvps'] },
            ],
            description: 'Constructs a new object. Object members are created from the $kvps key-value pairs. Requires an even number of arguments.',
            examples: [
                '(object)',
                '(object :x 10 :y true :z "A string")',
                '{}',
                '{:a 1 :b 2}',
            ],
        },
        keys: {
            title: 'keys',
            category: 'Object',
            linkName: 'keys',
            returns: {
                type: 'any',
                array: true,
            },
            args: {
                obj: {
                    type: 'object',
                },
            },
            variants: [
                { argumentNames: ['obj'] },
            ],
            description: 'Returns array of all keys in $obj.',
            examples: [
                '(keys (object))',
                '(keys (object :x 10 :y true :z "A string"))',
            ],
        },
        vals: {
            title: 'vals',
            category: 'Object',
            linkName: 'vals',
            returns: {
                type: 'any',
                array: true,
            },
            args: {
                obj: {
                    type: 'object',
                },
            },
            variants: [
                { argumentNames: ['obj'] },
            ],
            description: 'Returns array of all values in $obj.',
            examples: [
                '(vals (object))',
                '(vals (object :x 10 :y true :z "A string"))',
            ],
        },
        entries: {
            title: 'entries',
            category: 'Object',
            linkName: 'entries',
            clojureDocs: null,
            returns: {
                type: 'array',
            },
            args: {
                obj: {
                    type: 'object',
                },
            },
            variants: [
                { argumentNames: ['obj'] },
            ],
            description: 'Returns nested array of all key - value pairs in $obj.',
            examples: [
                '(entries (object))',
                '(entries (object :x 10 :y true :z "A string"))',
            ],
        },
        find: {
            title: 'find',
            category: 'Object',
            linkName: 'find',
            returns: {
                type: ['array', 'null'],
            },
            args: {
                obj: {
                    type: 'object',
                },
                key: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['obj', 'key'] },
            ],
            description: 'Returns entry (key-value pair) for $key, or `nil` if $key not present in $obj.',
            examples: [
                '(find (object :a 1 :b 2) :b)',
                '(find (object :a 1 :b 2) :c)',
            ],
        },
        merge: {
            title: 'merge',
            category: 'Object',
            linkName: 'merge',
            returns: {
                type: 'object',
            },
            args: {
                objs: {
                    type: 'object',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['objs'] },
            ],
            description: "Returns a new object created by merging together all arguments.\n\nIf two keys appears in more than one object the value from the last object is used.  \nIf no arguments are provided `nil` is returned.",
            examples: [
                '(merge (object :x 10) (object :y 20))',
                '(merge (object :x 10) (object :x 15 :y 20))',
            ],
        },
        merge_with: {
            title: 'merge_with',
            category: 'Object',
            linkName: 'merge_with',
            returns: {
                type: 'object',
            },
            args: {
                objs: {
                    type: 'object',
                    rest: true,
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['objs', 'fn'] },
            ],
            description: "\nReturns a new object created by merging together all arguments.\nIf two keys appears in more than one object $fn is used to calculate the new value.\n\nIf no arguments are provided `nil` is returned.",
            examples: [
                '(merge_with (object :x 10) (object :y 20) +)',
                '(merge_with (object :x 10) (object :x 15 :y 20) +)',
                '(merge_with (object :x 10) (object :x 20) (object :x 30) (object :x 40) -)',
            ],
        },
        zipmap: {
            title: 'zipmap',
            category: 'Object',
            linkName: 'zipmap',
            returns: {
                type: 'object',
            },
            args: {
                keys: {
                    type: 'string',
                    array: true,
                },
                values: {
                    type: 'any',
                    array: true,
                },
            },
            variants: [
                { argumentNames: ['keys', 'values'] },
            ],
            description: 'Returns a new object created by mapping $keys to $values.',
            examples: [
                '(zipmap [:a :b :c] [10 nil [1 2 3]])',
                '(zipmap [:a :b :c] [1])',
                '(zipmap [] [10 nil [1 2 3]])',
            ],
        },
        select_keys: {
            title: 'select_keys',
            category: 'Object',
            linkName: 'select_keys',
            returns: {
                type: 'object',
            },
            args: {
                obj: {
                    type: 'object',
                },
                keys: {
                    type: 'string',
                    array: true,
                },
            },
            variants: [
                { argumentNames: ['obj', 'keys'] },
            ],
            description: 'Returns an object containing only those entries in $obj whose key is in $keys.',
            examples: [
                '(select_keys {:a 1 :b 2 :c 3} [:a :b])',
                '(select_keys {:a 1} [:a :b])',
            ],
        },
    };

    var predicateReference = {
        'boolean?': {
            title: 'boolean?',
            category: 'Predicate',
            linkName: 'boolean-question',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is a `boolean`, otherwise `false`.',
            examples: [
                '(boolean? true)',
                '(boolean? false)',
                '(boolean? [1 2 3])',
                '(boolean? 0)',
                '(boolean? "A string")',
            ],
        },
        'nil?': {
            title: 'nil?',
            category: 'Predicate',
            linkName: 'nil-question',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is `nil`, otherwise `false`.',
            examples: [
                '(nil? nil)',
                '(nil? false)',
                '(nil? [1 2 3])',
                '(nil? 0)',
                '(nil? "A string")',
            ],
        },
        'number?': {
            title: 'number?',
            category: 'Predicate',
            linkName: 'number-question',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is a number, otherwise `false`.',
            examples: [
                '(number? 0)',
                '(number? 2)',
                '(number? -0.12)',
                '(number? false)',
                '(number? [1 2 3])',
                '(number? "A string")',
            ],
        },
        'string?': {
            title: 'string?',
            category: 'Predicate',
            linkName: 'string-question',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is a string, otherwise `false`.',
            examples: [
                '(string? "")',
                '(string? "A string")',
                '(string? (if true "A string" false))',
                '(string? false)',
                '(string? [1 2 3])',
                '(string? 100)',
            ],
        },
        'function?': {
            title: 'function?',
            category: 'Predicate',
            linkName: 'function-question',
            clojureDocs: null,
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is a function, otherwise `false`.',
            examples: [
                '(function? +)',
                '(function? /)',
                '(function? (fn [x y] (+ x y)))',
                '(function? false)',
                '(function? "false")',
                '(function? [1 2 3])',
            ],
        },
        'integer?': {
            title: 'integer?',
            category: 'Predicate',
            linkName: 'integer-question',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is an integer, otherwise `false`.',
            examples: [
                '(integer? 0)',
                '(integer? -12)',
                '(integer? 42)',
                '(integer? 10.1)',
                '(integer? (fn [x y] (+ x y)))',
                '(integer? false)',
                '(integer? "false")',
                '(integer? [1 2 3])',
            ],
        },
        'array?': {
            title: 'array?',
            category: 'Predicate',
            linkName: 'array-question',
            clojureDocs: 'vector_q',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is an array, otherwise `false`.',
            examples: [
                '(array? [])',
                '(array? [1 2 3])',
                '(array? (object :a 10))',
                '(array? 42)',
                '(array? 10.1)',
                '(array? (fn [x y] (+ x y)))',
            ],
        },
        'object?': {
            title: 'object?',
            category: 'Predicate',
            linkName: 'object-question',
            clojureDocs: null,
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is an object, otherwise `false`.',
            examples: [
                '(object? (object :a 10))',
                '(object? (object))',
                '(object? 42)',
                '(object? 10.1)',
                '(object? (fn [x y] (+ x y)))',
                '(object? #"^start")',
                '(object? "false")',
                '(object? [1 2 3])',
            ],
        },
        'coll?': {
            title: 'coll?',
            category: 'Predicate',
            linkName: 'coll-question',
            clojureDocs: null,
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is a Coll i.e. an array, an object or a string, otherwise `false`.',
            examples: [
                '(coll? [])',
                '(coll? [1 2 3])',
                '(coll? (object :a 10))',
                '(coll? "Albert")',
                '(coll? 42)',
                '(coll? 10.1)',
                '(coll? (fn [x y] (+ x y)))',
            ],
        },
        'seq?': {
            title: 'seq?',
            category: 'Predicate',
            linkName: 'seq-question',
            clojureDocs: null,
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is a Seq i.e. an array or a string, otherwise `false`.',
            examples: [
                '(seq? [])',
                '(seq? [1 2 3])',
                '(seq? (object :a 10))',
                '(seq? "Albert")',
                '(seq? 42)',
                '(seq? 10.1)',
                '(seq? (fn [x y] (+ x y)))',
            ],
        },
        'regexp?': {
            title: 'regexp?',
            category: 'Predicate',
            linkName: 'regexp-question',
            clojureDocs: null,
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is a regexp, otherwise `false`.',
            examples: [
                '(regexp? (regexp "^start"))',
                '(regexp? #"^start")',
                '(regexp? -12)',
                '(regexp? (object))',
                '(regexp? 10.1)',
                '(regexp? (fn [x y] (+ x y)))',
                '(regexp? false)',
                '(regexp? "false")',
                '(regexp? [1 2 3])',
            ],
        },
        'zero?': {
            title: 'zero?',
            category: 'Predicate',
            linkName: 'zero-question',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is `0`, otherwise `false`.',
            examples: [
                '(zero? 0)',
                '(zero? -0.0)',
                '(zero? 1)',
                '(zero? 0.1)',
            ],
        },
        'pos?': {
            title: 'pos?',
            category: 'Predicate',
            linkName: 'pos-question',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is greater than `0`, otherwise `false`.',
            examples: [
                '(pos? 0)',
                '(pos? -0.0)',
                '(pos? 1)',
                '(pos? -0.1)',
            ],
        },
        'neg?': {
            title: 'neg?',
            category: 'Predicate',
            linkName: 'neg-question',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is less than `0`, otherwise `false`.',
            examples: [
                '(neg? 0)',
                '(neg? -0.0)',
                '(neg? 1)',
                '(neg? -0.1)',
            ],
        },
        'even?': {
            title: 'even?',
            category: 'Predicate',
            linkName: 'even-question',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is even, otherwise `false`.',
            examples: [
                '(even? 0)',
                '(even? -0.0)',
                '(even? -1)',
                '(even? 2.1)',
            ],
        },
        'odd?': {
            title: 'odd?',
            category: 'Predicate',
            linkName: 'odd-question',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is odd, otherwise `false`.',
            examples: [
                '(odd? 1.0)',
                '(odd? 1.001)',
                '(odd? -1)',
                '(odd? 2.1)',
            ],
        },
        'finite?': {
            title: 'finite?',
            category: 'Predicate',
            linkName: 'finite-question',
            clojureDocs: null,
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is finite, otherwise `false`.',
            examples: [
                '(finite? 1.0)',
                '(finite? (/ 1 0))',
                '(finite? (/ -1 0))',
                '(finite? (sqrt -1))',
            ],
        },
        'nan?': {
            title: 'nan?',
            category: 'Predicate',
            linkName: 'nan-question',
            clojureDocs: null,
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is NaN (! a number), otherwise `false`.',
            examples: [
                '(nan? 1.0)',
                '(nan? (/ 1 0))',
                '(nan? (/ -1 0))',
                '(nan? (sqrt -1))',
            ],
        },
        'negative_infinity?': {
            title: 'negative_infinity?',
            category: 'Predicate',
            linkName: 'negative_infinity-question',
            clojureDocs: null,
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x equals negative infinity, otherwise `false`.',
            examples: [
                '(negative_infinity? 1.0)',
                '(negative_infinity? (/ 1 0))',
                '(negative_infinity? (/ -1 0))',
                '(negative_infinity? (sqrt -1))',
            ],
        },
        'positive_infinity?': {
            title: 'positive_infinity?',
            category: 'Predicate',
            linkName: 'positive_infinity-question',
            clojureDocs: null,
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x equals positive infinity, otherwise `false`.',
            examples: [
                '(positive_infinity? 1.0)',
                '(positive_infinity? (/ 1 0))',
                '(positive_infinity? (/ -1 0))',
                '(positive_infinity? (sqrt -1))',
            ],
        },
        'false?': {
            title: 'false?',
            category: 'Predicate',
            linkName: 'false-question',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is `true`, otherwise `false`.',
            examples: [
                '(false? false)',
                '(false? true)',
                '(false? 1)',
                '(false? 0)',
            ],
        },
        'true?': {
            title: 'true?',
            category: 'Predicate',
            linkName: 'true-question',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is `true`, otherwise `false`.',
            examples: [
                '(true? false)',
                '(true? true)',
                '(true? 1)',
                '(true? 0)',
            ],
        },
        'empty?': {
            title: 'empty?',
            category: 'Predicate',
            linkName: 'empty-question',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: ['collection', 'string', 'null'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `true` if $x is empty or `nil`, otherwise `false`.',
            examples: [
                '(empty? [])',
                '(empty? [1 2 3])',
                '(empty? {})',
                '(empty? {:a 2})',
                '(empty? "")',
                '(empty? "Albert")',
                '(empty? nil)',
            ],
        },
        'not_empty?': {
            title: 'not_empty?',
            category: 'Predicate',
            linkName: 'not_empty-question',
            clojureDocs: null,
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: ['collection', 'string', 'null'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns `false` if $x is empty or `nil`, otherwise `true`.',
            examples: [
                '(not_empty? [])',
                '(not_empty? [1 2 3])',
                '(not_empty? {})',
                '(not_empty? {:a 2})',
                '(not_empty? "")',
                '(not_empty? "Albert")',
                '(not_empty? nil)',
            ],
        },
    };

    var regularExpressionReference = {
        regexp: {
            title: 'regexp',
            category: 'Regular expression',
            linkName: 'regexp',
            clojureDocs: null,
            returns: {
                type: 'regexp',
            },
            args: {
                pattern: {
                    type: 'string',
                },
                flags: {
                    type: 'string',
                    description: 'Optional flags for the regular expression. Possible values are the same as Javascript RegExp takes.',
                },
            },
            variants: [
                { argumentNames: ['pattern'] },
                { argumentNames: ['pattern', 'flags'] },
            ],
            description: 'Creates a RegExp from $pattern and $flags.',
            examples: [
                '(regexp "^\\s*(.*)$")',
                '#"^\\s*(.*)$"',
                '(regexp "albert" :i)',
                '#"albert"ig',
            ],
        },
        match: {
            title: 'match',
            category: 'Regular expression',
            linkName: 'match',
            clojureDocs: null,
            returns: {
                type: 'any',
                array: true,
            },
            args: {
                r: {
                    type: 'regexp',
                },
                s: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['r', 's'] },
            ],
            description: "Matches $s against regular expression $r.\nIf $s is a string and matches the regular expression, a `match`-array is returned, otherwise `nil` is returned.",
            examples: [
                '(match (regexp "^\\s*(.*)$") "  A string")',
                '(match #"albert"i "My name is Albert")',
                '(match #"albert"i "My name is Ben")',
                '(match #"albert"i nil)',
                '(match #"albert"i 1)',
                '(match #"albert"i {})',
            ],
        },
        replace: {
            title: 'replace',
            category: 'Regular expression',
            linkName: 'replace',
            clojureDocs: null,
            returns: {
                type: 'any',
                array: true,
            },
            args: {
                s: {
                    type: 'string',
                },
                r: {
                    type: 'regexp',
                },
                x: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['s', 'r', 'x'] },
            ],
            description: 'Returns a new string with some or all matches of regular expression $r replaced by $x.',
            examples: [
                '(replace "Duck" (regexp :u) :i)',
                '(replace "abcABC" (regexp :a "gi") "-")',
                '(replace "abcABC" #"a"gi "-")',
            ],
        },
    };

    var specialExpressionsReference = {
        '&&': {
            title: '&&',
            category: 'Special expression',
            linkName: '-and-and',
            returns: {
                type: 'boolean',
            },
            args: {
                expressions: {
                    type: '*expression',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['expressions'] },
            ],
            description: "\nComputes logical `and`. Evaluation of $expressions starts from left.\nAs soon as a `expression` evaluates to a falsy value, the result is returned.\n\nIf all expressions evaluate to truthy values, the value of the last expression is returned.",
            examples: [
                '(&& 1 1)',
                '(&& (> 3 2) "string")',
                '(&& (< 3 2) "string")',
                '(&& true true true true)',
                '(&& true true 0 true)',
            ],
        },
        '||': {
            title: '||',
            category: 'Special expression',
            linkName: '-or-or',
            clojureDocs: 'or',
            returns: {
                type: 'boolean',
            },
            args: {
                expressions: {
                    type: '*expression',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['expressions'] },
            ],
            description: "\nComputes logical `or`. Evaluation of $expressions evaluation starts from left.\nAs soon as a `expression` evaluates to a truthy value, the result is returned.\n\nIf all expressions evaluate to falsy values, the value of the last expression is returned.",
            examples: [
                '(|| 1 1)',
                '(|| (> 3 2) "string")',
                '(|| (< 3 2) "string")',
                '(|| true true true true)',
                '(|| 1 2 3 4)',
            ],
        },
        'def': {
            title: 'def',
            category: 'Special expression',
            linkName: 'def',
            returns: {
                type: 'any',
            },
            args: {
                n: {
                    type: '*name',
                },
                value: {
                    type: '*expression',
                },
            },
            variants: [
                { argumentNames: ['n', 'value'] },
            ],
            description: "Bind $value to variable $n.\n\nIf $n is already defined, an error is thrown.",
            examples: [
                '(def a (object))',
                '(def a (object :x 10 :y true :z "A string"))',
            ],
        },
        'defs': {
            title: 'defs',
            category: 'Special expression',
            linkName: 'defs',
            clojureDocs: null,
            returns: {
                type: 'any',
            },
            args: {
                name: {
                    type: '*expression',
                },
                value: {
                    type: '*expression',
                },
            },
            variants: [
                { argumentNames: ['name', 'value'] },
            ],
            description: "\nCreates a variable with name set to $name evaluated and value set to $value.\n\nIf a variable with name $name is already defined, an error is thrown.",
            examples: [
                '(defs :a :b)',
                "\n(defs (str :a :1) (object :x 10 :y true :z \"A string\"))\na1",
                "\n(defs :a :b)\n(defs a :c)\nb",
            ],
        },
        'let': {
            title: 'let',
            category: 'Special expression',
            linkName: 'let',
            returns: {
                type: 'any',
            },
            args: {
                bindings: {
                    type: '*binding',
                    rest: true,
                },
                expressions: {
                    type: '*expression',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['bindings', 'expressions'] },
            ],
            description: "\nBinds local variables. The variables lives only within $expressions.\nIt returns evaluation of the last expression in $expressions.",
            examples: ["\n  (let [a (+ 1 2 3 4) \n        b (* 1 2 3 4)]\n    (write! a b))"],
        },
        'if_let': {
            title: 'if_let',
            category: 'Special expression',
            linkName: 'if_let',
            returns: {
                type: 'any',
            },
            args: {
                'binding': {
                    type: '*binding',
                },
                'then-expr': {
                    type: '*expression',
                },
                'else-expr': {
                    type: '*expression',
                },
            },
            variants: [
                { argumentNames: ['binding', 'then-expr'] },
                { argumentNames: ['binding', 'then-expr', 'else-expr'] },
            ],
            description: "\nBinds one local variable. If it evaluates to a truthy value\n$then-expr is executed with the variable accessable.\nIf the bound variable evaluates to false, the $else-expr is evaluated\n(without variable accessable).",
            examples: [
                "\n(if_let [a (> (count \"Albert\") 4)]\n  (write! (str a \", is big enough\"))\n  (write! \"Sorry, not big enough.\"))",
                "\n(if_let [a (> (count \"Albert\") 10)]\n  (write! (str a \", is big enough\"))\n  (write! \"Sorry, not big enough.\"))",
            ],
        },
        'when_let': {
            title: 'when_let',
            category: 'Special expression',
            linkName: 'when_let',
            returns: {
                type: 'any',
            },
            args: {
                binding: {
                    type: '*binding',
                },
                expressions: {
                    type: '*expression',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['binding', 'expressions'] },
            ],
            description: "\nBinds one local variable. If it evaluates to a truthy value\n$expressions is executed with the variable accessable.\nIf the bound variable evaluates to a falsy value, `nil` is returned.",
            examples: [
                "\n(when_let [a (> (count \"Albert\") 4)]\n  (write! a))",
            ],
        },
        'when_first': {
            title: 'when_first',
            category: 'Special expression',
            linkName: 'when_first',
            returns: {
                type: 'any',
            },
            args: {
                binding: {
                    type: '*binding',
                    rest: true,
                },
                expressions: {
                    type: '*expression',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['binding', 'expressions'] },
            ],
            description: 'When the binding value in $binding is a non empty sequence, the first element of that sequence (instead of the sequence itself) is bound to the variable.',
            examples: [
                "\n(when_first [x [1 2 3]]\n  (write! x)\n  x)",
                "\n(when_first [x \"Albert\"]\n  (write! x)\n  x)",
                "\n(when_first [x [0]]\n  (write! x)\n  x)",
                "\n(when_first [x [nil]]\n  (write! x)\n  x)",
                "\n(when_first [x []]\n  (write! x)\n  x)",
            ],
        },
        'fn': {
            title: 'fn',
            category: 'Special expression',
            linkName: 'fn',
            returns: {
                type: 'function',
            },
            args: {
                args: {
                    type: '*arguments',
                },
                expressions: {
                    type: '*expression',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['args', 'expressions'] },
            ],
            description: 'Creates a function. When called, evaluation of the last expression in the body is returned.',
            examples: [
                "\n(fn [a b]\n  (sqrt\n    (+\n      (* a a)\n      (* b b))))",
                "\n(\n  (fn [a b]\n    (sqrt\n      (+\n        (* a a)\n        (* b b))))\n  3\n  4)",
            ],
        },
        'defn': {
            title: 'defn',
            category: 'Special expression',
            linkName: 'defn',
            returns: {
                type: 'function',
            },
            args: {
                n: {
                    type: '*name',
                },
                args: {
                    type: '*arguments',
                },
                expressions: {
                    type: '*expression',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['n', 'args', 'expressions'] },
            ],
            description: 'Creates a named global function. When called, evaluation of the last expression in the body is returned.',
            examples: [
                "\n(defn hyp [a b]\n  (sqrt\n    (+\n      (* a a)\n      (* b b))))\nhyp",
                "\n(defn hyp [a b]\n  (sqrt\n    (+\n      (* a a)\n      (* b b))))\n(hyp 3 4)",
                "\n(defn sumOfSquares [& s]\n  (apply\n    +\n    (map\n      (fn [x] (* x x))\n      s)))\n(sumOfSquares 1 2 3 4 5)",
            ],
        },
        'defns': {
            title: 'defns',
            category: 'Special expression',
            linkName: 'defns',
            clojureDocs: null,
            returns: {
                type: 'function',
            },
            args: {
                name: {
                    type: '*expression',
                },
                args: {
                    type: '*arguments',
                },
                expressions: {
                    type: '*expression',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['name', 'args', 'expressions'] },
            ],
            description: 'Creates a named global function with its name set to $name evaluated. When called, evaluation of the last expression in the body is returned.',
            examples: [
                "\n(defns \"hyp\" [a b]\n  (sqrt\n    (+\n      (* a a)\n      (* b b))))\nhyp",
                "\n(defns\n  (str :h :y :p)\n  [a b]\n  (sqrt\n    (+\n      (* a a)\n      (* b b))))\n(hyp 3 4)",
                "\n(defns \"sumOfSquares\" [& s]\n  (apply\n    +\n    (map\n      (fn [x] (* x x))\n      s)))\n(sumOfSquares 1 2 3 4 5)",
            ],
        },
        'try': {
            title: 'try',
            category: 'Special expression',
            linkName: 'try',
            clojureDocs: null,
            returns: {
                type: 'any',
            },
            args: {
                exp: {
                    type: '*expression',
                },
                catch: {
                    type: '*catch-expression',
                },
            },
            variants: [
                { argumentNames: ['exp', 'catch'] },
            ],
            description: 'Executes $exp. If that throws, the $catch `body` gets executed. See examples for details.',
            examples: [
                "\n(try\n  (/ 2 4)\n  (catch error \"Oops!\"))",
                "\n(try\n  (foo)\n  (catch error \"Oops!\"))",
                "\n(try\n  (foo)\n  (catch error error))",
            ],
        },
        'throw': {
            title: 'throw',
            category: 'Special expression',
            linkName: 'throw',
            clojureDocs: null,
            returns: {
                type: 'never',
            },
            args: {
                expr: {
                    type: '*expression',
                },
            },
            variants: [
                { argumentNames: ['expr'] },
            ],
            description: 'Throws `UserDefinedError` with message set to $expr evaluated. $expr must evaluate to a string.',
            examples: [
                '(throw "You shall not pass!")',
                '(throw (subs "You shall not pass!" 0 3))',
            ],
        },
        'if': {
            title: 'if',
            category: 'Special expression',
            linkName: 'if',
            returns: {
                type: 'any',
            },
            args: {
                'test': {
                    type: 'any',
                },
                'then-expr': {
                    type: 'any',
                },
                'else-expr': {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['test', 'then-expr'] },
                { argumentNames: ['test', 'then-expr', 'else-expr'] },
            ],
            description: 'Either $then-expr or $else-expr branch is taken. $then-expr is selected when $test is truthy. If $test is falsy $else-expr is executed, if no $else-expr exists, `nil` is returned.',
            examples: [
                '(if true (write! "TRUE") (write! "FALSE"))',
                '(if false (write! "TRUE") (write! "FALSE"))',
                '(if true (write! "TRUE"))',
                '(if false (write! "TRUE"))',
            ],
        },
        'if_not': {
            title: 'if_not',
            category: 'Special expression',
            linkName: 'if_not',
            returns: {
                type: 'any',
            },
            args: {
                'test': {
                    type: 'any',
                },
                'then-expr': {
                    type: 'any',
                },
                'else-expr': {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['test', 'then-expr'] },
                { argumentNames: ['test', 'then-expr', 'else-expr'] },
            ],
            description: 'Either $then-expr or $else-expr branch is taken. $then-expr is selected when $test is falsy. If $test is truthy $else-expr is executed, if no $else-expr exists, `nil` is returned.',
            examples: [
                '(if_not true (write! "TRUE") (write! "FALSE"))',
                '(if_not false (write! "TRUE") (write! "FALSE"))',
                '(if_not true (write! "TRUE"))',
                '(if_not false (write! "TRUE"))',
            ],
        },
        'cond': {
            title: 'cond',
            category: 'Special expression',
            linkName: 'cond',
            returns: {
                type: 'any',
            },
            args: {
                conds: {
                    type: '*conditions',
                },
            },
            variants: [
                { argumentNames: ['conds'] },
            ],
            description: 'Used for branching. $conds are tested sequentially from the top. If no branch is tested truthy, `nil` is returned.',
            examples: [
                "\n(cond\n  false (write! \"FALSE\")\n  nil (write! \"nil\")\n  :else (write! \"TRUE\"))",
                "\n(cond\n  false (write! \"FALSE\")\n  nil (write! \"nil\")\n  true (write! \"TRUE\"))",
                "\n(cond\n  false (write! \"FALSE\")\n  nil (write! \"nil\"))",
            ],
        },
        'when': {
            title: 'when',
            category: 'Special expression',
            linkName: 'when',
            returns: {
                type: 'any',
            },
            args: {
                test: {
                    type: '*expression',
                },
                expressions: {
                    type: '*expression',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['test', 'expressions'] },
            ],
            description: "If $test yields a thruthy value, the expressions are evaluated\nand the value returned by the last expression is returned.\nOtherwise, if $test yields a falsy value, the expressions are not evaluated,\nand `nil` is returned. If no $expressions are provided, `nil` is returned.",
            examples: [
                "(when true\n      (write! \"Hi\")\n      (write! \"There\"))",
                "(when false\n      (write! \"Hi\")\n      (write! \"There\"))",
                '(when true)',
                '(when false)',
            ],
        },
        'when_not': {
            title: 'when_not',
            category: 'Special expression',
            linkName: 'when_not',
            returns: {
                type: 'any',
            },
            args: {
                test: {
                    type: '*expression',
                },
                expressions: {
                    type: '*expression',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['test', 'expressions'] },
            ],
            description: "If $test yields a falsy value, the expressions are evaluated\nand the value returned by the last `expression` is returned.\nOtherwise, if $test yields a truthy value, the $expressions are not evaluated,\nand `nil` is returned. If no `expression` is provided, `nil` is returned.",
            examples: [
                '(when_not true (write! "Hi") (write! "There"))',
                '(when_not false (write! "Hi") (write! "There"))',
                '(when_not true)',
                '(when_not false)',
            ],
        },
        'comment': {
            title: 'comment',
            category: 'Special expression',
            linkName: 'comment',
            returns: {
                type: 'null',
            },
            args: {
                expressions: {
                    type: '*expression',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['expressions'] },
            ],
            description: 'All $expressions are read and must be valid `lits` but they are not eveluated. `nil` is returned.',
            examples: ['(comment (write! "Hi") (write! "Albert"))', '(comment)'],
        },
        'do': {
            title: 'do',
            category: 'Special expression',
            linkName: 'do',
            returns: {
                type: 'any',
            },
            args: {
                expressions: {
                    type: '*expression',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['expressions'] },
            ],
            description: 'Evaluates $expressions. Resulting value is the value of the last expression.',
            examples: [
                "\n(do \n  (write! \"Hi\")\n  (write! \"Albert\"))",
                '(do)',
            ],
        },
        'recur': {
            title: 'recur',
            category: 'Special expression',
            linkName: 'recur',
            returns: {
                type: 'null',
            },
            args: {
                expressions: {
                    type: '*expression',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['expressions'] },
            ],
            description: 'Recursevly calls enclosing function or loop with its evaluated $expressions.',
            examples: [
                "\n(defn foo [n]\n  (write! n)\n  (when (! (zero? n))\n    (recur\n      (dec n))))\n(foo 3)",
                "\n(\n  (fn [n]\n    (write! n)\n    (when (! (zero? n))\n      (recur\n        (dec n))))\n  3)",
                "\n(\n  loop [n 3]\n    (write! n)\n    (when\n      (! (zero? n))\n      (recur (dec n))))",
            ],
        },
        'loop': {
            title: 'loop',
            category: 'Special expression',
            linkName: 'loop',
            returns: {
                type: 'any',
            },
            args: {
                bindings: {
                    type: '*binding',
                    rest: true,
                },
                expressions: {
                    type: '*expression',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['bindings', 'expressions'] },
            ],
            description: 'Executes $expressions with initial $bindings. The $bindings will be replaced with the recur parameters for subsequent recursions.',
            examples: [
                "\n(loop [n 3]\n  (write! n)\n  (when\n    (! (zero? n))\n    (recur (dec n))))",
                "\n(loop [n 3]\n  (write! n)\n  (if\n    (! (zero? n))\n    (recur (dec n))\n    n))",
            ],
        },
        'doseq': {
            title: 'doseq',
            category: 'Special expression',
            linkName: 'doseq',
            returns: {
                type: 'null',
            },
            args: {
                bindings: {
                    type: '*for-binding',
                    rest: true,
                },
                expr: {
                    type: '*expression',
                },
            },
            variants: [
                { argumentNames: ['vars', 'expr'] },
            ],
            description: 'Same syntax as `for`, but returns `nil`. Use for side effects. Consumes less memory than `for`.',
            examples: ['(doseq [x [1 2 4]] (write! x))'],
        },
        'for': {
            title: 'for',
            category: 'Special expression',
            linkName: 'for',
            returns: {
                type: 'any',
                array: true,
            },
            args: {
                bindings: {
                    type: '*for-binding',
                    rest: true,
                },
                expr: {
                    type: '*expression',
                },
            },
            variants: [
                { argumentNames: ['vars', 'expr'] },
            ],
            description: "List comprehension. Takes one or more $bindings, each followed by zero or more modifiers, and returns an array of evaluations of $expr.\n  \n  Collections are iterated in a nested fashion, rightmost fastest. Supported modifiers are: &let &while and &when.",
            examples: [
                "\n(for [x \"Al\" y [1 2]]\n  (repeat y x))",
                "\n(for [x {:a 10 :b 20} y [1 2]]\n  (repeat y x))",
                "\n(for [x [1 2] y [1 10]]\n  (* x y))",
                "\n(for\n  [x [1 2]\n  &let [z (* x x x)]]\n  \n  z)",
                "\n(for\n  [x [0 1 2 3 4 5]\n  &let [y (* x 3)]\n  &when (even? y)]\n  \n  y)",
                "\n(for\n  [x [0 1 2 3 4 5]\n  &let [y (* x 3)]\n  &while (even? y)]\n  \n  y)",
                "\n(for\n  [x [0 1 2 3 4 5]\n  &let [y (* x 3)]\n  &while (odd? y)]\n  \n  y)",
                "\n(for\n  [x [1 2 3] y [1 2 3]\n  &while (<= x y)\n  z [1 2 3]]\n  \n  [x y z])",
                "\n(for\n  [x [1 2 3] y [1 2 3] z [1 2 3]\n  &while (<= x y)]\n  \n  [x y z])",
            ],
        },
        'declared?': {
            title: 'declared?',
            category: 'Special expression',
            linkName: 'declared-question',
            returns: {
                type: 'boolean',
            },
            args: {
                n: {
                    type: '*name',
                },
            },
            variants: [
                { argumentNames: ['n'] },
            ],
            description: 'Returns `true` if $n is a declared variable or a builtin function, otherwise `false`.',
            examples: [
                '(declared? foo)',
                "\n(def foo :foo)\n(declared? foo)",
                '(declared? +)',
                "\n(def foo nil)\n(declared? foo)",
                '(declared? if)',
            ],
        },
        '??': {
            title: '??',
            category: 'Special expression',
            linkName: '-question-question',
            returns: {
                type: 'any',
            },
            args: {
                test: {
                    type: '*expression',
                },
                default: {
                    type: '*expression',
                },
            },
            variants: [
                { argumentNames: ['test'] },
                { argumentNames: ['test', 'default'] },
            ],
            description: 'If $test is declared and evaluated to non `nil` value $test is result, else $default is returned. If $default is not provided, `nil` is returned.',
            examples: [
                '(?? foo)',
                "\n(def foo :foo)\n(?? foo)",
                '(?? +)',
                "\n(def foo nil)\n(?? foo)",
                "\n(def foo nil)\n(?? foo :bar)",
                '(?? foo 1)',
                '(?? "")',
                '(?? 0)',
                '(?? 0 1)',
                '(?? 2 1)',
            ],
        },
    };

    var stringReference = {
        'subs': {
            title: 'subs',
            category: 'String',
            linkName: 'subs',
            returns: {
                type: 'string',
            },
            args: {
                s: {
                    type: 'string',
                },
                start: {
                    type: 'integer',
                },
                end: {
                    type: 'integer',
                },
            },
            variants: [
                { argumentNames: ['s', 'start', 'end'] },
                { argumentNames: ['s', 'start'] },
            ],
            description: 'Extracts characters from $start up to but not including $end.',
            examples: [
                '(subs "A string" 2)',
                '(subs "A string" 2 5)',
                '(subs "A string" 2 100)',
                '(subs "A string" 100)',
            ],
        },
        'string_repeat': {
            title: 'string_repeat',
            category: 'String',
            linkName: 'string_repeat',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                s: {
                    type: 'string',
                },
                n: {
                    type: 'integer',
                },
            },
            variants: [
                { argumentNames: ['s', 'n'] },
            ],
            description: 'Repeates $s $n times.',
            examples: [
                '(string_repeat "*" 10)',
                '(string_repeat "***" 0)',
            ],
        },
        'str': {
            title: 'str',
            category: 'String',
            linkName: 'str',
            returns: {
                type: 'string',
            },
            args: {
                values: {
                    type: 'any',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['values'] },
            ],
            description: 'Concatenats $values into one string. If `value` equals `nil` empty string is returned.',
            examples: [
                '(str "A string" ", and another string" " ...and more")',
                '(str "Just one string")',
                '(str)',
                '(str 0 false true nil #"^kalle" [1 2 3] {:a :a})',
            ],
        },
        'number': {
            title: 'number',
            category: 'String',
            linkName: 'number',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                s: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['s'] },
            ],
            description: 'Parses $s to a number.',
            examples: [
                '(number "10")',
                '(number "010")',
                '(number "-1.01")',
            ],
        },
        'lower_case': {
            title: 'lower_case',
            category: 'String',
            linkName: 'lower_case',
            returns: {
                type: 'string',
            },
            args: {
                s: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['s'] },
            ],
            description: 'Returns $s converted to lower case.',
            examples: [
                '(lower_case "Albert")',
                '(lower_case "")',
            ],
        },
        'upper_case': {
            title: 'upper_case',
            category: 'String',
            linkName: 'upper_case',
            clojureDocs: null,
            returns: {
                type: 'string',
            },
            args: {
                s: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['s'] },
            ],
            description: 'Returns $s converted to upper case.',
            examples: [
                '(upper_case "Albert")',
                '(upper_case "")',
            ],
        },
        'trim': {
            title: 'trim',
            category: 'String',
            linkName: 'trim',
            clojureDocs: null,
            returns: {
                type: 'string',
            },
            args: {
                s: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['s'] },
            ],
            description: 'Returns a new string with leading and trailing whitespaces removed.',
            examples: [
                '(trim "  Albert  ")',
                '(trim "   ")',
                '(trim "")',
            ],
        },
        'trim_left': {
            title: 'trim_left',
            category: 'String',
            linkName: 'trim_left',
            clojureDocs: null,
            returns: {
                type: 'string',
            },
            args: {
                s: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['s'] },
            ],
            description: 'Returns a new string with leading whitespaces removed.',
            examples: [
                '(trim_left "  Albert  ")',
                '(trim_left "   ")',
                '(trim_left "")',
            ],
        },
        'trim_right': {
            title: 'trim_right',
            category: 'String',
            linkName: 'trim_right',
            clojureDocs: null,
            returns: {
                type: 'string',
            },
            args: {
                s: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['s'] },
            ],
            description: 'Returns a new string with trailing whitespaces removed.',
            examples: [
                '(trim_right "  Albert  ")',
                '(trim_right "   ")',
                '(trim_right "")',
            ],
        },
        'pad_left': {
            title: 'pad_left',
            category: 'String',
            linkName: 'pad_left',
            clojureDocs: null,
            returns: {
                type: 'string',
            },
            args: {
                s: {
                    type: 'string',
                },
                length: {
                    type: 'integer',
                },
                padString: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['s', 'length'] },
                { argumentNames: ['s', 'length', 'padString'] },
            ],
            description: 'Pads from the start of $s with `padString` (multiple times, if needed) until the resulting string reaches the given $length.',
            examples: [
                '(pad_left "Albert" 20)',
                '(pad_left "Albert" 20 "-*-")',
                '(pad_left "Albert" 5)',
                '(pad_left "Albert" -1)',
            ],
        },
        'pad_right': {
            title: 'pad_right',
            category: 'String',
            linkName: 'pad_right',
            clojureDocs: null,
            returns: {
                type: 'string',
            },
            args: {
                s: {
                    type: 'string',
                },
                length: {
                    type: 'integer',
                },
                padString: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['s', 'length'] },
                { argumentNames: ['s', 'length', 'padString'] },
            ],
            description: 'Pads from the start of $s with `padString` (multiple times, if needed) until the resulting string reaches the given `length`.',
            examples: [
                '(pad_right "Albert" 20)',
                '(pad_right "Albert" 20 "-*-")',
                '(pad_right "Albert" 5)',
                '(pad_right "Albert" -1)',
            ],
        },
        'split': {
            title: 'split',
            category: 'String',
            linkName: 'split',
            clojureDocs: null,
            returns: {
                type: 'string',
            },
            args: {
                s: {
                    type: 'string',
                },
                delimiter: {
                    type: 'string',
                },
                limit: {
                    type: 'integer',
                },
            },
            variants: [
                { argumentNames: ['s', 'delimiter'] },
                { argumentNames: ['s', 'delimiter', 'limit'] },
            ],
            description: 'Divides $s into an array of substrings. The division is done by searching for `delimiter`. If `limit` as provided, at most `limit` number of substrings are returned.',
            examples: [
                '(split "Albert Mojir" " ")',
                '(split "abcdefghijklmnopqrstuvw" (regexp "[aoueiy]"))',
                '(split "0123456789" "")',
                '(map number (split "0123456789" "" 5))',
            ],
        },
        'template': {
            title: 'template',
            category: 'String',
            linkName: 'template',
            clojureDocs: null,
            returns: {
                type: 'string',
            },
            args: {
                s: {
                    type: 'string',
                },
                params: {
                    type: 'any',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['s', 'params'] },
            ],
            description: 'Applies placeholders to a string. Support for basic pluralization - see examples. If pluralization is used, first placeholder must be a number.',
            examples: [
                '(template "Hi, $1 and $2" "Carl" "Larry")',
                '(template "Hi $1, $2, $3, $4, $5, $6, $7, $8 and $9" :A :B :C :D :E :F :G :H :I)',
                '(template "$1 book||||$1 books" 0)',
                '(template "$1 book||||$1 books" 1)',
                '(template "$1 book||||$1 books" 2)',
                '(template "No book||||$1 book||||$1 books" 0)',
                '(template "No book||||$1 book||||$1 books" 1)',
                '(template "No book||||$1 book||||$1 books" 10)',
                '(template "No book||||One book||||Two books||||Three books||||$1 books" 0)',
                '(template "No book||||One book||||Two books||||Three books||||$1 books" 1)',
                '(template "No book||||One book||||Two books||||Three books||||$1 books" 2)',
                '(template "No book||||One book||||Two books||||Three books||||$1 books" 3)',
                '(template "No book||||One book||||Two books||||Three books||||$1 books" 4)',
            ],
        },
        'to_char_code': {
            title: 'to_char_code',
            category: 'String',
            linkName: 'to_char_code',
            clojureDocs: null,
            returns: {
                type: 'number',
            },
            args: {
                c: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['c'] },
            ],
            description: 'Return code point for first character in $c.',
            examples: [
                '(to_char_code :A)',
                '(to_char_code "Albert")',
            ],
        },
        'from_char_code': {
            title: 'from_char_code',
            category: 'String',
            linkName: 'from_char_code',
            clojureDocs: null,
            returns: {
                type: 'string',
            },
            args: {
                code: {
                    type: 'number',
                },
            },
            variants: [
                { argumentNames: ['code'] },
            ],
            description: 'Return character for code point $code.',
            examples: [
                '(from_char_code 65)',
                '(from_char_code 0)',
            ],
        },
        'encode_base64': {
            title: 'encode_base64',
            category: 'String',
            linkName: 'encode_base64',
            clojureDocs: null,
            returns: {
                type: 'string',
            },
            args: {
                s: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['s'] },
            ],
            description: 'Returns a Base64 encoded string from $s.',
            examples: [
                '(encode_base64 "Albert")',
            ],
        },
        'decode_base64': {
            title: 'decode_base64',
            category: 'String',
            linkName: 'decode_base64',
            clojureDocs: null,
            returns: {
                type: 'string',
            },
            args: {
                base64string: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['base64string'] },
            ],
            description: 'Returns a Base64 decoded string from $base64string.',
            examples: [
                '(decode_base64 "QWxiZXJ0IPCfkLs=")',
            ],
        },
        'encode_uri_component': {
            title: 'encode_uri_component',
            category: 'String',
            linkName: 'encode_uri_component',
            clojureDocs: null,
            returns: {
                type: 'string',
            },
            args: {
                s: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['s'] },
            ],
            description: 'Returns an escaped `URI` string.',
            examples: [
                '(encode_uri_component "Hi everyone!?")',
            ],
        },
        'decode_uri_component': {
            title: 'decode_uri_component',
            category: 'String',
            linkName: 'decode_uri_component',
            clojureDocs: null,
            returns: {
                type: 'string',
            },
            args: {
                s: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['s'] },
            ],
            description: 'Returns an un-escaped `URI` string.',
            examples: [
                '(decode_uri_component "Hi%20everyone!%3F%20%F0%9F%91%8D")',
            ],
        },
        'join': {
            title: 'join',
            category: 'String',
            linkName: 'join',
            clojureDocs: null,
            returns: {
                type: 'string',
            },
            args: {
                arr: {
                    type: 'array',
                },
                delimiter: {
                    type: 'string',
                },
            },
            variants: [{
                    argumentNames: ['arr', 'delimiter'],
                }],
            description: 'Returns a new string by concatenating all of the elements in $arr, separated by $delimiter.',
            examples: [
                '(join ["Albert" "Mojir"] " ")',
                '(join (map [0 1 2 3 4 5 6 7 8 9] str) ", ")',
            ],
        },
        '++': {
            title: '++',
            category: 'String',
            linkName: '-plus-plus',
            clojureDocs: null,
            returns: {
                type: 'string',
            },
            args: {
                strings: {
                    type: ['string', 'number', 'null'],
                    rest: true,
                },
            },
            variants: [{
                    argumentNames: ['strings'],
                }],
            description: 'Concatenats $strings into one string.',
            examples: [
                '(++ "Albert" " " "Mojir")',
                '(++ "Albert" "Mojir")',
                '(++ "Albert" null "Mojir")',
                '(++ "Albert")',
                '(++)',
            ],
        },
    };

    var bitwiseReference = { '<<': {
            title: '<<',
            category: 'Bitwise',
            linkName: '-lt-lt',
            clojureDocs: 'bit-shift-left',
            returns: {
                type: 'integer',
            },
            args: {
                x: {
                    type: 'integer',
                },
                n: {
                    type: 'integer',
                },
            },
            variants: [
                { argumentNames: ['x', 'n'] },
            ],
            description: 'Shifts $x arithmetically left by $n bit positions.',
            examples: ['(<< 1 10)', '(<< -4 2)'],
        }, '>>': {
            title: '>>',
            category: 'Bitwise',
            linkName: '-gt-gt',
            clojureDocs: 'bit-shift-right',
            returns: {
                type: 'integer',
            },
            args: {
                x: {
                    type: 'integer',
                },
                n: {
                    type: 'integer',
                },
            },
            variants: [
                { argumentNames: ['x', 'n'] },
            ],
            description: 'Shifts $x arithmetically right by $n bit positions.',
            examples: ['(>> 2048 10)', '(>> 4 10)'],
        }, '>>>': {
            title: '>>>',
            category: 'Bitwise',
            linkName: '-gt-gt-gt',
            clojureDocs: 'unsigned-bit-shift-right',
            returns: {
                type: 'integer',
            },
            args: {
                x: {
                    type: 'integer',
                },
                n: {
                    type: 'integer',
                },
            },
            variants: [
                { argumentNames: ['x', 'n'] },
            ],
            description: 'Shifts $x arithmetically right by $n bit positions without sign extension.',
            examples: ['(>>> 2048 10)', '(>>> 4 10)', '(>>> -1 10)'],
        }, '~': {
            title: '~',
            category: 'Bitwise',
            linkName: '-tilde',
            clojureDocs: 'bit-not',
            returns: {
                type: 'integer',
            },
            args: {
                x: {
                    type: 'integer',
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'Returns bitwise `not` of $x.',
            examples: ['(~ 0)', '(~ 255)'],
        }, '&': {
            title: '&',
            category: 'Bitwise',
            linkName: '-and',
            clojureDocs: 'bit-and',
            returns: {
                type: 'integer',
            },
            args: {
                x: {
                    type: 'integer',
                },
                y: {
                    type: 'integer',
                },
                rest: {
                    type: 'integer',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['x', 'y'] },
                { argumentNames: ['x', 'y', 'rest'] },
            ],
            description: 'Returns bitwise `and` of all arguments.',
            examples: [
                '(& 0b0011 0b0110)',
                '(& 0b0011 0b0110 0b1001)',
            ],
        }, '&!': {
            title: '&!',
            category: 'Bitwise',
            linkName: '-and-exclamation',
            clojureDocs: 'bit-and-not',
            returns: {
                type: 'integer',
            },
            args: {
                x: {
                    type: 'integer',
                },
                y: {
                    type: 'integer',
                },
                rest: {
                    type: 'integer',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['x', 'y'] },
                { argumentNames: ['x', 'y', 'rest'] },
            ],
            description: 'Returns bitwise `and` with complement.',
            examples: ['(&! 0b0011 0b0110)', '(&! 0b0011 0b0110 0b1001)'],
        }, '|': {
            title: '|',
            category: 'Bitwise',
            linkName: '-or',
            clojureDocs: 'bit-or',
            returns: {
                type: 'integer',
            },
            args: {
                x: {
                    type: 'integer',
                },
                y: {
                    type: 'integer',
                },
                rest: {
                    type: 'integer',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['x', 'y'] },
                { argumentNames: ['x', 'y', 'rest'] },
            ],
            description: 'Returns bitwise `or` of all arguments.',
            examples: ['(| 0b0011 0b0110)', '(| 0b1000 0b0100 0b0010)'],
        }, '^': {
            title: '^',
            category: 'Bitwise',
            linkName: '-caret',
            clojureDocs: 'bit-xor',
            returns: {
                type: 'integer',
            },
            args: {
                x: {
                    type: 'integer',
                },
                y: {
                    type: 'integer',
                },
                rest: {
                    type: 'integer',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['x', 'y'] },
                { argumentNames: ['x', 'y', 'rest'] },
            ],
            description: 'Returns bitwise `xor` of all arguments.',
            examples: ['(^ 0b0011 0b0110)', '(^ 0b11110000 0b00111100 0b10101010)'],
        }, 'bit_flip': {
            title: 'bit_flip',
            category: 'Bitwise',
            linkName: 'bit_flip',
            returns: {
                type: 'integer',
            },
            args: {
                x: {
                    type: 'integer',
                },
                n: {
                    type: 'integer',
                },
            },
            variants: [
                { argumentNames: ['x', 'n'] },
            ],
            description: 'Flips bit number $n.',
            examples: ['(bit_flip 0b0011 1)', '(bit_flip 0b1100 1)'],
        }, 'bit_clear': {
            title: 'bit_clear',
            category: 'Bitwise',
            linkName: 'bit_clear',
            returns: {
                type: 'integer',
            },
            args: {
                x: {
                    type: 'integer',
                },
                n: {
                    type: 'integer',
                },
            },
            variants: [
                { argumentNames: ['x', 'n'] },
            ],
            description: 'Clears bit number $n.',
            examples: ['(bit_clear 0b0011 1)', '(bit_clear 0b1100 1)'],
        }, 'bit_set': {
            title: 'bit_set',
            category: 'Bitwise',
            linkName: 'bit_set',
            returns: {
                type: 'integer',
            },
            args: {
                x: {
                    type: 'integer',
                },
                n: {
                    type: 'integer',
                },
            },
            variants: [
                { argumentNames: ['x', 'n'] },
            ],
            description: 'Sets bit number $n.',
            examples: ['(bit_set 0b0011 1)', '(bit_set 0b1100 1)'],
        }, 'bit_test': {
            title: 'bit_test',
            category: 'Bitwise',
            linkName: 'bit_test',
            returns: {
                type: 'boolean',
            },
            args: {
                x: {
                    type: 'integer',
                },
                n: {
                    type: 'integer',
                },
            },
            variants: [
                { argumentNames: ['x', 'n'] },
            ],
            description: 'Checks if bit number $n is set.',
            examples: ['(bit_test 0b0011 1)', '(bit_test 0b1100 1)'],
        } };

    var shorthand = {
        '-short-regexp': {
            shorthand: true,
            title: '#"pattern"',
            category: 'Shorthand',
            linkName: '-short-regexp',
            clojureDocs: null,
            description: 'Shorthand for ``(regexp pattern)``',
            examples: [
                '#"^\\s*(.*)$"',
                '#"albert"ig',
            ],
            seeAlso: ['regexp'],
        },
        '-short-fn': {
            shorthand: true,
            title: '#(expression)',
            category: 'Shorthand',
            linkName: '-short-fn',
            clojureDocs: null,
            description: "\nShorthand for ``(fn [args] (expression))``.\n`%1, %2, %3, ...` are shorthand for the first, second, third, ... argument.\n\nYou can reference the first argument using either `%1` or `%`.\nHowever, please note that `%1` and `%` are mutually exclusive and cannot be used simultaneously.\nE.g. ``#(* % %1)`` is not valid.",
            examples: [
                '#(+ %1 %2)',
                '#(* % %)',
            ],
            seeAlso: ['fn'],
        },
        '-short-string': {
            shorthand: true,
            title: ':abc',
            category: 'Shorthand',
            linkName: '-short-string',
            clojureDocs: null,
            description: 'Shorthand for ``"abc"``. The string can only contain `name` characters',
            examples: [
                ':abc',
                ':a-b',
            ],
        },
        '-short-dot': {
            shorthand: true,
            title: 'foo.bar',
            category: 'Shorthand',
            linkName: '-short-dot',
            clojureDocs: null,
            description: 'Shorthand for ``(foo "bar")``.',
            examples: [
                "\n(def foo {:bar {:baz 42}})\nfoo.bar.baz",
            ],
        },
        '-short-hash': {
            shorthand: true,
            title: 'foo#3',
            category: 'Shorthand',
            linkName: '-short-hash',
            clojureDocs: null,
            description: 'Shorthand for ``(foo 3)``.',
            examples: [
                "\n(def foo {:bar [1 2 3]})\nfoo.bar#2",
            ],
        },
    };

    var datatype = {
        '-type-number': {
            datatype: true,
            clojureDocs: null,
            title: 'number',
            category: 'Datatype',
            linkName: '-type-number',
            description: 'A number',
            examples: [
                '42',
                '3.14',
            ],
        },
        '-type-string': {
            datatype: true,
            clojureDocs: null,
            title: 'string',
            category: 'Datatype',
            linkName: '-type-string',
            description: 'A string',
            examples: [
                '"hello"',
                '""',
            ],
        },
        '-type-object': {
            datatype: true,
            clojureDocs: null,
            title: 'object',
            category: 'Datatype',
            linkName: '-type-object',
            description: 'An object, a collection of key-value pairs where keys are strings',
            examples: [
                '{}',
                '{:a 1 :b 2}',
            ],
        },
        '-type-array': {
            datatype: true,
            clojureDocs: null,
            title: 'array',
            category: 'Datatype',
            linkName: '-type-array',
            description: 'An array, a collection of values',
            examples: [
                '[]',
                '[1 2 3]',
                '[:a nil true]',
            ],
        },
        '-type-boolean': {
            datatype: true,
            clojureDocs: null,
            title: 'boolean',
            category: 'Datatype',
            linkName: '-type-boolean',
            description: 'A boolean',
            examples: [
                'true',
                'false',
            ],
        },
        '-type-function': {
            datatype: true,
            clojureDocs: null,
            title: 'function',
            category: 'Datatype',
            linkName: '-type-function',
            description: 'A function',
            examples: [
                '(fn [x] (+ x 1))',
                '(fn [] 42)',
                '#(+ %1 %2)',
            ],
        },
        '-type-integer': {
            datatype: true,
            clojureDocs: null,
            title: 'integer',
            category: 'Datatype',
            linkName: '-type-integer',
            description: 'An integer',
            examples: [
                '42',
                '-42',
            ],
        },
        '-type-any': {
            datatype: true,
            clojureDocs: null,
            title: 'any',
            category: 'Datatype',
            linkName: '-type-any',
            description: 'Any value',
            examples: [
                '42',
                '"hello"',
                'true',
                'nil',
            ],
        },
        '-type-nil': {
            datatype: true,
            clojureDocs: null,
            title: 'nil',
            category: 'Datatype',
            linkName: '-type-nil',
            description: 'The value nil',
            examples: [
                'nil',
            ],
        },
        '-type-collection': {
            datatype: true,
            clojureDocs: null,
            title: 'collection',
            category: 'Datatype',
            linkName: '-type-collection',
            description: 'A collection, an object, an array or a string',
            examples: [
                '{:foo 42}',
                '[1, 2, 3]',
                '"hello"',
            ],
        },
        '-type-sequence': {
            datatype: true,
            clojureDocs: null,
            title: 'sequence',
            category: 'Datatype',
            linkName: '-type-sequence',
            description: 'A sequence, an array or a string',
            examples: [
                '[1 2 3]',
                '"hello"',
            ],
        },
        '-type-regexp': {
            datatype: true,
            clojureDocs: null,
            title: 'regexp',
            category: 'Datatype',
            linkName: '-type-regexp',
            description: 'A regular expression',
            examples: [
                '(regexp "^\\s*(.*)$")',
                '#"albert"ig',
            ],
        },
        '-type-never': {
            datatype: true,
            clojureDocs: null,
            title: 'never',
            category: 'Datatype',
            linkName: '-type-never',
            description: 'A value that can never be created',
            examples: [
                '(throw "error")',
            ],
        },
    };

    var functionReference = __assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign({}, collectionReference), arrayReference), sequenceReference), mathReference), functionalReference), miscReference), objectReference), predicateReference), regularExpressionReference), specialExpressionsReference), stringReference), bitwiseReference), assertReference);
    var apiReference = __assign(__assign(__assign({}, functionReference), shorthand), datatype);
    Object.values(apiReference).forEach(function (ref) {
        ref.title = ref.title.replace(/"/g, '&quot;');
    });

    var e_1, _a, e_2, _b;
    var api = {
        collection: [
            'count',
            'get',
            'get_in',
            'contains?',
            'has?',
            'has_some?',
            'has_every?',
            'assoc',
            'assoc_in',
            'concat',
            'not_empty',
            'every?',
            'not_every?',
            'any?',
            'not_any?',
            'update',
            'update_in',
        ],
        array: [
            'array',
            'range',
            'repeat',
            'flatten',
            'mapcat',
        ],
        sequence: [
            'nth',
            'push',
            'pop',
            'unshift',
            'shift',
            'slice',
            'reductions',
            'reduce',
            'reduce_right',
            'map',
            'filter',
            'position',
            'index_of',
            'some',
            'reverse',
            'first',
            'second',
            'last',
            'rest',
            'nthrest',
            'next',
            'nthnext',
            'take',
            'take_last',
            'take_while',
            'drop',
            'drop_last',
            'drop_while',
            'sort',
            'sort_by',
            'distinct',
            'remove',
            'remove_at',
            'split_at',
            'split_with',
            'frequencies',
            'group_by',
            'partition',
            'partition_all',
            'partition_by',
        ],
        math: [
            '+',
            '-',
            '*',
            '/',
            'mod',
            '%',
            'quot',
            'inc',
            'dec',
            'sqrt',
            'cbrt',
            '**',
            'exp',
            'round',
            'trunc',
            'floor',
            'ceil',
            'min',
            'max',
            'abs',
            'sign',
            'positive_infinity',
            'negative_infinity',
            'max_safe_integer',
            'min_safe_integer',
            'max_value',
            'min_value',
            'epsilon',
            'nan',
            'e',
            'pi',
            'log',
            'log2',
            'log10',
            'sin',
            'cos',
            'tan',
            'asin',
            'acos',
            'atan',
            'sinh',
            'cosh',
            'tanh',
            'asinh',
            'acosh',
            'atanh',
        ],
        functional: [
            'apply',
            'identity',
            'partial',
            'comp',
            'constantly',
            'juxt',
            'complement',
            'every_pred',
            'some_pred',
            'fnil',
        ],
        misc: [
            '!=',
            '==',
            '<',
            '>',
            '<=',
            '>=',
            '!',
            'write!',
            'iso_date>epoch',
            'epoch>iso_date',
            'boolean',
            'compare',
            'equal?',
            'json_parse',
            'json_stringify',
        ],
        object: [
            'dissoc',
            'object',
            'keys',
            'vals',
            'entries',
            'find',
            'merge',
            'merge_with',
            'zipmap',
            'select_keys',
        ],
        predicate: [
            'boolean?',
            'nil?',
            'number?',
            'string?',
            'function?',
            'integer?',
            'array?',
            'object?',
            'coll?',
            'seq?',
            'regexp?',
            'zero?',
            'pos?',
            'neg?',
            'even?',
            'odd?',
            'finite?',
            'nan?',
            'negative_infinity?',
            'positive_infinity?',
            'false?',
            'true?',
            'empty?',
            'not_empty?',
        ],
        regularExpression: [
            'regexp',
            'match',
            'replace',
        ],
        specialExpressions: [
            '&&',
            '||',
            'def',
            'defs',
            'let',
            'if_let',
            'when_let',
            'when_first',
            'fn',
            'defn',
            'defns',
            'try',
            'throw',
            'if',
            'if_not',
            'cond',
            'when',
            'when_not',
            'comment',
            'do',
            'recur',
            'loop',
            'doseq',
            'for',
            'declared?',
            '??',
        ],
        string: [
            'subs',
            'string_repeat',
            'str',
            'number',
            'lower_case',
            'upper_case',
            'trim',
            'trim_left',
            'trim_right',
            'pad_left',
            'pad_right',
            'split',
            'template',
            'to_char_code',
            'from_char_code',
            'encode_base64',
            'decode_base64',
            'encode_uri_component',
            'decode_uri_component',
            'join',
            '++',
        ],
        bitwise: [
            '<<',
            '>>',
            '>>>',
            '~',
            '&',
            '&!',
            '|',
            '^',
            'bit_flip',
            'bit_clear',
            'bit_set',
            'bit_test',
        ],
        assert: [
            'assert',
            'assert=',
            'assert!=',
            'assert_equal',
            'assert_not_equal',
            'assert_gt',
            'assert_lt',
            'assert_gte',
            'assert_lte',
            'assert_true',
            'assert_false',
            'assert_truthy',
            'assert_falsy',
            'assert_null',
            'assert_throws',
            'assert_throws_error',
            'assert_not_throws',
        ],
        shorthand: [
            '-short-regexp',
            '-short-fn',
            '-short-string',
            '-short-dot',
            '-short-hash',
        ],
        datatype: [
            '-type-number',
            '-type-string',
            '-type-object',
            '-type-array',
            '-type-boolean',
            '-type-function',
            '-type-integer',
            '-type-any',
            '-type-nil',
            '-type-collection',
            '-type-sequence',
            '-type-regexp',
            '-type-never',
        ],
    };
    var functionNames = __spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray([], __read(api.collection), false), __read(api.array), false), __read(api.sequence), false), __read(api.math), false), __read(api.functional), false), __read(api.misc), false), __read(api.object), false), __read(api.predicate), false), __read(api.regularExpression), false), __read(api.specialExpressions), false), __read(api.string), false), __read(api.bitwise), false), __read(api.assert), false);
    var apiNames = __spreadArray(__spreadArray(__spreadArray([], __read(functionNames), false), __read(api.shorthand), false), __read(api.datatype), false);
    var functionNamesFromLitsSrc = __spreadArray(__spreadArray([], __read(normalExpressionKeys), false), __read(specialExpressionKeys), false);
    try {
        for (var functionNamesFromLitsSrc_1 = __values(functionNamesFromLitsSrc), functionNamesFromLitsSrc_1_1 = functionNamesFromLitsSrc_1.next(); !functionNamesFromLitsSrc_1_1.done; functionNamesFromLitsSrc_1_1 = functionNamesFromLitsSrc_1.next()) {
            var functionName = functionNamesFromLitsSrc_1_1.value;
            if (!apiNames.includes(functionName))
                throw new Error("Function name \"".concat(functionName, "\" is not included in the API"));
        }
    }
    catch (e_1_1) { e_1 = { error: e_1_1 }; }
    finally {
        try {
            if (functionNamesFromLitsSrc_1_1 && !functionNamesFromLitsSrc_1_1.done && (_a = functionNamesFromLitsSrc_1.return)) _a.call(functionNamesFromLitsSrc_1);
        }
        finally { if (e_1) throw e_1.error; }
    }
    try {
        for (var functionNames_1 = __values(functionNames), functionNames_1_1 = functionNames_1.next(); !functionNames_1_1.done; functionNames_1_1 = functionNames_1.next()) {
            var functionName = functionNames_1_1.value;
            if (!functionNamesFromLitsSrc.includes(functionName))
                throw new Error("Function name \"".concat(functionName, "\" is not included in the Lits source"));
        }
    }
    catch (e_2_1) { e_2 = { error: e_2_1 }; }
    finally {
        try {
            if (functionNames_1_1 && !functionNames_1_1.done && (_b = functionNames_1.return)) _b.call(functionNames_1);
        }
        finally { if (e_2) throw e_2.error; }
    }

    var getLits = (function () {
        var lits = new Lits({ debug: true });
        var litsNoDebug = new Lits({ debug: false });
        var algebraicLits = new Lits({ debug: true, algebraic: true });
        var algebraicLitsNoDebug = new Lits({ debug: false, algebraic: true });
        return function (forceDebug) {
            if (getState('algebraic')) {
                return forceDebug || getState('debug') ? algebraicLits : algebraicLitsNoDebug;
            }
            else {
                return forceDebug || getState('debug') ? lits : litsNoDebug;
            }
        };
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
        toggleAlgebraicMenuLabel: document.getElementById('toggle-algebraic-menu-label'),
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
                    case 'l':
                        evt.preventDefault();
                        toggleAlgebraic();
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
    function truncateCode(text, count) {
        if (count === void 0) { count = 1000; }
        var oneLiner = text
            .split('\n')
            .map(function (line) { return line.trim(); })
            .filter(function (line) { return line.length > 0; })
            .filter(function (line) { return !line.startsWith(';'); })
            .join(' ');
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
            var result = getLits('debug').analyze(code, litsParams);
            var unresolvedIdentifiers = __spreadArray([], __read(new Set(__spreadArray([], __read(result.unresolvedIdentifiers), false).map(function (s) { return s.symbol; }))), false).join(', ');
            var unresolvedIdentifiersOutput = "Unresolved identifiers: ".concat(unresolvedIdentifiers || '-');
            var possibleOutcomes = result.outcomes && result.outcomes
                .map(function (o) { return o instanceof UserDefinedError
                ? "".concat(o.name).concat(o.userMessage ? "(\"".concat(o.userMessage, "\")") : '')
                : o instanceof Error
                    ? o.name
                    : stringifyValue(o, false); }).join(', ');
            var possibleOutcomesString = "Possible outcomes: ".concat(possibleOutcomes || '-');
            appendOutput("".concat(unresolvedIdentifiersOutput, "\n").concat(possibleOutcomesString), 'analyze');
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
    function toggleAlgebraic() {
        var algebraic = !getState('algebraic');
        saveState({ algebraic: algebraic });
        updateCSS();
        addOutputSeparator();
        appendOutput("Algebraic mode toggled ".concat(algebraic ? 'ON' : 'OFF'), 'comment');
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
        elements.toggleAlgebraicMenuLabel.textContent = getState('algebraic') ? 'Algebraic: ON' : 'Algebraic: OFF';
        elements.litsPanelDebugInfo.style.display = debug ? 'flex' : 'none';
        elements.litsCodeTitle.style.color = (getState('focused-panel') === 'lits-code') ? 'white' : '';
        elements.litsCodeTitleString.textContent = getState('algebraic') ? 'Algebraic Code' : 'Lisp Code';
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
        var example = atob(encodedExample);
        appendLitsCode(";; Example - ".concat(name, " ;;\n\n").concat(example, "\n"));
        saveState({ 'focused-panel': 'lits-code' });
        applyState();
    }
    function setPlayground(name, encodedExample) {
        var example = JSON.parse(atob(encodedExample));
        var context = example.context
            // eslint-disable-next-line ts/no-unsafe-return
            ? JSON.stringify(example.context, function (_k, v) { return (v === undefined ? null : v); }, 2)
            : '';
        setContext(context, true, 'top');
        var code = example.code ? example.code : '';
        var size = Math.max(name.length + 10, 40);
        var paddingLeft = Math.floor((size - name.length) / 2);
        var paddingRight = Math.ceil((size - name.length) / 2);
        setLitsCode("\n".concat(";;".concat('-'.repeat(size), ";;"), "\n").concat(";;".concat(' '.repeat(paddingLeft)).concat(name).concat(' '.repeat(paddingRight), ";;"), "\n").concat(";;".concat('-'.repeat(size), ";;"), "\n\n").concat(code, "\n").trimStart(), true, 'top');
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
    exports.toggleAlgebraic = toggleAlgebraic;
    exports.toggleDebug = toggleDebug;
    exports.tokenize = tokenize;
    exports.undoContextHistory = undoContextHistory;
    exports.undoLitsCodeHistory = undoLitsCodeHistory;

    return exports;

})({});
