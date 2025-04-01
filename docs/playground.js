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

    function getCodeMarker(sourceCodeInfo) {
        if (!sourceCodeInfo.position || !sourceCodeInfo.code)
            return '';
        var leftPadding = sourceCodeInfo.position.column - 1;
        var rightPadding = sourceCodeInfo.code.length - leftPadding - 1;
        return "".concat(' '.repeat(Math.max(leftPadding, 0)), "^").concat(' '.repeat(Math.max(rightPadding, 0)));
    }

    function getLitsErrorMessage(message, sourceCodeInfo) {
        if (!sourceCodeInfo) {
            return message;
        }
        var location = "".concat(sourceCodeInfo.position.line, ":").concat(sourceCodeInfo.position.column);
        var filePathLine = sourceCodeInfo.filePath
            ? "\n".concat(sourceCodeInfo.filePath, ":").concat(location)
            : "\nLocation ".concat(location);
        var codeLine = "\n".concat(sourceCodeInfo.code);
        var codeMarker = "\n".concat(getCodeMarker(sourceCodeInfo));
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

    var NodeTypes = {
        Number: 1,
        String: 2,
        NormalExpression: 3,
        SpecialExpression: 4,
        UserDefinedSymbol: 5,
        NormalBuiltinSymbol: 6,
        SpecialBuiltinSymbol: 7,
        ReservedSymbol: 8,
        Binding: 9,
        Spread: 10,
    };
    var NodeTypesSet = new Set(Object.values(NodeTypes));
    function getNodeTypeName(type) {
        return Object.keys(NodeTypes).find(function (key) { return NodeTypes[key] === type; });
    }
    // TODO, is this needed?
    function isNodeType(type) {
        return typeof type === 'number' && NodeTypesSet.has(type);
    }
    var functionTypes = [
        'UserDefined',
        'Partial',
        'Comp',
        'Constantly',
        'Juxt',
        'Complement',
        'EveryPred',
        'SomePred',
        'Fnull',
        'Builtin',
        'SpecialBuiltin',
        'NativeJsFunction',
    ];
    var functionTypeSet = new Set(functionTypes);
    function isFunctionType(type) {
        return typeof type === 'string' && functionTypeSet.has(type);
    }

    var FUNCTION_SYMBOL = '^^fn^^';
    var REGEXP_SYMBOL = '^^re^^';

    function isLitsFunction$1(func) {
        if (func === null || typeof func !== 'object')
            return false;
        return FUNCTION_SYMBOL in func && 'functionType' in func && isFunctionType(func.functionType);
    }
    function isNode(value) {
        if (!Array.isArray(value) || value.length < 2)
            return false;
        return isNodeType(value[0]);
    }
    function valueToString(value) {
        if (isLitsFunction$1(value))
            // eslint-disable-next-line ts/no-unsafe-member-access
            return "<function ".concat(value.name || '\u03BB', ">");
        if (isNode(value))
            return "".concat(getNodeTypeName(value[0]), "-node");
        if (value === null)
            return 'null';
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

    function assertNumberOfParams(count, node) {
        var length = node[1][1].length;
        if (typeof count === 'number') {
            if (length !== count) {
                var name_1 = getNodeTypeName(node[0]);
                throw new LitsError("Wrong number of arguments to \"".concat(name_1, "\", expected ").concat(count, ", got ").concat(valueToString(length), "."), node[2]);
            }
        }
        else {
            var min = count.min, max = count.max, even = count.even, odd = count.odd;
            if (even) {
                var name_2 = getNodeTypeName(node[0]);
                if (length % 2 !== 0) {
                    throw new LitsError("Wrong number of arguments to \"".concat(name_2, "\",, expected an even number, got ").concat(valueToString(length), "."), node[2]);
                }
            }
            if (odd) {
                if (length % 2 !== 1) {
                    var name_3 = getNodeTypeName(node[0]);
                    throw new LitsError("Wrong number of arguments to \"".concat(name_3, "\",, expected an odd number, got ").concat(valueToString(length), "."), node[2]);
                }
            }
            if (typeof min === 'number' && length < min) {
                var name_4 = getNodeTypeName(node[0]);
                throw new LitsError("Wrong number of arguments to \"".concat(name_4, "\", expected at least ").concat(min, ", got ").concat(valueToString(length), "."), node[2]);
            }
            if (typeof max === 'number' && length > max) {
                var name_5 = getNodeTypeName(node[0]);
                throw new LitsError("Wrong number of arguments to \"".concat(name_5, "\", expected at most ").concat(max, ", got ").concat(valueToString(length), "."), node[2]);
            }
        }
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

    function isLitsFunction(value) {
        if (value === null || typeof value !== 'object')
            return false;
        return !!value[FUNCTION_SYMBOL];
    }
    function asLitsFunction(value, sourceCodeInfo) {
        assertLitsFunction(value, sourceCodeInfo);
        return value;
    }
    function assertLitsFunction(value, sourceCodeInfo) {
        if (!isLitsFunction(value))
            throw getAssertionError('LitsFunction', value, sourceCodeInfo);
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
            if (value.functionType === 'Builtin')
                return "".concat(lt, "builtin function ").concat(value.normalBuitinSymbolType).concat(gt);
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
        if (isMatrix$1(value))
            return stringifyMatrix(value);
        return JSON.stringify(value, null, 2);
    }
    function stringifyMatrix(matrix) {
        var padding = matrix.flat().reduce(function (max, cell) { return Math.max(max, "".concat(cell).length); }, 0) + 1;
        var rows = matrix.map(function (row) { return "[".concat(row.map(function (cell) { return "".concat(cell).padStart(padding); }).join(' '), " ]"); });
        return rows.join('\n');
    }
    function isMatrix$1(value) {
        var e_1, _a, e_2, _b;
        if (!Array.isArray(value)) {
            return false;
        }
        if (!value.every(function (row) { return Array.isArray(row); })) {
            return false;
        }
        var cols = -1;
        try {
            for (var value_1 = __values(value), value_1_1 = value_1.next(); !value_1_1.done; value_1_1 = value_1.next()) {
                var row = value_1_1.value;
                if (cols === -1) {
                    cols = row.length;
                    if (cols === 0) {
                        return false;
                    }
                }
                else {
                    if (row.length !== cols) {
                        return false;
                    }
                }
                try {
                    for (var row_1 = (e_2 = void 0, __values(row)), row_1_1 = row_1.next(); !row_1_1.done; row_1_1 = row_1.next()) {
                        var cell = row_1_1.value;
                        if (typeof cell !== 'number' && typeof cell !== 'string' && typeof cell !== 'boolean' && cell !== null) {
                            return false;
                        }
                    }
                }
                catch (e_2_1) { e_2 = { error: e_2_1 }; }
                finally {
                    try {
                        if (row_1_1 && !row_1_1.done && (_b = row_1.return)) _b.call(row_1);
                    }
                    finally { if (e_2) throw e_2.error; }
                }
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (value_1_1 && !value_1_1.done && (_a = value_1.return)) _a.call(value_1);
            }
            finally { if (e_1) throw e_1.error; }
        }
        return true;
    }

    var specialExpressionTypes = {
        '??': 0,
        '&&': 1,
        '||': 2,
        'array': 3,
        'cond': 4,
        '0_def': 5,
        'defined?': 6,
        '0_defn': 7,
        'do': 8,
        'doseq': 9,
        '0_fn': 10,
        'for': 11,
        'function': 12,
        'if': 13,
        'let': 14,
        'loop': 15,
        'object': 16,
        'recur': 17,
        'switch': 18,
        'throw': 19,
        'try': 20,
        'unless': 21,
    };

    function isSymbolNode(node) {
        var nodeType = node[0];
        return NodeTypes.UserDefinedSymbol === nodeType
            || NodeTypes.NormalBuiltinSymbol === nodeType
            || NodeTypes.SpecialBuiltinSymbol === nodeType;
    }
    function assertSymbolNode(node, sourceCodeInfo) {
        if (!isSymbolNode(node))
            throw getAssertionError('SymbolNode', node, sourceCodeInfo);
    }
    function isUserDefinedSymbolNode(node) {
        return NodeTypes.UserDefinedSymbol === node[0];
    }
    function asUserDefinedSymbolNode(node, sourceCodeInfo) {
        assertUserDefinedSymbolNode(node, sourceCodeInfo);
        return node;
    }
    function assertUserDefinedSymbolNode(node, sourceCodeInfo) {
        if (!isUserDefinedSymbolNode(node))
            throw getAssertionError('UserDefinedSymbolNode', node, sourceCodeInfo);
    }
    function isNormalBuiltinSymbolNode(node) {
        return NodeTypes.NormalBuiltinSymbol === node[0];
    }
    function isSpecialBuiltinSymbolNode(node) {
        return NodeTypes.SpecialBuiltinSymbol === node[0];
    }
    function isNormalExpressionNode(node) {
        return node[0] === NodeTypes.NormalExpression;
    }
    function isNormalExpressionNodeWithName(node) {
        if (!isNormalExpressionNode(node)) {
            return false;
        }
        return isSymbolNode(node[1][0]);
    }
    function isSpreadNode(node) {
        return node[0] === NodeTypes.Spread;
    }

    var getUndefinedSymbols = function (ast, contextStack, builtin, evaluateNode) {
        var e_1, _a;
        var _b;
        var nodes = Array.isArray(ast)
            ? ast
            : [[NodeTypes.SpecialExpression, [specialExpressionTypes.do, ast.body]]];
        var unresolvedSymbols = new Set();
        try {
            for (var nodes_1 = __values(nodes), nodes_1_1 = nodes_1.next(); !nodes_1_1.done; nodes_1_1 = nodes_1.next()) {
                var subNode = nodes_1_1.value;
                (_b = findUnresolvedSymbolsInNode(subNode, contextStack, builtin, evaluateNode)) === null || _b === void 0 ? void 0 : _b.forEach(function (symbol) { return unresolvedSymbols.add(symbol); });
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (nodes_1_1 && !nodes_1_1.done && (_a = nodes_1.return)) _a.call(nodes_1);
            }
            finally { if (e_1) throw e_1.error; }
        }
        return unresolvedSymbols;
    };
    function findUnresolvedSymbolsInNode(node, contextStack, builtin, evaluateNode) {
        var e_2, _a;
        var _b, _c;
        var nodeType = node[0];
        switch (nodeType) {
            case NodeTypes.UserDefinedSymbol: {
                var symbolNode = node;
                var lookUpResult = contextStack.lookUp(symbolNode);
                if (lookUpResult === null)
                    return new Set([symbolNode[1]]);
                return null;
            }
            case NodeTypes.NormalBuiltinSymbol:
            case NodeTypes.SpecialBuiltinSymbol:
            case NodeTypes.String:
            case NodeTypes.Number:
            case NodeTypes.ReservedSymbol:
            case NodeTypes.Binding:
                return null;
            case NodeTypes.NormalExpression: {
                var normalExpressionNode = node;
                var unresolvedSymbols_1 = new Set();
                if (isNormalExpressionNodeWithName(normalExpressionNode)) {
                    var _d = __read(normalExpressionNode, 2), _e = __read(_d[1], 1), symbolNode = _e[0];
                    if (isUserDefinedSymbolNode(symbolNode)) {
                        var lookUpResult = contextStack.lookUp(symbolNode);
                        if (lookUpResult === null)
                            unresolvedSymbols_1.add(symbolNode[1]);
                    }
                }
                else {
                    var _f = __read(normalExpressionNode, 2), _g = __read(_f[1], 1), expressionNode = _g[0];
                    (_b = findUnresolvedSymbolsInNode(expressionNode, contextStack, builtin, evaluateNode)) === null || _b === void 0 ? void 0 : _b.forEach(function (symbol) { return unresolvedSymbols_1.add(symbol); });
                }
                try {
                    for (var _h = __values(normalExpressionNode[1][1]), _j = _h.next(); !_j.done; _j = _h.next()) {
                        var subNode = _j.value;
                        (_c = findUnresolvedSymbolsInNode(subNode, contextStack, builtin, evaluateNode)) === null || _c === void 0 ? void 0 : _c.forEach(function (symbol) { return unresolvedSymbols_1.add(symbol); });
                    }
                }
                catch (e_2_1) { e_2 = { error: e_2_1 }; }
                finally {
                    try {
                        if (_j && !_j.done && (_a = _h.return)) _a.call(_h);
                    }
                    finally { if (e_2) throw e_2.error; }
                }
                return unresolvedSymbols_1;
            }
            case NodeTypes.SpecialExpression: {
                var specialExpressionNode = node;
                var specialExpressionType = specialExpressionNode[1][0];
                var specialExpression = builtin.specialExpressions[specialExpressionType];
                var castedGetUndefinedSymbols = specialExpression.getUndefinedSymbols;
                return castedGetUndefinedSymbols(specialExpressionNode, contextStack, {
                    getUndefinedSymbols: getUndefinedSymbols,
                    builtin: builtin,
                    evaluateNode: evaluateNode,
                });
            }
            case NodeTypes.Spread:
                return findUnresolvedSymbolsInNode(node[1], contextStack, builtin, evaluateNode);
            /* v8 ignore next 2 */
            default:
                throw new LitsError("Unhandled node type: ".concat(nodeType), node[2]);
        }
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
        var equal = typeof options.eq === 'number' ? "equal to ".concat(options.eq) : '';
        return [sign, finite, numberType, range, equal].filter(function (x) { return !!x; }).join(' ');
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

    var sequenceNormalExpression$1 = {
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
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var fn = asLitsFunction(params.at(-1));
                var seqs = params.slice(0, -1);
                assertSeq(seqs[0], sourceCodeInfo);
                var isString = typeof seqs[0] === 'string';
                var len = seqs[0].length;
                seqs.slice(1).forEach(function (seq) {
                    if (isString) {
                        assertString(seq, sourceCodeInfo);
                    }
                    else {
                        assertArray(seq, sourceCodeInfo);
                    }
                    len = Math.min(len, seq.length);
                });
                var paramArray = [];
                var _loop_1 = function (i) {
                    paramArray.push(seqs.map(function (seq) { return seq[i]; }));
                };
                for (var i = 0; i < len; i++) {
                    _loop_1(i);
                }
                var mapped = paramArray.map(function (p) { return executeFunction(fn, p, contextStack, sourceCodeInfo); });
                if (!isString) {
                    return mapped;
                }
                mapped.forEach(function (char) { return assertString(char, sourceCodeInfo); });
                return mapped.join('');
            },
            paramCount: { min: 2 },
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
                    var _loop_2 = function (item) {
                        assertAny(item, sourceCodeInfo);
                        if (!result.some(function (existingItem) { return deepEqual(existingItem, item, sourceCodeInfo); })) {
                            result.push(item);
                        }
                    };
                    try {
                        for (var input_1 = __values(input), input_1_1 = input_1.next(); !input_1_1.done; input_1_1 = input_1.next()) {
                            var item = input_1_1.value;
                            _loop_2(item);
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
                        return seq.split('');
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
            aliases: ['rem'],
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
                if (message !== undefined) {
                    assertString(message, sourceCodeInfo);
                }
                message !== null && message !== void 0 ? message : (message = '');
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
                if (message !== undefined) {
                    assertString(message, sourceCodeInfo);
                }
                message !== null && message !== void 0 ? message : (message = '');
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
                if (message !== undefined) {
                    assertString(message, sourceCodeInfo);
                }
                message !== null && message !== void 0 ? message : (message = '');
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
                if (message !== undefined) {
                    assertString(message, sourceCodeInfo);
                }
                message !== null && message !== void 0 ? message : (message = '');
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
                if (message !== undefined) {
                    assertString(message, sourceCodeInfo);
                }
                message !== null && message !== void 0 ? message : (message = '');
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
                if (message !== undefined) {
                    assertString(message, sourceCodeInfo);
                }
                message !== null && message !== void 0 ? message : (message = '');
                if (compare(first, second, sourceCodeInfo) > 0)
                    throw new AssertionError("Expected ".concat(first, " to be less than or equal to ").concat(second, ".").concat(message), sourceCodeInfo);
                return null;
            },
            paramCount: { min: 2, max: 3 },
        },
        'assert-true': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                if (message !== undefined) {
                    assertString(message, sourceCodeInfo);
                }
                message !== null && message !== void 0 ? message : (message = '');
                if (first !== true)
                    throw new AssertionError("Expected ".concat(first, " to be true.").concat(message), sourceCodeInfo);
                return null;
            },
            paramCount: { min: 1, max: 2 },
        },
        'assert-false': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                if (message !== undefined) {
                    assertString(message, sourceCodeInfo);
                }
                message !== null && message !== void 0 ? message : (message = '');
                if (first !== false)
                    throw new AssertionError("Expected ".concat(first, " to be false.").concat(message), sourceCodeInfo);
                return null;
            },
            paramCount: { min: 1, max: 2 },
        },
        'assert-truthy': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                if (message !== undefined) {
                    assertString(message, sourceCodeInfo);
                }
                message !== null && message !== void 0 ? message : (message = '');
                if (!first)
                    throw new AssertionError("Expected ".concat(first, " to be truthy.").concat(message), sourceCodeInfo);
                return null;
            },
            paramCount: { min: 1, max: 2 },
        },
        'assert-falsy': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                if (message !== undefined) {
                    assertString(message, sourceCodeInfo);
                }
                message !== null && message !== void 0 ? message : (message = '');
                if (first)
                    throw new AssertionError("Expected ".concat(first, " to be falsy.").concat(message), sourceCodeInfo);
                return null;
            },
            paramCount: { min: 1, max: 2 },
        },
        'assert-null': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                if (message !== undefined) {
                    assertString(message, sourceCodeInfo);
                }
                message !== null && message !== void 0 ? message : (message = '');
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
                if (message !== undefined) {
                    assertString(message, sourceCodeInfo);
                }
                message !== null && message !== void 0 ? message : (message = '');
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
                if (message !== undefined) {
                    assertString(message, sourceCodeInfo);
                }
                message !== null && message !== void 0 ? message : (message = '');
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
                if (message !== undefined) {
                    assertString(message, sourceCodeInfo);
                }
                message !== null && message !== void 0 ? message : (message = '');
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
                    _b.functionType = 'Partial',
                    _b.function = toAny(fn),
                    _b.params = params,
                    _b;
            },
            paramCount: { min: 1 },
        },
        'comp': {
            evaluate: function (params, sourceCodeInfo) {
                var _a;
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.sourceCodeInfo = sourceCodeInfo,
                    _a.functionType = 'Comp',
                    _a.params = params,
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
                    _b.functionType = 'Constantly',
                    _b.value = toAny(value),
                    _b;
            },
            paramCount: 1,
        },
        'juxt': {
            evaluate: function (params, sourceCodeInfo) {
                var _a;
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.sourceCodeInfo = sourceCodeInfo,
                    _a.functionType = 'Juxt',
                    _a.params = params,
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
                    _b.functionType = 'Complement',
                    _b.function = toAny(fn),
                    _b;
            },
            paramCount: 1,
        },
        'every-pred': {
            evaluate: function (params, sourceCodeInfo) {
                var _a;
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.sourceCodeInfo = sourceCodeInfo,
                    _a.functionType = 'EveryPred',
                    _a.params = params,
                    _a;
            },
            paramCount: { min: 1 },
        },
        'some-pred': {
            evaluate: function (params, sourceCodeInfo) {
                var _a;
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.sourceCodeInfo = sourceCodeInfo,
                    _a.functionType = 'SomePred',
                    _a.params = params,
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
                    _b.functionType = 'Fnull',
                    _b.function = toAny(fn),
                    _b.params = params,
                    _b;
            },
            paramCount: { min: 2 },
        },
    };

    /**
     * Creates a matrix from a flat array with specified dimensions
     *
     * @param flatArray The flat array of values
     * @param rows Number of rows in the resulting matrix
     * @param cols Number of columns in the resulting matrix
     * @returns A 2D array representing the matrix
     */
    function fromArray(flatArray, rows, cols) {
        // Create the matrix
        var table = [];
        // Reshape the flat array into rows and columns
        for (var i = 0; i < rows; i++) {
            var start = i * cols;
            var end = start + cols;
            table.push(flatArray.slice(start, end));
        }
        return table;
    }

    function tableEqual(tables) {
        var firstTable = tables[0];
        return tables.slice(1).every(function (table) {
            if (table.length !== firstTable.length) {
                return false;
            }
            if (table[0].length !== firstTable[0].length) {
                return false;
            }
            return table.every(function (row, i) {
                return row.every(function (cell, j) { return cell === firstTable[i][j]; });
            });
        });
    }

    function transpose(table) {
        var result = [];
        for (var i = 0; i < table[0].length; i += 1) {
            var row = [];
            for (var j = 0; j < table.length; j += 1) {
                row.push(table[j][i]);
            }
            result.push(row);
        }
        return result;
    }

    function isTable(table) {
        var e_1, _a;
        if (!Array.isArray(table)) {
            return false;
        }
        if (table.length === 0) {
            return false;
        }
        if (!Array.isArray(table[0])) {
            return false;
        }
        var nbrOfCols = table[0].length;
        try {
            for (var _b = __values(table.slice(1)), _c = _b.next(); !_c.done; _c = _b.next()) {
                var row = _c.value;
                if (!Array.isArray(row)) {
                    return false;
                }
                if (row.length !== nbrOfCols) {
                    return false;
                }
                if (row.some(function (cell) { return isAny(cell); })) {
                    return false;
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
        return true;
    }
    function assertTable(table, sourceCodeInfo) {
        if (!isTable(table)) {
            throw new LitsError("Expected a table, but got ".concat(table), sourceCodeInfo);
        }
    }
    function assertAnyArray(value, sourceCodeInfo) {
        if (!Array.isArray(value)) {
            throw new LitsError("Expected an array, but got ".concat(value), sourceCodeInfo);
        }
        if (value.some(function (cell) { return !isAny(cell); })) {
            throw new LitsError("Expected an array of Any, but got ".concat(value), sourceCodeInfo);
        }
    }
    var tableNormalExpression = {
        't:table?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), table = _b[0];
                return isTable(table);
            },
            paramCount: 1,
        },
        't:=': {
            evaluate: function (params, sourceCodeInfo) {
                assertArray(params, sourceCodeInfo);
                params.every(function (table) { return assertTable(table, sourceCodeInfo); });
                if (params.length <= 1) {
                    return true;
                }
                return tableEqual(params);
            },
            paramCount: { min: 1 },
        },
        't:!=': {
            evaluate: function (params, sourceCodeInfo) {
                assertArray(params, sourceCodeInfo);
                params.every(function (table) { return assertTable(table, sourceCodeInfo); });
                if (params.length <= 1) {
                    return false;
                }
                return !tableEqual(params);
            },
            paramCount: { min: 1 },
        },
        'table-every?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_2, _c, e_3, _d;
                var _e = __read(_a, 2), table = _e[0], predicate = _e[1];
                var executeFunction = _b.executeFunction;
                assertTable(table, sourceCodeInfo);
                assertLitsFunction(predicate, sourceCodeInfo);
                try {
                    for (var table_1 = __values(table), table_1_1 = table_1.next(); !table_1_1.done; table_1_1 = table_1.next()) {
                        var row = table_1_1.value;
                        try {
                            for (var row_1 = (e_3 = void 0, __values(row)), row_1_1 = row_1.next(); !row_1_1.done; row_1_1 = row_1.next()) {
                                var cell = row_1_1.value;
                                if (!executeFunction(predicate, [cell], contextStack, sourceCodeInfo)) {
                                    return false;
                                }
                            }
                        }
                        catch (e_3_1) { e_3 = { error: e_3_1 }; }
                        finally {
                            try {
                                if (row_1_1 && !row_1_1.done && (_d = row_1.return)) _d.call(row_1);
                            }
                            finally { if (e_3) throw e_3.error; }
                        }
                    }
                }
                catch (e_2_1) { e_2 = { error: e_2_1 }; }
                finally {
                    try {
                        if (table_1_1 && !table_1_1.done && (_c = table_1.return)) _c.call(table_1);
                    }
                    finally { if (e_2) throw e_2.error; }
                }
                return true;
            },
            paramCount: 2,
        },
        't:some?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_4, _c, e_5, _d;
                var _e = __read(_a, 2), table = _e[0], predicate = _e[1];
                var executeFunction = _b.executeFunction;
                assertTable(table, sourceCodeInfo);
                assertLitsFunction(predicate, sourceCodeInfo);
                try {
                    for (var table_2 = __values(table), table_2_1 = table_2.next(); !table_2_1.done; table_2_1 = table_2.next()) {
                        var row = table_2_1.value;
                        try {
                            for (var row_2 = (e_5 = void 0, __values(row)), row_2_1 = row_2.next(); !row_2_1.done; row_2_1 = row_2.next()) {
                                var cell = row_2_1.value;
                                if (executeFunction(predicate, [cell], contextStack, sourceCodeInfo)) {
                                    return true;
                                }
                            }
                        }
                        catch (e_5_1) { e_5 = { error: e_5_1 }; }
                        finally {
                            try {
                                if (row_2_1 && !row_2_1.done && (_d = row_2.return)) _d.call(row_2);
                            }
                            finally { if (e_5) throw e_5.error; }
                        }
                    }
                }
                catch (e_4_1) { e_4 = { error: e_4_1 }; }
                finally {
                    try {
                        if (table_2_1 && !table_2_1.done && (_c = table_2.return)) _c.call(table_2);
                    }
                    finally { if (e_4) throw e_4.error; }
                }
                return false;
            },
            paramCount: 2,
        },
        't:every-row?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_6, _c;
                var _d = __read(_a, 2), table = _d[0], predicate = _d[1];
                var executeFunction = _b.executeFunction;
                assertTable(table, sourceCodeInfo);
                assertLitsFunction(predicate, sourceCodeInfo);
                try {
                    for (var table_3 = __values(table), table_3_1 = table_3.next(); !table_3_1.done; table_3_1 = table_3.next()) {
                        var row = table_3_1.value;
                        if (!executeFunction(predicate, [row], contextStack, sourceCodeInfo)) {
                            return false;
                        }
                    }
                }
                catch (e_6_1) { e_6 = { error: e_6_1 }; }
                finally {
                    try {
                        if (table_3_1 && !table_3_1.done && (_c = table_3.return)) _c.call(table_3);
                    }
                    finally { if (e_6) throw e_6.error; }
                }
                return true;
            },
            paramCount: 2,
        },
        't:some-row?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_7, _c;
                var _d = __read(_a, 2), table = _d[0], predicate = _d[1];
                var executeFunction = _b.executeFunction;
                assertTable(table, sourceCodeInfo);
                assertLitsFunction(predicate, sourceCodeInfo);
                try {
                    for (var table_4 = __values(table), table_4_1 = table_4.next(); !table_4_1.done; table_4_1 = table_4.next()) {
                        var row = table_4_1.value;
                        if (executeFunction(predicate, [row], contextStack, sourceCodeInfo)) {
                            return true;
                        }
                    }
                }
                catch (e_7_1) { e_7 = { error: e_7_1 }; }
                finally {
                    try {
                        if (table_4_1 && !table_4_1.done && (_c = table_4.return)) _c.call(table_4);
                    }
                    finally { if (e_7) throw e_7.error; }
                }
                return false;
            },
            paramCount: 2,
        },
        't:every-col?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_8, _c;
                var _d = __read(_a, 2), table = _d[0], predicate = _d[1];
                var executeFunction = _b.executeFunction;
                assertTable(table, sourceCodeInfo);
                assertLitsFunction(predicate, sourceCodeInfo);
                var transposed = transpose(table);
                try {
                    for (var transposed_1 = __values(transposed), transposed_1_1 = transposed_1.next(); !transposed_1_1.done; transposed_1_1 = transposed_1.next()) {
                        var row = transposed_1_1.value;
                        if (!executeFunction(predicate, [row], contextStack, sourceCodeInfo)) {
                            return false;
                        }
                    }
                }
                catch (e_8_1) { e_8 = { error: e_8_1 }; }
                finally {
                    try {
                        if (transposed_1_1 && !transposed_1_1.done && (_c = transposed_1.return)) _c.call(transposed_1);
                    }
                    finally { if (e_8) throw e_8.error; }
                }
                return true;
            },
            paramCount: 2,
        },
        't:some-col?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_9, _c;
                var _d = __read(_a, 2), table = _d[0], predicate = _d[1];
                var executeFunction = _b.executeFunction;
                assertTable(table, sourceCodeInfo);
                assertLitsFunction(predicate, sourceCodeInfo);
                var transposed = transpose(table);
                try {
                    for (var transposed_2 = __values(transposed), transposed_2_1 = transposed_2.next(); !transposed_2_1.done; transposed_2_1 = transposed_2.next()) {
                        var row = transposed_2_1.value;
                        if (executeFunction(predicate, [row], contextStack, sourceCodeInfo)) {
                            return true;
                        }
                    }
                }
                catch (e_9_1) { e_9 = { error: e_9_1 }; }
                finally {
                    try {
                        if (transposed_2_1 && !transposed_2_1.done && (_c = transposed_2.return)) _c.call(transposed_2);
                    }
                    finally { if (e_9) throw e_9.error; }
                }
                return false;
            },
            paramCount: 2,
        },
        't:row': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), table = _b[0], row = _b[1];
                assertTable(table, sourceCodeInfo);
                assertNumber(row, sourceCodeInfo, { integer: true, nonNegative: true, lte: table.length });
                return table[row];
            },
            paramCount: 2,
        },
        't:col': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), table = _b[0], col = _b[1];
                assertTable(table, sourceCodeInfo);
                assertNumber(col, sourceCodeInfo, { integer: true, nonNegative: true, lte: table[0].length });
                return table.map(function (row) { return row[col]; });
            },
            paramCount: 2,
        },
        't:shape': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), table = _b[0];
                assertTable(table, sourceCodeInfo);
                return [table.length, table[0].length];
            },
            paramCount: 1,
        },
        't:generate': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 3), rows = _c[0], cols = _c[1], generator = _c[2];
                var executeFunction = _b.executeFunction;
                assertNumber(rows, sourceCodeInfo, { integer: true, positive: true });
                assertNumber(cols, sourceCodeInfo, { integer: true, positive: true });
                assertLitsFunction(generator, sourceCodeInfo);
                var result = [];
                for (var i = 0; i < rows; i += 1) {
                    var row = [];
                    for (var j = 0; j < cols; j += 1) {
                        var value = executeFunction(generator, [i, j], contextStack, sourceCodeInfo);
                        if (!isAny(value)) {
                            throw new LitsError("The generator function must return Any, but got ".concat(value), sourceCodeInfo);
                        }
                        row.push(value);
                    }
                    result.push(row);
                }
                return result;
            },
            paramCount: 3,
        },
        't:reshape': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), table = _b[0], rows = _b[1], cols = _b[2];
                assertTable(table, sourceCodeInfo);
                assertNumber(rows, sourceCodeInfo, { integer: true, positive: true });
                assertNumber(cols, sourceCodeInfo, { integer: true, positive: true });
                var flatTable = table.flat();
                if (flatTable.length !== rows * cols) {
                    throw new LitsError("The number of elements in the table must be equal to rows * cols, but got ".concat(flatTable.length, " and ").concat(rows, " * ").concat(cols), sourceCodeInfo);
                }
                var result = [];
                for (var i = 0; i < rows; i += 1) {
                    var row = [];
                    for (var j = 0; j < cols; j += 1) {
                        row.push(flatTable[i * cols + j]);
                    }
                    result.push(row);
                }
                return result;
            },
            paramCount: 3,
        },
        't:transpose': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), table = _b[0];
                assertTable(table, sourceCodeInfo);
                return transpose(table);
            },
            paramCount: 1,
        },
        't:slice': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 5), table = _b[0], rowStart = _b[1], rowEnd = _b[2], colStart = _b[3], colEnd = _b[4];
                assertTable(table, sourceCodeInfo);
                assertNumber(rowStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: table.length });
                assertNumber(rowEnd, sourceCodeInfo, { integer: true });
                rowEnd = rowEnd < 0 ? table.length + rowEnd : rowEnd;
                assertNumber(rowEnd, sourceCodeInfo, { gt: rowStart, lte: table.length });
                assertNumber(colStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: table[0].length });
                assertNumber(colEnd, sourceCodeInfo, { integer: true });
                colEnd = colEnd < 0 ? table[0].length + colEnd : colEnd;
                assertNumber(colEnd, sourceCodeInfo, { gt: colStart, lte: table[0].length });
                var result = [];
                for (var i = rowStart; i < rowEnd; i += 1) {
                    var row = [];
                    for (var j = colStart; j < colEnd; j += 1) {
                        row.push(table[i][j]);
                    }
                    result.push(row);
                }
                return result;
            },
            paramCount: 5,
        },
        't:slice-rows': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), table = _b[0], rowStart = _b[1], rowEnd = _b[2];
                assertTable(table, sourceCodeInfo);
                if (typeof rowEnd === 'undefined') {
                    assertNumber(rowStart, sourceCodeInfo, { integer: true, lte: table.length, gte: -table.length });
                    if (rowStart < 0) {
                        return table.slice(table.length + rowStart);
                    }
                    return table.slice(rowStart);
                }
                assertNumber(rowStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: table.length });
                assertNumber(rowEnd, sourceCodeInfo, { integer: true });
                rowEnd = rowEnd < 0 ? table.length + rowEnd : rowEnd;
                assertNumber(rowEnd, sourceCodeInfo, { gt: rowStart, lte: table.length });
                return table.slice(rowStart, rowEnd);
            },
            paramCount: 3,
        },
        't:slice-cols': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), table = _b[0], colStart = _b[1], colEnd = _b[2];
                assertTable(table, sourceCodeInfo);
                var trMatrix = transpose(table);
                if (typeof colEnd === 'undefined') {
                    assertNumber(colStart, sourceCodeInfo, { integer: true, lte: trMatrix.length, gte: -trMatrix.length });
                    if (colStart < 0) {
                        return transpose(trMatrix.slice(trMatrix.length + colStart));
                    }
                    return transpose(trMatrix.slice(colStart));
                }
                assertNumber(colStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: trMatrix.length });
                assertNumber(colEnd, sourceCodeInfo, { integer: true });
                colEnd = colEnd < 0 ? trMatrix.length + colEnd : colEnd;
                assertNumber(colEnd, sourceCodeInfo, { gt: colStart, lte: trMatrix.length });
                return transpose(trMatrix.slice(colStart, colEnd));
            },
            paramCount: 3,
        },
        't:splice-rows': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), table = _b[0], rowStart = _b[1], rowDeleteCount = _b[2], rows = _b.slice(3);
                assertTable(table, sourceCodeInfo);
                assertNumber(rowStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: table.length });
                assertNumber(rowDeleteCount, sourceCodeInfo, { integer: true, nonNegative: true });
                assertTable(rows, sourceCodeInfo);
                rows.every(function (row) {
                    assertArray(row, sourceCodeInfo);
                    if (table[0].length !== row.length) {
                        throw new LitsError("All rows must have the same length as the number of columns in table, but got ".concat(row.length), sourceCodeInfo);
                    }
                    return true;
                });
                var result = [];
                for (var i = 0; i < rowStart; i += 1) {
                    result.push(table[i]);
                }
                result.push.apply(result, __spreadArray([], __read(rows), false));
                for (var i = rowStart + rowDeleteCount; i < table.length; i += 1) {
                    result.push(table[i]);
                }
                return result;
            },
            paramCount: { min: 3 },
        },
        't:splice-cols': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), table = _b[0], colStart = _b[1], colDeleteCount = _b[2], cols = _b.slice(3);
                assertTable(table, sourceCodeInfo);
                var trMatrix = transpose(table);
                assertNumber(colStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: trMatrix.length });
                assertNumber(colDeleteCount, sourceCodeInfo, { integer: true, nonNegative: true });
                assertTable(cols, sourceCodeInfo);
                cols.every(function (row) {
                    assertArray(row, sourceCodeInfo);
                    if (trMatrix[0].length !== row.length) {
                        throw new LitsError("All rows must have the same length as the number of rows in table, but got ".concat(row.length), sourceCodeInfo);
                    }
                    return true;
                });
                var result = [];
                for (var i = 0; i < colStart; i += 1) {
                    result.push(trMatrix[i]);
                }
                result.push.apply(result, __spreadArray([], __read(cols), false));
                for (var i = colStart + colDeleteCount; i < trMatrix.length; i += 1) {
                    result.push(trMatrix[i]);
                }
                return transpose(result);
            },
            paramCount: { min: 3 },
        },
        't:concat-rows': {
            evaluate: function (params, sourceCodeInfo) {
                assertArray(params, sourceCodeInfo);
                params.every(function (table) { return assertTable(table, sourceCodeInfo); });
                var cols = params[0][0].length;
                params.slice(1).every(function (table) {
                    if (table[0].length !== cols) {
                        throw new LitsError("All matrices must have the same number of columns, but got ".concat(cols, " and ").concat(table[0].length), sourceCodeInfo);
                    }
                    return true;
                });
                var result = [];
                params.forEach(function (table) {
                    table.forEach(function (row) {
                        result.push(row);
                    });
                });
                return result;
            },
            paramCount: { min: 1 },
        },
        't:concat-cols': {
            evaluate: function (params, sourceCodeInfo) {
                assertArray(params, sourceCodeInfo);
                params.every(function (table) { return assertTable(table, sourceCodeInfo); });
                var rows = params[0].length;
                params.slice(1).every(function (table) {
                    if (table.length !== rows) {
                        throw new LitsError("All matrices must have the same number of rows, but got ".concat(rows, " and ").concat(table.length), sourceCodeInfo);
                    }
                    return true;
                });
                var result = [];
                var _loop_1 = function (i) {
                    var row = [];
                    params.forEach(function (table) {
                        row.push.apply(row, __spreadArray([], __read(table[i]), false));
                    });
                    result.push(row);
                };
                for (var i = 0; i < rows; i += 1) {
                    _loop_1(i);
                }
                return result;
            },
            paramCount: { min: 1 },
        },
        't:map': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var fn = asLitsFunction(params.at(-1));
                var matrices = params.slice(0, -1);
                assertTable(matrices[0], sourceCodeInfo);
                var rows = matrices[0].length;
                var cols = matrices[0][0].length;
                matrices.slice(1).forEach(function (table) {
                    assertTable(table, sourceCodeInfo);
                    if (table.length !== rows) {
                        throw new LitsError("All matrices must have the same number of rows, but got ".concat(rows, " and ").concat(table.length), sourceCodeInfo);
                    }
                    if (table[0].length !== cols) {
                        throw new LitsError("All matrices must have the same number of columns, but got ".concat(cols, " and ").concat(table[0].length), sourceCodeInfo);
                    }
                });
                var result = [];
                var _loop_2 = function (i) {
                    var row = [];
                    var _loop_3 = function (j) {
                        var args = matrices.map(function (table) { return table[i][j]; });
                        var value = executeFunction(fn, args, contextStack, sourceCodeInfo);
                        if (!isAny(value)) {
                            throw new LitsError("The function must return a number, but got ".concat(value), sourceCodeInfo);
                        }
                        row.push(value);
                    };
                    for (var j = 0; j < cols; j += 1) {
                        _loop_3(j);
                    }
                    result.push(row);
                };
                for (var i = 0; i < rows; i += 1) {
                    _loop_2(i);
                }
                return result;
            },
            paramCount: { min: 2 },
        },
        't:map-with-indices': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), table = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertTable(table, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                var rows = table.length;
                var cols = table[0].length;
                var result = [];
                for (var i = 0; i < rows; i += 1) {
                    var row = [];
                    for (var j = 0; j < cols; j += 1) {
                        var value = executeFunction(fn, [table[i][j], i, j], contextStack, sourceCodeInfo);
                        if (!isAny(value)) {
                            throw new LitsError("The function must return a number, but got ".concat(value), sourceCodeInfo);
                        }
                        row.push(value);
                    }
                    result.push(row);
                }
                return result;
            },
            paramCount: 2,
        },
        't:reduce': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_10, _c, e_11, _d;
                var _e = __read(_a, 3), table = _e[0], fn = _e[1], initialValue = _e[2];
                var executeFunction = _b.executeFunction;
                assertTable(table, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                var accumulator = asAny(initialValue);
                try {
                    for (var table_5 = __values(table), table_5_1 = table_5.next(); !table_5_1.done; table_5_1 = table_5.next()) {
                        var row = table_5_1.value;
                        try {
                            for (var row_3 = (e_11 = void 0, __values(row)), row_3_1 = row_3.next(); !row_3_1.done; row_3_1 = row_3.next()) {
                                var cell = row_3_1.value;
                                accumulator = executeFunction(fn, [accumulator, cell], contextStack, sourceCodeInfo);
                            }
                        }
                        catch (e_11_1) { e_11 = { error: e_11_1 }; }
                        finally {
                            try {
                                if (row_3_1 && !row_3_1.done && (_d = row_3.return)) _d.call(row_3);
                            }
                            finally { if (e_11) throw e_11.error; }
                        }
                    }
                }
                catch (e_10_1) { e_10 = { error: e_10_1 }; }
                finally {
                    try {
                        if (table_5_1 && !table_5_1.done && (_c = table_5.return)) _c.call(table_5);
                    }
                    finally { if (e_10) throw e_10.error; }
                }
                return accumulator;
            },
            paramCount: 3,
        },
        't:reduce-with-indices': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 3), table = _c[0], fn = _c[1], initialValue = _c[2];
                var executeFunction = _b.executeFunction;
                assertTable(table, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                var accumulator = asAny(initialValue);
                for (var i = 0; i < table.length; i += 1) {
                    for (var j = 0; j < table[i].length; j += 1) {
                        accumulator = executeFunction(fn, [accumulator, table[i][j], i, j], contextStack, sourceCodeInfo);
                    }
                }
                return accumulator;
            },
            paramCount: 3,
        },
        't:reduce-rows': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_12, _c, e_13, _d;
                var _e = __read(_a, 3), table = _e[0], fn = _e[1], initialValue = _e[2];
                var executeFunction = _b.executeFunction;
                assertTable(table, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                assertAny(initialValue);
                var result = [];
                try {
                    for (var table_6 = __values(table), table_6_1 = table_6.next(); !table_6_1.done; table_6_1 = table_6.next()) {
                        var row = table_6_1.value;
                        var accumulator = asAny(initialValue);
                        try {
                            for (var row_4 = (e_13 = void 0, __values(row)), row_4_1 = row_4.next(); !row_4_1.done; row_4_1 = row_4.next()) {
                                var cell = row_4_1.value;
                                accumulator = executeFunction(fn, [accumulator, cell], contextStack, sourceCodeInfo);
                            }
                        }
                        catch (e_13_1) { e_13 = { error: e_13_1 }; }
                        finally {
                            try {
                                if (row_4_1 && !row_4_1.done && (_d = row_4.return)) _d.call(row_4);
                            }
                            finally { if (e_13) throw e_13.error; }
                        }
                        result.push(accumulator);
                    }
                }
                catch (e_12_1) { e_12 = { error: e_12_1 }; }
                finally {
                    try {
                        if (table_6_1 && !table_6_1.done && (_c = table_6.return)) _c.call(table_6);
                    }
                    finally { if (e_12) throw e_12.error; }
                }
                return result;
            },
            paramCount: 3,
        },
        't:reduce-cols': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 3), table = _c[0], fn = _c[1], initialValue = _c[2];
                var executeFunction = _b.executeFunction;
                assertTable(table, sourceCodeInfo);
                assertLitsFunction(fn, sourceCodeInfo);
                assertAny(initialValue);
                var result = [];
                for (var j = 0; j < table[0].length; j += 1) {
                    var accumulator = asAny(initialValue);
                    for (var i = 0; i < table.length; i += 1) {
                        accumulator = executeFunction(fn, [accumulator, table[i][j]], contextStack, sourceCodeInfo);
                    }
                    result.push(accumulator);
                }
                return result;
            },
            paramCount: 3,
        },
        't:push-rows': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), table = _b[0], rows = _b.slice(1);
                assertTable(table, sourceCodeInfo);
                assertTable(rows, sourceCodeInfo);
                if (table[0].length !== rows[0].length) {
                    throw new LitsError("All rows must have the same length as the number of columns in table, but got ".concat(table[0].length, " and ").concat(rows[0].length), sourceCodeInfo);
                }
                return __spreadArray(__spreadArray([], __read(table), false), __read(rows), false);
            },
            paramCount: { min: 2 },
        },
        't:unshift-rows': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), table = _b[0], rows = _b.slice(1);
                assertTable(table, sourceCodeInfo);
                assertTable(rows, sourceCodeInfo);
                if (table[0].length !== rows[0].length) {
                    throw new LitsError("All rows must have the same length as the number of columns in table, but got ".concat(table[0].length, " and ").concat(rows[0].length), sourceCodeInfo);
                }
                return __spreadArray(__spreadArray([], __read(rows), false), __read(table), false);
            },
            paramCount: { min: 2 },
        },
        't:pop-row': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), table = _b[0];
                assertTable(table, sourceCodeInfo);
                if (table.length === 1) {
                    return null;
                }
                return table.slice(0, -1);
            },
            paramCount: 1,
        },
        't:shift-row': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), table = _b[0];
                assertTable(table, sourceCodeInfo);
                if (table.length === 1) {
                    return null;
                }
                return table.slice(1);
            },
            paramCount: 1,
        },
        't:push-cols': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), table = _b[0], cols = _b.slice(1);
                assertTable(table, sourceCodeInfo);
                assertTable(cols, sourceCodeInfo);
                if (table.length !== cols[0].length) {
                    throw new LitsError("All columns must have the same length as the number of rows in table, but got ".concat(cols.length), sourceCodeInfo);
                }
                var result = [];
                var _loop_4 = function (i) {
                    var row = [];
                    row.push.apply(row, __spreadArray([], __read(table[i]), false));
                    cols.forEach(function (col) {
                        row.push(col[i]);
                    });
                    result.push(row);
                };
                for (var i = 0; i < table.length; i += 1) {
                    _loop_4(i);
                }
                return result;
            },
            paramCount: { min: 2 },
        },
        't:unshift-cols': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), table = _b[0], cols = _b.slice(1);
                assertTable(table, sourceCodeInfo);
                assertTable(cols, sourceCodeInfo);
                if (table.length !== cols[0].length) {
                    throw new LitsError("All columns must have the same length as the number of rows in table, but got ".concat(cols.length), sourceCodeInfo);
                }
                var result = [];
                var _loop_5 = function (i) {
                    var row = [];
                    cols.forEach(function (col) {
                        row.push(col[i]);
                    });
                    row.push.apply(row, __spreadArray([], __read(table[i]), false));
                    result.push(row);
                };
                for (var i = 0; i < table.length; i += 1) {
                    _loop_5(i);
                }
                return result;
            },
            paramCount: { min: 2 },
        },
        't:pop-col': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), table = _b[0];
                assertTable(table, sourceCodeInfo);
                if (table[0].length === 1) {
                    return null;
                }
                return table.map(function (row) { return row.slice(0, -1); });
            },
            paramCount: 1,
        },
        't:shift-col': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), table = _b[0];
                assertTable(table, sourceCodeInfo);
                if (table[0].length === 1) {
                    return null;
                }
                return table.map(function (row) { return row.slice(1); });
            },
            paramCount: 1,
        },
        't:from-array': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), array = _b[0], rows = _b[1], cols = _b[2];
                assertAnyArray(array, sourceCodeInfo);
                assertNumber(rows, sourceCodeInfo, { integer: true, positive: true });
                assertNumber(cols, sourceCodeInfo, { integer: true, positive: true });
                if (array.length !== rows * cols) {
                    throw new LitsError("The number of elements in the array must be equal to rows * cols, but got ".concat(array.length, " and ").concat(rows, " * ").concat(cols), sourceCodeInfo);
                }
                return fromArray(array, rows, cols);
            },
            paramCount: 3,
        },
    };

    /**
     * Counts occurrences of each integer value in an array of non-negative integers.
     *
     * @param array - Array of non-negative integers to count
     * @param minLength - Minimum length of the output array (default: 0)
     * @param weights - Optional array of weights (same length as input array)
     * @returns An array where index i contains the count of occurrences of i in the input array
     */
    function bincount(array, minLength, weights) {
        if (minLength === void 0) { minLength = 0; }
        if (array.length === 0) {
            return Array.from({ length: minLength }, function () { return 0; });
        }
        // Find the maximum value to determine output array size
        var maxValue = Math.max.apply(Math, __spreadArray([], __read(array), false));
        var outputLength = Math.max(maxValue + 1, minLength);
        var counts = Array.from({ length: outputLength }, function () { return 0; });
        // Count occurrences (or sum weights if provided)
        for (var i = 0; i < array.length; i++) {
            var value = Math.floor(array[i]);
            if (value < outputLength) {
                // If weights provided, add weight; otherwise add 1
                counts[value] += weights ? weights[i] : 1;
            }
        }
        return counts;
    }

    function calcMean(vector) {
        if (vector.length === 0) {
            return 0;
        }
        var sum = vector.reduce(function (acc, val) { return acc + val; }, 0);
        return sum / vector.length;
    }

    function calcVariance(vector) {
        if (vector.length === 0) {
            return 0;
        }
        var mean = calcMean(vector);
        var variance = vector.reduce(function (acc, val) { return acc + Math.pow((val - mean), 2); }, 0) / vector.length;
        return variance;
    }

    function calcStdDev(vector) {
        if (vector.length === 0) {
            return 0;
        }
        var variance = calcVariance(vector);
        return Math.sqrt(variance);
    }

    /**
     * Calculates the Shannon entropy of a vector.
     * Entropy measures the amount of uncertainty or randomness in the data.
     *
     * @param vector - An array of values to calculate entropy for
     * @returns The entropy value (in bits) or 0 for empty arrays
     */
    function calculateEntropy(vector) {
        var e_1, _a, e_2, _b;
        // Return 0 for empty vectors
        if (vector.length === 0) {
            return 0;
        }
        // Count occurrences of each value
        var frequencies = new Map();
        try {
            for (var vector_1 = __values(vector), vector_1_1 = vector_1.next(); !vector_1_1.done; vector_1_1 = vector_1.next()) {
                var value = vector_1_1.value;
                frequencies.set(value, (frequencies.get(value) || 0) + 1);
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (vector_1_1 && !vector_1_1.done && (_a = vector_1.return)) _a.call(vector_1);
            }
            finally { if (e_1) throw e_1.error; }
        }
        // Get the total number of elements
        var total = vector.length;
        // Calculate entropy using Shannon's formula
        var entropy = 0;
        try {
            for (var _c = __values(frequencies.values()), _d = _c.next(); !_d.done; _d = _c.next()) {
                var frequency = _d.value;
                var probability = frequency / total;
                // Skip cases where probability is 0 (log(0) is undefined)
                if (probability > 0) {
                    entropy -= probability * Math.log2(probability);
                }
            }
        }
        catch (e_2_1) { e_2 = { error: e_2_1 }; }
        finally {
            try {
                if (_d && !_d.done && (_b = _c.return)) _b.call(_c);
            }
            finally { if (e_2) throw e_2.error; }
        }
        return entropy;
    }

    /**
     * Calculates the mode (most frequent value(s)) of a dataset
     * @param values An array of values of any type
     * @returns An array containing the mode(s) of the dataset
     */
    function mode(values) {
        var e_1, _a, e_2, _b, e_3, _c;
        if (values.length === 0) {
            return [];
        }
        // Create a frequency map
        var frequencyMap = new Map();
        try {
            // Count occurrences of each value
            for (var values_1 = __values(values), values_1_1 = values_1.next(); !values_1_1.done; values_1_1 = values_1.next()) {
                var value = values_1_1.value;
                frequencyMap.set(value, (frequencyMap.get(value) || 0) + 1);
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (values_1_1 && !values_1_1.done && (_a = values_1.return)) _a.call(values_1);
            }
            finally { if (e_1) throw e_1.error; }
        }
        // Find the maximum frequency
        var maxFrequency = 0;
        try {
            for (var _d = __values(frequencyMap.values()), _e = _d.next(); !_e.done; _e = _d.next()) {
                var frequency = _e.value;
                if (frequency > maxFrequency) {
                    maxFrequency = frequency;
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
        // If all values appear only once, there is no mode
        if (maxFrequency === 1) {
            return [];
        }
        // Collect all values that appear with the maximum frequency
        var modes = [];
        try {
            for (var _f = __values(frequencyMap.entries()), _g = _f.next(); !_g.done; _g = _f.next()) {
                var _h = __read(_g.value, 2), value = _h[0], frequency = _h[1];
                if (frequency === maxFrequency) {
                    modes.push(value);
                }
            }
        }
        catch (e_3_1) { e_3 = { error: e_3_1 }; }
        finally {
            try {
                if (_g && !_g.done && (_c = _f.return)) _c.call(_f);
            }
            finally { if (e_3) throw e_3.error; }
        }
        return modes;
    }

    function isVector(vector) {
        if (!Array.isArray(vector)) {
            return false;
        }
        return vector.every(function (elem) { return isNumber(elem, { finite: true }); });
    }
    function assertVector(vector, sourceCodeInfo) {
        if (!isVector(vector)) {
            throw new LitsError("Expected a vector, but got ".concat(vector), sourceCodeInfo);
        }
    }
    function assertNonEmptyVector(vector, sourceCodeInfo) {
        assertVector(vector, sourceCodeInfo);
        if (vector.length === 0) {
            throw new LitsError("Expected a non empty vector, but got ".concat(vector), sourceCodeInfo);
        }
    }
    var vectorNormalExpression = {
        'v:vector?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), vector = _b[0];
                return isVector(vector);
            },
            paramCount: 1,
        },
        'v:sorted?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return vector.every(function (val, i) { return i === 0 || val >= vector[i - 1]; });
            },
            paramCount: 1,
        },
        'v:monotonic?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return vector.every(function (val, i) { return i === 0 || val >= vector[i - 1]; })
                    || vector.every(function (val, i) { return i === 0 || val <= vector[i - 1]; });
            },
            paramCount: 1,
        },
        'v:+': {
            evaluate: function (params, sourceCodeInfo) {
                var e_1, _a;
                var firstParam = params[0];
                assertVector(firstParam, sourceCodeInfo);
                var restParams = params.slice(1);
                try {
                    for (var restParams_1 = __values(restParams), restParams_1_1 = restParams_1.next(); !restParams_1_1.done; restParams_1_1 = restParams_1.next()) {
                        var param = restParams_1_1.value;
                        assertVector(param, sourceCodeInfo);
                        if (firstParam.length !== param.length) {
                            throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                        }
                    }
                }
                catch (e_1_1) { e_1 = { error: e_1_1 }; }
                finally {
                    try {
                        if (restParams_1_1 && !restParams_1_1.done && (_a = restParams_1.return)) _a.call(restParams_1);
                    }
                    finally { if (e_1) throw e_1.error; }
                }
                var rest = restParams;
                return rest.reduce(function (acc, vector) { return acc.map(function (val, i) { return val + vector[i]; }); }, firstParam);
            },
            paramCount: { min: 1 },
        },
        'v:-': {
            evaluate: function (params, sourceCodeInfo) {
                var e_2, _a;
                var firstParam = params[0];
                assertVector(firstParam, sourceCodeInfo);
                var restParams = params.slice(1);
                try {
                    for (var restParams_2 = __values(restParams), restParams_2_1 = restParams_2.next(); !restParams_2_1.done; restParams_2_1 = restParams_2.next()) {
                        var param = restParams_2_1.value;
                        assertVector(param, sourceCodeInfo);
                        if (firstParam.length !== param.length) {
                            throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                        }
                    }
                }
                catch (e_2_1) { e_2 = { error: e_2_1 }; }
                finally {
                    try {
                        if (restParams_2_1 && !restParams_2_1.done && (_a = restParams_2.return)) _a.call(restParams_2);
                    }
                    finally { if (e_2) throw e_2.error; }
                }
                if (restParams.length === 0) {
                    return firstParam.map(function (val) { return -val; });
                }
                var rest = restParams;
                return rest.reduce(function (acc, vector) { return acc.map(function (val, i) { return val - vector[i]; }); }, firstParam);
            },
            paramCount: { min: 1 },
        },
        'v:*': {
            evaluate: function (params, sourceCodeInfo) {
                var e_3, _a;
                var firstParam = params[0];
                assertVector(firstParam, sourceCodeInfo);
                var restParams = params.slice(1);
                try {
                    for (var restParams_3 = __values(restParams), restParams_3_1 = restParams_3.next(); !restParams_3_1.done; restParams_3_1 = restParams_3.next()) {
                        var param = restParams_3_1.value;
                        assertVector(param, sourceCodeInfo);
                        if (firstParam.length !== param.length) {
                            throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                        }
                    }
                }
                catch (e_3_1) { e_3 = { error: e_3_1 }; }
                finally {
                    try {
                        if (restParams_3_1 && !restParams_3_1.done && (_a = restParams_3.return)) _a.call(restParams_3);
                    }
                    finally { if (e_3) throw e_3.error; }
                }
                var rest = restParams;
                return rest.reduce(function (acc, vector) { return acc.map(function (val, i) { return val * vector[i]; }); }, firstParam);
            },
            paramCount: { min: 1 },
        },
        'v:/': {
            evaluate: function (params, sourceCodeInfo) {
                var e_4, _a;
                var firstParam = params[0];
                assertVector(firstParam, sourceCodeInfo);
                var restParams = params.slice(1);
                try {
                    for (var restParams_4 = __values(restParams), restParams_4_1 = restParams_4.next(); !restParams_4_1.done; restParams_4_1 = restParams_4.next()) {
                        var param = restParams_4_1.value;
                        assertVector(param, sourceCodeInfo);
                        if (firstParam.length !== param.length) {
                            throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                        }
                    }
                }
                catch (e_4_1) { e_4 = { error: e_4_1 }; }
                finally {
                    try {
                        if (restParams_4_1 && !restParams_4_1.done && (_a = restParams_4.return)) _a.call(restParams_4);
                    }
                    finally { if (e_4) throw e_4.error; }
                }
                if (restParams.length === 0) {
                    return firstParam.map(function (val) { return 1 / val; });
                }
                var rest = restParams;
                return rest.reduce(function (acc, vector) { return acc.map(function (val, i) { return val / vector[i]; }); }, firstParam);
            },
            paramCount: { min: 1 },
        },
        'v:**': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vector = _b[0], exponent = _b[1];
                assertVector(vector, sourceCodeInfo);
                assertNumber(exponent, sourceCodeInfo, { finite: true });
                return vector.map(function (val) { return Math.pow(val, exponent); });
            },
            paramCount: 2,
        },
        'v:scale': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vector = _b[0], scalar = _b[1];
                assertVector(vector, sourceCodeInfo);
                assertNumber(scalar, sourceCodeInfo, { finite: true });
                return vector.map(function (val) { return val * scalar; });
            },
            paramCount: 2,
        },
        'v:abs': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return vector.map(function (val) { return Math.abs(val); });
            },
            paramCount: 1,
        },
        'v:dot': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                return vectorA.reduce(function (acc, val, i) { return acc + val * vectorB[i]; }, 0);
            },
            paramCount: 2,
        },
        'v:cross': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== 3 || vectorB.length !== 3) {
                    throw new LitsError('Cross product is only defined for 3D vectors', sourceCodeInfo);
                }
                return [
                    vectorA[1] * vectorB[2] - vectorA[2] * vectorB[1],
                    vectorA[2] * vectorB[0] - vectorA[0] * vectorB[2],
                    vectorA[0] * vectorB[1] - vectorA[1] * vectorB[0],
                ];
            },
            paramCount: 2,
        },
        'v:normalize': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                var magnitude = Math.sqrt(vector.reduce(function (acc, val) { return acc + val * val; }, 0));
                if (magnitude === 0) {
                    throw new LitsError('Cannot normalize a zero vector', sourceCodeInfo);
                }
                return vector.map(function (val) { return val / magnitude; });
            },
            paramCount: 1,
        },
        'v:magnitude': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return Math.sqrt(vector.reduce(function (acc, val) { return acc + val * val; }, 0));
            },
            paramCount: 1,
        },
        'v:sum': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return vector.reduce(function (acc, val) { return acc + val; }, 0);
            },
            paramCount: 1,
        },
        'v:product': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return vector.reduce(function (acc, val) { return acc * val; }, 1);
            },
            paramCount: 1,
        },
        'v:mean': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return calcMean(vector);
            },
            paramCount: 1,
        },
        'v:median': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                var sorted = __spreadArray([], __read(vector), false).sort(function (a, b) { return a - b; });
                var mid = Math.floor(sorted.length / 2);
                return sorted.length % 2 === 0
                    ? (sorted[mid - 1] + sorted[mid]) / 2
                    : sorted[mid];
            },
            paramCount: 1,
        },
        'v:mode': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return mode(vector);
            },
            paramCount: 1,
        },
        'v:variance': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                var mean = calcMean(vector);
                return vector.reduce(function (acc, val) { return acc + Math.pow((val - mean), 2); }, 0) / vector.length;
            },
            paramCount: 1,
        },
        'v:std-dev': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return Math.sqrt(vector.reduce(function (acc, val) { return acc + val; }, 0) / vector.length);
            },
            paramCount: 1,
        },
        'v:min': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertNonEmptyVector(vector, sourceCodeInfo);
                return vector.reduce(function (acc, val) { return (val < vector[acc] ? val : acc); }, vector[0]);
            },
            paramCount: 1,
        },
        'v:max': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertNonEmptyVector(vector, sourceCodeInfo);
                return vector.reduce(function (acc, val) { return (val > vector[acc] ? val : acc); }, vector[0]);
            },
            paramCount: 1,
        },
        'v:min-index': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertNonEmptyVector(vector, sourceCodeInfo);
                return vector.reduce(function (acc, val, i) { return (val < vector[acc] ? i : acc); }, 0);
            },
            paramCount: 1,
        },
        'v:max-index': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertNonEmptyVector(vector, sourceCodeInfo);
                return vector.reduce(function (acc, val, i) { return (val > vector[acc] ? i : acc); }, 0);
            },
            paramCount: 1,
        },
        'v:sort-indices': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return __spreadArray([], __read(vector.keys()), false).sort(function (a, b) { return vector[a] - vector[b]; });
            },
            paramCount: 1,
        },
        'v:count-values': {
            evaluate: function (_a, sourceCodeInfo) {
                var e_5, _b;
                var _c = __read(_a, 1), vector = _c[0];
                assertVector(vector, sourceCodeInfo);
                var frequencyMap = new Map();
                try {
                    for (var vector_1 = __values(vector), vector_1_1 = vector_1.next(); !vector_1_1.done; vector_1_1 = vector_1.next()) {
                        var value = vector_1_1.value;
                        frequencyMap.set(value, (frequencyMap.get(value) || 0) + 1);
                    }
                }
                catch (e_5_1) { e_5 = { error: e_5_1 }; }
                finally {
                    try {
                        if (vector_1_1 && !vector_1_1.done && (_b = vector_1.return)) _b.call(vector_1);
                    }
                    finally { if (e_5) throw e_5.error; }
                }
                return __spreadArray([], __read(frequencyMap.entries()), false).sort(function (a, b) {
                    // First compare by count (descending)
                    var countDiff = b[1] - a[1];
                    if (countDiff !== 0)
                        return countDiff;
                    // If counts are equal, sort by value (ascending)
                    return a[0] - b[0];
                });
            },
            paramCount: 1,
        },
        'v:linspace': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), start = _b[0], end = _b[1], numPoints = _b[2];
                assertNumber(start, sourceCodeInfo, { finite: true });
                assertNumber(end, sourceCodeInfo, { finite: true });
                assertNumber(numPoints, sourceCodeInfo, { integer: true, positive: true });
                var step = (end - start) / (numPoints - 1);
                return Array.from({ length: numPoints }, function (_, i) { return start + i * step; });
            },
            paramCount: 3,
        },
        'v:ones': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                return Array.from({ length: length }, function () { return 1; });
            },
            paramCount: 1,
        },
        'v:zeros': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                return Array.from({ length: length }, function () { return 0; });
            },
            paramCount: 1,
        },
        'v:fill': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), length = _b[0], value = _b[1];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                return Array.from({ length: length }, function () { return value; });
            },
            paramCount: 2,
        },
        'v:generate': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), length = _c[0], generator = _c[1];
                var executeFunction = _b.executeFunction;
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                assertLitsFunction(generator, sourceCodeInfo);
                return Array.from({ length: length }, function (_, i) {
                    var value = executeFunction(generator, [i], contextStack, sourceCodeInfo);
                    assertNumber(value, sourceCodeInfo, { finite: true });
                    return value;
                });
            },
            paramCount: 2,
        },
        'v:cumsum': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return vector.reduce(function (acc, val) {
                    var last = acc[acc.length - 1] || 0;
                    acc.push(last + val);
                    return acc;
                }, []);
            },
            paramCount: 1,
        },
        'v:cumprod': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return vector.reduce(function (acc, val) {
                    var last = acc[acc.length - 1] || 1;
                    acc.push(last * val);
                    return acc;
                }, []);
            },
            paramCount: 1,
        },
        'v:angle': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                var dotProduct = vectorA.reduce(function (acc, val, i) { return acc + val * vectorB[i]; }, 0);
                var magnitudeA = Math.sqrt(vectorA.reduce(function (acc, val) { return acc + val * val; }, 0));
                var magnitudeB = Math.sqrt(vectorB.reduce(function (acc, val) { return acc + val * val; }, 0));
                return Math.acos(dotProduct / (magnitudeA * magnitudeB));
            },
            paramCount: 2,
        },
        'v:projection': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                var dotProduct = vectorA.reduce(function (acc, val, i) { return acc + val * vectorB[i]; }, 0);
                var magnitudeB = Math.sqrt(vectorB.reduce(function (acc, val) { return acc + val * val; }, 0));
                return vectorB.map(function (val) { return (dotProduct / (Math.pow(magnitudeB, 2))) * val; });
            },
            paramCount: 2,
        },
        'v:orthogonal?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                var dotProduct = vectorA.reduce(function (acc, val, i) { return acc + val * vectorB[i]; }, 0);
                return dotProduct === 0;
            },
            paramCount: 2,
        },
        'v:parallel?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                var crossProduct = vectorA.reduce(function (acc, val, i) { return acc + val * vectorB[i]; }, 0);
                return crossProduct === 0;
            },
            paramCount: 2,
        },
        'v:collinear?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                var crossProduct = vectorA.reduce(function (acc, val, i) { return acc + val * vectorB[i]; }, 0);
                return crossProduct === 0;
            },
            paramCount: 2,
        },
        'v:cosine-similarity': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                var dotProduct = vectorA.reduce(function (acc, val, i) { return acc + val * vectorB[i]; }, 0);
                var magnitudeA = Math.sqrt(vectorA.reduce(function (acc, val) { return acc + val * val; }, 0));
                var magnitudeB = Math.sqrt(vectorB.reduce(function (acc, val) { return acc + val * val; }, 0));
                return dotProduct / (magnitudeA * magnitudeB);
            },
            paramCount: 2,
        },
        'v:distance': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                return Math.sqrt(vectorA.reduce(function (acc, val, i) { return acc + Math.pow((val - vectorB[i]), 2); }, 0));
            },
            paramCount: 2,
        },
        'v:euclidean-distance': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                return Math.sqrt(vectorA.reduce(function (acc, val, i) { return acc + Math.pow((val - vectorB[i]), 2); }, 0));
            },
            paramCount: 2,
        },
        'v:manhattan-distance': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                return vectorA.reduce(function (acc, val, i) { return acc + Math.abs(val - vectorB[i]); }, 0);
            },
            paramCount: 2,
        },
        'v:hamming-distance': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                return vectorA.reduce(function (acc, val, i) { return acc + (val !== vectorB[i] ? 1 : 0); }, 0);
            },
            paramCount: 2,
        },
        'v:chebyshev-distance': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                return Math.max.apply(Math, __spreadArray([], __read(vectorA.map(function (val, i) { return Math.abs(val - vectorB[i]); })), false));
            },
            paramCount: 2,
        },
        'v:minkowski-distance': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), vectorA = _b[0], vectorB = _b[1], p = _b[2];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                assertNumber(p, sourceCodeInfo, { finite: true });
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                return Math.pow(vectorA.reduce(function (acc, val, i) { return acc + Math.pow(Math.abs(val - vectorB[i]), p); }, 0), (1 / p));
            },
            paramCount: 3,
        },
        'v:jaccard-distance': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                var intersection = vectorA.filter(function (val) { return vectorB.includes(val); }).length;
                var union = new Set(__spreadArray(__spreadArray([], __read(vectorA), false), __read(vectorB), false)).size;
                return 1 - intersection / union;
            },
            paramCount: 2,
        },
        'v:dice-coefficient': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                var intersection = vectorA.filter(function (val) { return vectorB.includes(val); }).length;
                return (2 * intersection) / (vectorA.length + vectorB.length);
            },
            paramCount: 2,
        },
        'v:levenshtein-distance': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                var m = vectorA.length;
                var n = vectorB.length;
                var d = Array.from({ length: m + 1 }, function () { return Array(n + 1).fill(0); });
                for (var i = 0; i <= m; i += 1) {
                    d[i][0] = i;
                }
                for (var j = 0; j <= n; j += 1) {
                    d[0][j] = j;
                }
                for (var i = 1; i <= m; i += 1) {
                    for (var j = 1; j <= n; j += 1) {
                        var cost = vectorA[i - 1] === vectorB[j - 1] ? 0 : 1;
                        d[i][j] = Math.min(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + cost);
                    }
                }
                return d[m][n];
            },
            paramCount: 2,
        },
        'v:l1-norm': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return vector.reduce(function (acc, val) { return acc + Math.abs(val); }, 0);
            },
            paramCount: 1,
        },
        'v:l2-norm': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return Math.sqrt(vector.reduce(function (acc, val) { return acc + val * val; }, 0));
            },
            paramCount: 1,
        },
        'v:quartiles': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                var sorted = __spreadArray([], __read(vector), false).sort(function (a, b) { return a - b; });
                var q1 = sorted[Math.floor((sorted.length / 4))];
                var q2 = sorted[Math.floor((sorted.length / 2))];
                var q3 = sorted[Math.floor((3 * sorted.length) / 4)];
                return [q1, q2, q3];
            },
            paramCount: 1,
        },
        'v:iqr': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                var sorted = __spreadArray([], __read(vector), false).sort(function (a, b) { return a - b; });
                var q1 = sorted[Math.floor((sorted.length / 4))];
                var q3 = sorted[Math.floor((3 * sorted.length) / 4)];
                return q3 - q1;
            },
            paramCount: 1,
        },
        'v:percentile': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vector = _b[0], percentile = _b[1];
                assertVector(vector, sourceCodeInfo);
                assertNumber(percentile, sourceCodeInfo, { finite: true });
                var sorted = __spreadArray([], __read(vector), false).sort(function (a, b) { return a - b; });
                var index = Math.floor((percentile / 100) * (sorted.length - 1));
                return sorted[index];
            },
            paramCount: 2,
        },
        'v:quantile': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vector = _b[0], quantile = _b[1];
                assertVector(vector, sourceCodeInfo);
                assertNumber(quantile, sourceCodeInfo, { finite: true });
                var sorted = __spreadArray([], __read(vector), false).sort(function (a, b) { return a - b; });
                var index = Math.floor(quantile * (sorted.length - 1));
                return sorted[index];
            },
            paramCount: 2,
        },
        'v:range': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertNonEmptyVector(vector, sourceCodeInfo);
                var min = vector.reduce(function (acc, val) { return (val < vector[acc] ? val : acc); }, vector[0]);
                var max = vector.reduce(function (acc, val) { return (val > vector[acc] ? val : acc); }, vector[0]);
                return max - min;
            },
            paramCount: 1,
        },
        'v:skewness': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                var mean = calcMean(vector);
                var stdDev = calcStdDev(vector);
                return vector.reduce(function (acc, val) { return acc + (Math.pow((val - mean), 3)); }, 0) / (vector.length * Math.pow(stdDev, 3));
            },
            paramCount: 1,
        },
        'v:kurtosis': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                var mean = calcMean(vector);
                var stdDev = calcStdDev(vector);
                return vector.reduce(function (acc, val) { return acc + (Math.pow((val - mean), 4)); }, 0) / (vector.length * Math.pow(stdDev, 4));
            },
            paramCount: 1,
        },
        'v:geometric-mean': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return Math.exp(vector.reduce(function (acc, val) { return acc + Math.log(val); }, 0) / vector.length);
            },
            paramCount: 1,
        },
        'v:harmonic-mean': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return vector.length / vector.reduce(function (acc, val) { return acc + 1 / val; }, 0);
            },
            paramCount: 1,
        },
        'v:rms': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return Math.sqrt(vector.reduce(function (acc, val) { return acc + Math.pow(val, 2); }, 0) / vector.length);
            },
            paramCount: 1,
        },
        'v:z-score': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                var mean = calcMean(vector);
                var stdDev = calcStdDev(vector);
                return vector.map(function (val) { return (val - mean) / stdDev; });
            },
            paramCount: 1,
        },
        'v:normalize-minmax': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                var min = vector.reduce(function (acc, val) { return (val < vector[acc] ? val : acc); }, vector[0]);
                var max = vector.reduce(function (acc, val) { return (val > vector[acc] ? val : acc); }, vector[0]);
                return vector.map(function (val) { return (val - min) / (max - min); });
            },
            paramCount: 1,
        },
        'v:normalize-robust': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                var median = vector.reduce(function (acc, val) { return acc + val; }, 0) / vector.length;
                var iqr = vector.reduce(function (acc, val) { return acc + Math.abs(val - median); }, 0) / vector.length;
                return vector.map(function (val) { return (val - median) / iqr; });
            },
            paramCount: 1,
        },
        'v:covariance': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                var meanA = calcMean(vectorA);
                var meanB = calcMean(vectorB);
                return vectorA.reduce(function (acc, val, i) { return acc + (val - meanA) * (vectorB[i] - meanB); }, 0) / vectorA.length;
            },
            paramCount: 2,
        },
        'v:correlation': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                var meanA = calcMean(vectorA);
                var meanB = calcMean(vectorB);
                var numerator = vectorA.reduce(function (acc, val, i) { return acc + (val - meanA) * (vectorB[i] - meanB); }, 0);
                var denominator = Math.sqrt(vectorA.reduce(function (acc, val) { return acc + Math.pow((val - meanA), 2); }, 0) * vectorB.reduce(function (acc, val) { return acc + Math.pow((val - meanB), 2); }, 0));
                return numerator / denominator;
            },
            paramCount: 2,
        },
        'v:spearman-correlation': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                var rankA = __spreadArray([], __read(vectorA.keys()), false).sort(function (a, b) { return vectorA[a] - vectorA[b]; });
                var rankB = __spreadArray([], __read(vectorB.keys()), false).sort(function (a, b) { return vectorB[a] - vectorB[b]; });
                return vectorA.reduce(function (acc, _val, i) { return acc + (rankA[i] - rankB[i]); }, 0) / vectorA.length;
            },
            paramCount: 2,
        },
        'v:kendall-tau': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                var concordant = 0;
                var discordant = 0;
                for (var i = 0; i < vectorA.length; i += 1) {
                    for (var j = i + 1; j < vectorA.length; j += 1) {
                        if ((vectorA[i] - vectorA[j]) * (vectorB[i] - vectorB[j]) > 0) {
                            concordant += 1;
                        }
                        else {
                            discordant += 1;
                        }
                    }
                }
                return (concordant - discordant) / Math.sqrt(Math.pow((concordant + discordant), 2));
            },
            paramCount: 2,
        },
        'v:histogram': {
            evaluate: function (_a, sourceCodeInfo) {
                var e_6, _b;
                var _c = __read(_a, 2), vector = _c[0], bins = _c[1];
                assertVector(vector, sourceCodeInfo);
                assertNumber(bins, sourceCodeInfo, { integer: true, positive: true });
                var min = vector.reduce(function (acc, val) { return (val < vector[acc] ? val : acc); }, vector[0]);
                var max = vector.reduce(function (acc, val) { return (val > vector[acc] ? val : acc); }, vector[0]);
                var binSize = (max - min) / bins;
                var histogram = Array.from({ length: bins }, function () { return 0; });
                try {
                    for (var vector_2 = __values(vector), vector_2_1 = vector_2.next(); !vector_2_1.done; vector_2_1 = vector_2.next()) {
                        var value = vector_2_1.value;
                        var binIndex = Math.floor((value - min) / binSize);
                        if (binIndex >= 0 && binIndex < bins) {
                            histogram[binIndex] += 1;
                        }
                    }
                }
                catch (e_6_1) { e_6 = { error: e_6_1 }; }
                finally {
                    try {
                        if (vector_2_1 && !vector_2_1.done && (_b = vector_2.return)) _b.call(vector_2);
                    }
                    finally { if (e_6) throw e_6.error; }
                }
                return histogram;
            },
            paramCount: 2,
        },
        'v:cdf': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vector = _b[0], value = _b[1];
                assertVector(vector, sourceCodeInfo);
                assertNumber(value, sourceCodeInfo, { finite: true });
                var sorted = __spreadArray([], __read(vector), false).sort(function (a, b) { return a - b; });
                var index = sorted.findIndex(function (val) { return val > value; });
                return index === -1 ? 1 : index / sorted.length;
            },
            paramCount: 2,
        },
        'v:ecdf': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vector = _b[0], value = _b[1];
                assertVector(vector, sourceCodeInfo);
                assertNumber(value, sourceCodeInfo, { finite: true });
                var sorted = __spreadArray([], __read(vector), false).sort(function (a, b) { return a - b; });
                var index = sorted.findIndex(function (val) { return val >= value; });
                return index === -1 ? 1 : (index + 1) / sorted.length;
            },
            paramCount: 2,
        },
        'v:no-extreme-eutliers?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                var mean = calcMean(vector);
                var stdDev = calcStdDev(vector);
                return vector.every(function (val) { return Math.abs((val - mean) / stdDev) < 3; });
            },
            paramCount: 1,
        },
        'v:outliers': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                var mean = calcMean(vector);
                var stdDev = calcStdDev(vector);
                return vector.filter(function (val) { return Math.abs((val - mean) / stdDev) > 3; });
            },
            paramCount: 1,
        },
        'v:moving-average': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vector = _b[0], windowSize = _b[1];
                assertVector(vector, sourceCodeInfo);
                assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true });
                var result = [];
                for (var i = 0; i < vector.length - windowSize + 1; i += 1) {
                    var sum = vector.slice(i, i + windowSize).reduce(function (acc, val) { return acc + val; }, 0);
                    result.push(sum / windowSize);
                }
                return result;
            },
            paramCount: 2,
        },
        'v:moving-median': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vector = _b[0], windowSize = _b[1];
                assertVector(vector, sourceCodeInfo);
                assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true });
                var result = [];
                for (var i = 0; i < vector.length - windowSize + 1; i += 1) {
                    var median = vector.slice(i, i + windowSize).sort(function (a, b) { return a - b; })[Math.floor(windowSize / 2)];
                    result.push(median);
                }
                return result;
            },
            paramCount: 2,
        },
        'v:moving-std': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vector = _b[0], windowSize = _b[1];
                assertVector(vector, sourceCodeInfo);
                assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true });
                var result = [];
                for (var i = 0; i < vector.length - windowSize + 1; i += 1) {
                    var stdDev = calcStdDev(vector.slice(i, i + windowSize));
                    result.push(stdDev);
                }
                return result;
            },
            paramCount: 2,
        },
        'v:moving-sum': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vector = _b[0], windowSize = _b[1];
                assertVector(vector, sourceCodeInfo);
                assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true });
                var result = [];
                for (var i = 0; i < vector.length - windowSize + 1; i += 1) {
                    var sum = vector.slice(i, i + windowSize).reduce(function (acc, val) { return acc + val; }, 0);
                    result.push(sum);
                }
                return result;
            },
            paramCount: 2,
        },
        'v:moving-product': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vector = _b[0], windowSize = _b[1];
                assertVector(vector, sourceCodeInfo);
                assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true });
                var result = [];
                for (var i = 0; i < vector.length - windowSize + 1; i += 1) {
                    var product = vector.slice(i, i + windowSize).reduce(function (acc, val) { return acc * val; }, 1);
                    result.push(product);
                }
                return result;
            },
            paramCount: 2,
        },
        'v:moving-min': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vector = _b[0], windowSize = _b[1];
                assertVector(vector, sourceCodeInfo);
                assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true });
                var result = [];
                for (var i = 0; i < vector.length - windowSize + 1; i += 1) {
                    var min = vector.slice(i, i + windowSize).reduce(function (acc, val) { return (val < acc ? val : acc); }, vector[i]);
                    result.push(min);
                }
                return result;
            },
            paramCount: 2,
        },
        'v:moving-max': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vector = _b[0], windowSize = _b[1];
                assertVector(vector, sourceCodeInfo);
                assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true });
                var result = [];
                for (var i = 0; i < vector.length - windowSize + 1; i += 1) {
                    var max = vector.slice(i, i + windowSize).reduce(function (acc, val) { return (val > acc ? val : acc); }, vector[i]);
                    result.push(max);
                }
                return result;
            },
            paramCount: 2,
        },
        'moving-variance': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vector = _b[0], windowSize = _b[1];
                assertVector(vector, sourceCodeInfo);
                assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true });
                var result = [];
                for (var i = 0; i < vector.length - windowSize + 1; i += 1) {
                    result.push(calcVariance(vector.slice(i, i + windowSize)));
                }
                return result;
            },
            paramCount: 2,
        },
        'moving-rms': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vector = _b[0], windowSize = _b[1];
                assertVector(vector, sourceCodeInfo);
                assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true });
                var result = [];
                var _loop_1 = function (i) {
                    var mean = calcMean(vector.slice(i, i + windowSize));
                    var rms = Math.sqrt(vector.slice(i, i + windowSize).reduce(function (acc, val) { return acc + Math.pow((val - mean), 2); }, 0) / windowSize);
                    result.push(rms);
                };
                for (var i = 0; i < vector.length - windowSize + 1; i += 1) {
                    _loop_1(i);
                }
                return result;
            },
            paramCount: 2,
        },
        'v:moving-percentile': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), vector = _b[0], windowSize = _b[1], percentile = _b[2];
                assertVector(vector, sourceCodeInfo);
                assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true });
                assertNumber(percentile, sourceCodeInfo, { finite: true });
                var result = [];
                for (var i = 0; i < vector.length - windowSize + 1; i += 1) {
                    var sorted = vector.slice(i, i + windowSize).sort(function (a, b) { return a - b; });
                    var index = Math.floor(percentile * (sorted.length - 1));
                    result.push(sorted[index]);
                }
                return result;
            },
            paramCount: 3,
        },
        'v:moving-quantile': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), vector = _b[0], windowSize = _b[1], quantile = _b[2];
                assertVector(vector, sourceCodeInfo);
                assertNumber(windowSize, sourceCodeInfo, { integer: true, positive: true });
                assertNumber(quantile, sourceCodeInfo, { finite: true });
                var result = [];
                for (var i = 0; i < vector.length - windowSize + 1; i += 1) {
                    var sorted = vector.slice(i, i + windowSize).sort(function (a, b) { return a - b; });
                    var index = Math.floor(quantile * (sorted.length - 1));
                    result.push(sorted[index]);
                }
                return result;
            },
            paramCount: 3,
        },
        'v:entropy': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                return calculateEntropy(vector);
            },
            paramCount: 1,
        },
        'v:gini-coefficient': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                var sorted = __spreadArray([], __read(vector), false).sort(function (a, b) { return a - b; });
                var n = sorted.length;
                var sum = sorted.reduce(function (acc, val) { return acc + val; }, 0);
                var gini = (2 * sorted.reduce(function (acc, val, i) { return acc + (i + 1) * val; }, 0)) / (n * sum) - (n + 1) / n;
                return gini;
            },
            paramCount: 1,
        },
        'v:bincount': {
            evaluate: function (params, sourceCodeInfo) {
                var _a, _b;
                var vector = params[0];
                assertVector(vector, sourceCodeInfo);
                vector.forEach(function (val) { return assertNumber(val, sourceCodeInfo, { finite: true, integer: true, nonNegative: true }); });
                var minSize = (_a = params[1]) !== null && _a !== void 0 ? _a : 0;
                assertNumber(minSize, sourceCodeInfo, { integer: true, nonNegative: true });
                var weights = (_b = params[2]) !== null && _b !== void 0 ? _b : undefined;
                if (weights !== null) {
                    assertVector(weights, sourceCodeInfo);
                    if (weights.length !== vector.length) {
                        throw new LitsError('Weights vector must be the same length as the input vector', sourceCodeInfo);
                    }
                    weights.forEach(function (val) { return assertNumber(val, sourceCodeInfo, { finite: true }); });
                }
                return bincount(vector, minSize, weights);
            },
            paramCount: { min: 1, max: 3 },
        },
        'v:arithmetic-sum': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), start = _b[0], step = _b[1], length = _b[2];
                assertNumber(start, sourceCodeInfo, { finite: true });
                assertNumber(step, sourceCodeInfo, { finite: true });
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                return (length / 2) * (2 * start + (length - 1) * step);
            },
            paramCount: 3,
        },
        'v:autocorrelation': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vector = _b[0], lag = _b[1];
                assertVector(vector, sourceCodeInfo);
                var effectiveLag = lag !== null && lag !== void 0 ? lag : vector.length - 1;
                assertNumber(effectiveLag, sourceCodeInfo, { integer: true });
                if (effectiveLag >= vector.length) {
                    throw new LitsError('Lag must be less than the length of the vector', sourceCodeInfo);
                }
                var mean = calcMean(vector);
                var variance = calcVariance(vector);
                var autocovariance = vector.reduce(function (acc, val, i) { return acc + (val - mean) * (vector[i + effectiveLag] - mean); }, 0) / vector.length;
                return autocovariance / variance;
            },
            paramCount: { min: 1, max: 2 },
        },
        'v:cross-correlation': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), vectorA = _b[0], vectorB = _b[1], lag = _b[2];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                var effectiveLag = lag !== null && lag !== void 0 ? lag : vectorA.length - 1;
                assertNumber(effectiveLag, sourceCodeInfo, { integer: true, positive: true });
                if (effectiveLag >= vectorA.length || effectiveLag >= vectorB.length) {
                    throw new LitsError('Lag must be less than the length of the vectors', sourceCodeInfo);
                }
                var n = vectorA.length;
                var meanA = vectorA.reduce(function (sum, x) { return sum + x; }, 0) / n;
                var meanB = vectorB.reduce(function (sum, x) { return sum + x; }, 0) / n;
                var stdA = Math.sqrt(vectorA.reduce(function (sum, x) { return sum + Math.pow((x - meanA), 2); }, 0) / n);
                var stdB = Math.sqrt(vectorB.reduce(function (sum, x) { return sum + Math.pow((x - meanB), 2); }, 0) / n);
                if (stdA === 0 || stdB === 0)
                    return 0;
                var overlapLength = n - Math.abs(effectiveLag);
                var sum = 0;
                if (effectiveLag >= 0) {
                    for (var i = 0; i < overlapLength; i++) {
                        sum += (vectorA[i] - meanA) * (vectorB[i + effectiveLag] - meanB);
                    }
                }
                else {
                    for (var i = 0; i < overlapLength; i++) {
                        sum += (vectorA[i - effectiveLag] - meanA) * (vectorB[i] - meanB);
                    }
                }
                return sum / (overlapLength * stdA * stdB);
            },
            paramCount: 3,
        },
        'v:winsorize': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), vector = _b[0], lowerPercentile = _b[1], upperPercentile = _b[2];
                assertVector(vector, sourceCodeInfo);
                assertNumber(lowerPercentile, sourceCodeInfo, { finite: true });
                assertNumber(upperPercentile, sourceCodeInfo, { finite: true });
                if (vector.length === 0)
                    return [];
                if (lowerPercentile < 0 || lowerPercentile > 0.5 || upperPercentile < 0 || upperPercentile > 0.5) {
                    throw new LitsError('Percentiles must be between 0 and 0.5', sourceCodeInfo);
                }
                var sorted = __spreadArray([], __read(vector), false).sort(function (a, b) { return a - b; });
                var lowerIndex = Math.max(0, Math.floor(lowerPercentile * vector.length));
                var upperIndex = Math.min(vector.length - 1, Math.floor((1 - upperPercentile) * vector.length));
                var lowerBound = sorted[lowerIndex];
                var upperBound = sorted[upperIndex];
                return vector.map(function (val) { return Math.max(lowerBound, Math.min(val, upperBound)); });
            },
            paramCount: 3,
        },
        'v:mse': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                return vectorA.reduce(function (acc, val, i) { return acc + Math.pow((val - vectorB[i]), 2); }, 0) / vectorA.length;
            },
            paramCount: 2,
        },
        'v:mae': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), vectorA = _b[0], vectorB = _b[1];
                assertVector(vectorA, sourceCodeInfo);
                assertVector(vectorB, sourceCodeInfo);
                if (vectorA.length !== vectorB.length) {
                    throw new LitsError('Vectors must be of the same length', sourceCodeInfo);
                }
                return vectorA.reduce(function (acc, val, i) { return acc + Math.abs(val - vectorB[i]); }, 0) / vectorA.length;
            },
            paramCount: 2,
        },
    };

    /**
     * Calculates the determinant of a matrix using Gaussian Elimination
     * @param matrix A square matrix represented as a 2D array
     * @returns The determinant of the matrix
     */
    function determinant(matrix) {
        var _a;
        // First, make a deep copy of the matrix to avoid modifying the original
        var n = matrix.length;
        var A = [];
        for (var i = 0; i < n; i++) {
            A[i] = __spreadArray([], __read(matrix[i]), false);
        }
        // Handle special cases for small matrices
        if (n === 1) {
            return A[0][0];
        }
        if (n === 2) {
            return A[0][0] * A[1][1] - A[0][1] * A[1][0];
        }
        // For larger matrices, use Gaussian elimination
        var sign = 1; // Track sign changes from row swaps
        // Perform Gaussian elimination to get an upper triangular matrix
        for (var i = 0; i < n - 1; i += 1) {
            // Find pivot (maximum element in current column)
            var maxRow = i;
            for (var j = i + 1; j < n; j += 1) {
                if (Math.abs(A[j][i]) > Math.abs(A[maxRow][i])) {
                    maxRow = j;
                }
            }
            // If the pivot is zero, the determinant is zero
            if (Math.abs(A[maxRow][i]) < 1e-10) {
                return 0;
            }
            // Swap rows if necessary
            if (maxRow !== i) {
                _a = __read([A[maxRow], A[i]], 2), A[i] = _a[0], A[maxRow] = _a[1]; // ES6 array destructuring for swap
                sign = -sign; // Each row swap changes the sign
            }
            // Eliminate entries below the pivot
            for (var j = i + 1; j < n; j += 1) {
                var factor = A[j][i] / A[i][i];
                // Subtract (factor * pivot row) from current row
                for (var k = i; k < n; k++) {
                    A[j][k] -= factor * A[i][k];
                }
            }
        }
        // Calculate determinant as the product of diagonal elements
        var det = sign;
        for (var i = 0; i < n; i++) {
            det *= A[i][i];
        }
        return det;
    }

    function minor(matrix, row, col) {
        var n = matrix.length;
        var result = [];
        for (var i = 0; i < n; i++) {
            if (i !== row) {
                var minorRow = [];
                for (var j = 0; j < n; j++) {
                    if (j !== col) {
                        minorRow.push(matrix[i][j]);
                    }
                }
                result.push(minorRow);
            }
        }
        return result;
    }

    function adjugate(matrix) {
        var n = matrix.length;
        var adj = [];
        for (var i = 0; i < n; i++) {
            adj[i] = [];
            for (var j = 0; j < n; j++) {
                var min = minor(matrix, j, i);
                var sign = Math.pow((-1), (i + j));
                var cofactor = sign * determinant(min);
                adj[i][j] = cofactor;
            }
        }
        return adj;
    }

    /**
     * Creates a band matrix with specified lower and upper bandwidths
     *
     * @param n Size of the square matrix
     * @param lband Lower bandwidth (number of non-zero diagonals below main diagonal)
     * @param uband Upper bandwidth (number of non-zero diagonals above main diagonal)
     * @returns A 2D array representing the band matrix with 1s in the band and 0s elsewhere
     */
    function band(n, lband, uband) {
        // Create an n×n matrix filled with zeros
        var matrix = Array.from({ length: n }, function () { return Array.from({ length: n }, function () { return 0; }); });
        // Fill the band with 1s
        for (var i = 0; i < n; i++) {
            for (var j = Math.max(0, i - lband); j <= Math.min(n - 1, i + uband); j++) {
                matrix[i][j] = 1;
            }
        }
        return matrix;
    }

    function cofactor(matrix) {
        var n = matrix.length;
        var cofactors = [];
        // Create a new matrix to store cofactors
        for (var i = 0; i < n; i++) {
            cofactors[i] = [];
            for (var j = 0; j < n; j++) {
                // Get the minor by removing row i and column j
                var min = minor(matrix, i, j);
                var sign = Math.pow((-1), (i + j));
                cofactors[i][j] = sign * determinant(min);
            }
        }
        return cofactors;
    }

    /**
     * Performs Gauss-Jordan elimination on a matrix, transforming it to reduced row echelon form
     *
     * @param matrix - The input matrix
     * @returns A tuple containing the reduced row echelon form matrix and the rank
     */
    function gaussJordanElimination(matrix) {
        var _a;
        // Create a copy of the matrix to avoid modifying the original
        var m = matrix.map(function (row) { return __spreadArray([], __read(row), false); });
        var rows = m.length;
        var cols = m[0].length;
        // Tolerance for considering a value as zero
        var EPSILON = 1e-10;
        var rank = 0;
        var rowsProcessed = 0;
        // Row reduction to reduced row echelon form
        for (var col = 0; col < cols; col++) {
            // Find the pivot
            var pivotRow = -1;
            for (var row = rowsProcessed; row < rows; row++) {
                if (Math.abs(m[row][col]) > EPSILON) {
                    pivotRow = row;
                    break;
                }
            }
            if (pivotRow === -1)
                continue; // No pivot in this column
            // Increase rank
            rank += 1;
            // Swap rows
            if (pivotRow !== rowsProcessed) {
                _a = __read([m[rowsProcessed], m[pivotRow]], 2), m[pivotRow] = _a[0], m[rowsProcessed] = _a[1];
            }
            // Get the pivot value
            var pivotValue = m[rowsProcessed][col];
            // Normalize the pivot row (always, for RREF)
            for (var j = col; j < cols; j++) {
                m[rowsProcessed][j] /= pivotValue;
            }
            // Eliminate above and below (full Gauss-Jordan)
            for (var row = 0; row < rows; row++) {
                if (row !== rowsProcessed && Math.abs(m[row][col]) > EPSILON) {
                    var factor = m[row][col];
                    for (var j = col; j < cols; j++) {
                        m[row][j] -= factor * m[rowsProcessed][j];
                    }
                }
            }
            rowsProcessed++;
            if (rowsProcessed === rows)
                break;
        }
        return [m, rank];
    }

    /**
     * Calculate the inverse of a matrix using the adjugate method
     * @param matrix The input matrix
     * @returns The inverse matrix or null if the matrix is not invertible
     */
    var EPSILON = 1e-10;
    function inverse(matrix) {
        var n = matrix.length;
        // Special case for 1x1 matrix - handle it directly
        if (n === 1) {
            var element = matrix[0][0];
            if (Math.abs(element) < EPSILON) {
                return null; // Not invertible
            }
            return [[1 / element]];
        }
        // Calculate determinant
        var det = determinant(matrix);
        // Check if matrix is invertible
        if (Math.abs(det) < EPSILON) {
            return null; // Matrix is not invertible
        }
        // Get the adjugate matrix
        var adj = adjugate(matrix);
        // Calculate the inverse: inverse = adjugate / determinant
        var inverseMatrix = [];
        for (var i = 0; i < n; i++) {
            inverseMatrix[i] = [];
            for (var j = 0; j < n; j++) {
                inverseMatrix[i][j] = adj[i][j] / det;
            }
        }
        return inverseMatrix;
    }

    /**
     * Checks if a matrix is banded with the given lower and upper bandwidth.
     * A matrix is banded if all non-zero elements are within 'lower' diagonals
     * below the main diagonal and 'upper' diagonals above the main diagonal.
     *
     * @param matrix - The matrix to check, represented as a 2D array of numbers
     * @param lower - Number of non-zero diagonals below the main diagonal
     * @param upper - Number of non-zero diagonals above the main diagonal
     * @returns true if the matrix is banded with the given parameters, false otherwise
     */
    function isBanded(matrix, lower, upper) {
        var rows = matrix.length;
        var cols = matrix[0].length;
        // Check each element in the matrix
        for (var i = 0; i < rows; i++) {
            for (var j = 0; j < cols; j++) {
                // If we find a non-zero element outside the band, return false
                if (matrix[i][j] !== 0 && (j < i - lower || j > i + upper)) {
                    return false;
                }
            }
        }
        // All elements outside the band are zero
        return true;
    }

    function isSquare(matrix) {
        return matrix.length === matrix[0].length;
    }

    /**
     * Checks if a given matrix is diagonal.
     *
     * A matrix is considered diagonal if it is square (i.e., the number of rows equals the number of columns)
     * and all elements outside the main diagonal are zero.
     *
     * @param matrix - A two-dimensional array of numbers representing the matrix to check.
     * @returns `true` if the matrix is diagonal, otherwise `false`.
     */
    function isDiagonal(matrix) {
        if (!isSquare(matrix)) {
            return false;
        }
        var rows = matrix.length;
        for (var i = 0; i < rows; i += 1) {
            for (var j = 0; j < rows; j += 1) {
                if (i !== j && matrix[i][j] !== 0) {
                    return false;
                }
            }
        }
        return true;
    }

    var epsilon = 1e-10;
    function isIdentity(matrix) {
        if (!isSquare(matrix)) {
            return false;
        }
        var n = matrix.length;
        for (var i = 0; i < n; i++) {
            for (var j = 0; j < n; j++) {
                if (i === j) {
                    if (Math.abs(matrix[i][j] - 1) > epsilon) {
                        return false;
                    }
                }
                else {
                    if (Math.abs(matrix[i][j]) > epsilon) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    /**
     * Performs cache-optimized matrix multiplication.
     * @param A The first input matrix (m x n)
     * @param B The second input matrix (n x p)
     * @returns The result matrix C (m x p) where C = A × B
     */
    function multiply(A, B) {
        // Check if matrices can be multiplied
        if (A.length === 0 || B.length === 0 || A[0].length !== B.length) {
            throw new Error('Matrix dimensions do not match for multiplication');
        }
        var m = A.length; // Number of rows in A
        var n = A[0].length; // Number of columns in A / Number of rows in B
        var p = B[0].length; // Number of columns in B
        // Initialize result matrix C with zeros
        var C = Array(m).fill(0).map(function () { return Array(p).fill(0); });
        // Perform multiplication with cache-optimized loop order (i-k-j)
        for (var i = 0; i < m; i++) {
            for (var k = 0; k < n; k++) {
                var aik = A[i][k]; // Cache this value to avoid repeated lookups
                for (var j = 0; j < p; j++) {
                    C[i][j] += aik * B[k][j];
                }
            }
        }
        return C;
    }

    function isOrthogonal(matrix) {
        if (!isSquare(matrix)) {
            return false;
        }
        // Calculate matrix transpose
        var transposed = transpose(matrix);
        // Check if matrix * transpose = Identity
        var product = multiply(matrix, transposed);
        if (!product) {
            return false;
        }
        // Check if the product is an identity matrix
        return isIdentity(product);
    }

    /**
     * Checks if a given matrix is symmetric.
     * A matrix is symmetric if it is square and its transpose is equal to itself.
     *
     * @param matrix - A 2D array representing the matrix.
     * @returns `true` if the matrix is symmetric, otherwise `false`.
     */
    function isSymetric(matrix) {
        var rows = matrix.length;
        // Check if the matrix is square
        if (!isSquare(matrix)) {
            return false;
        }
        // Check symmetry
        for (var i = 0; i < rows; i += 1) {
            for (var j = 0; j < i; j += 1) {
                if (matrix[i][j] !== matrix[j][i]) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Determines whether a given matrix is triangular.
     *
     * A triangular matrix is a square matrix where all elements
     * below or above the main diagonal are zero. This function
     * checks if the matrix is square and symmetric.
     *
     * @param matrix - A two-dimensional array of numbers representing the matrix.
     * @returns `true` if the matrix is triangular, otherwise `false`.
     */
    function isTriangular$1(matrix) {
        if (!isSquare(matrix)) {
            return false;
        }
        var rows = matrix.length;
        var isUpperTriangular = true;
        var isLowerTriangular = true;
        for (var i = 0; i < rows; i++) {
            for (var j = 0; j < rows; j++) {
                if (i > j && matrix[i][j] !== 0) {
                    isUpperTriangular = false;
                    if (!isLowerTriangular) {
                        return false;
                    }
                }
                if (i < j && matrix[i][j] !== 0) {
                    isLowerTriangular = false;
                    if (!isUpperTriangular) {
                        return false;
                    }
                }
            }
        }
        return isUpperTriangular || isLowerTriangular;
    }
    function isTriangularUpper(matrix) {
        if (!isSquare(matrix)) {
            return false;
        }
        var rows = matrix.length;
        for (var i = 0; i < rows; i++) {
            for (var j = 0; j < i; j++) {
                if (matrix[i][j] !== 0) {
                    return false;
                }
            }
        }
        return true;
    }
    function isTriangularLower(matrix) {
        if (!isSquare(matrix)) {
            return false;
        }
        var rows = matrix.length;
        // Check if the matrix is square
        if (!matrix.every(function (row) { return row.length === rows; })) {
            return false;
        }
        for (var i = 0; i < rows; i++) {
            for (var j = i + 1; j < rows; j++) {
                if (matrix[i][j] !== 0) {
                    return false;
                }
            }
        }
        return true;
    }

    // Assuming a matrix is represented as a 2D array
    function norm1(matrix) {
        var numRows = matrix.length;
        var numCols = matrix[0].length;
        var maxColSum = 0;
        // Iterate through each column
        for (var j = 0; j < numCols; j += 1) {
            var colSum = 0;
            // Sum the absolute values of all elements in this column
            for (var i = 0; i < numRows; i += 1) {
                colSum += Math.abs(matrix[i][j]);
            }
            // Update the maximum column sum if necessary
            maxColSum = Math.max(maxColSum, colSum);
        }
        return maxColSum;
    }

    function identity(number) {
        var result = [];
        for (var i = 0; i < number; i += 1) {
            var row = [];
            for (var j = 0; j < number; j += 1) {
                row.push(i === j ? 1 : 0);
            }
            result.push(row);
        }
        return result;
    }

    /**
     * Compute the nth power of a square matrix
     *
     * @param A - A square matrix
     * @param n - The power to raise the matrix to
     * @returns The matrix A raised to power n
     */
    function pow(A, n) {
        var rows = A.length;
        var result = identity(rows);
        // Handle special cases
        if (n === 0) {
            return result;
        }
        if (n < 0) {
            var inverseA = inverse(A);
            if (!inverseA) {
                throw new Error('Matrix is not invertible');
            }
            A = inverseA;
            n = -n;
        }
        // Binary exponentiation method (faster than naive repeated multiplication)
        var power = A.map(function (row) { return __spreadArray([], __read(row), false); }); // Create a deep copy of A
        while (n > 0) {
            if (n % 2 === 1) {
                result = multiply(result, power);
            }
            power = multiply(power, power);
            n = Math.floor(n / 2);
        }
        return result;
    }

    /**
     * Solves a system of linear equations Ax = b
     *
     * @param A - The coefficient matrix
     * @param b - The constant vector
     * @returns The solution vector x, or null if no unique solution exists
     */
    function solve(A, b) {
        var n = A.length;
        // Create augmented matrix [A|b]
        var augmented = A.map(function (row, i) { return __spreadArray(__spreadArray([], __read(row), false), [b[i]], false); });
        // Convert to row echelon form using your existing function
        var _a = __read(gaussJordanElimination(augmented), 1), echelon = _a[0];
        // Check if the system has a unique solution
        for (var i = 0; i < n; i += 1) {
            if (Math.abs(echelon[i][i]) < 1e-10) {
                return null; // No unique solution
            }
        }
        // Back substitution
        var x = Array.from({ length: n }, function () { return 0; });
        for (var i = n - 1; i >= 0; i--) {
            var sum = 0;
            for (var j = i + 1; j < n; j++) {
                sum += echelon[i][j] * x[j];
            }
            x[i] = (echelon[i][n] - sum) / echelon[i][i];
        }
        return x;
    }

    /**
     * Calculates the trace of a square matrix.
     * The trace is defined as the sum of the elements on the main diagonal.
     *
     * @param matrix - A 2D array representing a square matrix.
     * @returns The trace of the matrix.
     */
    function trace(matrix) {
        return matrix.reduce(function (sum, row, i) { return sum + row[i]; }, 0);
    }

    function isMatrix(matrix) {
        var e_1, _a;
        if (!Array.isArray(matrix)) {
            return false;
        }
        if (matrix.length === 0) {
            return false;
        }
        if (!Array.isArray(matrix[0])) {
            return false;
        }
        var nbrOfCols = matrix[0].length;
        try {
            for (var _b = __values(matrix.slice(1)), _c = _b.next(); !_c.done; _c = _b.next()) {
                var row = _c.value;
                if (!Array.isArray(row)) {
                    return false;
                }
                if (row.length !== nbrOfCols) {
                    return false;
                }
                if (row.some(function (cell) { return !isNumber(cell, { finite: true }); })) {
                    return false;
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
        return true;
    }
    function assertMatrix(matrix, sourceCodeInfo) {
        if (!isMatrix(matrix)) {
            throw new LitsError("Expected a matrix, but got ".concat(matrix), sourceCodeInfo);
        }
    }
    function assertSquareMatrix(matrix, sourceCodeInfo) {
        if (!isMatrix(matrix)) {
            throw new LitsError("Expected a matrix, but got ".concat(matrix), sourceCodeInfo);
        }
        if (matrix.length !== matrix[0].length) {
            throw new LitsError("Expected square matrix, but got ".concat(matrix.length, " and ").concat(matrix[0].length), sourceCodeInfo);
        }
    }
    var matrixNormalExpression = {
        'm:matrix?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), matrix = _b[0];
                return isMatrix(matrix);
            },
            paramCount: 1,
        },
        'm:~': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), m1 = _b[0], m2 = _b[1], _c = _b[2], epsilon = _c === void 0 ? 1e-10 : _c;
                assertMatrix(m1, sourceCodeInfo);
                assertMatrix(m2, sourceCodeInfo);
                assertNumber(epsilon, sourceCodeInfo);
                if (m1.length !== m2.length) {
                    return false;
                }
                if (m1[0].length !== m2[0].length) {
                    return false;
                }
                for (var i = 0; i < m1.length; i += 1) {
                    for (var j = 0; j < m1[i].length; j += 1) {
                        if (Math.abs(m1[i][j] - m2[i][j]) > epsilon) {
                            return false;
                        }
                    }
                }
                return true;
            },
            paramCount: { min: 2, max: 3 },
        },
        'm:scale': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), matrix = _b[0], scalar = _b[1];
                assertMatrix(matrix, sourceCodeInfo);
                assertNumber(scalar, sourceCodeInfo);
                return matrix.map(function (row) { return row.map(function (cell) { return cell * scalar; }); });
            },
            paramCount: 2,
        },
        'm:+': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), firstMatrix = _b[0], params = _b.slice(1);
                assertMatrix(firstMatrix, sourceCodeInfo);
                assertArray(params, sourceCodeInfo);
                params.forEach(function (matrix) {
                    assertMatrix(matrix, sourceCodeInfo);
                    if (matrix.length !== firstMatrix.length) {
                        throw new LitsError("All matrices must have the same number of rows, but got ".concat(firstMatrix.length, " and ").concat(matrix.length), sourceCodeInfo);
                    }
                    if (matrix[0].length !== firstMatrix[0].length) {
                        throw new LitsError("All matrices must have the same number of columns, but got ".concat(firstMatrix[0].length, " and ").concat(matrix[0].length), sourceCodeInfo);
                    }
                });
                if (params.length === 0) {
                    return params[0];
                }
                var matrices = params;
                var result = [];
                var _loop_1 = function (i) {
                    var row = [];
                    var _loop_2 = function (j) {
                        var sum = firstMatrix[i][j];
                        matrices.forEach(function (matrix) {
                            sum += matrix[i][j];
                        });
                        row.push(sum);
                    };
                    for (var j = 0; j < firstMatrix[i].length; j += 1) {
                        _loop_2(j);
                    }
                    result.push(row);
                };
                for (var i = 0; i < firstMatrix.length; i += 1) {
                    _loop_1(i);
                }
                return result;
            },
            paramCount: { min: 1 },
        },
        'm:-': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), firstMatrix = _b[0], params = _b.slice(1);
                assertMatrix(firstMatrix, sourceCodeInfo);
                assertArray(params, sourceCodeInfo);
                params.forEach(function (matrix) {
                    assertMatrix(matrix, sourceCodeInfo);
                    if (matrix.length !== firstMatrix.length) {
                        throw new LitsError("All matrices must have the same number of rows, but got ".concat(firstMatrix.length, " and ").concat(matrix.length), sourceCodeInfo);
                    }
                    if (matrix[0].length !== firstMatrix[0].length) {
                        throw new LitsError("All matrices must have the same number of columns, but got ".concat(firstMatrix[0].length, " and ").concat(matrix[0].length), sourceCodeInfo);
                    }
                });
                if (params.length === 0) {
                    var result_1 = [];
                    for (var i = 0; i < firstMatrix.length; i += 1) {
                        var row = [];
                        for (var j = 0; j < firstMatrix[i].length; j += 1) {
                            row.push(-firstMatrix[i][j]);
                        }
                        result_1.push(row);
                    }
                }
                var matrices = params;
                var result = [];
                var _loop_3 = function (i) {
                    var row = [];
                    var _loop_4 = function (j) {
                        var sum = firstMatrix[i][j];
                        matrices.forEach(function (matrix) {
                            sum -= matrix[i][j];
                        });
                        row.push(sum);
                    };
                    for (var j = 0; j < firstMatrix[i].length; j += 1) {
                        _loop_4(j);
                    }
                    result.push(row);
                };
                for (var i = 0; i < firstMatrix.length; i += 1) {
                    _loop_3(i);
                }
                return result;
            },
            paramCount: { min: 1 },
        },
        'm:*': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), firstMatrix = _b[0], params = _b.slice(1);
                assertMatrix(firstMatrix, sourceCodeInfo);
                assertArray(params, sourceCodeInfo);
                params.forEach(function (matrix) {
                    assertMatrix(matrix, sourceCodeInfo);
                    if (matrix.length !== firstMatrix.length) {
                        throw new LitsError("All matrices must have the same number of rows, but got ".concat(firstMatrix.length, " and ").concat(matrix.length), sourceCodeInfo);
                    }
                    if (matrix[0].length !== firstMatrix[0].length) {
                        throw new LitsError("All matrices must have the same number of columns, but got ".concat(firstMatrix[0].length, " and ").concat(matrix[0].length), sourceCodeInfo);
                    }
                });
                if (params.length === 0) {
                    return params[0];
                }
                var matrices = params;
                var result = [];
                var _loop_5 = function (i) {
                    var row = [];
                    var _loop_6 = function (j) {
                        var prod = firstMatrix[i][j];
                        matrices.forEach(function (matrix) {
                            prod *= matrix[i][j];
                        });
                        row.push(prod);
                    };
                    for (var j = 0; j < firstMatrix[i].length; j += 1) {
                        _loop_6(j);
                    }
                    result.push(row);
                };
                for (var i = 0; i < firstMatrix.length; i += 1) {
                    _loop_5(i);
                }
                return result;
            },
            paramCount: { min: 1 },
        },
        'm:/': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), firstMatrix = _b[0], params = _b.slice(1);
                assertMatrix(firstMatrix, sourceCodeInfo);
                assertArray(params, sourceCodeInfo);
                params.forEach(function (matrix) {
                    assertMatrix(matrix, sourceCodeInfo);
                    if (matrix.length !== firstMatrix.length) {
                        throw new LitsError("All matrices must have the same number of rows, but got ".concat(firstMatrix.length, " and ").concat(matrix.length), sourceCodeInfo);
                    }
                    if (matrix[0].length !== firstMatrix[0].length) {
                        throw new LitsError("All matrices must have the same number of columns, but got ".concat(firstMatrix[0].length, " and ").concat(matrix[0].length), sourceCodeInfo);
                    }
                });
                if (params.length === 0) {
                    var result_2 = [];
                    for (var i = 0; i < firstMatrix.length; i += 1) {
                        var row = [];
                        for (var j = 0; j < firstMatrix[i].length; j += 1) {
                            row.push(1 / firstMatrix[i][j]);
                        }
                        result_2.push(row);
                    }
                }
                var matrices = params;
                var result = [];
                var _loop_7 = function (i) {
                    var row = [];
                    var _loop_8 = function (j) {
                        var prod = firstMatrix[i][j];
                        matrices.forEach(function (matrix) {
                            prod /= matrix[i][j];
                        });
                        row.push(prod);
                    };
                    for (var j = 0; j < firstMatrix[i].length; j += 1) {
                        _loop_8(j);
                    }
                    result.push(row);
                };
                for (var i = 0; i < firstMatrix.length; i += 1) {
                    _loop_7(i);
                }
                return result;
            },
            paramCount: { min: 1 },
        },
        'm:**': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), matrix = _b[0], power = _b[1];
                assertSquareMatrix(matrix, sourceCodeInfo);
                assertNumber(power, sourceCodeInfo, { integer: true });
                return pow(matrix, power);
            },
            paramCount: 2,
        },
        'm:dot': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), matrix1 = _b[0], matrix2 = _b[1];
                assertMatrix(matrix1, sourceCodeInfo);
                assertMatrix(matrix2, sourceCodeInfo);
                try {
                    return multiply(matrix1, matrix2);
                }
                catch (error) {
                    throw new LitsError("The number of columns in the first matrix must be equal to the number of rows in the second matrix, but got ".concat(matrix1[0].length, " and ").concat(matrix2.length), sourceCodeInfo);
                }
            },
            paramCount: 2,
        },
        'm:determinant': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertSquareMatrix(matrix, sourceCodeInfo);
                return determinant(matrix);
            },
            paramCount: 1,
        },
        'm:inverse': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertSquareMatrix(matrix, sourceCodeInfo);
                var result = inverse(matrix);
                if (result === null) {
                    throw new LitsError('The matrix must be invertible', sourceCodeInfo);
                }
                return result;
            },
            paramCount: 1,
        },
        'm:adjugate': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertSquareMatrix(matrix, sourceCodeInfo);
                return adjugate(matrix);
            },
            paramCount: 1,
        },
        'm:cofactor': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertSquareMatrix(matrix, sourceCodeInfo);
                return cofactor(matrix);
            },
            paramCount: 1,
        },
        'm:minor': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), matrix = _b[0], row = _b[1], col = _b[2];
                assertMatrix(matrix, sourceCodeInfo);
                assertNumber(row, sourceCodeInfo, { integer: true, nonNegative: true, lte: matrix.length });
                assertNumber(col, sourceCodeInfo, { integer: true, nonNegative: true, lte: matrix[0].length });
                return minor(matrix, row, col);
            },
            paramCount: 3,
        },
        'm:trace': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertSquareMatrix(matrix, sourceCodeInfo);
                return trace(matrix);
            },
            paramCount: 1,
        },
        'm:symmetric?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertMatrix(matrix, sourceCodeInfo);
                return isSymetric(matrix);
            },
            paramCount: 1,
        },
        'm:triangular?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertMatrix(matrix, sourceCodeInfo);
                return isTriangular$1(matrix);
            },
            paramCount: 1,
        },
        'm:upper-triangular?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertMatrix(matrix, sourceCodeInfo);
                return isTriangularUpper(matrix);
            },
            paramCount: 1,
        },
        'm:lower-triangular?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertMatrix(matrix, sourceCodeInfo);
                return isTriangularLower(matrix);
            },
            paramCount: 1,
        },
        'm:diagonal?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertMatrix(matrix, sourceCodeInfo);
                return isDiagonal(matrix);
            },
            paramCount: 1,
        },
        'm:square?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertMatrix(matrix, sourceCodeInfo);
                return isSquare(matrix);
            },
            paramCount: 1,
        },
        'm:orthogonal?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertMatrix(matrix, sourceCodeInfo);
                return isOrthogonal(matrix);
            },
            paramCount: 1,
        },
        'm:identity?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertMatrix(matrix, sourceCodeInfo);
                return isIdentity(matrix);
            },
            paramCount: 1,
        },
        'm:singular?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertSquareMatrix(matrix, sourceCodeInfo);
                return determinant(matrix) < 1e-10;
            },
            paramCount: 1,
        },
        'm:rref': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertMatrix(matrix, sourceCodeInfo);
                // Reduced Row Echelon Form (RREF)
                var _c = __read(gaussJordanElimination(matrix), 1), rref = _c[0];
                return rref;
            },
            paramCount: 1,
        },
        'm:rank': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertMatrix(matrix, sourceCodeInfo);
                var _c = __read(gaussJordanElimination(matrix), 2), result = _c[1];
                return result;
            },
            paramCount: 1,
        },
        'm:solve': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), matrix = _b[0], vector = _b[1];
                assertSquareMatrix(matrix, sourceCodeInfo);
                assertVector(vector, sourceCodeInfo);
                if (matrix.length !== vector.length) {
                    throw new LitsError("The number of rows in the matrix must be equal to the length of the vector, but got ".concat(matrix.length, " and ").concat(vector.length), sourceCodeInfo);
                }
                return solve(matrix, vector);
            },
            paramCount: 2,
        },
        // Frobenius norm
        'm:norm-frobenius': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertMatrix(matrix, sourceCodeInfo);
                return Math.sqrt(matrix.reduce(function (sum, row) { return sum + row.reduce(function (rowSum, cell) { return rowSum + cell * cell; }, 0); }, 0));
            },
            paramCount: 1,
        },
        // 1-norm
        'm:norm-1': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertMatrix(matrix, sourceCodeInfo);
                return norm1(matrix);
            },
            paramCount: 1,
        },
        // Infinity norm
        'm:norm-infinity': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertMatrix(matrix, sourceCodeInfo);
                return matrix.reduce(function (max, row) { return Math.max(max, row.reduce(function (sum, cell) { return sum + Math.abs(cell); }, 0)); }, 0);
            },
            paramCount: 1,
        },
        // Max norm
        'm:norm-max': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), matrix = _b[0];
                assertMatrix(matrix, sourceCodeInfo);
                return matrix.reduce(function (maxVal, row) {
                    var rowMax = row.reduce(function (max, val) { return Math.max(max, Math.abs(val)); }, 0);
                    return Math.max(maxVal, rowMax);
                }, 0);
            },
            paramCount: 1,
        },
        'm:hilbert': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), size = _b[0];
                assertNumber(size, sourceCodeInfo, { integer: true, positive: true });
                var result = [];
                for (var i = 0; i < size; i += 1) {
                    var row = [];
                    for (var j = 0; j < size; j += 1) {
                        row.push(1 / (i + j + 1));
                    }
                    result.push(row);
                }
                return result;
            },
            paramCount: 1,
        },
        'm:vandermonde': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), vector = _b[0];
                assertVector(vector, sourceCodeInfo);
                var result = [];
                for (var i = 0; i < vector.length; i += 1) {
                    var row = [];
                    for (var j = 0; j < vector.length; j += 1) {
                        row.push(Math.pow((vector[i]), j));
                    }
                    result.push(row);
                }
                return result;
            },
            paramCount: 1,
        },
        'm:band': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), n = _b[0], lband = _b[1], uband = _b[2];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                assertNumber(lband, sourceCodeInfo, { integer: true, nonNegative: true, lt: n });
                assertNumber(uband, sourceCodeInfo, { integer: true, nonNegative: true, lte: n });
                return band(n, lband, uband);
            },
            paramCount: 3,
        },
        'm:banded?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), matrix = _b[0], lband = _b[1], uband = _b[2];
                assertMatrix(matrix, sourceCodeInfo);
                var maxBand = Math.max(matrix.length, matrix[0].length);
                assertNumber(lband, sourceCodeInfo, { integer: true, nonNegative: true, lt: maxBand });
                assertNumber(uband, sourceCodeInfo, { integer: true, nonNegative: true, lt: maxBand });
                return isBanded(matrix, lband, uband);
            },
            paramCount: 1,
        },
    };

    // Pre-calculated Bell numbers (for efficient lookup)
    // Only including values up to the safe integer limit in JavaScript
    var bellNumbers = [
        1,
        1,
        2,
        5,
        15,
        52,
        203,
        877,
        4140,
        21147,
        115975,
        678570,
        4213597,
        27644437,
        190899322,
        1382958545,
        10480142147,
        82864869804,
        682076806159,
        5832742205057,
        51724158235372,
        474869816156751,
        4506715738447323,
    ];
    var bellSequence = {
        'maxLength': bellNumbers.length,
        'c:bell-seq': function (length) {
            return bellNumbers.slice(0, length);
        },
        'c:bell-nth': function (n) { return bellNumbers[n - 1]; },
        'c:bell?': function (n) { return bellNumbers.includes(n); },
        'c:bell-take-while': function (takeWhile) {
            var bell = [];
            for (var i = 0;; i += 1) {
                if (i >= bellNumbers.length) {
                    break;
                }
                var value = bellNumbers[i];
                if (!takeWhile(value, i)) {
                    break;
                }
                bell[i] = value;
            }
            return bell;
        },
    };

    var catalanNumbers = [
        1,
        1,
        2,
        5,
        14,
        42,
        132,
        429,
        1430,
        4862,
        16796,
        58786,
        208012,
        742900,
        2674440,
        9694845,
        35357670,
        129644790,
        477638700,
        1767263190,
        6564120420,
        24466267020,
        91482563640,
        343059613650,
        1289904147324,
        4861946401452,
        18367353072152,
        69533550916004,
        263747951750360,
        1002242216651368,
        3814986502092304,
    ];
    var catalanSequence = {
        'maxLength': catalanNumbers.length,
        'c:catalan-seq': function (length) {
            return catalanNumbers.slice(0, length);
        },
        'c:catalan-nth': function (n) { return catalanNumbers[n - 1]; },
        'c:catalan?': function (n) { return catalanNumbers.includes(n); },
        'c:catalan-take-while': function (takeWhile) {
            var catalan = [];
            for (var i = 0;; i += 1) {
                if (i >= catalanNumbers.length) {
                    break;
                }
                var value = catalanNumbers[i];
                if (!takeWhile(value, i)) {
                    break;
                }
                catalan[i] = value;
            }
            return catalan;
        },
    };

    /**
     * Checks if a number is part of the Look-and-Say sequence.
     *
     * The Look-and-Say sequence starts with "1" and each subsequent term describes
     * the previous term by counting consecutive digits. For example:
     * 1, 11, 21, 1211, 111221, 312211, 13112221, ...
     *
     * @param {string|number} target - The number to check (can be a string or number)
     * @param {number} limit - How many terms to generate for checking (default: 25)
     * @returns {boolean} - Whether the target is in the sequence
     */
    function isLookAndSay(target, limit) {
        if (limit === void 0) { limit = 25; }
        // The first term of the sequence
        var current = '1';
        // Check if the first term matches
        if (current === target) {
            return true;
        }
        // Generate terms and check against the target
        for (var i = 1; i < limit; i++) {
            current = getNextLookAndSayTerm(current);
            if (current === target) {
                return true;
            }
            // Optimization: if the current term is longer than the target, and
            // the sequence is strictly increasing in length, the target won't be found
            if (current.length > target.length) {
                return false;
            }
        }
        // If we've generated 'limit' terms without finding a match, return false
        return false;
    }
    /**
     * Generates the next term in the Look-and-Say sequence
     *
     * @param {string} term - The current term
     * @returns {string} - The next term in the sequence
     */
    function getNextLookAndSayTerm(term) {
        var result = '';
        var count = 1;
        for (var i = 0; i < term.length; i++) {
            // If the current digit is the same as the next one, increment count
            if (i + 1 < term.length && term[i] === term[i + 1]) {
                count++;
            }
            else {
                // Otherwise, append count and the digit to the result
                result += count.toString() + term[i];
                count = 1;
            }
        }
        return result;
    }
    var lookAndSaySequence = {
        'string': true,
        'c:look-and-say-seq': function (length) {
            var lookAndSay = ['1'];
            for (var i = 1; i < length; i += 1) {
                var prev = lookAndSay[i - 1];
                var next = prev.replace(/(\d)\1*/g, function (match) { return "".concat(match.length).concat(match[0]); });
                lookAndSay[i] = next;
            }
            return lookAndSay;
        },
        'c:look-and-say-take-while': function (takeWhile) {
            if (!takeWhile('1', 0)) {
                return [];
            }
            var lookAndSay = ['1'];
            for (var i = 1;; i += 1) {
                var prev = lookAndSay[i - 1];
                var next = prev.replace(/(\d)\1*/g, function (match) { return "".concat(match.length).concat(match[0]); });
                if (!takeWhile(next, i)) {
                    break;
                }
                lookAndSay[i] = next;
            }
            return lookAndSay;
        },
        'c:look-and-say-nth': function (n) {
            var lookAndSay = '1';
            for (var i = 1; i < n; i += 1) {
                lookAndSay = lookAndSay.replace(/(\d)\1*/g, function (match) { return "".concat(match.length).concat(match[0]); });
            }
            return lookAndSay;
        },
        'c:look-and-say?': function (n) { return isLookAndSay(n); },
    };

    /**
     * Checks if a number is a Padovan number.
     * Padovan numbers follow the recurrence relation: P(n) = P(n-2) + P(n-3) for n >= 3,
     * with initial values P(0) = P(1) = P(2) = 1.
     *
     * The first few Padovan numbers are:
     * 1, 1, 1, 2, 2, 3, 4, 5, 7, 9, 12, 16, 21, 28, 37, 49, 65, 86, 114, 151, 200, ...
     *
     * @param num - The number to check
     * @returns True if the number is a Padovan number, false otherwise
     */
    function isPadovan(num) {
        // Padovan numbers are always positive integers
        if (!Number.isInteger(num) || num <= 0) {
            return false;
        }
        // Special case: The first three Padovan numbers are all 1
        if (num === 1) {
            return true;
        }
        // Pre-calculated Padovan numbers (for efficient lookup, verified for correctness)
        var padovanNumbers = [
            1,
            1,
            1,
            2,
            2,
            3,
            4,
            5,
            7,
            9,
            12,
            16,
            21,
            28,
            37,
            49,
            65,
            86,
            114,
            151,
            200,
            265,
            351,
            465,
            616,
            816,
            1081,
            1432,
            1897,
            2513,
            3329,
            4410,
            5842,
            7739,
            10252,
            13581,
            17991,
            23833,
            31572,
            41824,
            55405,
            73396,
            97229,
            128801,
            170625,
            226030,
            299426,
            396655,
            525456,
            696081,
            922111,
        ];
        // Direct lookup for known values
        if (padovanNumbers.includes(num)) {
            return true;
        }
        // For numbers larger than our pre-calculated list but within JavaScript's safe range
        if (num > padovanNumbers[padovanNumbers.length - 1] && num <= Number.MAX_SAFE_INTEGER) {
            // Start with the last three values from our known sequence
            var a = padovanNumbers[padovanNumbers.length - 3];
            var b = padovanNumbers[padovanNumbers.length - 2];
            var c = padovanNumbers[padovanNumbers.length - 1];
            var next 
            // Generate Padovan numbers until we either find a match or exceed the input
            = void 0;
            // Generate Padovan numbers until we either find a match or exceed the input
            while (c < num) {
                next = a + b;
                a = b;
                b = c;
                c = next;
                if (c === num) {
                    return true;
                }
                // Check for numeric overflow/precision issues
                if (!Number.isSafeInteger(c)) {
                    return false;
                }
            }
        }
        return false;
    }
    var padovanSequence = {
        'c:padovan-seq': function (length) {
            var padovan = [1, 1, 1];
            for (var i = 3; i < length; i += 1) {
                padovan[i] = padovan[i - 2] + padovan[i - 3];
            }
            return padovan.slice(0, length);
        },
        'c:padovan-nth': function (n) {
            if (n === 1 || n === 2 || n === 3)
                return 1;
            var a = 1;
            var b = 1;
            var c = 1;
            for (var i = 4; i <= n; i += 1) {
                var temp = a + b;
                a = b;
                b = c;
                c = temp;
            }
            return c;
        },
        'c:padovan?': function (n) { return isPadovan(n); },
        'c:padovan-take-while': function (takeWhile) {
            var padovan = [];
            if (!takeWhile(1, 0)) {
                return padovan;
            }
            padovan.push(1);
            if (!takeWhile(1, 1)) {
                return padovan;
            }
            padovan.push(1);
            if (!takeWhile(1, 2)) {
                return padovan;
            }
            padovan.push(1);
            var a = 1;
            var b = 1;
            var c = 1;
            for (var i = 4;; i += 1) {
                var temp = a + b;
                a = b;
                b = c;
                c = temp;
                if (!takeWhile(c, i)) {
                    break;
                }
                padovan.push(c);
            }
            return padovan;
        },
    };

    var pellNumbers = [
        0,
        1,
        2,
        5,
        12,
        29,
        70,
        169,
        408,
        985,
        2378,
        5741,
        13860,
        33461,
        80782,
        195025,
        470832,
        1136689,
        2744210,
        6625109,
        15994428,
        38613965,
        93222358,
        225058681,
        543339720,
        1311738121,
        3166815962,
        7645370045,
        18457556052,
        44560482149,
        107578520350,
        259717522849,
        627013566048,
        1513744654945,
        3654502875938,
        8822750406821,
        21300003689580,
        51422757785981,
        124145519261542,
        299713796309065,
        723573111879672,
        1746860020068409,
        4217293152016490,
    ];
    var pellSequence = {
        'maxLength': pellNumbers.length,
        'c:pell-seq': function (length) {
            return pellNumbers.slice(0, length);
        },
        'c:pell-nth': function (n) { return pellNumbers[n - 1]; },
        'c:pell?': function (n) { return pellNumbers.includes(n); },
        'c:pell-take-while': function (takeWhile) {
            var pell = [];
            for (var i = 0;; i += 1) {
                if (i >= pellNumbers.length) {
                    break;
                }
                var value = pellNumbers[i];
                if (!takeWhile(value, i)) {
                    break;
                }
                pell[i] = value;
            }
            return pell;
        },
    };

    /**
     * Generates the first 'n' terms of the Recamán sequence.
     *
     * @param n - Number of terms to generate
     * @returns Array containing the first n terms of the Recamán sequence
     */
    function generateRecamanSequence(n) {
        if (n === 1)
            return [0];
        var sequence = [0];
        var seen = new Set([0]);
        for (var i = 1; i < n; i++) {
            // Try to go backward
            var next = sequence[i - 1] - i;
            // If that's not positive or already seen, go forward
            if (next <= 0 || seen.has(next)) {
                next = sequence[i - 1] + i;
            }
            sequence.push(next);
            seen.add(next);
        }
        return sequence;
    }
    var recamanSequence = {
        'c:recaman-seq': function (length) { return generateRecamanSequence(length); },
        'c:recaman-take-while': function (takeWhile) {
            if (!takeWhile(0, 0))
                return [];
            var sequence = [0];
            var seen = new Set([0]);
            for (var i = 1;; i++) {
                // Try to go backward
                var next = sequence[i - 1] - i;
                // If that's not positive or already seen, go forward
                if (next <= 0 || seen.has(next)) {
                    next = sequence[i - 1] + i;
                }
                if (!takeWhile(next, i))
                    break;
                sequence.push(next);
                seen.add(next);
            }
            return sequence;
        },
        'c:recaman-nth': function (n) { var _a; return (_a = generateRecamanSequence(n)[n - 1]) !== null && _a !== void 0 ? _a : 0; },
        'c:recaman?': function () { return true; },
    };

    var thueMorseSequence = {
        'c:thue-morse-seq': function (length) {
            var thueMorse = [];
            for (var i = 0; i < length; i += 1) {
                thueMorse[i] = countSetBits(i) % 2;
            }
            return thueMorse;
        },
        'c:thue-morse-take-while': function (takeWhile) {
            var thueMorse = [];
            for (var i = 0;; i += 1) {
                var value = countSetBits(i) % 2;
                if (!takeWhile(value, i)) {
                    break;
                }
                thueMorse[i] = value;
            }
            return thueMorse;
        },
        'c:thue-morse-nth': function (n) { return countSetBits(n - 1) % 2; },
        'c:thue-morse?': function (n) { return n === 1 || n === 0; },
    };
    function countSetBits(num) {
        var count = 0;
        while (num) {
            count += num & 1;
            num >>= 1;
        }
        return count;
    }

    var sequenceNormalExpression = {};
    addSequence(bellSequence);
    addSequence(catalanSequence);
    addSequence(lookAndSaySequence);
    addSequence(padovanSequence);
    addSequence(pellSequence);
    addSequence(recamanSequence);
    addSequence(thueMorseSequence);
    function addSequence(sequence) {
        var e_1, _a;
        try {
            for (var _b = __values(Object.entries(sequence)), _c = _b.next(); !_c.done; _c = _b.next()) {
                var _d = __read(_c.value, 2), key = _d[0], value = _d[1];
                if (sequenceNormalExpression[key]) {
                    throw new Error("Duplicate normal expression key found: ".concat(key));
                }
                if (key.endsWith('seq')) {
                    sequenceNormalExpression[key] = createSeqNormalExpression(value, sequence.maxLength);
                }
                else if (key.endsWith('take-while')) {
                    sequenceNormalExpression[key] = createTakeWhileNormalExpression(value, sequence.maxLength);
                }
                else if (key.endsWith('nth')) {
                    sequenceNormalExpression[key] = createNthNormalExpression(value, sequence.maxLength);
                }
                else if (key.endsWith('?')) {
                    if (sequence.string) {
                        sequenceNormalExpression[key] = createStringPredNormalExpression(value);
                    }
                    else {
                        sequenceNormalExpression[key] = createNumberPredNormalExpression(value);
                    }
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
    }
    function createSeqNormalExpression(seqFunction, maxLength) {
        return {
            evaluate: function (params, sourceCodeInfo) {
                var _a;
                var length = (_a = params[0]) !== null && _a !== void 0 ? _a : maxLength;
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true, lte: maxLength });
                return seqFunction(length, sourceCodeInfo);
            },
            paramCount: typeof maxLength === 'number' ? { max: 1 } : 1,
        };
    }
    function createTakeWhileNormalExpression(takeWhileFunction, maxLength) {
        return {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var fn = params[0];
                assertLitsFunction(fn, sourceCodeInfo);
                return takeWhileFunction(function (value, index) { return !!executeFunction(fn, [value, index], contextStack); }, sourceCodeInfo);
            },
            paramCount: typeof maxLength === 'number' ? { max: 1 } : 1,
        };
    }
    function createNthNormalExpression(nthFunction, maxLength) {
        return {
            evaluate: function (params, sourceCodeInfo) {
                var n = params[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true, lte: maxLength });
                return nthFunction(n, sourceCodeInfo);
            },
            paramCount: 1,
        };
    }
    function createNumberPredNormalExpression(predFunction) {
        return {
            evaluate: function (params, sourceCodeInfo) {
                var value = params[0];
                assertNumber(value, sourceCodeInfo);
                return predFunction(value, sourceCodeInfo);
            },
            paramCount: 1,
        };
    }
    function createStringPredNormalExpression(predFunction) {
        return {
            evaluate: function (params, sourceCodeInfo) {
                var value = params[0];
                assertString(value, sourceCodeInfo);
                return predFunction(value, sourceCodeInfo);
            },
            paramCount: 1,
        };
    }

    // Multinomial coefficient calculation - You have binomial coefficients (presumably through count-combinations), but multinomial coefficients are their natural extension and widely used.
    // Gray code generation - This is a fundamental combinatorial structure used in many algorithms and applications, particularly in coding theory and computer science.
    // Partition by specific constraints - Your partition functions are great, but adding specialized versions for common constraints (like partitions into distinct parts or odd parts) would enhance their utility.
    var perfectNumbers = [6, 28, 496, 8128, 33550336, 8589869056, 137438691328];
    var mersennePrimes = [3, 7, 31, 127, 2047, 8191, 131071, 524287, 2147483647];
    var armstrongNumbers = [
        // 1 digit (all single digits are narcissistic)
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        // 3 digits
        153,
        370,
        371,
        407,
        // 4 digits
        1634,
        8208,
        9474,
        // 5 digits
        54748,
        92727,
        93084,
        // 6 digits
        548834,
        // 7 digits
        1741725,
        4210818,
        9800817,
        9926315,
        // 8 digits
        24678050,
        24678051,
        88593477,
        // 9 digits
        146511208,
        472335975,
        534494836,
        912985153,
        // 10 digits
        4679307774,
        // 11 digits
        32164049650,
        32164049651,
        40028394225,
        42678290603,
        44708635679,
        49388550606,
        82693916578,
        94204591914,
        // 14 digits
        28116440335967,
        // 16 digits
        4338281769391370,
        4338281769391371,
    ];
    /**
     * Checks if a number is a perfect square.
     *
     * @param {number} n - The number to check
     * @return {boolean} - True if n is a perfect square, false otherwise
     */
    function isPerfectSquare(n) {
        var sqrt = Math.sqrt(n);
        return Math.floor(sqrt) === sqrt;
    }
    /**
     * Checks if a number is in the Fibonacci sequence.
     *
     * A number is in the Fibonacci sequence if and only if one of
     * (5n² + 4) or (5n² - 4) is a perfect square.
     * This is based on Binet's formula.
     *
     * @param {number} n - The number to check
     * @return {boolean} - True if n is a Fibonacci number, false otherwise
     */
    function isFibonacciNumber(n) {
        // Handle edge cases
        if (n < 0)
            return false;
        if (n === 0 || n === 1)
            return true;
        // Check if 5n² + 4 or 5n² - 4 is a perfect square
        var test1 = 5 * n * n + 4;
        var test2 = 5 * n * n - 4;
        return isPerfectSquare(test1) || isPerfectSquare(test2);
    }
    /**
     * Checks if a number is in the Lucas sequence.
     *
     * The Lucas sequence starts with L(0) = 2, L(1) = 1 and each subsequent number
     * is the sum of the two previous ones: L(n) = L(n-1) + L(n-2).
     *
     * @param {number} n - The number to check
     * @return {boolean} - True if n is a Lucas number, false otherwise
     */
    function isLucasNumber(n) {
        // Handle edge cases
        if (n <= 0)
            return false;
        // Direct check for small numbers
        if (n === 1 || n === 2)
            return true;
        // Use the iterative approach for all numbers
        var a = 2; // L(0)
        var b = 1; // L(1)
        while (b < n) {
            // Calculate the next Lucas number
            var temp = a + b;
            a = b;
            b = temp;
            // Check if we've found our number
            if (b === n) {
                return true;
            }
        }
        // If we've exceeded n without finding it, it's not a Lucas number
        return false;
    }
    /**
     * Checks if a number is part of the Tribonacci sequence.
     * The Tribonacci sequence starts with 0, 0, 1 and each subsequent
     * number is the sum of the three preceding ones.
     *
     * @param num - The number to check
     * @returns True if the number is in the Tribonacci sequence, false otherwise
     */
    function isTribonacciNumber(num) {
        // Handle edge cases
        if (num < 0)
            return false;
        if (num === 0 || num === 1)
            return true;
        // Special case for 2, which is in the sequence
        if (num === 2)
            return true;
        // Initialize the first three numbers of the sequence
        var a = 0;
        var b = 0;
        var c = 1;
        // Generate the sequence until we reach or exceed the input number
        while (c < num) {
            var next = a + b + c;
            a = b;
            b = c;
            c = next;
        }
        // If c equals the input number, it's in the sequence
        return c === num;
    }
    /**
     * Checks if a number is a triangular number.
     * A triangular number is a number that can be represented as the sum of consecutive integers from 1 to n.
     * The formula for the nth triangular number is n(n+1)/2.
     *
     * @param num - The number to check
     * @returns True if the number is triangular, false otherwise
     */
    function isTriangular(num) {
        // Negative numbers and non-integers cannot be triangular
        if (num < 0 || !Number.isInteger(num)) {
            return false;
        }
        // Edge case: 0 is considered triangular (0th triangular number)
        if (num === 0) {
            return true;
        }
        // Using the formula for triangular numbers: n(n+1)/2 = num
        // This can be rewritten as n² + n - 2*num = 0
        // Using the quadratic formula: n = (-1 + √(1 + 8*num))/2
        // If n is a positive integer, then the number is triangular
        var discriminant = 1 + 8 * num;
        var sqrtDiscriminant = Math.sqrt(discriminant);
        // Check if the result is an integer
        // Due to floating point precision, we check if the value is very close to an integer
        var n = (-1 + sqrtDiscriminant) / 2;
        return Math.abs(Math.round(n) - n) < 1e-10;
    }
    /* Checks if a number is a hexagonal number.
    * A hexagonal number is a figurate number that can be represented as a hexagonal arrangement of points.
    * The formula for the nth hexagonal number is n(2n-1).
    *
    * @param num - The number to check
    * @returns True if the number is hexagonal, false otherwise
    */
    function isHexagonal(num) {
        // Negative numbers and non-integers cannot be hexagonal
        if (num < 0 || !Number.isInteger(num)) {
            return false;
        }
        // Edge case: 0 is considered the 0th hexagonal number
        if (num === 0) {
            return true;
        }
        // Using the formula for hexagonal numbers: H_n = n(2n-1)
        // We can rewrite this as 2n² - n - num = 0
        // Using the quadratic formula: n = (1 + √(1 + 8*num))/4
        // If n is a positive integer, then the number is hexagonal
        var discriminant = 1 + 8 * num;
        var sqrtDiscriminant = Math.sqrt(discriminant);
        // Check if the result is an integer
        // Due to floating point precision, we check if the value is very close to an integer
        var n = (1 + sqrtDiscriminant) / 4;
        return Math.abs(Math.round(n) - n) < 1e-10;
    }
    /**
     * Checks if a number is a pentagonal number.
     * A pentagonal number is a figurate number that can be represented as a pentagonal arrangement of points.
     * The formula for the nth pentagonal number is n(3n-1)/2.
     *
     * The first few pentagonal numbers are:
     * 0, 1, 5, 12, 22, 35, 51, 70, 92, 117, 145, 176, 210, 247, 287, 330, ...
     *
     * @param num - The number to check
     * @returns True if the number is pentagonal, false otherwise
     */
    function isPentagonal(num) {
        // Negative numbers and non-integers cannot be pentagonal
        if (num < 0 || !Number.isInteger(num)) {
            return false;
        }
        // Edge case: 0 is considered the 0th pentagonal number
        if (num === 0) {
            return true;
        }
        // Using the formula for pentagonal numbers: P_n = n(3n-1)/2
        // We can rewrite this as 3n² - n - 2*num = 0
        // Using the quadratic formula: n = (1 + √(1 + 24*num))/6
        // If n is a positive integer, then the number is pentagonal
        var discriminant = 1 + 24 * num;
        var sqrtDiscriminant = Math.sqrt(discriminant);
        // Check if the result is an integer
        // Due to floating point precision, we check if the value is very close to an integer
        var n = (1 + sqrtDiscriminant) / 6;
        return Math.abs(Math.round(n) - n) < 1e-10;
    }
    /**
     * Checks if a number is a pentatope number.
     * A pentatope number (also known as a 4-simplex number or tetrahedral pyramidal number)
     * is a 4-dimensional figurate number that represents the number of points in a 4-simplex.
     *
     * The formula for the nth pentatope number is: n(n+1)(n+2)(n+3)/24
     *
     * The first few pentatope numbers are:
     * 0, 1, 5, 15, 35, 70, 126, 210, 330, 495, 715, 1001, 1365, 1820, 2380, ...
     *
     * @param num - The number to check
     * @returns True if the number is a pentatope number, false otherwise
     */
    function isPentatope(num) {
        // Negative numbers and non-integers cannot be pentatope numbers
        if (num < 0 || !Number.isInteger(num)) {
            return false;
        }
        // Edge case: 0 is considered the 0th pentatope number
        if (num === 0) {
            return true;
        }
        // For a more efficient approach, we can use an approximate solution
        // and then check nearby values
        // A rough approximation for n given P(n) = num is n ≈ (24*num)^(1/4)
        var estimatedN = Math.pow((24 * num), 0.25);
        // Check a small range around the estimate (typically only need to check 1-2 values)
        // We'll check n ± 2 to be safe
        var lowerBound = Math.max(1, Math.floor(estimatedN) - 2);
        var upperBound = Math.ceil(estimatedN) + 2;
        for (var n = lowerBound; n <= upperBound; n++) {
            var pentatope = (n * (n + 1) * (n + 2) * (n + 3)) / 24;
            if (pentatope === num) {
                return true;
            }
            // Since pentatope numbers grow monotonically, we can exit early
            // if we've exceeded the target number
            if (pentatope > num) {
                return false;
            }
        }
        return false;
    }
    /**
     * Checks if a number is a hexacube number.
     * A hexacube number is a perfect 6th power (n^6 for some integer n).
     *
     * The first few hexacube numbers are:
     * 0, 1, 64, 729, 4096, 15625, 46656, 117649, 262144, 531441, 1000000, ...
     *
     * @param num - The number to check
     * @returns True if the number is a hexacube, false otherwise
     */
    function isHexacube(num) {
        // Non-integers cannot be hexacubes
        if (!Number.isInteger(num)) {
            return false;
        }
        // Edge case: 0^6 = 0
        if (num === 0) {
            return true;
        }
        // A key insight: n^6 is always positive for any real n
        // So negative numbers cannot be hexacubes
        if (num < 0) {
            return false;
        }
        // For numbers within the safe integer range, use a direct approach
        if (num <= Number.MAX_SAFE_INTEGER) {
            // Calculate the 6th root
            var sixthRoot = Math.pow(num, (1 / 6));
            // Round to the nearest integer
            var roundedRoot = Math.round(sixthRoot);
            // Calculate the 6th power of the rounded root
            var calculatedHexacube = Math.pow(roundedRoot, 6);
            // Check for exact equality
            return calculatedHexacube === num;
        }
        // For extremely large numbers, use a safer approach that accounts for precision limits
        else {
            // Get a more accurate estimate of the 6th root using logarithms
            // log(n^6) = 6*log(n), so n = 10^(log(num)/6)
            var estimatedRoot = Math.pow(10, (Math.log10(num) / 6));
            var roundedRoot = Math.round(estimatedRoot);
            // For very large numbers, instead of computing roundedRoot^6 directly
            // (which might lose precision), compare logs
            var originalLog = Math.log10(num);
            var hexacubeLog = 6 * Math.log10(roundedRoot);
            // If the logs are very close, it's likely a hexacube
            return Math.abs(originalLog - hexacubeLog) < 1e-10;
        }
    }
    /**
     * Checks if a number is a factorial number.
     * A factorial number is a number that equals n! for some integer n >= 0.
     *
     * The first few factorial numbers are:
     * 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, ...
     * (Note: 0! = 1 and 1! = 1)
     *
     * @param num - The number to check
     * @returns True if the number is a factorial, false otherwise
     */
    function isFactorial(num) {
        // Non-integers cannot be factorials
        if (!Number.isInteger(num)) {
            return false;
        }
        // Negative numbers cannot be factorials
        if (num < 0) {
            return false;
        }
        // Special case: 0 is not a factorial (0! = 1, but 0 itself is not)
        if (num === 0) {
            return false;
        }
        // Special case: 1 is both 0! and 1!
        if (num === 1) {
            return true;
        }
        // Pre-calculate factorials for efficient lookup (up to a reasonable limit)
        var factorials = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800, 479001600, 6227020800, 87178291200, 1307674368000, 20922789888000, 355687428096000, 6402373705728000, 121645100408832000, 2432902008176640000];
        // Direct lookup for known factorials
        if (factorials.includes(num)) {
            return true;
        }
        // For numbers smaller than largest pre-computed factorial but not in the list
        if (num < factorials[factorials.length - 1]) {
            return false;
        }
        // For numbers larger than the pre-computed list, use the division approach
        // This is more expensive but necessary for large numbers
        var divisor = 2;
        var remainder = num;
        while (remainder > 1) {
            if (remainder % divisor !== 0) {
                return false;
            }
            remainder /= divisor;
            divisor++;
            // Safety check to avoid precision issues
            if (divisor > 25 || remainder > Number.MAX_SAFE_INTEGER / divisor) {
                // If we get here, either:
                // 1. The number is too large to safely check
                // 2. We've exceeded our divisor limit (unlikely to be a factorial)
                return false;
            }
        }
        return remainder === 1;
    }
    /**
     * Checks if a number is a perfect power and returns the base and exponent if it is.
     * A perfect power is a number that can be expressed as an integer power of another integer.
     *
     * @param n - The number to check
     * @returns [base, exponent] if n is a perfect power, null otherwise
     */
    function perfectPower(n) {
        // Handle edge cases
        if (n < 2) {
            if (n === 1) {
                // 1 is 1^k for any k, we return [1, 2] as the simplest representation
                return [1, 2];
            }
            if (n === 0) {
                // 0 is 0^k for any k > 0, we return [0, 2] as the simplest representation
                return [0, 2];
            }
            return null; // Negative numbers are not handled in this implementation
        }
        // For each possible exponent k, try to find base b such that b^k = n
        var maxK = Math.floor(Math.log2(n)) + 1;
        for (var k = 2; k <= maxK; k++) {
            // Calculate the potential base as n^(1/k)
            var b = Math.pow(n, (1 / k));
            var roundedB = Math.round(b);
            // Check if roundedB^k is equal to n (within a small epsilon to account for floating point errors)
            var epsilon = 1e-10;
            if (Math.abs(Math.pow(roundedB, k) - n) < epsilon) {
                return [roundedB, k];
            }
        }
        return null; // Not a perfect power
    }
    /**
     * Generates all possible combinations of a specified size from a collection.
     * @param collection The input collection to generate combinations from
     * @param size The size of each combination
     * @returns An array of arrays, where each inner array is a combination of the specified size
     */
    function combinations(collection, size) {
        var e_1, _a;
        // Return empty array if invalid inputs
        if (size <= 0 || size > collection.length) {
            return [];
        }
        // Base case: if size is 1, return each element as its own combination
        if (size === 1) {
            return collection.map(function (item) { return [item]; });
        }
        var result = [];
        // Recursive approach to build combinations
        for (var i = 0; i <= collection.length - size; i++) {
            // Take the current element
            var current = collection[i];
            // Get all combinations of size-1 from the rest of the elements
            var subCombinations = combinations(collection.slice(i + 1), size - 1);
            try {
                // Add the current element to each sub-combination
                for (var subCombinations_1 = (e_1 = void 0, __values(subCombinations)), subCombinations_1_1 = subCombinations_1.next(); !subCombinations_1_1.done; subCombinations_1_1 = subCombinations_1.next()) {
                    var subComb = subCombinations_1_1.value;
                    result.push(__spreadArray([current], __read(subComb), false));
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (subCombinations_1_1 && !subCombinations_1_1.done && (_a = subCombinations_1.return)) _a.call(subCombinations_1);
                }
                finally { if (e_1) throw e_1.error; }
            }
        }
        return result;
    }
    /**
     * Generates all possible permutations of a collection.
     * @param collection The input collection to generate permutations from
     * @returns An array of arrays, where each inner array is a permutation of the input collection
     */
    function permutations(collection) {
        var e_2, _a;
        // Base case: empty array has one permutation - itself
        if (collection.length === 0) {
            return [[]];
        }
        var result = [];
        // For each element in the array
        for (var i = 0; i < collection.length; i++) {
            // Extract the current element
            var current = collection[i];
            // Create a new array without the current element
            var remainingElements = __spreadArray(__spreadArray([], __read(collection.slice(0, i)), false), __read(collection.slice(i + 1)), false);
            // Generate all permutations of the remaining elements
            var subPermutations = permutations(remainingElements);
            try {
                // Add the current element to the beginning of each sub-permutation
                for (var subPermutations_1 = (e_2 = void 0, __values(subPermutations)), subPermutations_1_1 = subPermutations_1.next(); !subPermutations_1_1.done; subPermutations_1_1 = subPermutations_1.next()) {
                    var subPerm = subPermutations_1_1.value;
                    result.push(__spreadArray([current], __read(subPerm), false));
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (subPermutations_1_1 && !subPermutations_1_1.done && (_a = subPermutations_1.return)) _a.call(subPermutations_1);
                }
                finally { if (e_2) throw e_2.error; }
            }
        }
        return result;
    }
    function derangement(n) {
        if (n === 0)
            return 1;
        if (n === 1)
            return 0;
        var a = 1; // !0
        var b = 0; // !1
        var result = 0;
        for (var i = 2; i <= n; i++) {
            result = (i - 1) * (a + b);
            a = b;
            b = result;
        }
        return result;
    }
    function powerSet(set) {
        var e_3, _a;
        var result = [[]];
        var _loop_1 = function (value) {
            var newSubsets = result.map(function (subset) { return __spreadArray(__spreadArray([], __read(subset), false), [value], false); });
            result.push.apply(result, __spreadArray([], __read(newSubsets), false));
        };
        try {
            for (var set_1 = __values(set), set_1_1 = set_1.next(); !set_1_1.done; set_1_1 = set_1.next()) {
                var value = set_1_1.value;
                _loop_1(value);
            }
        }
        catch (e_3_1) { e_3 = { error: e_3_1 }; }
        finally {
            try {
                if (set_1_1 && !set_1_1.done && (_a = set_1.return)) _a.call(set_1);
            }
            finally { if (e_3) throw e_3.error; }
        }
        return result;
    }
    function getLuckyNumbers(length) {
        var numbers = [];
        for (var i = 1; i <= length * 10; i += 2) { // Generate more than needed
            numbers.push(i);
        }
        // Apply the sieve process
        var idx = 1; // Start from the second element (index 1, which is 3)
        while (idx < numbers.length) {
            var step = numbers[idx]; // Current lucky number
            // Remove every step-th number from the list
            // Count from the beginning each time, and account for changing indices
            var j = 0;
            var count = 0;
            while (j < numbers.length) {
                count++;
                if (count % step === 0) {
                    numbers.splice(j, 1);
                }
                else {
                    j++; // Only increment if we didn't remove an element
                }
            }
            // Get the new index of the next element (which may have changed)
            idx++;
        }
        // Return the first 'length' lucky numbers
        return numbers.slice(0, length);
    }
    function isHappyNumber(n) {
        // A happy number is defined by the following process:
        // 1. Starting with any positive integer, replace the number by the sum of the squares of its digits
        // 2. Repeat until either:
        //    - The number equals 1 (in which case it's a happy number)
        //    - It enters a cycle that doesn't include 1 (in which case it's not a happy number)
        if (n <= 0)
            return false;
        // Use a set to detect cycles
        var seen = new Set();
        // Continue until we either reach 1 or detect a cycle
        while (n !== 1 && !seen.has(n)) {
            seen.add(n);
            n = getSumOfSquaredDigits(n);
        }
        // If we reached 1, it's a happy number
        return n === 1;
    }
    function getSumOfSquaredDigits(n) {
        var sum = 0;
        while (n > 0) {
            var digit = n % 10;
            sum += digit * digit;
            n = Math.floor(n / 10);
        }
        return sum;
    }
    function calcPartitions(n) {
        var partition = Array.from({ length: n + 1 }, function () { return 0; });
        partition[0] = 1;
        for (var i = 1; i <= n; i += 1) {
            for (var j = i; j <= n; j += 1) {
                partition[j] += partition[j - i];
            }
        }
        return partition[n];
    }
    function gcd(a, b) {
        while (b !== 0) {
            var temp = b;
            b = a % b;
            a = temp;
        }
        return Math.abs(a);
    }
    function factorsOf(n) {
        var factors = [];
        for (var i = 1; i <= Math.sqrt(n); i++) {
            if (n % i === 0) {
                factors.push(i);
                if (i !== n / i) {
                    factors.push(n / i);
                }
            }
        }
        return factors;
    }
    function factorialOf(n) {
        if (n < 0)
            throw new Error('Factorial is not defined for negative numbers');
        if (n === 0 || n === 1)
            return 1;
        var result = 1;
        for (var i = 2; i <= n; i++)
            result *= i;
        return result;
    }
    function binomialCoefficient(n, k) {
        if (k < 0 || k > n)
            return 0;
        if (k === 0 || k === n)
            return 1;
        var result = 1;
        for (var i = 0; i < k; i++)
            result *= (n - i) / (i + 1);
        return result;
    }
    function isPrime(num) {
        if (num <= 1) {
            return false;
        }
        if (num <= 3) {
            return true;
        }
        if (num % 2 === 0 || num % 3 === 0) {
            return false;
        }
        for (var i = 5; i * i <= num; i += 6) {
            if (num % i === 0 || num % (i + 2) === 0) {
                return false;
            }
        }
        return true;
    }
    function modularExponentiation(base, exponent, mod) {
        if (mod === 1)
            return 0; // Any number mod 1 is 0
        var result = 1;
        base = base % mod;
        while (exponent > 0) {
            if (exponent % 2 === 1) {
                result = (result * base) % mod;
            }
            exponent = Math.floor(exponent / 2);
            base = (base * base) % mod;
        }
        return result;
    }
    /**
     * Extended Euclidean Algorithm
     * Finds gcd(a,b) and coefficients x,y such that ax + by = gcd(a,b)
     */
    function extendedGcd(a, b) {
        if (b === 0) {
            return [a, 1, 0];
        }
        var _a = __read(extendedGcd(b, a % b), 3), g = _a[0], x = _a[1], y = _a[2];
        return [g, y, x - Math.floor(a / b) * y];
    }
    /**
     * Modular Multiplicative Inverse
     * Finds x such that (a * x) % m = 1
     */
    function modInverse(a, m) {
        var _a = __read(extendedGcd(a, m), 2), g = _a[0], x = _a[1];
        if (g !== 1) {
            throw new Error("Modular inverse does not exist (gcd(".concat(a, ", ").concat(m, ") = ").concat(g, ")"));
        }
        return ((x % m) + m) % m; // Ensure positive result
    }
    /**
     * Chinese Remainder Theorem
     * Solve system of congruences: x ≡ remainders[i] (mod moduli[i])
     * Returns the smallest positive integer that satisfies all congruences
     */
    function chineseRemainder(remainders, moduli) {
        if (remainders.length !== moduli.length) {
            throw new Error('Number of remainders must equal number of moduli');
        }
        // Verify moduli are pairwise coprime
        for (var i = 0; i < moduli.length; i++) {
            for (var j = i + 1; j < moduli.length; j++) {
                var extGcd = extendedGcd(moduli[i], moduli[j])[0];
                if (extGcd !== 1) {
                    throw new Error("Moduli must be pairwise coprime, but gcd(".concat(moduli[i], ", ").concat(moduli[j], ") = ").concat(extGcd));
                }
            }
        }
        // Calculate product of all moduli
        var product = moduli.reduce(function (acc, val) { return acc * val; }, 1);
        var sum = 0;
        for (var i = 0; i < remainders.length; i++) {
            var ai = remainders[i];
            var ni = moduli[i];
            var bi = product / ni;
            // Find modular multiplicative inverse of bi modulo ni
            var inverse = modInverse(bi, ni);
            // Add contribution from this congruence
            sum = (sum + ai * bi * inverse) % product;
        }
        return sum;
    }
    var combinatoricalNormalExpression = {
        'c:factors': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), number = _b[0];
                assertNumber(number, sourceCodeInfo, { finite: true, integer: true, positive: true });
                return factorsOf(number);
            },
            paramCount: 1,
        },
        'c:prime-factors': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), number = _b[0];
                assertNumber(number, sourceCodeInfo, { finite: true });
                var factors = [];
                for (var i = 2; i <= number; i += 1) {
                    if (number % i === 0 && isPrime(i)) {
                        factors.push(i);
                    }
                }
                return factors;
            },
            paramCount: 1,
        },
        'c:divisible-by?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), value = _b[0], divisor = _b[1];
                assertNumber(value, sourceCodeInfo);
                assertNumber(divisor, sourceCodeInfo);
                if (divisor === 0)
                    return false;
                return value % divisor === 0;
            },
            paramCount: 2,
        },
        'c:gcd': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), a = _b[0], b = _b[1];
                assertNumber(a, sourceCodeInfo);
                assertNumber(b, sourceCodeInfo);
                return gcd(a, b);
            },
            paramCount: 2,
        },
        'c:lcm': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), a = _b[0], b = _b[1];
                assertNumber(a, sourceCodeInfo);
                assertNumber(b, sourceCodeInfo);
                return Math.abs((a * b) / gcd(a, b));
            },
            paramCount: 2,
        },
        'c:factorial': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true });
                if (n > 170) {
                    // Factorial of numbers greater than 170 exceeds the maximum safe integer in JavaScript
                    throw new LitsError('Factorial is too large to compute safely', sourceCodeInfo);
                }
                return factorialOf(n);
            },
            paramCount: 1,
        },
        'c:generate-combinations': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), set = _b[0], n = _b[1];
                assertArray(set, sourceCodeInfo);
                assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true, lte: set.length });
                return combinations(set, n);
            },
            paramCount: 1,
        },
        'c:generate-permutations': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), set = _b[0];
                assertArray(set, sourceCodeInfo);
                return permutations(set);
            },
            paramCount: 1,
        },
        'c:count-permutations': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), n = _b[0], k = _b[1];
                assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true });
                assertNumber(k, sourceCodeInfo, { integer: true, nonNegative: true, lte: n });
                return factorialOf(n) / factorialOf(n - k);
            },
            paramCount: 2,
        },
        'c:count-combinations': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), n = _b[0], k = _b[1];
                assertNumber(n, sourceCodeInfo, { integer: true, nonNegative: true });
                assertNumber(k, sourceCodeInfo, { integer: true, nonNegative: true, lte: n });
                return binomialCoefficient(n, k);
            },
            aliases: ['c:binomial-coefficient'],
            paramCount: 2,
        },
        'c:multinomial': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), args = _b.slice(0);
                assertVector(args, sourceCodeInfo);
                var sum = args.reduce(function (acc, curr) {
                    assertNumber(curr, sourceCodeInfo, { integer: true, nonNegative: true });
                    return acc + curr;
                }, 0);
                return factorialOf(sum) / args.reduce(function (acc, curr) { return acc * factorialOf(curr); }, 1);
            },
            paramCount: { min: 1 },
        },
        'c:prime?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo, { integer: true });
                return isPrime(value);
            },
            paramCount: 1,
        },
        'c:composite?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo, { integer: true });
                return !isPrime(value) && value > 1;
            },
            paramCount: 1,
        },
        'c:abundant?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo, { integer: true, positive: true });
                var factors = factorsOf(value - 1);
                var sum = factors.reduce(function (acc, curr) { return acc + curr; }, 0);
                return sum > value;
            },
            paramCount: 1,
        },
        'c:deficient?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo, { integer: true, positive: true });
                var factors = factorsOf(value - 1);
                var sum = factors.reduce(function (acc, curr) { return acc + curr; }, 0);
                return sum < value;
            },
            paramCount: 1,
        },
        'c:perfect?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo, { integer: true, positive: true });
                return perfectNumbers.includes(value);
            },
            paramCount: 1,
        },
        'c:mersenne?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo, { integer: true, positive: true });
                return mersennePrimes.includes(value);
            },
            paramCount: 1,
        },
        'c:amicable?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), a = _b[0], b = _b[1];
                assertNumber(a, sourceCodeInfo, { integer: true, positive: true });
                assertNumber(b, sourceCodeInfo, { integer: true, positive: true });
                var sumA = factorsOf(a).reduce(function (acc, curr) { return acc + curr; }, 0);
                var sumB = factorsOf(b).reduce(function (acc, curr) { return acc + curr; }, 0);
                return sumA === b && sumB === a && a !== b;
            },
            paramCount: 2,
        },
        'c:narcissistic?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo, { integer: true, positive: true });
                return armstrongNumbers.includes(value);
            },
            aliases: ['c:armstrong?'],
            paramCount: 1,
        },
        'c-narcissistic-seq': {
            evaluate: function (params, sourceCodeInfo) {
                var _a;
                var length = asNumber((_a = params[0]) !== null && _a !== void 0 ? _a : armstrongNumbers.length, sourceCodeInfo, { integer: true, positive: true, lte: armstrongNumbers.length });
                return armstrongNumbers.slice(0, length);
            },
            aliases: ['c:armstrong-seq'],
            paramCount: { max: 1 },
        },
        'c-narcissistic-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true, lte: armstrongNumbers.length });
                return armstrongNumbers[n - 1];
            },
            aliases: ['c:armstrong-nth'],
            paramCount: 1,
        },
        'c:euler-totient': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                var result = n;
                for (var p = 2; p * p <= n; p += 1) {
                    if (n % p === 0) {
                        while (n % p === 0)
                            n /= p;
                        result -= result / p;
                    }
                }
                if (n > 1)
                    result -= result / n;
                return result;
            },
            paramCount: 1,
        },
        'c:mobius': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                if (n === 1)
                    return 1;
                var factors = factorsOf(n);
                var uniqueFactors = new Set(factors);
                return uniqueFactors.size === factors.length ? -1 : 0;
            },
            paramCount: 1,
            aliases: ['c:möbius'],
        },
        'c:sigma': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                var factors = factorsOf(n);
                return factors.reduce(function (acc, curr) { return acc + curr; }, 0);
            },
            paramCount: 1,
        },
        'c:divisor-count': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                var factors = factorsOf(n);
                return factors.length;
            },
            paramCount: 1,
        },
        'c:carmichael-lambda': {
            evaluate: function (_a, sourceCodeInfo) {
                var e_4, _b;
                var _c = __read(_a, 1), n = _c[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                if (n === 1)
                    return 1;
                var result = n;
                var factors = factorsOf(n);
                try {
                    for (var factors_1 = __values(factors), factors_1_1 = factors_1.next(); !factors_1_1.done; factors_1_1 = factors_1.next()) {
                        var factor = factors_1_1.value;
                        result *= (factor - 1) / factor;
                    }
                }
                catch (e_4_1) { e_4 = { error: e_4_1 }; }
                finally {
                    try {
                        if (factors_1_1 && !factors_1_1.done && (_b = factors_1.return)) _b.call(factors_1);
                    }
                    finally { if (e_4) throw e_4.error; }
                }
                return result;
            },
            paramCount: 1,
        },
        'c:derangement': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return derangement(n);
            },
            paramCount: 1,
        },
        'c:power-set': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), set = _b[0];
                assertArray(set, sourceCodeInfo);
                return powerSet(set);
            },
            paramCount: 1,
        },
        'c:cartesian-product': {
            evaluate: function (params, sourceCodeInfo) {
                params.forEach(function (set) { return assertArray(set, sourceCodeInfo); });
                var sets = params;
                return sets.reduce(function (acc, set) {
                    var result = [];
                    acc.forEach(function (arr) {
                        set.forEach(function (value) {
                            result.push(__spreadArray(__spreadArray([], __read(arr), false), [value], false));
                        });
                    });
                    return result;
                }, [[]]);
            },
            paramCount: { min: 1 },
        },
        'c:perfect-power?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return perfectPower(n) !== null;
            },
            paramCount: 1,
        },
        'c:perfect-power': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return perfectPower(n);
            },
            paramCount: 1,
        },
        'c:perfect-power-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var perfectPowers = [];
                for (var i = 1; perfectPowers.length < length; i++) {
                    if (perfectPower(i)) {
                        perfectPowers.push(i);
                    }
                }
                return perfectPowers;
            },
            paramCount: 1,
        },
        'c:perfect-power-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                var count = 0;
                var current = 1;
                while (count < n) {
                    if (perfectPower(current)) {
                        count++;
                    }
                    current++;
                }
                return current - 1;
            },
            paramCount: 1,
        },
        'c:collatz-seq': {
            evaluate: function (params, sourceCodeInfo) {
                var x = asNumber(params[0], sourceCodeInfo, { integer: true, positive: true });
                var collatz = [x];
                while (x !== 1) {
                    if (x % 2 === 0) {
                        x /= 2;
                    }
                    else {
                        x = 3 * x + 1;
                    }
                    collatz.push(x);
                }
                return collatz;
            },
            paramCount: 1,
        },
        'c:mod-exp': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), base = _b[0], exponent = _b[1], modulus = _b[2];
                assertNumber(base, sourceCodeInfo, { finite: true });
                assertNumber(exponent, sourceCodeInfo, { integer: true, positive: true });
                assertNumber(modulus, sourceCodeInfo, { integer: true, positive: true });
                return modularExponentiation(base, exponent, modulus);
            },
            paramCount: 3,
        },
        'c:mod-inv': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), a = _b[0], m = _b[1];
                assertNumber(a, sourceCodeInfo, { integer: true, positive: true });
                assertNumber(m, sourceCodeInfo, { integer: true, positive: true });
                return modInverse(a, m);
            },
            paramCount: 2,
        },
        'c:extended-gcd': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), a = _b[0], b = _b[1];
                assertNumber(a, sourceCodeInfo, { integer: true });
                assertNumber(b, sourceCodeInfo, { integer: true });
                return extendedGcd(a, b);
            },
            paramCount: 2,
        },
        'c:chinese-remainder': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), remainders = _b[0], moduli = _b[1];
                assertVector(remainders, sourceCodeInfo);
                assertVector(moduli, sourceCodeInfo);
                if (remainders.length !== moduli.length) {
                    throw new Error('Remainders and moduli must have the same length.');
                }
                return chineseRemainder(remainders, moduli);
            },
            paramCount: 2,
        },
        'c:perfect-square?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return isPerfectSquare(n);
            },
            paramCount: 1,
        },
        'c:perfect-square-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var perfectSquares = [];
                for (var i = 0; perfectSquares.length < length; i++) {
                    perfectSquares.push(i * i);
                }
                return perfectSquares;
            },
            paramCount: 1,
        },
        'c:perfect-square-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return n * n;
            },
            paramCount: 1,
        },
        'c:perfect-cube?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                var cbrt = Math.cbrt(n);
                return Math.floor(cbrt) === cbrt;
            },
            paramCount: 1,
        },
        'c:perfect-cube-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var perfectCubes = [];
                for (var i = 0; perfectCubes.length < length; i++) {
                    perfectCubes.push(i * i * i);
                }
                return perfectCubes;
            },
            paramCount: 1,
        },
        'c:perfect-cube-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return n * n * n;
            },
            paramCount: 1,
        },
        'c:palindrome?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo, { integer: true, nonNegative: true });
                var strValue = value.toString();
                return strValue === strValue.split('').reverse().join('');
            },
            paramCount: 1,
        },
        'c:stirling-first-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var stirlingFirst = [];
                for (var n = 0; n < length; n += 1) {
                    stirlingFirst[n] = [];
                    for (var k = 0; k <= n; k += 1) {
                        if (k === 0 || k === n)
                            stirlingFirst[n][k] = 1;
                        else
                            stirlingFirst[n][k] = (n - 1) * (stirlingFirst[n - 1][k] + stirlingFirst[n - 1][k - 1]);
                    }
                }
                return stirlingFirst[length - 1];
            },
            paramCount: 1,
        },
        'c:stirling-first-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), n = _b[0], k = _b[1];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                assertNumber(k, sourceCodeInfo, { integer: true, positive: true, lte: n });
                if (k === 0 || k === n)
                    return 1;
                return (n - 1) * (calcPartitions(n - 1) + calcPartitions(n - k - 1));
            },
            paramCount: 2,
        },
        'c:stirling-second-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var stirlingSecond = [];
                for (var n = 0; n < length; n += 1) {
                    stirlingSecond[n] = [];
                    for (var k = 0; k <= n; k += 1) {
                        if (k === 0 || k === n)
                            stirlingSecond[n][k] = 1;
                        else
                            stirlingSecond[n][k] = k * stirlingSecond[n - 1][k] + stirlingSecond[n - 1][k - 1];
                    }
                }
                return stirlingSecond[length - 1];
            },
            paramCount: 1,
        },
        'c:stirling-second-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), n = _b[0], k = _b[1];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                assertNumber(k, sourceCodeInfo, { integer: true, positive: true, lte: n });
                if (k === 0 || k === n)
                    return 1;
                return k * calcPartitions(n - 1) + calcPartitions(n - k - 1);
            },
            paramCount: 2,
        },
        'c:bernoulli-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var bernoulli = [1];
                for (var n = 1; n < length; n += 1) {
                    var sum = 0;
                    for (var k = 0; k < n; k += 1) {
                        sum += binomialCoefficient(n + 1, k) * bernoulli[k];
                    }
                    bernoulli[n] = -sum / (n + 1);
                }
                return bernoulli;
            },
            paramCount: 1,
        },
        'c:bernoulli-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                if (n === 0)
                    return 1;
                var sum = 0;
                for (var k = 0; k < n; k += 1) {
                    sum += binomialCoefficient(n + 1, k) * calcPartitions(k);
                }
                return -sum / (n + 1);
            },
            paramCount: 1,
        },
        'c:brenoulli?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return n % 2 === 0 && n !== 2;
            },
            paramCount: 1,
        },
        'c:sylvester-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var sylvester = [2];
                for (var n = 1; n < length; n += 1) {
                    sylvester[n] = sylvester[n - 1] + Math.sqrt(sylvester[n - 1]);
                }
                return sylvester;
            },
            paramCount: 1,
        },
        'c:sylvester-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                var sylvester = 2;
                for (var i = 1; i < n; i += 1) {
                    sylvester += Math.sqrt(sylvester);
                }
                return sylvester;
            },
            paramCount: 1,
        },
        'c:sylvester?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return n === 2 || (n > 2 && n % Math.sqrt(n) === 0);
            },
            paramCount: 1,
        },
        'c:golomb-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var golomb = [0];
                for (var n = 1; n < length; n += 1) {
                    golomb[n] = 1 + golomb[n - golomb[golomb[n - 1]]];
                }
                return golomb;
            },
            paramCount: 1,
        },
        'c:golomb-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                var golomb = [0];
                for (var i = 1; i <= n; i += 1) {
                    golomb[i] = 1 + golomb[i - golomb[golomb[i - 1]]];
                }
                return golomb[n];
            },
            paramCount: 1,
        },
        'c:golomb?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return n === 1 || (n > 1 && n % Math.sqrt(n) === 0);
            },
            paramCount: 1,
        },
        'c:lucky-number-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                return getLuckyNumbers(length);
            },
            paramCount: 1,
        },
        'c:lucky-number-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return getLuckyNumbers(n).at(-1);
            },
            paramCount: 1,
        },
        'c:lucky-number?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return getLuckyNumbers(n * 5).includes(n);
            },
            paramCount: 1,
        },
        'c:happy-number-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var happyNumbers = [];
                for (var i = 1; happyNumbers.length < length; i++) {
                    var n = i;
                    var seen = new Set();
                    while (n !== 1 && !seen.has(n)) {
                        seen.add(n);
                        n = String(n)
                            .split('')
                            .reduce(function (sum, digit) { return sum + Math.pow(Number(digit), 2); }, 0);
                    }
                    if (n === 1)
                        happyNumbers.push(i);
                }
                return happyNumbers;
            },
            paramCount: 1,
        },
        'c:happy-number-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                var happyCount = 0;
                var currentNumber = 1;
                while (happyCount < n) {
                    var num = currentNumber;
                    var seen = new Set();
                    while (num !== 1 && !seen.has(num)) {
                        seen.add(num);
                        num = String(num)
                            .split('')
                            .reduce(function (sum, digit) { return sum + Math.pow(Number(digit), 2); }, 0);
                    }
                    if (num === 1)
                        happyCount++;
                    currentNumber++;
                }
                return currentNumber - 1;
            },
            paramCount: 1,
        },
        'c:happy-number?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return isHappyNumber(n);
            },
            paramCount: 1,
        },
        'c:juggler-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var juggler = [1];
                for (var n = 1; n < length; n += 1) {
                    juggler[n] = Math.floor(Math.sqrt(juggler[n - 1]));
                }
                return juggler;
            },
            paramCount: 1,
        },
        'c:juggler-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                var juggler = 1;
                for (var i = 1; i < n; i += 1) {
                    juggler = Math.floor(Math.sqrt(juggler));
                }
                return juggler;
            },
            paramCount: 1,
        },
        'c:juggler?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return n === 1 || (n > 1 && n % Math.sqrt(n) === 0);
            },
            paramCount: 1,
        },
        'c:partition-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var result = [1]; // First partition number is always 1
                for (var m = 1; m < length; m++) {
                    result.push(calcPartitions(m));
                }
                return result;
            },
            paramCount: 1,
        },
        'c:partition-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return calcPartitions(n);
            },
            paramCount: 1,
        },
        'c:partition?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return n === 1 || (n > 1 && n % Math.sqrt(n) === 0);
            },
            paramCount: 1,
        },
        'c:perfect-seq': {
            evaluate: function (params, sourceCodeInfo) {
                var _a;
                var length = asNumber((_a = params[0]) !== null && _a !== void 0 ? _a : perfectNumbers.length, sourceCodeInfo, { integer: true, positive: true, lte: perfectNumbers.length });
                return perfectNumbers.slice(0, length);
            },
            paramCount: { max: 1 },
        },
        'c:perfect-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true, lte: perfectNumbers.length });
                return perfectNumbers[n - 1];
            },
            paramCount: 1,
        },
        'c:mersenne-seq': {
            evaluate: function (params, sourceCodeInfo) {
                var _a;
                var length = asNumber((_a = params[0]) !== null && _a !== void 0 ? _a : mersennePrimes.length, sourceCodeInfo, { integer: true, positive: true, lte: mersennePrimes.length });
                return mersennePrimes.slice(0, length);
            },
            paramCount: { max: 1 },
        },
        'c:mersenne-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true, lte: mersennePrimes.length });
                return mersennePrimes[n - 1];
            },
            paramCount: 1,
        },
        'c:arithmetic-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), start = _b[0], step = _b[1], length = _b[2];
                assertNumber(start, sourceCodeInfo, { finite: true });
                assertNumber(step, sourceCodeInfo, { finite: true });
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                return Array.from({ length: length }, function (_, i) { return start + i * step; });
            },
            paramCount: 3,
        },
        'c:arithmetic-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), start = _b[0], step = _b[1], n = _b[2];
                assertNumber(start, sourceCodeInfo, { finite: true });
                assertNumber(step, sourceCodeInfo, { finite: true });
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return start + (n - 1) * step;
            },
            paramCount: 3,
        },
        'c:geometric-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), start = _b[0], ratio = _b[1], length = _b[2];
                assertNumber(start, sourceCodeInfo, { finite: true });
                assertNumber(ratio, sourceCodeInfo, { finite: true });
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                return Array.from({ length: length }, function (_, i) { return start * Math.pow(ratio, i); });
            },
            paramCount: 3,
        },
        'c:geometric-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), start = _b[0], ratio = _b[1], n = _b[2];
                assertNumber(start, sourceCodeInfo, { finite: true });
                assertNumber(ratio, sourceCodeInfo, { finite: true });
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return start * Math.pow(ratio, (n - 1));
            },
            paramCount: 3,
        },
        'c:fibonacci-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var fib = [0, 1];
                for (var i = 2; i < length; i += 1) {
                    fib[i] = fib[i - 1] + fib[i - 2];
                }
                return fib.slice(0, length);
            },
            paramCount: 1,
        },
        'c:fibonacci-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                if (n === 1)
                    return 0;
                if (n === 2)
                    return 1;
                var a = 0;
                var b = 1;
                for (var i = 3; i <= n; i += 1) {
                    var temp = a + b;
                    a = b;
                    b = temp;
                }
                return b;
            },
            paramCount: 1,
        },
        'c:fibbonacci?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return isFibonacciNumber(n);
            },
            paramCount: 1,
        },
        'c:lucas-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var lucas = [2, 1];
                for (var i = 2; i < length; i += 1) {
                    lucas[i] = lucas[i - 1] + lucas[i - 2];
                }
                return lucas.slice(0, length);
            },
            paramCount: 1,
        },
        'c:lucas-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                if (n === 1)
                    return 2;
                if (n === 2)
                    return 1;
                var a = 2;
                var b = 1;
                for (var i = 3; i <= n; i += 1) {
                    var temp = a + b;
                    a = b;
                    b = temp;
                }
                return b;
            },
            paramCount: 1,
        },
        'c:lucas?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return isLucasNumber(n);
            },
            paramCount: 1,
        },
        'c:tribonacci-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var tribonacci = [0, 1, 1];
                for (var i = 3; i < length; i += 1) {
                    tribonacci[i] = tribonacci[i - 1] + tribonacci[i - 2] + tribonacci[i - 3];
                }
                return tribonacci.slice(0, length);
            },
            paramCount: 1,
        },
        'c:tribonacci-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                if (n === 1)
                    return 0;
                if (n === 2)
                    return 1;
                if (n === 3)
                    return 1;
                var a = 0;
                var b = 1;
                var c = 1;
                for (var i = 4; i <= n; i += 1) {
                    var temp = a + b + c;
                    a = b;
                    b = c;
                    c = temp;
                }
                return c;
            },
            paramCount: 1,
        },
        'c:tribonacci?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return isTribonacciNumber(n);
            },
            paramCount: 1,
        },
        'c:prime-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var primes = [];
                var num = 2;
                while (primes.length < length) {
                    if (isPrime(num)) {
                        primes.push(num);
                    }
                    num += 1;
                }
                return primes;
            },
            paramCount: 1,
        },
        'c:prime-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                var count = 0;
                var num = 2;
                while (count < n) {
                    if (isPrime(num)) {
                        count += 1;
                    }
                    num += 1;
                }
                return num - 1;
            },
            paramCount: 1,
        },
        'c:triangular-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var triangular = [];
                for (var i = 0; i < length; i += 1) {
                    triangular[i] = (i * (i + 1)) / 2;
                }
                return triangular;
            },
            paramCount: 1,
        },
        'c:triangular-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return (n * (n + 1)) / 2;
            },
            paramCount: 1,
        },
        'c:triangular?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return isTriangular(n);
            },
            paramCount: 1,
        },
        'c:square-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var square = [];
                for (var i = 0; i < length; i += 1) {
                    square[i] = Math.pow(i, 2);
                }
                return square;
            },
            paramCount: 1,
        },
        'c:square-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return Math.pow(n, 2);
            },
            paramCount: 1,
        },
        'c:hexagonal-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var hexagonal = [];
                for (var i = 0; i < length; i += 1) {
                    hexagonal[i] = i * (2 * i - 1);
                }
                return hexagonal;
            },
            paramCount: 1,
        },
        'c:hexagonal-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return n * (2 * n - 1);
            },
            paramCount: 1,
        },
        'c:hexagonal?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return isHexagonal(n);
            },
            paramCount: 1,
        },
        'c:pentagonal-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var pentagonal = [];
                for (var i = 0; i < length; i += 1) {
                    pentagonal[i] = (3 * Math.pow(i, 2) - i) / 2;
                }
                return pentagonal;
            },
            paramCount: 1,
        },
        'c:pentagonal-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return (3 * Math.pow(n, 2) - n) / 2;
            },
            paramCount: 1,
        },
        'c:pentagonal?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return isPentagonal(n);
            },
            paramCount: 1,
        },
        'c:cubic-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var cubic = [];
                for (var i = 0; i < length; i += 1) {
                    cubic[i] = Math.pow(i, 3);
                }
                return cubic;
            },
            paramCount: 1,
        },
        'c:cubic-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return Math.pow(n, 3);
            },
            paramCount: 1,
        },
        'c:pentatope-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var pentatope = [];
                for (var i = 0; i < length; i += 1) {
                    pentatope[i] = (i * (i + 1) * (i + 2)) / 6;
                }
                return pentatope;
            },
            paramCount: 1,
        },
        'c:pentatope-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return (n * (n + 1) * (n + 2)) / 6;
            },
            paramCount: 1,
        },
        'c:pentatope?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return isPentatope(n);
            },
            paramCount: 1,
        },
        'c:hexacube-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var hexacube = [];
                for (var i = 0; i < length; i += 1) {
                    hexacube[i] = (i * (i + 1) * (i + 2) * (i + 3)) / 24;
                }
                return hexacube;
            },
            paramCount: 1,
        },
        'c:hexacube-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return (n * (n + 1) * (n + 2) * (n + 3)) / 24;
            },
            paramCount: 1,
        },
        'c:hexacube?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return isHexacube(n);
            },
            paramCount: 1,
        },
        'c:polygonal-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), length = _b[0], sides = _b[1];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                assertNumber(sides, sourceCodeInfo, { integer: true, positive: true });
                var polygonal = [];
                for (var i = 0; i < length; i += 1) {
                    polygonal[i] = (sides * i * (i - 1)) / 2;
                }
                return polygonal;
            },
            paramCount: 2,
        },
        'c:polygonal-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), n = _b[0], sides = _b[1];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                assertNumber(sides, sourceCodeInfo, { integer: true, positive: true });
                return (sides * n * (n - 1)) / 2;
            },
            paramCount: 2,
        },
        'c:factorial-seq': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), length = _b[0];
                assertNumber(length, sourceCodeInfo, { integer: true, positive: true });
                var factorial = [];
                for (var i = 0; i < length; i += 1) {
                    factorial[i] = factorialOf(i);
                }
                return factorial;
            },
            paramCount: 1,
        },
        'c:factorial-nth': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return factorialOf(n);
            },
            paramCount: 1,
        },
        'c:factorial?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), n = _b[0];
                assertNumber(n, sourceCodeInfo, { integer: true, positive: true });
                return isFactorial(n);
            },
            paramCount: 1,
        },
    };
    addSequences(sequenceNormalExpression);
    function addSequences(sequences) {
        var e_5, _a;
        try {
            for (var _b = __values(Object.entries(sequences)), _c = _b.next(); !_c.done; _c = _b.next()) {
                var _d = __read(_c.value, 2), key = _d[0], value = _d[1];
                if (combinatoricalNormalExpression[key]) {
                    throw new Error("Duplicate normal expression key found: ".concat(key));
                }
                combinatoricalNormalExpression[key] = value;
            }
        }
        catch (e_5_1) { e_5 = { error: e_5_1 }; }
        finally {
            try {
                if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
            }
            finally { if (e_5) throw e_5.error; }
        }
    }

    var expressions = __assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign({}, bitwiseNormalExpression), collectionNormalExpression), arrayNormalExpression), sequenceNormalExpression$1), mathNormalExpression), miscNormalExpression), assertNormalExpression), objectNormalExpression), predicatesNormalExpression), regexpNormalExpression), stringNormalExpression), functionalNormalExpression), vectorNormalExpression), tableNormalExpression), matrixNormalExpression), combinatoricalNormalExpression);
    var aliases = {};
    Object.values(expressions).forEach(function (normalExpression) {
        var _a;
        (_a = normalExpression.aliases) === null || _a === void 0 ? void 0 : _a.forEach(function (alias) {
            aliases[alias] = normalExpression;
        });
    });
    var normalExpressions = __assign(__assign({}, expressions), aliases);
    var normalExpressionTypes = {};
    var allNormalExpressions = [];
    Object.entries(normalExpressions).forEach(function (_a, index) {
        var _b = __read(_a, 2), key = _b[0], value = _b[1];
        normalExpressionTypes[key] = index;
        allNormalExpressions.push(value);
    });

    var andSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var evaluateNode = _a.evaluateNode;
            var value = true;
            try {
                for (var _c = __values(node[1][1]), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var param = _d.value;
                    value = evaluateNode(param, contextStack);
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
        evaluateAsNormalExpression: function (params, sourceCodeInfo) {
            var e_2, _a;
            var value = true;
            try {
                for (var params_1 = __values(params), params_1_1 = params_1.next(); !params_1_1.done; params_1_1 = params_1.next()) {
                    var param = params_1_1.value;
                    value = asAny(param, sourceCodeInfo);
                    if (!value)
                        break;
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (params_1_1 && !params_1_1.done && (_a = params_1.return)) _a.call(params_1);
                }
                finally { if (e_2) throw e_2.error; }
            }
            return value;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            return getUndefinedSymbols(node[1][1], contextStack, builtin, evaluateNode);
        },
    };

    var condSpecialExpression = {
        paramCount: { even: true },
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var evaluateNode = _a.evaluateNode;
            var params = node[1][1];
            try {
                for (var params_1 = __values(params), params_1_1 = params_1.next(); !params_1_1.done; params_1_1 = params_1.next()) {
                    var _c = __read(params_1_1.value, 2), test = _c[0], form = _c[1];
                    var value = evaluateNode(test, contextStack);
                    if (!value)
                        continue;
                    return evaluateNode(form, contextStack);
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (params_1_1 && !params_1_1.done && (_b = params_1.return)) _b.call(params_1);
                }
                finally { if (e_1) throw e_1.error; }
            }
            return null;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            return getUndefinedSymbols(node[1][1].flat(), contextStack, builtin, evaluateNode);
        },
    };

    var switchSpecialExpression = {
        paramCount: { odd: true },
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var evaluateNode = _a.evaluateNode;
            var _c = __read(node[1], 3), switchValueNode = _c[1], cases = _c[2];
            var switchValue = evaluateNode(switchValueNode, contextStack);
            try {
                for (var cases_1 = __values(cases), cases_1_1 = cases_1.next(); !cases_1_1.done; cases_1_1 = cases_1.next()) {
                    var _d = __read(cases_1_1.value, 2), test = _d[0], form = _d[1];
                    var value = evaluateNode(test, contextStack);
                    if (value === switchValue) {
                        return evaluateNode(form, contextStack);
                    }
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (cases_1_1 && !cases_1_1.done && (_b = cases_1.return)) _b.call(cases_1);
                }
                finally { if (e_1) throw e_1.error; }
            }
            return null;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            return getUndefinedSymbols(__spreadArray([node[1][1]], __read(node[1][2].flat()), false), contextStack, builtin, evaluateNode);
        },
    };

    var definedSpecialExpression = {
        paramCount: 1,
        evaluate: function (node, contextStack) {
            var symbolNode = node[1][1];
            assertSymbolNode(symbolNode);
            if (!isUserDefinedSymbolNode(symbolNode)) {
                return true; // If the symbol is not a user defined symbol, it is defined. normal or special builtin
            }
            var lookUpResult = contextStack.lookUp(symbolNode);
            return lookUpResult !== null;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            return getUndefinedSymbols([node[1][1]], contextStack, builtin, evaluateNode);
        },
    };

    var bindingTargetTypes = {
        symbol: 11,
        rest: 12,
        object: 13,
        array: 14,
    };

    function walkDefaults(bindingTarget, onDefault) {
        var _a;
        if (bindingTarget[0] === bindingTargetTypes.object) {
            Object.values(bindingTarget[1][0]).forEach(function (element) {
                if (element[1][1]) {
                    onDefault(element[1][1]);
                }
                walkDefaults(element, onDefault);
            });
        }
        else if (bindingTarget[0] === bindingTargetTypes.array) {
            for (var index = 0; index < bindingTarget[1][0].length; index += 1) {
                var element = (_a = bindingTarget[1][0][index]) !== null && _a !== void 0 ? _a : null;
                if (element === null) {
                    continue;
                }
                if (element[1][1]) {
                    onDefault(element[1][1]);
                }
                walkDefaults(element, onDefault);
            }
        }
    }
    function evalueateBindingNodeValues(target, value, evaluate) {
        var sourceCodeInfo = target[2];
        var record = {};
        createRecord(target, value, evaluate, sourceCodeInfo, record);
        return record;
    }
    function createRecord(bindingTarget, value, evaluate, sourceCodeInfo, record) {
        var _a, _b;
        if (bindingTarget[0] === bindingTargetTypes.object) {
            assertUnknownRecord(value, sourceCodeInfo);
            var capturedKeys_1 = new Set();
            var restElement_1;
            Object.entries(bindingTarget[1][0]).forEach(function (_a) {
                var _b;
                var _c = __read(_a, 2), key = _c[0], element = _c[1];
                if (element[0] === bindingTargetTypes.rest) {
                    restElement_1 = element;
                    return;
                }
                capturedKeys_1.add(key);
                var val = (_b = (value[key] !== undefined ? value[key] : element[1][1] && evaluate(element[1][1]))) !== null && _b !== void 0 ? _b : null;
                assertAny(val, sourceCodeInfo);
                createRecord(element, val, evaluate, sourceCodeInfo, record);
            });
            if (restElement_1) {
                var restValues = Object.entries(value)
                    .filter(function (_a) {
                    var _b = __read(_a, 1), key = _b[0];
                    return !capturedKeys_1.has(key);
                })
                    .reduce(function (acc, _a) {
                    var _b = __read(_a, 2), key = _b[0], val = _b[1];
                    acc[key] = asAny(val);
                    return acc;
                }, {});
                record[restElement_1[1][0]] = restValues;
            }
        }
        else if (bindingTarget[0] === bindingTargetTypes.array) {
            var restIndex = null;
            assertArray(value, sourceCodeInfo);
            for (var index = 0; index < bindingTarget[1][0].length; index += 1) {
                var element = (_a = bindingTarget[1][0][index]) !== null && _a !== void 0 ? _a : null;
                if (element === null) {
                    continue;
                }
                if (element[0] === bindingTargetTypes.rest) {
                    restIndex = index;
                    break;
                }
                var val = (_b = (value[index] !== undefined ? value[index] : element[1][1] && evaluate(element[1][1]))) !== null && _b !== void 0 ? _b : null;
                assertAny(val, sourceCodeInfo);
                createRecord(element, val, evaluate, sourceCodeInfo, record);
            }
            if (restIndex !== null) {
                var restValues = value.slice(restIndex);
                var restElement = bindingTarget[1][0][restIndex];
                record[restElement[1][0]] = restValues;
            }
        }
        else if (bindingTarget[0] === bindingTargetTypes.rest) {
            record[bindingTarget[1][0]] = asAny(value);
        }
        else {
            record[bindingTarget[1][0][1]] = asAny(value);
        }
    }
    function getAllBindingTargetNames(bindingTarget) {
        var names = {};
        getNamesFromBindingTarget(bindingTarget, names);
        return names;
    }
    function getNamesFromBindingTarget(target, names) {
        var e_1, _a, e_2, _b;
        if (target === null) {
            return;
        }
        if (target[0] === bindingTargetTypes.array) {
            try {
                for (var _c = __values(target[1][0]), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var element = _d.value;
                    getNamesFromBindingTarget(element, names);
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (_d && !_d.done && (_a = _c.return)) _a.call(_c);
                }
                finally { if (e_1) throw e_1.error; }
            }
        }
        else if (target[0] === bindingTargetTypes.object) {
            try {
                for (var _e = __values(Object.values(target[1][0])), _f = _e.next(); !_f.done; _f = _e.next()) {
                    var element = _f.value;
                    getNamesFromBindingTarget(element, names);
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (_f && !_f.done && (_b = _e.return)) _b.call(_e);
                }
                finally { if (e_2) throw e_2.error; }
            }
        }
        else if (target[0] === bindingTargetTypes.rest) {
            if (names[target[1][0]]) {
                throw new LitsError("Duplicate binding name: ".concat(target[1][0]), target[2]);
            }
            names[target[1][0]] = true;
        }
        else {
            if (names[target[1][0][1]]) {
                throw new LitsError("Duplicate binding name: ".concat(target[1][0]), target[2]);
            }
            names[target[1][0][1]] = true;
        }
    }

    var defSpecialExpression = {
        paramCount: 2,
        evaluate: function (node, contextStack, _a) {
            var evaluateNode = _a.evaluateNode;
            var bindingNode = node[1][1];
            var target = bindingNode[1][0];
            var value = bindingNode[1][1];
            var bindingValue = evaluateNode(value, contextStack);
            var values = evalueateBindingNodeValues(target, bindingValue, function (Node) { return evaluateNode(Node, contextStack); });
            contextStack.exportValues(values, target[2]);
            return null;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            var bindingNode = node[1][1];
            var target = bindingNode[1][0];
            var value = bindingNode[1][1];
            var bindingResult = getUndefinedSymbols([value], contextStack, builtin, evaluateNode);
            walkDefaults(target, function (defaultNode) {
                addToSet(bindingResult, getUndefinedSymbols([defaultNode], contextStack, builtin, evaluateNode));
            });
            contextStack.addValues(getAllBindingTargetNames(target), target[2]);
            return bindingResult;
        },
    };

    var doSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var evaluateNode = _a.evaluateNode;
            var newContext = {};
            var newContextStack = contextStack.create(newContext);
            var result = null;
            try {
                for (var _c = __values(node[1][1]), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var form = _d.value;
                    result = evaluateNode(form, newContextStack);
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
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            return getUndefinedSymbols(node[1][1], contextStack.create({}), builtin, evaluateNode);
        },
    };

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
        catch: null,
        function: null,
        export: null,
        as: null,
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
    var reservedSymbolRecord = __assign(__assign({}, nonNumberReservedSymbolRecord), numberReservedSymbolRecord);
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
        // TODO only subset of special expressions are necessary to check (CommonSpecialExpressionType)
        if (specialExpressionTypes[name])
            throw new LitsError("Cannot define variable ".concat(name, ", it's a special expression."), sourceCodeInfo);
        if (builtin.normalExpressions[name])
            throw new LitsError("Cannot define variable ".concat(name, ", it's a builtin function."), sourceCodeInfo);
        if (isReservedSymbol(name))
            throw new LitsError("Cannot define variable ".concat(name, ", it's a reserved name."), sourceCodeInfo);
        if (contextStack.globalContext[name])
            throw new LitsError("Name already defined \"".concat(name, "\"."), sourceCodeInfo);
    }

    var functionSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var _b, _c;
            var builtin = _a.builtin, getUndefinedSymbols = _a.getUndefinedSymbols, evaluateNode = _a.evaluateNode;
            var _d = __read(node[1], 3), functionSymbol = _d[1], fn = _d[2];
            assertUserDefinedSymbolNode(functionSymbol, node[2]);
            assertNameNotDefined(functionSymbol[1], contextStack, builtin, node[2]);
            var evaluatedFunction = evaluateFunction(fn, contextStack, builtin, getUndefinedSymbols, evaluateNode);
            var litsFunction = (_b = {},
                _b[FUNCTION_SYMBOL] = true,
                _b.sourceCodeInfo = node[2],
                _b.functionType = 'UserDefined',
                _b.name = functionSymbol[1],
                _b.evaluatedfunction = evaluatedFunction,
                _b);
            contextStack.addValues((_c = {}, _c[functionSymbol[1]] = litsFunction, _c), functionSymbol[2]);
            return null;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var _b, _c;
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            var functionName = node[1][1][1];
            contextStack.addValues((_b = {}, _b[functionName] = true, _b), node[1][1][2]);
            var newContext = (_c = {}, _c[functionName] = { value: true }, _c);
            return getFunctionUnresolvedSymbols(node[1][2], contextStack, getUndefinedSymbols, builtin, evaluateNode, newContext);
        },
    };
    var defnSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var _b, _c;
            var builtin = _a.builtin, getUndefinedSymbols = _a.getUndefinedSymbols, evaluateNode = _a.evaluateNode;
            var _d = __read(node[1], 3), functionSymbol = _d[1], fn = _d[2];
            assertUserDefinedSymbolNode(functionSymbol, node[2]);
            assertNameNotDefined(functionSymbol[1], contextStack, builtin, node[2]);
            var evaluatedFunctionOverloades = evaluateFunction(fn, contextStack, builtin, getUndefinedSymbols, evaluateNode);
            var litsFunction = (_b = {},
                _b[FUNCTION_SYMBOL] = true,
                _b.sourceCodeInfo = node[2],
                _b.functionType = 'UserDefined',
                _b.name = functionSymbol[1],
                _b.evaluatedfunction = evaluatedFunctionOverloades,
                _b);
            contextStack.exportValues((_c = {}, _c[functionSymbol[1]] = litsFunction, _c), functionSymbol[2]);
            return null;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var _b, _c;
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            var functionName = node[1][1][1];
            var fn = node[1][2];
            contextStack.exportValues((_b = {}, _b[functionName] = true, _b), node[1][1][2]);
            var newContext = (_c = {}, _c[functionName] = { value: true }, _c);
            return getFunctionUnresolvedSymbols(fn, contextStack, getUndefinedSymbols, builtin, evaluateNode, newContext);
        },
    };
    var fnSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var _b;
            var builtin = _a.builtin, getUndefinedSymbols = _a.getUndefinedSymbols, evaluateNode = _a.evaluateNode;
            var fn = node[1][1];
            var evaluatedFunction = evaluateFunction(fn, contextStack, builtin, getUndefinedSymbols, evaluateNode);
            var litsFunction = (_b = {},
                _b[FUNCTION_SYMBOL] = true,
                _b.sourceCodeInfo = node[2],
                _b.functionType = 'UserDefined',
                _b.name = undefined,
                _b.evaluatedfunction = evaluatedFunction,
                _b);
            return litsFunction;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            var fn = node[1][1];
            return getFunctionUnresolvedSymbols(fn, contextStack, getUndefinedSymbols, builtin, evaluateNode);
        },
    };
    function evaluateFunction(fn, contextStack, builtin, getUndefinedSymbols, evaluateNode) {
        var functionContext = {};
        var context = fn[0].reduce(function (ctx, arg) {
            Object.keys(getAllBindingTargetNames(arg)).forEach(function (name) {
                ctx[name] = { value: null };
            });
            return ctx;
        }, {});
        var undefinedSymbols = getUndefinedSymbols(fn[1], contextStack.new(context), builtin, evaluateNode);
        undefinedSymbols.forEach(function (name) {
            var value = contextStack.getValue(name);
            if (isAny(value)) {
                functionContext[name] = { value: value };
            }
        });
        var evaluatedFunction = [
            fn[0],
            fn[1],
            functionContext,
        ];
        return evaluatedFunction;
    }
    function getFunctionUnresolvedSymbols(fn, contextStack, getUndefinedSymbols, builtin, evaluateNode, functionNameContext) {
        var result = new Set();
        var contextStackWithFunctionName = functionNameContext ? contextStack.create(functionNameContext) : contextStack;
        var newContext = {};
        fn[0].forEach(function (arg) {
            Object.assign(newContext, getAllBindingTargetNames(arg));
            walkDefaults(arg, function (defaultNode) {
                addToSet(result, getUndefinedSymbols([defaultNode], contextStack, builtin, evaluateNode));
            });
        });
        var newContextStack = contextStackWithFunctionName.create(newContext);
        var overloadResult = getUndefinedSymbols(fn[1], newContextStack, builtin, evaluateNode);
        addToSet(result, overloadResult);
        return result;
    }

    var ifSpecialExpression = {
        paramCount: { min: 2, max: 3 },
        evaluate: function (node, contextStack, _a) {
            var evaluateNode = _a.evaluateNode;
            var _b = __read(node[1][1], 3), conditionNode = _b[0], trueNode = _b[1], falseNode = _b[2];
            if (evaluateNode(conditionNode, contextStack)) {
                return evaluateNode(trueNode, contextStack);
            }
            else if (falseNode) {
                return evaluateNode(falseNode, contextStack);
            }
            return null;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            return getUndefinedSymbols(node[1][1].filter(function (n) { return !!n; }), contextStack, builtin, evaluateNode);
        },
    };

    var unlessSpecialExpression = {
        paramCount: { min: 2, max: 3 },
        evaluate: function (node, contextStack, _a) {
            var evaluateNode = _a.evaluateNode;
            var _b = __read(node[1][1], 3), conditionNode = _b[0], trueNode = _b[1], falseNode = _b[2];
            if (!evaluateNode(conditionNode, contextStack)) {
                return evaluateNode(trueNode, contextStack);
            }
            else if (falseNode) {
                return evaluateNode(falseNode, contextStack);
            }
            return null;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            return getUndefinedSymbols(node[1][1].filter(function (n) { return !!n; }), contextStack, builtin, evaluateNode);
        },
    };

    var letSpecialExpression = {
        paramCount: 0,
        evaluate: function (node, contextStack, _a) {
            var evaluateNode = _a.evaluateNode;
            var bindingNode = node[1][1];
            var target = bindingNode[1][0];
            var value = bindingNode[1][1];
            var bindingValue = evaluateNode(value, contextStack);
            var values = evalueateBindingNodeValues(target, bindingValue, function (Node) { return evaluateNode(Node, contextStack); });
            contextStack.addValues(values, target[2]);
            return null;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            var bindingNode = node[1][1];
            var target = bindingNode[1][0];
            var value = bindingNode[1][1];
            var bindingResult = getUndefinedSymbols([value], contextStack, builtin, evaluateNode);
            walkDefaults(target, function (defaultNode) {
                addToSet(bindingResult, getUndefinedSymbols([defaultNode], contextStack, builtin, evaluateNode));
            });
            contextStack.addValues(getAllBindingTargetNames(target), target[2]);
            return bindingResult;
        },
    };

    var loopSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var evaluateNode = _a.evaluateNode;
            var bindingNodes = node[1][1];
            var bindingContext = bindingNodes.reduce(function (result, bindingNode) {
                var val = evaluateNode(bindingNode[1][1], contextStack.create(result));
                var valueRecord = evalueateBindingNodeValues(bindingNode[1][0], val, function (Node) { return evaluateNode(Node, contextStack); });
                Object.entries(valueRecord).forEach(function (_a) {
                    var _b = __read(_a, 2), name = _b[0], value = _b[1];
                    result[name] = { value: value };
                });
                return result;
            }, {});
            var newContextStack = contextStack.create(bindingContext);
            var body = node[1][2];
            var _loop_1 = function () {
                var e_1, _b;
                var result = null;
                try {
                    try {
                        for (var body_1 = (e_1 = void 0, __values(body)), body_1_1 = body_1.next(); !body_1_1.done; body_1_1 = body_1.next()) {
                            var form = body_1_1.value;
                            result = evaluateNode(form, newContextStack);
                        }
                    }
                    catch (e_1_1) { e_1 = { error: e_1_1 }; }
                    finally {
                        try {
                            if (body_1_1 && !body_1_1.done && (_b = body_1.return)) _b.call(body_1);
                        }
                        finally { if (e_1) throw e_1.error; }
                    }
                }
                catch (error) {
                    if (error instanceof RecurSignal) {
                        var params_1 = error.params;
                        if (params_1.length !== bindingNodes.length) {
                            throw new LitsError("recur expected ".concat(bindingNodes.length, " parameters, got ").concat(valueToString(params_1.length)), node[2]);
                        }
                        bindingNodes.forEach(function (bindingNode, index) {
                            var e_2, _a;
                            var valueRecord = evalueateBindingNodeValues(bindingNode[1][0], asAny(params_1[index]), function (Node) { return evaluateNode(Node, contextStack); });
                            try {
                                for (var _b = (e_2 = void 0, __values(Object.entries(valueRecord))), _c = _b.next(); !_c.done; _c = _b.next()) {
                                    var _d = __read(_c.value, 2), name_1 = _d[0], value = _d[1];
                                    bindingContext[name_1].value = value;
                                }
                            }
                            catch (e_2_1) { e_2 = { error: e_2_1 }; }
                            finally {
                                try {
                                    if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                                }
                                finally { if (e_2) throw e_2.error; }
                            }
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
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            var bindingNodes = node[1][1];
            var newContext = bindingNodes
                .reduce(function (context, bindingNode) {
                var names = getAllBindingTargetNames(bindingNode[1][0]);
                Object.keys(names).forEach(function (name) {
                    context[name] = { value: true };
                });
                return context;
            }, {});
            var bindingValueNodes = bindingNodes.map(function (bindingNode) { return bindingNode[1][1]; });
            var bindingsResult = getUndefinedSymbols(bindingValueNodes, contextStack, builtin, evaluateNode);
            var paramsResult = getUndefinedSymbols(node[1][2], contextStack.create(newContext), builtin, evaluateNode);
            return joinSets(bindingsResult, paramsResult);
        },
    };

    function addToContext(bindings, context, contextStack, evaluateNode) {
        var e_1, _a;
        try {
            for (var bindings_1 = __values(bindings), bindings_1_1 = bindings_1.next(); !bindings_1_1.done; bindings_1_1 = bindings_1.next()) {
                var bindingNode = bindings_1_1.value;
                var _b = __read(bindingNode[1], 2), target = _b[0], bindingValue = _b[1];
                var val = evaluateNode(bindingValue, contextStack);
                var valueRecord = evalueateBindingNodeValues(target, val, function (Node) { return evaluateNode(Node, contextStack); });
                Object.entries(valueRecord).forEach(function (_a) {
                    var _b = __read(_a, 2), name = _b[0], value = _b[1];
                    context[name] = { value: value };
                });
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
    function evaluateLoop(returnResult, loopNode, contextStack, evaluateNode) {
        var sourceCodeInfo = loopNode[2];
        var _a = __read(loopNode[1], 3), loopBindings = _a[1], body = _a[2];
        var result = [];
        var bindingIndices = loopBindings.map(function () { return 0; });
        var abort = false;
        var _loop_1 = function () {
            var e_2, _b;
            var context = {};
            var newContextStack = contextStack.create(context);
            var skip = false;
            bindingsLoop: for (var bindingIndex = 0; bindingIndex < loopBindings.length; bindingIndex += 1) {
                var _c = __read(loopBindings[bindingIndex], 4), bindingNode = _c[0], letBindings = _c[1], whenNode = _c[2], whileNode = _c[3];
                var _d = __read(bindingNode[1], 2), targetNode = _d[0], valueNode = _d[1];
                var coll = asColl(evaluateNode(valueNode, newContextStack), sourceCodeInfo);
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
                var val = asAny(seq[index], sourceCodeInfo);
                var valueRecord = evalueateBindingNodeValues(targetNode, val, function (Node) { return evaluateNode(Node, newContextStack); });
                Object.entries(valueRecord).forEach(function (_a) {
                    var _b = __read(_a, 2), name = _b[0], value = _b[1];
                    context[name] = { value: value };
                });
                if (letBindings) {
                    addToContext(letBindings, context, newContextStack, evaluateNode);
                }
                if (whenNode && !evaluateNode(whenNode, newContextStack)) {
                    bindingIndices[bindingIndex] = asNonUndefined(bindingIndices[bindingIndex], sourceCodeInfo) + 1;
                    skip = true;
                    break bindingsLoop;
                }
                if (whileNode && !evaluateNode(whileNode, newContextStack)) {
                    bindingIndices[bindingIndex] = Number.POSITIVE_INFINITY;
                    skip = true;
                    break bindingsLoop;
                }
            }
            if (!skip) {
                var value = null;
                try {
                    for (var body_1 = (e_2 = void 0, __values(body)), body_1_1 = body_1.next(); !body_1_1.done; body_1_1 = body_1.next()) {
                        var form = body_1_1.value;
                        value = evaluateNode(form, newContextStack);
                    }
                }
                catch (e_2_1) { e_2 = { error: e_2_1 }; }
                finally {
                    try {
                        if (body_1_1 && !body_1_1.done && (_b = body_1.return)) _b.call(body_1);
                    }
                    finally { if (e_2) throw e_2.error; }
                }
                if (returnResult)
                    result.push(value);
                if (bindingIndices.length > 0)
                    bindingIndices[bindingIndices.length - 1] += 1;
            }
        };
        while (!abort) {
            _loop_1();
        }
        return returnResult ? result : null;
    }
    function analyze$1(loopNode, contextStack, getUndefinedSymbols, builtin, evaluateNode) {
        var result = new Set();
        var newContext = {};
        var _a = __read(loopNode[1], 3), loopBindings = _a[1], body = _a[2];
        loopBindings.forEach(function (loopBindingNode) {
            var _a = __read(loopBindingNode, 4), bindingNode = _a[0], letBindings = _a[1], whenNode = _a[2], whileNode = _a[3];
            var _b = __read(bindingNode[1], 2), target = _b[0], value = _b[1];
            getUndefinedSymbols([value], contextStack.create(newContext), builtin, evaluateNode).forEach(function (symbol) {
                return result.add(symbol);
            });
            Object.assign(newContext, getAllBindingTargetNames(target));
            if (letBindings) {
                letBindings.forEach(function (letBindingNode) {
                    var _a = __read(letBindingNode[1], 2), letTarget = _a[0], letValue = _a[1];
                    getUndefinedSymbols([letValue], contextStack.create(newContext), builtin, evaluateNode).forEach(function (symbol) {
                        return result.add(symbol);
                    });
                    Object.assign(newContext, getAllBindingTargetNames(letTarget));
                });
            }
            if (whenNode) {
                getUndefinedSymbols([whenNode], contextStack.create(newContext), builtin, evaluateNode).forEach(function (symbol) {
                    return result.add(symbol);
                });
            }
            if (whileNode) {
                getUndefinedSymbols([whileNode], contextStack.create(newContext), builtin, evaluateNode).forEach(function (symbol) {
                    return result.add(symbol);
                });
            }
        });
        getUndefinedSymbols(body, contextStack.create(newContext), builtin, evaluateNode).forEach(function (symbol) {
            return result.add(symbol);
        });
        return result;
    }
    var forSpecialExpression = {
        paramCount: 1,
        evaluate: function (node, contextStack, helpers) { return evaluateLoop(true, node, contextStack, helpers.evaluateNode); },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            return analyze$1(node, contextStack, getUndefinedSymbols, builtin, evaluateNode);
        },
    };
    var doseqSpecialExpression = {
        paramCount: 1,
        evaluate: function (node, contextStack, helpers) {
            evaluateLoop(false, node, contextStack, helpers.evaluateNode);
            return null;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            return analyze$1(node, contextStack, getUndefinedSymbols, builtin, evaluateNode);
        },
    };

    var orSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var evaluateNode = _a.evaluateNode;
            var value = false;
            try {
                for (var _c = __values(node[1][1]), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var param = _d.value;
                    value = evaluateNode(param, contextStack);
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
        evaluateAsNormalExpression: function (params, sourceCodeInfo) {
            var e_2, _a;
            var value = false;
            try {
                for (var params_1 = __values(params), params_1_1 = params_1.next(); !params_1_1.done; params_1_1 = params_1.next()) {
                    var param = params_1_1.value;
                    value = asAny(param, sourceCodeInfo);
                    if (value)
                        break;
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (params_1_1 && !params_1_1.done && (_a = params_1.return)) _a.call(params_1);
                }
                finally { if (e_2) throw e_2.error; }
            }
            return value;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            return getUndefinedSymbols(node[1][1], contextStack, builtin, evaluateNode);
        },
    };

    var qqSpecialExpression = {
        paramCount: { min: 1, max: 2 },
        evaluate: function (node, contextStack, _a) {
            var evaluateNode = _a.evaluateNode;
            var _b = __read(node[1][1], 2), firstNode = _b[0], secondNode = _b[1];
            if (isUserDefinedSymbolNode(firstNode) && contextStack.lookUp(firstNode) === null) {
                return secondNode ? evaluateNode(secondNode, contextStack) : null;
            }
            assertAny(firstNode, node[2]);
            var firstResult = evaluateNode(firstNode, contextStack);
            return firstResult !== null && firstResult !== void 0 ? firstResult : (secondNode ? evaluateNode(secondNode, contextStack) : null);
        },
        evaluateAsNormalExpression: function (params, sourceCodeInfo) {
            var firstParam = asAny(params[0], sourceCodeInfo);
            var secondParam = params[1] !== undefined ? asAny(params[1], sourceCodeInfo) : null;
            return firstParam !== null && firstParam !== void 0 ? firstParam : secondParam;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            return getUndefinedSymbols(node[1][1].filter(function (n) { return !!n; }), contextStack, builtin, evaluateNode);
        },
    };

    var recurSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var evaluateNode = _a.evaluateNode;
            var params = node[1][1];
            var evaluatedParams = params.map(function (paramNode) { return evaluateNode(paramNode, contextStack); });
            throw new RecurSignal(evaluatedParams);
        },
        evaluateAsNormalExpression: function (params) {
            throw new RecurSignal(params);
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            return getUndefinedSymbols(node[1][1], contextStack, builtin, evaluateNode);
        },
    };

    var throwSpecialExpression = {
        paramCount: 1,
        evaluate: function (node, contextStack, _a) {
            var evaluateNode = _a.evaluateNode;
            var message = asString(evaluateNode(node[1][1], contextStack), node[2], {
                nonEmpty: true,
            });
            throw new UserDefinedError(message, node[2]);
        },
        evaluateAsNormalExpression: function (params, sourceCodeInfo) {
            var message = asString(params[0], sourceCodeInfo, {
                nonEmpty: true,
            });
            throw new UserDefinedError(message, undefined);
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            return getUndefinedSymbols([node[1][1]], contextStack, builtin, evaluateNode);
        },
    };

    var trySpecialExpression = {
        paramCount: 1,
        evaluate: function (node, contextStack, _a) {
            var _b;
            var evaluateNode = _a.evaluateNode;
            var _c = __read(node[1], 4), tryExpression = _c[1], errorSymbol = _c[2], catchExpression = _c[3];
            try {
                return evaluateNode(tryExpression, contextStack);
            }
            catch (error) {
                var newContext = errorSymbol
                    ? (_b = {},
                        _b[errorSymbol[1]] = { value: error },
                        _b) : {};
                return evaluateNode(catchExpression, contextStack.create(newContext));
            }
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var _b;
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            var _c = __read(node[1], 4), tryExpression = _c[1], errorSymbol = _c[2], catchExpression = _c[3];
            var tryResult = getUndefinedSymbols([tryExpression], contextStack, builtin, evaluateNode);
            var newContext = errorSymbol
                ? (_b = {},
                    _b[errorSymbol[1]] = { value: true },
                    _b) : {};
            var catchResult = getUndefinedSymbols([catchExpression], contextStack.create(newContext), builtin, evaluateNode);
            return joinSets(tryResult, catchResult);
        },
    };

    var arraySpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var evaluateNode = _a.evaluateNode;
            var result = [];
            try {
                for (var _c = __values(node[1][1]), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var param = _d.value;
                    if (isSpreadNode(param)) {
                        var spreadValue = evaluateNode(param[1], contextStack);
                        if (!Array.isArray(spreadValue)) {
                            throw new LitsError('Spread value is not an array', param[2]);
                        }
                        result.push.apply(result, __spreadArray([], __read(spreadValue), false));
                    }
                    else {
                        result.push(evaluateNode(param, contextStack));
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
            return result;
        },
        evaluateAsNormalExpression: function (params, sourceCodeInfo) {
            var e_2, _a;
            var result = [];
            try {
                for (var params_1 = __values(params), params_1_1 = params_1.next(); !params_1_1.done; params_1_1 = params_1.next()) {
                    var param = params_1_1.value;
                    result.push(asAny(param, sourceCodeInfo));
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (params_1_1 && !params_1_1.done && (_a = params_1.return)) _a.call(params_1);
                }
                finally { if (e_2) throw e_2.error; }
            }
            return result;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            return getUndefinedSymbols(node[1][1], contextStack, builtin, evaluateNode);
        },
    };

    var objectSpecialExpression = {
        paramCount: {},
        evaluate: function (node, contextStack, _a) {
            var evaluateNode = _a.evaluateNode;
            var result = {};
            var params = node[1][1];
            for (var i = 0; i < params.length; i += 2) {
                var keyNode = params[i];
                if (isSpreadNode(keyNode)) {
                    var spreadObject = evaluateNode(keyNode[1], contextStack);
                    if (!isUnknownRecord(spreadObject)) {
                        throw new LitsError('Spread value is not an object', keyNode[2]);
                    }
                    Object.assign(result, spreadObject);
                    i -= 1;
                }
                else {
                    var key = evaluateNode(keyNode, contextStack);
                    var value = evaluateNode(params[i + 1], contextStack);
                    assertString(key, keyNode[2]);
                    result[key] = value;
                }
            }
            return result;
        },
        evaluateAsNormalExpression: function (params, sourceCodeInfo) {
            var result = {};
            for (var i = 0; i < params.length; i += 2) {
                var key = params[i];
                var value = params[i + 1];
                assertString(key, sourceCodeInfo);
                result[key] = value !== null && value !== void 0 ? value : null;
            }
            return result;
        },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            return getUndefinedSymbols(node[1][1], contextStack, builtin, evaluateNode);
        },
    };

    var specialExpressions = [
        qqSpecialExpression,
        andSpecialExpression,
        orSpecialExpression,
        arraySpecialExpression,
        condSpecialExpression,
        defSpecialExpression,
        definedSpecialExpression,
        defnSpecialExpression,
        doSpecialExpression,
        doseqSpecialExpression,
        fnSpecialExpression,
        forSpecialExpression,
        functionSpecialExpression,
        ifSpecialExpression,
        letSpecialExpression,
        loopSpecialExpression,
        objectSpecialExpression,
        recurSpecialExpression,
        switchSpecialExpression,
        throwSpecialExpression,
        trySpecialExpression,
        unlessSpecialExpression,
    ];
    var builtin = {
        normalExpressions: normalExpressions,
        specialExpressions: specialExpressions,
        allNormalExpressions: allNormalExpressions,
    };
    var normalExpressionKeys = Object.keys(normalExpressions);
    var specialExpressionKeys = Object.keys(specialExpressionTypes);
    new Set(specialExpressionKeys);

    function checkParams(evaluatedFunction, nbrOfParams, sourceCodeInfo) {
        var minArity = evaluatedFunction[0].filter(function (arg) { return arg[0] !== bindingTargetTypes.rest && arg[1][1] === undefined; }).length;
        if (nbrOfParams < minArity) {
            throw new LitsError("Unexpected number of arguments. Expected at least ".concat(minArity, ", got ").concat(nbrOfParams, "."), sourceCodeInfo);
        }
    }
    var functionExecutors = {
        NativeJsFunction: function (fn, params, sourceCodeInfo) {
            var _a;
            try {
                // eslint-disable-next-line ts/no-unsafe-assignment
                var clonedParams = JSON.parse(JSON.stringify(params));
                // eslint-disable-next-line ts/no-unsafe-argument
                return toAny((_a = fn.nativeFn).fn.apply(_a, __spreadArray([], __read(clonedParams), false)));
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
        UserDefined: function (fn, params, sourceCodeInfo, contextStack, _a) {
            var evaluateNode = _a.evaluateNode;
            var _loop_1 = function () {
                var e_1, _b;
                checkParams(fn.evaluatedfunction, params.length, sourceCodeInfo);
                var evaluatedFunction = fn.evaluatedfunction;
                var args = evaluatedFunction[0];
                var nbrOfNonRestArgs = args.filter(function (arg) { return arg[0] !== bindingTargetTypes.rest; }).length;
                var newContextStack = contextStack.create(fn.evaluatedfunction[2]);
                var newContext = {};
                var rest = [];
                for (var i = 0; i < params.length; i += 1) {
                    if (i < nbrOfNonRestArgs) {
                        var param = toAny(params[i]);
                        var valueRecord = evalueateBindingNodeValues(args[i], param, function (Node) {
                            return evaluateNode(Node, newContextStack.create(newContext));
                        });
                        Object.entries(valueRecord).forEach(function (_a) {
                            var _b = __read(_a, 2), key = _b[0], value = _b[1];
                            newContext[key] = { value: value };
                        });
                    }
                    else {
                        rest.push(toAny(params[i]));
                    }
                }
                for (var i = params.length; i < nbrOfNonRestArgs; i++) {
                    var arg = args[i];
                    var defaultValue = evaluateNode(arg[1][1], contextStack.create(newContext));
                    var valueRecord = evalueateBindingNodeValues(arg, defaultValue, function (Node) {
                        return evaluateNode(Node, contextStack.create(newContext));
                    });
                    Object.entries(valueRecord).forEach(function (_a) {
                        var _b = __read(_a, 2), key = _b[0], value = _b[1];
                        newContext[key] = { value: value };
                    });
                }
                var restArgument = args.find(function (arg) { return arg[0] === bindingTargetTypes.rest; });
                if (restArgument !== undefined) {
                    var valueRecord = evalueateBindingNodeValues(restArgument, rest, function (Node) { return evaluateNode(Node, contextStack.create(newContext)); });
                    Object.entries(valueRecord).forEach(function (_a) {
                        var _b = __read(_a, 2), key = _b[0], value = _b[1];
                        newContext[key] = { value: value };
                    });
                }
                try {
                    var result = null;
                    var newContextStack2 = newContextStack.create(newContext);
                    try {
                        for (var _c = (e_1 = void 0, __values(evaluatedFunction[1])), _d = _c.next(); !_d.done; _d = _c.next()) {
                            var node = _d.value;
                            result = evaluateNode(node, newContextStack2);
                        }
                    }
                    catch (e_1_1) { e_1 = { error: e_1_1 }; }
                    finally {
                        try {
                            if (_d && !_d.done && (_b = _c.return)) _b.call(_c);
                        }
                        finally { if (e_1) throw e_1.error; }
                    }
                    return { value: result };
                }
                catch (error) {
                    if (error instanceof RecurSignal) {
                        params = error.params;
                        return "continue";
                    }
                    throw error;
                }
            };
            for (;;) {
                var state_1 = _loop_1();
                if (typeof state_1 === "object")
                    return state_1.value;
            }
        },
        Partial: function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return executeFunction(fn.function, __spreadArray(__spreadArray([], __read(fn.params), false), __read(params), false), contextStack, sourceCodeInfo);
        },
        Comp: function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var f = fn.params;
            if (f.length === 0) {
                if (params.length !== 1)
                    throw new LitsError("(comp) expects one argument, got ".concat(valueToString(params.length), "."), sourceCodeInfo);
                return asAny(params[0], sourceCodeInfo);
            }
            return asAny(f.reduceRight(function (result, fun) {
                return [executeFunction(toAny(fun), result, contextStack, sourceCodeInfo)];
            }, params)[0], sourceCodeInfo);
        },
        Constantly: function (fn) {
            return fn.value;
        },
        Juxt: function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return fn.params.map(function (fun) { return executeFunction(toAny(fun), params, contextStack, sourceCodeInfo); });
        },
        Complement: function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return !executeFunction(fn.function, params, contextStack, sourceCodeInfo);
        },
        EveryPred: function (fn, params, sourceCodeInfo, contextStack, _a) {
            var e_2, _b, e_3, _c;
            var executeFunction = _a.executeFunction;
            try {
                for (var _d = __values(fn.params), _e = _d.next(); !_e.done; _e = _d.next()) {
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
        SomePred: function (fn, params, sourceCodeInfo, contextStack, _a) {
            var e_4, _b, e_5, _c;
            var executeFunction = _a.executeFunction;
            try {
                for (var _d = __values(fn.params), _e = _d.next(); !_e.done; _e = _d.next()) {
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
        Fnull: function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var fnulledParams = params.map(function (param, index) { return (param === null ? toAny(fn.params[index]) : param); });
            return executeFunction(toAny(fn.function), fnulledParams, contextStack, sourceCodeInfo);
        },
        Builtin: function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var normalExpression = asNonUndefined(allNormalExpressions[fn.normalBuitinSymbolType], sourceCodeInfo);
            return normalExpression.evaluate(params, sourceCodeInfo, contextStack, { executeFunction: executeFunction });
        },
        SpecialBuiltin: function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var specialExpression = asNonUndefined(specialExpressions[fn.specialBuiltinSymbolType], sourceCodeInfo);
            if (specialExpression.evaluateAsNormalExpression) {
                return specialExpression.evaluateAsNormalExpression(params, sourceCodeInfo, contextStack, { executeFunction: executeFunction });
            }
            else {
                throw new LitsError("Special builtin function ".concat(fn.specialBuiltinSymbolType, " is not supported as normal expression."), sourceCodeInfo);
            }
        },
    };

    function evaluate(ast, contextStack) {
        var e_1, _a;
        var result = null;
        try {
            for (var _b = __values(ast.body), _c = _b.next(); !_c.done; _c = _b.next()) {
                var node = _c.value;
                result = evaluateNode(node, contextStack);
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
    function evaluateNode(node, contextStack) {
        switch (node[0]) {
            case NodeTypes.Number:
                return evaluateNumber(node);
            case NodeTypes.String:
                return evaluateString(node);
            case NodeTypes.NormalBuiltinSymbol:
            case NodeTypes.SpecialBuiltinSymbol:
            case NodeTypes.UserDefinedSymbol:
                return contextStack.evaluateSymbol(node);
            case NodeTypes.ReservedSymbol:
                return evaluateReservedSymbol(node);
            case NodeTypes.NormalExpression:
                return evaluateNormalExpression(node, contextStack);
            case NodeTypes.SpecialExpression:
                return evaluateSpecialExpression(node, contextStack);
            /* v8 ignore next 2 */
            default:
                throw new LitsError("".concat(getNodeTypeName(node[0]), "-node cannot be evaluated"), node[2]);
        }
    }
    function evaluateNumber(node) {
        return node[1];
    }
    function evaluateString(node) {
        return node[1];
    }
    function evaluateReservedSymbol(node) {
        var reservedName = node[1];
        var value = reservedSymbolRecord[reservedName];
        return asNonUndefined(value, node[2]);
    }
    function evaluateNormalExpression(node, contextStack) {
        var sourceCodeInfo = node[2];
        var paramNodes = node[1][1];
        var params = [];
        paramNodes.forEach(function (paramNode) {
            if (isSpreadNode(paramNode)) {
                var spreadValue = evaluateNode(paramNode[1], contextStack);
                if (Array.isArray(spreadValue)) {
                    params.push.apply(params, __spreadArray([], __read(spreadValue), false));
                }
                else {
                    throw new LitsError("Spread operator requires an array, got ".concat(valueToString(paramNode)), paramNode[2]);
                }
            }
            else {
                params.push(evaluateNode(paramNode, contextStack));
            }
        });
        if (isNormalExpressionNodeWithName(node)) {
            var nameSymbol = node[1][0];
            if (isNormalBuiltinSymbolNode(nameSymbol)) {
                var type = nameSymbol[1];
                var normalExpression = builtin.allNormalExpressions[type];
                return normalExpression.evaluate(params, node[2], contextStack, { executeFunction: executeFunction });
            }
            else {
                var fn = contextStack.getValue(nameSymbol[1]);
                if (fn !== undefined) {
                    return executeFunction(asAny(fn), params, contextStack, sourceCodeInfo);
                }
                throw new UndefinedSymbolError(nameSymbol[1], node[2]);
            }
        }
        else {
            var fnNode = node[1][0];
            var fn = evaluateNode(fnNode, contextStack);
            return executeFunction(fn, params, contextStack, sourceCodeInfo);
        }
    }
    function executeFunction(fn, params, contextStack, sourceCodeInfo) {
        if (isLitsFunction(fn))
            return functionExecutors[fn.functionType](fn, params, sourceCodeInfo, contextStack, { evaluateNode: evaluateNode, executeFunction: executeFunction });
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
    function evaluateSpecialExpression(node, contextStack) {
        var specialExpressionType = node[1][0];
        var specialExpression = asNonUndefined(builtin.specialExpressions[specialExpressionType], node[2]);
        var castedEvaluate = specialExpression.evaluate;
        return castedEvaluate(node, contextStack, { evaluateNode: evaluateNode, builtin: builtin, getUndefinedSymbols: getUndefinedSymbols });
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
        ContextStackImpl.prototype.create = function (context) {
            var globalContext = this.globalContext;
            var contextStack = new ContextStackImpl({
                contexts: __spreadArray([context], __read(this.contexts), false),
                values: this.values,
                lazyValues: this.lazyValues,
                nativeJsFunctions: this.nativeJsFunctions,
            });
            contextStack.globalContext = globalContext;
            return contextStack;
        };
        ContextStackImpl.prototype.new = function (context) {
            var contexts = [{}, context];
            return new ContextStackImpl({ contexts: contexts });
        };
        ContextStackImpl.prototype.exportValues = function (values, sourceCodeInfo) {
            var e_1, _a;
            try {
                for (var _b = __values(Object.entries(values)), _c = _b.next(); !_c.done; _c = _b.next()) {
                    var _d = __read(_c.value, 2), name_1 = _d[0], value = _d[1];
                    if (this.globalContext[name_1]) {
                        throw new LitsError("Cannot redefine exported value \"".concat(name_1, "\""), sourceCodeInfo);
                    }
                    if (specialExpressionKeys.includes(name_1)) {
                        throw new LitsError("Cannot shadow special expression \"".concat(name_1, "\""), sourceCodeInfo);
                    }
                    if (normalExpressionKeys.includes(name_1)) {
                        throw new LitsError("Cannot shadow builtin function \"".concat(name_1, "\""), sourceCodeInfo);
                    }
                    this.globalContext[name_1] = { value: value };
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                }
                finally { if (e_1) throw e_1.error; }
            }
            this.addValues(values, sourceCodeInfo);
        };
        ContextStackImpl.prototype.addValues = function (values, sourceCodeInfo) {
            var e_2, _a;
            var currentContext = this.contexts[0];
            try {
                for (var _b = __values(Object.entries(values)), _c = _b.next(); !_c.done; _c = _b.next()) {
                    var _d = __read(_c.value, 2), name_2 = _d[0], value = _d[1];
                    if (currentContext[name_2]) {
                        throw new LitsError("Cannot redefine value \"".concat(name_2, "\""), sourceCodeInfo);
                    }
                    if (specialExpressionKeys.includes(name_2)) {
                        throw new LitsError("Cannot shadow special expression \"".concat(name_2, "\""), sourceCodeInfo);
                    }
                    if (normalExpressionKeys.includes(name_2)) {
                        throw new LitsError("Cannot shadow builtin function \"".concat(name_2, "\""), sourceCodeInfo);
                    }
                    currentContext[name_2] = { value: toAny(value) };
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                }
                finally { if (e_2) throw e_2.error; }
            }
        };
        ContextStackImpl.prototype.getValue = function (name) {
            var e_3, _a;
            var _b, _c, _d;
            try {
                for (var _e = __values(this.contexts), _f = _e.next(); !_f.done; _f = _e.next()) {
                    var context = _f.value;
                    var contextEntry = context[name];
                    if (contextEntry)
                        return contextEntry.value;
                }
            }
            catch (e_3_1) { e_3 = { error: e_3_1 }; }
            finally {
                try {
                    if (_f && !_f.done && (_a = _e.return)) _a.call(_e);
                }
                finally { if (e_3) throw e_3.error; }
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
            var e_4, _a;
            var _b, _c, _d;
            var value = node[1];
            try {
                // const sourceCodeInfo = node[2]
                for (var _e = __values(this.contexts), _f = _e.next(); !_f.done; _f = _e.next()) {
                    var context = _f.value;
                    var contextEntry = context[value];
                    if (contextEntry)
                        return contextEntry;
                }
            }
            catch (e_4_1) { e_4 = { error: e_4_1 }; }
            finally {
                try {
                    if (_f && !_f.done && (_a = _e.return)) _a.call(_e);
                }
                finally { if (e_4) throw e_4.error; }
            }
            var lazyHostValue = (_b = this.lazyValues) === null || _b === void 0 ? void 0 : _b[value];
            if (lazyHostValue !== undefined) {
                return {
                    value: toAny(lazyHostValue.read()),
                };
            }
            var hostValue = (_c = this.values) === null || _c === void 0 ? void 0 : _c[value];
            if (hostValue !== undefined) {
                return {
                    value: toAny(hostValue),
                };
            }
            var nativeJsFunction = (_d = this.nativeJsFunctions) === null || _d === void 0 ? void 0 : _d[value];
            if (nativeJsFunction) {
                return {
                    value: nativeJsFunction,
                };
            }
            return null;
        };
        ContextStackImpl.prototype.evaluateSymbol = function (node) {
            var _a, _b;
            if (isSpecialBuiltinSymbolNode(node)) {
                var functionType = node[1];
                switch (functionType) {
                    case specialExpressionTypes['&&']:
                    case specialExpressionTypes['||']:
                    case specialExpressionTypes.array:
                    case specialExpressionTypes.object:
                    case specialExpressionTypes['defined?']:
                    case specialExpressionTypes.recur:
                    case specialExpressionTypes.throw:
                    case specialExpressionTypes['??']:
                        return _a = {},
                            _a[FUNCTION_SYMBOL] = true,
                            _a.functionType = 'SpecialBuiltin',
                            _a.specialBuiltinSymbolType = functionType,
                            _a.sourceCodeInfo = node[2],
                            _a;
                    default:
                        throw new LitsError("Unknown special builtin symbol type: ".concat(functionType), node[2]);
                }
            }
            if (isNormalBuiltinSymbolNode(node)) {
                var type = node[1];
                return _b = {},
                    _b[FUNCTION_SYMBOL] = true,
                    _b.functionType = 'Builtin',
                    _b.normalBuitinSymbolType = type,
                    _b.sourceCodeInfo = node[2],
                    _b;
            }
            var lookUpResult = this.lookUp(node);
            if (isContextEntry(lookUpResult))
                return lookUpResult.value;
            throw new UndefinedSymbolError(node[1], node[2]);
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
                            functionType: 'NativeJsFunction',
                            nativeFn: jsFunction,
                            name: name
                        },
                        _b[FUNCTION_SYMBOL] = true,
                        _b);
                    return acc;
                }, {}),
        });
        return contextStack.create({});
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
        // 'defn',
        'do',
        'doseq',
        // 'fn',
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
    var postNumberRegExp = /[\s)\]}(,;]/;
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
        var nextChar = input[i];
        if (nextChar && !postNumberRegExp.test(nextChar)) {
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
        var nextChar = input[i];
        if (nextChar && !postNumberRegExp.test(nextChar)) {
            return NO_MATCH;
        }
        return [length, ['BasePrefixedNumber', input.substring(position, i)]];
    };
    var tokenizeSymbol = function (input, position) {
        var value = input[position];
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
        var info = reservedSymbolRecord[symbolName];
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
            var sourceCodeInfo = debug
                ? createSourceCodeInfo(input, position, filePath)
                : undefined;
            if (!tokenDescriptor) {
                throw new LitsError("Unrecognized character '".concat(input[position], "'."), sourceCodeInfo);
            }
            var _a = __read(tokenDescriptor, 2), count = _a[0], token = _a[1];
            position += count;
            if (token) {
                if (sourceCodeInfo) {
                    token[2] = sourceCodeInfo;
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
        try {
            for (var tokenizers_1 = __values(tokenizers), tokenizers_1_1 = tokenizers_1.next(); !tokenizers_1_1.done; tokenizers_1_1 = tokenizers_1.next()) {
                var tokenizer = tokenizers_1_1.value;
                var _b = __read(tokenizer(input, position), 2), nbrOfCharacters = _b[0], token = _b[1];
                position += nbrOfCharacters;
                if (nbrOfCharacters === 0) {
                    continue;
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
        return null;
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
    function asReservedSymbolToken(token, symbolName) {
        assertReservedSymbolToken(token, symbolName);
        return token;
    }
    function isSingleLineCommentToken(token) {
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
                throw new LitsError("Unexpected token: ".concat(token, ", expected operator ").concat(operatorName), token[2]);
            }
            throwUnexpectedToken('Operator', operatorName, token);
        }
    }
    function isWhitespaceToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'Whitespace';
    }
    function isLParenToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'LParen';
    }
    function assertLParenToken(token) {
        if (!isLParenToken(token)) {
            throwUnexpectedToken('LParen', undefined, token);
        }
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
    function isA_BinaryOperatorToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'Operator' && isBinaryOperator(token[1]);
    }
    function throwUnexpectedToken(expected, expectedValue, actual) {
        var actualOutput = "".concat(actual[0], " '").concat(actual[1], "'");
        throw new LitsError("Unexpected token: ".concat(actualOutput, ", expected ").concat(expected).concat(expectedValue ? " '".concat(expectedValue, "'") : ''), actual[2]);
    }

    function minifyTokenStream(tokenStream, _a) {
        var removeWhiteSpace = _a.removeWhiteSpace;
        var tokens = tokenStream.tokens
            .filter(function (token) {
            if (isSingleLineCommentToken(token)
                || isMultiLineCommentToken(token)
                || (removeWhiteSpace && isWhitespaceToken(token))) {
                return false;
            }
            return true;
        });
        return __assign(__assign({}, tokenStream), { tokens: tokens });
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

    var exponentiationPrecedence = 10;
    var binaryFunctionalOperatorPrecedence = 1;
    var placeholderRegexp = /^\$([1-9]\d?)?$/;
    function withSourceCodeInfo(node, sourceCodeInfo) {
        if (sourceCodeInfo) {
            node[2] = sourceCodeInfo;
        }
        return node;
    }
    function getPrecedence(operatorSign, sourceCodeInfo) {
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
            /* v8 ignore next 2 */
            default:
                throw new LitsError("Unknown binary operator: ".concat(operatorSign), sourceCodeInfo);
        }
    }
    function createNamedNormalExpressionNode(symbolNode, params, sourceCodeInfo) {
        var node = withSourceCodeInfo([NodeTypes.NormalExpression, [symbolNode, params]], sourceCodeInfo);
        if (isNormalBuiltinSymbolNode(symbolNode)) {
            assertNumberOfParams(allNormalExpressions[symbolNode[1]].paramCount, node);
        }
        return node;
    }
    function createAccessorNode(left, right, sourceCodeInfo) {
        // Unnamed normal expression
        return withSourceCodeInfo([NodeTypes.NormalExpression, [left, [right]]], sourceCodeInfo);
    }
    function fromBinaryOperatorToNode(operator, symbolNode, left, right, sourceCodeInfo) {
        var operatorName = operator[1];
        switch (operatorName) {
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
                return createNamedNormalExpressionNode(symbolNode, [left, right], sourceCodeInfo);
            case '&&':
            case '||':
            case '??':
                return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes[operatorName], [left, right]]], sourceCodeInfo);
            /* v8 ignore next 10 */
            case '.':
            case ';':
            case ':=':
            case ',':
            case '->':
            case '...':
                throw new LitsError("Unknown binary operator: ".concat(operatorName), sourceCodeInfo);
            default:
                throw new LitsError("Unknown binary operator: ".concat(operatorName), sourceCodeInfo);
        }
    }
    var Parser = /** @class */ (function () {
        function Parser(tokenStream, parseState) {
            this.tokenStream = tokenStream;
            this.parseState = parseState;
        }
        Parser.prototype.peek = function () {
            return this.tokenStream.tokens[this.parseState.position];
        };
        Parser.prototype.peekAhead = function (count) {
            return this.tokenStream.tokens[this.parseState.position + count];
        };
        Parser.prototype.advance = function () {
            this.parseState.position += 1;
        };
        Parser.prototype.parse = function () {
            var nodes = [];
            while (!this.isAtEnd()) {
                nodes.push(this.parseExpression(0, true));
                if (isOperatorToken(this.peek(), ';')) {
                    this.advance();
                }
                else {
                    if (!this.isAtEnd()) {
                        throw new LitsError('Expected ;', this.peek()[2]);
                    }
                }
            }
            return nodes;
        };
        Parser.prototype.parseExpression = function (precedence, moduleScope) {
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
                    throw new LitsError('export is only allowed in module scope', firstToken[2]);
                }
                return this.parseExport(firstToken);
            }
            left || (left = this.parseOperand());
            var operator = this.peek();
            while (!this.isAtExpressionEnd()) {
                if (isA_BinaryOperatorToken(operator)) {
                    var name_1 = operator[1];
                    var newPrecedece = getPrecedence(name_1, operator[2]);
                    if (newPrecedece <= precedence
                        // ** (exponentiation) is right associative
                        && !(newPrecedece === exponentiationPrecedence && precedence === exponentiationPrecedence)) {
                        break;
                    }
                    var symbol = specialExpressionTypes[name_1]
                        ? withSourceCodeInfo([NodeTypes.SpecialBuiltinSymbol, specialExpressionTypes[name_1]], operator[2])
                        : withSourceCodeInfo([NodeTypes.NormalBuiltinSymbol, normalExpressionTypes[name_1]], operator[2]);
                    this.advance();
                    var right = this.parseExpression(newPrecedece);
                    left = fromBinaryOperatorToNode(operator, symbol, left, right, operator[2]);
                }
                else if (isSymbolToken(operator)) {
                    if (!isFunctionOperator(operator[1])) {
                        break;
                    }
                    var newPrecedece = binaryFunctionalOperatorPrecedence;
                    if (newPrecedece <= precedence) {
                        break;
                    }
                    var operatorSymbol = this.parseSymbol();
                    var right = this.parseExpression(newPrecedece);
                    if (isSpecialBuiltinSymbolNode(operatorSymbol)) {
                        throw new LitsError('Special expressions are not allowed in binary functional operators', operatorSymbol[2]);
                    }
                    left = createNamedNormalExpressionNode(operatorSymbol, [left, right], operator[2]);
                }
                else {
                    break;
                }
                operator = this.peek();
            }
            return left;
        };
        Parser.prototype.parseOperand = function () {
            var operand = this.parseOperandPart();
            var token = this.peek();
            while (isOperatorToken(token, '.') || isLBracketToken(token) || isLParenToken(token)) {
                if (token[1] === '.') {
                    this.advance();
                    var symbolToken = this.peek();
                    if (!isSymbolToken(symbolToken)) {
                        throw new LitsError('Expected symbol', this.peek()[2]);
                    }
                    var stringNode = withSourceCodeInfo([NodeTypes.String, symbolToken[1]], symbolToken[2]);
                    operand = createAccessorNode(operand, stringNode, token[2]);
                    this.advance();
                    token = this.peek();
                }
                else if (isLBracketToken(token)) {
                    this.advance();
                    var expression = this.parseExpression();
                    if (!isRBracketToken(this.peek())) {
                        throw new LitsError('Expected closing bracket', this.peek()[2]);
                    }
                    operand = createAccessorNode(operand, expression, token[2]);
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
        Parser.prototype.parseOperandPart = function () {
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
                    throw new LitsError('Expected closing parenthesis', this.peek()[2]);
                }
                this.advance();
                return expression;
            }
            else if (isOperatorToken(token)) {
                var operatorName = token[1];
                if (isBinaryOperator(operatorName)) {
                    this.advance();
                    if (specialExpressionTypes[operatorName] !== undefined) {
                        return withSourceCodeInfo([NodeTypes.SpecialBuiltinSymbol, specialExpressionTypes[operatorName]], token[2]);
                    }
                    return withSourceCodeInfo([NodeTypes.NormalBuiltinSymbol, normalExpressionTypes[operatorName]], token[2]);
                }
                if (operatorName === '->') {
                    return this.parseShorthandLamdaFunction();
                }
                else {
                    throw new LitsError("Illegal operator: ".concat(operatorName), token[2]);
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
                    return this.parseNumber();
                case 'String':
                    return this.parseString();
                case 'Symbol': {
                    var positionBefore = this.parseState.position;
                    var lamdaFunction = this.parseLambdaFunction();
                    if (lamdaFunction) {
                        return lamdaFunction;
                    }
                    this.parseState.position = positionBefore;
                    return this.parseSymbol();
                }
                case 'ReservedSymbol':
                    return this.parseReservedSymbol();
                case 'RegexpShorthand':
                    return this.parseRegexpShorthand();
                default:
                    throw new LitsError("Unknown token type: ".concat(tokenType), token[2]);
            }
        };
        Parser.prototype.parseObject = function () {
            var firstToken = asLBraceToken(this.peek());
            this.advance();
            var params = [];
            while (!this.isAtEnd() && !isRBraceToken(this.peek())) {
                if (isOperatorToken(this.peek(), '...')) {
                    this.advance();
                    params.push(withSourceCodeInfo([NodeTypes.Spread, this.parseExpression()], this.peek()[2]));
                }
                else {
                    var token = this.peek();
                    if (isStringToken(token)) {
                        var stringNode = this.parseString();
                        params.push(withSourceCodeInfo([NodeTypes.String, stringNode[1]], token[2]));
                    }
                    else if (isSymbolToken(token)) {
                        var value = token[1].startsWith('\'')
                            ? this.stringFromQuotedSymbol(token[1])
                            : token[1];
                        params.push(withSourceCodeInfo([NodeTypes.String, value], token[2]));
                        this.advance();
                    }
                    else {
                        throw new LitsError('Expected key to be a symbol or a string', this.peek()[2]);
                    }
                    assertOperatorToken(this.peek(), ':=');
                    this.advance();
                    params.push(this.parseExpression());
                }
                var nextToken = this.peek();
                if (!isOperatorToken(nextToken, ',') && !isRBraceToken(nextToken)) {
                    throw new LitsError('Expected comma or closing brace', this.peek()[2]);
                }
                if (isOperatorToken(nextToken, ',')) {
                    this.advance();
                }
            }
            assertRBraceToken(this.peek());
            this.advance();
            return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.object, params]], firstToken[2]);
        };
        Parser.prototype.parseArray = function () {
            var firstToken = asLBracketToken(this.peek());
            this.advance();
            var params = [];
            while (!this.isAtEnd() && !isRBracketToken(this.peek())) {
                if (isOperatorToken(this.peek(), '...')) {
                    this.advance();
                    params.push(withSourceCodeInfo([NodeTypes.Spread, this.parseExpression()], this.peek()[2]));
                }
                else {
                    params.push(this.parseExpression());
                }
                var nextToken = this.peek();
                if (!isOperatorToken(nextToken, ',') && !isRBracketToken(nextToken)) {
                    throw new LitsError('Expected comma or closing parenthesis', this.peek()[2]);
                }
                if (isOperatorToken(nextToken, ',')) {
                    this.advance();
                }
            }
            assertRBracketToken(this.peek());
            this.advance();
            return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.array, params]], firstToken[2]);
        };
        Parser.prototype.parseFunctionCall = function (symbol) {
            this.advance();
            var params = [];
            while (!this.isAtEnd() && !isRParenToken(this.peek())) {
                if (isOperatorToken(this.peek(), '...')) {
                    this.advance();
                    params.push(withSourceCodeInfo([NodeTypes.Spread, this.parseExpression()], this.peek()[2]));
                }
                else {
                    params.push(this.parseExpression());
                }
                var nextToken = this.peek();
                if (!isOperatorToken(nextToken, ',') && !isRParenToken(nextToken)) {
                    throw new LitsError('Expected comma or closing parenthesis', this.peek()[2]);
                }
                if (isOperatorToken(nextToken, ',')) {
                    this.advance();
                }
            }
            if (!isRParenToken(this.peek())) {
                throw new LitsError('Expected closing parenthesis', this.peek()[2]);
            }
            this.advance();
            if (isSpecialBuiltinSymbolNode(symbol)) { // Named function
                var specialExpressionType = symbol[1];
                var type = specialExpressionType;
                switch (type) {
                    case specialExpressionTypes['||']:
                        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, params]], symbol[2]);
                    case specialExpressionTypes['&&']:
                        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, params]], symbol[2]);
                    case specialExpressionTypes.recur:
                        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, params]], symbol[2]);
                    case specialExpressionTypes.array:
                        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, params]], symbol[2]);
                    case specialExpressionTypes.object:
                        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, params]], symbol[2]);
                    case specialExpressionTypes['??']: {
                        if (params.length === 1) {
                            return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, [params[0], undefined]]], symbol[2]);
                        }
                        if (params.length === 2) {
                            return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, [params[0], params[1]]]], symbol[2]);
                        }
                        throw new LitsError('Expected exactly two parameters', symbol[2]);
                    }
                    case specialExpressionTypes['defined?']: {
                        if (params.length !== 1) {
                            throw new LitsError('Expected exactly one parameter', symbol[2]);
                        }
                        var _a = __read(params, 1), param = _a[0];
                        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, param]], symbol[2]);
                    }
                    case specialExpressionTypes.throw: {
                        if (params.length !== 1) {
                            throw new LitsError('Expected exactly one parameter', symbol[2]);
                        }
                        var _b = __read(params, 1), param = _b[0];
                        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, param]], symbol[2]);
                    }
                    case specialExpressionTypes['0_fn']:
                    case specialExpressionTypes['0_def']:
                    case specialExpressionTypes['0_defn']:
                        throw new LitsError("".concat(type, " is not allowed"), symbol[2]);
                    /* v8 ignore next 2 */
                    default:
                        throw new LitsError("Unknown special expression: ".concat(type), symbol[2]);
                }
            }
            else if (isNormalBuiltinSymbolNode(symbol) || isNormalBuiltinSymbolNode(symbol)) {
                return createNamedNormalExpressionNode(symbol, params, symbol[2]);
            }
            else {
                return withSourceCodeInfo([NodeTypes.NormalExpression, [symbol, params]], symbol[2]);
            }
        };
        Parser.prototype.parseLambdaFunction = function () {
            var firstToken = this.peek();
            if (isLParenToken(firstToken)
                && isSymbolToken(this.peekAhead(1))
                && isOperatorToken(this.peekAhead(2), '->')) {
                return null;
            }
            try {
                var functionArguments = this.parseFunctionArguments();
                if (!isOperatorToken(this.peek(), '->')) {
                    return null;
                }
                this.advance();
                var body = this.parseExpression();
                return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes['0_fn'], [
                            functionArguments,
                            [body],
                        ]]], firstToken[2]);
            }
            catch (_a) {
                return null;
            }
        };
        Parser.prototype.parseFunctionArguments = function () {
            var firstToken = this.peek();
            if (isSymbolToken(firstToken)) {
                return [withSourceCodeInfo([bindingTargetTypes.symbol, [this.parseSymbol(), undefined]], firstToken[2])];
            }
            assertLParenToken(firstToken);
            this.advance();
            var rest = false;
            var defaults = false;
            var functionArguments = [];
            while (!this.isAtEnd() && !isRParenToken(this.peek()) && !isSymbolToken(this.peek(), 'let')) {
                if (rest) {
                    throw new LitsError('Rest argument must be last', this.peek()[2]);
                }
                var bindingTarget = this.parseBindingTarget();
                if (bindingTarget[1][1] !== undefined) {
                    defaults = true;
                }
                if (bindingTarget[0] === bindingTargetTypes.rest) {
                    rest = true;
                }
                if (defaults && !bindingTarget[1][1]) {
                    throw new LitsError('Default arguments must be last', this.peek()[2]);
                }
                functionArguments.push(bindingTarget);
                if (!isOperatorToken(this.peek(), ',') && !isRParenToken(this.peek()) && !isSymbolToken(this.peek(), 'let')) {
                    throw new LitsError('Expected comma or closing parenthesis', this.peek()[2]);
                }
                if (isOperatorToken(this.peek(), ',')) {
                    this.advance();
                }
            }
            if (!isRParenToken(this.peek())) {
                throw new LitsError('Expected closing parenthesis', this.peek()[2]);
            }
            this.advance();
            return functionArguments;
        };
        Parser.prototype.parseShorthandLamdaFunction = function () {
            var _a;
            var firstToken = this.peek();
            this.advance();
            var startPos = this.parseState.position;
            var exprNode = this.parseExpression();
            var endPos = this.parseState.position - 1;
            var arity = 0;
            var dollar1 = 'NOT_SET'; // referring to argument bindings. $ = NAKED, $1, $2, $3, etc = WITH_1
            for (var pos = startPos; pos <= endPos; pos += 1) {
                var token = this.tokenStream.tokens[pos];
                if (isSymbolToken(token)) {
                    var match = placeholderRegexp.exec(token[1]);
                    if (match) {
                        var number = (_a = match[1]) !== null && _a !== void 0 ? _a : '1';
                        if (number === '1') {
                            var mixedPercent1 = (!match[1] && dollar1 === 'WITH_1') || (match[1] && dollar1 === 'NAKED');
                            if (mixedPercent1)
                                throw new LitsError('Please make up your mind, either use $ or $1', firstToken[2]);
                            dollar1 = match[1] ? 'WITH_1' : 'NAKED';
                        }
                        arity = Math.max(arity, Number(number));
                        if (arity > 20)
                            throw new LitsError('Can\'t specify more than 20 arguments', firstToken[2]);
                    }
                }
            }
            var functionArguments = [];
            for (var i = 1; i <= arity; i += 1) {
                if (i === 1 && dollar1 === 'NAKED') {
                    functionArguments.push(withSourceCodeInfo([bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, '$'], undefined]], firstToken[2]));
                }
                else {
                    functionArguments.push(withSourceCodeInfo([bindingTargetTypes.symbol, [[NodeTypes.UserDefinedSymbol, "$".concat(i)], undefined]], firstToken[2]));
                }
            }
            var node = withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes['0_fn'], [
                        functionArguments,
                        [exprNode],
                    ]]], firstToken[2]);
            return node;
        };
        Parser.prototype.parseOptionalDefaulValue = function () {
            if (isOperatorToken(this.peek(), ':=')) {
                this.advance();
                return this.parseExpression();
            }
            return undefined;
        };
        Parser.prototype.parseBindingTarget = function (_a) {
            var _b = _a === void 0 ? {} : _a, requireDefaultValue = _b.requireDefaultValue, noRest = _b.noRest;
            var firstToken = this.peek();
            // Symbol
            if (isSymbolToken(firstToken)) {
                var symbol = this.parseSymbol();
                if (!isUserDefinedSymbolNode(symbol)) {
                    throw new LitsError('Expected user defined symbol', firstToken[2]);
                }
                var defaultValue = this.parseOptionalDefaulValue();
                if (requireDefaultValue && !defaultValue) {
                    throw new LitsError('Expected assignment', this.peek()[2]);
                }
                return withSourceCodeInfo([bindingTargetTypes.symbol, [symbol, defaultValue]], firstToken[2]);
            }
            // Rest
            if (isOperatorToken(firstToken, '...')) {
                if (noRest) {
                    throw new LitsError('Rest element not allowed', firstToken[2]);
                }
                this.advance();
                var symbol = asUserDefinedSymbolNode(this.parseSymbol());
                if (isOperatorToken(this.peek(), ':=')) {
                    throw new LitsError('Rest argument can not have default value', this.peek()[2]);
                }
                return withSourceCodeInfo([bindingTargetTypes.rest, [symbol[1], undefined]], firstToken[2]);
            }
            // Array
            if (isLBracketToken(firstToken)) {
                this.advance();
                var elements = [];
                var token = this.peek();
                var rest = false;
                while (!isRBracketToken(token)) {
                    if (rest) {
                        throw new LitsError('Rest argument must be last', token[2]);
                    }
                    if (isOperatorToken(token, ',')) {
                        elements.push(null);
                        this.advance();
                        token = this.peek();
                        continue;
                    }
                    var target = this.parseBindingTarget();
                    if (target[0] === bindingTargetTypes.rest) {
                        rest = true;
                    }
                    elements.push(target);
                    token = this.peek();
                    if (!isRBracketToken(token)) {
                        assertOperatorToken(token, ',');
                        this.advance();
                    }
                    token = this.peek();
                }
                this.advance();
                var defaultValue = this.parseOptionalDefaulValue();
                if (requireDefaultValue && !defaultValue) {
                    throw new LitsError('Expected assignment', this.peek()[2]);
                }
                return withSourceCodeInfo([bindingTargetTypes.array, [elements, defaultValue]], firstToken[2]);
            }
            // Object
            if (isLBraceToken(firstToken)) {
                this.advance();
                var elements = {};
                var token = this.peek();
                var rest = false;
                while (!isRBraceToken(token)) {
                    if (rest) {
                        throw new LitsError('Rest argument must be last', token[2]);
                    }
                    if (isOperatorToken(token, '...')) {
                        rest = true;
                        this.advance();
                    }
                    var key = asUserDefinedSymbolNode(this.parseSymbol());
                    token = this.peek();
                    if (isReservedSymbolToken(token, 'as')) {
                        if (rest) {
                            throw new LitsError('Rest argument can not have alias', token[2]);
                        }
                        this.advance();
                        var name_2 = asUserDefinedSymbolNode(this.parseSymbol());
                        if (elements[name_2[1]]) {
                            throw new LitsError("Duplicate binding name: ".concat(name_2), token[2]);
                        }
                        elements[key[1]] = withSourceCodeInfo([bindingTargetTypes.symbol, [name_2, this.parseOptionalDefaulValue()]], firstToken[2]);
                    }
                    else if (isRBraceToken(token) || isOperatorToken(token, ',') || isOperatorToken(token, ':=')) {
                        if (elements[key[1]]) {
                            throw new LitsError("Duplicate binding name: ".concat(key), token[2]);
                        }
                        if (rest && isOperatorToken(this.peek(), ':=')) {
                            throw new LitsError('Rest argument can not have default value', this.peek()[2]);
                        }
                        elements[key[1]] = rest
                            ? withSourceCodeInfo([bindingTargetTypes.rest, [key[1], this.parseOptionalDefaulValue()]], firstToken[2])
                            : withSourceCodeInfo([bindingTargetTypes.symbol, [key, this.parseOptionalDefaulValue()]], firstToken[2]);
                    }
                    else if (isLBraceToken(token) || isLBracketToken(token)) {
                        elements[key[1]] = this.parseBindingTarget();
                    }
                    if (!isRBraceToken(this.peek())) {
                        assertOperatorToken(this.peek(), ',');
                        this.advance();
                    }
                    token = this.peek();
                }
                this.advance();
                token = this.peek();
                var defaultValue = this.parseOptionalDefaulValue();
                if (requireDefaultValue && !defaultValue) {
                    throw new LitsError('Expected assignment', token[2]);
                }
                return withSourceCodeInfo([bindingTargetTypes.object, [elements, defaultValue]], firstToken[2]);
            }
            throw new LitsError('Expected symbol', this.peek()[2]);
        };
        Parser.prototype.parseLet = function (token, optionalSemicolon) {
            if (optionalSemicolon === void 0) { optionalSemicolon = false; }
            this.advance();
            var target = this.parseBindingTarget({ requireDefaultValue: true, noRest: true });
            var value = target[1][1];
            target[1][1] = undefined;
            if (!optionalSemicolon) {
                assertOperatorToken(this.peek(), ';');
            }
            var bindingTarget = withSourceCodeInfo([NodeTypes.Binding, [target, value]], token[2]);
            return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.let, bindingTarget]], token[2]);
        };
        Parser.prototype.parseDo = function (token) {
            this.advance();
            var expressions = [];
            while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
                expressions.push(this.parseExpression());
                if (isOperatorToken(this.peek(), ';')) {
                    this.advance();
                }
                else if (!isReservedSymbolToken(this.peek(), 'end')) {
                    throw new LitsError('Expected ;', this.peek()[2]);
                }
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.do, expressions]], token[2]);
        };
        Parser.prototype.parseLoop = function (firstToken) {
            this.advance();
            var bindingNodes = [];
            var token = this.peek();
            while (!this.isAtEnd() && !isSymbolToken(token, 'do')) {
                assertSymbolToken(token, 'let');
                this.advance();
                var target = this.parseBindingTarget({ requireDefaultValue: true, noRest: true });
                var value = target[1][1];
                target[1][1] = undefined;
                bindingNodes.push(withSourceCodeInfo([NodeTypes.Binding, [target, value]], token[2]));
                if (isOperatorToken(this.peek(), ',')) {
                    this.advance();
                }
                token = this.peek();
            }
            if (bindingNodes.length === 0) {
                throw new LitsError('Expected binding', this.peek()[2]);
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
                    throw new LitsError('Expected ;', this.peek()[2]);
                }
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.loop, bindingNodes, params]], firstToken[2]);
        };
        Parser.prototype.parseTry = function (token) {
            this.advance();
            var tryExpressions = [];
            while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'catch')) {
                tryExpressions.push(this.parseExpression());
                if (isOperatorToken(this.peek(), ';')) {
                    this.advance();
                }
                else if (!isReservedSymbolToken(this.peek(), 'catch')) {
                    throw new LitsError('Expected ;', this.peek()[2]);
                }
            }
            var tryExpression = tryExpressions.length === 1
                ? tryExpressions[0]
                : withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.do, tryExpressions]], token[2]);
            assertReservedSymbolToken(this.peek(), 'catch');
            this.advance();
            var errorSymbol;
            if (isLParenToken(this.peek())) {
                this.advance();
                errorSymbol = this.parseSymbol();
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
                    throw new LitsError('Expected ;', this.peek()[2]);
                }
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            var catchExpression = catchExpressions.length === 1
                ? catchExpressions[0]
                : withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.do, catchExpressions]], token[2]);
            return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.try, tryExpression, errorSymbol, catchExpression]], token[2]);
        };
        Parser.prototype.parseForOrDoseq = function (firstToken) {
            var isDoseq = firstToken[1] === 'doseq';
            this.advance();
            var forLoopBindings = [];
            var _loop_1 = function () {
                var loopBinding = this_1.parseForLoopBinding();
                var existingBoundNames = forLoopBindings.flatMap(function (b) { return Object.keys(getAllBindingTargetNames(b[0][1][0])); });
                var newBoundNames = getAllBindingTargetNames(loopBinding[0][1][0]);
                if (Object.keys(newBoundNames).some(function (n) { return existingBoundNames.includes(n); })) {
                    throw new LitsError('Duplicate binding', loopBinding[0][2]);
                }
                forLoopBindings.push(loopBinding);
            };
            var this_1 = this;
            while (!this.isAtEnd() && !isSymbolToken(this.peek(), 'do')) {
                _loop_1();
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
                    throw new LitsError('Expected ;', this.peek()[2]);
                }
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            return isDoseq
                ? withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.doseq, forLoopBindings, expressions]], firstToken[2])
                : withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.for, forLoopBindings, expressions]], firstToken[2]);
        };
        Parser.prototype.parseForLoopBinding = function () {
            assertReservedSymbolToken(this.peek(), 'each');
            this.advance();
            var bindingNode = this.parseBinding();
            var modifiers = [];
            var token = this.peek();
            if (!isSymbolToken(token, 'do') && !isReservedSymbolToken(this.peek(), 'each') && !isOperatorToken(token, ',')) {
                throw new LitsError('Expected do, each or comma', token[2]);
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
                throw new LitsError('Expected symbol each, do, let, when or while', token[2]);
            }
            var letBindings = [];
            if (token[1] === 'let') {
                modifiers.push('&let');
                var _loop_2 = function () {
                    var letNode = this_2.parseLet(token, true);
                    var existingBoundNames = letBindings.flatMap(function (b) { return Object.keys(getAllBindingTargetNames(b[1][0])); });
                    var newBoundNames = Object.keys(getAllBindingTargetNames(letNode[1][1][1][0]));
                    if (newBoundNames.some(function (n) { return existingBoundNames.includes(n); })) {
                        throw new LitsError('Duplicate binding', letNode[1][1][2]);
                    }
                    letBindings.push(letNode[1][1]);
                    token = this_2.peek();
                    if (!isSymbolToken(token, 'do') && !isReservedSymbolToken(this_2.peek(), 'each') && !isOperatorToken(token, ',')) {
                        throw new LitsError('Expected do, each or comma', token[2]);
                    }
                    if (isOperatorToken(token, ',')) {
                        this_2.advance();
                    }
                    token = this_2.peek();
                };
                var this_2 = this;
                while (isSymbolToken(token, 'let')) {
                    _loop_2();
                }
            }
            var whenNode;
            var whileNode;
            while (isReservedSymbolToken(token, 'when')
                || isReservedSymbolToken(token, 'while')) {
                this.advance();
                if (token[1] === 'when') {
                    if (modifiers.includes('&when')) {
                        throw new LitsError('Multiple when modifiers in for loop', token[2]);
                    }
                    modifiers.push('&when');
                    whenNode = this.parseExpression();
                }
                else {
                    if (modifiers.includes('&while')) {
                        throw new LitsError('Multiple while modifiers in for loop', token[2]);
                    }
                    modifiers.push('&while');
                    whileNode = this.parseExpression();
                }
                token = this.peek();
                if (!isSymbolToken(token, 'do') && !isReservedSymbolToken(this.peek(), 'each') && !isOperatorToken(token, ',')) {
                    throw new LitsError('Expected do or comma', token[2]);
                }
                if (isOperatorToken(token, ',')) {
                    this.advance();
                }
                token = this.peek();
            }
            if (!isSymbolToken(token, 'do') && !isReservedSymbolToken(this.peek(), 'each')) {
                throw new LitsError('Expected do or each', token[2]);
            }
            return [bindingNode, letBindings, whenNode, whileNode];
        };
        Parser.prototype.parseBinding = function () {
            var firstToken = asSymbolToken(this.peek());
            var name = asUserDefinedSymbolNode(this.parseSymbol());
            assertReservedSymbolToken(this.peek(), 'in');
            this.advance();
            var value = this.parseExpression();
            var node = withSourceCodeInfo([
                NodeTypes.Binding,
                [
                    withSourceCodeInfo([bindingTargetTypes.symbol, [name, undefined]], firstToken[2]),
                    value,
                ],
            ], firstToken[2]);
            return node;
        };
        Parser.prototype.parseIfOrUnless = function (token) {
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
                    throw new LitsError('Expected ;', this.peek()[2]);
                }
            }
            var thenExpression = thenExpressions.length === 1
                ? thenExpressions[0]
                : withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.do, thenExpressions]], token[2]);
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
                        throw new LitsError('Expected ;', this.peek()[2]);
                    }
                }
                elseExpression = elseExpressions.length === 1
                    ? elseExpressions[0]
                    : withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.do, elseExpressions]], token[2]);
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            return isUnless
                ? withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.unless, [condition, thenExpression, elseExpression]]], token[2])
                : withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.if, [condition, thenExpression, elseExpression]]], token[2]);
        };
        Parser.prototype.parseCond = function (token) {
            this.advance();
            var params = [];
            while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
                assertReservedSymbolToken(this.peek(), 'case');
                this.advance();
                var caseExpression = this.parseExpression();
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
                        throw new LitsError('Expected ;', this.peek()[2]);
                    }
                }
                var thenExpression = expressions.length === 1
                    ? expressions[0]
                    : withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.do, expressions]], token[2]);
                params.push([caseExpression, thenExpression]);
                if (isReservedSymbolToken(this.peek(), 'end')) {
                    break;
                }
                assertReservedSymbolToken(this.peek(), 'case');
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.cond, params]], token[2]);
        };
        Parser.prototype.parseSwitch = function (token) {
            this.advance();
            var valueExpression = this.parseExpression();
            var params = [];
            while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
                assertReservedSymbolToken(this.peek(), 'case');
                this.advance();
                var caseExpression = this.parseExpression();
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
                        throw new LitsError('Expected ;', this.peek()[2]);
                    }
                }
                var thenExpression = expressions.length === 1
                    ? expressions[0]
                    : withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.do, expressions]], token[2]);
                params.push([caseExpression, thenExpression]);
                if (isReservedSymbolToken(this.peek(), 'end')) {
                    break;
                }
                assertReservedSymbolToken(this.peek(), 'case');
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.switch, valueExpression, params]], token[2]);
        };
        Parser.prototype.parseFunction = function (token) {
            this.advance();
            var symbol = this.parseSymbol();
            var functionArguments = this.parseFunctionArguments();
            var body = [];
            while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
                body.push(this.parseExpression());
                if (isOperatorToken(this.peek(), ';')) {
                    this.advance();
                }
                else if (!isReservedSymbolToken(this.peek(), 'end')) {
                    throw new LitsError('Expected ;', this.peek()[2]);
                }
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            assertOperatorToken(this.peek(), ';');
            return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.function, symbol, [
                        functionArguments,
                        body,
                    ]]], token[2]);
        };
        Parser.prototype.isAtEnd = function () {
            return this.parseState.position >= this.tokenStream.tokens.length;
        };
        Parser.prototype.isAtExpressionEnd = function () {
            if (this.isAtEnd()) {
                return true;
            }
            var token = this.peek();
            if (isOperatorToken(token)) {
                return [';', ',', ':='].includes(token[1]);
            }
            if (isReservedSymbolToken(token)) {
                return ['else', 'when', 'while', 'then', 'end', 'case', 'catch'].includes(token[1]);
            }
            return false;
        };
        Parser.prototype.parseExport = function (token) {
            this.advance();
            if (isSymbolToken(this.peek(), 'let')) {
                var letNode = this.parseLet(asSymbolToken(this.peek()));
                return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes['0_def'], letNode[1][1]]], token[2]);
            }
            else if (isReservedSymbolToken(this.peek(), 'function')) {
                this.advance();
                var symbol = this.parseSymbol();
                var functionArguments = this.parseFunctionArguments();
                var body = [];
                while (!this.isAtEnd() && !isReservedSymbolToken(this.peek(), 'end')) {
                    body.push(this.parseExpression());
                    if (isOperatorToken(this.peek(), ';')) {
                        this.advance();
                    }
                    else if (!isReservedSymbolToken(this.peek(), 'end')) {
                        throw new LitsError('Expected ;', this.peek()[2]);
                    }
                }
                assertReservedSymbolToken(this.peek(), 'end');
                this.advance();
                return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes['0_defn'], symbol, [
                            functionArguments,
                            body,
                        ]]], token[2]);
            }
            else {
                throw new LitsError('Expected let or function', this.peek()[2]);
            }
        };
        Parser.prototype.stringToSymbolNode = function (value, sourceCodeInfo) {
            if (specialExpressionTypes[value] !== undefined && value !== 'fn' && value !== 'def' && value !== 'defn') {
                return withSourceCodeInfo([NodeTypes.SpecialBuiltinSymbol, specialExpressionTypes[value]], sourceCodeInfo);
            }
            if (normalExpressionTypes[value] !== undefined) {
                return withSourceCodeInfo([NodeTypes.NormalBuiltinSymbol, normalExpressionTypes[value]], sourceCodeInfo);
            }
            return withSourceCodeInfo([NodeTypes.UserDefinedSymbol, value], sourceCodeInfo);
        };
        Parser.prototype.stringFromQuotedSymbol = function (value) {
            return value.substring(1, value.length - 1)
                .replace(/(\\{2})|(\\')|\\(.)/g, function (_, backslash, singleQuote, normalChar) {
                if (backslash) {
                    return '\\';
                }
                if (singleQuote) {
                    return '\'';
                }
                return "\\".concat(normalChar);
            });
        };
        Parser.prototype.parseSymbol = function () {
            var token = this.peek();
            this.advance();
            if (!isSymbolToken(token)) {
                throw new LitsError("Expected symbol token, got ".concat(token[0]), token[2]);
            }
            if (token[1][0] === '\'') {
                return this.stringToSymbolNode(this.stringFromQuotedSymbol(token[1]), token[2]);
            }
            else {
                return this.stringToSymbolNode(token[1], token[2]);
            }
        };
        Parser.prototype.parseReservedSymbol = function () {
            var token = asReservedSymbolToken(this.peek());
            this.advance();
            var symbol = token[1];
            if (isNumberReservedSymbol(symbol)) {
                return withSourceCodeInfo([NodeTypes.Number, numberReservedSymbolRecord[symbol]], token[2]);
            }
            return withSourceCodeInfo([NodeTypes.ReservedSymbol, token[1]], token[2]);
        };
        Parser.prototype.parseNumber = function () {
            var token = this.peek();
            this.advance();
            var value = token[1];
            var negative = value[0] === '-';
            var numberString = (negative ? value.substring(1) : value).replace(/_/g, '');
            return withSourceCodeInfo([NodeTypes.Number, negative ? -Number(numberString) : Number(numberString)], token[2]);
        };
        Parser.prototype.parseString = function () {
            var token = this.peek();
            this.advance();
            var value = token[1].substring(1, token[1].length - 1)
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
            return withSourceCodeInfo([NodeTypes.String, value], token[2]);
        };
        Parser.prototype.parseRegexpShorthand = function () {
            var token = this.peek();
            this.advance();
            var endStringPosition = token[1].lastIndexOf('"');
            var regexpString = token[1].substring(2, endStringPosition);
            var optionsString = token[1].substring(endStringPosition + 1);
            var stringNode = withSourceCodeInfo([NodeTypes.String, regexpString], token[2]);
            var optionsNode = withSourceCodeInfo([NodeTypes.String, optionsString], token[2]);
            var node = withSourceCodeInfo([
                NodeTypes.NormalExpression,
                [
                    withSourceCodeInfo([NodeTypes.NormalBuiltinSymbol, normalExpressionTypes.regexp], token[2]),
                    [stringNode, optionsNode],
                ],
            ], token[2]);
            return node;
        };
        return Parser;
    }());

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
        Lits.prototype.context = function (programOrAst, params) {
            if (params === void 0) { params = {}; }
            var ast = typeof programOrAst === 'string' ? this.generateAst(programOrAst, params) : programOrAst;
            var contextStack = createContextStack(params);
            evaluate(ast, contextStack);
            return contextStack.globalContext;
        };
        Lits.prototype.getUndefinedSymbols = function (programOrAst, params) {
            if (params === void 0) { params = {}; }
            var ast = typeof programOrAst === 'string' ? this.generateAst(programOrAst, params) : programOrAst;
            var contextStack = createContextStack(params);
            return getUndefinedSymbols(ast, contextStack, builtin, evaluateNode);
        };
        Lits.prototype.tokenize = function (program, tokenizeParams) {
            if (tokenizeParams === void 0) { tokenizeParams = {}; }
            var tokenStream = tokenize$1(program, this.debug, tokenizeParams.filePath);
            return tokenizeParams.minify ? minifyTokenStream(tokenStream, { removeWhiteSpace: false }) : tokenStream;
        };
        Lits.prototype.parse = function (tokenStream) {
            tokenStream = minifyTokenStream(tokenStream, { removeWhiteSpace: true });
            var ast = {
                body: [],
                hasDebugData: tokenStream.hasDebugData,
            };
            var parseState = {
                position: 0,
            };
            ast.body = new Parser(tokenStream, parseState).parse();
            return ast;
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
            'matrix-a': [
                [1, 2, 3],
                [4, 5, 6],
            ],
            'matrix-b': [
                [7, 8],
                [9, 10],
                [11, 12],
            ],
            'matrix-c': [
                [3, 0, 2],
                [2, 0, -2],
                [0, 1, 1],
            ],
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
    function truncateCode(code) {
        var oneLiner = getLits().tokenize(code, { minify: true }).tokens.map(function (t) { return t[0] === 'Whitespace' ? ' ' : t[1]; }).join('').trim();
        var count = 100;
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
            hijacker.releaseConsole();
            console.log(result);
        }
        catch (error) {
            appendOutput(error, 'error');
            hijacker.releaseConsole();
        }
        finally {
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
            hijacker.releaseConsole();
            console.log(result);
        }
        catch (error) {
            appendOutput(error, 'error');
            hijacker.releaseConsole();
            return;
        }
        finally {
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
