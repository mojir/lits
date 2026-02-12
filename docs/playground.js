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
        function LitsError(err, sourceCodeInfo) {
            var _this = this;
            var message = err instanceof Error
                ? err.message
                : "".concat(err);
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
        'Namespace',
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
    function isFunctionLike(value) {
        if (typeof value === 'number')
            return true;
        if (isColl(value))
            return true;
        if (isLitsFunction(value))
            return true;
        return false;
    }
    function asFunctionLike(value, sourceCodeInfo) {
        assertFunctionLike(value, sourceCodeInfo);
        return value;
    }
    function assertFunctionLike(value, sourceCodeInfo) {
        if (!isFunctionLike(value))
            throw getAssertionError('FunctionLike', value, sourceCodeInfo);
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
        if (Number.isNaN(value))
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

    var annotatedArrays = new WeakSet();
    var vectors = new WeakSet();
    var matrices = new WeakSet();
    var grids = new WeakSet();
    function annotate(value) {
        if (!Array.isArray(value)) {
            return value;
        }
        if (annotatedArrays.has(value)) {
            return value;
        }
        isVector(value);
        if (!isMatrix(value)) {
            isGrid(value);
        }
        return value;
    }
    function isVector(vector) {
        if (!Array.isArray(vector)) {
            return false;
        }
        if (vectors.has(vector)) {
            return true;
        }
        if (vector.every(function (elem) { return isNumber(elem); })) {
            annotatedArrays.add(vector);
            vectors.add(vector);
            return true;
        }
        return false;
    }
    function assertVector(vector, sourceCodeInfo) {
        if (!isVector(vector)) {
            throw new LitsError("Expected a vector, but got ".concat(vector), sourceCodeInfo);
        }
    }
    function isGrid(grid) {
        var e_1, _a;
        if (!Array.isArray(grid)) {
            return false;
        }
        if (grids.has(grid)) {
            return true;
        }
        if (grid.length === 0) {
            return false;
        }
        if (!Array.isArray(grid[0])) {
            return false;
        }
        var nbrOfCols = grid[0].length;
        try {
            for (var _b = __values(grid.slice(1)), _c = _b.next(); !_c.done; _c = _b.next()) {
                var row = _c.value;
                if (!Array.isArray(row)) {
                    return false;
                }
                if (row.length !== nbrOfCols) {
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
        annotatedArrays.add(grid);
        grids.add(grid);
        return true;
    }
    function assertGrid(grid, sourceCodeInfo) {
        if (!isGrid(grid)) {
            throw new LitsError("Expected a grid, but got ".concat(grid), sourceCodeInfo);
        }
    }
    function isMatrix(matrix) {
        var e_2, _a;
        if (!Array.isArray(matrix)) {
            return false;
        }
        if (matrices.has(matrix)) {
            return true;
        }
        if (matrix.length === 0) {
            return false;
        }
        if (!Array.isArray(matrix[0])) {
            return false;
        }
        if (matrix[0].length === 0) {
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
                if (row.some(function (cell) { return !isNumber(cell); })) {
                    return false;
                }
            }
        }
        catch (e_2_1) { e_2 = { error: e_2_1 }; }
        finally {
            try {
                if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
            }
            finally { if (e_2) throw e_2.error; }
        }
        annotatedArrays.add(matrix);
        grids.add(matrix);
        matrices.add(matrix);
        return true;
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
        if (typeof value === 'number') {
            return "".concat(value);
        }
        if (isRegularExpression(value))
            return "/".concat(value.s, "/").concat(value.f);
        if (typeof value === 'string')
            return "\"".concat(value, "\"");
        if (Array.isArray(value) && isMatrix(value))
            return stringifyMatrix(value);
        if (Array.isArray(value) && isVector(value)) {
            if (value.length === 0)
                return '[]';
            if (value.length > 8) {
                return "[\n  ".concat(value.map(function (cell) {
                    return cell;
                }).join(',\n  '), "\n]");
            }
            else {
                return "[".concat(value.map(function (cell) {
                    return cell;
                }).join(', '), "]");
            }
        }
        return JSON.stringify(replaceInfinities(value), null, 2);
    }
    function replaceInfinities(value) {
        var e_1, _a;
        if (value === Number.POSITIVE_INFINITY) {
            return '∞';
        }
        if (value === Number.NEGATIVE_INFINITY) {
            return '-∞';
        }
        if (Array.isArray(value)) {
            return value.map(replaceInfinities);
        }
        if (typeof value === 'object' && value !== null) {
            var result = {};
            try {
                for (var _b = __values(Object.entries(value)), _c = _b.next(); !_c.done; _c = _b.next()) {
                    var _d = __read(_c.value, 2), key = _d[0], val = _d[1];
                    result[key] = replaceInfinities(val);
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
        return value;
    }
    function stringifyMatrix(matrix) {
        var padding = matrix.flat().reduce(function (max, cell) { return Math.max(max, "".concat(cell).length); }, 0) + 1;
        var rows = matrix.map(function (row) { return "[".concat(row.map(function (cell) { return "".concat(cell).padStart(padding); }).join(' '), " ]"); });
        return rows.join('\n');
    }

    var specialExpressionTypes = {
        '??': 0,
        '&&': 1,
        '||': 2,
        'array': 3,
        'cond': 4,
        '0_def': 5,
        'defined?': 6,
        // '0_defn': 7,
        'block': 7,
        'doseq': 8,
        '0_lambda': 9,
        'for': 10,
        // 'function': 10,
        'if': 11,
        'let': 12,
        'loop': 13,
        'object': 14,
        'recur': 15,
        'switch': 16,
        'throw': 17,
        'try': 18,
        'unless': 19,
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
            : [[NodeTypes.SpecialExpression, [specialExpressionTypes.block, ast.body]]];
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

    function arityAcceptsMin(arity, nbrOfParams) {
        var min = arity.min;
        if (typeof min === 'number' && nbrOfParams < min) {
            return false;
        }
        return true;
    }
    function getCommonArityFromFunctions(params) {
        return params.reduce(function (acc, param) {
            if (acc === null) {
                return null;
            }
            var arity = (typeof param === 'number' || isColl(param)) ? toFixedArity(1) : param.arity;
            var aMin = arity.min, aMax = arity.max;
            var bMin = acc.min, bMax = acc.max;
            var min = typeof aMin === 'number' && typeof bMin === 'number'
                ? Math.max(aMin, bMin)
                : typeof aMin === 'number' ? aMin : typeof bMin === 'number' ? bMin : undefined;
            var max = typeof aMax === 'number' && typeof bMax === 'number'
                ? Math.min(aMax, bMax)
                : typeof aMax === 'number' ? aMax : typeof bMax === 'number' ? bMax : undefined;
            if (typeof min === 'number' && typeof max === 'number' && min > max) {
                return null;
            }
            return { min: min, max: max };
        }, {});
    }
    function getArityFromFunction(param) {
        return (typeof param === 'number' || isColl(param)) ? toFixedArity(1) : param.arity;
    }
    function assertNumberOfParams(arity, length, sourceCodeInfo) {
        var min = arity.min, max = arity.max;
        if (typeof min === 'number' && length < min) {
            throw new LitsError("Wrong number of arguments, expected at least ".concat(min, ", got ").concat(valueToString(length), "."), sourceCodeInfo);
        }
        if (typeof max === 'number' && length > max) {
            throw new LitsError("Wrong number of arguments, expected at most ".concat(max, ", got ").concat(valueToString(length), "."), sourceCodeInfo);
        }
    }
    function canBeOperator(count) {
        if (typeof count.max === 'number' && count.max < 2) {
            return false;
        }
        if (typeof count.min === 'number' && count.min > 2) {
            return false;
        }
        return true;
    }
    function toFixedArity(arity) {
        return { min: arity, max: arity };
    }

    var bitwiseNormalExpression = {
        '<<': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], count = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true });
                return num << count;
            },
            arity: toFixedArity(2),
        },
        '>>': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], count = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true });
                return num >> count;
            },
            arity: toFixedArity(2),
        },
        '>>>': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], count = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true });
                return num >>> count;
            },
            arity: toFixedArity(2),
        },
        'bit-not': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), num = _b[0];
                assertNumber(num, sourceCodeInfo, { integer: true });
                return ~num;
            },
            arity: toFixedArity(1),
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
            arity: { min: 2 },
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
            arity: { min: 2 },
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
            arity: { min: 2 },
        },
        'xor': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), first = _b[0], rest = _b.slice(1);
                assertNumber(first, sourceCodeInfo, { integer: true });
                return rest.reduce(function (result, value) {
                    assertNumber(value, sourceCodeInfo, { integer: true });
                    return result ^ value;
                }, first);
            },
            arity: { min: 2 },
        },
        'bit-flip': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], index = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true });
                var mask = 1 << index;
                return (num ^= mask);
            },
            arity: toFixedArity(2),
        },
        'bit-set': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], index = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true });
                var mask = 1 << index;
                return (num |= mask);
            },
            arity: toFixedArity(2),
        },
        'bit-clear': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], index = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true });
                var mask = 1 << index;
                return (num &= ~mask);
            },
            arity: toFixedArity(2),
        },
        'bit-test': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], index = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true });
                var mask = 1 << index;
                return !!(num & mask);
            },
            arity: toFixedArity(2),
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
            return approxEqual(a, b);
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
                if (!deepEqual(a[key], b[key], sourceCodeInfo))
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
    var EPSILON = 1e-10;
    function approxEqual(a, b, epsilon) {
        if (epsilon === void 0) { epsilon = EPSILON; }
        if (a === b) {
            return true;
        }
        var diff = Math.abs(a - b);
        if (a === 0 || b === 0 || diff < epsilon) {
            // Use absolute error for values near zero
            return diff < epsilon;
        }
        var absA = Math.abs(a);
        var absB = Math.abs(b);
        // Use relative error for larger values
        return diff / (absA + absB) < epsilon;
    }
    function smartTrim(str, minIndent) {
        var _a, _b;
        if (minIndent === void 0) { minIndent = 0; }
        var lines = str.split('\n');
        while ((_a = lines[0]) === null || _a === void 0 ? void 0 : _a.match(/^\s*$/)) {
            lines.shift(); // Remove leading empty lines
        }
        while ((_b = lines[lines.length - 1]) === null || _b === void 0 ? void 0 : _b.match(/^\s*$/)) {
            lines.pop(); // Remove trailing empty lines
        }
        var indent = lines.reduce(function (acc, line) {
            if (line.match(/^\s*$/))
                return acc; // Skip empty lines
            var lineIndent = line.match(/^\s*/)[0].length;
            return Math.min(acc, lineIndent);
        }, Infinity);
        return lines.map(function (line) { return ' '.repeat(minIndent) + line.slice(indent); }).join('\n').trimEnd();
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

    function mapObjects(_a) {
        var colls = _a.colls, contextStack = _a.contextStack, executeFunction = _a.executeFunction, fn = _a.fn, sourceCodeInfo = _a.sourceCodeInfo;
        assertObj(colls[0], sourceCodeInfo);
        var keys = Object.keys(colls[0]);
        var params = {};
        colls.forEach(function (obj) {
            assertObj(obj, sourceCodeInfo);
            var objKeys = Object.keys(obj);
            if (objKeys.length !== keys.length) {
                throw new LitsError("All objects must have the same keys. Expected: ".concat(keys.join(', '), ". Found: ").concat(objKeys.join(', ')), sourceCodeInfo);
            }
            if (!objKeys.every(function (key) { return keys.includes(key); })) {
                throw new LitsError("All objects must have the same keys. Expected: ".concat(keys.join(', '), ". Found: ").concat(objKeys.join(', ')), sourceCodeInfo);
            }
            Object.entries(obj).forEach(function (_a) {
                var _b = __read(_a, 2), key = _b[0], value = _b[1];
                if (!params[key])
                    params[key] = [];
                params[key].push(value);
            });
        });
        return keys.reduce(function (result, key) {
            result[key] = executeFunction(fn, params[key], contextStack, sourceCodeInfo);
            return result;
        }, {});
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
        'filter': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), coll = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertColl(coll, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                if (Array.isArray(coll)) {
                    var result = coll.filter(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                    return result;
                }
                if (isString(coll)) {
                    return coll
                        .split('')
                        .filter(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); })
                        .join('');
                }
                return Object.entries(coll)
                    .filter(function (_a) {
                    var _b = __read(_a, 2), value = _b[1];
                    return executeFunction(fn, [value], contextStack, sourceCodeInfo);
                })
                    .reduce(function (result, _a) {
                    var _b = __read(_a, 2), key = _b[0], value = _b[1];
                    result[key] = value;
                    return result;
                }, {});
            },
            arity: toFixedArity(2),
        },
        'filteri': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), coll = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertColl(coll, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                if (Array.isArray(coll)) {
                    var result = coll.filter(function (elem, index) { return executeFunction(fn, [elem, index], contextStack, sourceCodeInfo); });
                    return result;
                }
                if (isString(coll)) {
                    return coll
                        .split('')
                        .filter(function (elem, index) { return executeFunction(fn, [elem, index], contextStack, sourceCodeInfo); })
                        .join('');
                }
                return Object.entries(coll)
                    .filter(function (_a) {
                    var _b = __read(_a, 2), key = _b[0], value = _b[1];
                    return executeFunction(fn, [value, key], contextStack, sourceCodeInfo);
                })
                    .reduce(function (result, _a) {
                    var _b = __read(_a, 2), key = _b[0], value = _b[1];
                    result[key] = value;
                    return result;
                }, {});
            },
            arity: toFixedArity(2),
        },
        'map': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var fn = asFunctionLike(params.at(-1), sourceCodeInfo);
                if (isObj(params[0])) {
                    return mapObjects({
                        colls: params.slice(0, -1),
                        fn: fn,
                        sourceCodeInfo: sourceCodeInfo,
                        contextStack: contextStack,
                        executeFunction: executeFunction,
                    });
                }
                var seqs = params.slice(0, -1);
                assertSeq(seqs[0], sourceCodeInfo);
                var isStr = typeof seqs[0] === 'string';
                var len = seqs[0].length;
                seqs.slice(1).forEach(function (seq) {
                    if (isStr) {
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
                if (!isStr) {
                    return mapped;
                }
                mapped.forEach(function (char) { return assertString(char, sourceCodeInfo); });
                return mapped.join('');
            },
            arity: { min: 2 },
        },
        'mapi': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), coll = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertColl(coll, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                if (Array.isArray(coll)) {
                    return coll.map(function (elem, index) { return executeFunction(fn, [elem, index], contextStack, sourceCodeInfo); });
                }
                if (isString(coll)) {
                    return coll
                        .split('')
                        .map(function (elem, index) { return executeFunction(fn, [elem, index], contextStack, sourceCodeInfo); })
                        .join('');
                }
                return Object.entries(coll)
                    .reduce(function (acc, _a) {
                    var _b = __read(_a, 2), key = _b[0], value = _b[1];
                    acc[key] = executeFunction(fn, [value, key], contextStack, sourceCodeInfo);
                    return acc;
                }, {});
            },
            arity: toFixedArity(2),
        },
        'reduce': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 3), coll = _c[0], fn = _c[1], initial = _c[2];
                var executeFunction = _b.executeFunction;
                assertColl(coll, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                assertAny(initial, sourceCodeInfo);
                if (typeof coll === 'string') {
                    assertString(initial, sourceCodeInfo);
                    if (coll.length === 0)
                        return initial;
                    return coll.split('').reduce(function (result, elem) {
                        return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                    }, initial);
                }
                else if (Array.isArray(coll)) {
                    if (coll.length === 0)
                        return initial;
                    return coll.reduce(function (result, elem) {
                        return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                    }, initial);
                }
                else {
                    if (Object.keys(coll).length === 0)
                        return initial;
                    return Object.entries(coll).reduce(function (result, _a) {
                        var _b = __read(_a, 2), elem = _b[1];
                        return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                    }, initial);
                }
            },
            arity: toFixedArity(3),
        },
        'reducei': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 3), coll = _c[0], fn = _c[1], initial = _c[2];
                var executeFunction = _b.executeFunction;
                assertColl(coll, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                assertAny(initial, sourceCodeInfo);
                if (typeof coll === 'string') {
                    assertString(initial, sourceCodeInfo);
                    if (coll.length === 0)
                        return initial;
                    return coll.split('').reduce(function (result, elem, index) {
                        return executeFunction(fn, [result, elem, index], contextStack, sourceCodeInfo);
                    }, initial);
                }
                else if (Array.isArray(coll)) {
                    if (coll.length === 0)
                        return initial;
                    return coll.reduce(function (result, elem, index) {
                        return executeFunction(fn, [result, elem, index], contextStack, sourceCodeInfo);
                    }, initial);
                }
                else {
                    if (Object.keys(coll).length === 0)
                        return initial;
                    return Object.entries(coll).reduce(function (result, _a) {
                        var _b = __read(_a, 2), key = _b[0], elem = _b[1];
                        return executeFunction(fn, [result, elem, key], contextStack, sourceCodeInfo);
                    }, initial);
                }
            },
            arity: toFixedArity(3),
        },
        'reduce-right': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 3), coll = _c[0], fn = _c[1], initial = _c[2];
                var executeFunction = _b.executeFunction;
                assertColl(coll, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                assertAny(initial, sourceCodeInfo);
                if (typeof coll === 'string') {
                    if (coll.length === 0)
                        return initial;
                    return coll.split('').reduceRight(function (result, elem) {
                        return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                    }, initial);
                }
                else if (Array.isArray(coll)) {
                    if (coll.length === 0)
                        return initial;
                    return coll.reduceRight(function (result, elem) {
                        return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                    }, initial);
                }
                else {
                    if (Object.keys(coll).length === 0)
                        return initial;
                    return Object.entries(coll).reduceRight(function (result, _a) {
                        var _b = __read(_a, 2), elem = _b[1];
                        return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                    }, initial);
                }
            },
            arity: toFixedArity(3),
        },
        'reducei-right': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 3), coll = _c[0], fn = _c[1], initial = _c[2];
                var executeFunction = _b.executeFunction;
                assertColl(coll, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                assertAny(initial, sourceCodeInfo);
                if (typeof coll === 'string') {
                    if (coll.length === 0)
                        return initial;
                    return coll.split('').reduceRight(function (result, elem, index) {
                        return executeFunction(fn, [result, elem, index], contextStack, sourceCodeInfo);
                    }, initial);
                }
                else if (Array.isArray(coll)) {
                    if (coll.length === 0)
                        return initial;
                    return coll.reduceRight(function (result, elem, index) {
                        return executeFunction(fn, [result, elem, index], contextStack, sourceCodeInfo);
                    }, initial);
                }
                else {
                    if (Object.keys(coll).length === 0)
                        return initial;
                    return Object.entries(coll).reduceRight(function (result, _a) {
                        var _b = __read(_a, 2), key = _b[0], elem = _b[1];
                        return executeFunction(fn, [result, elem, key], contextStack, sourceCodeInfo);
                    }, initial);
                }
            },
            arity: toFixedArity(3),
        },
        'reductions': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 3), coll = _c[0], fn = _c[1], initial = _c[2];
                var executeFunction = _b.executeFunction;
                assertColl(coll, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                assertAny(initial, sourceCodeInfo);
                assertAny(initial, sourceCodeInfo);
                if (typeof coll === 'string') {
                    assertString(initial, sourceCodeInfo);
                    if (coll.length === 0)
                        return [initial];
                    var resultArray_1 = [initial];
                    coll.split('').reduce(function (result, elem) {
                        var newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                        resultArray_1.push(newVal);
                        return newVal;
                    }, initial);
                    return resultArray_1;
                }
                else if (Array.isArray(coll)) {
                    if (coll.length === 0)
                        return [initial];
                    var resultArray_2 = [initial];
                    coll.reduce(function (result, elem) {
                        var newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                        resultArray_2.push(newVal);
                        return newVal;
                    }, initial);
                    return resultArray_2;
                }
                else {
                    if (Object.keys(coll).length === 0)
                        return [initial];
                    var resultArray_3 = [initial];
                    Object.entries(coll).reduce(function (result, _a) {
                        var _b = __read(_a, 2), elem = _b[1];
                        var newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                        resultArray_3.push(newVal);
                        return newVal;
                    }, initial);
                    return resultArray_3;
                }
            },
            arity: toFixedArity(3),
        },
        'reductionsi': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 3), coll = _c[0], fn = _c[1], initial = _c[2];
                var executeFunction = _b.executeFunction;
                assertColl(coll, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                assertAny(initial, sourceCodeInfo);
                assertAny(initial, sourceCodeInfo);
                if (typeof coll === 'string') {
                    assertString(initial, sourceCodeInfo);
                    if (coll.length === 0)
                        return [initial];
                    var resultArray_4 = [initial];
                    coll.split('').reduce(function (result, elem, index) {
                        var newVal = executeFunction(fn, [result, elem, index], contextStack, sourceCodeInfo);
                        resultArray_4.push(newVal);
                        return newVal;
                    }, initial);
                    return resultArray_4;
                }
                else if (Array.isArray(coll)) {
                    if (coll.length === 0)
                        return [initial];
                    var resultArray_5 = [initial];
                    coll.reduce(function (result, elem, index) {
                        var newVal = executeFunction(fn, [result, elem, index], contextStack, sourceCodeInfo);
                        resultArray_5.push(newVal);
                        return newVal;
                    }, initial);
                    return resultArray_5;
                }
                else {
                    if (Object.keys(coll).length === 0)
                        return [initial];
                    var resultArray_6 = [initial];
                    Object.entries(coll).reduce(function (result, _a) {
                        var _b = __read(_a, 2), key = _b[0], elem = _b[1];
                        var newVal = executeFunction(fn, [result, elem, key], contextStack, sourceCodeInfo);
                        resultArray_6.push(newVal);
                        return newVal;
                    }, initial);
                    return resultArray_6;
                }
            },
            arity: toFixedArity(3),
        },
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
            arity: { min: 2, max: 3 },
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
            arity: { min: 2, max: 3 },
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
            arity: toFixedArity(1),
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
            arity: toFixedArity(2),
        },
        'assoc': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), coll = _b[0], key = _b[1], value = _b[2];
                assertColl(coll, sourceCodeInfo);
                assertStringOrNumber(key, sourceCodeInfo);
                assertAny(value, sourceCodeInfo);
                return assoc(coll, key, value, sourceCodeInfo);
            },
            arity: toFixedArity(3),
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
            arity: toFixedArity(3),
        },
        'update': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a), coll = _c[0], key = _c[1], fn = _c[2], params = _c.slice(3);
                var executeFunction = _b.executeFunction;
                assertColl(coll, sourceCodeInfo);
                assertStringOrNumber(key, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                return update(coll, key, fn, params, contextStack, executeFunction, sourceCodeInfo);
            },
            arity: { min: 3 },
        },
        'update-in': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a), originalColl = _c[0], keys = _c[1], fn = _c[2], params = _c.slice(3);
                var executeFunction = _b.executeFunction;
                assertColl(originalColl, sourceCodeInfo);
                assertArray(keys, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
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
            arity: { min: 3 },
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
            arity: { min: 1 },
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
            arity: toFixedArity(1),
        },
        'every?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), coll = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertColl(coll, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                if (Array.isArray(coll))
                    return coll.every(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                if (typeof coll === 'string')
                    return coll.split('').every(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                return Object.entries(coll).every(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
            },
            arity: toFixedArity(2),
        },
        'any?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), coll = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertFunctionLike(fn, sourceCodeInfo);
                assertColl(coll, sourceCodeInfo);
                if (Array.isArray(coll))
                    return coll.some(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                if (typeof coll === 'string')
                    return coll.split('').some(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                return Object.entries(coll).some(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
            },
            arity: toFixedArity(2),
        },
        'not-any?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), coll = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertFunctionLike(fn, sourceCodeInfo);
                assertColl(coll, sourceCodeInfo);
                if (Array.isArray(coll))
                    return !coll.some(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                if (typeof coll === 'string')
                    return !coll.split('').some(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                return !Object.entries(coll).some(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
            },
            arity: toFixedArity(2),
        },
        'not-every?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), coll = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertFunctionLike(fn, sourceCodeInfo);
                assertColl(coll, sourceCodeInfo);
                if (Array.isArray(coll))
                    return !coll.every(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                if (typeof coll === 'string')
                    return !coll.split('').every(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                return !Object.entries(coll).every(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
            },
            arity: toFixedArity(2),
        },
    };

    var arrayNormalExpression = {
        'range': {
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
            arity: { min: 1, max: 3 },
        },
        'repeat': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), value = _b[0], count = _b[1];
                assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true });
                var result = [];
                for (var i = 0; i < count; i += 1)
                    result.push(value);
                return result;
            },
            arity: toFixedArity(2),
        },
        'flatten': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), seq = _b[0], depth = _b[1];
                assertArray(seq, sourceCodeInfo);
                var actualDepth = depth === undefined || depth === Number.POSITIVE_INFINITY
                    ? Number.POSITIVE_INFINITY
                    : asNumber(depth, sourceCodeInfo, { integer: true, nonNegative: true });
                return seq.flat(actualDepth);
            },
            arity: { min: 1, max: 2 },
        },
        'mapcat': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), arr = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertArray(arr, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                return arr.map(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); }).flat(1);
            },
            arity: toFixedArity(2),
        },
        'moving-fn': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 3), arr = _c[0], windowSize = _c[1], fn = _c[2];
                var executeFunction = _b.executeFunction;
                assertArray(arr, sourceCodeInfo);
                assertNumber(windowSize, sourceCodeInfo, { integer: true, lte: arr.length });
                assertFunctionLike(fn, sourceCodeInfo);
                var result = [];
                for (var i = 0; i <= arr.length - windowSize; i++) {
                    var window_1 = arr.slice(i, i + windowSize);
                    var value = executeFunction(fn, [window_1], contextStack, sourceCodeInfo);
                    result.push(value);
                }
                return result;
            },
            arity: toFixedArity(3),
        },
        'running-fn': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), arr = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertArray(arr, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                var result = [];
                for (var i = 0; i < arr.length; i += 1) {
                    var subArr = arr.slice(0, i + 1);
                    result.push(executeFunction(fn, [subArr], contextStack, sourceCodeInfo));
                }
                return result;
            },
            arity: toFixedArity(2),
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
                if (i >= 0 && i < seq.length) {
                    var result = toAny(seq[i]);
                    return result;
                }
                else {
                    return defaultValue;
                }
            },
            arity: { min: 2, max: 3 },
        },
        'first': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), array = _b[0];
                if (array === null)
                    return null;
                assertSeq(array, sourceCodeInfo);
                var result = toAny(array[0]);
                return result;
            },
            arity: toFixedArity(1),
        },
        'last': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), array = _b[0];
                if (array === null)
                    return null;
                assertSeq(array, sourceCodeInfo);
                var result = toAny(array.at(-1));
                return result;
            },
            arity: toFixedArity(1),
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
            arity: toFixedArity(1),
        },
        'position': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), seq = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertFunctionLike(fn, sourceCodeInfo);
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
            arity: toFixedArity(2),
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
            arity: toFixedArity(2),
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
            arity: toFixedArity(2),
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
            arity: { min: 2 },
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
            arity: toFixedArity(1),
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
            arity: toFixedArity(1),
        },
        'reverse': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), seq = _b[0];
                if (seq === null)
                    return null;
                assertSeq(seq, sourceCodeInfo);
                if (Array.isArray(seq)) {
                    return __spreadArray([], __read(seq), false).reverse();
                }
                return seq.split('').reverse().join('');
            },
            arity: toFixedArity(1),
        },
        'second': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), seq = _b[0];
                if (seq === null)
                    return null;
                assertSeq(seq, sourceCodeInfo);
                return toAny(seq[1]);
            },
            arity: toFixedArity(1),
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
            arity: toFixedArity(1),
        },
        'slice': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(params, 3), seq = _a[0], from = _a[1], to = _a[2];
                assertSeq(seq, sourceCodeInfo);
                assertNumber(from, sourceCodeInfo, { integer: true });
                if (params.length === 2) {
                    if (Array.isArray(seq)) {
                        return seq.slice(from);
                    }
                    return seq.slice(from);
                }
                assertNumber(to, sourceCodeInfo, { integer: true });
                if (Array.isArray(seq)) {
                    return seq.slice(from, to);
                }
                return seq.slice(from, to);
            },
            arity: { min: 2, max: 3 },
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
            arity: { min: 3 },
        },
        'some': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c;
                var _d = __read(_a, 2), seq = _d[0], fn = _d[1];
                var executeFunction = _b.executeFunction;
                assertFunctionLike(fn, sourceCodeInfo);
                if (seq === null)
                    return null;
                assertSeq(seq, sourceCodeInfo);
                if (seq.length === 0)
                    return null;
                if (typeof seq === 'string')
                    return (_c = seq.split('').find(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); })) !== null && _c !== void 0 ? _c : null;
                return toAny(seq.find(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); }));
            },
            arity: toFixedArity(2),
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
                        assertFunctionLike(comparer, sourceCodeInfo);
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
                        assertFunctionLike(comparer, sourceCodeInfo);
                        var compareValue = executeFunction(comparer, [a, b], contextStack, sourceCodeInfo);
                        assertNumber(compareValue, sourceCodeInfo, { finite: true });
                        return compareValue;
                    });
                }
                return result;
            },
            arity: { min: 1, max: 2 },
        },
        'sort-by': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var _b = __read(params, 2), seq = _b[0], keyfn = _b[1];
                var defaultComparer = params.length === 2;
                assertSeq(seq, sourceCodeInfo);
                assertFunctionLike(keyfn, sourceCodeInfo);
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
                        assertFunctionLike(comparer, sourceCodeInfo);
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
                    assertFunctionLike(comparer, sourceCodeInfo);
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
            arity: { min: 2, max: 3 },
        },
        'take': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), input = _b[0], n = _b[1];
                assertNumber(n, sourceCodeInfo);
                assertSeq(input, sourceCodeInfo);
                var num = Math.max(Math.ceil(n), 0);
                return input.slice(0, num);
            },
            arity: toFixedArity(2),
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
            arity: toFixedArity(2),
        },
        'take-while': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_1, _c;
                var _d = __read(_a, 2), seq = _d[0], fn = _d[1];
                var executeFunction = _b.executeFunction;
                assertSeq(seq, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
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
            arity: toFixedArity(2),
        },
        'drop': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), input = _b[0], n = _b[1];
                assertNumber(n, sourceCodeInfo);
                var num = Math.max(Math.ceil(n), 0);
                assertSeq(input, sourceCodeInfo);
                return input.slice(num);
            },
            arity: toFixedArity(2),
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
            arity: toFixedArity(2),
        },
        'drop-while': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), seq = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertSeq(seq, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                if (Array.isArray(seq)) {
                    var from_1 = seq.findIndex(function (elem) { return !executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                    return seq.slice(from_1);
                }
                var charArray = seq.split('');
                var from = charArray.findIndex(function (elem) { return !executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                return charArray.slice(from).join('');
            },
            arity: toFixedArity(2),
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
            arity: { min: 2 },
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
            arity: toFixedArity(1),
        },
        'remove': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), input = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertFunctionLike(fn, sourceCodeInfo);
                assertSeq(input, sourceCodeInfo);
                if (Array.isArray(input))
                    return input.filter(function (elem) { return !executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                return input
                    .split('')
                    .filter(function (elem) { return !executeFunction(fn, [elem], contextStack, sourceCodeInfo); })
                    .join('');
            },
            arity: toFixedArity(2),
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
            arity: toFixedArity(2),
        },
        'split-at': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), seq = _b[0], pos = _b[1];
                assertNumber(pos, sourceCodeInfo, { integer: true });
                assertSeq(seq, sourceCodeInfo);
                var at = pos < 0 ? seq.length + pos : pos;
                return [seq.slice(0, at), seq.slice(at)];
            },
            arity: toFixedArity(2),
        },
        'split-with': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), seq = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertFunctionLike(fn, sourceCodeInfo);
                assertSeq(seq, sourceCodeInfo);
                var seqIsArray = Array.isArray(seq);
                var arr = seqIsArray ? seq : seq.split('');
                var index = arr.findIndex(function (elem) { return !executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                if (index === -1)
                    return [seq, seqIsArray ? [] : ''];
                return [seq.slice(0, index), seq.slice(index)];
            },
            arity: toFixedArity(2),
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
            arity: toFixedArity(1),
        },
        'group-by': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), seq = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertFunctionLike(fn, sourceCodeInfo);
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
            arity: toFixedArity(2),
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
            arity: { min: 2, max: 4 },
        },
        'partition-all': {
            evaluate: function (params, sourceCodeInfo) {
                var seq = asSeq(params[0], sourceCodeInfo);
                var n = toNonNegativeInteger(asNumber(params[1], sourceCodeInfo));
                var step = params.length === 3 ? toNonNegativeInteger(asNumber(params[2], sourceCodeInfo)) : n;
                return partition(n, step, seq, [], sourceCodeInfo);
            },
            arity: { min: 2, max: 3 },
        },
        'partition-by': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), seq = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertFunctionLike(fn, sourceCodeInfo);
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
            arity: toFixedArity(2),
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
            arity: toFixedArity(2),
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
            arity: toFixedArity(2),
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
            arity: { min: 1 },
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
            arity: toFixedArity(2),
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

    function getNumberVectorOrMatrixOperation(params, sourceCodeInfo) {
        var e_1, _a, e_2, _b, e_3, _c;
        var hasVector = false;
        var hasMatrix = false;
        try {
            for (var params_1 = __values(params), params_1_1 = params_1.next(); !params_1_1.done; params_1_1 = params_1.next()) {
                var param = params_1_1.value;
                if (isVector(param)) {
                    hasVector = true;
                }
                else if (isMatrix(param)) {
                    hasMatrix = true;
                }
                else if (!isNumber(param)) {
                    throw new LitsError("Invalid parameter type: ".concat(typeof param), sourceCodeInfo);
                }
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (params_1_1 && !params_1_1.done && (_a = params_1.return)) _a.call(params_1);
            }
            finally { if (e_1) throw e_1.error; }
        }
        if (hasMatrix) {
            if (hasVector) {
                throw new LitsError('Cannot mix vector and matrix types', sourceCodeInfo);
            }
            var rows_1 = null;
            var cold_1 = null;
            try {
                for (var params_2 = __values(params), params_2_1 = params_2.next(); !params_2_1.done; params_2_1 = params_2.next()) {
                    var param = params_2_1.value;
                    if (isMatrix(param)) {
                        if (rows_1 === null) {
                            rows_1 = param.length;
                            cold_1 = param[0].length;
                        }
                        else {
                            if (param.length !== rows_1 || param[0].length !== cold_1) {
                                throw new LitsError('Matrix dimensions do not match', sourceCodeInfo);
                            }
                        }
                    }
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (params_2_1 && !params_2_1.done && (_b = params_2.return)) _b.call(params_2);
                }
                finally { if (e_2) throw e_2.error; }
            }
            var matrices = params.map(function (param) {
                if (isMatrix(param)) {
                    return param;
                }
                return Array.from({ length: rows_1 }, function () { return Array.from({ length: cold_1 }, function () { return param; }); });
            });
            return ['matrix', matrices];
        }
        if (hasVector) {
            var length_1 = null;
            try {
                for (var params_3 = __values(params), params_3_1 = params_3.next(); !params_3_1.done; params_3_1 = params_3.next()) {
                    var param = params_3_1.value;
                    if (isVector(param)) {
                        if (length_1 === null) {
                            length_1 = param.length;
                        }
                        else {
                            if (param.length !== length_1) {
                                throw new LitsError('Vector lengths do not match', sourceCodeInfo);
                            }
                        }
                    }
                }
            }
            catch (e_3_1) { e_3 = { error: e_3_1 }; }
            finally {
                try {
                    if (params_3_1 && !params_3_1.done && (_c = params_3.return)) _c.call(params_3);
                }
                finally { if (e_3) throw e_3.error; }
            }
            var vectors = params.map(function (param) {
                if (isVector(param)) {
                    return param;
                }
                return Array.from({ length: length_1 }, function () { return param; });
            });
            return ['vector', vectors];
        }
        return ['number', params];
    }
    var mathNormalExpression = {
        'inc': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return operands[0] + 1;
                }
                else if (operation === 'vector') {
                    var firstVector = operands[0];
                    return firstVector.map(function (val) { return val + 1; });
                }
                else {
                    var firstMatrix = operands[0];
                    return firstMatrix.map(function (row) { return row.map(function (val) { return val + 1; }); });
                }
            },
            arity: toFixedArity(1),
        },
        'dec': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return operands[0] - 1;
                }
                else if (operation === 'vector') {
                    var firstVector = operands[0];
                    return firstVector.map(function (val) { return val - 1; });
                }
                else {
                    var firstMatrix = operands[0];
                    return firstMatrix.map(function (row) { return row.map(function (val) { return val - 1; }); });
                }
            },
            arity: toFixedArity(1),
        },
        '+': {
            evaluate: function (params, sourceCodeInfo) {
                if (params.length === 0) {
                    return 0;
                }
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return operands.reduce(function (result, param) { return result + (param); }, 0);
                }
                else if (operation === 'vector') {
                    var firstVector = operands[0];
                    var restVectors = operands.slice(1);
                    return restVectors.reduce(function (acc, vector) { return acc.map(function (val, i) { return val + vector[i]; }); }, firstVector);
                }
                else {
                    var firstMatrix = operands[0];
                    var restMatrices = operands.slice(1);
                    return restMatrices.reduce(function (acc, matrix) { return acc.map(function (row, i) { return row.map(function (val, j) { return val + matrix[i][j]; }); }); }, firstMatrix);
                }
            },
            arity: {},
        },
        '*': {
            evaluate: function (params, sourceCodeInfo) {
                if (params.length === 0) {
                    return 1;
                }
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return operands.reduce(function (result, param) { return result * (param); }, 1);
                }
                else if (operation === 'vector') {
                    var firstVector = operands[0];
                    var restVectors = operands.slice(1);
                    return restVectors.reduce(function (acc, vector) { return acc.map(function (val, i) { return val * vector[i]; }); }, firstVector);
                }
                else {
                    var firstMatrix = operands[0];
                    var restMatrices = operands.slice(1);
                    return restMatrices.reduce(function (acc, matrix) { return acc.map(function (row, i) { return row.map(function (val, j) { return val * matrix[i][j]; }); }); }, firstMatrix);
                }
            },
            aliases: ['·'],
            arity: {},
        },
        '/': {
            evaluate: function (params, sourceCodeInfo) {
                if (params.length === 0) {
                    return 1;
                }
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    var _b = __read(operands), first = _b[0], rest = _b.slice(1);
                    if (rest.length === 0) {
                        return 1 / first;
                    }
                    return rest.reduce(function (result, param) {
                        return result / param;
                    }, first);
                }
                else if (operation === 'vector') {
                    var firstVector = operands[0];
                    var restVectors = operands.slice(1);
                    return restVectors.reduce(function (acc, vector) { return acc.map(function (val, i) { return val / vector[i]; }); }, firstVector);
                }
                else {
                    var firstMatrix = operands[0];
                    var restMatrices = operands.slice(1);
                    return restMatrices.reduce(function (acc, matrix) { return acc.map(function (row, i) { return row.map(function (val, j) { return val / matrix[i][j]; }); }); }, firstMatrix);
                }
            },
            arity: {},
        },
        '-': {
            evaluate: function (params, sourceCodeInfo) {
                if (params.length === 0) {
                    return 0;
                }
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    var _b = __read(operands), first = _b[0], rest = _b.slice(1);
                    if (rest.length === 0)
                        return -first;
                    return rest.reduce(function (result, param) {
                        return result - param;
                    }, first);
                }
                else if (operation === 'vector') {
                    var firstVector = operands[0];
                    var restVectors = operands.slice(1);
                    return restVectors.reduce(function (acc, vector) { return acc.map(function (val, i) { return val - vector[i]; }); }, firstVector);
                }
                else {
                    var firstMatrix = operands[0];
                    var restMatrices = operands.slice(1);
                    return restMatrices.reduce(function (acc, matrix) { return acc.map(function (row, i) { return row.map(function (val, j) { return val - matrix[i][j]; }); }); }, firstMatrix);
                }
            },
            arity: {},
        },
        'quot': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.trunc(operands[0] / operands[1]);
                }
                else if (operation === 'vector') {
                    var firstVector = operands[0];
                    var secondVector_1 = operands[1];
                    return firstVector.map(function (val, i) { return Math.trunc(val / secondVector_1[i]); });
                }
                else {
                    var firstMatrix = operands[0];
                    var secondMatrix_1 = operands[1];
                    return firstMatrix.map(function (row, i) { return row.map(function (val, j) { return Math.trunc(val / secondMatrix_1[i][j]); }); });
                }
            },
            arity: toFixedArity(2),
        },
        'mod': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    var quotient = Math.floor(operands[0] / operands[1]);
                    return operands[0] - operands[1] * quotient;
                }
                else if (operation === 'vector') {
                    var firstVector = operands[0];
                    var secondVector_2 = operands[1];
                    return firstVector.map(function (dividend, i) {
                        var divisor = secondVector_2[i];
                        var quotient = Math.floor(dividend / divisor);
                        return dividend - divisor * quotient;
                    });
                }
                else {
                    var firstMatrix = operands[0];
                    var secondMatrix_2 = operands[1];
                    return firstMatrix.map(function (row, i) { return row.map(function (val, j) {
                        var quotient = Math.floor(val / secondMatrix_2[i][j]);
                        return val - secondMatrix_2[i][j] * quotient;
                    }); });
                }
            },
            arity: toFixedArity(2),
        },
        '%': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return operands[0] % operands[1];
                }
                else if (operation === 'vector') {
                    var firstVector = operands[0];
                    var secondVector_3 = operands[1];
                    return firstVector.map(function (dividend, i) { return dividend % secondVector_3[i]; });
                }
                else {
                    var firstMatrix = operands[0];
                    var secondMatrix_3 = operands[1];
                    return firstMatrix.map(function (row, i) { return row.map(function (dividend, j) { return dividend % secondMatrix_3[i][j]; }); });
                }
            },
            arity: toFixedArity(2),
            aliases: ['rem'],
        },
        'sqrt': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.sqrt(operands[0]);
                }
                else if (operation === 'vector') {
                    var firstVector = operands[0];
                    return firstVector.map(function (val) { return Math.sqrt(val); });
                }
                else {
                    var firstMatrix = operands[0];
                    return firstMatrix.map(function (row) { return row.map(function (val) { return Math.sqrt(val); }); });
                }
            },
            arity: toFixedArity(1),
            aliases: ['√'],
        },
        'cbrt': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.cbrt(operands[0]);
                }
                else if (operation === 'vector') {
                    var firstVector = operands[0];
                    return firstVector.map(function (val) { return Math.cbrt(val); });
                }
                else {
                    var firstMatrix = operands[0];
                    return firstMatrix.map(function (row) { return row.map(function (val) { return Math.cbrt(val); }); });
                }
            },
            arity: toFixedArity(1),
            aliases: ['∛'],
        },
        '^': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.pow(operands[0], operands[1]);
                }
                else if (operation === 'vector') {
                    var firstVector = operands[0];
                    var secondVector_4 = operands[1];
                    return firstVector.map(function (base, i) { return Math.pow(base, secondVector_4[i]); });
                }
                else {
                    var firstMatrix = operands[0];
                    var secondMatrix_4 = operands[1];
                    return firstMatrix.map(function (row, i) { return row.map(function (base, j) { return Math.pow(base, secondMatrix_4[i][j]); }); });
                }
            },
            arity: toFixedArity(2),
        },
        'round': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), value = _b[0], decimals = _b[1];
                var _c = __read(getNumberVectorOrMatrixOperation([value], sourceCodeInfo), 2), operation = _c[0], operands = _c[1];
                if (operation === 'number') {
                    if (decimals === undefined || decimals === 0) {
                        return Math.round(operands[0]);
                    }
                    else {
                        assertNumber(decimals, sourceCodeInfo, { integer: true, positive: true });
                        var factor = Math.pow(10, decimals);
                        return Math.round(operands[0] * factor) / factor;
                    }
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    if (decimals === undefined || decimals === 0) {
                        return vector.map(function (val) { return Math.round(val); });
                    }
                    else {
                        assertNumber(decimals, sourceCodeInfo, { integer: true, positive: true });
                        var factor_1 = Math.pow(10, decimals);
                        return vector.map(function (val) { return Math.round(val * factor_1) / factor_1; });
                    }
                }
                else {
                    var matrix = operands[0];
                    if (decimals === undefined || decimals === 0) {
                        return matrix.map(function (row) { return row.map(function (val) { return Math.round(val); }); });
                    }
                    else {
                        assertNumber(decimals, sourceCodeInfo, { integer: true, positive: true });
                        var factor_2 = Math.pow(10, decimals);
                        return matrix.map(function (row) { return row.map(function (val) { return Math.round(val * factor_2) / factor_2; }); });
                    }
                }
            },
            arity: { min: 1, max: 2 },
        },
        'trunc': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.trunc(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.trunc(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.trunc(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'floor': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.floor(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.floor(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.floor(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'ceil': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.ceil(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.ceil(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.ceil(val); }); });
                }
            },
            arity: toFixedArity(1),
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
            arity: { min: 1 },
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
            arity: { min: 1 },
        },
        'abs': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.abs(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.abs(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.abs(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'sign': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.sign(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.sign(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.sign(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'ln': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.log(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.log(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.log(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'log2': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.log2(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.log2(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.log2(val); }); });
                }
            },
            arity: toFixedArity(1),
            aliases: ['log₂'],
        },
        'log10': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.log10(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.log10(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.log10(val); }); });
                }
            },
            arity: toFixedArity(1),
            aliases: ['log₁₀'],
        },
        'sin': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.sin(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.sin(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.sin(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'asin': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.asin(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.asin(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.asin(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'sinh': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.sinh(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.sinh(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.sinh(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'asinh': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.asinh(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.asinh(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.asinh(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'cos': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.cos(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.cos(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.cos(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'acos': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.acos(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.acos(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.acos(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'cosh': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.cosh(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.cosh(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.cosh(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'acosh': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.acosh(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.acosh(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.acosh(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'tan': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.tan(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.tan(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.tan(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'atan': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.atan(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.atan(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.atan(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'tanh': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.tanh(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.tanh(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.tanh(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'atanh': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return Math.atanh(operands[0]);
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return Math.atanh(val); });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return Math.atanh(val); }); });
                }
            },
            arity: toFixedArity(1),
        },
        'to-rad': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return (operands[0] * Math.PI) / 180;
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return (val * Math.PI) / 180; });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return (val * Math.PI) / 180; }); });
                }
            },
            arity: toFixedArity(1),
        },
        'to-deg': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(getNumberVectorOrMatrixOperation(params, sourceCodeInfo), 2), operation = _a[0], operands = _a[1];
                if (operation === 'number') {
                    return (operands[0] * 180) / Math.PI;
                }
                else if (operation === 'vector') {
                    var vector = operands[0];
                    return vector.map(function (val) { return (val * 180) / Math.PI; });
                }
                else {
                    var matrix = operands[0];
                    return matrix.map(function (row) { return row.map(function (val) { return (val * 180) / Math.PI; }); });
                }
            },
            arity: toFixedArity(1),
        },
    };

    /**
     * Global registry for Lits namespaces.
     * Namespaces are registered here and can be imported via import("namespaceName")
     */
    var namespaceRegistry = new Map();
    /**
     * Register a namespace so it can be imported in Lits code.
     * @param namespace The namespace to register
     */
    function registerNamespace(namespace) {
        if (namespaceRegistry.has(namespace.name)) {
            throw new Error("Namespace '".concat(namespace.name, "' is already registered"));
        }
        namespaceRegistry.set(namespace.name, namespace);
    }
    /**
     * Get a registered namespace by name.
     * @param name The namespace name
     * @returns The namespace or undefined if not found
     */
    function getNamespace(name) {
        return namespaceRegistry.get(name);
    }

    /**
     * Creates a grid from a flat array with specified dimensions
     *
     * @param flatArray The flat array of values
     * @param rows Number of rows in the resulting grid
     * @returns A 2D array representing the grid
     */
    function fromArray(flatArray, rows) {
        // Create the grid
        var grid = [];
        var cols = flatArray.length / rows;
        // Reshape the flat array into rows and columns
        for (var i = 0; i < rows; i++) {
            var start = i * cols;
            var end = start + cols;
            grid.push(flatArray.slice(start, end));
        }
        return grid;
    }

    function transpose(grid) {
        var result = [];
        for (var i = 0; i < grid[0].length; i += 1) {
            var row = [];
            for (var j = 0; j < grid.length; j += 1) {
                row.push(grid[j][i]);
            }
            result.push(row);
        }
        return result;
    }

    var gridFunctions = {
        'every?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_1, _c, e_2, _d;
                var _e = __read(_a, 2), grid = _e[0], predicate = _e[1];
                var executeFunction = _b.executeFunction;
                assertGrid(grid, sourceCodeInfo);
                assertFunctionLike(predicate, sourceCodeInfo);
                try {
                    for (var grid_1 = __values(grid), grid_1_1 = grid_1.next(); !grid_1_1.done; grid_1_1 = grid_1.next()) {
                        var row = grid_1_1.value;
                        try {
                            for (var row_1 = (e_2 = void 0, __values(row)), row_1_1 = row_1.next(); !row_1_1.done; row_1_1 = row_1.next()) {
                                var cell = row_1_1.value;
                                if (!executeFunction(predicate, [cell], contextStack, sourceCodeInfo)) {
                                    return false;
                                }
                            }
                        }
                        catch (e_2_1) { e_2 = { error: e_2_1 }; }
                        finally {
                            try {
                                if (row_1_1 && !row_1_1.done && (_d = row_1.return)) _d.call(row_1);
                            }
                            finally { if (e_2) throw e_2.error; }
                        }
                    }
                }
                catch (e_1_1) { e_1 = { error: e_1_1 }; }
                finally {
                    try {
                        if (grid_1_1 && !grid_1_1.done && (_c = grid_1.return)) _c.call(grid_1);
                    }
                    finally { if (e_1) throw e_1.error; }
                }
                return true;
            },
            arity: toFixedArity(2),
        },
        'some?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_3, _c, e_4, _d;
                var _e = __read(_a, 2), grid = _e[0], predicate = _e[1];
                var executeFunction = _b.executeFunction;
                assertGrid(grid, sourceCodeInfo);
                assertFunctionLike(predicate, sourceCodeInfo);
                try {
                    for (var grid_2 = __values(grid), grid_2_1 = grid_2.next(); !grid_2_1.done; grid_2_1 = grid_2.next()) {
                        var row = grid_2_1.value;
                        try {
                            for (var row_2 = (e_4 = void 0, __values(row)), row_2_1 = row_2.next(); !row_2_1.done; row_2_1 = row_2.next()) {
                                var cell = row_2_1.value;
                                if (executeFunction(predicate, [cell], contextStack, sourceCodeInfo)) {
                                    return true;
                                }
                            }
                        }
                        catch (e_4_1) { e_4 = { error: e_4_1 }; }
                        finally {
                            try {
                                if (row_2_1 && !row_2_1.done && (_d = row_2.return)) _d.call(row_2);
                            }
                            finally { if (e_4) throw e_4.error; }
                        }
                    }
                }
                catch (e_3_1) { e_3 = { error: e_3_1 }; }
                finally {
                    try {
                        if (grid_2_1 && !grid_2_1.done && (_c = grid_2.return)) _c.call(grid_2);
                    }
                    finally { if (e_3) throw e_3.error; }
                }
                return false;
            },
            arity: toFixedArity(2),
        },
        'every-row?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_5, _c;
                var _d = __read(_a, 2), grid = _d[0], predicate = _d[1];
                var executeFunction = _b.executeFunction;
                assertGrid(grid, sourceCodeInfo);
                assertFunctionLike(predicate, sourceCodeInfo);
                try {
                    for (var grid_3 = __values(grid), grid_3_1 = grid_3.next(); !grid_3_1.done; grid_3_1 = grid_3.next()) {
                        var row = grid_3_1.value;
                        if (!executeFunction(predicate, [row], contextStack, sourceCodeInfo)) {
                            return false;
                        }
                    }
                }
                catch (e_5_1) { e_5 = { error: e_5_1 }; }
                finally {
                    try {
                        if (grid_3_1 && !grid_3_1.done && (_c = grid_3.return)) _c.call(grid_3);
                    }
                    finally { if (e_5) throw e_5.error; }
                }
                return true;
            },
            arity: toFixedArity(2),
        },
        'some-row?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_6, _c;
                var _d = __read(_a, 2), grid = _d[0], predicate = _d[1];
                var executeFunction = _b.executeFunction;
                assertGrid(grid, sourceCodeInfo);
                assertFunctionLike(predicate, sourceCodeInfo);
                try {
                    for (var grid_4 = __values(grid), grid_4_1 = grid_4.next(); !grid_4_1.done; grid_4_1 = grid_4.next()) {
                        var row = grid_4_1.value;
                        if (executeFunction(predicate, [row], contextStack, sourceCodeInfo)) {
                            return true;
                        }
                    }
                }
                catch (e_6_1) { e_6 = { error: e_6_1 }; }
                finally {
                    try {
                        if (grid_4_1 && !grid_4_1.done && (_c = grid_4.return)) _c.call(grid_4);
                    }
                    finally { if (e_6) throw e_6.error; }
                }
                return false;
            },
            arity: toFixedArity(2),
        },
        'every-col?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_7, _c;
                var _d = __read(_a, 2), grid = _d[0], predicate = _d[1];
                var executeFunction = _b.executeFunction;
                assertGrid(grid, sourceCodeInfo);
                assertFunctionLike(predicate, sourceCodeInfo);
                var transposed = transpose(grid);
                try {
                    for (var transposed_1 = __values(transposed), transposed_1_1 = transposed_1.next(); !transposed_1_1.done; transposed_1_1 = transposed_1.next()) {
                        var row = transposed_1_1.value;
                        if (!executeFunction(predicate, [row], contextStack, sourceCodeInfo)) {
                            return false;
                        }
                    }
                }
                catch (e_7_1) { e_7 = { error: e_7_1 }; }
                finally {
                    try {
                        if (transposed_1_1 && !transposed_1_1.done && (_c = transposed_1.return)) _c.call(transposed_1);
                    }
                    finally { if (e_7) throw e_7.error; }
                }
                return true;
            },
            arity: toFixedArity(2),
        },
        'some-col?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_8, _c;
                var _d = __read(_a, 2), grid = _d[0], predicate = _d[1];
                var executeFunction = _b.executeFunction;
                assertGrid(grid, sourceCodeInfo);
                assertFunctionLike(predicate, sourceCodeInfo);
                var transposed = transpose(grid);
                try {
                    for (var transposed_2 = __values(transposed), transposed_2_1 = transposed_2.next(); !transposed_2_1.done; transposed_2_1 = transposed_2.next()) {
                        var row = transposed_2_1.value;
                        if (executeFunction(predicate, [row], contextStack, sourceCodeInfo)) {
                            return true;
                        }
                    }
                }
                catch (e_8_1) { e_8 = { error: e_8_1 }; }
                finally {
                    try {
                        if (transposed_2_1 && !transposed_2_1.done && (_c = transposed_2.return)) _c.call(transposed_2);
                    }
                    finally { if (e_8) throw e_8.error; }
                }
                return false;
            },
            arity: toFixedArity(2),
        },
        'row': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), grid = _b[0], row = _b[1];
                assertGrid(grid, sourceCodeInfo);
                assertNumber(row, sourceCodeInfo, { integer: true, nonNegative: true, lt: grid.length });
                return grid[row];
            },
            arity: toFixedArity(2),
        },
        'col': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), grid = _b[0], col = _b[1];
                assertGrid(grid, sourceCodeInfo);
                assertNumber(col, sourceCodeInfo, { integer: true, nonNegative: true, lt: grid[0].length });
                return grid.map(function (row) { return row[col]; });
            },
            arity: toFixedArity(2),
        },
        'shape': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), grid = _b[0];
                assertGrid(grid, sourceCodeInfo);
                return [grid.length, grid[0].length];
            },
            arity: toFixedArity(1),
        },
        'fill': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), rows = _b[0], cols = _b[1], value = _b[2];
                assertNumber(rows, sourceCodeInfo, { integer: true, positive: true });
                assertNumber(cols, sourceCodeInfo, { integer: true, positive: true });
                assertAny(value, sourceCodeInfo);
                var result = [];
                for (var i = 0; i < rows; i += 1) {
                    var row = [];
                    for (var j = 0; j < cols; j += 1) {
                        row.push(value);
                    }
                    result.push(row);
                }
                return result;
            },
            arity: toFixedArity(3),
        },
        'generate': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 3), rows = _c[0], cols = _c[1], generator = _c[2];
                var executeFunction = _b.executeFunction;
                assertNumber(rows, sourceCodeInfo, { integer: true, positive: true });
                assertNumber(cols, sourceCodeInfo, { integer: true, positive: true });
                assertFunctionLike(generator, sourceCodeInfo);
                var result = [];
                for (var i = 0; i < rows; i += 1) {
                    var row = [];
                    for (var j = 0; j < cols; j += 1) {
                        var value = executeFunction(generator, [i, j], contextStack, sourceCodeInfo);
                        assertAny(value, sourceCodeInfo);
                        row.push(value);
                    }
                    result.push(row);
                }
                return result;
            },
            arity: toFixedArity(3),
        },
        'reshape': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), grid = _b[0], rows = _b[1];
                assertGrid(grid, sourceCodeInfo);
                assertNumber(rows, sourceCodeInfo, { integer: true, positive: true });
                var flatTable = grid.flat();
                if (flatTable.length % rows !== 0) {
                    throw new LitsError("The number of elements in the grid must be divisible by rows, but got ".concat(flatTable.length, " and ").concat(rows), sourceCodeInfo);
                }
                var cols = flatTable.length / rows;
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
            arity: toFixedArity(2),
        },
        'transpose': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), grid = _b[0];
                assertGrid(grid, sourceCodeInfo);
                return transpose(grid);
            },
            arity: toFixedArity(1),
            aliases: ['tr'],
        },
        'flip-h': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), grid = _b[0];
                assertGrid(grid, sourceCodeInfo);
                return grid.map(function (row) { return row.reverse(); });
            },
            arity: toFixedArity(1),
        },
        'flip-v': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), grid = _b[0];
                assertGrid(grid, sourceCodeInfo);
                return grid.reverse();
            },
            arity: toFixedArity(1),
        },
        'rotate': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), grid = _b[0], times = _b[1];
                assertGrid(grid, sourceCodeInfo);
                assertNumber(times, sourceCodeInfo, { integer: true });
                // Normalize times to be between 0 and 3
                times = ((times % 4) + 4) % 4;
                // If times is 0, return the original grid
                if (times === 0 || grid.length === 0) {
                    return grid.map(function (row) { return __spreadArray([], __read(row), false); });
                }
                var height = grid.length;
                var width = grid[0].length;
                var result;
                switch (times) {
                    case 1: // 90 degrees clockwise
                        result = Array(width).fill(null).map(function () { return Array(height).fill(null); });
                        for (var y = 0; y < height; y++) {
                            for (var x = 0; x < width; x++) {
                                result[x][height - 1 - y] = grid[y][x];
                            }
                        }
                        break;
                    case 2: // 180 degrees
                        result = Array(height).fill(null).map(function () { return Array(width).fill(null); });
                        for (var y = 0; y < height; y++) {
                            for (var x = 0; x < width; x++) {
                                result[height - 1 - y][width - 1 - x] = grid[y][x];
                            }
                        }
                        break;
                    case 3: // 270 degrees clockwise (or 90 degrees counter-clockwise)
                        result = Array(width).fill(null).map(function () { return Array(height).fill(null); });
                        for (var y = 0; y < height; y++) {
                            for (var x = 0; x < width; x++) {
                                result[width - 1 - x][y] = grid[y][x];
                            }
                        }
                        break;
                }
                return result;
            },
            arity: toFixedArity(2),
        },
        'reverse-rows': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), grid = _b[0];
                assertGrid(grid, sourceCodeInfo);
                return grid.reverse();
            },
            arity: toFixedArity(1),
        },
        'reverse-cols': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), grid = _b[0];
                assertGrid(grid, sourceCodeInfo);
                return grid.map(function (row) { return row.reverse(); });
            },
            arity: toFixedArity(1),
        },
        'slice': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), grid = _b[0], start = _b[1], end = _b[2];
                assertGrid(grid, sourceCodeInfo);
                assertVector(start, sourceCodeInfo);
                if (start.length !== 2) {
                    throw new LitsError("The start vector must have 2 elements, but got ".concat(start.length), sourceCodeInfo);
                }
                var _c = __read(start, 2), rowStart = _c[0], colStart = _c[1];
                assertNumber(rowStart, sourceCodeInfo, { integer: true, nonNegative: true, lt: grid.length });
                assertNumber(colStart, sourceCodeInfo, { integer: true, nonNegative: true, lt: grid[0].length });
                end !== null && end !== void 0 ? end : (end = [grid.length, grid[0].length]);
                assertVector(end, sourceCodeInfo);
                if (end.length !== 2) {
                    throw new LitsError("The end vector must have 2 elements, but got ".concat(end.length), sourceCodeInfo);
                }
                var _d = __read(end, 2), rowEnd = _d[0], colEnd = _d[1];
                assertNumber(rowEnd, sourceCodeInfo, { gt: rowStart, lte: grid.length });
                assertNumber(colEnd, sourceCodeInfo, { gt: colStart, lte: grid[0].length });
                var result = [];
                for (var i = rowStart; i < rowEnd; i += 1) {
                    var row = [];
                    for (var j = colStart; j < colEnd; j += 1) {
                        row.push(grid[i][j]);
                    }
                    result.push(row);
                }
                return result;
            },
            arity: { min: 2, max: 3 },
        },
        'slice-rows': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), grid = _b[0], rowStart = _b[1], rowEnd = _b[2];
                assertGrid(grid, sourceCodeInfo);
                if (typeof rowEnd === 'undefined') {
                    assertNumber(rowStart, sourceCodeInfo, { integer: true, lte: grid.length, gte: -grid.length });
                    if (rowStart < 0) {
                        return grid.slice(grid.length + rowStart);
                    }
                    return grid.slice(rowStart);
                }
                assertNumber(rowStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: grid.length });
                assertNumber(rowEnd, sourceCodeInfo, { integer: true });
                rowEnd = rowEnd < 0 ? grid.length + rowEnd : rowEnd;
                assertNumber(rowEnd, sourceCodeInfo, { gt: rowStart, lte: grid.length });
                return grid.slice(rowStart, rowEnd);
            },
            arity: { min: 2, max: 3 },
        },
        'slice-cols': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), grid = _b[0], colStart = _b[1], colEnd = _b[2];
                assertGrid(grid, sourceCodeInfo);
                var trMatrix = transpose(grid);
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
            arity: { min: 2, max: 3 },
        },
        'splice-rows': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), grid = _b[0], rowStart = _b[1], rowDeleteCount = _b[2], rows = _b.slice(3);
                assertGrid(grid, sourceCodeInfo);
                assertNumber(rowStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: grid.length });
                assertNumber(rowDeleteCount, sourceCodeInfo, { integer: true, nonNegative: true });
                if (rows.length !== 0) {
                    assertGrid(rows, sourceCodeInfo);
                    rows.every(function (row) {
                        assertArray(row, sourceCodeInfo);
                        if (grid[0].length !== row.length) {
                            throw new LitsError("All rows must have the same length as the number of columns in grid, but got ".concat(row.length), sourceCodeInfo);
                        }
                        return true;
                    });
                }
                var result = [];
                for (var i = 0; i < rowStart; i += 1) {
                    result.push(grid[i]);
                }
                if (rows.length > 0) {
                    result.push.apply(result, __spreadArray([], __read(rows), false));
                }
                for (var i = rowStart + rowDeleteCount; i < grid.length; i += 1) {
                    result.push(grid[i]);
                }
                return result;
            },
            arity: { min: 3 },
        },
        'splice-cols': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), grid = _b[0], colStart = _b[1], colDeleteCount = _b[2], cols = _b.slice(3);
                assertGrid(grid, sourceCodeInfo);
                var trMatrix = transpose(grid);
                assertNumber(colStart, sourceCodeInfo, { integer: true, nonNegative: true, lte: trMatrix.length });
                assertNumber(colDeleteCount, sourceCodeInfo, { integer: true, nonNegative: true });
                if (cols.length !== 0) {
                    assertGrid(cols, sourceCodeInfo);
                    cols.every(function (row) {
                        assertArray(row, sourceCodeInfo);
                        if (trMatrix[0].length !== row.length) {
                            throw new LitsError("All rows must have the same length as the number of rows in grid, but got ".concat(row.length), sourceCodeInfo);
                        }
                        return true;
                    });
                }
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
            arity: { min: 3 },
        },
        'concat-rows': {
            evaluate: function (params, sourceCodeInfo) {
                assertArray(params, sourceCodeInfo);
                params.every(function (grid) { return assertGrid(grid, sourceCodeInfo); });
                var cols = params[0][0].length;
                params.slice(1).every(function (grid) {
                    if (grid[0].length !== cols) {
                        throw new LitsError("All matrices must have the same number of columns, but got ".concat(cols, " and ").concat(grid[0].length), sourceCodeInfo);
                    }
                    return true;
                });
                var result = [];
                params.forEach(function (grid) {
                    grid.forEach(function (row) {
                        result.push(row);
                    });
                });
                return result;
            },
            arity: { min: 1 },
        },
        'concat-cols': {
            evaluate: function (params, sourceCodeInfo) {
                assertArray(params, sourceCodeInfo);
                params.every(function (grid) { return assertGrid(grid, sourceCodeInfo); });
                var rows = params[0].length;
                params.slice(1).every(function (grid) {
                    if (grid.length !== rows) {
                        throw new LitsError("All matrices must have the same number of rows, but got ".concat(rows, " and ").concat(grid.length), sourceCodeInfo);
                    }
                    return true;
                });
                var result = [];
                var _loop_1 = function (i) {
                    var row = [];
                    params.forEach(function (grid) {
                        row.push.apply(row, __spreadArray([], __read(grid[i]), false));
                    });
                    result.push(row);
                };
                for (var i = 0; i < rows; i += 1) {
                    _loop_1(i);
                }
                return result;
            },
            arity: { min: 1 },
        },
        'map': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var fn = asFunctionLike(params.at(-1), sourceCodeInfo);
                var grids = params.slice(0, -1);
                assertGrid(grids[0], sourceCodeInfo);
                var rows = grids[0].length;
                var cols = grids[0][0].length;
                grids.slice(1).forEach(function (grid) {
                    assertGrid(grid, sourceCodeInfo);
                    if (grid.length !== rows) {
                        throw new LitsError("All matrices must have the same number of rows, but got ".concat(rows, " and ").concat(grid.length), sourceCodeInfo);
                    }
                    if (grid[0].length !== cols) {
                        throw new LitsError("All matrices must have the same number of columns, but got ".concat(cols, " and ").concat(grid[0].length), sourceCodeInfo);
                    }
                });
                var result = [];
                var _loop_2 = function (i) {
                    var row = [];
                    var _loop_3 = function (j) {
                        var args = grids.map(function (grid) { return grid[i][j]; });
                        row.push(asAny(executeFunction(fn, args, contextStack, sourceCodeInfo)));
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
            arity: { min: 2 },
        },
        'mapi': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), grid = _c[0], fn = _c[1];
                var executeFunction = _b.executeFunction;
                assertGrid(grid, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                var rows = grid.length;
                var cols = grid[0].length;
                var result = [];
                for (var i = 0; i < rows; i += 1) {
                    var row = [];
                    for (var j = 0; j < cols; j += 1) {
                        row.push(asAny(executeFunction(fn, [grid[i][j], i, j], contextStack, sourceCodeInfo)));
                    }
                    result.push(row);
                }
                return result;
            },
            arity: toFixedArity(2),
        },
        'reduce': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_9, _c, e_10, _d;
                var _e = __read(_a, 3), grid = _e[0], fn = _e[1], initialValue = _e[2];
                var executeFunction = _b.executeFunction;
                assertGrid(grid, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                var accumulator = asAny(initialValue);
                try {
                    for (var grid_5 = __values(grid), grid_5_1 = grid_5.next(); !grid_5_1.done; grid_5_1 = grid_5.next()) {
                        var row = grid_5_1.value;
                        try {
                            for (var row_3 = (e_10 = void 0, __values(row)), row_3_1 = row_3.next(); !row_3_1.done; row_3_1 = row_3.next()) {
                                var cell = row_3_1.value;
                                accumulator = executeFunction(fn, [accumulator, cell], contextStack, sourceCodeInfo);
                            }
                        }
                        catch (e_10_1) { e_10 = { error: e_10_1 }; }
                        finally {
                            try {
                                if (row_3_1 && !row_3_1.done && (_d = row_3.return)) _d.call(row_3);
                            }
                            finally { if (e_10) throw e_10.error; }
                        }
                    }
                }
                catch (e_9_1) { e_9 = { error: e_9_1 }; }
                finally {
                    try {
                        if (grid_5_1 && !grid_5_1.done && (_c = grid_5.return)) _c.call(grid_5);
                    }
                    finally { if (e_9) throw e_9.error; }
                }
                return accumulator;
            },
            arity: toFixedArity(3),
        },
        'reducei': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 3), grid = _c[0], fn = _c[1], initialValue = _c[2];
                var executeFunction = _b.executeFunction;
                assertGrid(grid, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
                var accumulator = asAny(initialValue);
                for (var i = 0; i < grid.length; i += 1) {
                    for (var j = 0; j < grid[i].length; j += 1) {
                        accumulator = executeFunction(fn, [accumulator, grid[i][j], i, j], contextStack, sourceCodeInfo);
                    }
                }
                return accumulator;
            },
            arity: toFixedArity(3),
        },
        'push-rows': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), grid = _b[0], rows = _b.slice(1);
                assertGrid(grid, sourceCodeInfo);
                assertGrid(rows, sourceCodeInfo);
                if (grid[0].length !== rows[0].length) {
                    throw new LitsError("All rows must have the same length as the number of columns in grid, but got ".concat(grid[0].length, " and ").concat(rows[0].length), sourceCodeInfo);
                }
                return __spreadArray(__spreadArray([], __read(grid), false), __read(rows), false);
            },
            arity: { min: 2 },
        },
        'unshift-rows': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), grid = _b[0], rows = _b.slice(1);
                assertGrid(grid, sourceCodeInfo);
                assertGrid(rows, sourceCodeInfo);
                if (grid[0].length !== rows[0].length) {
                    throw new LitsError("All rows must have the same length as the number of columns in grid, but got ".concat(grid[0].length, " and ").concat(rows[0].length), sourceCodeInfo);
                }
                return __spreadArray(__spreadArray([], __read(rows), false), __read(grid), false);
            },
            arity: { min: 2 },
        },
        'pop-row': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), grid = _b[0];
                assertGrid(grid, sourceCodeInfo);
                if (grid.length === 1) {
                    return null;
                }
                return grid.slice(0, -1);
            },
            arity: toFixedArity(1),
        },
        'shift-row': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), grid = _b[0];
                assertGrid(grid, sourceCodeInfo);
                if (grid.length === 1) {
                    return null;
                }
                return grid.slice(1);
            },
            arity: toFixedArity(1),
        },
        'push-cols': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), grid = _b[0], cols = _b.slice(1);
                assertGrid(grid, sourceCodeInfo);
                assertGrid(cols, sourceCodeInfo);
                if (grid.length !== cols[0].length) {
                    throw new LitsError("All columns must have the same length as the number of rows in grid, but got ".concat(cols.length), sourceCodeInfo);
                }
                var result = [];
                var _loop_4 = function (i) {
                    var row = [];
                    row.push.apply(row, __spreadArray([], __read(grid[i]), false));
                    cols.forEach(function (col) {
                        row.push(col[i]);
                    });
                    result.push(row);
                };
                for (var i = 0; i < grid.length; i += 1) {
                    _loop_4(i);
                }
                return result;
            },
            arity: { min: 2 },
        },
        'unshift-cols': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a), grid = _b[0], cols = _b.slice(1);
                assertGrid(grid, sourceCodeInfo);
                assertGrid(cols, sourceCodeInfo);
                if (grid.length !== cols[0].length) {
                    throw new LitsError("All columns must have the same length as the number of rows in grid, but got ".concat(cols.length), sourceCodeInfo);
                }
                var result = [];
                var _loop_5 = function (i) {
                    var row = [];
                    cols.forEach(function (col) {
                        row.push(col[i]);
                    });
                    row.push.apply(row, __spreadArray([], __read(grid[i]), false));
                    result.push(row);
                };
                for (var i = 0; i < grid.length; i += 1) {
                    _loop_5(i);
                }
                return result;
            },
            arity: { min: 2 },
        },
        'pop-col': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), grid = _b[0];
                assertGrid(grid, sourceCodeInfo);
                if (grid[0].length === 1) {
                    return null;
                }
                return grid.map(function (row) { return row.slice(0, -1); });
            },
            arity: toFixedArity(1),
        },
        'shift-col': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), grid = _b[0];
                assertGrid(grid, sourceCodeInfo);
                if (grid[0].length === 1) {
                    return null;
                }
                return grid.map(function (row) { return row.slice(1); });
            },
            arity: toFixedArity(1),
        },
        'from-array': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), array = _b[0], rows = _b[1];
                assertArray(array, sourceCodeInfo);
                assertNumber(rows, sourceCodeInfo, { integer: true, positive: true });
                if (array.length % rows !== 0) {
                    throw new LitsError("The number of elements in the array must be divisible by rows, but got ".concat(array.length, " and ").concat(rows), sourceCodeInfo);
                }
                return fromArray(array, rows);
            },
            arity: toFixedArity(2),
        },
    };
    /**
     * The grid namespace containing 2D array manipulation functions.
     */
    var gridNamespace = {
        name: 'grid',
        functions: gridFunctions,
    };

    // Export registry functions
    registerNamespace(gridNamespace);

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
        '==': {
            evaluate: function (params, sourceCodeInfo) {
                return isEqual(params, sourceCodeInfo);
            },
            arity: { min: 1 },
        },
        '≠': {
            evaluate: function (params, sourceCodeInfo) {
                return !isEqual(params, sourceCodeInfo);
            },
            arity: { min: 1 },
            aliases: ['!='],
        },
        'identical?': {
            evaluate: function (params) {
                return isIdentical(params);
            },
            arity: { min: 1 },
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
            arity: { min: 1 },
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
            arity: { min: 1 },
        },
        '>=': {
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
            arity: { min: 1 },
            aliases: ['≥'],
        },
        '<=': {
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
            arity: { min: 1 },
            aliases: ['≤'],
        },
        '!': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return !first;
            },
            arity: toFixedArity(1),
        },
        'epoch->iso-date': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), ms = _b[0];
                assertNumber(ms, sourceCodeInfo);
                return new Date(ms).toISOString();
            },
            arity: toFixedArity(1),
        },
        'iso-date->epoch': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), dateTime = _b[0];
                assertString(dateTime, sourceCodeInfo);
                var ms = new Date(dateTime).valueOf();
                assertNumber(ms, sourceCodeInfo, { finite: true });
                return ms;
            },
            arity: toFixedArity(1),
        },
        'write!': {
            evaluate: function (params, sourceCodeInfo) {
                // eslint-disable-next-line no-console
                console.log.apply(console, __spreadArray([], __read(params), false));
                if (params.length > 0)
                    return asAny(params[params.length - 1], sourceCodeInfo);
                return null;
            },
            arity: {},
        },
        'boolean': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), value = _b[0];
                return !!value;
            },
            arity: toFixedArity(1),
        },
        'compare': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), a = _b[0], b = _b[1];
                assertStringOrNumber(a, sourceCodeInfo);
                assertStringOrNumber(b, sourceCodeInfo);
                return compare(a, b, sourceCodeInfo);
            },
            arity: toFixedArity(2),
        },
        'json-parse': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertString(first, sourceCodeInfo);
                // eslint-disable-next-line ts/no-unsafe-return
                return JSON.parse(first);
            },
            arity: toFixedArity(1),
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
            arity: { min: 1, max: 2 },
        },
        'import': {
            evaluate: function (_a, sourceCodeInfo) {
                var e_7, _b, _c, e_8, _d, _e, e_9, _f, _g;
                var _h;
                var _j = __read(_a, 1), importPath = _j[0];
                assertString(importPath, sourceCodeInfo);
                // Check if importing a specific function (e.g., "grid.row")
                var dotIndex = importPath.indexOf('.');
                if (dotIndex !== -1) {
                    var namespaceName_1 = importPath.substring(0, dotIndex);
                    var functionName = importPath.substring(dotIndex + 1);
                    var namespace_1 = getNamespace(namespaceName_1);
                    if (!namespace_1) {
                        throw new LitsError("Unknown namespace: '".concat(namespaceName_1, "'"), sourceCodeInfo);
                    }
                    // Look for the function by name or alias
                    var targetFunctionName = void 0;
                    var expression = namespace_1.functions[functionName];
                    if (expression) {
                        targetFunctionName = functionName;
                    }
                    else {
                        try {
                            // Check if it's an alias
                            for (var _k = __values(Object.entries(namespace_1.functions)), _l = _k.next(); !_l.done; _l = _k.next()) {
                                var _m = __read(_l.value, 2), fnName = _m[0], expr = _m[1];
                                if ((_h = expr.aliases) === null || _h === void 0 ? void 0 : _h.includes(functionName)) {
                                    targetFunctionName = fnName;
                                    expression = expr;
                                    break;
                                }
                            }
                        }
                        catch (e_7_1) { e_7 = { error: e_7_1 }; }
                        finally {
                            try {
                                if (_l && !_l.done && (_b = _k.return)) _b.call(_k);
                            }
                            finally { if (e_7) throw e_7.error; }
                        }
                    }
                    if (!expression || !targetFunctionName) {
                        throw new LitsError("Function '".concat(functionName, "' not found in namespace '").concat(namespaceName_1, "'"), sourceCodeInfo);
                    }
                    return _c = {},
                        _c[FUNCTION_SYMBOL] = true,
                        _c.sourceCodeInfo = sourceCodeInfo,
                        _c.functionType = 'Namespace',
                        _c.namespaceName = namespaceName_1,
                        _c.functionName = targetFunctionName,
                        _c.arity = expression.arity,
                        _c;
                }
                // Import entire namespace
                var namespaceName = importPath;
                var namespace = getNamespace(namespaceName);
                if (!namespace) {
                    throw new LitsError("Unknown namespace: '".concat(namespaceName, "'"), sourceCodeInfo);
                }
                // Create an object where each key is a function name and value is a NamespaceFunction
                var result = {};
                try {
                    for (var _o = __values(Object.entries(namespace.functions)), _p = _o.next(); !_p.done; _p = _o.next()) {
                        var _q = __read(_p.value, 2), functionName = _q[0], expression = _q[1];
                        result[functionName] = (_e = {},
                            _e[FUNCTION_SYMBOL] = true,
                            _e.sourceCodeInfo = sourceCodeInfo,
                            _e.functionType = 'Namespace',
                            _e.namespaceName = namespaceName,
                            _e.functionName = functionName,
                            _e.arity = expression.arity,
                            _e);
                        // Also add aliases
                        if (expression.aliases) {
                            try {
                                for (var _r = (e_9 = void 0, __values(expression.aliases)), _s = _r.next(); !_s.done; _s = _r.next()) {
                                    var alias = _s.value;
                                    result[alias] = (_g = {},
                                        _g[FUNCTION_SYMBOL] = true,
                                        _g.sourceCodeInfo = sourceCodeInfo,
                                        _g.functionType = 'Namespace',
                                        _g.namespaceName = namespaceName,
                                        _g.functionName = functionName,
                                        _g.arity = expression.arity,
                                        _g);
                                }
                            }
                            catch (e_9_1) { e_9 = { error: e_9_1 }; }
                            finally {
                                try {
                                    if (_s && !_s.done && (_f = _r.return)) _f.call(_r);
                                }
                                finally { if (e_9) throw e_9.error; }
                            }
                        }
                    }
                }
                catch (e_8_1) { e_8 = { error: e_8_1 }; }
                finally {
                    try {
                        if (_p && !_p.done && (_d = _o.return)) _d.call(_o);
                    }
                    finally { if (e_8) throw e_8.error; }
                }
                return result;
            },
            arity: toFixedArity(1),
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
            arity: { min: 1, max: 2 },
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
            arity: { min: 2, max: 3 },
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
            arity: { min: 2, max: 3 },
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
            arity: { min: 2, max: 3 },
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
            arity: { min: 2, max: 3 },
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
            arity: { min: 2, max: 3 },
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
            arity: { min: 2, max: 3 },
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
            arity: { min: 1, max: 2 },
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
            arity: { min: 1, max: 2 },
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
            arity: { min: 1, max: 2 },
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
            arity: { min: 1, max: 2 },
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
            arity: { min: 1, max: 2 },
        },
        'assert-throws': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), func = _c[0], message = _c[1];
                var executeFunction = _b.executeFunction;
                if (message !== undefined) {
                    assertString(message, sourceCodeInfo);
                }
                message !== null && message !== void 0 ? message : (message = '');
                assertFunctionLike(func, sourceCodeInfo);
                try {
                    executeFunction(func, [], contextStack, sourceCodeInfo);
                }
                catch (_d) {
                    return null;
                }
                throw new AssertionError("Expected function to throw.".concat(message), sourceCodeInfo);
            },
            arity: { min: 1, max: 2 },
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
                assertFunctionLike(func, sourceCodeInfo);
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
            arity: { min: 2, max: 3 },
        },
        'assert-not-throws': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), func = _c[0], message = _c[1];
                var executeFunction = _b.executeFunction;
                if (message !== undefined) {
                    assertString(message, sourceCodeInfo);
                }
                message !== null && message !== void 0 ? message : (message = '');
                assertFunctionLike(func, sourceCodeInfo);
                try {
                    executeFunction(func, [], contextStack, sourceCodeInfo);
                }
                catch (_d) {
                    throw new AssertionError("Expected function not to throw.".concat(message), sourceCodeInfo);
                }
                return null;
            },
            arity: { min: 1, max: 2 },
        },
    };

    var objectNormalExpression = {
        'keys': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), obj = _b[0];
                assertObj(obj, sourceCodeInfo);
                return Object.keys(obj);
            },
            arity: toFixedArity(1),
        },
        'vals': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), obj = _b[0];
                assertObj(obj, sourceCodeInfo);
                return Object.values(obj);
            },
            arity: toFixedArity(1),
        },
        'entries': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), obj = _b[0];
                assertObj(obj, sourceCodeInfo);
                return Object.entries(obj);
            },
            arity: toFixedArity(1),
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
            arity: toFixedArity(2),
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
            arity: toFixedArity(2),
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
            arity: { min: 0 },
        },
        'merge-with': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var first = params[0];
                var fn = params.at(-1);
                var rest = params.slice(1, -1);
                assertObj(first, sourceCodeInfo);
                assertFunctionLike(fn, sourceCodeInfo);
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
            arity: { min: 2 },
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
            arity: toFixedArity(2),
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
            arity: toFixedArity(2),
        },
    };

    var predicatesNormalExpression = {
        'function?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return isLitsFunction(first);
            },
            arity: toFixedArity(1),
        },
        'string?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return typeof first === 'string';
            },
            arity: toFixedArity(1),
        },
        'number?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return typeof first === 'number';
            },
            arity: toFixedArity(1),
        },
        'integer?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return typeof first === 'number' && isNumber(first, { integer: true });
            },
            arity: toFixedArity(1),
        },
        'boolean?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return typeof first === 'boolean';
            },
            arity: toFixedArity(1),
        },
        'null?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return first === null || first === undefined;
            },
            arity: toFixedArity(1),
        },
        'zero?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo, { finite: true });
                return Math.abs(value) < EPSILON;
            },
            arity: toFixedArity(1),
        },
        'pos?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo, { finite: true });
                return first > 0;
            },
            arity: toFixedArity(1),
        },
        'neg?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo, { finite: true });
                return first < 0;
            },
            arity: toFixedArity(1),
        },
        'even?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo, { finite: true });
                return first % 2 === 0;
            },
            arity: toFixedArity(1),
        },
        'odd?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo, { finite: true });
                return isNumber(first, { integer: true }) && first % 2 !== 0;
            },
            arity: toFixedArity(1),
        },
        'array?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return Array.isArray(first);
            },
            arity: toFixedArity(1),
        },
        'coll?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return isColl(first);
            },
            arity: toFixedArity(1),
        },
        'seq?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return isSeq(first);
            },
            arity: toFixedArity(1),
        },
        'object?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), first = _b[0];
                return isObj(first);
            },
            arity: toFixedArity(1),
        },
        'regexp?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), value = _b[0];
                return isRegularExpression(value);
            },
            arity: toFixedArity(1),
        },
        'finite?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return Number.isFinite(value);
            },
            arity: toFixedArity(1),
        },
        'positive-infinity?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return value === Number.POSITIVE_INFINITY;
            },
            arity: toFixedArity(1),
        },
        'negative-infinity?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return value === Number.NEGATIVE_INFINITY;
            },
            arity: toFixedArity(1),
        },
        'true?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), value = _b[0];
                return value === true;
            },
            arity: toFixedArity(1),
        },
        'false?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), value = _b[0];
                return value === false;
            },
            arity: toFixedArity(1),
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
            arity: toFixedArity(1),
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
            arity: toFixedArity(1),
        },
        'vector?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), vector = _b[0];
                return isVector(vector);
            },
            arity: toFixedArity(1),
        },
        'matrix?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), matrix = _b[0];
                return isMatrix(matrix);
            },
            arity: toFixedArity(1),
        },
        'grid?': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), table = _b[0];
                return isGrid(table);
            },
            arity: toFixedArity(1),
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
                try {
                    // eslint-disable-next-line no-new
                    new RegExp(source, flags); // Throws if invalid regexp
                }
                catch (e) {
                    throw new LitsError("Invalid regular expression: ".concat(source, " ").concat(flags), sourceCodeInfo);
                }
                return _b = {},
                    _b[REGEXP_SYMBOL] = true,
                    _b.sourceCodeInfo = sourceCodeInfo,
                    _b.s = source,
                    _b.f = flags,
                    _b;
            },
            arity: { min: 1, max: 2 },
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
            arity: toFixedArity(2),
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
            arity: toFixedArity(3),
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
            arity: toFixedArity(3),
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
            arity: toFixedArity(2),
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
            arity: {},
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
            arity: toFixedArity(1),
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
            arity: toFixedArity(1),
        },
        'to-char-code': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo, { nonEmpty: true });
                return asNonUndefined(str.codePointAt(0), sourceCodeInfo);
            },
            arity: toFixedArity(1),
        },
        'lower-case': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.toLowerCase();
            },
            arity: toFixedArity(1),
        },
        'upper-case': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.toUpperCase();
            },
            arity: toFixedArity(1),
        },
        'trim': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.trim();
            },
            arity: toFixedArity(1),
        },
        'trim-left': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.replace(/^\s+/, '');
            },
            arity: toFixedArity(1),
        },
        'trim-right': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.replace(/\s+$/, '');
            },
            arity: toFixedArity(1),
        },
        'join': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), stringList = _b[0], delimiter = _b[1];
                assertArray(stringList, sourceCodeInfo);
                stringList.forEach(function (str) { return assertStringOrNumber(str, sourceCodeInfo); });
                assertString(delimiter, sourceCodeInfo);
                return stringList.join(delimiter);
            },
            arity: toFixedArity(2),
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
            arity: { min: 2, max: 3 },
        },
        'split-lines': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.split((/\r\n|\n|\r/)).filter(function (line) { return line !== ''; });
            },
            arity: toFixedArity(1),
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
            arity: { min: 2, max: 3 },
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
            arity: { min: 2, max: 3 },
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
            arity: { min: 1, max: 10 },
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
            arity: toFixedArity(1),
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
            arity: toFixedArity(1),
        },
        'encode-uri-component': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertString(value, sourceCodeInfo);
                return encodeURIComponent(value);
            },
            arity: toFixedArity(1),
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
            arity: toFixedArity(1),
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
            arity: toFixedArity(1),
        },
        'capitalize': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.charAt(0).toUpperCase() + str.slice(1).toLowerCase();
            },
            arity: toFixedArity(1),
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
        '|>': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), value = _c[0], func = _c[1];
                var executeFunction = _b.executeFunction;
                assertFunctionLike(func, sourceCodeInfo);
                return executeFunction(func, [value], contextStack, sourceCodeInfo);
            },
            arity: toFixedArity(2),
        },
        'apply': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a), func = _c[0], params = _c.slice(1);
                var executeFunction = _b.executeFunction;
                assertFunctionLike(func, sourceCodeInfo);
                var paramsLength = params.length;
                var last = params[paramsLength - 1];
                assertArray(last, sourceCodeInfo);
                var applyArray = __spreadArray(__spreadArray([], __read(params.slice(0, -1)), false), __read(last), false);
                return executeFunction(func, applyArray, contextStack, sourceCodeInfo);
            },
            arity: { min: 2 },
        },
        'identity': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), value = _b[0];
                return toAny(value);
            },
            arity: toFixedArity(1),
        },
        'comp': {
            evaluate: function (params, sourceCodeInfo) {
                var _a;
                params.forEach(function (param) { return assertFunctionLike(param, sourceCodeInfo); });
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.sourceCodeInfo = sourceCodeInfo,
                    _a.functionType = 'Comp',
                    _a.params = params,
                    _a.arity = params.length > 0 ? getArityFromFunction(params.at(-1)) : { min: 1, max: 1 },
                    _a;
            },
            arity: {},
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
                    _b.arity = {},
                    _b;
            },
            arity: toFixedArity(1),
        },
        'juxt': {
            evaluate: function (params, sourceCodeInfo) {
                var _a;
                params.forEach(function (param) { return assertFunctionLike(param, sourceCodeInfo); });
                var arity = getCommonArityFromFunctions(params);
                if (arity === null) {
                    throw new LitsError('All functions must accept the same number of arguments', sourceCodeInfo);
                }
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.sourceCodeInfo = sourceCodeInfo,
                    _a.functionType = 'Juxt',
                    _a.params = params,
                    _a.arity = arity,
                    _a;
            },
            arity: { min: 1 },
        },
        'complement': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b;
                var _c = __read(_a, 1), fn = _c[0];
                var fun = asFunctionLike(fn, sourceCodeInfo);
                return _b = {},
                    _b[FUNCTION_SYMBOL] = true,
                    _b.sourceCodeInfo = sourceCodeInfo,
                    _b.functionType = 'Complement',
                    _b.function = fun,
                    _b.arity = getArityFromFunction(fun),
                    _b;
            },
            arity: toFixedArity(1),
        },
        'every-pred': {
            evaluate: function (params, sourceCodeInfo) {
                var _a;
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.sourceCodeInfo = sourceCodeInfo,
                    _a.functionType = 'EveryPred',
                    _a.params = params,
                    _a.arity = { min: 1, max: 1 },
                    _a;
            },
            arity: { min: 1 },
        },
        'some-pred': {
            evaluate: function (params, sourceCodeInfo) {
                var _a;
                return _a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.sourceCodeInfo = sourceCodeInfo,
                    _a.functionType = 'SomePred',
                    _a.params = params,
                    _a.arity = { min: 1, max: 1 },
                    _a;
            },
            arity: { min: 1 },
        },
        'fnull': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b;
                var _c = __read(_a), fn = _c[0], params = _c.slice(1);
                var fun = asFunctionLike(fn, sourceCodeInfo);
                return _b = {},
                    _b[FUNCTION_SYMBOL] = true,
                    _b.sourceCodeInfo = sourceCodeInfo,
                    _b.functionType = 'Fnull',
                    _b.function = fun,
                    _b.params = params,
                    _b.arity = getArityFromFunction(fun),
                    _b;
            },
            arity: { min: 2 },
        },
    };

    function generateDocString(reference) {
        return smartTrim("\n    ".concat(reference.title).concat(getAliases(reference), "\n\n    ").concat(reference.description
            .replace(/`(.+?)`/g, '$1')
            .replace(/\$(\w+)/g, '$1')
            .replace(/\*\*\*(.+)\*\*\*/g, '$1')
            .replace(/\*\*(.+)\*\*/g, '$1'), "\n\n    Signature:\n    ").concat(signature(reference).join('\n    '), "\n\n    Arguments:\n      ").concat(argStrings(reference).join('\n      '), "\n  \n    Examples:\n").concat(reference.examples.map(function (example) { return smartTrim(example, 4); }).join('\n\n')));
    }
    function signature(_a) {
        var title = _a.title, variants = _a.variants, args = _a.args, returns = _a.returns, _isOperator = _a._isOperator;
        var functionForms = variants.map(function (variant) {
            var form = "  ".concat(title, "(").concat(variant.argumentNames.map(function (argName) {
                var result = '';
                var arg = args[argName];
                if (arg.rest) {
                    result += '...';
                }
                result += argName;
                return result;
            }).join(', '), ")");
            return "".concat(form, " -> ").concat(type(returns));
        });
        var operatorForm = _isOperator ? ['', 'Operator:', "  a ".concat(title, " b -> ").concat(type(returns))] : [];
        return __spreadArray(__spreadArray([], __read(functionForms), false), __read(operatorForm), false);
    }
    function type(arg) {
        var argType = arg.type;
        var types = Array.isArray(argType) ? argType : [argType];
        var typeString = types.join(' | ');
        return arg.array || arg.rest ? "Array<".concat(typeString, ">") : typeString;
    }
    function argStrings(reference) {
        return Object.entries(reference.args).map(function (_a) {
            var _b = __read(_a, 2), argName = _b[0], arg = _b[1];
            return "".concat(argName, ": ").concat(type(arg));
        });
    }
    function getAliases(reference) {
        if (!reference.aliases || reference.aliases.length === 0)
            return '';
        return "\n\n    Alias:\n      ".concat(reference.aliases.join('\n      '));
    }

    function getMetaNormalExpression(normalExpressionReference) {
        return {
            doc: {
                evaluate: function (_a, sourceCodeInfo) {
                    var _b = __read(_a, 1), fn = _b[0];
                    assertNonUndefined(normalExpressionReference);
                    assertFunctionLike(fn, sourceCodeInfo);
                    if (!isLitsFunction(fn)) {
                        return '';
                    }
                    if (fn.functionType === 'Builtin') {
                        var reference = normalExpressionReference[fn.name];
                        return generateDocString(reference);
                    }
                    if (fn.functionType === 'UserDefined' || fn.functionType === 'NativeJsFunction') {
                        return fn.docString;
                    }
                    return '';
                },
                arity: toFixedArity(1),
            },
            arity: {
                evaluate: function (_a, sourceCodeInfo) {
                    var _b = __read(_a, 1), fn = _b[0];
                    assertFunctionLike(fn, sourceCodeInfo);
                    return isLitsFunction(fn) ? fn.arity : toFixedArity(1);
                },
                arity: toFixedArity(1),
            },
        };
    }

    // TODO: Phase 1 - Namespaces commented out for refactoring
    // These will require import() to use
    // import { gridNormalExpression } from './categories/namespaces/grid'
    // import { vectorNormalExpression } from './categories/namespaces/vector'
    // import { linearAlgebraNormalExpression } from './categories/namespaces/linearAlgebra'
    // import { matrixNormalExpression } from './categories/namespaces/matrix'
    // import { combinatoricalNormalExpression } from './categories/namespaces/numberTheory'
    // import { randomNormalExpression } from './categories/namespaces/random'
    var normalExpressionReference$1 = {};
    function setNormalExpressionReference(reference) {
        Object.assign(normalExpressionReference$1, reference);
    }
    var expressions = __assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign({}, bitwiseNormalExpression), collectionNormalExpression), arrayNormalExpression), sequenceNormalExpression), mathNormalExpression), getMetaNormalExpression(normalExpressionReference$1)), miscNormalExpression), assertNormalExpression), objectNormalExpression), predicatesNormalExpression), regexpNormalExpression), stringNormalExpression), functionalNormalExpression);
    Object.entries(expressions).forEach(function (_a) {
        var _b = __read(_a, 2), name = _b[0], expression = _b[1];
        expression.name = name;
    });
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
        arity: {},
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
        arity: {},
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
        arity: {},
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
        arity: toFixedArity(1),
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

    function isJsFunction(fn) {
        return typeof fn === 'object' && fn !== null && 'fn' in fn && typeof fn.fn === 'function';
    }
    function assertJsFunction(fn) {
        if (!isJsFunction(fn)) {
            throw new TypeError('Expected a NativeJsFunction');
        }
    }
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
        arity: {},
        evaluate: function (node, contextStack, _a) {
            var evaluateNode = _a.evaluateNode;
            var bindingNode = node[1][1];
            var target = bindingNode[1][0];
            var value = bindingNode[1][1];
            var bindingValue = evaluateNode(value, contextStack);
            var values = evalueateBindingNodeValues(target, bindingValue, function (Node) { return evaluateNode(Node, contextStack); });
            contextStack.exportValues(values, target[2]);
            return bindingValue;
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
        arity: {},
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

    var lambdaSpecialExpression = {
        arity: {},
        evaluate: function (node, contextStack, _a) {
            var _b;
            var builtin = _a.builtin, getUndefinedSymbols = _a.getUndefinedSymbols, evaluateNode = _a.evaluateNode;
            var fn = node[1][1];
            var docString = node[1][2];
            var evaluatedFunction = evaluateFunction(fn, contextStack, builtin, getUndefinedSymbols, evaluateNode);
            var min = evaluatedFunction[0].filter(function (arg) { return arg[0] !== bindingTargetTypes.rest && arg[1][1] === undefined; }).length;
            var max = evaluatedFunction[0].some(function (arg) { return arg[0] === bindingTargetTypes.rest; }) ? undefined : evaluatedFunction[0].length;
            var arity = { min: min > 0 ? min : undefined, max: max };
            var litsFunction = (_b = {},
                _b[FUNCTION_SYMBOL] = true,
                _b.sourceCodeInfo = node[2],
                _b.functionType = 'UserDefined',
                _b.name = undefined,
                _b.evaluatedfunction = evaluatedFunction,
                _b.arity = arity,
                _b.docString = docString,
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
    function getFunctionUnresolvedSymbols(fn, contextStack, getUndefinedSymbols, builtin, evaluateNode) {
        var result = new Set();
        var newContext = { self: { value: null } };
        fn[0].forEach(function (arg) {
            Object.assign(newContext, getAllBindingTargetNames(arg));
            walkDefaults(arg, function (defaultNode) {
                addToSet(result, getUndefinedSymbols([defaultNode], contextStack, builtin, evaluateNode));
            });
        });
        var newContextStack = contextStack.create(newContext);
        var overloadResult = getUndefinedSymbols(fn[1], newContextStack, builtin, evaluateNode);
        addToSet(result, overloadResult);
        return result;
    }

    var ifSpecialExpression = {
        arity: { min: 2, max: 3 },
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
        arity: {},
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
        arity: toFixedArity(0),
        evaluate: function (node, contextStack, _a) {
            var evaluateNode = _a.evaluateNode;
            var bindingNode = node[1][1];
            var target = bindingNode[1][0];
            var value = bindingNode[1][1];
            var bindingValue = evaluateNode(value, contextStack);
            var values = evalueateBindingNodeValues(target, bindingValue, function (Node) { return evaluateNode(Node, contextStack); });
            contextStack.addValues(values, target[2]);
            return bindingValue;
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
        arity: {},
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
                var result = null;
                try {
                    result = evaluateNode(body, newContextStack);
                }
                catch (error) {
                    if (error instanceof RecurSignal) {
                        var params_1 = error.params;
                        if (params_1.length !== bindingNodes.length) {
                            throw new LitsError("recur expected ".concat(bindingNodes.length, " parameters, got ").concat(valueToString(params_1.length)), node[2]);
                        }
                        bindingNodes.forEach(function (bindingNode, index) {
                            var e_1, _a;
                            var valueRecord = evalueateBindingNodeValues(bindingNode[1][0], asAny(params_1[index]), function (Node) { return evaluateNode(Node, contextStack); });
                            try {
                                for (var _b = (e_1 = void 0, __values(Object.entries(valueRecord))), _c = _b.next(); !_c.done; _c = _b.next()) {
                                    var _d = __read(_c.value, 2), name_1 = _d[0], value = _d[1];
                                    bindingContext[name_1].value = value;
                                }
                            }
                            catch (e_1_1) { e_1 = { error: e_1_1 }; }
                            finally {
                                try {
                                    if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                                }
                                finally { if (e_1) throw e_1.error; }
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
            var paramsResult = getUndefinedSymbols([node[1][2]], contextStack.create(newContext), builtin, evaluateNode);
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
            var context = {};
            var newContextStack = contextStack.create(context);
            var skip = false;
            bindingsLoop: for (var bindingIndex = 0; bindingIndex < loopBindings.length; bindingIndex += 1) {
                var _b = __read(loopBindings[bindingIndex], 4), bindingNode = _b[0], letBindings = _b[1], whenNode = _b[2], whileNode = _b[3];
                var _c = __read(bindingNode[1], 2), targetNode = _c[0], valueNode = _c[1];
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
                var value = evaluateNode(body, newContextStack);
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
        getUndefinedSymbols([body], contextStack.create(newContext), builtin, evaluateNode).forEach(function (symbol) {
            return result.add(symbol);
        });
        return result;
    }
    var forSpecialExpression = {
        arity: toFixedArity(1),
        evaluate: function (node, contextStack, helpers) { return evaluateLoop(true, node, contextStack, helpers.evaluateNode); },
        getUndefinedSymbols: function (node, contextStack, _a) {
            var getUndefinedSymbols = _a.getUndefinedSymbols, builtin = _a.builtin, evaluateNode = _a.evaluateNode;
            return analyze$1(node, contextStack, getUndefinedSymbols, builtin, evaluateNode);
        },
    };
    var doseqSpecialExpression = {
        arity: toFixedArity(1),
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
        arity: {},
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
        arity: { min: 1, max: 2 },
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
        arity: {},
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
        arity: toFixedArity(1),
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
        arity: {},
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
        arity: {},
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
        arity: {},
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
                    var valueNode = params[i + 1];
                    if (valueNode === undefined) {
                        throw new LitsError('Missing value for key', keyNode[2]);
                    }
                    var value = evaluateNode(valueNode, contextStack);
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
        // defnSpecialExpression,
        doSpecialExpression,
        doseqSpecialExpression,
        lambdaSpecialExpression,
        forSpecialExpression,
        // functionSpecialExpression,
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
    // TODO, remove
    // console.log('builtin', [...specialExpressionKeys, ...normalExpressionKeys].length)

    var nonNumberReservedSymbolRecord = {
        true: true,
        false: false,
        null: null,
        else: null,
        case: null,
        each: null,
        in: null,
        when: null,
        while: null,
        catch: null,
        function: null,
        export: null,
        as: null,
        then: null,
        end: null,
        _: null,
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
        'NaN': Number.NaN,
    };
    var reservedSymbolRecord = __assign(__assign({}, nonNumberReservedSymbolRecord), numberReservedSymbolRecord);
    function isNumberReservedSymbol(symbol) {
        return symbol in numberReservedSymbolRecord;
    }

    var functionExecutors = {
        NativeJsFunction: function (fn, params, sourceCodeInfo) {
            var _a;
            try {
                return toAny((_a = fn.nativeFn).fn.apply(_a, __spreadArray([], __read(params), false)));
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
                if (!arityAcceptsMin(fn.arity, params.length)) {
                    throw new LitsError("Expected ".concat(fn.arity, " arguments, got ").concat(params.length, "."), sourceCodeInfo);
                }
                // checkParams(fn.evaluatedfunction, params.length, sourceCodeInfo)
                var evaluatedFunction = fn.evaluatedfunction;
                var args = evaluatedFunction[0];
                var nbrOfNonRestArgs = args.filter(function (arg) { return arg[0] !== bindingTargetTypes.rest; }).length;
                var newContextStack = contextStack.create(fn.evaluatedfunction[2]);
                var newContext = { self: { value: fn } };
                var rest = [];
                for (var i = 0; i < params.length; i += 1) {
                    if (i < nbrOfNonRestArgs) {
                        var param = toAny(params[i]);
                        var valueRecord = evalueateBindingNodeValues(args[i], param, function (node) {
                            return evaluateNode(node, newContextStack.create(newContext));
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
                    var valueRecord = evalueateBindingNodeValues(arg, defaultValue, function (node) {
                        return evaluateNode(node, contextStack.create(newContext));
                    });
                    Object.entries(valueRecord).forEach(function (_a) {
                        var _b = __read(_a, 2), key = _b[0], value = _b[1];
                        newContext[key] = { value: value };
                    });
                }
                var restArgument = args.find(function (arg) { return arg[0] === bindingTargetTypes.rest; });
                if (restArgument !== undefined) {
                    var valueRecord = evalueateBindingNodeValues(restArgument, rest, function (node) { return evaluateNode(node, contextStack.create(newContext)); });
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
            var e_2, _b;
            var executeFunction = _a.executeFunction;
            var actualParams = __spreadArray([], __read(fn.params), false);
            if (params.length !== fn.placeholders.length) {
                throw new LitsError("(partial) expects ".concat(fn.placeholders.length, " arguments, got ").concat(params.length, "."), sourceCodeInfo);
            }
            var paramsCopy = __spreadArray([], __read(params), false);
            try {
                for (var _c = __values(fn.placeholders), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var placeholderIndex = _d.value;
                    actualParams.splice(placeholderIndex, 0, paramsCopy.shift());
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (_d && !_d.done && (_b = _c.return)) _b.call(_c);
                }
                finally { if (e_2) throw e_2.error; }
            }
            return executeFunction(fn.function, actualParams, contextStack, sourceCodeInfo);
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
                return [executeFunction(asFunctionLike(fun, sourceCodeInfo), result, contextStack, sourceCodeInfo)];
            }, params)[0], sourceCodeInfo);
        },
        Constantly: function (fn) {
            return fn.value;
        },
        Juxt: function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return fn.params.map(function (fun) { return executeFunction(asFunctionLike(fun, sourceCodeInfo), params, contextStack, sourceCodeInfo); });
        },
        Complement: function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            return !executeFunction(fn.function, params, contextStack, sourceCodeInfo);
        },
        EveryPred: function (fn, params, sourceCodeInfo, contextStack, _a) {
            var e_3, _b, e_4, _c;
            var executeFunction = _a.executeFunction;
            try {
                for (var _d = __values(fn.params), _e = _d.next(); !_e.done; _e = _d.next()) {
                    var f = _e.value;
                    try {
                        for (var params_1 = (e_4 = void 0, __values(params)), params_1_1 = params_1.next(); !params_1_1.done; params_1_1 = params_1.next()) {
                            var param = params_1_1.value;
                            var result = executeFunction(asFunctionLike(f, sourceCodeInfo), [param], contextStack, sourceCodeInfo);
                            if (!result)
                                return false;
                        }
                    }
                    catch (e_4_1) { e_4 = { error: e_4_1 }; }
                    finally {
                        try {
                            if (params_1_1 && !params_1_1.done && (_c = params_1.return)) _c.call(params_1);
                        }
                        finally { if (e_4) throw e_4.error; }
                    }
                }
            }
            catch (e_3_1) { e_3 = { error: e_3_1 }; }
            finally {
                try {
                    if (_e && !_e.done && (_b = _d.return)) _b.call(_d);
                }
                finally { if (e_3) throw e_3.error; }
            }
            return true;
        },
        SomePred: function (fn, params, sourceCodeInfo, contextStack, _a) {
            var e_5, _b, e_6, _c;
            var executeFunction = _a.executeFunction;
            try {
                for (var _d = __values(fn.params), _e = _d.next(); !_e.done; _e = _d.next()) {
                    var f = _e.value;
                    try {
                        for (var params_2 = (e_6 = void 0, __values(params)), params_2_1 = params_2.next(); !params_2_1.done; params_2_1 = params_2.next()) {
                            var param = params_2_1.value;
                            var result = executeFunction(asFunctionLike(f, sourceCodeInfo), [param], contextStack, sourceCodeInfo);
                            if (result)
                                return true;
                        }
                    }
                    catch (e_6_1) { e_6 = { error: e_6_1 }; }
                    finally {
                        try {
                            if (params_2_1 && !params_2_1.done && (_c = params_2.return)) _c.call(params_2);
                        }
                        finally { if (e_6) throw e_6.error; }
                    }
                }
            }
            catch (e_5_1) { e_5 = { error: e_5_1 }; }
            finally {
                try {
                    if (_e && !_e.done && (_b = _d.return)) _b.call(_d);
                }
                finally { if (e_5) throw e_5.error; }
            }
            return false;
        },
        Fnull: function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var fnulledParams = params.map(function (param, index) { return (param === null ? toAny(fn.params[index]) : param); });
            return executeFunction(asFunctionLike(fn.function, sourceCodeInfo), fnulledParams, contextStack, sourceCodeInfo);
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
        Namespace: function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var namespace = getNamespace(fn.namespaceName);
            if (!namespace) {
                throw new LitsError("Namespace '".concat(fn.namespaceName, "' not found."), sourceCodeInfo);
            }
            var expression = namespace.functions[fn.functionName];
            if (!expression) {
                throw new LitsError("Function '".concat(fn.functionName, "' not found in namespace '").concat(fn.namespaceName, "'."), sourceCodeInfo);
            }
            return expression.evaluate(params, sourceCodeInfo, contextStack, { executeFunction: executeFunction });
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
            case NodeTypes.NormalExpression: {
                var result = evaluateNormalExpression(node, contextStack);
                if (typeof result === 'number' && Number.isNaN(result)) {
                    throw new LitsError('Number is NaN', node[2]);
                }
                return annotate(result);
            }
            case NodeTypes.SpecialExpression:
                return annotate(evaluateSpecialExpression(node, contextStack));
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
        if (!['true', 'false', 'null'].includes(reservedName)) {
            throw new LitsError("Reserved symbol ".concat(reservedName, " cannot be evaluated"), node[2]);
        }
        var value = reservedSymbolRecord[reservedName];
        return asNonUndefined(value, node[2]);
    }
    function evaluateNormalExpression(node, contextStack) {
        var _a, _b;
        var sourceCodeInfo = node[2];
        var paramNodes = node[1][1];
        var params = [];
        var placeholders = [];
        paramNodes.forEach(function (paramNode, index) {
            if (isSpreadNode(paramNode)) {
                var spreadValue = evaluateNode(paramNode[1], contextStack);
                if (Array.isArray(spreadValue)) {
                    params.push.apply(params, __spreadArray([], __read(spreadValue), false));
                }
                else {
                    throw new LitsError("Spread operator requires an array, got ".concat(valueToString(paramNode)), paramNode[2]);
                }
            }
            else if (paramNode[0] === NodeTypes.ReservedSymbol && paramNode[1] === '_') {
                placeholders.push(index);
            }
            else {
                params.push(evaluateNode(paramNode, contextStack));
            }
        });
        if (isNormalExpressionNodeWithName(node)) {
            var nameSymbol = node[1][0];
            if (placeholders.length > 0) {
                var fn = evaluateNode(nameSymbol, contextStack);
                var partialFunction = (_a = {},
                    _a[FUNCTION_SYMBOL] = true,
                    _a.function = asFunctionLike(fn, sourceCodeInfo),
                    _a.functionType = 'Partial',
                    _a.params = params,
                    _a.placeholders = placeholders,
                    _a.sourceCodeInfo = sourceCodeInfo,
                    _a.arity = toFixedArity(placeholders.length),
                    _a);
                return partialFunction;
            }
            if (isNormalBuiltinSymbolNode(nameSymbol)) {
                var type = nameSymbol[1];
                var normalExpression = builtin.allNormalExpressions[type];
                return normalExpression.evaluate(params, node[2], contextStack, { executeFunction: executeFunction });
            }
            else {
                var fn = contextStack.getValue(nameSymbol[1]);
                if (fn !== undefined) {
                    return executeFunction(asFunctionLike(fn, sourceCodeInfo), params, contextStack, sourceCodeInfo);
                }
                throw new UndefinedSymbolError(nameSymbol[1], node[2]);
            }
        }
        else {
            var fnNode = node[1][0];
            var fn = asFunctionLike(evaluateNode(fnNode, contextStack), sourceCodeInfo);
            if (placeholders.length > 0) {
                var partialFunction = (_b = {},
                    _b[FUNCTION_SYMBOL] = true,
                    _b.function = fn,
                    _b.functionType = 'Partial',
                    _b.params = params,
                    _b.placeholders = placeholders,
                    _b.sourceCodeInfo = sourceCodeInfo,
                    _b.arity = toFixedArity(placeholders.length),
                    _b);
                return partialFunction;
            }
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
        if (isNumber(fn)) {
            return evaluateNumberAsFunction(fn, params, sourceCodeInfo);
            /* v8 ignore next 4 */
        }
        throw new LitsError('Unexpected function type', sourceCodeInfo);
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
            var contexts = _a.contexts, hostValues = _a.values, nativeJsFunctions = _a.nativeJsFunctions;
            this.globalContext = asNonUndefined(contexts[0]);
            this.contexts = contexts;
            this.values = hostValues;
            this.nativeJsFunctions = nativeJsFunctions;
        }
        ContextStackImpl.prototype.create = function (context) {
            var globalContext = this.globalContext;
            var contextStack = new ContextStackImpl({
                contexts: __spreadArray([context], __read(this.contexts), false),
                values: this.values,
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
                    if (name_1 === 'self') {
                        throw new LitsError("Cannot shadow builtin value \"".concat(name_1, "\""), sourceCodeInfo);
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
            if (this.contexts[0] !== this.globalContext) {
                this.addValues(values, sourceCodeInfo);
            }
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
                    if (name_2 === 'self') {
                        throw new LitsError("Cannot shadow builtin value \"".concat(name_2, "\""), sourceCodeInfo);
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
            var _b, _c;
            try {
                for (var _d = __values(this.contexts), _e = _d.next(); !_e.done; _e = _d.next()) {
                    var context = _e.value;
                    var contextEntry = context[name];
                    if (contextEntry)
                        return contextEntry.value;
                }
            }
            catch (e_3_1) { e_3 = { error: e_3_1 }; }
            finally {
                try {
                    if (_e && !_e.done && (_a = _d.return)) _a.call(_d);
                }
                finally { if (e_3) throw e_3.error; }
            }
            var nativeJsFunction = (_b = this.nativeJsFunctions) === null || _b === void 0 ? void 0 : _b[name];
            if (nativeJsFunction)
                return nativeJsFunction;
            return (_c = this.values) === null || _c === void 0 ? void 0 : _c[name];
        };
        ContextStackImpl.prototype.lookUp = function (node) {
            var e_4, _a;
            var _b, _c;
            var value = node[1];
            try {
                for (var _d = __values(this.contexts), _e = _d.next(); !_e.done; _e = _d.next()) {
                    var context = _e.value;
                    var contextEntry = context[value];
                    if (contextEntry)
                        return contextEntry;
                }
            }
            catch (e_4_1) { e_4 = { error: e_4_1 }; }
            finally {
                try {
                    if (_e && !_e.done && (_a = _d.return)) _a.call(_d);
                }
                finally { if (e_4) throw e_4.error; }
            }
            var hostValue = (_b = this.values) === null || _b === void 0 ? void 0 : _b[value];
            if (hostValue !== undefined) {
                return {
                    value: toAny(hostValue),
                };
            }
            var nativeJsFunction = (_c = this.nativeJsFunctions) === null || _c === void 0 ? void 0 : _c[value];
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
                    case specialExpressionTypes['??']: {
                        var specialExpression = asNonUndefined(builtin.specialExpressions[functionType], node[2]);
                        return _a = {},
                            _a[FUNCTION_SYMBOL] = true,
                            _a.functionType = 'SpecialBuiltin',
                            _a.specialBuiltinSymbolType = functionType,
                            _a.sourceCodeInfo = node[2],
                            _a.arity = specialExpression.arity,
                            _a;
                    }
                    default:
                        throw new LitsError("Unknown special builtin symbol type: ".concat(functionType), node[2]);
                }
            }
            if (isNormalBuiltinSymbolNode(node)) {
                var type = node[1];
                var normalExpression = allNormalExpressions[type];
                var name_3 = normalExpression.name;
                return _b = {},
                    _b[FUNCTION_SYMBOL] = true,
                    _b.functionType = 'Builtin',
                    _b.normalBuitinSymbolType = type,
                    _b.sourceCodeInfo = node[2],
                    _b.arity = normalExpression.arity,
                    _b.name = name_3,
                    _b;
            }
            var lookUpResult = this.lookUp(node);
            if (isContextEntry(lookUpResult))
                return lookUpResult.value;
            throw new UndefinedSymbolError(node[1], node[2]);
        };
        return ContextStackImpl;
    }());
    function checkNotDefined(name) {
        if (specialExpressionKeys.includes(name)) {
            console.warn("Cannot shadow special expression \"".concat(name, "\", ignoring."));
            return false;
        }
        if (normalExpressionKeys.includes(name)) {
            console.warn("Cannot shadow builtin function \"".concat(name, "\", ignoring."));
            return false;
        }
        if (name === 'self') {
            console.warn("Cannot shadow builtin value \"".concat(name, "\", ignoring."));
            return false;
        }
        return true;
    }
    function createContextStack(params) {
        var _a;
        if (params === void 0) { params = {}; }
        var globalContext = (_a = params.globalContext) !== null && _a !== void 0 ? _a : {};
        // Contexts are checked from left to right
        var contexts = params.contexts ? __spreadArray([globalContext], __read(params.contexts), false) : [globalContext];
        var contextStack = new ContextStackImpl({
            contexts: contexts,
            values: params.values,
            nativeJsFunctions: params.jsFunctions
                && Object.entries(params.jsFunctions).reduce(function (acc, _a) {
                    var e_5, _b, _c;
                    var _d, _e;
                    var _f = __read(_a, 2), identifier = _f[0], entry = _f[1];
                    var identifierParts = identifier.split('.');
                    var name = identifierParts.pop();
                    if (/^[A-Z]/.test(name)) {
                        console.warn("Invalid identifier \"".concat(identifier, "\" in jsFunctions, function name must not start with an uppercase letter"), undefined);
                        return acc;
                    }
                    var scope = acc;
                    try {
                        for (var identifierParts_1 = __values(identifierParts), identifierParts_1_1 = identifierParts_1.next(); !identifierParts_1_1.done; identifierParts_1_1 = identifierParts_1.next()) {
                            var part = identifierParts_1_1.value;
                            if (part.length === 0) {
                                console.warn("Invalid empty identifier \"".concat(identifier, "\" in nativeJsFunctions"), undefined);
                                return acc;
                            }
                            if (!/^[A-Z]/.test(part)) {
                                console.warn("Invalid identifier \"".concat(identifier, "\" in jsFunctions, module name must start with an uppercase letter"), undefined);
                                return acc;
                            }
                            if (!scope[part]) {
                                scope[part] = {};
                            }
                            scope = scope[part];
                        }
                    }
                    catch (e_5_1) { e_5 = { error: e_5_1 }; }
                    finally {
                        try {
                            if (identifierParts_1_1 && !identifierParts_1_1.done && (_b = identifierParts_1.return)) _b.call(identifierParts_1);
                        }
                        finally { if (e_5) throw e_5.error; }
                    }
                    assertJsFunction(entry);
                    var natifeFn = (_c = {
                            functionType: 'NativeJsFunction',
                            nativeFn: entry,
                            name: name
                        },
                        _c[FUNCTION_SYMBOL] = true,
                        _c.arity = (_d = entry.arity) !== null && _d !== void 0 ? _d : {},
                        _c.docString = (_e = entry.docString) !== null && _e !== void 0 ? _e : '',
                        _c);
                    if (scope === acc && !checkNotDefined(name)) {
                        return acc;
                    }
                    scope[name] = natifeFn;
                    return acc;
                }, {}),
        });
        return params.globalModuleScope ? contextStack : contextStack.create({});
    }

    var binaryOperators = [
        '^', // exponentiation
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
        '==', // equal
        '!=', // not equal
        '≠', // not equal
        '&', // bitwise AND
        'xor', // bitwise XOR
        '|', // bitwise OR
        '&&', // logical AND
        '||', // logical OR
        '??', // nullish coalescing
        '|>', // pipe
    ];
    var otherOperators = [
        '?', // conditional operator
        ':', // conditional operator
        '->', // lambda
        '...', // rest
        '.', // property accessor
        ',', // item separator
        '=', // assignment
        ':', // property assignment
        ';', // statement terminator
    ];
    var symbolicOperators = __spreadArray(__spreadArray([], __read(binaryOperators), false), __read(otherOperators), false);
    var nonFunctionOperators = [
        'comment',
        'cond',
        'def',
        'defined?',
        'block',
        'doseq',
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
    var tokenizeDocString = function (input, position) {
        if (input[position] !== '"' || input[position + 1] !== '"' || input[position + 2] !== '"')
            return NO_MATCH;
        var value = '"""';
        var length = 3;
        var char = input[position + length];
        var nextThreeChars = input.slice(position + length, position + length + 3);
        var escaping = false;
        while (char && (nextThreeChars !== '"""' || escaping)) {
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
            nextThreeChars = input.slice(position + length, position + length + 3);
        }
        if (!char) {
            return [length, ['Error', value, undefined, "Unclosed doc string at position ".concat(position)]];
        }
        value += '"""'; // closing quote
        return [length + 3, ['DocString', value]];
    };
    var tokenizeString = function (input, position) {
        if (input[position] !== '"')
            return NO_MATCH;
        var value = '"';
        var length = 1;
        var char = input[position + length];
        var escaping = false;
        while (char && (char !== '"' || escaping)) {
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
        if (!char) {
            return [length, ['Error', value, undefined, "Unclosed string at position ".concat(position)]];
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
        if (token[0] === 'Error') {
            var errorToken = ['Error', "#".concat(token[1]), undefined, "Unclosed regexp at position ".concat(position)];
            return [stringLength + 1, errorToken];
        }
        position += stringLength + 1;
        var length = stringLength + 1;
        var options = '';
        while (input[position] === 'g' || input[position] === 'i') {
            options += input[position];
            length += 1;
            position += 1;
            if (options.includes(input[position])) {
                return [length, ['Error', "#".concat(token[1]).concat(options), undefined, "Duplicated regexp option \"".concat(input[position], "\"")]];
            }
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
                    if (i === start) {
                        return NO_MATCH;
                    }
                    return [i - position + 1, ['Error', input.substring(position, i + 1), undefined, "Invalid number format at position ".concat(i + 1)]];
                }
            }
            else if (char === '.') {
                if (i === start) {
                    return NO_MATCH;
                }
                if (hasDecimalPoint || hasExponent) {
                    return [i - position + 1, ['Error', input.substring(position, i + 1), undefined, "Invalid number format at position ".concat(i + 1)]];
                }
                hasDecimalPoint = true;
            }
            else if (char === 'e' || char === 'E') {
                if (i === start) {
                    return NO_MATCH;
                }
                if (hasExponent) {
                    return [i - position + 1, ['Error', input.substring(position, i + 1), undefined, "Invalid number format at position ".concat(i + 1)]];
                }
                if (input[i - 1] === '.' || input[i - 1] === '+' || input[i - 1] === '-') {
                    return [i - position + 1, ['Error', input.substring(position, i + 1), undefined, "Invalid number format at position ".concat(i + 1)]];
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
        if (nextChar && nextChar !== ':' && !postNumberRegExp.test(nextChar)) {
            return [i - position + 1, ['Error', input.substring(position, i + 1), undefined, "Invalid number format at position ".concat(i + 1)]];
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
                    return [length_1, ['Error', value, undefined, "Unclosed quoted symbol at position ".concat(position)]];
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
            // : can be used as symbol character, but it must not be the last character
            return value.endsWith(':')
                ? [position - initialPosition - 1, ['Symbol', value.slice(0, -1)]]
                : [position - initialPosition, ['Symbol', value]];
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
                return [length_2, ['Error', value, undefined, "Unclosed multi-line comment at position ".concat(position)]];
            }
            value += '*/';
            length_2 += 2;
            return [length_2, ['MultiLineComment', value]];
        }
        return NO_MATCH;
    };
    var tokenizeShebang = function (input, position) {
        if (input[position] === '#' && input[position + 1] === '!') {
            var length_3 = 2;
            var value = '#!';
            while (input[position + length_3] !== '\n' && position + length_3 < input.length) {
                value += input[position + length_3];
                length_3 += 1;
            }
            return [length_3, ['SingleLineComment', value]];
        }
        return NO_MATCH;
    };
    var tokenizeSingleLineComment = function (input, position) {
        if (input[position] === '/' && input[position + 1] === '/') {
            var length_4 = 2;
            var value = '//';
            while (input[position + length_4] !== '\n' && position + length_4 < input.length) {
                value += input[position + length_4];
                length_4 += 1;
            }
            return [length_4, ['SingleLineComment', value]];
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
        tokenizeDocString,
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
            var sourceCodeInfo = debug
                ? createSourceCodeInfo(input, position, filePath)
                : undefined;
            var tokenDescriptor = getCurrentToken(input, position);
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
        if (position === 0) {
            var _b = __read(tokenizeShebang(input, position), 2), nbrOfCharacters = _b[0], token = _b[1];
            position += nbrOfCharacters;
            if (nbrOfCharacters > 0) {
                return [position - initialPosition, token];
            }
        }
        try {
            for (var tokenizers_1 = __values(tokenizers), tokenizers_1_1 = tokenizers_1.next(); !tokenizers_1_1.done; tokenizers_1_1 = tokenizers_1.next()) {
                var tokenizer = tokenizers_1_1.value;
                var _c = __read(tokenizer(input, position), 2), nbrOfCharacters = _c[0], token = _c[1];
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
        return [1, ['Error', input[initialPosition], undefined, 'Unrecognized character']];
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
    function isShebangToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'Shebang';
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
    function isDocStringToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'DocString';
    }
    function isA_BinaryOperatorToken(token) {
        return (token === null || token === void 0 ? void 0 : token[0]) === 'Operator' && isBinaryOperator(token[1]);
    }
    function throwUnexpectedToken(expected, expectedValue, actual) {
        var actualOutput = actual ? "".concat(actual[0], " '").concat(actual[1], "'") : 'end of input';
        throw new LitsError("Unexpected token: ".concat(actualOutput, ", expected ").concat(expected).concat(expectedValue ? " '".concat(expectedValue, "'") : ''), actual === null || actual === void 0 ? void 0 : actual[2]);
    }

    function minifyTokenStream(tokenStream, _a) {
        var removeWhiteSpace = _a.removeWhiteSpace;
        var tokens = tokenStream.tokens
            .filter(function (token) {
            if (isSingleLineCommentToken(token)
                || isMultiLineCommentToken(token)
                || isShebangToken(token)
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

    var exponentiationPrecedence = 12;
    var binaryFunctionalOperatorPrecedence = 3;
    var conditionalOperatorPrecedence = 1;
    var placeholderRegexp = /^\$([1-9]\d?)?$/;
    function withSourceCodeInfo(node, sourceCodeInfo) {
        if (sourceCodeInfo) {
            node[2] = sourceCodeInfo;
        }
        return node;
    }
    function getPrecedence(operatorSign, sourceCodeInfo) {
        switch (operatorSign) {
            case '^': // exponentiation
                return exponentiationPrecedence;
            case '*': // multiplication
            case '/': // division
            case '%': // remainder
                return 11;
            case '+': // addition
            case '-': // subtraction
                return 10;
            case '<<': // left shift
            case '>>': // signed right shift
            case '>>>': // unsigned right shift
                return 9;
            case '++': // string concatenation
                return 8;
            case '<': // less than
            case '<=': // less than or equal
            case '≤': // less than or equal
            case '>': // greater than
            case '>=': // greater than or equal
            case '≥': // greater than or equal
                return 7;
            case '==': // equal
            case '!=': // not equal
            case '≠': // not equal
                return 6;
            case '&': // bitwise AND
            case 'xor': // bitwise XOR
            case '|': // bitwise OR
                return 5;
            case '&&': // logical AND
            case '||': // logical OR
            case '??': // nullish coalescing
                return 4;
            // leave room for binaryFunctionalOperatorPrecedence = 3
            case '|>': // pipe
                return 2;
            // leave room for conditionalOperatorPrecedence = 1
            /* v8 ignore next 2 */
            default:
                throw new LitsError("Unknown binary operator: ".concat(operatorSign), sourceCodeInfo);
        }
    }
    function createNamedNormalExpressionNode(symbolNode, params, sourceCodeInfo) {
        var node = withSourceCodeInfo([NodeTypes.NormalExpression, [symbolNode, params]], sourceCodeInfo);
        if (isNormalBuiltinSymbolNode(symbolNode)) {
            assertNumberOfParams(allNormalExpressions[symbolNode[1]].arity, node[1][1].length, sourceCodeInfo);
        }
        return node;
    }
    function createAccessorNode(left, right, sourceCodeInfo) {
        return withSourceCodeInfo([NodeTypes.NormalExpression, [[NodeTypes.NormalBuiltinSymbol, normalExpressionTypes.get], [left, right]]], sourceCodeInfo);
    }
    function fromBinaryOperatorToNode(operator, symbolNode, left, right, sourceCodeInfo) {
        var operatorName = operator[1];
        switch (operatorName) {
            case '^': // exponentiation
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
            case '==':
            case '!=':
            case '≠':
            case '&':
            case 'xor':
            case '|':
            case '|>':
                return createNamedNormalExpressionNode(symbolNode, [left, right], sourceCodeInfo);
            case '&&':
            case '||':
            case '??':
                return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes[operatorName], [left, right]]], sourceCodeInfo);
            /* v8 ignore next 11 */
            case '.':
            case ';':
            case ':':
            case '=':
            case ',':
            case '->':
            case '...':
            case '?':
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
        Parser.prototype.peekSourceCodeInfo = function () {
            var _a;
            var currentToken = this.peek();
            return currentToken ? currentToken[2] : (_a = this.tokenStream.tokens.at(-1)) === null || _a === void 0 ? void 0 : _a[2];
        };
        Parser.prototype.peekAhead = function (count) {
            return this.tokenStream.tokens[this.parseState.position + count];
        };
        Parser.prototype.advance = function () {
            this.parseState.position += 1;
        };
        Parser.prototype.parse = function () {
            this.tokenStream.tokens.forEach(function (token) {
                if (token[0] === 'Error') {
                    throw new LitsError(token[3], token[2]);
                }
            });
            var nodes = [];
            while (!this.isAtEnd()) {
                nodes.push(this.parseExpression(0, true));
                if (isOperatorToken(this.peek(), ';')) {
                    this.advance();
                }
                else {
                    if (!this.isAtEnd()) {
                        throw new LitsError('Expected ;', this.peekSourceCodeInfo());
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
                    case 'loop':
                        left = this.parseLoop(firstToken);
                        break;
                    case 'try':
                        left = this.parseTry(firstToken);
                        break;
                }
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
                        // ^ (exponentiation) is right associative
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
                else if ((operator === null || operator === void 0 ? void 0 : operator[1]) === '?') {
                    if (conditionalOperatorPrecedence <= precedence) {
                        break;
                    }
                    this.advance();
                    var trueNode = this.parseExpression();
                    if (!isOperatorToken(this.peek(), ':')) {
                        throw new LitsError('Expected :', this.peekSourceCodeInfo());
                    }
                    this.advance();
                    var falseNode = this.parseExpression();
                    left = withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.if, [left, trueNode, falseNode]]], left[2]);
                }
                else {
                    break;
                }
                operator = this.peek();
            }
            return left;
        };
        Parser.prototype.asToken = function (token) {
            if (!token) {
                throw new LitsError('Unexpected end of input', this.peekSourceCodeInfo());
            }
            return token;
        };
        Parser.prototype.parseOperand = function () {
            var operand = this.parseOperandPart();
            var token = this.peek();
            while (isOperatorToken(token, '.') || isLBracketToken(token) || isLParenToken(token)) {
                if (token[1] === '.') {
                    this.advance();
                    var symbolToken = this.asToken(this.peek());
                    if (!isSymbolToken(symbolToken)) {
                        throw new LitsError('Expected symbol', this.peekSourceCodeInfo());
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
                        throw new LitsError('Expected closing bracket', this.peekSourceCodeInfo());
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
            var token = this.asToken(this.peek());
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
                    throw new LitsError('Expected closing parenthesis', this.peekSourceCodeInfo());
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
            // Object litteral, e.g. {a: 1, b: 2}
            // Or block.
            if (isLBraceToken(token)) {
                var positionBefore = this.parseState.position;
                try {
                    return this.parseObject();
                }
                catch (_a) {
                    this.parseState.position = positionBefore;
                    return this.parseBlock()[0];
                }
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
                    return this.parseString(token);
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
                    params.push(withSourceCodeInfo([NodeTypes.Spread, this.parseExpression()], this.peekSourceCodeInfo()));
                }
                else {
                    var token = this.peek();
                    if (isStringToken(token)) {
                        var stringNode = this.parseString(token);
                        params.push(withSourceCodeInfo([NodeTypes.String, stringNode[1]], token[2]));
                    }
                    else if (isSymbolToken(token)) {
                        var value = token[1].startsWith('\'')
                            ? this.stringFromQuotedSymbol(token[1])
                            : token[1];
                        params.push(withSourceCodeInfo([NodeTypes.String, value], token[2]));
                        this.advance();
                    }
                    else if (isLBracketToken(token)) {
                        this.advance();
                        params.push(this.parseExpression());
                        assertRBracketToken(this.peek());
                        this.advance();
                    }
                    else {
                        throw new LitsError('Expected key to be a symbol or a string', this.peekSourceCodeInfo());
                    }
                    assertOperatorToken(this.peek(), ':');
                    this.advance();
                    params.push(this.parseExpression());
                }
                var nextToken = this.peek();
                if (!isOperatorToken(nextToken, ',') && !isRBraceToken(nextToken)) {
                    throw new LitsError('Expected comma or closing brace', this.peekSourceCodeInfo());
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
                    params.push(withSourceCodeInfo([NodeTypes.Spread, this.parseExpression()], this.peekSourceCodeInfo()));
                }
                else {
                    params.push(this.parseExpression());
                }
                var nextToken = this.peek();
                if (!isOperatorToken(nextToken, ',') && !isRBracketToken(nextToken)) {
                    throw new LitsError('Expected comma or closing parenthesis', this.peekSourceCodeInfo());
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
            var _a;
            this.advance();
            var params = [];
            while (!this.isAtEnd() && !isRParenToken(this.peek())) {
                if (isOperatorToken(this.peek(), '...')) {
                    this.advance();
                    params.push(withSourceCodeInfo([NodeTypes.Spread, this.parseExpression()], this.peekSourceCodeInfo()));
                }
                else {
                    params.push(this.parseExpression());
                }
                var nextToken = this.peek();
                if (!isOperatorToken(nextToken, ',') && !isRParenToken(nextToken)) {
                    throw new LitsError('Expected comma or closing parenthesis', (_a = this.peek()) === null || _a === void 0 ? void 0 : _a[2]);
                }
                if (isOperatorToken(nextToken, ',')) {
                    this.advance();
                }
            }
            if (!isRParenToken(this.peek())) {
                throw new LitsError('Expected closing parenthesis', this.peekSourceCodeInfo());
            }
            this.advance();
            if (isSpecialBuiltinSymbolNode(symbol)) { // Named function
                var specialExpressionType = symbol[1];
                var type = specialExpressionType;
                var specialExpression = builtin.specialExpressions[type];
                assertNumberOfParams(specialExpression.arity, params.length, symbol[2]);
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
                        else {
                            return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, [params[0], params[1]]]], symbol[2]);
                        }
                    }
                    case specialExpressionTypes['defined?']: {
                        var _b = __read(params, 1), param = _b[0];
                        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, param]], symbol[2]);
                    }
                    case specialExpressionTypes.throw: {
                        var _c = __read(params, 1), param = _c[0];
                        return withSourceCodeInfo([NodeTypes.SpecialExpression, [type, param]], symbol[2]);
                    }
                    case specialExpressionTypes['0_lambda']:
                    case specialExpressionTypes['0_def']:
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
            var firstToken = this.asToken(this.peek());
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
                var nodes = void 0;
                var docString = '';
                if (isLBraceToken(this.peek())) {
                    var positionBefore = this.parseState.position;
                    try {
                        var objectNode = this.parseObject();
                        nodes = [objectNode];
                    }
                    catch (_a) {
                        this.parseState.position = positionBefore;
                        var parsedBlock = this.parseBlock(true);
                        docString = parsedBlock[1];
                        nodes = parsedBlock[0][1][1];
                    }
                }
                else {
                    nodes = [this.parseExpression()];
                }
                return withSourceCodeInfo([
                    NodeTypes.SpecialExpression,
                    [
                        specialExpressionTypes['0_lambda'],
                        [
                            functionArguments,
                            nodes,
                        ],
                        docString,
                    ],
                ], firstToken[2]);
            }
            catch (_b) {
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
                    throw new LitsError('Rest argument must be last', this.peekSourceCodeInfo());
                }
                var bindingTarget = this.parseBindingTarget();
                if (bindingTarget[1][1] !== undefined) {
                    defaults = true;
                }
                if (bindingTarget[0] === bindingTargetTypes.rest) {
                    rest = true;
                }
                if (defaults && !bindingTarget[1][1]) {
                    throw new LitsError('Default arguments must be last', this.peekSourceCodeInfo());
                }
                functionArguments.push(bindingTarget);
                if (!isOperatorToken(this.peek(), ',') && !isRParenToken(this.peek()) && !isSymbolToken(this.peek(), 'let')) {
                    throw new LitsError('Expected comma or closing parenthesis', this.peekSourceCodeInfo());
                }
                if (isOperatorToken(this.peek(), ',')) {
                    this.advance();
                }
            }
            if (!isRParenToken(this.peek())) {
                throw new LitsError('Expected closing parenthesis', this.peekSourceCodeInfo());
            }
            this.advance();
            return functionArguments;
        };
        Parser.prototype.parseShorthandLamdaFunction = function () {
            var _a;
            var firstToken = this.asToken(this.peek());
            this.advance();
            var startPos = this.parseState.position;
            var nodes;
            var docString = '';
            if (isLBraceToken(this.peek())) {
                var positionBefore = this.parseState.position;
                try {
                    var objectNode = this.parseObject();
                    nodes = [objectNode];
                }
                catch (_b) {
                    this.parseState.position = positionBefore;
                    var parsedBlock = this.parseBlock(true);
                    docString = parsedBlock[1];
                    nodes = parsedBlock[0][1][1];
                }
            }
            else {
                nodes = [this.parseExpression()];
            }
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
            var node = withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes['0_lambda'], [
                        functionArguments,
                        nodes,
                    ], docString]], firstToken[2]);
            return node;
        };
        Parser.prototype.parseOptionalDefaulValue = function () {
            if (isOperatorToken(this.peek(), '=')) {
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
                    throw new LitsError('Expected assignment', this.peekSourceCodeInfo());
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
                if (isOperatorToken(this.peek(), '=')) {
                    throw new LitsError('Rest argument can not have default value', this.peekSourceCodeInfo());
                }
                return withSourceCodeInfo([bindingTargetTypes.rest, [symbol[1], undefined]], firstToken[2]);
            }
            // Array
            if (isLBracketToken(firstToken)) {
                this.advance();
                var elements = [];
                var token = this.asToken(this.peek());
                var rest = false;
                while (!isRBracketToken(token)) {
                    if (rest) {
                        throw new LitsError('Rest argument must be last', token[2]);
                    }
                    if (isOperatorToken(token, ',')) {
                        elements.push(null);
                        this.advance();
                        token = this.asToken(this.peek());
                        continue;
                    }
                    var target = this.parseBindingTarget();
                    if (target[0] === bindingTargetTypes.rest) {
                        rest = true;
                    }
                    elements.push(target);
                    token = this.asToken(this.peek());
                    if (!isRBracketToken(token)) {
                        assertOperatorToken(token, ',');
                        this.advance();
                    }
                    token = this.asToken(this.peek());
                }
                this.advance();
                var defaultValue = this.parseOptionalDefaulValue();
                if (requireDefaultValue && !defaultValue) {
                    throw new LitsError('Expected assignment', this.peekSourceCodeInfo());
                }
                return withSourceCodeInfo([bindingTargetTypes.array, [elements, defaultValue]], firstToken[2]);
            }
            // Object
            if (isLBraceToken(firstToken)) {
                this.advance();
                var elements = {};
                var token = this.asToken(this.peek());
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
                    token = this.asToken(this.peek());
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
                    else if (isRBraceToken(token) || isOperatorToken(token, ',') || isOperatorToken(token, '=')) {
                        if (elements[key[1]]) {
                            throw new LitsError("Duplicate binding name: ".concat(key), token[2]);
                        }
                        if (rest && isOperatorToken(this.peek(), '=')) {
                            throw new LitsError('Rest argument can not have default value', this.peekSourceCodeInfo());
                        }
                        elements[key[1]] = rest
                            ? withSourceCodeInfo([bindingTargetTypes.rest, [key[1], this.parseOptionalDefaulValue()]], firstToken[2])
                            : withSourceCodeInfo([bindingTargetTypes.symbol, [key, this.parseOptionalDefaulValue()]], firstToken[2]);
                    }
                    else if (isOperatorToken(token, ':')) {
                        this.advance();
                        token = this.asToken(this.peek());
                        if (!isLBraceToken(token) && !isLBracketToken(token)) {
                            throw new LitsError('Expected object or array', token[2]);
                        }
                        elements[key[1]] = this.parseBindingTarget();
                    }
                    if (!isRBraceToken(this.peek())) {
                        assertOperatorToken(this.peek(), ',');
                        this.advance();
                    }
                    token = this.asToken(this.peek());
                }
                this.advance();
                token = this.asToken(this.peek());
                var defaultValue = this.parseOptionalDefaulValue();
                if (requireDefaultValue && !defaultValue) {
                    throw new LitsError('Expected assignment', token[2]);
                }
                return withSourceCodeInfo([bindingTargetTypes.object, [elements, defaultValue]], firstToken[2]);
            }
            throw new LitsError('Expected symbol', this.peekSourceCodeInfo());
        };
        Parser.prototype.parseLet = function (token) {
            this.advance();
            var target = this.parseBindingTarget({ requireDefaultValue: true, noRest: true });
            var value = target[1][1];
            target[1][1] = undefined;
            var bindingTarget = withSourceCodeInfo([NodeTypes.Binding, [target, value]], token[2]);
            return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.let, bindingTarget]], token[2]);
        };
        Parser.prototype.parseBlock = function (allowDocString) {
            if (allowDocString === void 0) { allowDocString = false; }
            var token = asLBraceToken(this.peek());
            this.advance();
            var docString = '';
            if (allowDocString && isDocStringToken(this.peek())) {
                docString = this.parseDocString();
            }
            var expressions = [];
            while (!this.isAtEnd() && !isRBraceToken(this.peek())) {
                expressions.push(this.parseExpression());
                if (isOperatorToken(this.peek(), ';')) {
                    this.advance();
                }
                else if (!isRBraceToken(this.peek())) {
                    throw new LitsError('Expected }', this.peekSourceCodeInfo());
                }
            }
            assertRBraceToken(this.peek());
            this.advance();
            return [
                withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.block, expressions]], token[2]),
                docString,
            ];
        };
        Parser.prototype.parseImplicitBlock = function (ends) {
            var nodes = [];
            while (!this.isAtEnd() && !this.isImplicitBlockEnd(ends)) {
                if (isOperatorToken(this.peek(), ';')) {
                    this.advance();
                }
                else {
                    nodes.push(this.parseExpression());
                }
            }
            this.assertImplicitBlockEnd(ends);
            if (nodes.length === 0) {
                throw new LitsError('Expected expression', this.peekSourceCodeInfo());
            }
            return nodes.length === 1
                ? nodes[0]
                : withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.block, nodes]], this.peekSourceCodeInfo());
        };
        Parser.prototype.assertImplicitBlockEnd = function (ends) {
            if (!this.isImplicitBlockEnd(ends)) {
                throw new LitsError("Expected ".concat(ends.map(function (e) { return e[1]; }).join(' or ')), this.peekSourceCodeInfo());
            }
        };
        Parser.prototype.isImplicitBlockEnd = function (ends) {
            var e_1, _a;
            try {
                for (var ends_1 = __values(ends), ends_1_1 = ends_1.next(); !ends_1_1.done; ends_1_1 = ends_1.next()) {
                    var end = ends_1_1.value;
                    if (isReservedSymbolToken(this.peek(), end)) {
                        return true;
                    }
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (ends_1_1 && !ends_1_1.done && (_a = ends_1.return)) _a.call(ends_1);
                }
                finally { if (e_1) throw e_1.error; }
            }
            return false;
        };
        Parser.prototype.parseLoop = function (firstToken) {
            this.advance();
            assertLParenToken(this.peek());
            this.advance();
            var bindingNodes = [];
            var token = this.peek();
            while (!this.isAtEnd() && !isRParenToken(token)) {
                var target = this.parseBindingTarget({ requireDefaultValue: true, noRest: true });
                var value = target[1][1];
                target[1][1] = undefined;
                bindingNodes.push(withSourceCodeInfo([NodeTypes.Binding, [target, value]], target[2]));
                if (isOperatorToken(this.peek(), ',')) {
                    this.advance();
                }
                token = this.peek();
            }
            if (bindingNodes.length === 0) {
                throw new LitsError('Expected binding', this.peekSourceCodeInfo());
            }
            assertRParenToken(token);
            this.advance();
            assertOperatorToken(this.peek(), '->');
            this.advance();
            var expression = this.parseExpression();
            return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.loop, bindingNodes, expression]], firstToken[2]);
        };
        Parser.prototype.parseTry = function (token) {
            this.advance();
            var tryExpression = this.parseImplicitBlock(['catch']);
            this.advance();
            var errorSymbol;
            if (isLParenToken(this.peek())) {
                this.advance();
                errorSymbol = this.parseSymbol();
                assertRParenToken(this.peek());
                this.advance();
            }
            var catchExpression = this.parseImplicitBlock(['end']);
            this.advance();
            return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.try, tryExpression, errorSymbol, catchExpression]], token[2]);
        };
        Parser.prototype.parseForOrDoseq = function (firstToken) {
            var isDoseq = firstToken[1] === 'doseq';
            this.advance();
            assertLParenToken(this.peek());
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
                if (isOperatorToken(this_1.peek(), ',')) {
                    this_1.advance();
                }
            };
            var this_1 = this;
            while (!this.isAtEnd() && !isRParenToken(this.peek())) {
                _loop_1();
            }
            assertRParenToken(this.peek());
            this.advance();
            assertOperatorToken(this.peek(), '->');
            this.advance();
            var expression = this.parseExpression();
            return isDoseq
                ? withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.doseq, forLoopBindings, expression]], firstToken[2])
                : withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.for, forLoopBindings, expression]], firstToken[2]);
        };
        Parser.prototype.parseForLoopBinding = function () {
            var bindingNode = this.parseBinding();
            var modifiers = [];
            var token = this.asToken(this.peek());
            this.assertInternalLoopBindingDelimiter(token, ['let', 'when', 'while']);
            var letBindings = [];
            if (token[1] === 'let') {
                modifiers.push('&let');
                var _loop_2 = function () {
                    var letNode = this_2.parseLet(token);
                    var existingBoundNames = letBindings.flatMap(function (b) { return Object.keys(getAllBindingTargetNames(b[1][0])); });
                    var newBoundNames = Object.keys(getAllBindingTargetNames(letNode[1][1][1][0]));
                    if (newBoundNames.some(function (n) { return existingBoundNames.includes(n); })) {
                        throw new LitsError('Duplicate binding', letNode[1][1][2]);
                    }
                    letBindings.push(letNode[1][1]);
                    token = this_2.asToken(this_2.peek());
                    this_2.assertInternalLoopBindingDelimiter(token, ['let', 'when', 'while']);
                    token = this_2.asToken(this_2.peek());
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
                    modifiers.push('&when');
                    whenNode = this.parseExpression();
                }
                else {
                    modifiers.push('&while');
                    whileNode = this.parseExpression();
                }
                token = this.asToken(this.peek());
                var symbols = modifiers.includes('&when') && modifiers.includes('&while')
                    ? []
                    : modifiers.includes('&when')
                        ? ['while']
                        : ['when'];
                this.assertInternalLoopBindingDelimiter(token, symbols);
                token = this.asToken(this.peek());
            }
            this.assertInternalLoopBindingDelimiter(token, []);
            return [bindingNode, letBindings, whenNode, whileNode];
        };
        Parser.prototype.assertInternalLoopBindingDelimiter = function (token, symbols) {
            if (!this.isInternalLoopBindingDelimiter(token, symbols)) {
                var symbolsString = "".concat(__spreadArray(__spreadArray([], __read(symbols), false), [','], false).map(function (symbol) { return "\"".concat(symbol, "\""); }).join(', '), " or \")\"");
                throw new LitsError("Expected symbol ".concat(symbolsString), token[2]);
            }
        };
        Parser.prototype.isInternalLoopBindingDelimiter = function (token, symbols) {
            var e_2, _a;
            // end of loop binding
            if (isOperatorToken(token, ',') || isRParenToken(token)) {
                return true;
            }
            try {
                for (var symbols_1 = __values(symbols), symbols_1_1 = symbols_1.next(); !symbols_1_1.done; symbols_1_1 = symbols_1.next()) {
                    var symbol = symbols_1_1.value;
                    if (symbol === 'let' && isSymbolToken(token, 'let')) {
                        return true;
                    }
                    if (['when', 'while'].includes(symbol) && isReservedSymbolToken(token, symbol)) {
                        return true;
                    }
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (symbols_1_1 && !symbols_1_1.done && (_a = symbols_1.return)) _a.call(symbols_1);
                }
                finally { if (e_2) throw e_2.error; }
            }
            return false;
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
            var thenExpression = this.parseImplicitBlock(['else', 'end']);
            var elseExpression;
            if (isReservedSymbolToken(this.peek(), 'else')) {
                this.advance();
                elseExpression = this.parseImplicitBlock(['end']);
            }
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
                var thenExpression = this.parseImplicitBlock(['case', 'end']);
                params.push([caseExpression, thenExpression]);
                if (isReservedSymbolToken(this.peek(), 'end')) {
                    break;
                }
            }
            assertReservedSymbolToken(this.peek());
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
                var thenExpression = this.parseImplicitBlock(['case', 'end']);
                params.push([caseExpression, thenExpression]);
                if (isReservedSymbolToken(this.peek(), 'end')) {
                    break;
                }
            }
            assertReservedSymbolToken(this.peek(), 'end');
            this.advance();
            return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes.switch, valueExpression, params]], token[2]);
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
                return [';', ',', ':'].includes(token[1]);
            }
            if (isReservedSymbolToken(token)) {
                return ['else', 'when', 'while', 'case', 'catch', 'let', 'then'].includes(token[1]);
            }
            return false;
        };
        Parser.prototype.parseExport = function (exportToken) {
            this.advance();
            var token = this.peek();
            if (isSymbolToken(token, 'let')) {
                var letNode = this.parseLet(asSymbolToken(token));
                return withSourceCodeInfo([NodeTypes.SpecialExpression, [specialExpressionTypes['0_def'], letNode[1][1]]], exportToken[2]);
            }
            else {
                throw new LitsError('Expected let', this.peekSourceCodeInfo());
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
            var token = this.asToken(this.peek());
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
            var token = this.asToken(this.peek());
            this.advance();
            var value = token[1];
            var negative = value[0] === '-';
            var numberString = (negative ? value.substring(1) : value).replace(/_/g, '');
            return withSourceCodeInfo([NodeTypes.Number, negative ? -Number(numberString) : Number(numberString)], token[2]);
        };
        Parser.prototype.parseDocString = function () {
            var token = this.asToken(this.peek());
            var stringToken = token[2] ? ['String', token[1].slice(2, -2), token[2]] : ['String', token[1].slice(2, -2)];
            var stringNode = this.parseString(stringToken);
            return smartTrim(stringNode[1]); // Extract the string value from the StringNode
        };
        Parser.prototype.parseString = function (token) {
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
            var token = this.asToken(this.peek());
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

    var litsCommands = new Set(__spreadArray(__spreadArray(__spreadArray([], __read(normalExpressionKeys), false), __read(specialExpressionKeys), false), __read(Object.keys(reservedSymbolRecord)), false));
    // TODO: replace with get suggestions function
    var AutoCompleter = /** @class */ (function () {
        function AutoCompleter(originalProgram, originalPosition, lits, params) {
            this.originalProgram = originalProgram;
            this.originalPosition = originalPosition;
            this.prefixProgram = '';
            this.suffixProgram = '';
            this.searchString = '';
            this.suggestions = [];
            this.suggestionIndex = null;
            var partialProgram = this.originalProgram.slice(0, this.originalPosition);
            var tokenStream = lits.tokenize(partialProgram);
            var lastToken = tokenStream.tokens.at(-1);
            if (!lastToken) {
                return;
            }
            if (lastToken[0] === 'Error') {
                return;
            }
            this.searchString = lastToken[1];
            this.prefixProgram = this.originalProgram.slice(0, this.originalPosition - this.searchString.length);
            this.suffixProgram = this.originalProgram.slice(this.prefixProgram.length + this.searchString.length);
            this.originalProgram.slice(this.prefixProgram.length + this.searchString.length);
            this.suggestions = this.generateSuggestions(params);
        }
        AutoCompleter.prototype.getNextSuggestion = function () {
            return this.getAutoCompleteSuggestionResult(this.getNextSuggestionSymbol());
        };
        AutoCompleter.prototype.getPreviousSuggestion = function () {
            return this.getAutoCompleteSuggestionResult(this.getPreviousSuggestionSymbol());
        };
        AutoCompleter.prototype.getAutoCompleteSuggestionResult = function (suggestion) {
            if (suggestion === null) {
                return null;
            }
            return {
                program: this.prefixProgram + suggestion + this.suffixProgram,
                position: this.prefixProgram.length + suggestion.length,
            };
        };
        AutoCompleter.prototype.getNextSuggestionSymbol = function () {
            if (this.suggestions.length === 0) {
                return null;
            }
            if (this.suggestionIndex === null) {
                this.suggestionIndex = 0;
            }
            else {
                this.suggestionIndex += 1;
                if (this.suggestionIndex >= this.suggestions.length) {
                    this.suggestionIndex = 0;
                }
            }
            return this.suggestions[this.suggestionIndex];
        };
        AutoCompleter.prototype.getPreviousSuggestionSymbol = function () {
            if (this.suggestions.length === 0) {
                return null;
            }
            if (this.suggestionIndex === null) {
                this.suggestionIndex = this.suggestions.length - 1;
            }
            else {
                this.suggestionIndex -= 1;
                if (this.suggestionIndex < 0) {
                    this.suggestionIndex = this.suggestions.length - 1;
                }
            }
            return this.suggestions[this.suggestionIndex];
        };
        AutoCompleter.prototype.getSuggestions = function () {
            return __spreadArray([], __read(this.suggestions), false);
        };
        AutoCompleter.prototype.getSearchString = function () {
            return this.searchString;
        };
        AutoCompleter.prototype.generateSuggestions = function (params) {
            var _this = this;
            var blacklist = new Set(['0_def', '0_defn', '0_lambda']);
            var startsWithCaseSensitive = this.generateWithPredicate(params, function (suggestion) {
                return !blacklist.has(suggestion) && suggestion.startsWith(_this.searchString);
            });
            startsWithCaseSensitive.forEach(function (suggestion) { return blacklist.add(suggestion); });
            var startsWithCaseInsensitive = this.generateWithPredicate(params, function (suggestion) {
                return !blacklist.has(suggestion) && suggestion.toLowerCase().startsWith(_this.searchString.toLowerCase());
            });
            startsWithCaseInsensitive.forEach(function (suggestion) { return blacklist.add(suggestion); });
            var includesCaseSensitive = this.generateWithPredicate(params, function (suggestion) {
                return !blacklist.has(suggestion) && suggestion.includes(_this.searchString);
            });
            includesCaseSensitive.forEach(function (suggestion) { return blacklist.add(suggestion); });
            var includesCaseInsensitive = this.generateWithPredicate(params, function (suggestion) {
                return !blacklist.has(suggestion) && suggestion.includes(_this.searchString.toLowerCase());
            });
            includesCaseInsensitive.forEach(function (suggestion) { return blacklist.add(suggestion); });
            return __spreadArray(__spreadArray(__spreadArray(__spreadArray([], __read(startsWithCaseSensitive), false), __read(startsWithCaseInsensitive), false), __read(includesCaseSensitive), false), __read(includesCaseInsensitive), false);
        };
        AutoCompleter.prototype.generateWithPredicate = function (params, shouldInclude) {
            var _a, _b, _c, _d;
            var suggestions = new Set();
            litsCommands.forEach(function (suggestion) {
                if (shouldInclude(suggestion)) {
                    suggestions.add(suggestion);
                }
            });
            Object.keys((_a = params.globalContext) !== null && _a !== void 0 ? _a : {})
                .filter(shouldInclude)
                .forEach(function (suggestion) { return suggestions.add(suggestion); });
            (_b = params.contexts) === null || _b === void 0 ? void 0 : _b.forEach(function (context) {
                Object.keys(context)
                    .filter(shouldInclude)
                    .forEach(function (suggestion) { return suggestions.add(suggestion); });
            });
            Object.keys((_c = params.jsFunctions) !== null && _c !== void 0 ? _c : {})
                .filter(shouldInclude)
                .forEach(function (suggestion) { return suggestions.add(suggestion); });
            Object.keys((_d = params.values) !== null && _d !== void 0 ? _d : {})
                .filter(shouldInclude)
                .forEach(function (suggestion) { return suggestions.add(suggestion); });
            return __spreadArray([], __read(suggestions), false).sort(function (a, b) { return a.localeCompare(b); });
        };
        return AutoCompleter;
    }());

    function getNumberTheorySequenceNames(name) {
        return ["nth:".concat(name, "-seq"), "nth:".concat(name, "-nth"), "nth:".concat(name, "-take-while"), "nth:".concat(name, "?")];
    }
    function getVectorReductionNames(name) {
        return ["vec:".concat(name), "vec:moving-".concat(name), "vec:centered-moving-".concat(name), "vec:running-".concat(name)];
    }
    var api = {
        collection: [
            'filter',
            'filteri',
            'map',
            'mapi',
            'reduce',
            'reducei',
            'reduce-right',
            'reducei-right',
            'reductions',
            'reductionsi',
            'count',
            'get',
            'get-in',
            'contains?',
            'assoc',
            'assoc-in',
            '++',
            'not-empty',
            'every?',
            'not-every?',
            'any?',
            'not-any?',
            'update',
            'update-in',
        ],
        array: [
            'range',
            'repeat',
            'flatten',
            'mapcat',
            'moving-fn',
            'running-fn',
        ],
        sequence: [
            'nth',
            'push',
            'pop',
            'unshift',
            'shift',
            'slice',
            'splice',
            'position',
            'index-of',
            'last-index-of',
            'some',
            'reverse',
            'first',
            'second',
            'last',
            'rest',
            'next',
            'take',
            'take-last',
            'take-while',
            'drop',
            'drop-last',
            'drop-while',
            'sort',
            'sort-by',
            'distinct',
            'remove',
            'remove-at',
            'split-at',
            'split-with',
            'frequencies',
            'group-by',
            'partition',
            'partition-all',
            'partition-by',
            'starts-with?',
            'ends-with?',
            'interleave',
            'interpose',
        ],
        math: [
            '+',
            '-',
            '*',
            '/',
            'mod',
            'rem',
            'quot',
            'inc',
            'dec',
            'sqrt',
            'cbrt',
            '^',
            'round',
            'trunc',
            'floor',
            'ceil',
            'min',
            'max',
            'abs',
            'sign',
            'ln',
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
            'to-rad',
            'to-deg',
        ],
        functional: [
            '|>',
            'apply',
            'identity',
            'comp',
            'constantly',
            'juxt',
            'complement',
            'every-pred',
            'some-pred',
            'fnull',
        ],
        meta: [
            'doc',
            'arity',
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
            'iso-date->epoch',
            'epoch->iso-date',
            'boolean',
            'compare',
            'identical?',
            'json-parse',
            'json-stringify',
        ],
        object: [
            'dissoc',
            'keys',
            'vals',
            'entries',
            'find',
            'merge',
            'merge-with',
            'zipmap',
            'select-keys',
        ],
        predicate: [
            'boolean?',
            'null?',
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
            'negative-infinity?',
            'positive-infinity?',
            'false?',
            'true?',
            'empty?',
            'not-empty?',
            'vector?',
            'grid?',
            'matrix?',
        ],
        regularExpression: [
            'regexp',
            'match',
            'replace',
            'replace-all',
        ],
        string: [
            'string-repeat',
            'str',
            'number',
            'lower-case',
            'upper-case',
            'trim',
            'trim-left',
            'trim-right',
            'pad-left',
            'pad-right',
            'split',
            'split-lines',
            'template',
            'to-char-code',
            'from-char-code',
            'encode-base64',
            'decode-base64',
            'encode-uri-component',
            'decode-uri-component',
            'join',
            'capitalize',
            'blank?',
        ],
        bitwise: [
            '<<',
            '>>',
            '>>>',
            'bit-not',
            '&',
            'bit-and-not',
            '|',
            'xor',
            'bit-flip',
            'bit-clear',
            'bit-set',
            'bit-test',
        ],
        // TODO, remove some, add some. E.g. type guards, assert-number, assert-string, etc.
        assert: [
            'assert',
            'assert=',
            'assert!=',
            'assert-gt',
            'assert-lt',
            'assert-gte',
            'assert-lte',
            'assert-true',
            'assert-false',
            'assert-truthy',
            'assert-falsy',
            'assert-null',
            'assert-throws',
            'assert-throws-error',
            'assert-not-throws',
        ],
        grid: [
            'grid:every?',
            'grid:some?',
            'grid:every-row?',
            'grid:some-row?',
            'grid:every-col?',
            'grid:some-col?',
            'grid:row',
            'grid:col',
            'grid:shape',
            'grid:fill',
            'grid:generate',
            'grid:reshape',
            'grid:transpose',
            'grid:flip-h',
            'grid:flip-v',
            'grid:rotate',
            'grid:reverse-rows',
            'grid:reverse-cols',
            'grid:slice',
            'grid:slice-rows',
            'grid:slice-cols',
            'grid:splice-rows',
            'grid:splice-cols',
            'grid:concat-rows',
            'grid:concat-cols',
            'grid:map',
            'grid:mapi',
            'grid:reduce',
            'grid:reducei',
            'grid:push-rows',
            'grid:unshift-rows',
            'grid:pop-row',
            'grid:shift-row',
            'grid:push-cols',
            'grid:unshift-cols',
            'grid:pop-col',
            'grid:shift-col',
            'grid:from-array',
        ],
        matrix: [
            'mat:mul',
            'mat:det',
            'mat:inv',
            'mat:adj',
            'mat:cofactor',
            'mat:minor',
            'mat:trace',
            'mat:symmetric?',
            'mat:triangular?',
            'mat:upper-triangular?',
            'mat:lower-triangular?',
            'mat:diagonal?',
            'mat:square?',
            'mat:orthogonal?',
            'mat:identity?',
            'mat:invertible?',
            'mat:hilbert',
            'mat:vandermonde',
            'mat:band',
            'mat:banded?',
            'mat:rank',
            'mat:frobenius-norm',
            'mat:1-norm',
            'mat:inf-norm',
            'mat:max-norm',
        ],
        vector: __spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray([
            'vec:monotonic?',
            'vec:strictly-monotonic?',
            'vec:increasing?',
            'vec:decreasing?',
            'vec:strictly-increasing?',
            'vec:strictly-decreasing?',
            'vec:median',
            'vec:mode',
            'vec:min-index',
            'vec:max-index',
            'vec:sort-indices',
            'vec:count-values',
            'vec:linspace',
            'vec:ones',
            'vec:zeros',
            'vec:fill',
            'vec:generate',
            'vec:cumsum',
            'vec:cumprod',
            'vec:quartiles',
            'vec:percentile',
            'vec:quantile',
            'vec:histogram',
            'vec:ecdf',
            'vec:outliers?',
            'vec:outliers',
            'vec:bincount',
            'vec:winsorize',
            'vec:mse',
            'vec:mae',
            'vec:rmse',
            'vec:smape'
        ], __read(getVectorReductionNames('mean')), false), __read(getVectorReductionNames('median')), false), __read(getVectorReductionNames('variance')), false), __read(getVectorReductionNames('sample-variance')), false), __read(getVectorReductionNames('sum')), false), __read(getVectorReductionNames('prod')), false), __read(getVectorReductionNames('min')), false), __read(getVectorReductionNames('max')), false), __read(getVectorReductionNames('stdev')), false), __read(getVectorReductionNames('sample-stdev')), false), __read(getVectorReductionNames('iqr')), false), __read(getVectorReductionNames('span')), false), __read(getVectorReductionNames('geometric-mean')), false), __read(getVectorReductionNames('harmonic-mean')), false), __read(getVectorReductionNames('skewness')), false), __read(getVectorReductionNames('sample-skewness')), false), __read(getVectorReductionNames('kurtosis')), false), __read(getVectorReductionNames('sample-kurtosis')), false), __read(getVectorReductionNames('excess-kurtosis')), false), __read(getVectorReductionNames('sample-excess-kurtosis')), false), __read(getVectorReductionNames('rms')), false), __read(getVectorReductionNames('mad')), false), __read(getVectorReductionNames('medad')), false), __read(getVectorReductionNames('gini-coefficient')), false), __read(getVectorReductionNames('entropy')), false), __read(getVectorReductionNames('skewness')), false),
        linAlg: [
            'lin:reflect',
            'lin:refract',
            'lin:lerp',
            'lin:rotate2d',
            'lin:rotate3d',
            'lin:dot',
            'lin:cross',
            'lin:normalize-minmax',
            'lin:normalize-zscore',
            'lin:normalize-robust',
            'lin:normalize-l1',
            'lin:normalize-l2',
            'lin:normalize-log',
            'lin:angle',
            'lin:projection',
            'lin:orthogonal?',
            'lin:parallel?',
            'lin:collinear?',
            'lin:cosine-similarity',
            'lin:euclidean-distance',
            'lin:euclidean-norm',
            'lin:manhattan-distance',
            'lin:manhattan-norm',
            'lin:hamming-distance',
            'lin:hamming-norm',
            'lin:chebyshev-distance',
            'lin:chebyshev-norm',
            'lin:minkowski-distance',
            'lin:minkowski-norm',
            'lin:cov',
            'lin:corr',
            'lin:spearman-corr',
            'lin:pearson-corr',
            'lin:kendall-tau',
            'lin:autocorrelation',
            'lin:cross-correlation',
            'lin:rref',
            'lin:solve',
            'lin:to-polar',
            'lin:from-polar',
        ],
        numberTheory: __spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray([], __read(getNumberTheorySequenceNames('abundant')), false), __read(getNumberTheorySequenceNames('bell')), false), __read(getNumberTheorySequenceNames('catalan')), false), __read(getNumberTheorySequenceNames('composite')), false), __read(getNumberTheorySequenceNames('factorial')), false), __read(getNumberTheorySequenceNames('fibonacci')), false), __read(getNumberTheorySequenceNames('geometric')), false), __read(getNumberTheorySequenceNames('golomb')), false), __read(getNumberTheorySequenceNames('happy')), false), __read(getNumberTheorySequenceNames('look-and-say')), false), __read(getNumberTheorySequenceNames('lucas')), false), __read(getNumberTheorySequenceNames('lucky')), false), __read(getNumberTheorySequenceNames('mersenne')), false), __read(getNumberTheorySequenceNames('padovan')), false), __read(getNumberTheorySequenceNames('partition')), false), __read(getNumberTheorySequenceNames('pell')), false), __read(getNumberTheorySequenceNames('perfect')), false), __read(getNumberTheorySequenceNames('perfect-cube')), false), __read(getNumberTheorySequenceNames('perfect-power')), false), __read(getNumberTheorySequenceNames('perfect-square')), false), __read(getNumberTheorySequenceNames('polygonal')), false), __read(getNumberTheorySequenceNames('prime')), false), __read(getNumberTheorySequenceNames('recaman')), false), __read(getNumberTheorySequenceNames('sylvester')), false), __read(getNumberTheorySequenceNames('thue-morse')), false), __read(getNumberTheorySequenceNames('tribonacci')), false), [
            'nth:collatz-seq',
            'nth:juggler-seq',
            'nth:bernoulli-seq',
            'nth:bernoulli-take-while',
            'nth:bernoulli-nth',
            'nth:combinations',
            'nth:count-combinations',
            'nth:derangements',
            'nth:count-derangements',
            'nth:divisors',
            'nth:count-divisors',
            'nth:proper-divisors',
            'nth:count-proper-divisors',
            'nth:prime-factors',
            'nth:count-prime-factors',
            'nth:distinct-prime-factors',
            'nth:count-distinct-prime-factors',
            'nth:factorial',
            'nth:partitions',
            'nth:count-partitions',
            'nth:permutations',
            'nth:count-permutations',
            'nth:power-set',
            'nth:count-power-set',
            'nth:coprime?',
            'nth:divisible-by?',
            'nth:gcd',
            'nth:lcm',
            'nth:multinomial',
            'nth:amicable?',
            'nth:euler-totient',
            'nth:mobius',
            'nth:mertens',
            'nth:sigma',
            'nth:carmichael-lambda',
            'nth:cartesian-product',
            'nth:perfect-power',
            'nth:mod-exp',
            'nth:mod-inv',
            'nth:extended-gcd',
            'nth:chinese-remainder',
            'nth:stirling-first',
            'nth:stirling-second',
        ], false),
        random: [
            '!:random',
            '!:random-int',
            '!:random-int-inclusive',
            '!:random-float',
            '!:random-boolean',
            '!:random-item',
            '!:random-sample',
            '!:random-sample-unique',
            '!:shuffle',
            '!:random-normal',
            '!:random-exponential',
            '!:random-binomial',
            '!:random-poisson',
            '!:random-gamma',
            '!:random-pareto',
            '!:uuid',
            '!:random-char',
            '!:random-string',
            '!:random-id',
            '!:random-color',
        ],
        shorthand: [
            '-short-regexp',
            '-short-fn',
        ],
        datatype: [
            '-type-number',
            '-type-string',
            '-type-object',
            '-type-array',
            '-type-vector',
            '-type-matrix',
            '-type-grid',
            '-type-boolean',
            '-type-function',
            '-type-integer',
            '-type-any',
            '-type-null',
            '-type-collection',
            '-type-sequence',
            '-type-regexp',
            '-type-never',
        ],
    };
    // Core API function names (always available)
    var coreApiFunctionNames = __spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray([], __read(api.collection), false), __read(api.array), false), __read(api.sequence), false), __read(api.math), false), __read(api.functional), false), __read(api.meta), false), __read(api.misc), false), __read(api.object), false), __read(api.predicate), false), __read(api.regularExpression), false), __read(api.string), false), __read(api.bitwise), false), __read(api.assert), false);
    // Namespace API function names (require import())
    var namespaceApiFunctionNames = __spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray(__spreadArray([], __read(api.matrix), false), __read(api.vector), false), __read(api.linAlg), false), __read(api.grid), false), __read(api.numberTheory), false), __read(api.random), false);
    // All API function names
    var apiFunctionNames = __spreadArray(__spreadArray([], __read(coreApiFunctionNames), false), __read(namespaceApiFunctionNames), false);
    // Core API names (core functions + shorthand + datatype)
    __spreadArray(__spreadArray(__spreadArray([], __read(coreApiFunctionNames), false), __read(api.shorthand), false), __read(api.datatype), false);
    __spreadArray(__spreadArray(__spreadArray([], __read(apiFunctionNames), false), __read(api.shorthand), false), __read(api.datatype), false);
    function getOperatorArgs(a, b) {
        return { a: { type: a }, b: { type: b } };
    }

    var arrayReference = {
        'range': {
            title: 'range',
            category: 'Array',
            returns: {
                type: 'number',
                array: true,
            },
            args: __assign(__assign({}, getOperatorArgs('number', 'number')), { step: {
                    type: 'number',
                } }),
            variants: [
                { argumentNames: ['b'] },
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'step'] },
            ],
            description: "$range creates an array with a range of numbers from $a to $b (exclusive), by $step.\n\n$a defaults to 0.  \n$step defaults to 1.",
            examples: [
                'range(4)',
                'range(1, 4)',
                '1 range 10',
                'range(0.4, 4.9)',
                "\nrange(\n  0.25, // start value\n  1,    // end value (exclusive)\n  0.25, // step value\n)",
            ],
        },
        'repeat': {
            title: 'repeat',
            category: 'Array',
            returns: {
                type: 'any',
                array: true,
            },
            args: __assign({}, getOperatorArgs('any', 'integer')),
            variants: [{
                    argumentNames: ['a', 'b'],
                }],
            description: 'Returns an array with $a repeated $b times.',
            examples: [
                'repeat(10, 3)',
                'repeat(10, 0)',
                '"Albert" repeat 5',
            ],
        },
        'flatten': {
            title: 'flatten',
            category: 'Array',
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
                'flatten([1, 2, [3, 4], 5])',
                "\nlet foo = \"bar\";\nflatten([\n  1,\n  \" 2 A \",\n  [foo, [4, [\"ABC\"]]],\n  6,\n])",
            ],
            noOperatorDocumentation: true,
        },
        'mapcat': {
            title: 'mapcat',
            category: 'Array',
            returns: {
                type: 'collection',
            },
            args: __assign(__assign({}, getOperatorArgs('collection', 'function')), { colls: {
                    type: 'collection',
                    array: true,
                }, fun: {
                    type: 'function',
                } }),
            variants: [{
                    argumentNames: ['colls', 'fun'],
                }],
            description: 'Returns the result of applying concat to the result of applying map to $fun and $colls.',
            examples: [
                '[[3, 2, 1, 0], [6, 5, 4], [9, 8, 7]] mapcat reverse',
                'mapcat([[3, 2, 1, 0], [6, 5, 4], [9, 8, 7]], reverse)',
                '[[3, 2, 1, 0,], [6, 5, 4,], [9, 8, 7]] mapcat reverse',
                "\nlet foo = (n) -> {\n  [n - 1, n, n + 1]\n};\n[1, 2, 3] mapcat foo",
                "\nmapcat(\n  [[1, 2], [2, 2], [2, 3]],\n  -> $ remove even?\n)",
            ],
        },
        'moving-fn': {
            title: 'moving-fn',
            category: 'Array',
            returns: {
                type: 'array',
            },
            args: {
                arr: {
                    type: 'array',
                },
                windowSize: {
                    type: 'number',
                    description: 'The size of the moving window.',
                },
                fn: {
                    type: 'function',
                },
            },
            variants: [{
                    argumentNames: ['arr', 'windowSize', 'fn'],
                }],
            description: 'Returns the result of applying $fn to each moving window of size $windowSize in $arr.',
            examples: [
                'moving-fn([1, 2, 3], 2, vec:sum)',
                'moving-fn([1, 2, 3], 1, vec:sum)',
                'moving-fn([1, 2, 3], 3, vec:sum)',
            ],
        },
        'running-fn': {
            title: 'running-fn',
            category: 'Array',
            returns: {
                type: 'array',
            },
            args: {
                a: {
                    type: 'array',
                },
                b: {
                    type: 'function',
                },
            },
            variants: [{
                    argumentNames: ['a', 'b'],
                }],
            description: 'Returns the result of applying $b to each element of $a.',
            examples: [
                'running-fn([1, 2, 3], vec:sum)',
                'running-fn([1, 2, 3], vec:max)',
                'running-fn([1, 2, 3], vec:min)',
            ],
        },
    };

    var assertReference = {
        'assert': {
            title: 'assert',
            category: 'Assert',
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
            examples: ['try assert(0, "Expected a positive value") catch (e) e.message end'],
            noOperatorDocumentation: true,
        },
        'assert!=': {
            title: 'assert!=',
            category: 'Assert',
            returns: {
                type: 'null',
            },
            args: __assign(__assign({}, getOperatorArgs('any', 'any')), { message: {
                    type: 'string',
                } }),
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'message'] },
            ],
            description: 'If $a is the same as $b it throws `AssertionError`.',
            examples: [
                'try assert!=(0, 0, "Expected different values") catch (e) e.message end',
                'try assert!=(0, 0) catch (e) e.message end',
                'try 0 assert!= 0 catch (e) e.message end',
                'try assert!=(0, 1) catch (e) e.message end',
            ],
            noOperatorDocumentation: true,
        },
        'assert=': {
            title: 'assert=',
            category: 'Assert',
            returns: {
                type: 'null',
            },
            args: __assign(__assign({}, getOperatorArgs('any', 'any')), { message: {
                    type: 'string',
                } }),
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'message'] },
            ],
            description: 'If $a is not structural equal to $b it throws `AssertionError`.',
            examples: [
                'try assert=({ "a": 1 }, { "a": 2 }, "Expected equal values") catch (e) e.message end',
                'try assert=({ "a": 1 }, { "a": 2 }) catch (e) e.message end',
                'try assert=({ "a": 1 }, { "a": 1 }) catch (e) e.message end',
            ],
            noOperatorDocumentation: true,
        },
        'assert-gt': {
            title: 'assert-gt',
            category: 'Assert',
            returns: {
                type: 'null',
            },
            args: __assign(__assign({}, getOperatorArgs('any', 'any')), { message: {
                    type: 'string',
                } }),
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'message'] },
            ],
            description: 'If $a is not greater than $b it throws `AssertionError`.',
            examples: [
                'try assert-gt(0, 1, "Expected greater value") catch (e) e.message end',
                'try assert-gt(0, 0) catch (e) e.message end',
                'try assert-gt(1, 0) catch (e) e.message end',
            ],
            noOperatorDocumentation: true,
        },
        'assert-lt': {
            title: 'assert-lt',
            category: 'Assert',
            returns: {
                type: 'null',
            },
            args: __assign(__assign({}, getOperatorArgs('any', 'any')), { message: {
                    type: 'string',
                } }),
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'message'] },
            ],
            description: 'If $a is not less than $b it throws `AssertionError`.',
            examples: [
                'try assert-lte(1, 0, "Expected smaller value value") catch (e) e.message end',
                'try assert-lte(1, 1) catch (e) e.message end',
                'try assert-lte(0, 1) catch (e) e.message end',
            ],
            noOperatorDocumentation: true,
        },
        'assert-gte': {
            title: 'assert-gte',
            category: 'Assert',
            returns: {
                type: 'null',
            },
            args: __assign(__assign({}, getOperatorArgs('any', 'any')), { message: {
                    type: 'string',
                } }),
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'message'] },
            ],
            description: 'If $a is less than $b it throws `AssertionError`.',
            examples: [
                'try assert-gte(0, 1, "Expected greater value") catch (e) e.message end',
                'try assert-gte(0, 1) catch (e) e.message end',
                'try assert-gte(1, 1) catch (e) e.message end',
            ],
            noOperatorDocumentation: true,
        },
        'assert-lte': {
            title: 'assert-lte',
            category: 'Assert',
            returns: {
                type: 'null',
            },
            args: __assign(__assign({}, getOperatorArgs('any', 'any')), { message: {
                    type: 'string',
                } }),
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'message'] },
            ],
            description: 'If $a is grater than $b it throws `AssertionError`.',
            examples: [
                'try assert-lte(1, 0, "Expected smaller value value") catch (e) e.message end',
                'try assert-lte(1, 0) catch (e) e.message end',
                'try assert-lte(1, 1) catch (e) e.message end',
            ],
            noOperatorDocumentation: true,
        },
        'assert-true': {
            title: 'assert-true',
            category: 'Assert',
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
                'try assert-true(false, "Expected true") catch (e) e.message end',
                'try assert-true(false) catch (e) e.message end',
                'try assert-true(true) catch (e) e.message end',
            ],
            noOperatorDocumentation: true,
        },
        'assert-false': {
            title: 'assert-false',
            category: 'Assert',
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
                'try assert-false(true, "Expected false") catch (e) e.message end',
                'try assert-false(true) catch (e) e.message end',
                'try assert-false(false) catch (e) e.message end',
            ],
            noOperatorDocumentation: true,
        },
        'assert-truthy': {
            title: 'assert-truthy',
            category: 'Assert',
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
                'try assert-truthy(false, "Expected truthy") catch (e) e.message end',
                'try assert-truthy(false) catch (e) e.message end',
                'try assert-truthy(0) catch (e) e.message end',
                'try assert-truthy(null) catch (e) e.message end',
                'try assert-truthy("") catch (e) e.message end',
                'try assert-truthy(true) catch (e) e.message end',
                'try assert-truthy(1) catch (e) e.message end',
                'try assert-truthy("x") catch (e) e.message end',
                'try assert-truthy([]) catch (e) e.message end',
                'try assert-truthy(nd) catch (e) e.message end',
            ],
            noOperatorDocumentation: true,
        },
        'assert-falsy': {
            title: 'assert-falsy',
            category: 'Assert',
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
                'try assert-falsy(true, "Expected falsy") catch (e) e.message end',
                'try assert-falsy("x") catch (e) e.message end',
                'try assert-falsy([]) catch (e) e.message end',
                'try assert-falsy(nd) catch (e) e.message end',
                'try assert-falsy(1) catch (e) e.message end',
                'try assert-falsy(false) catch (e) e.message end',
                'try assert-falsy(0) catch (e) e.message end',
                'try assert-falsy(null) catch (e) e.message end',
                'try assert-falsy("") catch (e) e.message end',
            ],
            noOperatorDocumentation: true,
        },
        'assert-null': {
            title: 'assert-null',
            category: 'Assert',
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
            description: 'If $value is not `null` it throws `AssertionError`.',
            examples: [
                'try assert-null(null) catch (e) e.message end',
                'try assert-null(true, "Expected null") catch (e) e.message end',
                'try assert-null("x") catch (e) e.message end',
                'try assert-null([]) catch (e) e.message end',
                'try assert-null(nd) catch (e) e.message end',
                'try assert-null(1) catch (e) e.message end',
                'try assert-null(false) catch (e) e.message end',
                'try assert-null(0) catch (e) e.message end',
                'try assert-null("") catch (e) e.message end',
            ],
            noOperatorDocumentation: true,
        },
        'assert-throws': {
            title: 'assert-throws',
            category: 'Assert',
            returns: {
                type: 'null',
            },
            args: {
                fun: {
                    type: 'function',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['fun'] },
                { argumentNames: ['fun', 'message'] },
            ],
            description: 'If $fun does not throw, it throws `AssertionError`.',
            examples: [
                'assert-throws(-> throw("Error"))',
                'try assert-throws(-> identity("Error")) catch (e) e.message end',
            ],
            noOperatorDocumentation: true,
        },
        'assert-throws-error': {
            title: 'assert-throws-error',
            category: 'Assert',
            returns: {
                type: 'null',
            },
            args: {
                'fun': {
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
            description: 'If $fun does not throw $error-message, it throws `AssertionError`.',
            examples: [
                'try assert-throws-error(-> throw("Error"), "Error") catch (e) e.message end',
                'try assert-throws-error(-> identity("Error"), "Error") catch (e) e.message end',
            ],
            noOperatorDocumentation: true,
        },
        'assert-not-throws': {
            title: 'assert-not-throws',
            category: 'Assert',
            returns: {
                type: 'null',
            },
            args: {
                fun: {
                    type: 'function',
                },
                message: {
                    type: 'string',
                },
            },
            variants: [
                { argumentNames: ['fun'] },
                { argumentNames: ['fun', 'message'] },
            ],
            description: 'If $fun throws, it throws `AssertionError`.',
            examples: [
                'try assert-not-throws(-> identity("Error")) catch (e) e.message end',
                'try assert-not-throws(-> throw("Error")) catch (e) e.message end',
            ],
            noOperatorDocumentation: true,
        },
    };

    var bitwiseReference = {
        '<<': {
            title: '<<',
            category: 'Bitwise',
            returns: {
                type: 'integer',
            },
            args: __assign({}, getOperatorArgs('integer', 'integer')),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Shifts $a arithmetically left by $b bit positions.',
            examples: [
                '1 << 10',
                '<<(1, 10)',
                '<<(-4, 2)',
            ],
        },
        '>>': {
            title: '>>',
            category: 'Bitwise',
            returns: {
                type: 'integer',
            },
            args: __assign({}, getOperatorArgs('integer', 'integer')),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Shifts $a arithmetically right by $b bit positions.',
            examples: [
                '2048 >> 10',
                '>>(2048, 10)',
                '>>>(-16, 2)',
                '>>(4, 10)',
            ],
        },
        '>>>': {
            title: '>>>',
            category: 'Bitwise',
            returns: {
                type: 'integer',
            },
            args: __assign({}, getOperatorArgs('integer', 'integer')),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Shifts $a arithmetically right by $b bit positions without sign extension.',
            examples: [
                '-16 >>> 2',
                '>>>(2048, 10)',
                '>>>(-16, 2)',
                '>>>(4, 10)',
                '>>>(-1, 10)',
            ],
        },
        'bit-not': {
            title: 'bit-not',
            category: 'Bitwise',
            returns: {
                type: 'integer',
            },
            args: {
                a: {
                    type: 'integer',
                },
            },
            variants: [
                { argumentNames: ['a'] },
            ],
            description: 'Returns bitwise `not` of $a.',
            examples: [
                'bit-not(0)',
                'bit-not(255)',
            ],
        },
        '&': {
            title: '&',
            category: 'Bitwise',
            returns: {
                type: 'integer',
            },
            args: __assign(__assign({}, getOperatorArgs('integer', 'integer')), { c: {
                    type: 'integer',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'c'] },
            ],
            description: 'Returns bitwise `and` of all arguments.',
            examples: [
                '0b0011 & 0b0110',
                '&(0b0011, 0b0110)',
                '&(0b0011, 0b0110, 0b1001)',
            ],
        },
        'bit-and-not': {
            title: 'bit-and-not',
            category: 'Bitwise',
            returns: {
                type: 'integer',
            },
            args: __assign(__assign({}, getOperatorArgs('integer', 'integer')), { c: {
                    type: 'integer',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'c'] },
            ],
            description: 'Returns bitwise `and` with complement.',
            examples: [
                '0b0011 bit-and-not 0b0110',
                'bit-and-not(0b0011, 0b0110)',
                'bit-and-not(0b0011, 0b0110, 0b1001)',
            ],
        },
        '|': {
            title: '|',
            category: 'Bitwise',
            returns: {
                type: 'integer',
            },
            args: __assign(__assign({}, getOperatorArgs('integer', 'integer')), { c: {
                    type: 'integer',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'c'] },
            ],
            description: 'Returns bitwise `or` of all arguments.',
            examples: [
                '0b0011 | 0b0110',
                '|(0b0011, 0b0110)',
                '|(0b1000, 0b0100, 0b0010)',
            ],
        },
        'xor': {
            title: 'xor',
            category: 'Bitwise',
            returns: {
                type: 'integer',
            },
            args: __assign(__assign({}, getOperatorArgs('integer', 'integer')), { c: {
                    type: 'integer',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'c'] },
            ],
            description: 'Returns bitwise `xor` of all arguments.',
            examples: [
                '0b0011 xor 0b0110',
                'xor(0b0011, 0b0110)',
                'xor(0b11110000, 0b00111100, 0b10101010)',
            ],
        },
        'bit-flip': {
            title: 'bit-flip',
            category: 'Bitwise',
            returns: {
                type: 'integer',
            },
            args: __assign({}, getOperatorArgs('integer', 'integer')),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Flips bit number $b.',
            examples: [
                '0b0011 bit-flip 1',
                'bit-flip(0b0011, 1)',
                'bit-flip(0b1100, 1)',
            ],
        },
        'bit-clear': {
            title: 'bit-clear',
            category: 'Bitwise',
            returns: {
                type: 'integer',
            },
            args: __assign({}, getOperatorArgs('integer', 'integer')),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Clears bit number $b.',
            examples: [
                '0b0011 bit-clear 1',
                'bit-clear(0b0011, 1)',
                'bit-clear(0b1100, 1)',
            ],
        },
        'bit-set': {
            title: 'bit-set',
            category: 'Bitwise',
            returns: {
                type: 'integer',
            },
            args: __assign({}, getOperatorArgs('integer', 'integer')),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Sets bit number $b.',
            examples: [
                '0b0010 bit-set 1',
                'bit-set(0b0011, 1)',
                'bit-set(0b1100, 1)',
            ],
        },
        'bit-test': {
            title: 'bit-test',
            category: 'Bitwise',
            returns: {
                type: 'boolean',
            },
            args: __assign({}, getOperatorArgs('integer', 'integer')),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Checks if bit number $b is set.',
            examples: [
                '0b0011 bit-test 1',
                'bit-test(0b0011, 1)',
                'bit-test(0b1100, 1)',
            ],
        },
    };

    var collectionReference = {
        'filter': {
            title: 'filter',
            category: 'Collection',
            returns: {
                type: 'collection',
            },
            args: __assign(__assign({}, getOperatorArgs('collection', 'function')), { coll: {
                    type: 'collection',
                }, fun: {
                    type: 'function',
                } }),
            variants: [
                { argumentNames: ['coll', 'fun'] },
            ],
            description: 'Creates a new collection with all elements that pass the test implemented by $fun.',
            examples: [
                "\nfilter(\n  [\"Albert\", \"Mojir\", 160, [1, 2]],\n  string?\n)",
                "\nfilter(\n  [5, 10, 15, 20],\n  -> $ > 10\n)",
                "\nfilter(\n  { a: 1, b: 2 },\n  odd?\n)",
            ],
        },
        'filteri': {
            title: 'filteri',
            category: 'Collection',
            returns: {
                type: 'collection',
            },
            args: {
                a: {
                    type: 'collection',
                },
                b: {
                    type: 'function',
                    description: 'The function to call for each element in the collection. The function should take two arguments: the element itself and the index.',
                },
            },
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Creates a new collection with all elements that pass the test implemented by $b. The function is called for each element in the collection, and it should take two arguments: the element itself and the index.',
            examples: [
                'filteri([1, 2, 3], (x, i) -> i % 2 == 0)',
                'filteri([1, 2, 3], (x, i) -> x % 2 == 0)',
                'filteri([1, 2, 3], (x, i) -> x + i > 3)',
            ],
        },
        'map': {
            title: 'map',
            category: 'Collection',
            returns: {
                type: 'collection',
            },
            args: __assign(__assign({}, getOperatorArgs('collection', 'function')), { colls: {
                    type: 'collection',
                    rest: true,
                    description: 'At least one.',
                }, fun: {
                    type: 'function',
                } }),
            variants: [
                { argumentNames: ['colls', 'fun'] },
            ],
            description: 'Creates a new collection populated with the results of calling $fun on every element in $colls.',
            examples: [
                '[1, 2, 3] map -',
                '[1, 2, 3] map -> -($)',
                'map(["Albert", "Mojir", 42], str)',
                'map([1, 2, 3], inc)',
                'map([1, 2, 3], [1, 10, 100], *)',
                'map({ a: 1, b: 2 }, inc)',
                'map({ a: 1, b: 2 }, { a: 10, b: 20 }, +)',
            ],
        },
        'mapi': {
            title: 'mapi',
            category: 'Collection',
            returns: {
                type: 'collection',
            },
            args: {
                a: {
                    type: 'collection',
                },
                b: {
                    type: 'function',
                    description: 'The function to call for each element in the collection. The function should take two arguments: the element itself and the index.',
                },
            },
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Creates a new collection populated with the results of calling $b on every element in $a. The function is called for each element in the collection, and it should take two arguments: the element itself and the index.',
            examples: [
                'mapi([1, 2, 3], (x, i) -> x + i)',
                'mapi([1, 2, 3], (x, i) -> x * i)',
                'mapi([1, 2, 3], (x, i) -> x - i)',
                'mapi([1, 2, 3], (x, i) -> x / i)',
                'mapi([1, 2, 3], (x, i) -> x % inc(i))',
            ],
        },
        'reduce': {
            title: 'reduce',
            category: 'Collection',
            returns: {
                type: 'any',
            },
            args: {
                fun: {
                    type: 'function',
                },
                coll: {
                    type: 'collection',
                },
                initial: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['coll', 'fun', 'initial'] },
            ],
            description: 'Runs $fun function on each element of the $coll, passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $coll is a single value.',
            examples: [
                'reduce([1, 2, 3], +, 0)',
                'reduce([], +, 0)',
                'reduce({ a: 1, b: 2 }, +, 0)',
                "\nreduce(\n  [1, 2, 3, 4, 5, 6, 7, 8, 9],\n  (result, value) -> result + (even?(value) ? value : 0),\n  0)",
            ],
        },
        'reduce-right': {
            title: 'reduce-right',
            category: 'Collection',
            returns: {
                type: 'any',
            },
            args: {
                fun: {
                    type: 'function',
                },
                coll: {
                    type: 'collection',
                },
                initial: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['coll', 'fun', 'initial'] },
            ],
            description: 'Runs $fun function on each element of the $coll (starting from the last item), passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $coll is a single value.',
            examples: [
                'reduce-right(["A", "B", "C"], str, "")',
                'reduce-right({ a: 1, b: 2 }, +, 0)',
            ],
        },
        'reducei-right': {
            title: 'reducei-right',
            category: 'Collection',
            returns: {
                type: 'any',
            },
            args: {
                coll: {
                    type: 'collection',
                },
                fun: {
                    type: 'function',
                    description: 'The function to call for each element in the collection. The function should take three arguments: the accumulator, the element itself, and the index.',
                },
                initial: {
                    type: 'any',
                    description: 'The initial value to use as the accumulator.',
                },
            },
            variants: [
                { argumentNames: ['coll', 'fun', 'initial'] },
            ],
            description: 'Runs $fun function on each element of the $coll (starting from the last item), passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $coll is a single value. The function is called for each element in the collection, and it should take three arguments: the accumulator, the element itself, and the index.',
            examples: [
                'reducei-right([1, 2, 3], (acc, x, i) -> acc + x + i, 0)',
                'reducei-right("Albert", (acc, x, i) -> acc ++ x ++ i, "")',
                'reducei-right({ a: 1, b: 2 }, -> $1 ++ $3, "")',
            ],
        },
        'reducei': {
            title: 'reducei',
            category: 'Collection',
            returns: {
                type: 'any',
            },
            args: {
                coll: {
                    type: 'collection',
                },
                fun: {
                    type: 'function',
                    description: 'The function to call for each element in the collection. The function should take three arguments: the accumulator, the element itself, and the index.',
                },
                initial: {
                    type: 'any',
                    description: 'The initial value to use as the accumulator.',
                },
            },
            variants: [
                { argumentNames: ['coll', 'fun', 'initial'] },
            ],
            description: 'Runs $fun function on each element of the $coll, passing in the return value from the calculation on the preceding element. The final result of running the reducer across all elements of the $coll is a single value. The function is called for each element in the collection, and it should take three arguments: the accumulator, the element itself, and the index.',
            examples: [
                'reducei([1, 2, 3], (acc, x, i) -> acc + x + i, 0)',
                'reducei("Albert", (acc, x, i) -> acc ++ x ++ i, "")',
                'reducei({ a: 1, b: 2 }, -> $1 ++ $3, "")',
            ],
        },
        'reductions': {
            title: 'reductions',
            category: 'Collection',
            returns: {
                type: 'any',
                array: true,
            },
            args: {
                fun: {
                    type: 'function',
                },
                coll: {
                    type: 'collection',
                },
                initial: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['coll', 'fun', 'initial'] },
            ],
            description: 'Returns an array of the intermediate values of the reduction (see `reduce`) of $coll by $fun.',
            examples: [
                'reductions([1, 2, 3], +, 0)',
                'reductions([1, 2, 3], +, 10)',
                'reductions([], +, 0)',
                'reductions({ a: 1, b: 2 }, +, 0)',
                "\nreductions(\n  [1, 2, 3, 4, 5, 6, 7, 8, 9],\n  (result, value) -> result + (even?(value) ? value : 0),\n  0\n)",
            ],
        },
        'reductionsi': {
            title: 'reductionsi',
            category: 'Collection',
            returns: {
                type: 'any',
                array: true,
            },
            args: {
                coll: {
                    type: 'collection',
                },
                fun: {
                    type: 'function',
                    description: 'The function to call for each element in the collection. The function should take three arguments: the accumulator, the element itself, and the index.',
                },
                initial: {
                    type: 'any',
                    description: 'The initial value to use as the accumulator.',
                },
            },
            variants: [
                { argumentNames: ['coll', 'fun', 'initial'] },
            ],
            description: 'Returns an array of the intermediate values of the reduction (see `reduce`) of $coll by $fun. The function is called for each element in the collection, and it should take three arguments: the accumulator, the element itself, and the index.',
            examples: [
                'reductionsi([1, 2, 3], (acc, x, i) -> acc + x + i, 0)',
                'reductionsi("Albert", (acc, x, i) -> acc ++ x ++ i, "")',
                'reductionsi({ a: 1, b: 2 }, -> $1 ++ $3, "")',
            ],
        },
        'count': {
            title: 'count',
            category: 'Collection',
            returns: {
                type: 'number',
            },
            args: {
                coll: {
                    type: ['collection', 'null'],
                },
            },
            variants: [
                { argumentNames: ['coll'] },
            ],
            description: 'Returns number of elements in $coll.',
            examples: [
                'count([1, 2, 3])',
                'count([])',
                'count({ a: 1 })',
                'count("")',
                'count("Albert")',
                'count(null)',
            ],
        },
        'get': {
            title: 'get',
            category: 'Collection',
            returns: {
                type: 'any',
            },
            args: __assign(__assign({}, getOperatorArgs('collection', ['string', 'integer'])), { 'not-found': {
                    type: 'any',
                    description: 'Default value to return if $b is not found.',
                } }),
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'not-found'] },
            ],
            description: 'Returns value in $a mapped at $b.',
            examples: [
                '[1, 2, 3] get 1',
                '{ a: 1 } get "a"',
                '"Albert" get "3"',
                "\nget(\n  [1, 2, 3],\n  1, // Optional comma after last argument\n)",
                "\nget(\n  [],\n  1\n)",
                "\nget(\n  [],\n  1,\n  \"default\"\n)",
                "\nget(\n  { a: 1 },\n  \"a\"\n)",
                "\nget(\n  { a: 1 },\n  \"b\"\n)",
                "\nget(\n  { a: 1 },\n  \"b\",\n  \"default\"\n)",
                "\nget(\n  null,\n  \"a\"\n)",
                "\nget(\n  null,\n  \"b\",\n  \"default\"\n)",
            ],
        },
        'get-in': {
            title: 'get-in',
            category: 'Collection',
            returns: {
                type: 'any',
            },
            args: __assign(__assign({}, getOperatorArgs('collection', 'array')), { 'not-found': {
                    type: 'any',
                } }),
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'not-found'] },
            ],
            description: 'Returns the value in a nested collection, where $b is an array of keys. Returns $not-found if the key is not present. If $not-found is not set, `null` is returned.',
            examples: [
                "\nget-in(\n  [[1, 2, 3], [4, { a: \"Kalle\" }, 6]],\n  [1, 1, \"a\", 0]\n)",
                "\nget-in(\n  [[1, 2, 3], [4, { a: \"Kalle\" }, 6]],\n  [1, 1, \"b\", 0]\n)",
                "\nget-in(\n  [[1, 2, 3], [4, { a: \"Kalle\" }, 6]],\n  [1, 1, \"b\", 0],\n  \"Lisa\"\n)",
            ],
        },
        'contains?': {
            title: 'contains?',
            category: 'Collection',
            returns: {
                type: 'boolean',
            },
            args: __assign({}, getOperatorArgs(['collection', 'null'], ['string', 'integer'])),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Returns `true` if $a contains $b, otherwise returns `false`. For strings, it checks if substring is included.',
            examples: [
                '[1, 2, 3] contains? 1',
                'null contains? 1',
                '{ a: 1, b: 2 } contains? "a"',
                "\ncontains?(\n  [],\n  1\n)",
                "\ncontains?(\n  [1],\n  1\n)",
                "\ncontains?(\n  [1, 2, 3],\n  1\n)",
                "\ncontains?(\n  {},\n  \"a\"\n)",
                "\ncontains?(\n  { a: 1, b: 2 },\n  \"a\"\n)",
            ],
        },
        'assoc': {
            title: 'assoc',
            category: 'Collection',
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
                "\nassoc(\n  [1, 2, 3],\n  1,\n  \"Two\"\n)",
                "\nassoc(\n  [1, 2, 3],\n  3,\n  \"Four\"\n)",
                "\nassoc(\n  { a: 1, b: 2 },\n  \"a\",\n  \"One\")",
                "\nassoc(\n  { a: 1, b: 2 },\n  \"c\",\n  \"Three\")",
                "\nassoc(\n  \"Albert\",\n  6,\n  \"a\")",
            ],
        },
        'assoc-in': {
            title: 'assoc-in',
            category: 'Collection',
            returns: {
                type: 'collection',
            },
            args: {
                coll: {
                    type: 'collection',
                },
                ks: {
                    type: ['number', 'string'],
                    array: true,
                },
                value: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['coll', 'ks', 'value'] },
            ],
            description: "\nAssociates a value in the nested collection $coll, where $ks is an array of keys and $value is the new value.\n\nIf any levels do not exist, objects will be created - and the corresponding keys must be of type string.",
            examples: [
                "\nassoc-in(\n  {},\n  [\"a\", \"b\", \"c\"],\n  \"Albert\"\n)",
                "\nassoc-in(\n  [1, 2, [1, 2, 3]],\n  [2, 1],\n  \"Albert\"\n)",
                "\nassoc-in(\n  [1, 2, { name: \"albert\" }],\n  [2, \"name\", 0],\n  \"A\"\n)",
            ],
        },
        '++': {
            title: '++',
            category: 'Collection',
            returns: {
                type: 'collection',
            },
            args: __assign(__assign({}, getOperatorArgs('collection', 'collection')), { colls: {
                    type: 'collection',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['a'] },
                { argumentNames: ['a', 'colls'] },
            ],
            description: 'Concatenates collections into one collection.',
            examples: [
                '"Albert" ++ " " ++ "Mojir"',
                '"Albert" ++ "Mojir"',
                '"Hi " concat "Albert"',
                '[1, 2] concat [3, 4]',
                '++("Albert", "-", "Mojir")',
                '++("Albert")',
                'concat("A", "l", "b", "e", "r", "t")',
                'concat([1, 2], [3, 4])',
                'concat([], [3, 4])',
                'concat([1, 2], [])',
                'concat([1, 2], [3, 4], [5, 6])',
                'concat([])',
                'concat({ a: 1, b: 2 }, { b: 1, c: 2 })',
                'concat({}, { a: 1 })',
            ],
            aliases: ['concat'],
        },
        'not-empty': {
            title: 'not-empty',
            category: 'Collection',
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
            description: 'Returns `null` if $coll is empty or `null`, otherwise $coll.',
            examples: [
                'not-empty([])',
                'not-empty([1, 2, 3])',
                'not-empty({})',
                'not-empty({ a: 2 })',
                'not-empty("")',
                'not-empty("Albert")',
                'not-empty(null)',
            ],
        },
        'every?': {
            title: 'every?',
            category: 'Collection',
            returns: {
                type: 'boolean',
            },
            args: __assign({}, getOperatorArgs('collection', 'function')),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Returns `true` if all entries in $a pass the test implemented by $b, otherwise returns `false`.',
            examples: [
                '[1, 2, 3] every? number?',
                '[1, 2, 3] every? even?',
                "\nevery?(\n  [\"Albert\", \"Mojir\", 160, [1, 2]],\n  string?,\n)",
                "\nevery?(\n  [50, 100, 150, 200],\n  -> $ > 10,\n)",
                "\nevery?(\n  [],\n  number?\n)",
                "\nevery?(\n  \"\",\n  number?\n)",
                "\nevery?(\n  {},\n  number?\n)",
                "\nevery?(\n  { a: 2, b: 4},\n  -> even?(second($))\n)",
                "\nevery?(\n  { a: 2, b: 3 },\n  -> even?(second($))\n)",
            ],
        },
        'not-every?': {
            title: 'not-every?',
            category: 'Collection',
            returns: {
                type: 'boolean',
            },
            args: __assign({}, getOperatorArgs('collection', 'function')),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Returns `true` if at least one element in $a does not pass the test implemented by $b, otherwise returns `false`.',
            examples: [
                "\nnot-every?(\n  [\"Albert\", \"Mojir\", 160, [1, 2]],\n  string?\n)",
                "\nnot-every?(\n  [50, 100, 150, 200],\n  x -> x > 10\n)",
                "\nnot-every?(\n  [],\n  number?\n)",
                "\nnot-every?(\n  \"\",\n  number?\n)",
                "\nnot-every?(\n  {},\n  number?\n)",
                "\nnot-every?(\n  { a: 2, b: 4 },\n  -> even?(second($))\n)",
                "\nnot-every?(\n  { a: 2, b: 3 },\n  -> even?(second($))\n)",
            ],
        },
        'any?': {
            title: 'any?',
            category: 'Collection',
            returns: {
                type: 'boolean',
            },
            args: __assign({}, getOperatorArgs('collection', 'function')),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Returns `true` if any element in $a pass the test implemented by $b, otherwise returns `false`.',
            examples: [
                "\nany?(\n  [\"Albert\", \"Mojir\", 160, [1, 2]],\n  string?\n)",
                "\nany?(\n  [50, 100, 150, 200],\n  x -> x > 10\n)",
                "\nany?(\n  [],\n  number?\n)",
                "\nany?(\n  \"\",\n  number?\n)",
                "\nany?(\n  {},\n  number?\n)",
                "\nany?(\n  { a: 2, b: 3 },\n  -> even?(second($))\n)",
                "\nany?(\n  { a: 1, b: 3 },\n  -> even?(second($))\n)",
            ],
        },
        'not-any?': {
            title: 'not-any?',
            category: 'Collection',
            returns: {
                type: 'boolean',
            },
            args: __assign({}, getOperatorArgs('collection', 'function')),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Returns `false` if any element in $a pass the test implemented by $b, otherwise returns `true`.',
            examples: [
                "\nnot-any?(\n  [\"Albert\", \"Mojir\", 160, [1, 2]],\n  string?\n)",
                "\nnot-any?(\n  [50, 100, 150, 200],\n  x -> x > 10\n)",
                "\nnot-any?(\n  [],\n  number?\n)",
                "\nnot-any?(\n  \"\",\n  number?\n)",
                "\nnot-any?(\n  {},\n  number?\n)",
                "\nnot-any?(\n  { a: 2, b: 3 },\n  -> even?(second($))\n)",
                "\nnot-any?(\n  { a: 1, b: 3 },\n  -> even?(second($))\n)",
            ],
        },
        'update': {
            title: 'update',
            category: 'Collection',
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
                'fun': {
                    type: 'function',
                },
                'fun-args': {
                    type: 'any',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['coll', 'value', 'fun'] },
                { argumentNames: ['coll', 'value', 'fun', 'fun-args'] },
            ],
            description: "\nUpdates a value in the $coll collection, where $key is a key. $fun is a function\nthat will take the old value and any supplied $fun-args and\nreturn the new value.\nIf the key does not exist, `null` is passed as the old value.",
            examples: [
                "\nlet x = { a: 1, b: 2 };\nupdate(x, \"a\", inc)",
                "\nlet x = { a: 1, b: 2 };\nupdate(\n  x,\n  \"c\",\n  val -> null?(val) ? 0 : inc(val)\n)",
            ],
        },
        'update-in': {
            title: 'update-in',
            category: 'Collection',
            returns: {
                type: 'collection',
            },
            args: {
                'coll': {
                    type: 'collection',
                },
                'ks': {
                    type: 'array',
                },
                'fun': {
                    type: 'function',
                },
                'fun-args': {
                    type: 'any',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['coll', 'ks', 'fun'] },
                { argumentNames: ['coll', 'ks', 'fun', 'fun-args'] },
            ],
            description: "Updates a value in the $coll collection, where $ks is an array of\nkeys and $fun is a function that will take the old value and\nany supplied $fun-args and return the new value. If any levels do not exist,\nobjects will be created - and the corresponding keys must be of type string.",
            examples: [
                "\nupdate-in(\n  { a: [1, 2, 3] },\n  [\"a\", 1],\n  -> null?($) ? 0 : inc($)\n)",
                "\nupdate-in(\n  { a: { foo: \"bar\"} },\n  [\"a\", \"foo\"],\n  -> null?($) ? \"?\" : \"!\"\n)",
                "\nupdate-in(\n  { a: { foo: \"bar\"} },\n  [\"a\", \"baz\"],\n  -> null?($) ? \"?\" : \"!\"\n)",
                "\nupdate-in(\n  { a: [1, 2, 3] },\n  [\"a\", 1],\n  *,\n  10,\n  10,\n  10,\n)",
            ],
        },
    };

    var functionalReference = {
        '|>': {
            title: '|>',
            category: 'Functional',
            returns: {
                type: 'any',
            },
            args: __assign({}, getOperatorArgs('any', 'function')),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Takes a value $a and a function $b, and returns the result of applying $b to $a.',
            examples: [
                "\n1 |> inc |> inc",
                "range(10)\n  |> map(_, -> $ ^ 2) // [0, 1, 4, 9, 16, 25, 36, 49, 64, 81]\n  |> filter(_, odd?)  // [1, 9, 25, 49, 81]\n  |> reduce(_, +, 0)  // 165\n  |> sqrt             // 12.84523257866513\n  |> round(_, 2)",
            ],
        },
        'apply': {
            title: 'apply',
            category: 'Functional',
            returns: {
                type: 'any',
            },
            args: __assign(__assign({}, getOperatorArgs('function', 'array')), { fun: {
                    type: 'function',
                }, args: {
                    type: 'array',
                } }),
            variants: [
                { argumentNames: ['fun', 'args'] },
            ],
            description: 'Call supplied function $fun with specified arguments $args.',
            examples: [
                "\napply(+, [1, 2, 3])",
                "\napply(\n  (x, y) -> sqrt(x ^ 2 + y ^ 2),\n  [3, 4]\n)",
                "\n(x, y) -> sqrt(x ^ 2 + y ^ 2) apply [3, 4]",
            ],
        },
        'identity': {
            title: 'identity',
            category: 'Functional',
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
            examples: ['identity(1)', 'identity("Albert")', 'identity({ a: 1 })', 'identity(null)'],
        },
        'comp': {
            title: 'comp',
            category: 'Functional',
            returns: {
                type: 'function',
            },
            args: __assign(__assign({}, getOperatorArgs('function', 'function')), { fns: {
                    type: 'function',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['fns'] },
            ],
            description: "Takes a variable number of functions and returns a function that is the composition of those.\n\n  The returned function takes a variable number of arguments,\n  applies the rightmost function to the args,\n  the next function (right-to-left) to the result, etc.",
            examples: [
                "\nlet negative-quotient = comp(-, /);\nnegative-quotient(9, 3)",
                "\nlet x = { bar: { foo: 42 } };\ncomp(\"foo\", \"bar\")(x)",
            ],
        },
        'constantly': {
            title: 'constantly',
            category: 'Functional',
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
                "\nlet always-true = constantly(true);\nalways-true(9, 3)",
            ],
        },
        'juxt': {
            title: 'juxt',
            category: 'Functional',
            returns: {
                type: 'function',
            },
            args: __assign(__assign({}, getOperatorArgs('function', 'function')), { fun: {
                    type: 'function',
                }, fns: {
                    type: 'function',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['fun'] },
                { argumentNames: ['fun', 'fns'] },
            ],
            description: "Takes one or many function and returns a function that is the juxtaposition of those functions.  \nThe returned function takes a variable number of args,\nand returns a vector containing the result of applying each function to the args (left-to-right).",
            examples: [
                "\njuxt(+, *, min, max)(\n  3,\n  4,\n  6,\n)",
                "\njuxt(\"a\", \"b\")(\n  {\n    a: 1,\n    b: 2,\n    c: 3,\n    d: 4\n  }\n)",
                "\njuxt(+, *, min, max) apply range(1, 11)",
            ],
        },
        'complement': {
            title: 'complement',
            category: 'Functional',
            returns: {
                type: 'function',
            },
            args: {
                fun: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['fun'] },
            ],
            description: 'Takes a function $fun and returns a new function that takes the same arguments as f, has the same effects, if any, and returns the opposite truth value.',
            examples: [
                'complement(>)(1, 3)',
                'complement(<)(1, 3)',
                'complement(+)(1, 3)',
                'complement(+)(0, 0)',
            ],
        },
        'every-pred': {
            title: 'every-pred',
            category: 'Functional',
            returns: {
                type: 'function',
            },
            args: {
                fun: {
                    type: 'function',
                },
                fns: {
                    type: 'function',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['fun'] },
                { argumentNames: ['fun', 'fns'] },
            ],
            description: "\nTakes a number of predicates and returns a function that returns `true` if all predicates\nreturn a truthy value against all of its arguments, else it returns `false`.",
            examples: [
                "\nevery-pred(string?, -> count($) > 3)(\n  \"Albert\",\n  \"Mojir\"\n)",
                "\n(string? every-pred -> count($) > 3)(\n  \"Albert\",\n  \"M\"\n)",
            ],
            noOperatorDocumentation: true,
        },
        'some-pred': {
            title: 'some-pred',
            category: 'Functional',
            returns: {
                type: 'function',
            },
            args: {
                fun: {
                    type: 'function',
                },
                fns: {
                    type: 'function',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['fun'] },
                { argumentNames: ['fun', 'fns'] },
            ],
            description: 'Takes a number of `predicates` and returns a function that returns \`true\` if at least one of the `predicates` return a truthy \`true\` value against at least one of its arguments, else it returns `false`.',
            examples: [
                'some-pred(string?, -> count($) > 3)("Albert", "Mojir")',
                'some-pred(string?, -> count($) > 3)("a", "M")',
                'some-pred(string?, -> count($) > 3)("a", [1, 2, 3])',
                'some-pred(string?, -> count($) > 3)([1, 2, 3], [2])',
            ],
            noOperatorDocumentation: true,
        },
        'fnull': {
            title: 'fnull',
            category: 'Functional',
            returns: {
                type: 'function',
            },
            args: __assign(__assign({}, getOperatorArgs('function', 'any')), { fun: {
                    type: 'function',
                }, arg: {
                    type: 'any',
                }, args: {
                    type: 'any',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['fun', 'arg'] },
                { argumentNames: ['fun', 'arg', 'args'] },
            ],
            description: 'Takes a function $fun, and returns a function that calls $fun, replacing a null argument to the corresponding argument.',
            examples: [
                'fnull(inc, 0)(1)',
                'fnull(inc, 0)(null)',
                '(inc fnull 0)(null)',
                'fnull(+, 1, 2)(null, 0)',
                'fnull(+, 1, 2)(0, null)',
                'fnull(+, 1, 2)(null, null)',
                'fnull(+, 1, 2)(null, null, 3, 4)',
            ],
        },
    };

    var mixedArgs = {
        type: ['number', 'vector', 'matrix'],
        rest: true,
    };
    var mixedOperatorArgs = getOperatorArgs(['number', 'vector', 'matrix'], ['number', 'vector', 'matrix']);
    var mathReference = {
        '+': {
            title: '+',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: __assign({ xs: mixedArgs }, mixedOperatorArgs),
            variants: [
                { argumentNames: ['xs'] },
            ],
            description: 'The `+` function performs addition of numbers and element-wise addition of `vectors` and `matrices` of compatible dimensions, returning the same type as its inputs. When used with mixed types, it adds the scalar to each element of the collection.',
            examples: [
                '1 + 2',
                '1 + 20 + 30',
                '+(1, 2, 3, 4)',
                '+()',
                '+(1)',
                '[1, 2, 3] + 2',
                '[1, 2, 3] + [4, 5, 6]',
                '[[1, 2, 3], [4, 5, 6]] + [[7, 8, 9], [10, 11, 12]]',
                '[[1, 2, 3], [4, 5, 6]] + 2',
            ],
        },
        '-': {
            title: '-',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: __assign({ xs: mixedArgs }, mixedOperatorArgs),
            variants: [
                { argumentNames: ['xs'] },
            ],
            description: 'Computes difference between first value and sum of the rest. When called with only one argument, it does negation.',
            examples: [
                '50 - 8',
                '1 - 1 - 1',
                '-()',
                '-(4, 2)',
                '-(4, 3, 2, 1,)',
                '[1, 2, 3] - 2',
                '[1, 2, 3] - [4, 5, 6]',
                '[[1, 2, 3], [4, 5, 6]] - [[7, 8, 9], [10, 11, 12]]',
                '[[1, 2, 3], [4, 5, 6]] - 2',
            ],
        },
        '*': {
            title: '*',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: __assign({ xs: mixedArgs }, mixedOperatorArgs),
            variants: [
                { argumentNames: ['xs'] },
            ],
            description: 'The `*` function performs multiplication of `numbers` and element-wise multiplication of `vectors` and `matrices` of compatible dimensions, returning the same type as its inputs. When used with mixed types, it multiplies each element of the collection by the scalar.',
            examples: [
                '6 * 7',
                '-1 * 4',
                '*(4, 7)',
                '*(1, 2, 3, 4, 5)',
                '*()',
                '*(8)',
                '[1, 2, 3] * 2',
                '[1, 2, 3] * [4, 5, 6]',
                '[[1, 2, 3], [4, 5, 6]] * [[7, 8, 9], [10, 11, 12]]',
                '[[1, 2, 3], [4, 5, 6]] * 2',
            ],
            aliases: ['·'],
        },
        '/': {
            title: '/',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: __assign({ xs: mixedArgs }, mixedOperatorArgs),
            variants: [
                { argumentNames: ['xs'] },
            ],
            description: 'The `/` function performs division of `numbers` and element-wise division of `vectors` and `matrices` of compatible dimensions, returning the same type as its inputs. When used with mixed types, it divides each element of the collection by the scalar.',
            examples: [
                '12 / 100',
                '-1 / 4',
                '/(7, 4)',
                '/(1, 2, 4, 8)',
                '/()',
                '/(8)',
                '[1, 2, 3] / 2',
                '[1, 2, 3] / [4, 5, 6]',
                '[[1, 2, 3], [4, 5, 6]] / [[7, 8, 9], [10, 11, 12]]',
                '[[1, 2, 3], [4, 5, 6]] / 2',
            ],
        },
        'mod': {
            title: 'mod',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: __assign({}, mixedOperatorArgs),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'The `mod` function computes the modulo of division with the same sign as the divisor, working on `numbers` and element-wise on `vectors` and `matrices` of compatible dimensions. When used with mixed types, it applies the modulo operation between each element of the collection and the scalar.',
            examples: [
                'mod(5, 3)',
                'mod(5.2, 3.1)',
                'mod(-5, 3)',
                '5 mod -3',
                '-5 mod -3',
                '[1, 2, 3] mod 2',
                '2 mod [1, 2, 3]',
                'mod([1, 2, 3], [4, 5, 6])',
                '[[1, 2, 3], [4, 5, 6]] mod [[7, 8, 9], [10, 11, 12]]',
                'mod([[1, 2, 3], [4, 5, 6]], 2)',
            ],
        },
        'rem': {
            title: 'rem',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: __assign({}, mixedOperatorArgs),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'The `rem` function computes the remainder of division with the same sign as the dividend, working on `numbers` and element-wise on `vectors` and `matrices` of compatible dimensions. When used with mixed types, it applies the remainder operation between each element of the collection and the scalar.',
            examples: [
                '5 % 3',
                '5.2 % 3.1',
                '-5 % 3',
                '%(5, -3)',
                '%(-5, -3)',
                '5 rem -3',
                '-5 rem -3',
                '[1, 2, 3] % 2',
                '2 % [1, 2, 3]',
                '%([1, 2, 3], [4, 5, 6])',
                '[[1, 2, 3], [4, 5, 6]] % [[7, 8, 9], [10, 11, 12]]',
                '%([[1, 2, 3], [4, 5, 6]], 2)',
                '[[1, 2, 3], [4, 5, 6]] rem [[7, 8, 9], [10, 11, 12]]',
            ],
            aliases: ['%'],
        },
        'quot': {
            title: 'quot',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: __assign({}, mixedOperatorArgs),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'The `quot` function performs integer division truncated toward zero, working on `numbers` and element-wise on `vectors` and `matrices` of compatible dimensions. When used with mixed types, it applies integer division between each element of the collection and the scalar.',
            examples: [
                'quot(5, 3)',
                'quot(5.2, 3.1)',
                'quot(-5, 3)',
                '5 quot -3',
                '-5 quot -3',
                'quot(5, 0)',
                'quot(0, 5)',
                '[1, 2, 3] quot 2',
                '2 quot [1, 2, 3]',
                'quot([1, 2, 3], [4, 5, 6])',
                '[[1, 2, 3], [4, 5, 6]] quot [[7, 8, 9], [10, 11, 12]]',
                'quot([[1, 2, 3], [4, 5, 6]], 2)',
                '[[1, 2, 3], [4, 5, 6]] quot [[7, 8, 9], [10, 11, 12]]',
            ],
        },
        'inc': {
            title: 'inc',
            category: 'Math',
            returns: {
                type: 'number',
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `inc` function increments its argument by 1, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it increases each element by 1 while preserving the original structure.',
            examples: [
                'inc(0)',
                'inc(1)',
                'inc(100.1)',
                'inc([1, 2, 3])',
                'inc([[1, 2], [3, 4]])',
            ],
        },
        'dec': {
            title: 'dec',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `dec` function decrements its argument by 1, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it decreases each element by 1 while preserving the original structure.',
            examples: [
                'dec(0)',
                'dec(1)',
                'dec(100.1)',
                'dec([1, 2, 3])',
                'dec([[1, 2], [3, 4]])',
            ],
        },
        'sqrt': {
            title: 'sqrt',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `sqrt` function calculates the square root of `numbers` and computes element-wise square roots of `vectors` and `matrices`. When applied to collections, it returns the square root of each element while preserving the original structure.',
            examples: [
                '√(0)',
                '√(9)',
                '√(2)',
                'sqrt(0)',
                'sqrt(9)',
                'sqrt(2)',
                'sqrt([1, 4, 9])',
                'sqrt([[1, 4], [9, 16]])',
            ],
            aliases: ['√'],
        },
        'cbrt': {
            title: 'cbrt',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `cbrt` function calculates the cube root of `numbers` and computes element-wise cube roots of `vectors` and `matrices`. When applied to collections, it returns the cube root of each element while preserving the original structure.',
            examples: [
                '∛(0)',
                '∛(27)',
                '∛(2)',
                '∛(1)',
                'cbrt(0)',
                'cbrt(27)',
                'cbrt(2)',
                'cbrt(1)',
                'cbrt([1, 8, 27])',
                'cbrt([[1, 8], [27, 64]])',
            ],
            aliases: ['∛'],
        },
        '^': {
            title: '^',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: __assign({}, mixedOperatorArgs),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'The ^ function computes exponentiation, raising the first argument to the power of the second, working on `numbers` and element-wise on `vectors` and `matrices` of compatible dimensions. When used with mixed types, it applies the power operation between each element of the collection and the scalar.',
            examples: [
                '2 ^ 3',
                '2 ^ 0',
                '2 ^ -3',
                '^(-2, 3)',
                '^(-2, -3)',
                '[1, 2, 3] ^ 2',
                '2 ^ [1, 2, 3]',
                '^([1, 2, 3], [4, 5, 6])',
                '[[1, 2, 3], [4, 5, 6]] ^ [[7, 8, 9], [10, 11, 12]]',
                '^([[1, 2, 3], [4, 5, 6]], 2)',
            ],
        },
        'round': {
            title: 'round',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: __assign({}, getOperatorArgs(['number', 'vector', 'matrix'], 'integer')),
            variants: [
                { argumentNames: ['a'] },
                { argumentNames: ['a', 'b'] },
            ],
            description: 'The `round` function rounds a `number` to the nearest `integer` or to a specified number of `decimal` places, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it rounds each element while preserving the original structure.',
            examples: [
                'round(2)',
                'round(2.49)',
                'round(2.5)',
                'round(-2.49)',
                'round(-2.5)',
                'round(-2.501)',
                'round(1.23456789, 4)',
                '1.123456789 round 2',
                'round([1.23456789, 2.3456789], 1)',
                '[1.23456789, 2.3456789] round 4',
                '[[1.23456789, 2.3456789], [3.456789, 4.56789]] round 4',
                'round([[1.23456789, 2.3456789], [3.456789, 4.56789]], 2)',
            ],
        },
        'trunc': {
            title: 'trunc',
            category: 'Math',
            returns: {
                type: ['integer', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `trunc` function truncates `numbers` toward zero (removing decimal portions without rounding), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it truncates each element while preserving the original structure.',
            examples: [
                'trunc(2)',
                'trunc(2.49)',
                'trunc(2.5)',
                'trunc(-2.49)',
                'trunc(-2.5)',
                'trunc(-2.501)',
                'trunc([1.23456789, 2.3456789])',
                'trunc([[1.23456789, 2.3456789], [3.456789, 4.56789]])',
            ],
        },
        'floor': {
            title: 'floor',
            category: 'Math',
            returns: {
                type: ['integer', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `floor` function returns the largest `integer` less than or equal to a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the floor of each element while preserving the original structure.',
            examples: [
                'floor(2)',
                'floor(2.49)',
                'floor(2.5)',
                'floor(-2.49)',
                'floor(-2.5)',
                'floor(-2.501)',
                'floor([1.23456789, 2.3456789])',
                'floor([[1.23456789, 2.3456789], [3.456789, 4.56789]])',
            ],
        },
        'ceil': {
            title: 'ceil',
            category: 'Math',
            returns: {
                type: ['integer', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `ceil` function returns the smallest `integer` greater than or equal to a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the ceiling of each element while preserving the original structure.',
            examples: [
                'ceil(2)',
                'ceil(2.49)',
                'ceil(2.5)',
                'ceil(-2.49)',
                'ceil(-2.5)',
                'ceil(-2.501)',
                'ceil([1.23456789, 2.3456789])',
                'ceil([[1.23456789, 2.3456789], [3.456789, 4.56789]])',
            ],
        },
        'min': {
            title: 'min',
            category: 'Math',
            returns: {
                type: 'number',
            },
            args: __assign(__assign({}, getOperatorArgs('number', 'number')), { xs: {
                    type: 'number',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['xs'] },
            ],
            description: 'Returns the smallest number of the arguments.',
            examples: [
                '2 min 3',
                'min(2, 0, 1)',
                'min(2, -1, 1)',
                'min(2.5)',
                '12 min 14',
            ],
        },
        'max': {
            title: 'max',
            category: 'Math',
            returns: {
                type: 'number',
            },
            args: __assign(__assign({}, getOperatorArgs('number', 'number')), { xs: {
                    type: 'number',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['xs'] },
            ],
            description: 'Returns the largest number of the arguments.',
            examples: [
                ' 2 max 3',
                'max(2, 0, 1)',
                'max(2, -1, 1)',
                'max(2, 0.5)',
                '4 max 2',
            ],
        },
        'abs': {
            title: 'abs',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The abs function returns the absolute value (magnitude) of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the absolute value of each element while preserving the original structure.',
            examples: [
                'abs(-2.3)',
                'abs(0)',
                'abs(2.5)',
                'abs([1, -2, 3])',
                'abs([[1, -2], [3, -4]])',
            ],
        },
        'sign': {
            title: 'sign',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `sign` function returns the `sign** of a **number` (-1 for negative, 0 for zero, 1 for positive), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the sign of each element while preserving the original structure.',
            examples: [
                'sign(-2.3)',
                'sign(-0)',
                'sign(0)',
                'sign(12312)',
                'sign([1, -2, 3])',
                'sign([[1, -2], [3, -4]])',
            ],
        },
        'ln': {
            title: 'ln',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `ln` function computes the natural logarithm (base `e`) of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the natural logarithm of each element while preserving the original structure.',
            examples: [
                'ln(0.01)',
                'ln(2.5)',
                'ln(E)',
                'ln([1, 2, 3])',
                'ln([[1, 2], [3, 4]])',
            ],
        },
        'log2': {
            title: 'log2',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `log2` function computes the base `2` logarithm of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the base-2 logarithm of each element while preserving the original structure.',
            examples: [
                'log2(0.01)',
                'log2(2 ^ 12)',
                'log2(2.5)',
                'log2([1, 2, 3])',
                'log2([[1, 2], [3, 4]])',
            ],
            aliases: ['log₂'],
        },
        'log10': {
            title: 'log10',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            aliases: ['log₁₀'],
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `log10` function computes the base `10` logarithm of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the base-10 logarithm of each element while preserving the original structure.',
            examples: [
                'log10(0.01)',
                'log10(10 ^ 12)',
                'log10(2.5)',
                'log10([1, 2, 3])',
                'log10([[1, 2], [3, 4]])',
            ],
        },
        'sin': {
            title: 'sin',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `sin` function computes the sine of an angle (in radians), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the sine of each element while preserving the original structure.',
            examples: [
                'sin(0)',
                'sin(1)',
                'sin(PI)',
                'sin(-0.5)',
                'sin([1, 2, 3])',
                'sin([[1, 2], [3, 4]])',
            ],
        },
        'cos': {
            title: 'cos',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `cos` function computes the cosine of an angle (in radians), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the cosine of each element while preserving the original structure.',
            examples: [
                'cos(0)',
                'cos(1)',
                'cos(PI)',
                'cos(-0.5)',
                'cos([1, 2, 3])',
                'cos([[1, 2], [3, 4]])',
            ],
        },
        'tan': {
            title: 'tan',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `tan` function computes the tangent of an angle (in radians), working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the tangent of each element while preserving the original structure.',
            examples: [
                'tan(0)',
                'tan(1)',
                'tan(PI)',
                'tan(-0.5)',
                'tan([1, 2, 3])',
                'tan([[1, 2], [3, 4]])',
            ],
        },
        'asin': {
            title: 'asin',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `asin` function computes the arcsine (inverse sine) of a `number` in radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the arcsine of each element while preserving the original structure.',
            examples: [
                'asin(0)',
                'asin(1)',
                'asin(-0.5)',
                'asin([1, 2, 3])',
                'asin([[1, 2], [3, 4]])',
            ],
        },
        'acos': {
            title: 'acos',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `acos` function computes the arccosine (inverse cosine) of a `number` in radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the arccosine of each element while preserving the original structure.',
            examples: [
                'acos(0)',
                'acos(1)',
                'acos(-0.5)',
                'acos([0.1, 0.2, 0.3])',
                'acos([[0.1, 0.2], [0.3, 0.4]])',
            ],
        },
        'atan': {
            title: 'atan',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `atan` function computes the arctangent (inverse tangent) of a `number` in radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the arctangent of each element while preserving the original structure.',
            examples: [
                'atan(0)',
                'atan(1)',
                'atan(-0.5)',
                'atan([0.1, 0.2, 0.3])',
                'atan([[0.1, 0.2], [0.3, 0.4]])',
            ],
        },
        'sinh': {
            title: 'sinh',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `sinh` function computes the hyperbolic sine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the hyperbolic sine of each element while preserving the original structure.',
            examples: [
                'sinh(0)',
                'sinh(1)',
                'sinh(-0.5)',
                'sinh([0.1, 0.2, 0.3])',
                'sinh([[0.1, 0.2], [0.3, 0.4]])',
            ],
        },
        'cosh': {
            title: 'cosh',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `cosh` function computes the hyperbolic cosine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the hyperbolic cosine of each element while preserving the original structure.',
            examples: [
                'cosh(0)',
                'cosh(1)',
                'cosh(-0.5)',
                'cosh([0.1, 0.2, 0.3])',
                'cosh([[0.1, 0.2], [0.3, 0.4]])',
            ],
        },
        'tanh': {
            title: 'tanh',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `tanh` function computes the hyperbolic tangent of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the hyperbolic tangent of each element while preserving the original structure.',
            examples: ['tanh(0)', 'tanh(1)', 'tanh(-0.5)', 'tanh(50)'],
        },
        'asinh': {
            title: 'asinh',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `asinh` function computes the inverse hyperbolic sine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the inverse hyperbolic sine of each element while preserving the original structure.',
            examples: [
                'asinh(10)',
                'asinh(90)',
                'asinh (50)',
                'asinh([10, 20, 30])',
                'asinh([[10, 20], [30, 40]])',
            ],
        },
        'acosh': {
            title: 'acosh',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `acosh` function computes the inverse hyperbolic cosine of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the inverse hyperbolic cosine of each element while preserving the original structure.',
            examples: [
                'acosh(1)',
                'acosh(2)',
                'acosh(100)',
                'acosh(50)',
                'acosh([1, 2, 3])',
                'acosh([[1, 2], [3, 4]])',
            ],
        },
        'atanh': {
            title: 'atanh',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `atanh` function computes the inverse hyperbolic tangent of a `number`, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it returns the inverse hyperbolic tangent of each element while preserving the original structure.',
            examples: [
                'atanh(0)',
                'atanh(0.9)',
                'atanh(-0.5)',
                'atanh([0.1, 0.2, 0.3])',
                'atanh([[0.1, 0.2], [0.3, 0.4]])',
            ],
        },
        'to-rad': {
            title: 'to-rad',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `to-rad` function converts an angle from degrees to radians, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it converts each element while preserving the original structure.',
            examples: [
                'to-rad(0)',
                'to-rad(90)',
                'to-rad(180)',
                'to-rad(360)',
                'to-rad([0, 90, 180])',
                'to-rad([[0, 90], [180, 360]])',
            ],
        },
        'to-deg': {
            title: 'to-deg',
            category: 'Math',
            returns: {
                type: ['number', 'vector', 'matrix'],
            },
            args: {
                x: {
                    type: ['number', 'vector', 'matrix'],
                },
            },
            variants: [
                { argumentNames: ['x'] },
            ],
            description: 'The `to-deg` function converts an angle from radians to degrees, working on `numbers` and element-wise on `vectors` and `matrices`. When applied to collections, it converts each element while preserving the original structure.',
            examples: [
                'to-deg(0)',
                'to-deg(PI)',
                'to-deg(PI / 2)',
                'to-deg(3 * PI / 2)',
                'to-deg([0, PI, PI / 2])',
                'to-deg([[0, PI], [PI / 2, 3 * PI / 2]])',
            ],
        },
    };

    var metaReference = {
        doc: {
            title: 'doc',
            category: 'Meta',
            returns: {
                type: 'string',
            },
            args: {
                fun: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['fun'] },
            ],
            description: 'Returns documentation string of the $fun.',
            examples: [
                'doc(+)',
                "\nlet add = (x, y) -> {\n  \"\"\"\n  Adds two numbers.\n  Args:\n    x: First number.\n    y: Second number.\n  Returns:\n    Sum of x and y.\n  \"\"\"\n  x + y;\n};\n\ndoc(add)",
            ],
        },
        arity: {
            title: 'arity',
            category: 'Meta',
            returns: {
                type: 'object',
            },
            args: {
                fun: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['fun'] },
            ],
            description: 'Returns arity of the $fun. The arity is an object with the properties: `min` and `max`. If the function has fixed arity, `min` and `max` are equal to the number of required parameters. If no restrictions apply, empty object is returned.',
            examples: [
                'arity(+)',
                'arity(defined?)',
                "\nlet add = (x, y = 0) -> {\n  x + y;\n};\n\narity(add)",
                "\nlet foo = (k, ...x) -> {\n  k + x;\n};\n  arity(foo)",
            ],
        },
    };

    var miscReference = {
        '!=': {
            title: '!=',
            category: 'Misc',
            returns: {
                type: 'boolean',
            },
            args: __assign(__assign({}, getOperatorArgs('any', 'any')), { x: {
                    type: 'any',
                }, ys: {
                    type: 'any',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['x'] },
                { argumentNames: ['x', 'ys'] },
            ],
            description: 'Returns `true` if all `values` are not equal to each other, otherwise result is `false`. `(!= a b c)` is same as `(! (== a b c))`.',
            examples: [
                '1 ≠ 2',
                '3 != 3',
                '≠(3)',
                '!=(3, 3, 2)',
                '≠("3", "2", "1", "0",)',
                '!=(0, -0)',
            ],
            aliases: ['≠'],
        },
        '==': {
            title: '==',
            category: 'Misc',
            returns: {
                type: 'boolean',
            },
            args: __assign(__assign({}, getOperatorArgs('any', 'any')), { x: {
                    type: 'any',
                }, ys: {
                    type: 'any',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['x'] },
                { argumentNames: ['x', 'ys'] },
            ],
            description: 'Returns `true` if all `values` are structaul equal to each other, otherwise result is `false`.',
            examples: [
                '1 == 1',
                '[1, 2] == [1, 2]',
                "\n{\n a: 1,\n b: 2,\n} == {\n b: 2,\n a: 1,\n}",
                '==(1, 1)',
                '==(1.01, 1)',
                '==("1", 1)',
                '==("2", "2", "2", "2")',
                '==(2, 2, 1, 2)',
                '==([1, 2], [1, 2])',
                '==({ a: 1, b: 2 }, { b: 2, a: 1 })',
            ],
        },
        '<': {
            title: '<',
            category: 'Misc',
            returns: {
                type: 'boolean',
            },
            args: __assign(__assign({}, getOperatorArgs(['number', 'string'], ['number', 'string'])), { x: {
                    type: ['number', 'string'],
                }, ys: {
                    type: ['number', 'string'],
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['x'] },
                { argumentNames: ['x', 'ys'] },
            ],
            description: 'Returns `true` if $x and $ys are in increasing order, `false` otherwise.',
            examples: [
                '<(0, 1)',
                '<(1, 1.01)',
                '<(1, 1)',
                '<(1, 2, 2, 3)',
                '<("a", "b")',
            ],
        },
        '>': {
            title: '>',
            category: 'Misc',
            returns: {
                type: 'boolean',
            },
            args: __assign(__assign({}, getOperatorArgs(['number', 'string'], ['number', 'string'])), { x: {
                    type: ['number', 'string'],
                }, ys: {
                    type: ['number', 'string'],
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['x'] },
                { argumentNames: ['x', 'ys'] },
            ],
            description: 'Returns `true` if $x and $ys are in decreasing order, `false` otherwise.',
            examples: [
                '>(1, 0)',
                '>(1.01, 1)',
                '>(1, 1)',
                '>(4, 3, 2, 1)',
                '>(3, 2, 2, 1)',
            ],
        },
        '<=': {
            title: '<=',
            category: 'Misc',
            returns: {
                type: 'boolean',
            },
            args: __assign(__assign({}, getOperatorArgs(['number', 'string'], ['number', 'string'])), { x: {
                    type: ['number', 'string'],
                }, ys: {
                    type: ['number', 'string'],
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['x'] },
                { argumentNames: ['x', 'ys'] },
            ],
            description: 'Returns `true` if $x and $ys are in non decreasing order, `false` otherwise.',
            examples: [
                '1 ≤ 1',
                '<=(0, 1)',
                '≤(1, 1.01)',
                '<=(1, 1)',
                '≤(1, 2, 3, 4)',
                '<=(1, 2, 2, 3)',
            ],
            aliases: ['≤'],
        },
        '>=': {
            title: '>=',
            category: 'Misc',
            returns: {
                type: 'boolean',
            },
            args: __assign(__assign({}, getOperatorArgs(['number', 'string'], ['number', 'string'])), { x: {
                    type: ['number', 'string'],
                }, ys: {
                    type: ['number', 'string'],
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['x'] },
                { argumentNames: ['x', 'ys'] },
            ],
            description: 'Returns `true` if $x and $ys are in non increasing order, `false` otherwise.',
            examples: [
                '1 ≥ 1',
                '0 ≥ 1',
                '>=(1, 0)',
                '≥(1.01, 1)',
                '>=(1, 1)',
                '≥(4, 3, 2, 1)',
                '>=(3, 2, 2, 1)',
            ],
            aliases: ['≥'],
        },
        '!': {
            title: '!',
            category: 'Misc',
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
            description: 'Computes logical negation. Note that any other $x than `false`, `0`, `null` and `\'\'` is truthy.',
            examples: [
                '!(3)',
                '!(true)',
                '!("A string")',
                '!(0)',
                '!(false)',
                '!(null)',
                '!("")',
            ],
        },
        'write!': {
            title: 'write!',
            category: 'Misc',
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
            description: 'It logs the $values and then returns the last argument. If called with no arguments `null` is returned.',
            examples: [
                'write!("A string")',
                'write!(100, "items")',
                'write!(object("a", 10))',
                'write!(["a", "b", "c"])',
                'write!(#"^start")',
                'write!(null, true, false)',
            ],
            noOperatorDocumentation: true,
        },
        'iso-date->epoch': {
            title: 'iso-date->epoch',
            category: 'Misc',
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
                'iso-date->epoch("2022-04-12T09:37:10.899Z")',
                'iso-date->epoch("1980-01-01")',
            ],
        },
        'epoch->iso-date': {
            title: 'epoch->iso-date',
            category: 'Misc',
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
                'epoch->iso-date(1649756230899)',
                'epoch->iso-date(0)',
            ],
        },
        'boolean': {
            title: 'boolean',
            category: 'Misc',
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
                'boolean(0)',
                'boolean(1)',
                'boolean(null)',
                'boolean("Albert")',
            ],
        },
        'compare': {
            title: 'compare',
            category: 'Misc',
            returns: {
                type: 'number',
            },
            args: __assign({}, getOperatorArgs(['number', 'string'], ['number', 'string'])),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Compares two values. Returns `-1` if $a < $b, `1` if $a > $b and `0` if $a and $b have the same sort order.',
            examples: [
                'compare(0, 1)',
                'compare(0, 0)',
                'compare(1, 0)',
                'compare("Albert", "Mojir")',
            ],
        },
        'identical?': {
            title: 'identical?',
            category: 'Misc',
            returns: {
                type: 'boolean',
            },
            args: __assign({}, getOperatorArgs('any', 'any')),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Returns true if $a and $b are referential equal.',
            examples: [
                'identical?({ a: 10, b: 20 }, { b: 20, a: 10 })',
                'identical?([1, true, null], [1, true, null])',
                'identical?(0.3, 0.1 + 0.2)',
            ],
        },
        'json-parse': {
            title: 'json-parse',
            category: 'Misc',
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
                'json-parse("[1, 2, 3]")',
            ],
        },
        'json-stringify': {
            title: 'json-stringify',
            category: 'Misc',
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
                'json-stringify([1, 2, 3])',
                'json-stringify({ a: { b: 10 }}, 2)',
            ],
            noOperatorDocumentation: true,
        },
    };

    var objectReference = {
        'dissoc': {
            title: 'dissoc',
            category: 'Object',
            returns: {
                type: 'object',
            },
            args: __assign(__assign({}, getOperatorArgs('object', 'string')), { obj: {
                    type: 'object',
                }, key: {
                    type: 'string',
                } }),
            variants: [
                { argumentNames: ['obj', 'key'] },
            ],
            description: 'Return shallow copy of $obj with $key deleted.',
            examples: [
                '{ x: 10, y: 20 } dissoc "y"',
                'dissoc({ x: 10, y: 20 }, "x")',
                'dissoc({ x: 10 }, "y")',
                "\nlet o = { a: 5 };\ndissoc(o, \"a\");\no",
            ],
        },
        'keys': {
            title: 'keys',
            category: 'Object',
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
                'keys({})',
                'keys({ x: 10, y: true, z: "A string" })',
                'keys(object("x", 10, "y", true, "z", "A string"))',
            ],
        },
        'vals': {
            title: 'vals',
            category: 'Object',
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
                'vals({})',
                'vals({ x: 10, y: true, z: "A string" })',
                'vals(object("x", 10, "y", true, "z", "A string"))',
            ],
        },
        'entries': {
            title: 'entries',
            category: 'Object',
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
                'entries({})',
                'entries({ x: 10, y: true, z: "A string" })',
                'entries(object("x", 10, "y", true, "z", "A string"))',
            ],
        },
        'find': {
            title: 'find',
            category: 'Object',
            returns: {
                type: ['array', 'null'],
            },
            args: __assign(__assign({}, getOperatorArgs('object', 'string')), { obj: {
                    type: 'object',
                }, key: {
                    type: 'string',
                } }),
            variants: [
                { argumentNames: ['obj', 'key'] },
            ],
            description: 'Returns entry (key-value pair) for $key, or `null` if $key not present in $obj.',
            examples: [
                '{ a: 1, "b": 2 } find "a"',
                'find(object("a", 1, "b", 2), "b")',
                'find(object("a", 1, "b", 2), "c")',
            ],
        },
        'merge': {
            title: 'merge',
            category: 'Object',
            returns: {
                type: 'object',
            },
            args: __assign(__assign({}, getOperatorArgs('object', 'object')), { objs: {
                    type: 'object',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['objs'] },
            ],
            description: "Returns a new object created by merging together all arguments.\n\nIf two keys appears in more than one object the value from the last object is used.  \nIf no arguments are provided `null` is returned.",
            examples: [
                '{ x: 10 } merge { y: 20 }',
                'merge(object("x", 10), object("y", 20))',
                'merge(object("x", 10), object("x", 15, "y", 20))',
            ],
        },
        'merge-with': {
            title: 'merge-with',
            category: 'Object',
            returns: {
                type: 'object',
            },
            args: {
                objs: {
                    type: 'object',
                    rest: true,
                },
                fun: {
                    type: 'function',
                },
            },
            variants: [
                { argumentNames: ['objs', 'fun'] },
            ],
            description: "\nReturns a new object created by merging together all arguments.\nIf two keys appears in more than one object $fun is used to calculate the new value.\n\nIf no arguments are provided `null` is returned.",
            examples: [
                'merge-with(object("x", 10), object("y", 20), +)',
                'merge-with(object("x", 10), object("x", 15, "y", 20), +)',
                'merge-with({ x: 10 }, { x: 20 }, { x: 30 }, { x: 40 }, -)',
            ],
            noOperatorDocumentation: true,
        },
        'zipmap': {
            title: 'zipmap',
            category: 'Object',
            returns: {
                type: 'object',
            },
            args: __assign({}, getOperatorArgs('array', 'array')),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Returns a new object created by mapping $a to $b.',
            examples: [
                '["a", "b", "c"] zipmap [1, 2, 3]',
                'zipmap(["a", "b", "c"], [10, null, [1, 2, 3]])',
                'zipmap(["a", "b", "c"], [1])',
                'zipmap([], [10, null, [1, 2, 3]])',
            ],
        },
        'select-keys': {
            title: 'select-keys',
            category: 'Object',
            returns: {
                type: 'object',
            },
            args: __assign({}, getOperatorArgs('object', 'array')),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: 'Returns an object containing only those entries in $a whose key is in $b.',
            examples: [
                '{ a: 1, b: 2, c: 3 } select-keys ["a", "b"]',
                'select-keys({ a: 1, b: 2, c: 3 }, ["a", "b"])',
                'select-keys({ a: 1 }, ["a", "b"])',
            ],
        },
    };

    var predicateReference = {
        'boolean?': {
            title: 'boolean?',
            category: 'Predicate',
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
                'boolean?(true)',
                'boolean?(false)',
                'boolean?([1, 2, 3])',
                'boolean?(0)',
                'boolean?("A string")',
            ],
        },
        'null?': {
            title: 'null?',
            category: 'Predicate',
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
            description: 'Returns `true` if $x is `null`, otherwise `false`.',
            examples: [
                'null?(null)',
                'null?(false)',
                'null?([1, 2, 3])',
                'null?(0)',
                'null?("A string")',
            ],
        },
        'number?': {
            title: 'number?',
            category: 'Predicate',
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
                'number?(0)',
                'number?(2)',
                'number?(-0.12)',
                'number?(false)',
                'number?([1, 2, 3])',
                'number?("A string")',
            ],
        },
        'string?': {
            title: 'string?',
            category: 'Predicate',
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
                'string?("")',
                'string?("A string")',
                'string?(true ? "A string" : false)',
                'string?(false)',
                'string?([1, 2, 3])',
                'string?(100)',
            ],
        },
        'function?': {
            title: 'function?',
            category: 'Predicate',
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
                'function?(+)',
                'function?(/)',
                'function?((x, y) -> x + y)',
                'function?(false)',
                'function?("false")',
                'function?([1, 2, 3])',
            ],
        },
        'integer?': {
            title: 'integer?',
            category: 'Predicate',
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
                'integer?(0)',
                'integer?(-12)',
                'integer?(42)',
                'integer?(10.1)',
                'integer?((x, y) -> x + y)',
                'integer?(false)',
                'integer?("false")',
                'integer?([1, 2, 3])',
            ],
        },
        'array?': {
            title: 'array?',
            category: 'Predicate',
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
                'array?([])',
                'array?([1, 2, 3])',
                'array?(object("a", 10))',
                'array?(42)',
                'array?(10.1)',
                'array?((x, y) -> x + y)',
            ],
        },
        'object?': {
            title: 'object?',
            category: 'Predicate',
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
                'object?(object("a", 10))',
                'object?(42)',
                'object?(10.1)',
                'object?((x, y) -> x + y)',
                'object?(#"^start")',
                'object?("false")',
                'object?([1, 2, 3])',
            ],
        },
        'coll?': {
            title: 'coll?',
            category: 'Predicate',
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
                'coll?([])',
                'coll?([1, 2, 3])',
                'coll?(object("a", 10))',
                'coll?("Albert")',
                'coll?(42)',
                'coll?(10.1)',
                'coll?((x, y) -> x + y)',
            ],
        },
        'seq?': {
            title: 'seq?',
            category: 'Predicate',
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
                'seq?([])',
                'seq?([1, 2, 3])',
                'seq?(object("a", 10))',
                'seq?("Albert")',
                'seq?(42)',
                'seq?(10.1)',
                'seq?((x, y) -> x + y)',
            ],
        },
        'regexp?': {
            title: 'regexp?',
            category: 'Predicate',
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
                'regexp?(regexp("^start"))',
                'regexp?(#"^start")',
                'regexp?(-12)',
                'regexp?({})',
                'regexp?(10.1)',
                'regexp?((x, y) -> x + y)',
                'regexp?(false)',
                'regexp?("false")',
                'regexp?([1, 2, 3])',
            ],
        },
        'zero?': {
            title: 'zero?',
            category: 'Predicate',
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
                'zero?(0)',
                'zero?(-0.0)',
                'zero?(1)',
                'zero?(0.1)',
            ],
        },
        'pos?': {
            title: 'pos?',
            category: 'Predicate',
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
                'pos?(0)',
                'pos?(-0.0)',
                'pos?(1)',
                'pos?(-0.1)',
            ],
        },
        'neg?': {
            title: 'neg?',
            category: 'Predicate',
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
                'neg?(0)',
                'neg?(-0.0)',
                'neg?(1)',
                'neg?(-0.1)',
            ],
        },
        'even?': {
            title: 'even?',
            category: 'Predicate',
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
                'even?(0)',
                'even?(-0.0)',
                'even?(-1)',
                'even?(2.1)',
            ],
        },
        'odd?': {
            title: 'odd?',
            category: 'Predicate',
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
                'odd?(1.0)',
                'odd?(1.001)',
                'odd?(-1)',
                'odd?(2.1)',
            ],
        },
        'finite?': {
            title: 'finite?',
            category: 'Predicate',
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
                'finite?(1.0)',
                'finite?(1 / 0)',
                'finite?(-1 / 0)',
            ],
        },
        'negative-infinity?': {
            title: 'negative-infinity?',
            category: 'Predicate',
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
                'negative-infinity?(1.0)',
                'negative-infinity?(1 / 0)',
                'negative-infinity?(-1 / 0)',
            ],
        },
        'positive-infinity?': {
            title: 'positive-infinity?',
            category: 'Predicate',
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
                'positive-infinity?(1.0)',
                'positive-infinity?(1 / 0)',
                'positive-infinity?(-1 / 0)',
            ],
        },
        'false?': {
            title: 'false?',
            category: 'Predicate',
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
                'false?(false)',
                'false?(true)',
                'false?(1)',
                'false?(0)',
            ],
        },
        'true?': {
            title: 'true?',
            category: 'Predicate',
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
                'true?(false)',
                'true?(true)',
                'true?(1)',
                'true?(0)',
            ],
        },
        'empty?': {
            title: 'empty?',
            category: 'Predicate',
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
            description: 'Returns `true` if $x is empty or `null`, otherwise `false`.',
            examples: [
                'empty?([])',
                'empty?([1, 2, 3])',
                'empty?({})',
                'empty?({ a: 2 })',
                'empty?("")',
                'empty?("Albert")',
                'empty?(null)',
            ],
        },
        'not-empty?': {
            title: 'not-empty?',
            category: 'Predicate',
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
            description: 'Returns `false` if $x is empty or `null`, otherwise `true`.',
            examples: [
                'not-empty?([])',
                'not-empty?([1, 2, 3])',
                'not-empty?({})',
                'not-empty?({ a: 2 })',
                'not-empty?("")',
                'not-empty?("Albert")',
                'not-empty?(null)',
            ],
        },
        'vector?': {
            title: 'vector?',
            category: 'Predicate',
            description: 'Checks if a value is a `vector`. A `vector` is an array of `numbers`.',
            returns: {
                type: 'boolean',
            },
            args: {
                value: {
                    type: 'any',
                    description: 'The value to check.',
                },
            },
            variants: [
                { argumentNames: ['value'] },
            ],
            examples: [
                'vector?(1)',
                'vector?([1, 2, 3])',
                'vector?([1, 2, "3"])',
            ],
        },
        'matrix?': {
            title: 'matrix?',
            category: 'Predicate',
            description: 'Checks if a value is a `matrix`. A `matrix` is an array of arrays of `numbers`.',
            returns: {
                type: 'boolean',
            },
            args: {
                value: {
                    type: 'any',
                    description: 'The value to check.',
                },
            },
            variants: [
                { argumentNames: ['value'] },
            ],
            examples: [
                'matrix?(1)',
                'matrix?([1, 2, 3])',
                'matrix?([[1, 2], [3, 4]])',
                'matrix?([[1, 2], [3, "4"]])',
                'matrix?([[1, 2], [3]])',
            ],
        },
        'grid?': {
            title: 'grid?',
            category: 'Predicate',
            description: 'Checks if a `value` is a `grid`. A `grid` is an `array` of `arrays` where all inner `arrays` have the same length.',
            returns: {
                type: 'boolean',
            },
            args: {
                value: {
                    type: 'any',
                    description: 'The value to check.',
                },
            },
            variants: [
                { argumentNames: ['value'] },
            ],
            examples: [
                'grid?("1")',
                'grid?(["1", 2, 3])',
                'grid?([["1", 2], [3, 4]])',
                'grid?([["1", 2], [3, "4"]])',
                'grid?([["1", 2], [3]])',
            ],
        },
    };

    var regularExpressionReference = {
        'regexp': {
            title: 'regexp',
            category: 'Regular expression',
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
                'regexp("^\\s*(.*)$")',
                '#"^\\s*(.*)$"',
                'regexp("albert", "ig")',
                '#"albert"ig',
            ],
            noOperatorDocumentation: true,
        },
        'match': {
            title: 'match',
            category: 'Regular expression',
            returns: {
                type: 'any',
                array: true,
            },
            args: __assign({}, getOperatorArgs('regexp', 'string')),
            variants: [
                { argumentNames: ['a', 'b'] },
            ],
            description: "Matches $b against regular expression $a.\nIf $b is a string and matches the regular expression, a `match`-array is returned, otherwise `null` is returned.",
            examples: [
                'match("  A string", regexp("^\\\\s*(.*)$"))',
                'match("  A string", #"^\\s*(.*)$")',
                'match("My name is Albert", #"albert"i)',
                'match("My name is Ben", #"albert"i)',
                'match(null, #"albert"i)',
                'match(1, #"albert"i)',
                'match({}, #"albert"i)',
            ],
        },
        'replace': {
            title: 'replace',
            category: 'Regular expression',
            returns: {
                type: 'any',
                array: true,
            },
            args: __assign(__assign({}, getOperatorArgs('string', ['regexp', 'string'])), { x: {
                    type: 'string',
                } }),
            variants: [
                { argumentNames: ['a', 'b', 'x'] },
            ],
            description: 'Returns a new string with first match of regular expression $b replaced by $x.',
            examples: [
                'replace("Duck duck", "u", "i")',
                'replace("Duck duck", #"u", "i")',
                'replace("abcABC", regexp("a", "i"), "-")',
                'replace("abcABC", regexp("a", "gi"), "-")',
                'replace("abcABC", #"a"i, "-")',
                'replace("abcABC", #"a"gi, "-")',
            ],
        },
        'replace-all': {
            title: 'replace-all',
            category: 'Regular expression',
            returns: {
                type: 'any',
                array: true,
            },
            args: __assign(__assign({}, getOperatorArgs('string', ['regexp', 'string'])), { x: {
                    type: 'string',
                } }),
            variants: [
                { argumentNames: ['a', 'b', 'x'] },
            ],
            description: 'Returns a new string with all matches of regular expression $b replaced by $x.',
            examples: [
                'replace-all("Duck duck", "u", "i")',
                'replace-all("Duck duck", regexp("u"), "i")',
                'replace-all("abcABC", regexp("a", "i"), "-")',
                'replace-all("abcABC", regexp("a", "gi"), "-")',
                'replace-all("abcABC", #"a"i, "-")',
                'replace-all("abcABC", #"a"gi, "-")',
            ],
        },
    };

    var sequenceReference = {
        'nth': {
            title: 'nth',
            category: 'Sequence',
            returns: {
                type: 'any',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'integer')), { 'seq': {
                    type: ['sequence', 'null'],
                }, 'n': {
                    type: 'integer',
                }, 'not-found': {
                    type: 'any',
                } }),
            variants: [
                { argumentNames: ['seq', 'n'] },
                { argumentNames: ['seq', 'n', 'not-found'] },
            ],
            description: 'Accesses element $n of $seq. Accessing out-of-bounds indices returns $not-found, if present, else `null`.',
            examples: [
                '[1, 2, 3] nth 1',
                '"A string" nth 3',
                'nth([1, 2, 3], 1)',
                'nth([1, 2, 3], 3)',
                'nth([1, 2, 3], -1)',
                'nth([1, 2, 3], 3, 99)',
                'nth("A string", 1)',
                'nth("A string", 3)',
                'nth("A string", -3)',
                'nth("A string", 30, "X")',
                'nth(null, 1)',
                'nth(null, 1, "Default value")',
            ],
        },
        'push': {
            title: 'push',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'any')), { seq: {
                    type: 'sequence',
                }, values: {
                    type: 'any',
                    rest: true,
                    description: 'At least one.',
                } }),
            variants: [
                { argumentNames: ['seq', 'values'] },
            ],
            description: 'Returns copy of $seq with $values added to the end of it.',
            examples: [
                '[1, 2, 3] push 4',
                '"Albert" push "!"',
                'push([1, 2, 3], 4)',
                'push([1, 2, 3], 4, 5, 6)',
                "\nlet l = [1, 2, 3];\npush(l, 4);\nl",
            ],
        },
        'pop': {
            title: 'pop',
            category: 'Sequence',
            returns: {
                type: ['sequence', 'null'],
                rest: true,
            },
            args: {
                seq: {
                    type: 'sequence',
                },
            },
            variants: [
                { argumentNames: ['seq'] },
            ],
            description: 'Returns a copy of $seq with last element removed. If $seq is empty `null` is returned.',
            examples: [
                'pop([1, 2, 3])',
                'pop([])',
            ],
        },
        'unshift': {
            title: 'unshift',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'any')), { seq: {
                    type: 'sequence',
                }, values: {
                    type: 'any',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['seq', 'values'] },
            ],
            description: 'Returns copy of $seq with $values added to the beginning.',
            examples: [
                '[1, 2, 3] unshift 4',
                'unshift([1, 2, 3], 4)',
                'unshift([1, 2, 3], 4, 5, 6)',
                "\nlet l = [1, 2, 3];\nunshift(l, 4);\nl",
            ],
        },
        'shift': {
            title: 'shift',
            category: 'Sequence',
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
            description: 'Returns a copy of $seq with first element removed. If $seq is empty `null` is returned.',
            examples: [
                'shift([1, 2, 3])',
                'shift([])',
            ],
        },
        'slice': {
            title: 'slice',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'integer')), { seq: {
                    type: 'sequence',
                    rest: true,
                }, start: {
                    type: 'integer',
                    description: 'Defaults to `0`.',
                }, stop: {
                    type: 'integer',
                    description: 'Defaults lenght of sequence + 1.',
                } }),
            variants: [
                { argumentNames: ['seq'] },
                { argumentNames: ['seq', 'start'] },
                { argumentNames: ['seq', 'start', 'stop'] },
            ],
            description: 'Returns a copy of a portion of $seq from index $start (inclusive) to $stop (exclusive).',
            examples: [
                '[1, 2, 3, 4, 5] slice 2',
                'slice([1, 2, 3, 4, 5], 2, 4)',
                'slice([1, 2, 3, 4, 5], 2)',
            ],
        },
        'splice': {
            title: 'splice',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: {
                seq: {
                    type: 'sequence',
                    rest: true,
                },
                start: {
                    type: 'integer',
                },
                deleteCount: {
                    type: 'integer',
                },
                items: {
                    type: 'any',
                    rest: true,
                },
            },
            variants: [
                { argumentNames: ['seq', 'start', 'deleteCount'] },
                { argumentNames: ['seq', 'start', 'deleteCount', 'items'] },
            ],
            description: 'Returns a a spliced array. Removes $deleteCount elements from $seq starting at $start and replaces them with $items. If $start is negative, it is counting from the end of the array.',
            examples: [
                'splice([1, 2, 3, 4, 5], 2, 2, "x")',
                'splice([1, 2, 3, 4, 5], -2, 1, "x")',
                'splice("Albert", 2, 2, "fo")',
            ],
        },
        'position': {
            title: 'position',
            category: 'Sequence',
            returns: {
                type: ['number', 'null'],
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'function')), { seq: {
                    type: ['sequence', 'null'],
                }, fun: {
                    type: 'function',
                } }),
            variants: [
                { argumentNames: ['seq', 'fun'] },
            ],
            description: 'Returns the index of the first elements that passes the test implemented by $fun. If no element was found, `null` is returned.',
            examples: [
                "\nposition(\n  [\"Albert\", \"Mojir\", 160, [1, 2]],\n  string?\n)",
                "\nposition(\n  [5, 10, 15, 20],\n  -> $ > 10\n)",
                "\nposition(\n  [5, 10, 15, 20],\n  -> $ > 100\n)",
                "\nposition(\n  null,\n  -> $ > 100\n)",
            ],
        },
        'index-of': {
            title: 'index-of',
            category: 'Sequence',
            returns: {
                type: ['number', 'null'],
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'any')), { seq: {
                    type: ['sequence', 'null'],
                }, x: {
                    type: 'any',
                } }),
            variants: [
                { argumentNames: ['seq', 'x'] },
            ],
            description: 'Returns the index of $x in $seq. If element is not present in $seq `null` is returned.',
            examples: [
                '[[1], [2], [1], [2]] index-of [1]',
                'index-of(["Albert", "Mojir", 160, [1, 2]], "Mojir")',
                'index-of([5, 10, 15, 20], 15)',
                'index-of([5, 10, 15, 20], 1)',
                'index-of(null, 1)',
            ],
        },
        'last-index-of': {
            title: 'last-index-of',
            category: 'Sequence',
            returns: {
                type: ['number', 'null'],
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'any')), { seq: {
                    type: ['sequence', 'null'],
                }, x: {
                    type: 'any',
                } }),
            variants: [
                { argumentNames: ['seq', 'x'] },
            ],
            description: 'Returns the last index of $x in $seq. If element is not present in $seq `null` is returned.',
            examples: [
                '[[1], [2], [1], [2]] last-index-of [1]',
                'last-index-of(["Albert", "Mojir", 160, [1, 2]], "Mojir")',
                'last-index-of([5, 10, 15, 20, 15], 15)',
                'last-index-of([5, 10, 15, 20], 1)',
                'last-index-of(null, 1)',
            ],
        },
        'some': {
            title: 'some',
            category: 'Sequence',
            returns: {
                type: 'any',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'function')), { seq: {
                    type: ['sequence', 'null'],
                }, fun: {
                    type: 'function',
                } }),
            variants: [
                { argumentNames: ['seq', 'fun'] },
            ],
            description: 'Returns the first element that passes the test implemented by $fun. I no element was found, `null` is returned.',
            examples: [
                "\nsome(\n  [\"Albert\", \"Mojir\", 160, [1, 2]],\n  string?\n)",
                "\nsome(\n  [5, 10, 15, 20],\n  -> $ > 10\n)",
                "\nsome(\n  [1, 2, 3, 4],\n  -> $ > 10\n)",
                "\nsome(\n  [],\n  -> $ > 10\n)",
                "\nsome(\n  null,\n  -> $ > 10\n)",
            ],
        },
        'reverse': {
            title: 'reverse',
            category: 'Sequence',
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
                'reverse(["Albert", "Mojir", 160, [1, 2]])',
                'reverse([])',
                'reverse("Albert")',
                'reverse(null)',
            ],
        },
        'first': {
            title: 'first',
            category: 'Sequence',
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
            description: 'Returns the first element of $seq. If $seq is empty or `null`, `null` is returned.',
            examples: [
                'first(["Albert", "Mojir", 160, [1, 2]])',
                'first([])',
                'first(null)',
            ],
        },
        'second': {
            title: 'second',
            category: 'Sequence',
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
            description: 'Returns the second element of $seq. If $seq has less than two elements or is `null`, `null` is returned.',
            examples: [
                'second(["Albert", "Mojir", 160, [1, 2]])',
                'second([1])',
                'second([])',
                'second(null)',
            ],
        },
        'last': {
            title: 'last',
            category: 'Sequence',
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
            description: 'Returns the last element of $seq. If $seq is empty, `null` is returned.',
            examples: [
                'last(["Albert", "Mojir", 160, [1, 2]])',
                'last([1, 2])',
                'last([1])',
                'last([])',
                'last(null)',
            ],
        },
        'rest': {
            title: 'rest',
            category: 'Sequence',
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
                'rest(["Albert", "Mojir", 160, [1, 2]])',
                'rest(["Albert"])',
                'rest([])',
                'rest("Albert")',
                'rest("A",)',
                'rest("")',
            ],
        },
        'next': {
            title: 'next',
            category: 'Sequence',
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
            description: 'If $seq is an array, returns a new array with all but the first element from $seq. If $seq has less than two elements, `null` is returned. For string $seq returns all but the first characters in $seq. If length of string $seq is less than two, `null` is returned.',
            examples: [
                'next(["Albert", "Mojir", 160, [1, 2]])',
                'next(["Albert"])',
                'next([])',
                'next("Albert")',
                'next("A",)',
                'next("")',
            ],
        },
        'take': {
            title: 'take',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'integer')), { n: {
                    type: 'integer',
                }, seq: {
                    type: 'sequence',
                } }),
            variants: [
                { argumentNames: ['seq', 'n'] },
            ],
            description: 'Constructs a new array/string with the $n first elements from $seq.',
            examples: [
                '[1, 2, 3, 4, 5] take 3',
                'take([1, 2, 3, 4, 5], 3)',
                'take([1, 2, 3, 4, 5], 0)',
                'take("Albert", 2)',
                'take("Albert", 50)',
            ],
        },
        'take-last': {
            title: 'take-last',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'integer')), { n: {
                    type: 'integer',
                }, seq: {
                    type: 'sequence',
                } }),
            variants: [
                { argumentNames: ['n', 'seq'] },
            ],
            description: 'Constructs a new array with the $n last elements from $seq.',
            examples: [
                '[1, 2, 3, 4, 5] take-last 3',
                'take-last([1, 2, 3, 4, 5], 3)',
                'take-last([1, 2, 3, 4, 5], 0)',
            ],
        },
        'take-while': {
            title: 'take-while',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'function')), { seq: {
                    type: 'sequence',
                }, fun: {
                    type: 'function',
                } }),
            variants: [
                { argumentNames: ['seq', 'fun'] },
            ],
            description: 'Returns the members of $seq in order, stopping before the first one for which `predicate` returns a falsy value.',
            examples: [
                "\ntake-while(\n  [1, 2, 3, 2, 1],\n  -> $ < 3\n)",
                "\ntake-while(\n  [1, 2, 3, 2, 1],\n  -> $ > 3\n)",
            ],
        },
        'drop': {
            title: 'drop',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'integer')), { seq: {
                    type: 'sequence',
                }, n: {
                    type: 'integer',
                } }),
            variants: [
                { argumentNames: ['seq', 'n'] },
            ],
            description: 'Constructs a new array/string with the $n first elements dropped from $seq.',
            examples: [
                'drop([1, 2, 3, 4, 5], 3)',
                'drop([1, 2, 3, 4, 5], 0)',
                'drop("Albert", 2)',
                'drop("Albert", 50)',
            ],
        },
        'drop-last': {
            title: 'drop-last',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'integer')), { seq: {
                    type: 'sequence',
                }, n: {
                    type: 'integer',
                } }),
            variants: [
                { argumentNames: ['seq', 'n'] },
            ],
            description: 'Constructs a new array with the $n last elements dropped from $seq.',
            examples: [
                '[1, 2, 3, 4, 5] drop-last 3',
                'drop-last([1, 2, 3, 4, 5], 3)',
                'drop-last([1, 2, 3, 4, 5], 0)',
            ],
        },
        'drop-while': {
            title: 'drop-while',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'function')), { seq: {
                    type: 'sequence',
                }, fun: {
                    type: 'function',
                } }),
            variants: [
                { argumentNames: ['seq', 'fun'] },
            ],
            description: 'Returns the members of $seq in order, skipping the fist elements for witch the `predicate` returns a truethy value.',
            examples: [
                "\ndrop-while(\n  [1, 2, 3, 2, 1],\n  -> $ < 3\n)",
                "\ndrop-while(\n  [1, 2, 3, 2, 1],\n  -> $ > 3\n)",
            ],
        },
        'sort': {
            title: 'sort',
            category: 'Sequence',
            returns: {
                type: 'any',
                rest: true,
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'function')), { seq: {
                    type: 'sequence',
                }, fun: {
                    type: 'function',
                } }),
            variants: [
                { argumentNames: ['seq'] },
                { argumentNames: ['seq', 'fun'] },
            ],
            description: 'Returns a new sequence with the elements from $seq sorted according to $fun. If no $fun is supplied, builtin `compare` will be used.',
            examples: [
                '[3, 1, 2] sort (a, b) -> b - a',
                'sort([3, 1, 2])',
                "\nsort(\n  [3, 1, 2],\n  (a, b) -> cond case a < b then -1 case a > b then 1 case true then -1 end\n)",
                "\nsort(\n  [3, 1, 2],\n  (a, b) -> cond case a > b then -1 case a < b then 1 case true then -1 end\n)",
            ],
        },
        'sort-by': {
            title: 'sort-by',
            category: 'Sequence',
            returns: {
                type: 'any',
                rest: true,
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'function')), { seq: {
                    type: 'sequence',
                }, keyfn: {
                    type: 'function',
                }, comparer: {
                    type: 'function',
                } }),
            variants: [
                { argumentNames: ['seq', 'keyfn'] },
                { argumentNames: ['seq', 'keyfn', 'comparer'] },
            ],
            description: 'Returns a sorted sequence of the items in $seq, where the sort order is determined by comparing `(keyfn item)`. If no $comparer is supplied, uses builtin `compare`.',
            examples: [
                '["Albert", "Mojir", "Nina"] sort-by count',
                'sort-by(["Albert", "Mojir", "Nina"], count)',
                'sort-by("Albert", lower-case, -> $2 compare $1)',
            ],
        },
        'distinct': {
            title: 'distinct',
            category: 'Sequence',
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
                'distinct([[1], [2], [3], [1], [3], [5]])',
                'distinct([1, 2, 3, 1, 3, 5])',
                'distinct("Albert Mojir")',
                'distinct([])',
                'distinct("")',
            ],
        },
        'remove': {
            title: 'remove',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'function')), { seq: {
                    type: 'sequence',
                }, fun: {
                    type: 'function',
                } }),
            variants: [
                { argumentNames: ['seq', 'fun'] },
            ],
            description: 'Returns a new sequence of items in $seq for witch `pred(item)` returns a falsy value.',
            examples: [
                '[1, 2, 3, 1, 3, 5] remove odd?',
                'remove([1, 2, 3, 1, 3, 5], even?)',
                'remove("Albert Mojir", -> "aoueiyAOUEIY" contains? $)',
            ],
        },
        'remove-at': {
            title: 'remove-at',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'integer')), { seq: {
                    type: 'sequence',
                }, n: {
                    type: 'number',
                } }),
            variants: [
                { argumentNames: ['seq', 'n'] },
            ],
            description: 'Returns a new sequence of all items in $seq except item at position $n. If $n is negative, it is counting from the end of the sequence.',
            examples: [
                '[1, 2, 3, 1, 3, 5] remove-at 2',
                '"Albert" remove-at -2',
                'remove-at([1, 2, 3, 1, 3, 5], 0)',
                'remove-at([1, 2, 3, 1, 3, 5], -1)',
                'remove-at("Albert Mojir", 6)',
            ],
        },
        'split-at': {
            title: 'split-at',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'integer')), { seq: {
                    type: 'sequence',
                }, n: {
                    type: 'number',
                } }),
            variants: [
                { argumentNames: ['seq', 'n'] },
            ],
            description: 'Returns a pair of sequence `[take(pos input), drop(pos input)]`.',
            examples: [
                '[1, 2, 3, 4, 5] split-at 2',
                '"Albert" split-at -2',
                'split-at([1, 2, 3, 4, 5], -2)',
                'split-at("Albert", 2)',
            ],
        },
        'split-with': {
            title: 'split-with',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'function')), { seq: {
                    type: 'sequence',
                }, fun: {
                    type: 'function',
                } }),
            variants: [
                { argumentNames: ['seq', 'fun'] },
            ],
            description: 'Returns a pair of sequences `[take-while(input, fun), drop-while(input, fun)]`.',
            examples: [
                '[1, 2, 3, 4, 5] split-with odd?',
                'split-with([1, 2, 3, 4, 5], -> $ > 3)',
                'split-with("Albert", -> $ <= "o")',
            ],
        },
        'frequencies': {
            title: 'frequencies',
            category: 'Sequence',
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
                'frequencies(["Albert", "Mojir", "Nina", "Mojir"])',
                'frequencies("Pneumonoultramicroscopicsilicovolcanoconiosis")',
            ],
        },
        'group-by': {
            title: 'group-by',
            category: 'Sequence',
            returns: {
                type: 'object',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'function')), { seq: {
                    type: 'sequence',
                }, fun: {
                    type: 'function',
                } }),
            variants: [
                { argumentNames: ['seq', 'fun'] },
            ],
            description: 'Returns an object of the elements of $seq keyed by the result of $fun on each element. The value at each key will be an array of the corresponding elements.',
            examples: [
                '[{ name: "Albert" }, { name: "Albert" }, { name: "Mojir" }] group-by "name"',
                'group-by([{name: "Albert"}, {name: "Albert"}, {name: "Mojir"}], "name")',
                'group-by("Albert Mojir", -> "aoueiAOUEI" contains? $ ? "vowel" : "other")',
            ],
        },
        'partition': {
            title: 'partition',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'number')), { seq: {
                    type: 'sequence',
                }, n: {
                    type: 'number',
                }, step: {
                    type: 'number',
                }, pad: {
                    type: 'array',
                } }),
            variants: [
                { argumentNames: ['seq', 'n'] },
                { argumentNames: ['seq', 'n', 'step'] },
                { argumentNames: ['seq', 'n', 'step', 'pad'] },
            ],
            description: 'Returns an array of sequences of $n items each, at offsets $step apart. If $step is not supplied, defaults to $n. If a $pad array is supplied, use its elements as necessary to complete last partition upto $n items. In case there are not enough padding elements, return a partition with less than $n items.',
            examples: [
                'range(20) partition 4',
                'partition(range(20), 4)',
                'partition(range(22), 4)',
                'partition(range(20), 4, 6)',
                'partition(range(20), 4, 3)',
                'partition(range(20), 3, 6, ["a"])',
                'partition(range(20), 4, 6, ["a"])',
                'partition(range(20), 4, 6, ["a", "b", "c", "d"])',
                'partition(["a", "b", "c", "d", "e", "f"], 3, 1)',
                'partition([1, 2, 3, 4], 10)',
                'partition([1, 2, 3, 4], 10, 10)',
                'partition([1, 2, 3, 4], 10, 10, [])',
                'partition([1, 2, 3, 4], 10, 10, null)',
                'partition("superfragilistic", 5)',
                'partition("superfragilistic", 5, 5, null)',
                'let foo = [5, 6, 7, 8]; partition(foo, 2, 1, foo)',
            ],
        },
        'partition-all': {
            title: 'partition-all',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'number')), { seq: {
                    type: 'sequence',
                }, n: {
                    type: 'number',
                }, step: {
                    type: 'number',
                } }),
            variants: [
                { argumentNames: ['seq', 'n'] },
                { argumentNames: ['seq', 'n', 'step'] },
            ],
            description: 'Returns an array of sequences like partition, but may include partitions with fewer than n items at the end.',
            examples: [
                '[0, 1, 2, 3, 4, 5, 6, 7, 8, 9] partition-all 4',
                'partition-all([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 4)',
                'partition([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 4)',
                'partition-all([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 2, 4)',
            ],
        },
        'partition-by': {
            title: 'partition-by',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'function')), { seq: {
                    type: 'sequence',
                }, fun: {
                    type: 'function',
                } }),
            variants: [
                { argumentNames: ['seq', 'fun'] },
            ],
            description: 'Applies $fun to each value in $seq, splitting it each time $fun returns a new value. Returns an array of sequences.',
            examples: [
                '[1, 2, 3, 4, 5] partition-by odd?',
                'partition-by([1, 2, 3, 4, 5], -> $ == 3)',
                'partition-by([1, 1, 1, 2, 2, 3, 3], odd?)',
                'partition-by("Leeeeeerrroyyy", identity)',
            ],
        },
        'starts-with?': {
            title: 'starts-with?',
            category: 'Sequence',
            returns: {
                type: 'boolean',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'sequence')), { seq: {
                    type: 'sequence',
                }, prefix: {
                    type: 'sequence',
                } }),
            variants: [
                { argumentNames: ['seq', 'prefix'] },
            ],
            description: 'Returns `true` if $seq starts with $prefix, otherwise `false`.',
            examples: [
                '[[1], [2], [3], [4], [5]] starts-with? [1]',
                'starts-with?([1, 2, 3, 4, 5], 1)',
                'starts-with?([1, 2, 3, 4, 5], [1])',
                'starts-with?("Albert", "Al")',
                'starts-with?("Albert", "al")',
            ],
        },
        'ends-with?': {
            title: 'ends-with?',
            category: 'Sequence',
            returns: {
                type: 'boolean',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'sequence')), { seq: {
                    type: 'sequence',
                }, suffix: {
                    type: 'sequence',
                } }),
            variants: [
                { argumentNames: ['seq', 'suffix'] },
            ],
            description: 'Returns `true` if $seq ends with $suffix, otherwise `false`.',
            examples: [
                '[[1], [2], [3], [4], [5]] starts-with? [5]',
                '[[1], [2], [3], [4], [5]] starts-with? 5',
                'ends-with?([1, 2, 3, 4, 5], 5)',
                'ends-with?([1, 2, 3, 4, 5], [5])',
                'ends-with?("Albert", "rt")',
                'ends-with?("Albert", "RT")',
            ],
        },
        'interleave': {
            title: 'interleave',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'sequence')), { seqs: {
                    type: 'sequence',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['seqs'] },
            ],
            description: 'Returns a sequence of the first item from each of the $seqs, then the second item from each of the $seqs, until all items from the shortest seq are exhausted.',
            examples: [
                '[1, 2, 3] interleave [4, 5, 6]',
                '"Albert" interleave ".,.,.,"',
                'interleave([1, 2, 3], [4, 5, 6])',
                'interleave([1, 2, 3], [4, 5, 6], [7, 8, 9])',
                'interleave([1, 2, 3], [4, 5, 6], [7, 8])',
                'interleave([1, 2, 3], [4, 5, 6], [7])',
                'interleave([1, 2, 3], [4, 5, 6], [])',
                'interleave([1, 2, 3], [])',
                'interleave([])',
            ],
        },
        'interpose': {
            title: 'interpose',
            category: 'Sequence',
            returns: {
                type: 'sequence',
            },
            args: __assign(__assign({}, getOperatorArgs('sequence', 'any')), { seq: {
                    type: 'sequence',
                }, separator: {
                    type: 'any',
                } }),
            variants: [
                { argumentNames: ['seq', 'separator'] },
            ],
            description: 'Returns a sequence of the elements of $seq separated by $separator. If $seq is a string, the separator must be a string.',
            examples: [
                '"Albert" interpose "-"',
                'interpose([1, 2, 3, 4, 5], "a")',
                'interpose(["Albert", "Mojir", "Nina"], ", ")',
                'interpose("Albert", ".")',
            ],
        },
    };

    var specialExpressionsReference = {
        'doseq': {
            title: 'doseq',
            category: 'Special expression',
            customVariants: ['doseq (...binding) -> body'],
            details: [
                ['binding', 'loop-var in collection [...let-binding] [where whereExpr] [while whileExp]', 'A doseq loop binding'],
                ['loop-var', 'symbol', 'The name of the loop variable.'],
                ['collection', 'any', 'The collection to iterate over.'],
                ['let-binding', 'let binding', 'A let binding to create a local variable.'],
                ['whereExpr', 'expression', 'An expression that must evaluate to truthy for the loop body to be executed.'],
                ['whileExp', 'expression', 'An expression that must evaluate to truthy for the loop to continue.'],
                ['body', 'expressions', 'The expressions to evaluate for each iteration of the loop.'],
            ],
            returns: {
                type: 'null',
            },
            description: 'Iterates over `bindings`, evaluates `body` for each `binding` and returns `null`. This is useful for side effects.',
            examples: [
                "\ndoseq (i in [1, 2, 3]) -> write!(i * 2)\n      ",
            ],
        },
        'for': {
            title: 'for',
            category: 'Special expression',
            customVariants: ['for (...binding) -> body'],
            details: [
                ['binding', 'loop-var in collection [...let-binding] [where whereExpr] [while whileExp]', 'A for loop binding'],
                ['loop-var', 'symbol', 'The name of the loop variable.'],
                ['collection', 'any', 'The collection to iterate over.'],
                ['let-binding', 'let binding', 'A let binding to create a local variable.'],
                ['whereExpr', 'expression', 'An expression that must evaluate to truthy for the loop body to be executed.'],
                ['whileExp', 'expression', 'An expression that must evaluate to truthy for the loop to continue.'],
                ['body', 'expressions', 'The expressions to evaluate for each iteration of the loop.'],
            ],
            returns: {
                type: 'any',
                array: true,
            },
            description: 'Iterates over `bindings`, evaluates `body` for each `binding` and returns an `array` of results.',
            examples: [
                "\nfor (i in [1, 2, 3]) -> i * 2\n      ",
                "\nfor (\n  i in range(10) let ii = i ^ 2 while ii < 40 when ii % 3 == 0,\n  j in range(10) when j % 2 == 1\n) -> ii + j\n      ",
            ],
        },
        'array': {
            title: 'array',
            category: 'Special expression',
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
                'array(1, 2, 3)',
                'array(array(null, false, true))',
                '[]',
                '[1, 2, 3]',
                '[1, 2, ...[3, 4, 5], 6]',
                '[[null, false, true]]',
                '[1, 2, 3][1]',
            ],
            noOperatorDocumentation: true,
        },
        'object': {
            title: 'object',
            category: 'Special expression',
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
                'object()',
                "\nlet default = {\n  type: \"Person\",\n  name: \"John Doe\",\n  age: 42\n};\n\n{\n  ...default,\n  name: \"Lisa\"\n}",
                'object("x", 10, "y", true, "z", "A string")',
                '{}',
                '{ a: 1, b: 2 }',
            ],
            noOperatorDocumentation: true,
        },
        '&&': {
            title: '&&',
            category: 'Special expression',
            returns: {
                type: 'any',
            },
            args: __assign(__assign({}, getOperatorArgs('any', 'any')), { c: {
                    type: 'any',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'c'] },
            ],
            description: "\nComputes logical `and`. Evaluation of expressions starts from left.\nAs soon as an `expression` evaluates to a falsy value, the result is returned.\n\nIf all expressions evaluate to truthy values, the value of the last expression is returned.",
            examples: [
                'true && 1',
                '&&(1, 1)',
                '&&(3 > 2, "string")',
                '&&(3 < 2, "string")',
                '&&(true, true, true, true)',
                '&&(true, true, 0, true)',
            ],
        },
        '||': {
            title: '||',
            category: 'Special expression',
            returns: {
                type: 'boolean',
            },
            args: __assign(__assign({}, getOperatorArgs('any', 'any')), { c: {
                    type: 'any',
                    rest: true,
                } }),
            variants: [
                { argumentNames: ['a', 'b'] },
                { argumentNames: ['a', 'b', 'c'] },
            ],
            description: "\n  Computes logical `or`. Evaluation of expressions evaluation starts from left.\n  As soon as a `expression` evaluates to a truthy value, the result is returned.\n\n  If all expressions evaluate to falsy values, the value of the last expression is returned.",
            examples: [
                'false || 1',
                '||(1, 1)',
                '||(3 > 2, "string")',
                '||(3 < 2, "string")',
                '||(false, false, false, true)',
                '||(1, 2, 3, 4)',
            ],
        },
        'let': {
            title: 'let',
            category: 'Special expression',
            customVariants: ['let s = value;'],
            details: [
                ['s', 'symbol', 'The name of the variable to bind.'],
                ['value', 'any', 'The value to bind to the variable.'],
            ],
            description: "\n  Binds local variables s to `value`. `value` can be any expression. The scope of the variables is the body of the let expression.",
            examples: ["\nlet a = 1 + 2 + 3 + 4;\nlet b = -> $ * ( $ + 1 );\nwrite!(\"a\", a, \"b\", b)"],
        },
        'try': {
            title: 'try',
            category: 'Special expression',
            customVariants: ['try { try-body } catch { catch-body }', 'try { try-body } catch(error) { catch-body }'],
            details: [
                ['try-body', 'expressions', 'The expressions to try.'],
                ['error', 'symbol', 'The error variable to bind.'],
                ['catch-body', 'expression', 'The expressions to evaluate if the try-body throws an error.'],
            ],
            description: 'Executes `try-body`. If that throws, the `catch-body` gets executed. See examples for details.',
            examples: [
                "\ntry\n  2 / 4\ncatch\n  \"Oops!\"\nend",
                "\ntry\n  foo()\ncatch(error)\n  \"Error: \" ++ error.message\nend",
                "\ntry\n  foo()\ncatch\n  42\nend",
            ],
        },
        'throw': {
            title: 'throw',
            category: 'Special expression',
            returns: {
                type: 'never',
            },
            args: {
                expr: {
                    type: 'any',
                },
            },
            variants: [
                { argumentNames: ['expr'] },
            ],
            description: 'Throws `UserDefinedError` with message set to $expr evaluated. $expr must evaluate to a string.',
            examples: [
                'try throw("You shall not pass!") catch(error) "Error: " ++ error.message end',
                'try throw(slice("You shall not pass!", 0, 3)) catch(error) "Error: " ++ error.message end',
            ],
        },
        'if': {
            title: 'if',
            category: 'Special expression',
            customVariants: ['if test then true-expr else false-expr', 'if test then true-expr'],
            details: [
                ['test', 'expression', 'The condition to test.'],
                ['true-expr', 'expression', 'The expression to evaluate if the test is truthy.'],
                ['false-expr', 'expression', 'The expression to evaluate if the test is falsy.'],
            ],
            description: 'Either `true-expr` or `false-expr` branch is taken. `true-expr` is selected when $test is truthy. If $test is falsy `false-expr` is executed, if no `false-expr` exists, `null` is returned.',
            examples: [
                "\nif true then\n  write!(\"TRUE\")\nelse\n  write!(\"FALSE\")\nend",
                'if false then write!("TRUE") else write!("FALSE") end',
                'if true then write!("TRUE") end',
                'if false then write!("TRUE") end',
            ],
        },
        'unless': {
            title: 'unless',
            category: 'Special expression',
            customVariants: ['unless test then true-expr else false-expr end', 'unless test true-expr end'],
            details: [
                ['test', 'expression', 'The condition to test.'],
                ['true-expr', 'expression', 'The expressions to evaluate if the test is falsy.'],
                ['false-expr', 'expression', 'The expressions to evaluate if the test is truthy.'],
            ],
            description: 'Either `true-expr` or `false-expr` branch is taken. `true-expr` is selected when $test is falsy. If $test is truthy `false-expr` is executed, if no `false-expr` exists, `null` is returned.',
            examples: [
                "\nunless true then\n  write!(\"TRUE\")\nelse\n  write!(\"FALSE\")\nend",
                'unless false then write!("TRUE") else write!("FALSE") end',
                'unless true then write!("TRUE") end',
                'unless false then write!("TRUE") end',
            ],
        },
        'cond': {
            title: 'cond',
            category: 'Special expression',
            customVariants: ['cond cond-branch cond-branch ... end'],
            details: [
                ['cond-branch', 'case test then body', 'A branch of the cond expression.'],
                ['test', 'expression', 'The condition to test.'],
                ['body', 'expressions', 'The expressions to evaluate if the test is truthy.'],
            ],
            description: 'Used for branching. `cond-branches` are tested sequentially from the top. If no branch is tested truthy, `null` is returned.',
            examples: [
                "\ncond\n  case false then write!(\"FALSE\")\n  case true then write!(\"TRUE\")\nend",
                "\ncond\n  case false then write!(\"FALSE\")\n  case null then write!(\"null\")\nend ?? write!(\"TRUE\")",
                "\ncond\n  case false then write!(\"FALSE\")\n  case null then write!(\"null\")\nend ?? write!(\"TRUE\")",
            ],
        },
        'switch': {
            title: 'switch',
            category: 'Special expression',
            customVariants: ['switch value switch-branch switch-branch ... end'],
            details: [
                ['value', 'any', 'The value to test.'],
                ['switch-branch', 'case test then body', 'A branch of the switch expression.'],
                ['test', 'expression', 'The condition to test.'],
                ['body', 'expressions', 'The expressions to evaluate if the test is truthy.'],
            ],
            description: 'Used for branching. `switch-branches` are tested sequentially from the top against `value`. If no branch is tested truthy, `null` is returned.',
            examples: [
                "\nswitch 1\n  case 1 then write!(\"One\")\n  case 2 then write!(\"Two\")\nend",
                "\nswitch 2\n  case 1 then write!(\"One\")\n  case 2 then write!(\"Two\")\nend",
                "\nswitch 3\n  case 1 then write!(\"One\")\n  case 2 then write!(\"Two\")\nend",
            ],
        },
        'block': {
            title: 'block',
            category: 'Special expression',
            customVariants: ['{ body }'],
            details: [
                ['body', 'expressions', 'The expressions to evaluate.'],
            ],
            description: 'Evaluates `body`. Resulting value is the value of the last expression.',
            examples: [
                "\n{\n  let a = 1 + 2 + 3 + 4;\n  let b = -> $ * ( $ + 1 );\n  b(a)\n}",
            ],
        },
        'recur': {
            title: 'recur',
            category: 'Special expression',
            customVariants: ['recur(...recur-args)'],
            description: 'Recursevly calls enclosing function or loop with its evaluated `recur-args`.',
            examples: [
                "\nlet foo = (n) -> {\n  write!(n);\n  if !(zero?(n)) then\n    recur(n - 1)\n  end\n};\nfoo(3)",
                "\n(n -> {\n  write!(n);\n  if !(zero?(n)) then\n    recur(n - 1)\n  end\n})(3)",
                "\nloop (n = 3) -> {\n  write!(n);\n  if !(zero?(n)) then\n    recur(n - 1)\n  end\n}",
            ],
        },
    };

    var stringReference = {
        'string-repeat': {
            title: 'string-repeat',
            category: 'String',
            returns: {
                type: 'number',
            },
            args: __assign(__assign({}, getOperatorArgs('string', 'integer')), { s: {
                    type: 'string',
                }, n: {
                    type: 'integer',
                } }),
            variants: [
                { argumentNames: ['s', 'n'] },
            ],
            description: 'Repeates $s $n times.',
            examples: [
                '"*" string-repeat 10',
                'string-repeat("*", 10)',
                'string-repeat("***", 0)',
            ],
        },
        'str': {
            title: 'str',
            category: 'String',
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
            description: 'Concatenats $values into one string. If `value` equals `null` empty string is returned.',
            examples: [
                'str("A string", ", and another string", " ...and more")',
                'str("Just one string")',
                'str()',
                'str(0, false, true, null, #"^kalle", [1, 2, 3], {a: "a"})',
            ],
            noOperatorDocumentation: true,
        },
        'number': {
            title: 'number',
            category: 'String',
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
                'number("10")',
                'number("010")',
                'number("-1.01")',
            ],
        },
        'lower-case': {
            title: 'lower-case',
            category: 'String',
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
                'lower-case("Albert")',
                'lower-case("")',
            ],
        },
        'upper-case': {
            title: 'upper-case',
            category: 'String',
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
                'upper-case("Albert")',
                'upper-case("")',
            ],
        },
        'trim': {
            title: 'trim',
            category: 'String',
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
                'trim("  Albert  ")',
                'trim("   ")',
                'trim("")',
            ],
        },
        'trim-left': {
            title: 'trim-left',
            category: 'String',
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
                'trim-left("  Albert  ")',
                'trim-left("   ")',
                'trim-left("")',
            ],
        },
        'trim-right': {
            title: 'trim-right',
            category: 'String',
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
                'trim-right("  Albert  ")',
                'trim-right("   ")',
                'trim-right("")',
            ],
        },
        'pad-left': {
            title: 'pad-left',
            category: 'String',
            returns: {
                type: 'string',
            },
            args: __assign(__assign({}, getOperatorArgs('string', 'integer')), { s: {
                    type: 'string',
                }, length: {
                    type: 'integer',
                }, padString: {
                    type: 'string',
                } }),
            variants: [
                { argumentNames: ['s', 'length'] },
                { argumentNames: ['s', 'length', 'padString'] },
            ],
            description: 'Pads from the start of $s with `padString` (multiple times, if needed) until the resulting string reaches the given $length.',
            examples: [
                '"Albert" pad-left 20',
                'pad-left("Albert", 20)',
                'pad-left("Albert", 20, "-*-")',
                'pad-left("Albert", 5)',
                'pad-left("Albert", -1)',
            ],
        },
        'pad-right': {
            title: 'pad-right',
            category: 'String',
            returns: {
                type: 'string',
            },
            args: __assign(__assign({}, getOperatorArgs('string', 'integer')), { s: {
                    type: 'string',
                }, length: {
                    type: 'integer',
                }, padString: {
                    type: 'string',
                } }),
            variants: [
                { argumentNames: ['s', 'length'] },
                { argumentNames: ['s', 'length', 'padString'] },
            ],
            description: 'Pads from the start of $s with `padString` (multiple times, if needed) until the resulting string reaches the given `length`.',
            examples: [
                '"Albert" pad-right 20',
                'pad-right("Albert", 20)',
                'pad-right("Albert", 20, "-*-")',
                'pad-right("Albert", 5)',
                'pad-right("Albert", -1)',
            ],
        },
        'split': {
            title: 'split',
            category: 'String',
            returns: {
                type: 'string',
            },
            args: __assign(__assign({}, getOperatorArgs('string', 'string')), { s: {
                    type: 'string',
                }, delimiter: {
                    type: 'string',
                }, limit: {
                    type: 'integer',
                } }),
            variants: [
                { argumentNames: ['s', 'delimiter'] },
                { argumentNames: ['s', 'delimiter', 'limit'] },
            ],
            description: 'Divides $s into an array of substrings. The division is done by searching for `delimiter`. If `limit` as provided, at most `limit` number of substrings are returned.',
            examples: [
                '"Albert Mojir" split " "',
                'split("Albert Mojir", " ")',
                'split("abcdefghijklmnopqrstuvw", #"[aoueiy]")',
                'split("0123456789", "")',
                'split("0123456789", "", 5) map number',
            ],
        },
        'split-lines': {
            title: 'split-lines',
            category: 'String',
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
            description: 'Divides $s into an array of substrings, each representing a line.',
            examples: [
                'split-lines("Albert\nMojir\n")',
                'split-lines("Albert\n\nMojir")',
                'split-lines("Albert\nMojir\n\n")',
                'split-lines("")',
            ],
        },
        'template': {
            title: 'template',
            category: 'String',
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
                'template("Hi, $1 and $2", "Carl", "Larry")',
                'template("Hi $1, $2, $3, $4, $5, $6, $7, $8 and $9", "A", "B", "C", "D", "E", "F", "G", "H", "I")',
                'template("$1 book||||$1 books", 0)',
                'template("$1 book||||$1 books", 1)',
                'template("$1 book||||$1 books", 2)',
                'template("No book||||$1 book||||$1 books", 0)',
                'template("No book||||$1 book||||$1 books", 1)',
                'template("No book||||$1 book||||$1 books", 10)',
                'template("No book||||One book||||Two books||||Three books||||$1 books", 0)',
                'template("No book||||One book||||Two books||||Three books||||$1 books", 1)',
                'template("No book||||One book||||Two books||||Three books||||$1 books", 2)',
                'template("No book||||One book||||Two books||||Three books||||$1 books", 3)',
                'template("No book||||One book||||Two books||||Three books||||$1 books", 4)',
            ],
            noOperatorDocumentation: true,
        },
        'to-char-code': {
            title: 'to-char-code',
            category: 'String',
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
                'to-char-code("A")',
                'to-char-code("Albert")',
            ],
        },
        'from-char-code': {
            title: 'from-char-code',
            category: 'String',
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
                'from-char-code(65)',
                'from-char-code(0)',
            ],
        },
        'encode-base64': {
            title: 'encode-base64',
            category: 'String',
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
                'encode-base64("Albert")',
            ],
        },
        'decode-base64': {
            title: 'decode-base64',
            category: 'String',
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
                'decode-base64("QWxiZXJ0IPCfkLs=")',
            ],
        },
        'encode-uri-component': {
            title: 'encode-uri-component',
            category: 'String',
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
                'encode-uri-component("Hi everyone!?")',
            ],
        },
        'decode-uri-component': {
            title: 'decode-uri-component',
            category: 'String',
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
                'decode-uri-component("Hi%20everyone!%3F%20%F0%9F%91%8D")',
            ],
        },
        'join': {
            title: 'join',
            category: 'String',
            returns: {
                type: 'string',
            },
            args: __assign(__assign({}, getOperatorArgs('array', 'string')), { arr: {
                    type: 'array',
                }, delimiter: {
                    type: 'string',
                } }),
            variants: [{
                    argumentNames: ['arr', 'delimiter'],
                }],
            description: 'Returns a new string by concatenating all of the elements in $arr, separated by $delimiter.',
            examples: [
                'map([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], str) join ", "',
                '([0, 1, 2, 3, 4, 5, 6, 7, 8, 9] map str) join ", "',
                'join(["Albert", 10], ", ")',
                'join(["Albert", "Mojir"], " ")',
                'join(map([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], str), ", ")',
            ],
        },
        'capitalize': {
            title: 'capitalize',
            category: 'String',
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
            description: 'Returns $s with the first character converted to uppercase and the rest to lowercase.',
            examples: [
                'capitalize("albert")',
                'capitalize("ALBERT")',
                'capitalize("aLBERT")',
                'capitalize("")',
            ],
        },
        'blank?': {
            title: 'blank?',
            category: 'String',
            returns: {
                type: 'boolean',
            },
            args: {
                s: {
                    type: ['string', 'null'],
                },
            },
            variants: [
                { argumentNames: ['s'] },
            ],
            description: 'Returns true if $s is null or only contains whitespace characters.',
            examples: [
                'blank?("")',
                'blank?(null)',
                'blank?("\n")',
                'blank?(" ")',
                'blank?(".")',
            ],
        },
    };

    var datatype = {
        '-type-number': {
            datatype: true,
            title: 'number',
            category: 'Datatype',
            description: 'A `number`',
            examples: [
                '42',
                '3.14',
            ],
        },
        '-type-string': {
            datatype: true,
            title: 'string',
            category: 'Datatype',
            description: 'A `string`',
            examples: [
                '"hello"',
                '""',
            ],
        },
        '-type-object': {
            datatype: true,
            title: 'object',
            category: 'Datatype',
            description: 'An `object`, a collection of key-value pairs where keys are `strings`',
            examples: [
                '{}',
                '{ a: 1, b: 2}',
            ],
        },
        '-type-array': {
            datatype: true,
            title: 'array',
            category: 'Datatype',
            description: 'An `array`',
            examples: [
                '[]',
                '[1, 2, 3]',
                '["a", null, true]',
            ],
        },
        '-type-vector': {
            datatype: true,
            title: 'vector',
            category: 'Datatype',
            description: 'An `array` of `numbers`',
            examples: [
                '[]',
                '[1, 2, 3]',
            ],
        },
        '-type-matrix': {
            datatype: true,
            title: 'matrix',
            category: 'Datatype',
            description: 'A `matrix`, a two-dimensional `array` with `numbers` where each row has the same number of columns. A `matrix` is also a `grid`.',
            examples: [
                '[[]]',
                '[[1, 2], [3, 4]]',
                '[[1, 2], [3, 4], [5, 6]]',
            ],
        },
        '-type-grid': {
            datatype: true,
            title: 'grid',
            category: 'Datatype',
            description: 'A `grid`, a two-dimensional `array` where each row has the same number of columns',
            examples: [
                '[[]]',
                '[[1, 2], [3, 4]]',
                '[["a", "b"], [3, 4], [5, 6]]',
            ],
        },
        '-type-boolean': {
            datatype: true,
            title: 'boolean',
            category: 'Datatype',
            description: 'A `boolean`',
            examples: [
                'true',
                'false',
            ],
        },
        '-type-function': {
            datatype: true,
            title: 'function',
            category: 'Datatype',
            description: 'A `function`',
            examples: [
                'x -> x + 1',
                '(a, b, c) -> (a + b) * c',
                '() -> 42',
                '-> $1 + $2',
            ],
        },
        '-type-integer': {
            datatype: true,
            title: 'integer',
            category: 'Datatype',
            description: 'An `integer`',
            examples: [
                '42',
                '-42',
            ],
        },
        '-type-any': {
            datatype: true,
            title: 'any',
            category: 'Datatype',
            description: '`Any` value',
            examples: [
                '42',
                '"hello"',
                'true',
                'null',
            ],
        },
        '-type-null': {
            datatype: true,
            title: 'null',
            category: 'Datatype',
            description: 'The value `null`',
            examples: [
                'null',
            ],
        },
        '-type-collection': {
            datatype: true,
            title: 'collection',
            category: 'Datatype',
            description: 'A collection, an `object`, an `array` or a `string`',
            examples: [
                '{ foo: 42 }',
                '[1, 2, 3]',
                '"hello"',
            ],
        },
        '-type-sequence': {
            datatype: true,
            title: 'sequence',
            category: 'Datatype',
            description: 'A sequence, an `array` or a `string`',
            examples: [
                '[1, 2, 3]',
                '"hello"',
            ],
        },
        '-type-regexp': {
            datatype: true,
            title: 'regexp',
            category: 'Datatype',
            description: 'A regular expression',
            examples: [
                'regexp("^\\\\s*(.*)$")',
                '#"albert"ig',
            ],
        },
        '-type-never': {
            datatype: true,
            title: 'never',
            category: 'Datatype',
            description: 'A value that can never be created',
            examples: ["\n// throw(\"error\") will never return a value\ntry throw(\"error\") catch \"never\" end",
            ],
        },
    };

    var shorthand = {
        '-short-regexp': {
            shorthand: true,
            title: '#"pattern"',
            category: 'Shorthand',
            description: 'Shorthand for `regexp(pattern)`. Only difference is that escaping is not needed.',
            examples: [
                '#"^\\s*(.*)$"',
                '#"albert"ig',
            ],
            seeAlso: ['regexp'],
        },
        '-short-fn': {
            shorthand: true,
            title: '-> expression',
            category: 'Shorthand',
            description: "\nShorthand for `(args, ...) -> expression`.\n`$1, $2, $3, ...` are shorthand for the first, second, third, ... argument.\n\nYou can reference the first argument using either `$1` or `$`.\nHowever, please note that `$1` and `$` are mutually exclusive and cannot be used simultaneously.\nE.g. `#(* $ $1)` is not valid.",
            examples: [
                '-> $1 + $2',
                '(-> $ * $)(9)',
            ],
        },
    };

    function isFunctionReference(ref) {
        return 'returns' in ref && 'args' in ref && 'variants' in ref;
    }
    var normalExpressionReference = __assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign(__assign({}, bitwiseReference), collectionReference), arrayReference), sequenceReference), mathReference), functionalReference), metaReference), miscReference), assertReference), objectReference), predicateReference), regularExpressionReference), stringReference);
    Object.entries(normalExpressionReference).forEach(function (_a) {
        var _b = __read(_a, 2), key = _b[0], obj = _b[1];
        if (!normalExpressions[key]) {
            throw new Error("Missing normal expression ".concat(key, " in normalExpressions"));
        }
        var arity = normalExpressions[key].arity;
        if (!obj.noOperatorDocumentation && canBeOperator(arity)) {
            obj._isOperator = true;
            if (isSymbolicOperator(key)) {
                obj._prefereOperator = true;
            }
        }
    });
    Object.entries(specialExpressionsReference).forEach(function (_a) {
        var _b;
        var _c = __read(_a, 2), key = _c[0], obj = _c[1];
        if (isFunctionReference(obj)) {
            var arity = (_b = specialExpressions[specialExpressionTypes[key]]) === null || _b === void 0 ? void 0 : _b.arity;
            if (arity && canBeOperator(arity)) {
                obj._isOperator = true;
            }
        }
    });
    var functionReference = __assign(__assign({}, normalExpressionReference), specialExpressionsReference);
    var apiReference = __assign(__assign(__assign({}, functionReference), shorthand), datatype);
    Object.values(apiReference).forEach(function (ref) {
        ref.title = ref.title.replace(/"/g, '&quot;');
    });

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

    setNormalExpressionReference(normalExpressionReference);
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
            var result = this.evaluate(ast, params);
            // const stringifiedResult = JSON.stringify(result)
            // const parsedResult = JSON.parse(stringifiedResult) as unknown
            // if (!deepEqual(result, parsedResult)) {
            //   throw new Error(`Result is not serializable: ${result} != ${parsedResult}\nstringifiedResult: ${stringifiedResult}\nprogram: ${program}`)
            // }
            return result;
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
        Lits.prototype.getAutoCompleter = function (program, position, params) {
            if (params === void 0) { params = {}; }
            return new AutoCompleter(program, position, this, params);
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
    var autoCompleter = null;
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
        var target = evt.target;
        var start = target.selectionStart;
        var end = target.selectionEnd;
        var indexOfReturn = target.value.lastIndexOf('\n', start - 1);
        var rowLength = start - indexOfReturn - 1;
        var onTabStop = rowLength % 2 === 0;
        if ((!['Shift', 'Control', 'Meta', 'Alt', 'Escape'].includes(evt.key) && evt.code !== 'Space')
            || (evt.code === 'Space' && !evt.altKey)) {
            autoCompleter = null;
        }
        if (evt.code === 'Space' && evt.altKey) {
            evt.preventDefault();
            if (!autoCompleter) {
                autoCompleter = getLits().getAutoCompleter(target.value, start, getLitsParamsFromContext());
            }
            var suggestion = evt.shiftKey ? autoCompleter.getPreviousSuggestion() : autoCompleter.getNextSuggestion();
            if (suggestion) {
                target.value = suggestion.program;
                target.selectionStart = target.selectionEnd = suggestion.position;
                onChange();
            }
            return;
        }
        switch (evt.code) {
            case 'Tab':
                evt.preventDefault();
                if (!evt.shiftKey) {
                    target.value = target.value.substring(0, start) + (onTabStop ? '  ' : ' ') + target.value.substring(end);
                    target.selectionStart = target.selectionEnd = start + (onTabStop ? 2 : 1);
                    onChange();
                }
                break;
            case 'Escape':
                evt.preventDefault();
                if (autoCompleter) {
                    target.value = autoCompleter.originalProgram;
                    target.selectionStart = target.selectionEnd = autoCompleter.originalPosition;
                    autoCompleter = null;
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
        elements.litsCodeTitleString.textContent = 'Lits Code';
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
