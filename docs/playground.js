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
        AstNodeType[AstNodeType["Name"] = 205] = "Name";
        AstNodeType[AstNodeType["Modifier"] = 206] = "Modifier";
        AstNodeType[AstNodeType["ReservedName"] = 207] = "ReservedName";
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
        [AstNodeType.Name, 'Name'],
        [AstNodeType.Modifier, 'Modifier'],
        [AstNodeType.ReservedName, 'ReservedName'],
        [AstNodeType.Binding, 'Binding'],
        [AstNodeType.Argument, 'Argument'],
        [AstNodeType.Partial, 'Partial'],
    ]);
    function isAstNodeType(type) {
        return typeof type === 'number' && astNodeTypeName.has(type);
    }
    var TokenType;
    (function (TokenType) {
        TokenType[TokenType["Bracket"] = 101] = "Bracket";
        TokenType[TokenType["Number"] = 102] = "Number";
        TokenType[TokenType["Name"] = 103] = "Name";
        TokenType[TokenType["String"] = 104] = "String";
        TokenType[TokenType["ReservedName"] = 105] = "ReservedName";
        TokenType[TokenType["Modifier"] = 106] = "Modifier";
        TokenType[TokenType["RegexpShorthand"] = 107] = "RegexpShorthand";
        TokenType[TokenType["FnShorthand"] = 108] = "FnShorthand";
        TokenType[TokenType["CollectionAccessor"] = 109] = "CollectionAccessor";
        TokenType[TokenType["Comment"] = 110] = "Comment";
        TokenType[TokenType["NewLine"] = 111] = "NewLine";
    })(TokenType || (TokenType = {}));
    var tokenTypeName = new Map([
        [TokenType.Bracket, 'Bracket'],
        [TokenType.Number, 'Number'],
        [TokenType.Name, 'Name'],
        [TokenType.String, 'String'],
        [TokenType.ReservedName, 'ReservedName'],
        [TokenType.Modifier, 'Modifier'],
        [TokenType.RegexpShorthand, 'RegexpShorthand'],
        [TokenType.FnShorthand, 'FnShorthand'],
        [TokenType.CollectionAccessor, 'CollectionAccessor'],
        [TokenType.Comment, 'Comment'],
        [TokenType.NewLine, 'NewLine'],
    ]);
    function isTokenType(type) {
        return typeof type === 'number' && tokenTypeName.has(type);
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
    function isToken$1(value) {
        return isUnknownRecord$1(value) && isTokenType(value.t) && typeof value.v === 'string';
    }
    function isAstNode$1(value) {
        return isUnknownRecord$1(value) && isAstNodeType(value.t);
    }
    function valueToString(value) {
        if (isLitsFunction$1(value))
            // eslint-disable-next-line ts/no-unsafe-member-access
            return "<function ".concat(value.name || '\u03BB', ">");
        if (isToken$1(value))
            return "".concat(tokenTypeName.get(value.t), "-token \"").concat(value.v, "\"");
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
            var message = "Expected function, got ".concat(valueToString(fn), ".");
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
    function isNameNode(value) {
        if (!isAstNode(value))
            return false;
        return value.t === AstNodeType.Name;
    }
    function asNameNode(value, sourceCodeInfo) {
        assertNameNode(value, sourceCodeInfo);
        return value;
    }
    function assertNameNode(value, sourceCodeInfo) {
        if (!isNameNode(value))
            throw getAssertionError('NameNode', value, sourceCodeInfo);
    }
    function isNormalExpressionNode(value) {
        if (!isAstNode(value))
            return false;
        return value.t === AstNodeType.NormalExpression;
    }
    function asNormalExpressionNode(value, sourceCodeInfo) {
        assertNormalExpressionNode(value, sourceCodeInfo);
        return value;
    }
    function assertNormalExpressionNode(value, sourceCodeInfo) {
        if (!isNormalExpressionNode(value))
            throw getAssertionError('NormalExpressionNode', value, sourceCodeInfo);
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
        'and': function (node, removeOptions) {
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
        'if-let': function (_node, _removeOptions) { },
        'if': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
        },
        'if-not': function (node, removeOptions) {
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
        'or': function (node, removeOptions) {
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
        'time!': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
        },
        'throw': function (node, removeOptions) {
            removeOptions.removeCommenNodesFromArray(node.p);
            node.p.forEach(removeOptions.recursivelyRemoveCommentNodes);
        },
        'try': function (_node, _removeOptions) { },
        'when-first': function (_node, _removeOptions) { },
        'when-let': function (_node, _removeOptions) { },
        'when': function (_node, _removeOptions) { },
        'when-not': function (_node, _removeOptions) { },
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
        var _a, _b;
        var length = withoutCommentNodes(node.p).length;
        if (length % 2 !== 0) {
            throw new LitsError("Wrong number of arguments, expected an even number, got ".concat(valueToString(length), "."), (_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
        }
    }
    function assertNumberOfParams(count, node) {
        var _a, _b, _c;
        assertCount({
            count: count,
            length: withoutCommentNodes(node.p).length,
            name: (_a = node.n) !== null && _a !== void 0 ? _a : 'expression',
            sourceCodeInfo: (_c = (_b = node.debugData) === null || _b === void 0 ? void 0 : _b.token.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo,
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
    /* v8 ignore next 3 */
    function assertUnreachable(_) {
        throw new Error('This should not be reached');
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
        'bit-shift-left': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], count = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true });
                return num << count;
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'bit-shift-right': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], count = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(count, sourceCodeInfo, { integer: true, nonNegative: true });
                return num >> count;
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'bit-not': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), num = _b[0];
                assertNumber(num, sourceCodeInfo, { integer: true });
                return ~num;
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'bit-and': {
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
        'bit-and-not': {
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
        'bit-or': {
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
        'bit-xor': {
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
        'bit-flip': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], index = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true });
                var mask = 1 << index;
                return (num ^= mask);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'bit-set': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], index = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true });
                var mask = 1 << index;
                return (num |= mask);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'bit-clear': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), num = _b[0], index = _b[1];
                assertNumber(num, sourceCodeInfo, { integer: true });
                assertNumber(index, sourceCodeInfo, { integer: true, nonNegative: true });
                var mask = 1 << index;
                return (num &= ~mask);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'bit-test': {
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
            throw new LitsError(getAssertionError("".concat(options.nonEmpty ? 'non empty string' : options.char ? 'character' : 'string'), value, sourceCodeInfo));
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
        'get-in': {
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
                if (typeof coll === 'string')
                    return typeof value === 'string' ? coll.split('').includes(value) : false;
                return Object.values(coll).includes(value);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'has-some?': {
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
        'has-every?': {
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'every?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), fn = _c[0], coll = _c[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, sourceCodeInfo);
                assertColl(coll, sourceCodeInfo);
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
                var _c = __read(_a, 2), fn = _c[0], coll = _c[1];
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
        'not-any?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), fn = _c[0], coll = _c[1];
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
        'not-every?': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), fn = _c[0], coll = _c[1];
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

    var evaluateMap = function (params, sourceCodeInfo, contextStack, _a) {
        var executeFunction = _a.executeFunction;
        var _b = __read(params, 2), fn = _b[0], firstList = _b[1];
        assertLitsFunction(fn, sourceCodeInfo);
        assertSeq(firstList, sourceCodeInfo);
        var isStringSeq = typeof firstList === 'string';
        var length = firstList.length;
        if (params.length === 2) {
            if (Array.isArray(firstList)) {
                return firstList.map(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
            }
            else {
                return firstList
                    .split('')
                    .map(function (elem) {
                    var newVal = executeFunction(fn, [elem], contextStack, sourceCodeInfo);
                    assertString(newVal, sourceCodeInfo, { char: true });
                    return newVal;
                })
                    .join('');
            }
        }
        else {
            params.slice(2).forEach(function (collParam) {
                if (isStringSeq)
                    assertString(collParam, sourceCodeInfo);
                else
                    assertArray(collParam, sourceCodeInfo);
                if (length !== collParam.length)
                    throw new LitsError('All arguments to "map" must have the same length.', sourceCodeInfo);
            });
            if (isStringSeq) {
                var result = '';
                var _loop_1 = function (i) {
                    var fnParams = params.slice(1).map(function (l) { return l[i]; });
                    var newValue = executeFunction(fn, fnParams, contextStack, sourceCodeInfo);
                    assertString(newValue, sourceCodeInfo, { char: true });
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
                    result.push(executeFunction(fn, fnParams, contextStack, sourceCodeInfo));
                };
                for (var i = 0; i < length; i += 1) {
                    _loop_2(i);
                }
                return result;
            }
        }
    };
    var sequenceNormalExpression = {
        'cons': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), elem = _b[0], seq = _b[1];
                assertAny(elem, sourceCodeInfo);
                assertSeq(seq, sourceCodeInfo);
                if (Array.isArray(seq))
                    return __spreadArray([elem], __read(seq), false);
                assertString(elem, sourceCodeInfo, { char: true });
                return "".concat(elem).concat(seq);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
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
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'filter': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), fn = _c[0], seq = _c[1];
                var executeFunction = _b.executeFunction;
                assertLitsFunction(fn, sourceCodeInfo);
                assertSeq(seq, sourceCodeInfo);
                if (Array.isArray(seq))
                    return seq.filter(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); });
                return seq
                    .split('')
                    .filter(function (elem) { return executeFunction(fn, [elem], contextStack, sourceCodeInfo); })
                    .join('');
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'first': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), array = _b[0];
                if (array === null)
                    return null;
                assertSeq(array, sourceCodeInfo);
                return toAny(array[0]);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'last': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), array = _b[0];
                if (array === null)
                    return null;
                assertSeq(array, sourceCodeInfo);
                return toAny(array[array.length - 1]);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'map': {
            evaluate: evaluateMap,
            validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
        },
        'pop': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), seq = _b[0];
                assertSeq(seq, sourceCodeInfo);
                if (typeof seq === 'string')
                    return seq.substring(0, seq.length - 1);
                var copy = __spreadArray([], __read(seq), false);
                copy.pop();
                return copy;
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'position': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), fn = _c[0], seq = _c[1];
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
                    var index = seq.indexOf(value);
                    return index !== -1 ? index : null;
                }
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
        },
        'reductions': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var fn = params[0];
                assertLitsFunction(fn, sourceCodeInfo);
                if (params.length === 2) {
                    var _b = __read(params, 2), arr = _b[1];
                    assertSeq(arr, sourceCodeInfo);
                    if (arr.length === 0)
                        return [executeFunction(fn, [], contextStack, sourceCodeInfo)];
                    else if (arr.length === 1)
                        return [toAny(arr[0])];
                    if (typeof arr === 'string') {
                        var chars = arr.split('');
                        var resultArray_1 = [asAny(chars[0], sourceCodeInfo)];
                        chars.slice(1).reduce(function (result, elem) {
                            var newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                            resultArray_1.push(newVal);
                            return newVal;
                        }, asAny(chars[0], sourceCodeInfo));
                        return resultArray_1;
                    }
                    else {
                        var resultArray_2 = [toAny(arr[0])];
                        arr.slice(1).reduce(function (result, elem) {
                            var newVal = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                            resultArray_2.push(newVal);
                            return newVal;
                        }, toAny(arr[0]));
                        return resultArray_2;
                    }
                }
                else {
                    var _c = __read(params, 3), val = _c[1], seq = _c[2];
                    assertAny(val, sourceCodeInfo);
                    assertSeq(seq, sourceCodeInfo);
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
        'reduce': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var fn = params[0];
                assertLitsFunction(fn, sourceCodeInfo);
                if (params.length === 2) {
                    var _b = __read(params, 2), arr = _b[1];
                    assertSeq(arr, sourceCodeInfo);
                    if (arr.length === 0)
                        return executeFunction(fn, [], contextStack, sourceCodeInfo);
                    else if (arr.length === 1)
                        return toAny(arr[0]);
                    if (typeof arr === 'string') {
                        var chars = arr.split('');
                        return chars.slice(1).reduce(function (result, elem) {
                            var val = executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                            return val;
                        }, asAny(chars[0], sourceCodeInfo));
                    }
                    else {
                        return arr.slice(1).reduce(function (result, elem) {
                            return executeFunction(fn, [result, elem], contextStack, sourceCodeInfo);
                        }, toAny(arr[0]));
                    }
                }
                else {
                    var _c = __read(params, 3), val = _c[1], seq = _c[2];
                    assertAny(val, sourceCodeInfo);
                    assertSeq(seq, sourceCodeInfo);
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
        'reduce-right': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var fn = params[0];
                assertLitsFunction(fn, sourceCodeInfo);
                if (params.length === 2) {
                    var _b = __read(params, 2), seq = _b[1];
                    assertSeq(seq, sourceCodeInfo);
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
                    var _c = __read(params, 3), val = _c[1], seq = _c[2];
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
        'rest': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertSeq(first, sourceCodeInfo);
                if (Array.isArray(first)) {
                    if (first.length <= 1)
                        return [];
                    return first.slice(1);
                }
                return first.substring(1);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'nthrest': {
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
        'next': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertSeq(first, sourceCodeInfo);
                if (Array.isArray(first)) {
                    if (first.length <= 1)
                        return null;
                    return first.slice(1);
                }
                if (first.length <= 1)
                    return null;
                return first.substring(1);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'nthnext': {
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'second': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), array = _b[0];
                if (array === null)
                    return null;
                assertSeq(array, sourceCodeInfo);
                return toAny(array[1]);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'slice': {
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
        'some': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c;
                var _d = __read(_a, 2), fn = _d[0], seq = _d[1];
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
        'sort': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var defaultComparer = params.length === 1;
                var seq = defaultComparer ? params[0] : params[1];
                var comparer = defaultComparer ? null : params[0];
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
        'sort-by': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var defaultComparer = params.length === 2;
                var keyfn = asAny(params[0], sourceCodeInfo);
                var comparer = defaultComparer ? null : params[1];
                var seq = asSeq(defaultComparer ? params[1] : params[2], sourceCodeInfo);
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
        'take': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), n = _b[0], input = _b[1];
                assertNumber(n, sourceCodeInfo);
                assertSeq(input, sourceCodeInfo);
                var num = Math.max(Math.ceil(n), 0);
                return input.slice(0, num);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'take-last': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), n = _b[0], array = _b[1];
                assertSeq(array, sourceCodeInfo);
                assertNumber(n, sourceCodeInfo);
                var num = Math.max(Math.ceil(n), 0);
                var from = array.length - num;
                return array.slice(from);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'take-while': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var e_1, _c;
                var _d = __read(_a, 2), fn = _d[0], seq = _d[1];
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
        'drop': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), n = _b[0], input = _b[1];
                assertNumber(n, sourceCodeInfo);
                var num = Math.max(Math.ceil(n), 0);
                assertSeq(input, sourceCodeInfo);
                return input.slice(num);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'drop-last': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), n = _b[0], array = _b[1];
                assertSeq(array, sourceCodeInfo);
                assertNumber(n, sourceCodeInfo);
                var num = Math.max(Math.ceil(n), 0);
                var from = array.length - num;
                return array.slice(0, from);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'drop-while': {
            evaluate: function (_a, sourceCodeInfo, contextStack, _b) {
                var _c = __read(_a, 2), fn = _c[0], seq = _c[1];
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
            validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
        },
        'random-sample!': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), prob = _b[0], seq = _b[1];
                assertNumber(prob, sourceCodeInfo, { finite: true });
                assertSeq(seq, sourceCodeInfo);
                if (typeof seq === 'string') {
                    return seq
                        .split('')
                        .filter(function () { return Math.random() < prob; })
                        .join('');
                }
                else {
                    return seq.filter(function () { return Math.random() < prob; });
                }
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'rand-nth!': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), seq = _b[0];
                assertSeq(seq, sourceCodeInfo);
                if (seq.length === 0)
                    return null;
                var index = Math.floor(Math.random() * seq.length);
                if (typeof seq === 'string')
                    return toAny(seq.split('')[index]);
                return toAny(seq[index]);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'shuffle!': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), input = _b[0];
                assertSeq(input, sourceCodeInfo);
                var array = typeof input === 'string' ? __spreadArray([], __read(input.split('')), false) : __spreadArray([], __read(input), false);
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
                return typeof input === 'string' ? array.join('') : array;
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'distinct': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), input = _b[0];
                assertSeq(input, sourceCodeInfo);
                if (Array.isArray(input))
                    return Array.from(new Set(input));
                return Array.from(new Set(input.split(''))).join('');
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'remove': {
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
        'remove-at': {
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
        'split-at': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), pos = _b[0], seq = _b[1];
                assertNumber(pos, sourceCodeInfo, { finite: true });
                var intPos = toNonNegativeInteger(pos);
                assertSeq(seq, sourceCodeInfo);
                return [seq.slice(0, intPos), seq.slice(intPos)];
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
        'split-with': {
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'group-by': {
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
        'partition': {
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
        'partition-all': {
            evaluate: function (params, sourceCodeInfo) {
                var len = params.length;
                var n = toNonNegativeInteger(asNumber(params[0], sourceCodeInfo));
                var seq = len === 2 ? asSeq(params[1], sourceCodeInfo) : asSeq(params[2], sourceCodeInfo);
                var step = len >= 3 ? toNonNegativeInteger(asNumber(params[1], sourceCodeInfo)) : n;
                return partition(n, step, seq, [], sourceCodeInfo);
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'partition-by': {
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
                var _b = __read(_a, 2), count = _b[0], value = _b[1];
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
            evaluate: function (params, sourceCodeInfo, contextStack, helpers) {
                var mapResult = evaluateMap(params, sourceCodeInfo, contextStack, helpers);
                assertArray(mapResult, sourceCodeInfo);
                return mapResult.flat(1);
            },
            validate: function (node) { return assertNumberOfParams(2, node); },
        },
    };

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
        'rem': {
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
        'pow': {
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
        'rand!': {
            evaluate: function (parameters, sourceCodeInfo) {
                var num = asNumber(parameters.length === 1 ? parameters[0] : 1, sourceCodeInfo);
                return Math.random() * num;
            },
            validate: function (node) { return assertNumberOfParams({ min: 0, max: 1 }, node); },
        },
        'rand-int!': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertNumber(first, sourceCodeInfo);
                return Math.floor(Math.random() * Math.abs(first)) * Math.sign(first);
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
        'epsilon': {
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

    var version = "1.2.0";

    var uuidTemplate = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx';
    var xyRegexp = /[xy]/g;
    var miscNormalExpression = {
        'not=': {
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
        '=': {
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
        'not': {
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
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), ms = _b[0];
                assertNumber(ms, sourceCodeInfo);
                return new Date(ms).toISOString();
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'iso-date-time->inst-ms': {
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
        'uuid!': {
            evaluate: function () {
                return uuidTemplate.replace(xyRegexp, function (character) {
                    var randomNbr = Math.floor(Math.random() * 16);
                    var newValue = character === 'x' ? randomNbr : (randomNbr & 0x3) | 0x8;
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
        'json-parse': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertString(first, sourceCodeInfo);
                // eslint-disable-next-line ts/no-unsafe-return
                return JSON.parse(first);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
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
        'assert-not=': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (first === second)
                    throw new AssertionError("Expected ".concat(first, " not to be ").concat(second, ".").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'assert-equal': {
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
        'assert-not-equal': {
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
        'assert>': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (compare(first, second) <= 0)
                    throw new AssertionError("Expected ".concat(first, " to be grater than ").concat(second, ".").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'assert>=': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (compare(first, second) < 0)
                    throw new AssertionError("Expected ".concat(first, " to be grater than or equal to ").concat(second, ".").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'assert<': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (compare(first, second) >= 0)
                    throw new AssertionError("Expected ".concat(first, " to be less than ").concat(second, ".").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'assert<=': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 3), first = _b[0], second = _b[1], message = _b[2];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (compare(first, second) > 0)
                    throw new AssertionError("Expected ".concat(first, " to be less than or equal to ").concat(second, ".").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
        },
        'assert-true': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (first !== true)
                    throw new AssertionError("Expected ".concat(first, " to be true.").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
        },
        'assert-false': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (first !== false)
                    throw new AssertionError("Expected ".concat(first, " to be false.").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
        },
        'assert-truthy': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (!first)
                    throw new AssertionError("Expected ".concat(first, " to be truthy.").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
        },
        'assert-falsy': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (first)
                    throw new AssertionError("Expected ".concat(first, " to be falsy.").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
        },
        'assert-nil': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 2), first = _b[0], message = _b[1];
                message = typeof message === 'string' && message ? " \"".concat(message, "\"") : '';
                if (first !== null)
                    throw new AssertionError("Expected ".concat(first, " to be nil.").concat(message), sourceCodeInfo);
                return null;
            },
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 2, max: 3 }, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
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
            validate: function (node) { return assertEvenNumberOfParams(node); },
        },
        'keys': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertObj(first, sourceCodeInfo);
                return Object.keys(first);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'vals': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertObj(first, sourceCodeInfo);
                return Object.values(first);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'entries': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), first = _b[0];
                assertObj(first, sourceCodeInfo);
                return Object.entries(first);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
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
            validate: function (node) { return assertNumberOfParams(2, node); },
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
            validate: function (node) { return assertNumberOfParams(2, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 0 }, node); },
        },
        'merge-with': {
            evaluate: function (params, sourceCodeInfo, contextStack, _a) {
                var executeFunction = _a.executeFunction;
                var _b = __read(params), fn = _b[0], first = _b[1], rest = _b.slice(2);
                assertLitsFunction(fn, sourceCodeInfo);
                if (params.length === 1)
                    return null;
                assertObj(first, sourceCodeInfo);
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
            validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
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
            validate: function (node) { return assertNumberOfParams(2, node); },
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
        'positive-infinity?': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertNumber(value, sourceCodeInfo);
                return value === Number.POSITIVE_INFINITY;
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'negative-infinity?': {
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
        'string-repeat': {
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
        'number-to-string': {
            evaluate: function (params, sourceCodeInfo) {
                var _a = __read(params, 2), num = _a[0], base = _a[1];
                assertNumber(num, sourceCodeInfo, { finite: true });
                if (params.length === 1) {
                    return "".concat(num);
                }
                else {
                    assertNumber(base, sourceCodeInfo, { finite: true });
                    if (base !== 2 && base !== 8 && base !== 10 && base !== 16) {
                        throw new LitsError("Expected \"number-to-string\" base argument to be 2, 8, 10 or 16, got: ".concat(base), sourceCodeInfo);
                    }
                    if (base === 10)
                        return "".concat(num);
                    assertNumber(num, sourceCodeInfo, { integer: true, nonNegative: true });
                    return Number(num).toString(base);
                }
            },
            validate: function (node) { return assertNumberOfParams({ min: 1, max: 2 }, node); },
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'to-char-code': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo, { nonEmpty: true });
                return asNonUndefined(str.codePointAt(0), sourceCodeInfo);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'lower-case': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.toLowerCase();
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'upper-case': {
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
        'trim-left': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.replace(/^\s+/, '');
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'trim-right': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), str = _b[0];
                assertString(str, sourceCodeInfo);
                return str.replace(/\s+$/, '');
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
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
        'pad-left': {
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
        'pad-right': {
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
        'encode-base64': {
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
            validate: function (node) { return assertNumberOfParams(1, node); },
        },
        'encode-uri-component': {
            evaluate: function (_a, sourceCodeInfo) {
                var _b = __read(_a, 1), value = _b[0];
                assertString(value, sourceCodeInfo);
                return encodeURIComponent(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 2 }, node); },
        },
        'identity': {
            evaluate: function (_a) {
                var _b = __read(_a, 1), value = _b[0];
                return toAny(value);
            },
            validate: function (node) { return assertNumberOfParams(1, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
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
            validate: function (node) { return assertNumberOfParams(1, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
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
            validate: function (node) { return assertNumberOfParams(1, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
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
            validate: function (node) { return assertNumberOfParams({ min: 1 }, node); },
        },
        'fnil': {
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

    function isToken(value, options) {
        if (options === void 0) { options = {}; }
        if (typeof value !== 'object' || value === null)
            return false;
        var tkn = value;
        if (typeof tkn.v !== 'string')
            return false;
        if (!isTokenType(tkn.t))
            return false;
        if ((options.type != null) && tkn.t !== options.type)
            return false;
        if (options.value && tkn.v !== options.value)
            return false;
        return true;
    }
    function assertToken(value, filePath, options) {
        var _a;
        if (options === void 0) { options = {}; }
        if (!isToken(value, options)) {
            var sourceCodeInfo = isToken(value) && ((_a = value.debugData) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo)
                ? value.debugData.sourceCodeInfo
                : typeof filePath === 'string'
                    ? { filePath: filePath }
                    : undefined;
            throw new LitsError("Expected ".concat((options.type != null) ? "".concat(TokenType[options.type], "-") : '', "token").concat(typeof options.value === 'string' ? " value='".concat(options.value, "'") : '', ", got ").concat(valueToString(value), "."), getSourceCodeInfo(value, sourceCodeInfo));
        }
    }
    function asToken(value, filePath, options) {
        if (options === void 0) { options = {}; }
        assertToken(value, filePath, options);
        return value;
    }

    var andSpecialExpression = {
        parse: function (tokenStream, position, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var _b = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), newPosition = _b[0], params = _b[1];
            var lastToken = asToken(tokenStream.tokens[newPosition], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'and',
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            return [
                newPosition + 1,
                node,
            ];
        },
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

    var commentSpecialExpression = {
        parse: function (tokenStream, position, firstToken, _a) {
            var _b;
            var parseToken = _a.parseToken;
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'comment',
                p: [],
                debugData: undefined,
            };
            var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
            while (!isToken(tkn, { type: TokenType.Bracket, value: ')' })) {
                var bodyNode = void 0;
                _b = __read(parseToken(tokenStream, position), 2), position = _b[0], bodyNode = _b[1];
                node.p.push(bodyNode);
                tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
            }
            node.debugData = firstToken.debugData
                ? {
                    token: firstToken,
                    lastToken: tkn,
                }
                : undefined;
            return [position + 1, node];
        },
        evaluate: function () { return null; },
        findUnresolvedIdentifiers: function () { return new Set(); },
    };

    var condSpecialExpression = {
        parse: function (tokenStream, position, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var _b = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), newPosition = _b[0], params = _b[1];
            var lastToken = asToken(tokenStream.tokens[newPosition], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'cond',
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            assertEvenNumberOfParams(node);
            return [newPosition + 1, node];
        },
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
        parse: function (tokenStream, position, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var _b = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), newPosition = _b[0], params = _b[1];
            var lastToken = asToken(tokenStream.tokens[newPosition], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'declared?',
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            assertNumberOfParams(1, node);
            return [newPosition + 1, node];
        },
        evaluate: function (node, contextStack) {
            var lookUpResult = contextStack.lookUp(node.p[0]);
            return lookUpResult !== null;
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return findUnresolvedIdentifiers(node.p, contextStack, builtin);
        },
    };

    var reservedNamesRecord = {
        'true': { value: true },
        'false': { value: false },
        'nil': { value: null },
        'null': { value: null, forbidden: true },
        'undefined': { value: null, forbidden: true },
        '===': { value: null, forbidden: true },
        '!==': { value: null, forbidden: true },
        '&&': { value: null, forbidden: true },
        '||': { value: null, forbidden: true },
    };

    function assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo) {
        if (typeof name !== 'string')
            return;
        if (builtin.specialExpressions[name])
            throw new LitsError("Cannot define variable ".concat(name, ", it's a special expression."), sourceCodeInfo);
        if (builtin.normalExpressions[name])
            throw new LitsError("Cannot define variable ".concat(name, ", it's a builtin function."), sourceCodeInfo);
        // eslint-disable-next-line ts/no-unsafe-member-access
        if (reservedNamesRecord[name])
            throw new LitsError("Cannot define variable ".concat(name, ", it's a reserved name."), sourceCodeInfo);
        if (contextStack.globalContext[name])
            throw new LitsError("Name already defined \"".concat(name, "\"."), sourceCodeInfo);
    }

    var defSpecialExpression = {
        parse: function (tokenStream, position, firstToken, _a) {
            var _b, _c;
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var _d = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), newPosition = _d[0], params = _d[1];
            var lastToken = asToken(tokenStream.tokens[newPosition], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'def',
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            assertNameNode(node.p[0], (_c = (_b = node.debugData) === null || _b === void 0 ? void 0 : _b.token.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
            assertNumberOfParams(2, node);
            return [newPosition + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var _b, _c;
            var evaluateAstNode = _a.evaluateAstNode, builtin = _a.builtin;
            var sourceCodeInfo = (_c = (_b = node.debugData) === null || _b === void 0 ? void 0 : _b.token.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo;
            var name = node.p[0].v;
            assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo);
            contextStack.globalContext[name] = {
                value: evaluateAstNode(node.p[1], contextStack),
            };
            return null;
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var _b, _c;
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            var sourceCodeInfo = (_c = (_b = node.debugData) === null || _b === void 0 ? void 0 : _b.token.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo;
            var subNode = asAstNode(node.p[1]);
            var result = findUnresolvedIdentifiers([subNode], contextStack, builtin);
            var name = asNameNode(node.p[0]).v;
            assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo);
            contextStack.globalContext[name] = { value: true };
            return result;
        },
    };

    var defsSpecialExpression = {
        parse: function (tokenStream, position, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var _b = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), newPosition = _b[0], params = _b[1];
            var lastToken = asToken(tokenStream.tokens[newPosition], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'defs',
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            assertNumberOfParams(2, node);
            return [newPosition + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var _b, _c, _d, _e;
            var evaluateAstNode = _a.evaluateAstNode, builtin = _a.builtin;
            var sourceCodeInfo = (_c = (_b = node.debugData) === null || _b === void 0 ? void 0 : _b.token.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo;
            var name = evaluateAstNode(node.p[0], contextStack);
            assertString(name, sourceCodeInfo);
            assertNameNotDefined(name, contextStack, builtin, (_e = (_d = node.debugData) === null || _d === void 0 ? void 0 : _d.token.debugData) === null || _e === void 0 ? void 0 : _e.sourceCodeInfo);
            contextStack.globalContext[name] = {
                value: evaluateAstNode(node.p[1], contextStack),
            };
            return null;
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var _b, _c;
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin, evaluateAstNode = _a.evaluateAstNode;
            var sourceCodeInfo = (_c = (_b = node.debugData) === null || _b === void 0 ? void 0 : _b.token.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo;
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
        parse: function (tokenStream, position, firstToken, _a) {
            var _b;
            var parseToken = _a.parseToken;
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'do',
                p: [],
                debugData: undefined,
            };
            var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
            while (!isToken(tkn, { type: TokenType.Bracket, value: ')' })) {
                var bodyNode = void 0;
                _b = __read(parseToken(tokenStream, position), 2), position = _b[0], bodyNode = _b[1];
                node.p.push(bodyNode);
                tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
            }
            node.debugData = firstToken.debugData
                ? {
                    token: firstToken,
                    lastToken: tkn,
                }
                : undefined;
            return [position + 1, node];
        },
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
        parse: function (tokenStream, position, firstToken, parsers) {
            var _a, _b;
            var _c, _d;
            var parseToken = parsers.parseToken;
            var functionName;
            _a = __read(parseToken(tokenStream, position), 2), position = _a[0], functionName = _a[1];
            assertNameNode(functionName, (_d = (_c = functionName.debugData) === null || _c === void 0 ? void 0 : _c.token.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
            var functionOverloades;
            _b = __read(parseFunctionOverloades(tokenStream, position, parsers), 2), position = _b[0], functionOverloades = _b[1];
            var lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'defn',
                f: functionName,
                p: [],
                o: functionOverloades,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            return [
                position + 1,
                node,
            ];
        },
        evaluate: function (node, contextStack, _a) {
            var _b;
            var _c, _d, _e, _f;
            var builtin = _a.builtin, evaluateAstNode = _a.evaluateAstNode;
            var name = getFunctionName('defn', node, contextStack, evaluateAstNode);
            assertNameNotDefined(name, contextStack, builtin, (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.token.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
            var evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode);
            var litsFunction = (_b = {},
                _b[FUNCTION_SYMBOL] = true,
                _b.sourceCodeInfo = (_f = (_e = node.debugData) === null || _e === void 0 ? void 0 : _e.token.debugData) === null || _f === void 0 ? void 0 : _f.sourceCodeInfo,
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
        parse: function (tokenStream, position, firstToken, parsers) {
            var _a, _b;
            var parseToken = parsers.parseToken;
            var functionName;
            _a = __read(parseToken(tokenStream, position), 2), position = _a[0], functionName = _a[1];
            var functionOverloades;
            _b = __read(parseFunctionOverloades(tokenStream, position, parsers), 2), position = _b[0], functionOverloades = _b[1];
            var lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'defns',
                p: [],
                f: functionName,
                o: functionOverloades,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            return [
                position + 1,
                node,
            ];
        },
        evaluate: function (node, contextStack, _a) {
            var _b;
            var _c, _d, _e, _f;
            var builtin = _a.builtin, evaluateAstNode = _a.evaluateAstNode;
            var name = getFunctionName('defns', node, contextStack, evaluateAstNode);
            assertNameNotDefined(name, contextStack, builtin, (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.token.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
            var evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode);
            var litsFunction = (_b = {},
                _b[FUNCTION_SYMBOL] = true,
                _b.sourceCodeInfo = (_f = (_e = node.debugData) === null || _e === void 0 ? void 0 : _e.token.debugData) === null || _f === void 0 ? void 0 : _f.sourceCodeInfo,
                _b.t = FunctionType.UserDefined,
                _b.n = name,
                _b.o = evaluatedFunctionOverloades,
                _b);
            contextStack.globalContext[name] = { value: litsFunction };
            return null;
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var _b;
            var _c, _d;
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin, evaluateAstNode = _a.evaluateAstNode;
            var sourceCodeInfo = (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.token.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo;
            var name = evaluateAstNode(asAstNode(node.f, sourceCodeInfo), contextStack);
            assertString(name, sourceCodeInfo);
            assertNameNotDefined(name, contextStack, builtin, sourceCodeInfo);
            contextStack.globalContext[name] = { value: true };
            var newContext = (_b = {}, _b[name] = { value: true }, _b);
            return addOverloadsUnresolvedIdentifiers(node.o, contextStack, findUnresolvedIdentifiers, builtin, newContext);
        },
    };
    var fnSpecialExpression = {
        parse: function (tokenStream, position, firstToken, parsers) {
            var _a;
            var functionOverloades;
            _a = __read(parseFunctionOverloades(tokenStream, position, parsers), 2), position = _a[0], functionOverloades = _a[1];
            var lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'fn',
                p: [],
                o: functionOverloades,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            return [
                position + 1,
                node,
            ];
        },
        evaluate: function (node, contextStack, _a) {
            var _b;
            var _c, _d;
            var evaluateAstNode = _a.evaluateAstNode;
            var evaluatedFunctionOverloades = evaluateFunctionOverloades(node, contextStack, evaluateAstNode);
            var litsFunction = (_b = {},
                _b[FUNCTION_SYMBOL] = true,
                _b.sourceCodeInfo = (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.token.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo,
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
        var _a, _b;
        var sourceCodeInfo = (_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo;
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
    function parseFunctionBody(tokenStream, position, _a) {
        var _b;
        var _c;
        var parseToken = _a.parseToken;
        var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        var body = [];
        while (!(tkn.t === TokenType.Bracket && tkn.v === ')')) {
            var bodyNode = void 0;
            _b = __read(parseToken(tokenStream, position), 2), position = _b[0], bodyNode = _b[1];
            body.push(bodyNode);
            tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        }
        if (body.length === 0)
            throw new LitsError('Missing body in function', (_c = tkn.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
        return [position, body];
    }
    function parseFunctionOverloades(tokenStream, position, parsers) {
        var _a, _b, _c, _d;
        var _e, _f, _g;
        var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket });
        if (tkn.v === '(') {
            var functionOverloades = [];
            while (!(tkn.t === TokenType.Bracket && tkn.v === ')')) {
                position += 1;
                tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
                var functionArguments = void 0;
                _a = __read(parseFunctionArguments(tokenStream, position, parsers), 2), position = _a[0], functionArguments = _a[1];
                var arity = functionArguments.r ? { min: functionArguments.m.length } : functionArguments.m.length;
                if (!arityOk(functionOverloades, arity))
                    throw new LitsError('All overloaded functions must have different arity', (_e = tkn.debugData) === null || _e === void 0 ? void 0 : _e.sourceCodeInfo);
                var functionBody = void 0;
                _b = __read(parseFunctionBody(tokenStream, position, parsers), 2), position = _b[0], functionBody = _b[1];
                functionOverloades.push({
                    as: functionArguments,
                    b: functionBody,
                    a: arity,
                });
                tkn = asToken(tokenStream.tokens[position + 1], tokenStream.filePath, { type: TokenType.Bracket });
                if (tkn.v !== ')' && tkn.v !== '(')
                    throw new LitsError("Expected ( or ) token, got ".concat(valueToString(tkn), "."), (_f = tkn.debugData) === null || _f === void 0 ? void 0 : _f.sourceCodeInfo);
                position += 1;
            }
            return [position, functionOverloades];
        }
        else if (tkn.v === '[') {
            var functionArguments = void 0;
            _c = __read(parseFunctionArguments(tokenStream, position, parsers), 2), position = _c[0], functionArguments = _c[1];
            var arity = functionArguments.r ? { min: functionArguments.m.length } : functionArguments.m.length;
            var functionBody = void 0;
            _d = __read(parseFunctionBody(tokenStream, position, parsers), 2), position = _d[0], functionBody = _d[1];
            return [
                position,
                [
                    {
                        as: functionArguments,
                        b: functionBody,
                        a: arity,
                    },
                ],
            ];
        }
        else {
            throw new LitsError("Expected [ or ( token, got ".concat(valueToString(tkn)), (_g = tkn.debugData) === null || _g === void 0 ? void 0 : _g.sourceCodeInfo);
        }
    }
    function parseFunctionArguments(tokenStream, position, parsers) {
        var _a;
        var _b, _c, _d, _e, _f;
        var parseArgument = parsers.parseArgument, parseBindings = parsers.parseBindings;
        var bindings = [];
        var restArgument;
        var mandatoryArguments = [];
        var state = 'mandatory';
        var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        position += 1;
        tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        while (!(tkn.t === TokenType.Bracket && tkn.v === ']')) {
            if (state === 'let') {
                _a = __read(parseBindings(tokenStream, position), 2), position = _a[0], bindings = _a[1];
                break;
            }
            else {
                var _g = __read(parseArgument(tokenStream, position), 2), newPosition = _g[0], node = _g[1];
                position = newPosition;
                tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
                if (node.t === AstNodeType.Modifier) {
                    switch (node.v) {
                        case '&':
                            if (state === 'rest')
                                throw new LitsError('& can only appear once', (_b = tkn.debugData) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
                            state = 'rest';
                            break;
                        case '&let':
                            if (state === 'rest' && !restArgument)
                                throw new LitsError('No rest argument was specified', (_c = tkn.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
                            state = 'let';
                            break;
                        default:
                            throw new LitsError("Illegal modifier: ".concat(node.v), (_d = tkn.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
                    }
                }
                else {
                    switch (state) {
                        case 'mandatory':
                            mandatoryArguments.push(node.n);
                            break;
                        case 'rest':
                            if (restArgument !== undefined)
                                throw new LitsError('Can only specify one rest argument', (_e = tkn.debugData) === null || _e === void 0 ? void 0 : _e.sourceCodeInfo);
                            restArgument = node.n;
                            break;
                    }
                }
            }
        }
        if (state === 'rest' && restArgument === undefined)
            throw new LitsError('Missing rest argument name', (_f = tkn.debugData) === null || _f === void 0 ? void 0 : _f.sourceCodeInfo);
        position += 1;
        var args = {
            m: mandatoryArguments,
            r: restArgument,
            b: bindings,
        };
        return [position, args];
    }

    var ifSpecialExpression = {
        parse: function (tokenStream, position, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var _b = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), newPosition = _b[0], params = _b[1];
            var lastToken = asToken(tokenStream.tokens[newPosition], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'if',
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            assertNumberOfParams({ min: 2, max: 3 }, node);
            return [newPosition + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var _b, _c;
            var evaluateAstNode = _a.evaluateAstNode;
            var sourceCodeInfo = (_c = (_b = node.debugData) === null || _b === void 0 ? void 0 : _b.token.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo;
            var _d = __read(node.p, 3), conditionNode = _d[0], trueNode = _d[1], falseNode = _d[2];
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
        parse: function (tokenStream, position, firstToken, _a) {
            var _b, _c;
            var _d, _e, _f, _g;
            var parseBindings = _a.parseBindings, parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket, parseToken = _a.parseToken;
            var bindingArray = ((_d = firstToken.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo) ? asNormalExpressionNode(parseToken(tokenStream, position)[1]) : undefined;
            var bindings;
            _b = __read(parseBindings(tokenStream, position), 2), position = _b[0], bindings = _b[1];
            if (bindings.length !== 1) {
                throw new LitsError("Expected exactly one binding, got ".concat(valueToString(bindings.length)), (_e = firstToken.debugData) === null || _e === void 0 ? void 0 : _e.sourceCodeInfo);
            }
            var params;
            _c = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), position = _c[0], params = _c[1];
            var lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'if-let',
                b: asNonUndefined(bindings[0], (_f = firstToken.debugData) === null || _f === void 0 ? void 0 : _f.sourceCodeInfo),
                p: params,
                debugData: ((_g = firstToken.debugData) === null || _g === void 0 ? void 0 : _g.sourceCodeInfo) && bindingArray && {
                    token: firstToken,
                    lastToken: lastToken,
                    bindingArray: bindingArray,
                },
            };
            assertNumberOfParams({ min: 1, max: 2 }, node);
            return [position + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var _b, _c;
            var evaluateAstNode = _a.evaluateAstNode;
            var sourceCodeInfo = (_c = (_b = node.debugData) === null || _b === void 0 ? void 0 : _b.token.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo;
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
        parse: function (tokenStream, position, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var _b = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), newPosition = _b[0], params = _b[1];
            var lastToken = asToken(tokenStream.tokens[newPosition], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'if-not',
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            assertNumberOfParams({ min: 2, max: 3 }, node);
            return [newPosition + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var _b, _c;
            var evaluateAstNode = _a.evaluateAstNode;
            var sourceCodeInfo = (_c = (_b = node.debugData) === null || _b === void 0 ? void 0 : _b.token.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo;
            var _d = __read(node.p, 3), conditionNode = _d[0], trueNode = _d[1], falseNode = _d[2];
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
        parse: function (tokenStream, position, firstToken, _a) {
            var _b, _c;
            var _d, _e;
            var parseBindings = _a.parseBindings, parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket, parseToken = _a.parseToken;
            var bindingArray = ((_d = firstToken.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo) && asNormalExpressionNode(parseToken(tokenStream, position)[1]);
            var bindings;
            _b = __read(parseBindings(tokenStream, position), 2), position = _b[0], bindings = _b[1];
            var params;
            _c = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), position = _c[0], params = _c[1];
            var lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'let',
                p: params,
                bs: bindings,
                debugData: ((_e = firstToken.debugData) === null || _e === void 0 ? void 0 : _e.sourceCodeInfo) && bindingArray && {
                    token: firstToken,
                    lastToken: lastToken,
                    bindingArray: bindingArray,
                },
            };
            return [position + 1, node];
        },
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
        parse: function (tokenStream, position, firstToken, _a) {
            var _b, _c;
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket, parseBindings = _a.parseBindings;
            var bindings;
            _b = __read(parseBindings(tokenStream, position), 2), position = _b[0], bindings = _b[1];
            var params;
            _c = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), position = _c[0], params = _c[1];
            var lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'loop',
                p: params,
                bs: bindings,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            return [position + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var _b, _c;
            var evaluateAstNode = _a.evaluateAstNode;
            var sourceCodeInfo = (_c = (_b = node.debugData) === null || _b === void 0 ? void 0 : _b.token.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo;
            var bindingContext = node.bs.reduce(function (result, binding) {
                result[binding.n] = { value: evaluateAstNode(binding.v, contextStack) };
                return result;
            }, {});
            var newContextStack = contextStack.create(bindingContext);
            var _loop_1 = function () {
                var e_1, _d;
                var result = null;
                try {
                    try {
                        for (var _e = (e_1 = void 0, __values(node.p)), _f = _e.next(); !_f.done; _f = _e.next()) {
                            var form = _f.value;
                            result = evaluateAstNode(form, newContextStack);
                        }
                    }
                    catch (e_1_1) { e_1 = { error: e_1_1 }; }
                    finally {
                        try {
                            if (_f && !_f.done && (_d = _e.return)) _d.call(_e);
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

    function parseLoopBinding(tokenStream, position, _a) {
        var _b, _c, _d, _e;
        var _f, _g, _h, _j;
        var parseBinding = _a.parseBinding, parseBindings = _a.parseBindings, parseToken = _a.parseToken;
        var bindingNode;
        _b = __read(parseBinding(tokenStream, position), 2), position = _b[0], bindingNode = _b[1];
        var loopBinding = {
            b: bindingNode,
            m: [],
        };
        var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        while (tkn.t === TokenType.Modifier) {
            switch (tkn.v) {
                case '&let':
                    if (loopBinding.l) {
                        throw new LitsError('Only one &let modifier allowed', (_f = tkn.debugData) === null || _f === void 0 ? void 0 : _f.sourceCodeInfo);
                    }
                    _c = __read(parseBindings(tokenStream, position + 1), 2), position = _c[0], loopBinding.l = _c[1];
                    loopBinding.m.push('&let');
                    break;
                case '&when':
                    if (loopBinding.wn) {
                        throw new LitsError('Only one &when modifier allowed', (_g = tkn.debugData) === null || _g === void 0 ? void 0 : _g.sourceCodeInfo);
                    }
                    _d = __read(parseToken(tokenStream, position + 1), 2), position = _d[0], loopBinding.wn = _d[1];
                    loopBinding.m.push('&when');
                    break;
                case '&while':
                    if (loopBinding.we) {
                        throw new LitsError('Only one &while modifier allowed', (_h = tkn.debugData) === null || _h === void 0 ? void 0 : _h.sourceCodeInfo);
                    }
                    _e = __read(parseToken(tokenStream, position + 1), 2), position = _e[0], loopBinding.we = _e[1];
                    loopBinding.m.push('&while');
                    break;
                default:
                    throw new LitsError("Illegal modifier: ".concat(tkn.v), (_j = tkn.debugData) === null || _j === void 0 ? void 0 : _j.sourceCodeInfo);
            }
            tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        }
        return [position, loopBinding];
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
    function parseLoopBindings(tokenStream, position, parsers) {
        var _a;
        assertToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: '[' });
        position += 1;
        var loopBindings = [];
        var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        while (!isToken(tkn, { type: TokenType.Bracket, value: ']' })) {
            var loopBinding = void 0;
            _a = __read(parseLoopBinding(tokenStream, position, parsers), 2), position = _a[0], loopBinding = _a[1];
            loopBindings.push(loopBinding);
            tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        }
        return [position + 1, loopBindings];
    }
    function evaluateLoop(returnResult, node, contextStack, evaluateAstNode) {
        var e_2, _a;
        var _b, _c;
        var sourceCodeInfo = (_c = (_b = node.debugData) === null || _b === void 0 ? void 0 : _b.token.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo;
        var _d = node, loopBindings = _d.l, params = _d.p;
        var result = [];
        var bindingIndices = loopBindings.map(function () { return 0; });
        var abort = false;
        while (!abort) {
            var context = {};
            var newContextStack = contextStack.create(context);
            var skip = false;
            bindingsLoop: for (var bindingIndex = 0; bindingIndex < loopBindings.length; bindingIndex += 1) {
                var _e = asNonUndefined(loopBindings[bindingIndex], sourceCodeInfo), binding = _e.b, letBindings = _e.l, whenNode = _e.wn, whileNode = _e.we, modifiers = _e.m;
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
        parse: function (tokenStream, position, firstToken, parsers) {
            var _a, _b;
            var parseTokensUntilClosingBracket = parsers.parseTokensUntilClosingBracket;
            var loopBindings;
            _a = __read(parseLoopBindings(tokenStream, position, parsers), 2), position = _a[0], loopBindings = _a[1];
            var params;
            _b = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), position = _b[0], params = _b[1];
            var lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                n: 'for',
                t: AstNodeType.SpecialExpression,
                l: loopBindings,
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            assertNumberOfParams(1, node);
            return [position + 1, node];
        },
        evaluate: function (node, contextStack, helpers) { return evaluateLoop(true, node, contextStack, helpers.evaluateAstNode); },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return analyze$2(node, contextStack, findUnresolvedIdentifiers, builtin);
        },
    };
    var doseqSpecialExpression = {
        parse: function (tokenStream, position, firstToken, parsers) {
            var _a, _b;
            var parseTokensUntilClosingBracket = parsers.parseTokensUntilClosingBracket;
            var loopBindings;
            _a = __read(parseLoopBindings(tokenStream, position, parsers), 2), position = _a[0], loopBindings = _a[1];
            var params;
            _b = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), position = _b[0], params = _b[1];
            var lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                n: 'doseq',
                t: AstNodeType.SpecialExpression,
                l: loopBindings,
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            assertNumberOfParams(1, node);
            return [position + 1, node];
        },
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
        parse: function (tokenStream, position, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var _b = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), newPosition = _b[0], params = _b[1];
            var lastToken = asToken(tokenStream.tokens[newPosition], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'or',
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            return [newPosition + 1, node];
        },
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
        parse: function (tokenStream, position, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var _b = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), newPosition = _b[0], params = _b[1];
            var lastToken = asToken(tokenStream.tokens[newPosition], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: '??',
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            assertNumberOfParams({ min: 1, max: 2 }, node);
            return [newPosition + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var _b, _c;
            var evaluateAstNode = _a.evaluateAstNode;
            var _d = __read(node.p, 2), firstNode = _d[0], secondNode = _d[1];
            if (isNameNode(firstNode)) {
                if (contextStack.lookUp(firstNode) === null)
                    return secondNode ? evaluateAstNode(secondNode, contextStack) : null;
            }
            assertAny(firstNode, (_c = (_b = node.debugData) === null || _b === void 0 ? void 0 : _b.token.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
            var firstResult = evaluateAstNode(firstNode, contextStack);
            return firstResult !== null && firstResult !== void 0 ? firstResult : (secondNode ? evaluateAstNode(secondNode, contextStack) : null);
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return findUnresolvedIdentifiers(node.p, contextStack, builtin);
        },
    };

    var recurSpecialExpression = {
        parse: function (tokenStream, position, firstToken, _a) {
            var _b;
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var params;
            _b = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), position = _b[0], params = _b[1];
            var lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'recur',
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            return [position + 1, node];
        },
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
        parse: function (tokenStream, position, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var _b = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), newPosition = _b[0], params = _b[1];
            position = newPosition;
            assertToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'throw',
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            assertNumberOfParams(1, node);
            return [position + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var _b, _c, _d, _e;
            var evaluateAstNode = _a.evaluateAstNode;
            var message = asString(evaluateAstNode(node.p[0], contextStack), (_c = (_b = node.debugData) === null || _b === void 0 ? void 0 : _b.token.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo, {
                nonEmpty: true,
            });
            throw new UserDefinedError(message, (_e = (_d = node.debugData) === null || _d === void 0 ? void 0 : _d.token.debugData) === null || _e === void 0 ? void 0 : _e.sourceCodeInfo);
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return findUnresolvedIdentifiers(node.p, contextStack, builtin);
        },
    };

    var timeSpecialExpression = {
        parse: function (tokenStream, position, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var _b = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), newPosition = _b[0], params = _b[1];
            var lastToken = asToken(tokenStream.tokens[newPosition], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'time!',
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            assertNumberOfParams(1, node);
            return [newPosition + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var evaluateAstNode = _a.evaluateAstNode;
            var param = node.p[0];
            var startTime = Date.now();
            var result = evaluateAstNode(param, contextStack);
            var totalTime = Date.now() - startTime;
            // eslint-disable-next-line no-console
            console.log("Elapsed time: ".concat(totalTime, " ms"));
            return result;
        },
        findUnresolvedIdentifiers: function (node, contextStack, _a) {
            var findUnresolvedIdentifiers = _a.findUnresolvedIdentifiers, builtin = _a.builtin;
            return findUnresolvedIdentifiers(node.p, contextStack, builtin);
        },
    };

    var trySpecialExpression = {
        parse: function (tokenStream, position, firstToken, _a) {
            var _b, _c, _d, _e;
            var _f, _g, _h, _j, _k, _l;
            var parseToken = _a.parseToken;
            var tryExpression;
            _b = __read(parseToken(tokenStream, position), 2), position = _b[0], tryExpression = _b[1];
            assertToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: '(' });
            position += 1;
            var catchNode;
            _c = __read(parseToken(tokenStream, position), 2), position = _c[0], catchNode = _c[1];
            assertNameNode(catchNode, (_g = (_f = catchNode.debugData) === null || _f === void 0 ? void 0 : _f.token.debugData) === null || _g === void 0 ? void 0 : _g.sourceCodeInfo);
            if (catchNode.v !== 'catch') {
                throw new LitsError("Expected 'catch', got '".concat(catchNode.v, "'."), getSourceCodeInfo(catchNode, (_j = (_h = catchNode.debugData) === null || _h === void 0 ? void 0 : _h.token.debugData) === null || _j === void 0 ? void 0 : _j.sourceCodeInfo));
            }
            var error;
            _d = __read(parseToken(tokenStream, position), 2), position = _d[0], error = _d[1];
            assertNameNode(error, (_l = (_k = error.debugData) === null || _k === void 0 ? void 0 : _k.token.debugData) === null || _l === void 0 ? void 0 : _l.sourceCodeInfo);
            var catchExpression;
            _e = __read(parseToken(tokenStream, position), 2), position = _e[0], catchExpression = _e[1];
            assertToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            position += 1;
            var lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'try',
                p: [tryExpression],
                ce: catchExpression,
                e: error,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            assertNumberOfParams(1, node);
            return [position + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var _b;
            var _c, _d;
            var evaluateAstNode = _a.evaluateAstNode;
            var tryExpressions = node.p, catchExpression = node.ce, errorNode = node.e;
            try {
                return evaluateAstNode(tryExpressions[0], contextStack);
            }
            catch (error) {
                var newContext = (_b = {},
                    _b[errorNode.v] = { value: asAny(error, (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.token.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo) },
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
        parse: function (tokenStream, position, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var _b = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), newPosition = _b[0], params = _b[1];
            var lastToken = asToken(tokenStream.tokens[newPosition], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'when',
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            assertNumberOfParams({ min: 1 }, node);
            return [newPosition + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var _c, _d;
            var evaluateAstNode = _a.evaluateAstNode;
            var _e = __read(node.p), whenExpression = _e[0], body = _e.slice(1);
            assertAstNode(whenExpression, (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.token.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
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
        parse: function (tokenStream, position, firstToken, _a) {
            var _b, _c;
            var _d, _e;
            var parseBindings = _a.parseBindings, parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var bindings;
            _b = __read(parseBindings(tokenStream, position), 2), position = _b[0], bindings = _b[1];
            if (bindings.length !== 1) {
                throw new LitsError("Expected exactly one binding, got ".concat(valueToString(bindings.length)), (_d = firstToken.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
            }
            var params;
            _c = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), position = _c[0], params = _c[1];
            var lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'when-first',
                b: asNonUndefined(bindings[0], (_e = firstToken.debugData) === null || _e === void 0 ? void 0 : _e.sourceCodeInfo),
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            return [position + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var _c, _d;
            var evaluateAstNode = _a.evaluateAstNode;
            var locals = {};
            var binding = node.b;
            var evaluatedBindingForm = evaluateAstNode(binding.v, contextStack);
            if (!isSeq(evaluatedBindingForm)) {
                throw new LitsError("Expected undefined or a sequence, got ".concat(valueToString(evaluatedBindingForm)), (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.token.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
            }
            if (evaluatedBindingForm.length === 0)
                return null;
            var bindingValue = toAny(evaluatedBindingForm[0]);
            locals[binding.n] = { value: bindingValue };
            var newContextStack = contextStack.create(locals);
            var result = null;
            try {
                for (var _e = __values(node.p), _f = _e.next(); !_f.done; _f = _e.next()) {
                    var form = _f.value;
                    result = evaluateAstNode(form, newContextStack);
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (_f && !_f.done && (_b = _e.return)) _b.call(_e);
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
        parse: function (tokenStream, position, firstToken, _a) {
            var _b, _c;
            var _d, _e;
            var parseBindings = _a.parseBindings, parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var bindings;
            _b = __read(parseBindings(tokenStream, position), 2), position = _b[0], bindings = _b[1];
            if (bindings.length !== 1) {
                throw new LitsError("Expected exactly one binding, got ".concat(valueToString(bindings.length)), (_d = firstToken.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
            }
            var params;
            _c = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), position = _c[0], params = _c[1];
            var lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'when-let',
                b: asNonUndefined(bindings[0], (_e = firstToken.debugData) === null || _e === void 0 ? void 0 : _e.sourceCodeInfo),
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            return [position + 1, node];
        },
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
        parse: function (tokenStream, position, firstToken, _a) {
            var parseTokensUntilClosingBracket = _a.parseTokensUntilClosingBracket;
            var _b = __read(parseTokensUntilClosingBracket(tokenStream, position), 2), newPosition = _b[0], params = _b[1];
            var lastToken = asToken(tokenStream.tokens[newPosition], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
            var node = {
                t: AstNodeType.SpecialExpression,
                n: 'when-not',
                p: params,
                debugData: firstToken.debugData && {
                    token: firstToken,
                    lastToken: lastToken,
                },
            };
            assertNumberOfParams({ min: 1 }, node);
            return [newPosition + 1, node];
        },
        evaluate: function (node, contextStack, _a) {
            var e_1, _b;
            var _c, _d;
            var evaluateAstNode = _a.evaluateAstNode;
            var _e = __read(node.p), whenExpression = _e[0], body = _e.slice(1);
            assertAstNode(whenExpression, (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.token.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
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
        'and': andSpecialExpression,
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
        'if-let': ifLetSpecialExpression,
        'if-not': ifNotSpecialExpression,
        'let': letSpecialExpression,
        'loop': loopSpecialExpression,
        'or': orSpecialExpression,
        'recur': recurSpecialExpression,
        'throw': throwSpecialExpression,
        'time!': timeSpecialExpression,
        'try': trySpecialExpression,
        'when': whenSpecialExpression,
        'when-first': whenFirstSpecialExpression,
        'when-let': whenLetSpecialExpression,
        'when-not': whenNotSpecialExpression,
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
            var _c, _d, _e, _f, _g;
            var value = node.v;
            var sourceCodeInfo = (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.token.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo;
            try {
                for (var _h = __values(this.contexts), _j = _h.next(); !_j.done; _j = _h.next()) {
                    var context = _j.value;
                    var contextEntry = context[value];
                    if (contextEntry)
                        return contextEntry;
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (_j && !_j.done && (_a = _h.return)) _a.call(_h);
                }
                finally { if (e_2) throw e_2.error; }
            }
            var lazyHostValue = (_e = this.lazyValues) === null || _e === void 0 ? void 0 : _e[value];
            if (lazyHostValue !== undefined) {
                return {
                    value: toAny(lazyHostValue.read()),
                };
            }
            var hostValue = (_f = this.values) === null || _f === void 0 ? void 0 : _f[value];
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
            var nativeJsFunction = (_g = this.nativeJsFunctions) === null || _g === void 0 ? void 0 : _g[value];
            if (nativeJsFunction) {
                return {
                    value: nativeJsFunction,
                };
            }
            return null;
        };
        ContextStackImpl.prototype.evaluateName = function (node) {
            var _a, _b;
            var lookUpResult = this.lookUp(node);
            if (isContextEntry(lookUpResult))
                return lookUpResult.value;
            else if (isBuiltinFunction(lookUpResult))
                return lookUpResult;
            throw new UndefinedSymbolError(node.v, (_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
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
        _a[FunctionType.Fnil] = function (fn, params, sourceCodeInfo, contextStack, _a) {
            var executeFunction = _a.executeFunction;
            var fniledParams = params.map(function (param, index) { return (param === null ? toAny(fn.p[index]) : param); });
            return executeFunction(toAny(fn.f), fniledParams, contextStack, sourceCodeInfo);
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
        var _a, _b;
        switch (node.t) {
            case AstNodeType.Number:
                return evaluateNumber(node);
            case AstNodeType.String:
                return evaluateString(node);
            case AstNodeType.Name:
                return contextStack.evaluateName(node);
            case AstNodeType.ReservedName:
                return evaluateReservedName(node);
            case AstNodeType.NormalExpression:
                return evaluateNormalExpression(node, contextStack);
            case AstNodeType.SpecialExpression:
                return evaluateSpecialExpression(node, contextStack);
            default:
                throw new LitsError("".concat(node.t, "-node cannot be evaluated"), (_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
        }
    }
    function evaluateNumber(node) {
        return node.v;
    }
    function evaluateString(node) {
        return node.v;
    }
    function evaluateReservedName(node) {
        var _a, _b;
        return asNonUndefined(reservedNamesRecord[node.v], (_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo).value;
    }
    function evaluateNormalExpression(node, contextStack) {
        var _a, _b;
        var params = node.p.map(function (paramNode) { return evaluateAstNode(paramNode, contextStack); });
        var sourceCodeInfo = (_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo;
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
        var _a, _b, _c, _d;
        var normalExpression = builtin.normalExpressions[node.n];
        if (!normalExpression)
            throw new UndefinedSymbolError(node.n, (_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
        return normalExpression.evaluate(params, (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.token.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo, contextStack, { executeFunction: executeFunction });
    }
    function evaluateSpecialExpression(node, contextStack) {
        var _a, _b;
        var specialExpression = asNonUndefined(builtin.specialExpressions[node.n], (_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
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
        var _b, _c, _d, _e;
        var emptySet = new Set();
        switch (astNode.t) {
            case AstNodeType.Name: {
                var lookUpResult = contextStack.lookUp(astNode);
                if (lookUpResult === null)
                    return new Set([{ symbol: astNode.v, token: (_b = astNode.debugData) === null || _b === void 0 ? void 0 : _b.token }]);
                return emptySet;
            }
            case AstNodeType.String:
            case AstNodeType.Number:
            case AstNodeType.Modifier:
            case AstNodeType.ReservedName:
            case AstNodeType.Comment:
                return emptySet;
            case AstNodeType.NormalExpression: {
                var unresolvedIdentifiers_1 = new Set();
                var name_1 = astNode.n, debug = astNode.debugData;
                if (typeof name_1 === 'string') {
                    var lookUpResult = contextStack.lookUp({ t: AstNodeType.Name, v: name_1, debugData: debug, p: [], n: undefined });
                    if (lookUpResult === null)
                        unresolvedIdentifiers_1.add({ symbol: name_1, token: (_c = astNode.debugData) === null || _c === void 0 ? void 0 : _c.token });
                }
                try {
                    for (var _f = __values(astNode.p), _g = _f.next(); !_g.done; _g = _f.next()) {
                        var subNode = _g.value;
                        var innerUnresolvedIdentifiers = findUnresolvedIdentifiersInAstNode(subNode, contextStack, builtin);
                        innerUnresolvedIdentifiers.forEach(function (symbol) { return unresolvedIdentifiers_1.add(symbol); });
                    }
                }
                catch (e_2_1) { e_2 = { error: e_2_1 }; }
                finally {
                    try {
                        if (_g && !_g.done && (_a = _f.return)) _a.call(_f);
                    }
                    finally { if (e_2) throw e_2.error; }
                }
                return unresolvedIdentifiers_1;
            }
            case AstNodeType.SpecialExpression: {
                var specialExpression = asNonUndefined(builtin.specialExpressions[astNode.n], (_e = (_d = astNode.debugData) === null || _d === void 0 ? void 0 : _d.token.debugData) === null || _e === void 0 ? void 0 : _e.sourceCodeInfo);
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
            n: 'and',
            t: AstNodeType.SpecialExpression,
            p: p,
            debugData: astNode.debugData,
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

    var trueNode = { t: AstNodeType.ReservedName, v: 'true', debugData: undefined, p: [], n: undefined };
    var falseNode = { t: AstNodeType.ReservedName, v: 'false', debugData: undefined, p: [], n: undefined };
    var calculateDeclaredOutcomes = function (_a) {
        var astNode = _a.astNode, isAstComputable = _a.isAstComputable;
        if (isAstComputable(astNode.p))
            return [trueNode];
        return [trueNode, falseNode];
    };

    var calculateDefOutcomes = function (_a) {
        var astNode = _a.astNode, calculatePossibleAstNodes = _a.calculatePossibleAstNodes, addGlobalIdentifier = _a.addGlobalIdentifier;
        var nameNode = asNameNode(astNode.p[0]);
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
                n: 'if-not',
                t: astNode.t,
                p: p,
                debugData: astNode.debugData,
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
                debugData: astNode.debugData,
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
                    debugData: astNode.debugData,
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
                debugData: astNode.debugData,
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
            n: 'or',
            t: AstNodeType.SpecialExpression,
            p: p,
            debugData: astNode.debugData,
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

    var calculateTimeOutcomes = function (_a) {
        var astNode = _a.astNode, calculatePossibleAstNodes = _a.calculatePossibleAstNodes;
        return calculatePossibleAstNodes(astNode.p[0])
            .map(function (p) { return (__assign(__assign({}, astNode), { n: 'do', p: [p] })); });
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
                        debugData: undefined,
                        p: [],
                    }],
                p: [astNode.ce],
                debugData: undefined,
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
                debugData: astNode.debugData,
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
                n: 'when-not',
                t: astNode.t,
                p: p,
                debugData: astNode.debugData,
            }); });
        }
        var body = astNode.p.slice(1);
        return __spreadArray(__spreadArray([], __read(combinateAstNodes(body)
            .map(function (p) { return ({
            n: 'do',
            t: astNode.t,
            p: p,
            debugData: astNode.debugData,
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
                debugData: astNode.debugData,
            }); });
        }
        var body = astNode.p.slice(1);
        return __spreadArray(__spreadArray([], __read(combinateAstNodes(body)
            .map(function (p) { return ({
            n: 'do',
            t: astNode.t,
            p: p,
            debugData: astNode.debugData,
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
        'and': function (astNode, helperOptions) { return calculateAndOutcomes(__assign({ astNode: astNode }, helperOptions)); },
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
        'if-let': function (astNode, helperOptions) { return calculateIfLetOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'if': function (astNode, helperOptions) { return calculateIfOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'if-not': function (astNode, helperOptions) { return calculateIfNotOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'let': function (astNode, helperOptions) { return calculateLetOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'loop': function (astNode, helperOptions) { return calculateLoopOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'or': function (astNode, helperOptions) { return calculateOrOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        '??': function (astNode, helperOptions) { return calculateQqOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'recur': function (astNode, helperOptions) { return calculateRecurOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'time!': function (astNode, helperOptions) { return calculateTimeOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'throw': function (astNode, helperOptions) { return calculateThrowOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'try': function (astNode, helperOptions) { return calculateTryOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'when-first': function (astNode, helperOptions) { return calculateWhenFirstOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'when-let': function (astNode, helperOptions) { return calculateWhenLetOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'when': function (astNode, helperOptions) { return calculateWhenOutcomes(__assign({ astNode: astNode }, helperOptions)); },
        'when-not': function (astNode, helperOptions) { return calculateWhenNotOutcomes(__assign({ astNode: astNode }, helperOptions)); },
    };

    function isIdempotent(normalExpressionName) {
        return !normalExpressionName.endsWith('!')
            || normalExpressionName === 'write!';
    }
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
    var nilNode = { t: AstNodeType.ReservedName, v: 'nil', debugData: undefined, p: [], n: undefined };
    function calculatePossibleAstNodes(contextStack, astNode, newIndentifiers) {
        var newContext = newIndentifiers
            ? newIndentifiers.reduce(function (acc, identity) {
                acc[identity] = { value: null };
                return acc;
            }, {})
            : undefined;
        var newContextStack = newContext ? contextStack.create(newContext) : contextStack;
        if (astNode.t === AstNodeType.NormalExpression) {
            if (astNode.n && !isIdempotent(astNode.n))
                throw new Error("NormalExpressionNode with name ".concat(astNode.n, " is not idempotent. Cannot calculate possible ASTs."));
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

    function parseNumber(tokenStream, position) {
        var _a;
        var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        return [
            position + 1,
            {
                t: AstNodeType.Number,
                v: Number(tkn.v),
                p: [],
                n: undefined,
                debugData: ((_a = tkn.debugData) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo)
                    ? { token: tkn, lastToken: tkn }
                    : undefined,
            },
        ];
    }
    function parseString(tokenStream, position) {
        var _a;
        var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        return [
            position + 1,
            {
                t: AstNodeType.String,
                v: tkn.v,
                p: [],
                n: undefined,
                debugData: ((_a = tkn.debugData) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo)
                    ? { token: tkn, lastToken: tkn }
                    : undefined,
            },
        ];
    }
    function parseName(tokenStream, position) {
        var _a;
        var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        return [
            position + 1,
            {
                t: AstNodeType.Name,
                v: tkn.v,
                p: [],
                n: undefined,
                debugData: ((_a = tkn.debugData) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo)
                    ? { token: tkn, lastToken: tkn }
                    : undefined,
            },
        ];
    }
    function parseReservedName(tokenStream, position) {
        var _a;
        var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        return [
            position + 1,
            {
                t: AstNodeType.ReservedName,
                v: tkn.v,
                p: [],
                n: undefined,
                debugData: ((_a = tkn.debugData) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo)
                    ? { token: tkn, lastToken: tkn }
                    : undefined,
            },
        ];
    }
    function parseComment(tokenStream, position) {
        var _a;
        var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        return [
            position + 1,
            {
                t: AstNodeType.Comment,
                v: tkn.v,
                p: [],
                n: undefined,
                debugData: ((_a = tkn.debugData) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo)
                    ? { token: tkn, lastToken: tkn }
                    : undefined,
            },
        ];
    }
    function parseTokensUntilClosingBracket(tokenStream, position) {
        var _a;
        var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        var astNodes = [];
        var astNode;
        while (!(tkn.t === TokenType.Bracket && (tkn.v === ')' || tkn.v === ']'))) {
            _a = __read(parseToken(tokenStream, position), 2), position = _a[0], astNode = _a[1];
            astNodes.push(astNode);
            tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        }
        return [position, astNodes];
    }
    var parseExpression = function (tokenStream, position) {
        var tkn = asToken(tokenStream.tokens[position + 1], tokenStream.filePath);
        if (tkn.t === TokenType.Name && builtin.specialExpressions[tkn.v])
            return parseSpecialExpression(tokenStream, position);
        return parseNormalExpression(tokenStream, position);
    };
    function parseArrayLitteral(tokenStream, position) {
        var _a;
        var _b;
        var firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath);
        position = position + 1;
        var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        var params = [];
        var param;
        while (!(tkn.t === TokenType.Bracket && tkn.v === ']')) {
            _a = __read(parseToken(tokenStream, position), 2), position = _a[0], param = _a[1];
            params.push(param);
            tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        }
        position = position + 1;
        var node = {
            t: AstNodeType.NormalExpression,
            n: 'array',
            p: params,
            debugData: ((_b = firstToken.debugData) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo)
                ? {
                    token: firstToken,
                    lastToken: tkn,
                }
                : undefined,
        };
        return [position, node];
    }
    function parseObjectLitteral(tokenStream, position) {
        var _a;
        var _b;
        var firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath);
        position = position + 1;
        var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        var params = [];
        var param;
        while (!(tkn.t === TokenType.Bracket && tkn.v === '}')) {
            _a = __read(parseToken(tokenStream, position), 2), position = _a[0], param = _a[1];
            params.push(param);
            tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        }
        position = position + 1;
        var node = {
            t: AstNodeType.NormalExpression,
            n: 'object',
            p: params,
            debugData: ((_b = firstToken.debugData) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo)
                ? {
                    token: firstToken,
                    lastToken: tkn,
                }
                : undefined,
        };
        assertEvenNumberOfParams(node);
        return [position, node];
    }
    function parseRegexpShorthand(tokenStream, position) {
        var _a, _b, _c, _d;
        var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        var stringNode = {
            t: AstNodeType.String,
            v: tkn.v,
            p: [],
            n: undefined,
            debugData: ((_a = tkn.debugData) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo)
                ? {
                    token: tkn,
                    lastToken: tkn,
                }
                : undefined,
        };
        assertNonUndefined(tkn.o, (_b = tkn.debugData) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
        var optionsNode = {
            t: AstNodeType.String,
            v: "".concat(tkn.o.g ? 'g' : '').concat(tkn.o.i ? 'i' : ''),
            p: [],
            n: undefined,
            debugData: ((_c = tkn.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo)
                ? {
                    token: tkn,
                    lastToken: tkn,
                }
                : undefined,
        };
        var node = {
            t: AstNodeType.NormalExpression,
            n: 'regexp',
            p: [stringNode, optionsNode],
            debugData: ((_d = tkn.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo)
                ? {
                    token: tkn,
                    lastToken: tkn,
                }
                : undefined,
        };
        return [
            position + 1,
            node,
        ];
    }
    var placeholderRegexp = /^%([1-9]\d?)?$/;
    var parseFnShorthand = function (tokenStream, position) {
        var _a, _b, _c, _d, _e;
        var firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath);
        position += 1;
        var _f = __read(parseExpression(tokenStream, position), 2), newPosition = _f[0], exprNode = _f[1];
        var arity = 0;
        var percent1 = 'NOT_SET';
        for (var pos = position + 1; pos < newPosition - 1; pos += 1) {
            var tkn = asToken(tokenStream.tokens[pos], tokenStream.filePath);
            if (tkn.t === TokenType.Name) {
                var match = placeholderRegexp.exec(tkn.v);
                if (match) {
                    var number = (_a = match[1]) !== null && _a !== void 0 ? _a : '1';
                    if (number === '1') {
                        var mixedPercent1 = (!match[1] && percent1 === 'WITH_1') || (match[1] && percent1 === 'NAKED');
                        if (mixedPercent1)
                            throw new LitsError('Please make up your mind, either use % or %1', (_b = firstToken.debugData) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo);
                        percent1 = match[1] ? 'WITH_1' : 'NAKED';
                    }
                    arity = Math.max(arity, Number(number));
                    if (arity > 20)
                        throw new LitsError('Can\'t specify more than 20 arguments', (_c = firstToken.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
                }
            }
            if (tkn.t === TokenType.FnShorthand)
                throw new LitsError('Nested shortcut functions are not allowed', (_d = firstToken.debugData) === null || _d === void 0 ? void 0 : _d.sourceCodeInfo);
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
            debugData: ((_e = firstToken.debugData) === null || _e === void 0 ? void 0 : _e.sourceCodeInfo)
                ? {
                    token: firstToken,
                    lastToken: exprNode.debugData.lastToken,
                }
                : undefined,
        };
        return [newPosition, node];
    };
    var parseArgument = function (tokenStream, position) {
        var _a, _b, _c;
        var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        if (tkn.t === TokenType.Name) {
            return [
                position + 1,
                {
                    t: AstNodeType.Argument,
                    n: tkn.v,
                    p: [],
                    debugData: ((_a = tkn.debugData) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo)
                        ? {
                            token: tkn,
                            lastToken: tkn,
                        }
                        : undefined,
                },
            ];
        }
        else if (tkn.t === TokenType.Modifier) {
            var value = tkn.v;
            return [
                position + 1,
                {
                    t: AstNodeType.Modifier,
                    v: value,
                    p: [],
                    n: undefined,
                    debugData: ((_b = tkn.debugData) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo)
                        ? {
                            token: tkn,
                            lastToken: tkn,
                        }
                        : undefined,
                },
            ];
        }
        else {
            throw new LitsError("Expected name or modifier token, got ".concat(valueToString(tkn), "."), (_c = tkn.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
        }
    };
    function parseBindings(tokenStream, position) {
        var _a;
        var tkn = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: '[' });
        position += 1;
        tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        var bindings = [];
        var binding;
        while (!(tkn.t === TokenType.Bracket && tkn.v === ']')) {
            _a = __read(parseBinding(tokenStream, position), 2), position = _a[0], binding = _a[1];
            bindings.push(binding);
            tkn = asToken(tokenStream.tokens[position], tokenStream.filePath);
        }
        position += 1;
        return [position, bindings];
    }
    function parseBinding(tokenStream, position) {
        var _a;
        var _b;
        var firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Name });
        var name = firstToken.v;
        position += 1;
        var value;
        _a = __read(parseToken(tokenStream, position), 2), position = _a[0], value = _a[1];
        var node = {
            t: AstNodeType.Binding,
            n: name,
            v: value,
            p: [],
            debugData: ((_b = firstToken.debugData) === null || _b === void 0 ? void 0 : _b.sourceCodeInfo)
                ? {
                    token: firstToken,
                    lastToken: value.debugData.lastToken,
                }
                : undefined,
        };
        return [position, node];
    }
    function parseNormalExpression(tokenStream, position) {
        var _a;
        var _b, _c, _d, _e;
        var startBracketToken = tokenStream.hasDebugData ? asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: '(' }) : undefined;
        position += 1;
        var _f = __read(parseToken(tokenStream, position), 2), newPosition = _f[0], fnNode = _f[1];
        var params;
        _a = __read(parseTokensUntilClosingBracket(tokenStream, newPosition), 2), position = _a[0], params = _a[1];
        var lastToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: ')' });
        position += 1;
        if (isExpressionNode(fnNode)) {
            var node_1 = {
                t: AstNodeType.NormalExpression,
                p: __spreadArray([fnNode], __read(params), false),
                n: undefined,
                debugData: startBracketToken
                    ? {
                        token: startBracketToken,
                        lastToken: lastToken,
                    }
                    : undefined,
            };
            return [position, node_1];
        }
        assertNameNode(fnNode, (_c = (_b = fnNode.debugData) === null || _b === void 0 ? void 0 : _b.token.debugData) === null || _c === void 0 ? void 0 : _c.sourceCodeInfo);
        var node = {
            t: AstNodeType.NormalExpression,
            n: fnNode.v,
            p: params,
            debugData: startBracketToken
                ? {
                    token: startBracketToken,
                    nameToken: (_d = fnNode.debugData) === null || _d === void 0 ? void 0 : _d.token,
                    lastToken: lastToken,
                }
                : undefined,
        };
        var builtinExpression = builtin.normalExpressions[node.n];
        if (builtinExpression) {
            (_e = builtinExpression.validate) === null || _e === void 0 ? void 0 : _e.call(builtinExpression, __assign(__assign({}, node), { p: withoutCommentNodes(node.p) }));
        }
        return [position, node];
    }
    function parseSpecialExpression(tokenStream, position) {
        var firstToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Bracket, value: '(' });
        position += 1;
        var nameToken = asToken(tokenStream.tokens[position], tokenStream.filePath, { type: TokenType.Name });
        var expressionName = nameToken.v, debugData = nameToken.debugData;
        position += 1;
        var parse = asNonUndefined(builtin.specialExpressions[expressionName], debugData === null || debugData === void 0 ? void 0 : debugData.sourceCodeInfo).parse;
        var _a = __read(parse(tokenStream, position, firstToken, {
            parseExpression: parseExpression,
            parseTokensUntilClosingBracket: parseTokensUntilClosingBracket,
            parseToken: parseToken,
            parseBinding: parseBinding,
            parseBindings: parseBindings,
            parseArgument: parseArgument,
        }), 2), positionAfterParse = _a[0], node = _a[1];
        if (node.debugData)
            node.debugData.nameToken = nameToken;
        return [positionAfterParse, node];
    }
    function parseToken(tokenStream, position) {
        var _a;
        var tkn = getNextParsableToken(tokenStream, position);
        switch (tkn.t) {
            case TokenType.Number:
                return parseNumber(tokenStream, position);
            case TokenType.String:
                return parseString(tokenStream, position);
            case TokenType.Name:
                return parseName(tokenStream, position);
            case TokenType.ReservedName:
                return parseReservedName(tokenStream, position);
            case TokenType.Bracket:
                if (tkn.v === '(')
                    return parseExpression(tokenStream, position);
                else if (tkn.v === '[')
                    return parseArrayLitteral(tokenStream, position);
                else if (tkn.v === '{')
                    return parseObjectLitteral(tokenStream, position);
                break;
            case TokenType.RegexpShorthand:
                return parseRegexpShorthand(tokenStream, position);
            case TokenType.FnShorthand:
                return parseFnShorthand(tokenStream, position);
            case TokenType.Comment:
                return parseComment(tokenStream, position);
            case TokenType.CollectionAccessor:
            case TokenType.Modifier:
            case TokenType.NewLine:
                break;
            /* v8 ignore next 2 */
            default:
                assertUnreachable(tkn.t);
        }
        throw new LitsError("Unrecognized token: ".concat(tkn.t, " value=").concat(tkn.v), (_a = tkn.debugData) === null || _a === void 0 ? void 0 : _a.sourceCodeInfo);
    }
    function getNextParsableToken(tokenStream, position) {
        var token = asToken(tokenStream.tokens[position], tokenStream.filePath);
        while (token.t === TokenType.NewLine) {
            position += 1;
            token = asToken(tokenStream.tokens[position], tokenStream.filePath);
        }
        return token;
    }

    function parse$1(tokenStream) {
        var _a;
        var ast = {
            b: [],
            hasDebugData: tokenStream.hasDebugData,
        };
        var position = 0;
        var node;
        while (position < tokenStream.tokens.length) {
            _a = __read(parseToken(tokenStream, position), 2), position = _a[0], node = _a[1];
            ast.b.push(node);
        }
        return ast;
    }

    var applyCollectionAccessors = function (tokenStream) {
        var dotTokenIndex = tokenStream.tokens.findIndex(function (tkn) { return tkn.t === TokenType.CollectionAccessor; });
        while (dotTokenIndex >= 0) {
            applyCollectionAccessor(tokenStream, dotTokenIndex);
            dotTokenIndex = tokenStream.tokens.findIndex(function (tkn) { return tkn.t === TokenType.CollectionAccessor; });
        }
        return tokenStream;
    };
    function applyCollectionAccessor(tokenStream, position) {
        var dotTkn = asNonUndefined(tokenStream.tokens[position]);
        var debugData = dotTkn.debugData;
        var backPosition = getPositionBackwards(tokenStream, position, debugData === null || debugData === void 0 ? void 0 : debugData.sourceCodeInfo);
        checkForward(tokenStream, position, dotTkn, debugData === null || debugData === void 0 ? void 0 : debugData.sourceCodeInfo);
        tokenStream.tokens.splice(position, 1);
        tokenStream.tokens.splice(backPosition, 0, {
            t: TokenType.Bracket,
            v: '(',
            debugData: debugData,
        });
        var nextTkn = asNonUndefined(tokenStream.tokens[position + 1]);
        if (dotTkn.v === '.') {
            tokenStream.tokens[position + 1] = {
                t: TokenType.String,
                v: nextTkn.v,
                debugData: nextTkn.debugData,
            };
        }
        else {
            assertNumber(Number(nextTkn.v), debugData === null || debugData === void 0 ? void 0 : debugData.sourceCodeInfo, { integer: true, nonNegative: true });
            tokenStream.tokens[position + 1] = {
                t: TokenType.Number,
                v: nextTkn.v,
                debugData: nextTkn.debugData,
            };
        }
        tokenStream.tokens.splice(position + 2, 0, {
            t: TokenType.Bracket,
            v: ')',
            debugData: debugData,
        });
    }
    function getPositionBackwards(tokenStream, position, sourceCodeInfo) {
        var bracketCount = null;
        if (position <= 0)
            throw new LitsError('Array accessor # must come after a sequence', sourceCodeInfo);
        var prevToken = asNonUndefined(tokenStream.tokens[position - 1]);
        var openBracket = null;
        var closeBracket = null;
        if (prevToken.t === TokenType.Bracket) {
            switch (prevToken.v) {
                case ')':
                    openBracket = '(';
                    closeBracket = ')';
                    break;
                case ']':
                    openBracket = '[';
                    closeBracket = ']';
                    break;
                case '}':
                    openBracket = '{';
                    closeBracket = '}';
                    break;
                default:
                    throw new LitsError('# or . must be preceeded by a collection', sourceCodeInfo);
            }
        }
        while (bracketCount !== 0) {
            bracketCount = bracketCount === null ? 0 : bracketCount;
            position -= 1;
            var tkn = asNonUndefined(tokenStream.tokens[position], sourceCodeInfo);
            if (tkn.t === TokenType.Bracket) {
                if (tkn.v === openBracket)
                    bracketCount += 1;
                if (tkn.v === closeBracket)
                    bracketCount -= 1;
            }
        }
        if (openBracket === '(' && position > 0) {
            var tokenBeforeBracket = asNonUndefined(tokenStream.tokens[position - 1]);
            if (tokenBeforeBracket.t === TokenType.FnShorthand)
                throw new LitsError('# or . must NOT be preceeded by shorthand lambda function', sourceCodeInfo);
        }
        return position;
    }
    function checkForward(tokenStream, position, dotTkn, sourceCodeInfo) {
        var tkn = asNonUndefined(tokenStream.tokens[position + 1], sourceCodeInfo);
        if (dotTkn.v === '.' && tkn.t !== TokenType.Name)
            throw new LitsError('# as a collection accessor must be followed by an name', sourceCodeInfo);
        if (dotTkn.v === '#' && tkn.t !== TokenType.Number)
            throw new LitsError('# as a collection accessor must be followed by an integer', sourceCodeInfo);
    }

    function getSugar() {
        return [applyCollectionAccessors];
    }

    var NO_MATCH = [0, undefined];
    var nameCharacters = '[\\w@%^?=!$<>+*/-]';
    var nameRegExp = new RegExp(nameCharacters);
    var whitespaceRegExp = /\s|,/;
    var newLineRegExp = /\n/;
    var tokenizeNewLine = function (input, current, debugData) {
        return newLineRegExp.test(input[current])
            ? [1, { t: TokenType.NewLine, v: '\n', debugData: debugData }]
            : NO_MATCH;
    };
    var tokenizeComment = function (input, current, debugData) {
        if (input[current] === ';') {
            var length_1 = 0;
            var value = '';
            while (input[current + length_1] !== '\n' && current + length_1 < input.length) {
                value += input[current + length_1];
                length_1 += 1;
            }
            if (input[current + length_1] === '\n' && current + length_1 < input.length)
                length_1 += 1;
            return [length_1, { t: TokenType.Comment, v: value.trim(), debugData: debugData }];
        }
        return NO_MATCH;
    };
    var skipWhiteSpace = function (input, current) {
        return whitespaceRegExp.test(input[current]) ? [1, undefined] : NO_MATCH;
    };
    var tokenizeLeftParen = function (input, position, debugData) {
        return tokenizeCharacter(TokenType.Bracket, '(', input, position, debugData);
    };
    var tokenizeRightParen = function (input, position, debugData) {
        return tokenizeCharacter(TokenType.Bracket, ')', input, position, debugData);
    };
    var tokenizeLeftBracket = function (input, position, debugData) {
        return tokenizeCharacter(TokenType.Bracket, '[', input, position, debugData);
    };
    var tokenizeRightBracket = function (input, position, debugData) {
        return tokenizeCharacter(TokenType.Bracket, ']', input, position, debugData);
    };
    var tokenizeLeftCurly = function (input, position, debugData) {
        return tokenizeCharacter(TokenType.Bracket, '{', input, position, debugData);
    };
    var tokenizeRightCurly = function (input, position, debugData) {
        return tokenizeCharacter(TokenType.Bracket, '}', input, position, debugData);
    };
    var tokenizeString = function (input, position, debugData) {
        if (input[position] !== '"')
            return NO_MATCH;
        var value = '';
        var length = 1;
        var char = input[position + length];
        var escape = false;
        while (char !== '"' || escape) {
            if (char === undefined)
                throw new LitsError("Unclosed string at position ".concat(position, "."), debugData === null || debugData === void 0 ? void 0 : debugData.sourceCodeInfo);
            length += 1;
            if (escape) {
                escape = false;
                if (char === '"' || char === '\\') {
                    value += char;
                }
                else {
                    value += '\\';
                    value += char;
                }
            }
            else {
                if (char === '\\')
                    escape = true;
                else
                    value += char;
            }
            char = input[position + length];
        }
        return [length + 1, { t: TokenType.String, v: value, debugData: debugData }];
    };
    var tokenizeCollectionAccessor = function (input, position, debugData) {
        var char = input[position];
        if (char !== '.' && char !== '#')
            return NO_MATCH;
        return [
            1,
            {
                t: TokenType.CollectionAccessor,
                v: char,
                debugData: debugData,
            },
        ];
    };
    var tokenizeSymbolString = function (input, position, debugData) {
        if (input[position] !== ':')
            return NO_MATCH;
        var value = '';
        var length = 1;
        var char = input[position + length];
        while (char && nameRegExp.test(char)) {
            length += 1;
            value += char;
            char = input[position + length];
        }
        if (length === 1)
            return NO_MATCH;
        return [length, { t: TokenType.String, v: value, debugData: debugData, o: { s: true } }];
    };
    var tokenizeRegexpShorthand = function (input, position, debugData) {
        var _a;
        if (input[position] !== '#')
            return NO_MATCH;
        var _b = __read(tokenizeString(input, position + 1, debugData), 2), stringLength = _b[0], token = _b[1];
        if (!token)
            return NO_MATCH;
        position += stringLength + 1;
        var length = stringLength + 1;
        var options = {};
        while (input[position] === 'g' || input[position] === 'i') {
            if (input[position] === 'g') {
                if (options.g)
                    throw new LitsError("Duplicated regexp option \"".concat(input[position], "\" at position ").concat(position, "."), debugData === null || debugData === void 0 ? void 0 : debugData.sourceCodeInfo);
                length += 1;
                options.g = true;
            }
            else {
                if (options.i)
                    throw new LitsError("Duplicated regexp option \"".concat(input[position], "\" at position ").concat(position, "."), debugData === null || debugData === void 0 ? void 0 : debugData.sourceCodeInfo);
                length += 1;
                options.i = true;
            }
            position += 1;
        }
        if (nameRegExp.test((_a = input[position]) !== null && _a !== void 0 ? _a : ''))
            throw new LitsError("Unexpected regexp option \"".concat(input[position], "\" at position ").concat(position, "."), debugData === null || debugData === void 0 ? void 0 : debugData.sourceCodeInfo);
        return [
            length,
            {
                t: TokenType.RegexpShorthand,
                v: token.v,
                o: options,
                debugData: debugData,
            },
        ];
    };
    var tokenizeFnShorthand = function (input, position, debugData) {
        if (input.slice(position, position + 2) !== '#(')
            return NO_MATCH;
        return [
            1,
            {
                t: TokenType.FnShorthand,
                v: '#',
                debugData: debugData,
            },
        ];
    };
    var endOfNumberRegExp = /[\s)\]},#]/;
    var decimalNumberRegExp = /\d/;
    var octalNumberRegExp = /[0-7]/;
    var hexNumberRegExp = /[0-9a-f]/i;
    var binaryNumberRegExp = /[01]/;
    var firstCharRegExp = /[0-9.-]/;
    var tokenizeNumber = function (input, position, debugData) {
        var type = 'decimal';
        var firstChar = input[position];
        if (!firstCharRegExp.test(firstChar))
            return NO_MATCH;
        var hasDecimals = firstChar === '.';
        var i;
        for (i = position + 1; i < input.length; i += 1) {
            var char = asString(input[i], debugData === null || debugData === void 0 ? void 0 : debugData.sourceCodeInfo, { char: true });
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
        return [length, { t: TokenType.Number, v: value, debugData: debugData }];
    };
    var tokenizeReservedName = function (input, position, debugData) {
        var e_1, _a;
        try {
            for (var _b = __values(Object.entries(reservedNamesRecord)), _c = _b.next(); !_c.done; _c = _b.next()) {
                var _d = __read(_c.value, 2), reservedName = _d[0], forbidden = _d[1].forbidden;
                var length_2 = reservedName.length;
                var nextChar = input[position + length_2];
                if (nextChar && nameRegExp.test(nextChar))
                    continue;
                var name_1 = input.substring(position, position + length_2);
                if (name_1 === reservedName) {
                    if (forbidden)
                        throw new LitsError("".concat(name_1, " is forbidden!"), debugData === null || debugData === void 0 ? void 0 : debugData.sourceCodeInfo);
                    return [length_2, { t: TokenType.ReservedName, v: reservedName, debugData: debugData }];
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
    var tokenizeName = function (input, position, debugData) {
        return tokenizePattern(TokenType.Name, nameRegExp, input, position, debugData);
    };
    var tokenizeModifier = function (input, position, debugData) {
        var e_2, _a;
        var modifiers = ['&', '&let', '&when', '&while'];
        try {
            for (var modifiers_1 = __values(modifiers), modifiers_1_1 = modifiers_1.next(); !modifiers_1_1.done; modifiers_1_1 = modifiers_1.next()) {
                var modifier = modifiers_1_1.value;
                var length_3 = modifier.length;
                var charAfterModifier = input[position + length_3];
                if (input.substring(position, position + length_3) === modifier && (!charAfterModifier || !nameRegExp.test(charAfterModifier))) {
                    var value = modifier;
                    return [length_3, { t: TokenType.Modifier, v: value, debugData: debugData }];
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
    function tokenizeCharacter(type, value, input, position, debugData) {
        if (value === input[position])
            return [1, { t: type, v: value, debugData: debugData }];
        else
            return NO_MATCH;
    }
    function tokenizePattern(type, pattern, input, position, debugData) {
        var char = input[position];
        var length = 0;
        var value = '';
        if (!char || !pattern.test(char))
            return NO_MATCH;
        while (char && pattern.test(char)) {
            value += char;
            length += 1;
            char = input[position + length];
        }
        return [length, { t: type, v: value, debugData: debugData }];
    }

    // All tokenizers, order matters!
    var tokenizers = [
        skipWhiteSpace,
        tokenizeComment,
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
        tokenizeCollectionAccessor,
    ];
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
    function tokenize$1(input, params) {
        var _a, _b, e_1, _c, _d;
        var debug = !!params.debug;
        var tokens = [];
        var position = 0;
        var tokenized = false;
        while (position < input.length) {
            tokenized = false;
            var leadingNewLineTokens = void 0;
            _a = __read(readLeadingNewLineTokens(input, position, params), 2), position = _a[0], leadingNewLineTokens = _a[1];
            if (position >= input.length)
                break;
            var leadingCommentTokens = void 0;
            _b = __read(readLeadingCommentTokens(input, position, params), 2), position = _b[0], leadingCommentTokens = _b[1];
            if (position >= input.length)
                break;
            var leadingMetaTokens = __spreadArray(__spreadArray([], __read(leadingNewLineTokens), false), __read(leadingCommentTokens), false);
            // Loop through all tokenizer until one matches
            var debugData = debug
                ? {
                    sourceCodeInfo: createSourceCodeInfo(input, position, params.filePath),
                    metaTokens: { inlineCommentToken: null, leadingMetaTokens: leadingMetaTokens },
                }
                : undefined;
            try {
                for (var tokenizers_1 = (e_1 = void 0, __values(tokenizers)), tokenizers_1_1 = tokenizers_1.next(); !tokenizers_1_1.done; tokenizers_1_1 = tokenizers_1.next()) {
                    var tokenizer = tokenizers_1_1.value;
                    var _e = __read(tokenizer(input, position, debugData), 2), nbrOfCharacters = _e[0], token = _e[1];
                    // tokenizer matched
                    if (nbrOfCharacters > 0) {
                        tokenized = true;
                        position += nbrOfCharacters;
                        if (token) {
                            var inlineCommentToken = null;
                            if (!isCommentToken(token))
                                _d = __read(readInlineCommentToken(input, position, params), 2), position = _d[0], inlineCommentToken = _d[1];
                            if (token.debugData)
                                token.debugData.metaTokens.inlineCommentToken = inlineCommentToken;
                            if (!isCommentToken(token) || debug)
                                tokens.push(token);
                        }
                        break;
                    }
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (tokenizers_1_1 && !tokenizers_1_1.done && (_c = tokenizers_1.return)) _c.call(tokenizers_1);
                }
                finally { if (e_1) throw e_1.error; }
            }
            if (!tokenized)
                throw new LitsError("Unrecognized character '".concat(input[position], "'."), debugData === null || debugData === void 0 ? void 0 : debugData.sourceCodeInfo);
        }
        var tokenStream = {
            tokens: tokens,
            filePath: params.filePath,
            hasDebugData: debug,
        };
        applySugar(tokenStream);
        return tokenStream;
    }
    function applySugar(tokenStream) {
        var sugar = getSugar();
        sugar.forEach(function (sugarFn) { return sugarFn(tokenStream); });
    }
    var newLineTokenizers = [
        tokenizeNewLine,
        skipWhiteSpace,
    ];
    function readLeadingNewLineTokens(input, position, params) {
        var e_2, _a;
        var newLineTokens = [];
        var tokenized = false;
        while (position < input.length) {
            tokenized = false;
            var debugData = params.debug
                ? {
                    sourceCodeInfo: createSourceCodeInfo(input, position, params.filePath),
                    metaTokens: { inlineCommentToken: null, leadingMetaTokens: [] },
                }
                : undefined;
            try {
                // Loop through all tokenizer until one matches
                for (var newLineTokenizers_1 = (e_2 = void 0, __values(newLineTokenizers)), newLineTokenizers_1_1 = newLineTokenizers_1.next(); !newLineTokenizers_1_1.done; newLineTokenizers_1_1 = newLineTokenizers_1.next()) {
                    var tokenizer = newLineTokenizers_1_1.value;
                    var _b = __read(tokenizer(input, position, debugData), 2), nbrOfCharacters = _b[0], token = _b[1];
                    // tokenizer matched
                    if (nbrOfCharacters > 0) {
                        tokenized = true;
                        position += nbrOfCharacters;
                        if (token) {
                            assertNewLineToken(token);
                            if (newLineTokens.length < 2)
                                newLineTokens.push(token);
                        }
                        break;
                    }
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (newLineTokenizers_1_1 && !newLineTokenizers_1_1.done && (_a = newLineTokenizers_1.return)) _a.call(newLineTokenizers_1);
                }
                finally { if (e_2) throw e_2.error; }
            }
            if (!tokenized)
                // All newline tokens read!
                return [position, newLineTokens];
        }
        // Ending up here means that no non newline token was found. I.e. this cannot be leading newline tokens
        return [position, []];
    }
    var metaTokenizers = [
        tokenizeNewLine,
        skipWhiteSpace,
        tokenizeComment,
    ];
    function readLeadingCommentTokens(input, position, params) {
        var e_3, _a;
        var commentTokens = [];
        var rollbackPosition = position;
        var tokenized = false;
        while (position < input.length) {
            tokenized = false;
            var debugData = params.debug
                ? {
                    sourceCodeInfo: createSourceCodeInfo(input, position, params.filePath),
                    metaTokens: { inlineCommentToken: null, leadingMetaTokens: [] },
                }
                : undefined;
            try {
                // Loop through all tokenizer until one matches
                for (var metaTokenizers_1 = (e_3 = void 0, __values(metaTokenizers)), metaTokenizers_1_1 = metaTokenizers_1.next(); !metaTokenizers_1_1.done; metaTokenizers_1_1 = metaTokenizers_1.next()) {
                    var tokenizer = metaTokenizers_1_1.value;
                    var _b = __read(tokenizer(input, position, debugData), 2), nbrOfCharacters = _b[0], token = _b[1];
                    // tokenizer matched
                    if (nbrOfCharacters > 0) {
                        tokenized = true;
                        position += nbrOfCharacters;
                        if (token) {
                            assertMetaToken(token);
                            // If a newline token is found, then this is not a leading comment
                            if (isNewLineToken(token))
                                return [rollbackPosition, []];
                            commentTokens.push(token);
                        }
                        break;
                    }
                }
            }
            catch (e_3_1) { e_3 = { error: e_3_1 }; }
            finally {
                try {
                    if (metaTokenizers_1_1 && !metaTokenizers_1_1.done && (_a = metaTokenizers_1.return)) _a.call(metaTokenizers_1);
                }
                finally { if (e_3) throw e_3.error; }
            }
            if (!tokenized)
                // All metatokens read!
                return [position, commentTokens];
        }
        // Ending up here means that no non meta token was found. I.e. this cannot be leading meta tokens
        return [rollbackPosition, []];
    }
    var commentTokenizers = [
        tokenizeNewLine,
        skipWhiteSpace,
        tokenizeComment,
    ];
    function readInlineCommentToken(input, position, params) {
        var e_4, _a;
        var rollbackPosition = position;
        var tokenized = false;
        while (position < input.length) {
            tokenized = false;
            var debugData = params.debug
                ? {
                    sourceCodeInfo: createSourceCodeInfo(input, position, params.filePath),
                    metaTokens: { inlineCommentToken: null, leadingMetaTokens: [] },
                }
                : undefined;
            try {
                // Loop through all tokenizer until one matches
                for (var commentTokenizers_1 = (e_4 = void 0, __values(commentTokenizers)), commentTokenizers_1_1 = commentTokenizers_1.next(); !commentTokenizers_1_1.done; commentTokenizers_1_1 = commentTokenizers_1.next()) {
                    var tokenizer = commentTokenizers_1_1.value;
                    var _b = __read(tokenizer(input, position, debugData), 2), nbrOfCharacters = _b[0], token = _b[1];
                    // tokenizer matched
                    if (nbrOfCharacters > 0) {
                        tokenized = true;
                        position += nbrOfCharacters;
                        if (token) {
                            if (isNewLineToken(token))
                                return [rollbackPosition, null];
                            assertCommentToken(token);
                            return [position, token];
                        }
                        break;
                    }
                }
            }
            catch (e_4_1) { e_4 = { error: e_4_1 }; }
            finally {
                try {
                    if (commentTokenizers_1_1 && !commentTokenizers_1_1.done && (_a = commentTokenizers_1.return)) _a.call(commentTokenizers_1);
                }
                finally { if (e_4) throw e_4.error; }
            }
            if (!tokenized)
                // All metatokens read! Return undefined if not debug mode
                return [rollbackPosition, null];
        }
        // Ending up here means that no comment token was found and end of tokens reached
        return [position, null];
    }
    function isMetaToken(token) {
        return !!token && (token.t === TokenType.NewLine || token.t === TokenType.Comment);
    }
    function assertMetaToken(token) {
        if (!isMetaToken(token))
            throw new LitsError("Expected meta token, got ".concat(token === null || token === void 0 ? void 0 : token.t, "."));
    }
    function isCommentToken(token) {
        return !!token && token.t === TokenType.Comment;
    }
    function assertCommentToken(token) {
        if (!isCommentToken(token))
            throw new LitsError("Expected comment token, got ".concat(token === null || token === void 0 ? void 0 : token.t, "."));
    }
    function isNewLineToken(token) {
        return !!token && token.t === TokenType.NewLine;
    }
    function assertNewLineToken(token) {
        if (!isNewLineToken(token))
            throw new LitsError("Expected newline token, got ".concat(token === null || token === void 0 ? void 0 : token.t, "."));
    }

    var UnparseOptions = /** @class */ (function () {
        function UnparseOptions(unparse, lineLength, col, inlined, locked) {
            if (col === void 0) { col = 0; }
            if (inlined === void 0) { inlined = false; }
            if (locked === void 0) { locked = false; }
            this.unparse = unparse;
            this.lineLength = lineLength;
            this.col = col;
            this.inlined = inlined;
            this.locked = locked;
            this.indent = ' '.repeat(col);
        }
        UnparseOptions.prototype.inc = function (count) {
            if (count === void 0) { count = 1; }
            return new UnparseOptions(this.unparse, this.lineLength, this.col + count, this.inlined, this.locked);
        };
        UnparseOptions.prototype.inline = function () {
            return new UnparseOptions(this.unparse, this.lineLength, this.col, true, this.locked);
        };
        UnparseOptions.prototype.noInline = function () {
            return new UnparseOptions(this.unparse, this.lineLength, this.col, false, this.locked);
        };
        UnparseOptions.prototype.lock = function () {
            return new UnparseOptions(this.unparse, this.lineLength, this.col, this.inlined, true);
        };
        UnparseOptions.prototype.assertNotOverflown = function (value) {
            var _this = this;
            value.split('\n').forEach(function (line, index) {
                var fullLine = (index === 0 && _this.inlined) ? _this.indent + line : line;
                var length = fullLine.length;
                if (length > _this.lineLength)
                    throw new Error("Line length exceeded ".concat(_this.lineLength, " chars, value: \"").concat(fullLine, "\" (").concat(fullLine.length, " chars)"));
            });
            return value;
        };
        return UnparseOptions;
    }());

    function ensureNewlineSeparator(a, b) {
        return !a || !b || a.endsWith('\n') || b.startsWith('\n') ? "".concat(a).concat(b) : "".concat(a, "\n").concat(b);
    }
    function ensureSpaceSeparator(a, b) {
        return !a || !b || a.endsWith(' ') || b.startsWith(' ') || b.startsWith('\n')
            ? "".concat(a).concat(b)
            : "".concat(a, " ").concat(b);
    }
    function applyMetaTokens(value, metaTokens, options) {
        if (!metaTokens) {
            return "".concat(options.inlined ? '' : options.indent).concat(value);
        }
        else {
            var result = "".concat(metaTokensToString(metaTokens.leadingMetaTokens, value)).concat(value).concat((metaTokens === null || metaTokens === void 0 ? void 0 : metaTokens.inlineCommentToken) ? " ".concat(metaTokens.inlineCommentToken.v, "\n") : '');
            return result.split('\n').map(function (line, index) {
                return "".concat(line && (!options.inlined || index > 0) ? options.indent : '').concat(line);
            }).join('\n');
        }
    }
    function metaTokensToString(metaTokens, tokenValue) {
        var isEndBracket = tokenValue === ')' || tokenValue === ']' || tokenValue === '}';
        var isOnlyNewLine = metaTokens.length === 1 && isNewLineToken(metaTokens[0]);
        if (isEndBracket && isOnlyNewLine)
            return '';
        return metaTokens.map(function (metaToken) {
            return isNewLineToken(metaToken) ? metaToken.v : "".concat(metaToken.v, "\n");
        }).join('');
    }

    function unparseParams(_a) {
        var params = _a.params, options = _a.options, prefix = _a.prefix, inline = _a.inline, endBracket = _a.endBracket, _b = _a.body, body = _b === void 0 ? false : _b, _c = _a.name, name = _c === void 0 ? '' : _c, _d = _a.noMultilineInline, noMultilineInline = _d === void 0 ? false : _d, _e = _a.pairs, pairs = _e === void 0 ? false : _e;
        // If no parameters, return the expression with brackets
        if (params.length === 0)
            return "".concat(prefix).concat(endBracket);
        // 1. Try to unparse the parameters as a single line
        try {
            var unparsedParams = pairs
                ? unparseSingleLinePairs(params, options.inline().lock())
                : unparseSingleLineParams(params, options.inline().lock());
            if (!unparsedParams.includes('\n'))
                return options.assertNotOverflown("".concat(prefix, " ").concat(unparsedParams).concat(endBracket));
        }
        catch (error) {
            // If locked, we do not try anything else
            if (options.locked)
                throw error;
        }
        if (pairs) {
            if (name && !name.includes('\n')) {
                try {
                    var unparsedParams = unparseMultilinePairwise(params, options.inline().inc(name.length + 2).lock());
                    var result = options.assertNotOverflown("".concat(prefix, " ").concat(unparsedParams).concat(endBracket));
                    return result;
                }
                catch (_f) {
                }
            }
            try {
                var unparsedParams = unparseMultilinePairwise(params, options.noInline().inc(body ? 2 : 1));
                return options.assertNotOverflown(ensureNewlineSeparator(prefix, "".concat(unparsedParams).concat(endBracket)));
            }
            catch (_g) {
            }
        }
        // 2. Try to unparse the parameters in multiple lines, first parameter on the same line
        // e.g. (round 1 2 3 4 5)
        // ==>  (round 1
        //             2
        //             3
        //             4
        //             5)
        else if (inline) {
            var newOptions = options.inc(name.length + 2).lock();
            try {
                var firstParam = options.unparse(params[0], noMultilineInline ? newOptions.noInline() : newOptions.inline());
                // If the first parameter is multiline, fallback to option 3
                if (!firstParam.startsWith('\n')) {
                    var indentedParams = unparseMultilineParams(params.slice(1), newOptions.noInline());
                    return noMultilineInline
                        ? options.assertNotOverflown("".concat(ensureNewlineSeparator(prefix, ensureNewlineSeparator(firstParam, indentedParams))).concat(endBracket))
                        : options.assertNotOverflown("".concat(prefix, " ").concat(ensureNewlineSeparator(firstParam, indentedParams)).concat(endBracket));
                }
            }
            catch (_h) {
                // Try option 3
            }
        }
        // 3. Try to unparse the parameters in multiple lines
        // e.g. (round 1 2 3 4 5)
        // ==>  (round
        //       1
        //       2
        //       3
        //       4
        //       5)
        return "".concat(ensureNewlineSeparator(prefix, unparseMultilineParams(params, options.noInline().inc(body ? 2 : 1)))).concat(endBracket);
    }
    function unparseSingleLineParams(params, options) {
        return params.reduce(function (acc, param) {
            return ensureSpaceSeparator(acc, options.unparse(param, options.inline()));
        }, '');
    }
    function unparseSingleLinePairs(params, options) {
        return params.reduce(function (acc, param, index) {
            if (index > 0 && index % 2 === 0)
                acc += ',';
            return ensureSpaceSeparator(acc, options.unparse(param, options.inline()));
        }, '');
    }
    function unparseMultilineParams(params, options) {
        return params.reduce(function (acc, param, index) { return ensureNewlineSeparator(acc, options.unparse(param, index === 0 && options.inlined ? options.inline() : options.noInline())); }, '');
    }
    function unparseMultilinePairwise(params, options) {
        var keyLength;
        return params.reduce(function (acc, param, index) {
            var _a, _b;
            if (index % 2 === 0) {
                var key = options.unparse(param, options.inline());
                var leadingMetaTokens = (_b = (_a = param.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.metaTokens.leadingMetaTokens;
                if (index === 0 && options.inlined && leadingMetaTokens && leadingMetaTokens.length > 0)
                    throw new Error('First key with leading meta tokens is not allowed.');
                key = key.trimStart();
                keyLength = key.length;
                key = index === 0 && options.inlined ? key : "".concat(options.indent).concat(key);
                if (key.includes('\n'))
                    throw new Error('Key with new line character is not allowed.');
                return index > 0 ? ensureNewlineSeparator(acc, key) : "".concat(acc).concat(key);
            }
            else {
                var value = options.unparse(param, options.inline().inc(keyLength + 1));
                return ensureSpaceSeparator(acc, value);
            }
        }, '');
    }

    function unparseArrayLiteral(node, options) {
        var _a, _b, _c, _d, _e;
        var startBracket = applyMetaTokens('[', (_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.metaTokens, options);
        var endBracket = applyMetaTokens(']', (_e = (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.lastToken) === null || _d === void 0 ? void 0 : _d.debugData) === null || _e === void 0 ? void 0 : _e.metaTokens, options.inline());
        // If no parameters, return empty array literal
        if (node.p.length === 0)
            return "".concat(startBracket).concat(endBracket);
        var firstElementOptions = startBracket.endsWith('\n') ? options.noInline().inc() : options.inline().inc();
        var unparsedFirstElement = options.unparse(node.p[0], firstElementOptions);
        var params = node.p.slice(1);
        var prefix = startBracket + unparsedFirstElement;
        // 1. Try to unparse the parameters
        try {
            var unparsedParams = unparseSingleLineParams(params, options.inline().lock());
            if (!unparsedParams.includes('\n')) {
                return options.assertNotOverflown("".concat(ensureSpaceSeparator(prefix, unparsedParams)).concat(endBracket));
            }
        }
        catch (error) {
            // If locked, we do not try anything else
            if (options.locked)
                throw error;
        }
        // 2. Try to unparse the parameters in multiple lines
        // e.g. [1 2 3 4 5]
        // ==>  [
        //       1
        //       2
        //       3
        //       4
        //       5]
        return "".concat(ensureNewlineSeparator(prefix, unparseMultilineParams(params, options.noInline().inc()))).concat(endBracket);
    }

    function unparseObjectLiteral(node, options) {
        var _a, _b, _c, _d, _e;
        var startBracket = applyMetaTokens('{', (_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.metaTokens, options);
        var endBracket = applyMetaTokens('}', (_e = (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.lastToken) === null || _d === void 0 ? void 0 : _d.debugData) === null || _e === void 0 ? void 0 : _e.metaTokens, options.inline());
        // If no parameters, return empty object literal
        if (node.p.length === 0)
            return "".concat(startBracket).concat(endBracket);
        var params = node.p;
        if (!startBracket.endsWith('\n')) {
            // 1. Try to unparse the parameters as one line
            try {
                var unparsedParams = unparseSingleLinePairs(params, options.inline().lock());
                if (!unparsedParams.includes('\n')) {
                    var result = "".concat(startBracket).concat(unparsedParams).concat(endBracket);
                    return options.assertNotOverflown(result);
                }
            }
            catch (error) {
                // If locked, we do not try anything else
                if (options.locked)
                    throw error;
            }
        }
        // 2. Try to unparse the parameters pairwise on multiple lines
        // e.g. {:a 1 :b 2 :c 3 :d 4 :e 5}
        // ==>  {:a 1
        //       :b 2
        //       :c 3
        //       :d 4
        //       :e 5}
        var multilineOptions = startBracket.endsWith('\n') ? options.noInline().inc() : options.inline().inc();
        try {
            var paiwise = unparseMultilinePairwise(params, multilineOptions);
            var result = "".concat(startBracket).concat(paiwise).concat(endBracket);
            return options.assertNotOverflown(result);
        }
        catch (_f) {
            // Continue to the next step
        }
        // 3. Try to unparse the parameters in multiple lines
        return startBracket + unparseMultilineParams(node.p, multilineOptions) + endBracket;
    }

    function unparseNormalExpression(node, options) {
        var _a, _b, _c, _d, _e, _f, _g, _h, _j, _k;
        if (isNormalExpressionNode(node)) {
            if (((_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.t) === TokenType.Bracket && node.debugData.token.v === '[')
                return unparseArrayLiteral(node, options);
            else if (((_b = node.debugData) === null || _b === void 0 ? void 0 : _b.token.t) === TokenType.Bracket && node.debugData.token.v === '{')
                return unparseObjectLiteral(node, options);
        }
        var startBracket = applyMetaTokens('(', (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.token.debugData) === null || _d === void 0 ? void 0 : _d.metaTokens, options);
        var endBracket = applyMetaTokens(')', (_g = (_f = (_e = node.debugData) === null || _e === void 0 ? void 0 : _e.lastToken) === null || _f === void 0 ? void 0 : _f.debugData) === null || _g === void 0 ? void 0 : _g.metaTokens, options.inline());
        // if expression node e.g. ("Albert" 2), first parameter is the name ("Albert")
        var nameOptions = startBracket.endsWith('\n') ? options.noInline().inc() : options.inline().inc();
        var name = node.n
            ? applyMetaTokens(node.n, (_k = (_j = (_h = node.debugData) === null || _h === void 0 ? void 0 : _h.nameToken) === null || _j === void 0 ? void 0 : _j.debugData) === null || _k === void 0 ? void 0 : _k.metaTokens, nameOptions)
            : options.unparse(node.p[0], nameOptions);
        var params = node.n ? node.p : node.p.slice(1);
        var prefix = startBracket + name;
        var inline = !name.includes('\n');
        return unparseParams({ params: params, options: options, prefix: prefix, inline: inline, name: name, endBracket: endBracket });
    }

    function unparseCond(node, options) {
        var _a, _b, _c, _d, _e, _f, _g, _h;
        var startBracket = applyMetaTokens('(', (_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.metaTokens, options);
        var endBracket = applyMetaTokens(')', (_e = (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.lastToken) === null || _d === void 0 ? void 0 : _d.debugData) === null || _e === void 0 ? void 0 : _e.metaTokens, options.inline());
        var name = applyMetaTokens(node.n, (_h = (_g = (_f = node.debugData) === null || _f === void 0 ? void 0 : _f.nameToken) === null || _g === void 0 ? void 0 : _g.debugData) === null || _h === void 0 ? void 0 : _h.metaTokens, options.inline());
        var prefix = startBracket + name;
        var inline = !name.includes('\n');
        return unparseParams({
            params: node.p,
            options: options,
            prefix: prefix,
            inline: inline,
            name: name,
            endBracket: endBracket,
            body: true,
            pairs: true,
        });
    }

    function unparseDo(node, options) {
        var _a, _b, _c, _d, _e, _f, _g, _h;
        var startBracket = applyMetaTokens('(', (_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.metaTokens, options);
        var endBracket = applyMetaTokens(')', (_e = (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.lastToken) === null || _d === void 0 ? void 0 : _d.debugData) === null || _e === void 0 ? void 0 : _e.metaTokens, options.inline());
        var name = applyMetaTokens(node.n, (_h = (_g = (_f = node.debugData) === null || _f === void 0 ? void 0 : _f.nameToken) === null || _g === void 0 ? void 0 : _g.debugData) === null || _h === void 0 ? void 0 : _h.metaTokens, options.inline());
        var prefix = startBracket + name;
        var inline = !name.includes('\n');
        return unparseParams({
            params: node.p,
            options: options,
            prefix: prefix,
            inline: inline,
            name: name,
            endBracket: endBracket,
            body: true,
        });
    }

    function unparseBindings(node, options) {
        var _a, _b, _c, _d, _e, _f, _g, _h, _j, _k, _l, _m, _o, _p, _q, _r, _s;
        var startBracket = applyMetaTokens('[', (_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.metaTokens, options);
        // If no parameters, return empty array literal
        if (node.p.length === 0) {
            var endBracket = applyMetaTokens(']', (_e = (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.lastToken) === null || _d === void 0 ? void 0 : _d.debugData) === null || _e === void 0 ? void 0 : _e.metaTokens, options.inline());
            return "".concat(startBracket).concat(endBracket);
        }
        var params = node.p;
        // 1. Try to unparse the bindings as one line
        try {
            var unparsedParams = unparseSingleLinePairs(params, options.inline().lock());
            var endBracket = applyMetaTokens(']', (_h = (_g = (_f = node.debugData) === null || _f === void 0 ? void 0 : _f.lastToken) === null || _g === void 0 ? void 0 : _g.debugData) === null || _h === void 0 ? void 0 : _h.metaTokens, options.inline());
            if (!unparsedParams.includes('\n')) {
                var result = "".concat(startBracket).concat(unparsedParams).concat(endBracket);
                return options.assertNotOverflown(result);
            }
        }
        catch (error) {
            // If locked, we do not try anything else
            if (options.locked)
                throw error;
        }
        // 2. Try to unparse the bindings pairwise on multiple lines
        // e.g. [a 1 b 2]
        // ==>  [a 1
        //       b 2]
        try {
            var endBracket = applyMetaTokens(']', (_l = (_k = (_j = node.debugData) === null || _j === void 0 ? void 0 : _j.lastToken) === null || _k === void 0 ? void 0 : _k.debugData) === null || _l === void 0 ? void 0 : _l.metaTokens, options.inline());
            var result = startBracket + unparseMultilinePairwise(params, options.inline().inc()) + endBracket;
            return options.assertNotOverflown(result);
        }
        catch (_t) {
            // 2. Unparse the bindings on multiple lines
            // e.g. [a 1 b 2]
            // ==>  [a
            //       1,
            //       b
            //       2]
            var unparsedParams = unparseMultilineParams(params, options.inline().inc());
            var endBracket = unparsedParams.endsWith('\n')
                ? applyMetaTokens(']', (_p = (_o = (_m = node.debugData) === null || _m === void 0 ? void 0 : _m.lastToken) === null || _o === void 0 ? void 0 : _o.debugData) === null || _p === void 0 ? void 0 : _p.metaTokens, options.noInline())
                : applyMetaTokens(']', (_s = (_r = (_q = node.debugData) === null || _q === void 0 ? void 0 : _q.lastToken) === null || _r === void 0 ? void 0 : _r.debugData) === null || _s === void 0 ? void 0 : _s.metaTokens, options.inline());
            return startBracket + unparseMultilineParams(params, options.inline().inc()) + endBracket;
        }
    }

    function unparseIfLet(node, options) {
        var _a, _b, _c, _d, _e, _f, _g, _h;
        var startBracket = applyMetaTokens('(', (_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.metaTokens, options);
        var endBracket = applyMetaTokens(')', (_e = (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.lastToken) === null || _d === void 0 ? void 0 : _d.debugData) === null || _e === void 0 ? void 0 : _e.metaTokens, options.inline());
        var name = applyMetaTokens(node.n, (_h = (_g = (_f = node.debugData) === null || _f === void 0 ? void 0 : _f.nameToken) === null || _g === void 0 ? void 0 : _g.debugData) === null || _h === void 0 ? void 0 : _h.metaTokens, options.inline());
        var letArray = node.debugData ? node.debugData.bindingArray : createLetArray$1(node.b);
        var inc = name.includes('\n') ? 1 : name.length + 2;
        var unparsedLetArray = unparseBindings(letArray, options.inc(inc).inline());
        var prefix = "".concat(startBracket + name, " ").concat(unparsedLetArray);
        var inline = !(name + unparsedLetArray).includes('\n');
        return unparseParams({
            params: node.p,
            options: options,
            prefix: prefix,
            inline: inline,
            endBracket: endBracket,
            body: true,
            noMultilineInline: true,
        });
    }
    function createLetArray$1(bindingNode) {
        var params = [
            {
                t: AstNodeType.Name,
                n: undefined,
                p: [],
                debugData: undefined,
                v: bindingNode.n,
            },
            bindingNode.v,
        ];
        var node = {
            t: AstNodeType.NormalExpression,
            n: 'array',
            p: params,
            debugData: undefined,
        };
        return node;
    }

    function unparseLet(node, options) {
        var _a, _b, _c, _d, _e, _f, _g, _h;
        var startBracket = applyMetaTokens('(', (_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.metaTokens, options);
        var endBracket = applyMetaTokens(')', (_e = (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.lastToken) === null || _d === void 0 ? void 0 : _d.debugData) === null || _e === void 0 ? void 0 : _e.metaTokens, options.inline());
        var name = applyMetaTokens(node.n, (_h = (_g = (_f = node.debugData) === null || _f === void 0 ? void 0 : _f.nameToken) === null || _g === void 0 ? void 0 : _g.debugData) === null || _h === void 0 ? void 0 : _h.metaTokens, options.inline());
        var letArray = node.debugData ? node.debugData.bindingArray : createLetArray(node.bs);
        var inc = name.includes('\n') ? 1 : name.length + 2;
        var unparsedLetArray = unparseBindings(letArray, options.inc(inc).inline());
        var prefix = "".concat(startBracket + name, " ").concat(unparsedLetArray);
        var inline = !(name + unparsedLetArray).includes('\n');
        return unparseParams({
            params: node.p,
            options: options,
            prefix: prefix,
            inline: inline,
            endBracket: endBracket,
            body: true,
            noMultilineInline: true,
        });
    }
    function createLetArray(bindingNodes) {
        var params = bindingNodes.flatMap(function (binding) { return [
            {
                t: AstNodeType.Name,
                n: undefined,
                p: [],
                debugData: undefined,
                v: binding.n,
            },
            binding.v,
        ]; });
        var node = {
            t: AstNodeType.NormalExpression,
            n: 'array',
            p: params,
            debugData: undefined,
        };
        return node;
    }

    function unparseIfOrWhenLike(node, options) {
        var _a, _b, _c, _d, _e, _f, _g, _h;
        var startBracket = applyMetaTokens('(', (_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.debugData) === null || _b === void 0 ? void 0 : _b.metaTokens, options);
        var endBracket = applyMetaTokens(')', (_e = (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.lastToken) === null || _d === void 0 ? void 0 : _d.debugData) === null || _e === void 0 ? void 0 : _e.metaTokens, options.inline());
        var nameOptions = startBracket.endsWith('\n') ? options.noInline().inc() : options.inline().inc();
        var name = applyMetaTokens(node.n, (_h = (_g = (_f = node.debugData) === null || _f === void 0 ? void 0 : _f.nameToken) === null || _g === void 0 ? void 0 : _g.debugData) === null || _h === void 0 ? void 0 : _h.metaTokens, nameOptions);
        var inc = name.includes('\n') ? 1 : name.length + 2;
        var unparsedCondition = options.unparse(node.p[0], options.inc(inc).inline());
        var prefix = "".concat(startBracket + name, " ").concat(unparsedCondition);
        var params = node.p.slice(1);
        // const inline = !(name + unparsedCondition).includes('\n')
        var inline = false;
        return unparseParams({ params: params, options: options, prefix: prefix, inline: inline, name: name, endBracket: endBracket, body: node.n === 'when' || node.n === 'when-not' });
    }

    // type ExpressionWithSingleParamNode = Pick<NormalExpressionNode, 'debug' | 'n'> & { p: AstNode }
    // function expressionWithSingleParamUnparser(astNode: ExpressionWithSingleParamNode, options: UnparseOptions) {
    //   return unparseNormalExpressionNode({ ...astNode, p: [astNode.p] }, options)
    // }
    var specialExpressionUnparser = {
        'and': unparseNormalExpression,
        'comment': unparseNormalExpression,
        'cond': unparseCond,
        'declared?': unparseNormalExpression,
        // 'defn': (astNode: DefnNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
        'def': unparseNormalExpression,
        // 'defns': (astNode: DefnsNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
        'defs': unparseNormalExpression,
        'do': unparseDo,
        // 'doseq': (astNode: DoSeqNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
        // 'fn': (astNode: FnNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
        // 'for': (astNode: ForNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
        'if-let': unparseIfLet,
        'if': unparseIfOrWhenLike,
        'if-not': unparseIfOrWhenLike,
        'let': unparseLet,
        // 'loop': (astNode: LoopNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
        'or': unparseNormalExpression,
        '??': unparseNormalExpression,
        'recur': unparseNormalExpression,
        'time!': unparseNormalExpression,
        'throw': unparseNormalExpression,
        // 'try': (astNode: TryNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
        // 'when-first': (astNode: WhenFirstNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
        // 'when-let': (astNode: WhenLetNode, options: UnparseOptions) => unparseNormalExpressionNode({ ...astNode, t }, options),
        'when': unparseIfOrWhenLike,
        'when-not': unparseIfOrWhenLike,
    };
    function unparseSpecialExpression(node, options) {
        var unparser = specialExpressionUnparser[node.n];
        return unparser(node, options);
    }

    var unparse = function (node, options) {
        var _a, _b, _c, _d, _e, _f, _g, _h, _j, _k;
        switch (node.t) {
            case AstNodeType.String:
                return ((_b = (_a = node.debugData) === null || _a === void 0 ? void 0 : _a.token.o) === null || _b === void 0 ? void 0 : _b.s)
                    ? applyMetaTokens(":".concat(node.v), (_d = (_c = node.debugData) === null || _c === void 0 ? void 0 : _c.token.debugData) === null || _d === void 0 ? void 0 : _d.metaTokens, options) // Keyword
                    : applyMetaTokens("\"".concat(node.v, "\""), (_f = (_e = node.debugData) === null || _e === void 0 ? void 0 : _e.token.debugData) === null || _f === void 0 ? void 0 : _f.metaTokens, options);
            case AstNodeType.Number:
            case AstNodeType.Name:
            case AstNodeType.Modifier:
            case AstNodeType.ReservedName:
                return applyMetaTokens(node.v, (_h = (_g = node.debugData) === null || _g === void 0 ? void 0 : _g.token.debugData) === null || _h === void 0 ? void 0 : _h.metaTokens, options);
            case AstNodeType.Comment:
                return "".concat(applyMetaTokens(node.v, (_k = (_j = node.debugData) === null || _j === void 0 ? void 0 : _j.token.debugData) === null || _k === void 0 ? void 0 : _k.metaTokens, options), "\n");
            case AstNodeType.NormalExpression: {
                return unparseNormalExpression(node, options);
            }
            case AstNodeType.SpecialExpression:
                return unparseSpecialExpression(node, options);
        }
    };
    function unparseAst(ast, lineLength) {
        var options = new UnparseOptions(unparse, lineLength || Number.MAX_SAFE_INTEGER);
        var result = ast.b.reduce(function (acc, node) {
            var nodeUnparsed = unparse(node, options);
            return ensureNewlineSeparator(acc, nodeUnparsed);
        }, '');
        return result.trim();
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
        Lits.prototype.format = function (program, params) {
            var _a;
            if (params === void 0) { params = {}; }
            var lineLength = (_a = params.lineLength) !== null && _a !== void 0 ? _a : 80;
            var ast = this.generateAst(program, params);
            return unparseAst(ast, lineLength);
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
            var _a;
            if (tokenizeParams === void 0) { tokenizeParams = {}; }
            return tokenize$1(program, __assign(__assign({}, tokenizeParams), { debug: (_a = tokenizeParams.debug) !== null && _a !== void 0 ? _a : this.debug }));
        };
        Lits.prototype.parse = function (tokenStream) {
            return parse$1(tokenStream);
        };
        Lits.prototype.evaluate = function (ast, params) {
            var contextStack = createContextStack(params);
            return evaluate(ast, contextStack);
        };
        Lits.prototype.apply = function (fn, fnParams, params) {
            var _a;
            if (params === void 0) { params = {}; }
            var fnName = 'FN_2eb7b316-471c-5bfa-90cb-d3dfd9164a59';
            var paramsString = fnParams
                .map(function (_, index) {
                return "".concat(fnName, "_").concat(index);
            })
                .join(' ');
            var program = "(".concat(fnName, " ").concat(paramsString, ")");
            var ast = this.generateAst(program, params);
            var hostValues = fnParams.reduce(function (result, param, index) {
                result["".concat(fnName, "_").concat(index)] = param;
                return result;
            }, (_a = {}, _a[fnName] = fn, _a));
            params.values = __assign(__assign({}, params.values), hostValues);
            return this.evaluate(ast, params);
        };
        Lits.prototype.generateAst = function (program, params) {
            var _a, _b;
            if (this.astCache) {
                var cachedAst = this.astCache.get(program);
                if (cachedAst)
                    return cachedAst;
            }
            var tokenStream = this.tokenize(program, { debug: (_a = params.debug) !== null && _a !== void 0 ? _a : this.debug, filePath: params.filePath });
            var ast = this.parse(tokenStream);
            (_b = this.astCache) === null || _b === void 0 ? void 0 : _b.set(program, ast);
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
        var hijacker = hijackConsole();
        var result = '';
        try {
            result = getLits('debug').format(code, { lineLength: getLitsCodeCols() });
            if (selectedCode.code) {
                setLitsCode("".concat(selectedCode.leadingCode).concat(result).concat(selectedCode.trailingCode), true);
            }
            else {
                setLitsCode(result, true);
            }
        }
        catch (error) {
            appendOutput(error, 'error');
            return;
        }
        finally {
            hijacker.releaseConsole();
            if (selectedCode.code) {
                saveState({
                    'focused-panel': 'lits-code',
                    'lits-code-selection-start': selectedCode.selectionStart,
                    'lits-code-selection-end': selectedCode.selectionStart + result.length,
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
    function getLitsCodeCols() {
        // Create a temporary element
        var _a = window.getComputedStyle(elements.litsTextArea), font = _a.font, paddingLeft = _a.paddingLeft, paddingRight = _a.paddingRight;
        var tempElement = document.createElement('span');
        tempElement.style.font = font;
        tempElement.style.visibility = 'hidden';
        tempElement.style.whiteSpace = 'pre';
        tempElement.textContent = 'M'; // Use a common monospace character
        // Append the element to the body
        document.body.appendChild(tempElement);
        // Measure the width of the character
        var characterWidth = tempElement.getBoundingClientRect().width;
        var textAreawidth = elements.litsTextArea.clientWidth
            - Number.parseInt(paddingLeft)
            - Number.parseInt(paddingRight);
        // Remove the temporary element
        document.body.removeChild(tempElement);
        return Math.max(1, Math.floor(textAreawidth / characterWidth));
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
