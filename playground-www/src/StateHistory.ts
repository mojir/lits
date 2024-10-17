export interface HistoryStatus {
  canUndo: boolean
  canRedo: boolean
}

export interface HistoryEntry {
  text: string
  selectionStart: number
  selectionEnd: number
}

export class StateHistory {
  private history: HistoryEntry[] = []
  private index: number
  private listener: (status: HistoryStatus) => void
  private lastStatus: HistoryStatus = { canUndo: false, canRedo: false }
  constructor(initial: HistoryEntry, listener: (status: HistoryStatus) => void) {
    this.history.push(initial)
    this.index = 0
    this.listener = listener
  }

  private get canUndo() {
    return this.index > 0
  }

  private get canRedo() {
    return this.index < this.history.length - 1
  }

  private get current(): HistoryEntry {
    return this.history[this.index]!
  }

  public push(entry: HistoryEntry) {
    if (entry.text !== this.current.text) {
      this.history.splice(this.index + 1)
      this.history.push(entry)
      this.index = this.history.length - 1
      this.notify()
    }
    else {
      this.replace(entry)
    }
  }

  private replace(entry: HistoryEntry) {
    this.current.text = entry.text
    this.current.selectionStart = entry.selectionStart
    this.current.selectionEnd = entry.selectionEnd
    this.notify()
  }

  public undo(): HistoryEntry {
    if (!this.canUndo)
      throw new Error('Cannot undo')
    this.index -= 1
    this.notify()
    return this.history[this.index]!
  }

  public redo(): HistoryEntry {
    if (!this.canRedo)
      throw new Error('Cannot redo')
    this.index += 1
    this.notify()
    return this.current
  }

  peek(): HistoryEntry {
    return this.current
  }

  reset(initialState: HistoryEntry) {
    this.history = [initialState]
    this.index = 0
    this.notify()
  }

  notify() {
    const status = { canUndo: this.canUndo, canRedo: this.canRedo }
    if (status.canUndo !== this.lastStatus.canUndo || status.canRedo !== this.lastStatus.canRedo) {
      this.lastStatus = status
      setTimeout(() => this.listener(status), 0)
    }
  }
}
