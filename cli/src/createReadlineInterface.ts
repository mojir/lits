#!/usr/bin/env node
/* eslint-disable node/prefer-global/process */

import type { ReadLine, ReadLineOptions } from 'node:readline'
import readline from 'node:readline'
import path from 'node:path'
import fs from 'node:fs'
import os from 'node:os'
import type { UnknownRecord } from '../../src/interface'

const historyDir = path.join(os.homedir(), '.config')
const historyFile = path.join(historyDir, 'lits_history.txt')

function isHistoryEnabled() {
  if (fs.existsSync(historyFile))
    return true

  try {
    fs.openSync(historyFile, 'w')
  }
  catch (e) {
    console.error(`No history for you!
If you would like to enable history persistence, make sure the directory "${path.resolve(
  historyDir,
)}" exists and is writable.
`)
    return false
  }
  return true
}

type Options = Required<Pick<ReadLineOptions, 'completer' | 'historySize' | 'prompt'>>

export function createReadlineInterface(options: Options): ReadLine {
  const readlineOptions: ReadLineOptions = {
    input: process.stdin,
    output: process.stdout,
    ...options,
  }
  const historyEnabled = isHistoryEnabled()
  const history = historyEnabled
    ? fs.readFileSync(historyFile, 'utf8')
        .toString()
        .split('\n')
        .slice(0, -1)
        .reverse()
        .slice(0, options.historySize)
    : []

  ;(readline as UnknownRecord).kHistorySize = Math.max((readline as UnknownRecord).kHistorySize as number, options.historySize)

  // eslint-disable-next-line ts/no-unsafe-assignment
  const rl = readline.createInterface(readlineOptions) as any

  if (historyEnabled) {
    // eslint-disable-next-line ts/no-unsafe-assignment, ts/no-unsafe-member-access
    const oldAddHistory = rl._addHistory

    // eslint-disable-next-line ts/no-unsafe-member-access
    rl._addHistory = function () {
      // eslint-disable-next-line ts/no-unsafe-assignment, ts/no-unsafe-member-access
      const last = rl.history[0]
      // eslint-disable-next-line ts/no-unsafe-assignment, ts/no-unsafe-call, ts/no-unsafe-member-access
      const line = oldAddHistory.call(rl)

      // eslint-disable-next-line ts/no-unsafe-member-access
      if (line.length > 0 && line !== last)
        fs.appendFileSync(historyFile, `${line}\n`)

      // eslint-disable-next-line ts/no-unsafe-return
      return line
    }

    // eslint-disable-next-line ts/no-unsafe-member-access
    if (Array.isArray(rl.history))
      // eslint-disable-next-line ts/no-unsafe-call, ts/no-unsafe-member-access
      rl.history.push(...history)
  }

  // eslint-disable-next-line ts/no-unsafe-return
  return rl
}
