import path from 'path'
import { LitsError } from '../../../src/errors'
import { getErrorYaml, runTest } from '../../../src/testFramework'

describe(`testFramework`, () => {
  test(`expecting .lits file`, () => {
    expect(() => runTest({ testPath: path.join(__dirname, `empty.test`) })).toThrow()
  })
  test(`illegal import`, () => {
    expect(() => runTest({ testPath: path.join(__dirname, `illegal-import.test.lits`) })).toThrow()
  })
  test(`empty test`, () => {
    const testResult = runTest({ testPath: path.join(__dirname, `empty.test.lits`) })
    expect(testResult.success).toBe(true)
    expect(testResult.tap).toBe(`TAP version 13\n1..0\n`)
  })
  test(`duplicate test names`, () => {
    const testResult = runTest({ testPath: path.join(__dirname, `duplicate-test-name.test.lits`) })
    expect(testResult.success).toBe(false)
    expect(testResult.tap).toBe(`TAP version 13
Bail out! Duplicate test name add
`)
  })
  test(`missing test name`, () => {
    const testResult = runTest({ testPath: path.join(__dirname, `missing-test-name.test.lits`) })
    expect(testResult.success).toBe(false)
    expect(testResult.tap).toBe(`TAP version 13
Bail out! Missing test name on line 3
`)
  })
  test(`success`, () => {
    const testResult = runTest({ testPath: path.join(__dirname, `test.test.lits`) })
    expect(testResult.success).toBe(true)
    expect(testResult.tap).toBe(`TAP version 13
1..2
ok 1 add
ok 2 sub
`)
  })

  test(`success import plus`, () => {
    const testResult = runTest({
      testPath: path.join(__dirname, `test-import-plus.test.lits`),
    })
    expect(testResult.success).toBe(true)
    expect(testResult.tap).toBe(`TAP version 13
1..2
ok 1 add
ok 2 sub
`)
  })

  test(`skip-test`, () => {
    const testResult = runTest({ testPath: path.join(__dirname, `skip.test.lits`) })
    expect(testResult.success).toBe(true)
    expect(testResult.tap).toBe(`TAP version 13
1..2
ok 1 add
ok 2 sub # skip
`)
  })

  test(`1 fail.`, () => {
    const testResult = runTest({ testPath: path.join(__dirname, `one-success.test.lits`) })
    expect(testResult.success).toBe(false)
    expect(testResult.tap).toBe(`TAP version 13
1..2
ok 1 add
not ok 2 sub
  ---
  error: "AssertionError"
  message: "Expected 3 to be -1."
  location: "${path.resolve(__dirname, `one-success.test.lits`)}:10:2"
  code:
    - "(assert= (sub one 2) -1)"
    - " ^                      "
  ...
`)
  })

  test(`1 fail, 1 not matching pattern`, () => {
    const testResult = runTest({ testPath: path.join(__dirname, `failure-test.lits`), testNamePattern: /ad/ })
    expect(testResult.success).toBe(false)
    expect(testResult.tap).toBe(`TAP version 13
1..2
not ok 1 add
  ---
  error: "AssertionError"
  message: "Expected -1 to be 3."
  location: "${path.resolve(__dirname, `failure-test.lits`)}:7:2"
  code:
    - "(assert= (add one 2) 3)"
    - " ^                     "
  ...
ok 2 sub # skip - Not matching testNamePattern /ad/
`)
  })

  test(`2 fail`, () => {
    const testResult = runTest({ testPath: path.join(__dirname, `failure-test.lits`) })
    expect(testResult.success).toBe(false)
    expect(testResult.tap).toBe(`TAP version 13
1..2
not ok 1 add
  ---
  error: "AssertionError"
  message: "Expected -1 to be 3."
  location: "${path.resolve(__dirname, `failure-test.lits`)}:7:2"
  code:
    - "(assert= (add one 2) 3)"
    - " ^                     "
  ...
not ok 2 sub
  ---
  error: "AssertionError"
  message: "Expected 3 to be -1."
  location: "${path.resolve(__dirname, `failure-test.lits`)}:10:2"
  code:
    - "(assert= (sub one 2) -1)"
    - " ^                      "
  ...
`)
  })

  test(`Broken include`, () => {
    const testResult = runTest({
      testPath: path.join(__dirname, `broken-include.test.lits`),
    })
    expect(testResult.success).toBe(false)
    expect(testResult.tap).toBe(`TAP version 13
1..2
not ok 1 add
  ---
  error: "UndefinedSymbolError"
  message: "Undefined symbol '++'."
  location: "${path.resolve(__dirname, `broken-plus-lib.lits`)}:1:19"
  code:
    - "(defn plus [a b] (++ a b))"
    - "                  ^       "
  ...
ok 2 sub
`)
  })

  test(`deep equals fails`, () => {
    const testResult = runTest({ testPath: path.join(__dirname, `object-diff.test.lits`) })
    expect(testResult.success).toBe(false)
    expect(testResult.tap).toBe(`TAP version 13
1..1
not ok 1 equals
  ---
  error: "AssertionError"
  message: |
    Expected
    {
      "id": "id1",
      "val": "value1"
    }
    to deep equal
    {
      "id": "id2",
      "val": "value2"
    }.
  location: "${path.resolve(__dirname, `object-diff.test.lits`)}:5:2"
  code:
    - "(assert-equal obj-a obj-b)"
    - " ^                        "
  ...
`)
  })
  test(`getErrorYaml`, () => {
    const error = new LitsError(`Error`, {
      code: `x`,
      column: 1,
      line: 1,
    })
    expect(getErrorYaml(error)).toBe(`
  ---
  error: "LitsError"
  message: "Error"
  location: "(1:1)"
  code:
    - "x"
    - "^"
  ...
`)
  })
})
