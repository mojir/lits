import { vi } from "vitest"

vi.mock('@mojir/pretty-pi', () => {
  return {
    prettyPi: vi.fn().mockReturnValue('π'),
  }
})