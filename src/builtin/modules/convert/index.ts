import { assertNumber } from '../../../typeGuards/number'
import { toFixedArity } from '../../../utils/arity'
import type { BuiltinNormalExpressions } from '../../interface'
import type { LitsModule } from '../interface'

// --- Conversion factor tables ---
// Each category maps unit abbreviations to a factor relative to a base unit.
// To convert from unit A to unit B: value * (factorA / factorB)
// Temperature is special-cased (non-linear).

interface UnitCategory {
  baseUnit: string
  units: Record<string, number>
  description: string
}

const lengthUnits: UnitCategory = {
  baseUnit: 'm',
  description: 'length',
  units: {
    angstrom: 1e-10,
    um: 0.000001,
    mm: 0.001,
    cm: 0.01,
    m: 1,
    km: 1000,
    in: 0.0254,
    ft: 0.3048,
    yd: 0.9144,
    mi: 1609.344,
    nmi: 1852,
  },
}

const weightUnits: UnitCategory = {
  baseUnit: 'kg',
  description: 'weight',
  units: {
    mg: 0.000001,
    g: 0.001,
    kg: 1,
    t: 1000,
    oz: 0.028349523125,
    lb: 0.45359237,
    st: 6.35029318,
  },
}

const volumeUnits: UnitCategory = {
  baseUnit: 'l',
  description: 'volume',
  units: {
    'ml': 0.001,
    'cl': 0.01,
    'dl': 0.1,
    'l': 1,
    'tsp': 0.00492892159375,
    'tbsp': 0.01478676478125,
    'fl-oz': 0.0295735295625,
    'cup': 0.2365882365,
    'pt': 0.473176473,
    'qt': 0.946352946,
    'gal': 3.785411784,
  },
}

const timeUnits: UnitCategory = {
  baseUnit: 's',
  description: 'time',
  units: {
    ms: 0.001,
    s: 1,
    min: 60,
    h: 3600,
    day: 86400,
    week: 604800,
  },
}

const areaUnits: UnitCategory = {
  baseUnit: 'm2',
  description: 'area',
  units: {
    mm2: 0.000001,
    cm2: 0.0001,
    m2: 1,
    km2: 1000000,
    in2: 0.00064516,
    ft2: 0.09290304,
    yd2: 0.83612736,
    acre: 4046.8564224,
    hectare: 10000,
  },
}

const speedUnits: UnitCategory = {
  baseUnit: 'm/s',
  description: 'speed',
  units: {
    'm/s': 1,
    'km/h': 1 / 3.6,
    'mph': 0.44704,
    'kn': 0.514444,
    'ft/s': 0.3048,
  },
}

const dataUnits: UnitCategory = {
  baseUnit: 'b',
  description: 'data',
  units: {
    b: 1,
    kb: 1000,
    mb: 1000000,
    gb: 1e9,
    tb: 1e12,
    pb: 1e15,
  },
}

const pressureUnits: UnitCategory = {
  baseUnit: 'pa',
  description: 'pressure',
  units: {
    pa: 1,
    kpa: 1000,
    bar: 100000,
    atm: 101325,
    psi: 6894.757293168,
    mmhg: 133.322387415,
  },
}

const energyUnits: UnitCategory = {
  baseUnit: 'j',
  description: 'energy',
  units: {
    j: 1,
    kj: 1000,
    cal: 4.184,
    kcal: 4184,
    wh: 3600,
    kwh: 3600000,
    btu: 1055.06,
  },
}

const powerUnits: UnitCategory = {
  baseUnit: 'w',
  description: 'power',
  units: {
    w: 1,
    kw: 1000,
    mw: 1000000,
    hp: 745.7,
  },
}

const frequencyUnits: UnitCategory = {
  baseUnit: 'hz',
  description: 'frequency',
  units: {
    hz: 1,
    khz: 1000,
    mhz: 1000000,
    ghz: 1000000000,
  },
}

const angleUnits: UnitCategory = {
  baseUnit: 'rad',
  description: 'angle',
  units: {
    deg: Math.PI / 180,
    rad: 1,
    grad: Math.PI / 200,
    turn: 2 * Math.PI,
  },
}

// --- Temperature conversion functions ---

function celsiusTo(unit: 'c' | 'f' | 'k', value: number): number {
  switch (unit) {
    case 'c': return value
    case 'f': return value * 9 / 5 + 32
    case 'k': return value + 273.15
  }
}

function toCelsius(unit: 'c' | 'f' | 'k', value: number): number {
  switch (unit) {
    case 'c': return value
    case 'f': return (value - 32) * 5 / 9
    case 'k': return value - 273.15
  }
}

const temperatureUnits = ['c', 'f', 'k'] as const

const unitDescriptions: Record<string, string> = {
  // Length
  'angstrom': 'ångströms',
  'um': 'micrometers',
  'mm': 'millimeters',
  'cm': 'centimeters',
  'm': 'meters',
  'km': 'kilometers',
  'in': 'inches',
  'ft': 'feet',
  'yd': 'yards',
  'mi': 'miles',
  'nmi': 'nautical miles',
  // Weight
  'mg': 'milligrams',
  'g': 'grams',
  'kg': 'kilograms',
  't': 'metric tons',
  'oz': 'ounces',
  'lb': 'pounds',
  'st': 'stones',
  // Volume
  'ml': 'milliliters',
  'cl': 'centiliters',
  'dl': 'deciliters',
  'l': 'liters',
  'tsp': 'teaspoons',
  'tbsp': 'tablespoons',
  'fl-oz': 'fluid ounces',
  'cup': 'cups',
  'pt': 'pints',
  'qt': 'quarts',
  'gal': 'gallons',
  // Time
  'ms': 'milliseconds',
  's': 'seconds',
  'min': 'minutes',
  'h': 'hours',
  'day': 'days',
  'week': 'weeks',
  // Area
  'mm2': 'square millimeters',
  'cm2': 'square centimeters',
  'm2': 'square meters',
  'km2': 'square kilometers',
  'in2': 'square inches',
  'ft2': 'square feet',
  'yd2': 'square yards',
  'acre': 'acres',
  'hectare': 'hectares',
  // Speed
  'm/s': 'meters per second',
  'km/h': 'kilometers per hour',
  'mph': 'miles per hour',
  'kn': 'knots',
  'ft/s': 'feet per second',
  // Data
  'b': 'bytes',
  'kb': 'kilobytes',
  'mb': 'megabytes',
  'gb': 'gigabytes',
  'tb': 'terabytes',
  'pb': 'petabytes',
  // Pressure
  'pa': 'pascals',
  'kpa': 'kilopascals',
  'bar': 'bars',
  'atm': 'atmospheres',
  'psi': 'pounds per square inch',
  'mmhg': 'millimeters of mercury',
  // Energy
  'j': 'joules',
  'kj': 'kilojoules',
  'cal': 'calories',
  'kcal': 'kilocalories',
  'wh': 'watt-hours',
  'kwh': 'kilowatt-hours',
  'btu': 'British thermal units',
  // Power
  'w': 'watts',
  'kw': 'kilowatts',
  'mw': 'megawatts',
  'hp': 'horsepower',
  // Frequency
  'hz': 'hertz',
  'khz': 'kilohertz',
  'mhz': 'megahertz',
  'ghz': 'gigahertz',
  // Angle
  'deg': 'degrees',
  'rad': 'radians',
  'grad': 'gradians',
  'turn': 'turns',
  // Temperature
  'c': 'Celsius',
  'f': 'Fahrenheit',
  'k': 'Kelvin',
}

// --- Generate linear conversion functions ---

function generateLinearConversions(category: UnitCategory): BuiltinNormalExpressions {
  const result: BuiltinNormalExpressions = {}
  const unitNames = Object.keys(category.units)

  for (const from of unitNames) {
    for (const to of unitNames) {
      if (from === to)
        continue

      const fromFactor = category.units[from]!
      const toFactor = category.units[to]!
      const conversionFactor = fromFactor / toFactor
      const fnName = `${from}->${to}`
      const fromDesc = unitDescriptions[from]!
      const toDesc = unitDescriptions[to]!

      const seeAlso = [
        `convert.${to}->${from}`,
        ...unitNames
          .filter(u => u !== from && u !== to)
          .map(u => `convert.${from}->${u}`),
      ]

      result[fnName] = {
        evaluate: ([value], sourceCodeInfo): number => {
          assertNumber(value, sourceCodeInfo)
          return value * conversionFactor
        },
        arity: toFixedArity(1),
        docs: {
          category: 'convert',
          returns: { type: 'number' },
          args: {
            value: { type: 'number', description: `Value in ${fromDesc}` },
          },
          variants: [{ argumentNames: ['value'] }],
          description: `Converts a value from ${fromDesc} (\`${from}\`) to ${toDesc} (\`${to}\`).`,
          seeAlso,
          examples: [
            `let { ${fnName} } = import(convert);
${fnName}(1)`,
          ],
        },
      }
    }
  }
  return result
}

// --- Generate temperature conversion functions ---

function generateTemperatureConversions(): BuiltinNormalExpressions {
  const result: BuiltinNormalExpressions = {}

  for (const from of temperatureUnits) {
    for (const to of temperatureUnits) {
      if (from === to)
        continue

      const fnName = `${from}->${to}`
      const fromDesc = unitDescriptions[from]!
      const toDesc = unitDescriptions[to]!

      const seeAlso = [
        `convert.${to}->${from}`,
        ...temperatureUnits
          .filter(u => u !== from && u !== to)
          .map(u => `convert.${from}->${u}`),
      ]

      result[fnName] = {
        evaluate: ([value], sourceCodeInfo): number => {
          assertNumber(value, sourceCodeInfo)
          const celsius = toCelsius(from, value)
          return celsiusTo(to, celsius)
        },
        arity: toFixedArity(1),
        docs: {
          category: 'convert',
          returns: { type: 'number' },
          args: {
            value: { type: 'number', description: `Value in ${fromDesc}` },
          },
          variants: [{ argumentNames: ['value'] }],
          description: `Converts a temperature from ${fromDesc} (\`${from}\`) to ${toDesc} (\`${to}\`).`,
          seeAlso,
          examples: [
            `let { ${fnName} } = import(convert);
${fnName}(100)`,
          ],
        },
      }
    }
  }
  return result
}

// --- Assemble all conversion functions ---

const convertFunctions: BuiltinNormalExpressions = {
  ...generateLinearConversions(lengthUnits),
  ...generateLinearConversions(weightUnits),
  ...generateLinearConversions(volumeUnits),
  ...generateLinearConversions(timeUnits),
  ...generateLinearConversions(areaUnits),
  ...generateLinearConversions(speedUnits),
  ...generateLinearConversions(dataUnits),
  ...generateLinearConversions(pressureUnits),
  ...generateLinearConversions(energyUnits),
  ...generateLinearConversions(powerUnits),
  ...generateLinearConversions(frequencyUnits),
  ...generateLinearConversions(angleUnits),
  ...generateTemperatureConversions(),
}

export const convertModule: LitsModule = {
  name: 'convert',
  functions: convertFunctions,
}
