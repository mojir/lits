import { describe, expect, it } from 'vitest'
import { Lits } from '../../../Lits/Lits'
import { convertModule } from './'

const lits = new Lits({ modules: [convertModule] })

function runConvert(code: string): unknown {
  return lits.run(`let c = import(convert); ${code.replace(/convert:/g, 'c.')}`)
}

describe('convert module', () => {
  describe('length conversions', () => {
    it('should convert meters to feet', () => {
      expect(runConvert('convert:m->ft(1)')).toBeCloseTo(3.28084, 4)
    })

    it('should convert feet to meters', () => {
      expect(runConvert('convert:ft->m(1)')).toBeCloseTo(0.3048, 4)
    })

    it('should convert kilometers to miles', () => {
      expect(runConvert('convert:km->mi(1)')).toBeCloseTo(0.621371, 4)
    })

    it('should convert miles to kilometers', () => {
      expect(runConvert('convert:mi->km(1)')).toBeCloseTo(1.60934, 4)
    })

    it('should convert inches to centimeters', () => {
      expect(runConvert('convert:in->cm(1)')).toBeCloseTo(2.54, 4)
    })

    it('should convert centimeters to inches', () => {
      expect(runConvert('convert:cm->in(1)')).toBeCloseTo(0.393701, 4)
    })

    it('should convert yards to meters', () => {
      expect(runConvert('convert:yd->m(1)')).toBeCloseTo(0.9144, 4)
    })

    it('should convert nautical miles to kilometers', () => {
      expect(runConvert('convert:nmi->km(1)')).toBeCloseTo(1.852, 4)
    })

    it('should convert millimeters to meters', () => {
      expect(runConvert('convert:mm->m(1000)')).toBeCloseTo(1, 4)
    })

    it('should handle zero', () => {
      expect(runConvert('convert:m->ft(0)')).toBe(0)
    })

    it('should handle negative values', () => {
      expect(runConvert('convert:m->ft(-1)')).toBeCloseTo(-3.28084, 4)
    })
  })

  describe('weight conversions', () => {
    it('should convert kilograms to pounds', () => {
      expect(runConvert('convert:kg->lb(1)')).toBeCloseTo(2.20462, 4)
    })

    it('should convert pounds to kilograms', () => {
      expect(runConvert('convert:lb->kg(1)')).toBeCloseTo(0.453592, 4)
    })

    it('should convert grams to ounces', () => {
      expect(runConvert('convert:g->oz(1)')).toBeCloseTo(0.035274, 4)
    })

    it('should convert ounces to grams', () => {
      expect(runConvert('convert:oz->g(1)')).toBeCloseTo(28.3495, 3)
    })

    it('should convert metric tons to kilograms', () => {
      expect(runConvert('convert:t->kg(1)')).toBe(1000)
    })

    it('should convert milligrams to grams', () => {
      expect(runConvert('convert:mg->g(1000)')).toBe(1)
    })
  })

  describe('temperature conversions', () => {
    it('should convert celsius to fahrenheit', () => {
      expect(runConvert('convert:c->f(0)')).toBeCloseTo(32, 4)
    })

    it('should convert celsius to fahrenheit (100)', () => {
      expect(runConvert('convert:c->f(100)')).toBeCloseTo(212, 4)
    })

    it('should convert fahrenheit to celsius', () => {
      expect(runConvert('convert:f->c(32)')).toBeCloseTo(0, 4)
    })

    it('should convert fahrenheit to celsius (212)', () => {
      expect(runConvert('convert:f->c(212)')).toBeCloseTo(100, 4)
    })

    it('should convert celsius to kelvin', () => {
      expect(runConvert('convert:c->k(0)')).toBeCloseTo(273.15, 4)
    })

    it('should convert kelvin to celsius', () => {
      expect(runConvert('convert:k->c(273.15)')).toBeCloseTo(0, 4)
    })

    it('should convert fahrenheit to kelvin', () => {
      expect(runConvert('convert:f->k(32)')).toBeCloseTo(273.15, 4)
    })

    it('should convert kelvin to fahrenheit', () => {
      expect(runConvert('convert:k->f(273.15)')).toBeCloseTo(32, 4)
    })

    it('should handle negative celsius', () => {
      expect(runConvert('convert:c->f(-40)')).toBeCloseTo(-40, 4)
    })

    it('should handle negative fahrenheit (equal point)', () => {
      expect(runConvert('convert:f->c(-40)')).toBeCloseTo(-40, 4)
    })
  })

  describe('volume conversions', () => {
    it('should convert liters to gallons', () => {
      expect(runConvert('convert:l->gal(1)')).toBeCloseTo(0.264172, 4)
    })

    it('should convert gallons to liters', () => {
      expect(runConvert('convert:gal->l(1)')).toBeCloseTo(3.78541, 4)
    })

    it('should convert milliliters to liters', () => {
      expect(runConvert('convert:ml->l(1000)')).toBe(1)
    })

    it('should convert cups to milliliters', () => {
      expect(runConvert('convert:cup->ml(1)')).toBeCloseTo(236.588, 2)
    })

    it('should convert fluid ounces to milliliters', () => {
      expect(runConvert('convert:fl-oz->ml(1)')).toBeCloseTo(29.5735, 3)
    })

    it('should convert tablespoons to teaspoons', () => {
      expect(runConvert('convert:tbsp->tsp(1)')).toBeCloseTo(3, 4)
    })

    it('should convert quarts to pints', () => {
      expect(runConvert('convert:qt->pt(1)')).toBeCloseTo(2, 4)
    })
  })

  describe('time conversions', () => {
    it('should convert hours to minutes', () => {
      expect(runConvert('convert:h->min(1)')).toBe(60)
    })

    it('should convert minutes to seconds', () => {
      expect(runConvert('convert:min->s(1)')).toBe(60)
    })

    it('should convert days to hours', () => {
      expect(runConvert('convert:day->h(1)')).toBe(24)
    })

    it('should convert weeks to days', () => {
      expect(runConvert('convert:week->day(1)')).toBe(7)
    })

    it('should convert milliseconds to seconds', () => {
      expect(runConvert('convert:ms->s(1000)')).toBe(1)
    })

    it('should convert hours to seconds', () => {
      expect(runConvert('convert:h->s(1)')).toBe(3600)
    })
  })

  describe('area conversions', () => {
    it('should convert square meters to square feet', () => {
      expect(runConvert('convert:m2->ft2(1)')).toBeCloseTo(10.7639, 3)
    })

    it('should convert square feet to square meters', () => {
      expect(runConvert('convert:ft2->m2(1)')).toBeCloseTo(0.092903, 4)
    })

    it('should convert hectares to acres', () => {
      expect(runConvert('convert:hectare->acre(1)')).toBeCloseTo(2.47105, 4)
    })

    it('should convert acres to hectares', () => {
      expect(runConvert('convert:acre->hectare(1)')).toBeCloseTo(0.404686, 4)
    })

    it('should convert square kilometers to square meters', () => {
      expect(runConvert('convert:km2->m2(1)')).toBe(1000000)
    })

    it('should convert square inches to square centimeters', () => {
      expect(runConvert('convert:in2->cm2(1)')).toBeCloseTo(6.4516, 4)
    })
  })

  describe('edge cases', () => {
    it('should handle large numbers', () => {
      expect(runConvert('convert:km->mm(1)')).toBe(1000000)
    })

    it('should handle small fractions', () => {
      expect(runConvert('convert:mm->km(1)')).toBeCloseTo(0.000001, 10)
    })

    it('should be consistent with roundtrip conversions', () => {
      const original = 42.5
      const result = runConvert(`convert:ft->m(convert:m->ft(${original}))`) as number
      expect(result).toBeCloseTo(original, 10)
    })

    it('should be consistent with temperature roundtrip', () => {
      const original = 37
      const result = runConvert(`convert:f->c(convert:c->f(${original}))`) as number
      expect(result).toBeCloseTo(original, 10)
    })
  })

  describe('speed conversions', () => {
    it('should convert m/s to km/h', () => {
      expect(runConvert('convert:m/s->km/h(1)')).toBeCloseTo(3.6, 4)
    })

    it('should convert km/h to mph', () => {
      expect(runConvert('convert:km/h->mph(100)')).toBeCloseTo(62.1371, 3)
    })

    it('should convert mph to m/s', () => {
      expect(runConvert('convert:mph->m/s(1)')).toBeCloseTo(0.44704, 4)
    })

    it('should convert knots to km/h', () => {
      expect(runConvert('convert:kn->km/h(1)')).toBeCloseTo(1.852, 3)
    })

    it('should convert ft/s to m/s', () => {
      expect(runConvert('convert:ft/s->m/s(1)')).toBeCloseTo(0.3048, 4)
    })
  })

  describe('data conversions', () => {
    it('should convert kb to b', () => {
      expect(runConvert('convert:kb->b(1)')).toBe(1000)
    })

    it('should convert mb to kb', () => {
      expect(runConvert('convert:mb->kb(1)')).toBe(1000)
    })

    it('should convert gb to mb', () => {
      expect(runConvert('convert:gb->mb(1)')).toBe(1000)
    })

    it('should convert tb to gb', () => {
      expect(runConvert('convert:tb->gb(1)')).toBe(1000)
    })

    it('should convert pb to tb', () => {
      expect(runConvert('convert:pb->tb(1)')).toBe(1000)
    })
  })

  describe('pressure conversions', () => {
    it('should convert atm to pa', () => {
      expect(runConvert('convert:atm->pa(1)')).toBe(101325)
    })

    it('should convert bar to atm', () => {
      expect(runConvert('convert:bar->atm(1)')).toBeCloseTo(0.986923, 4)
    })

    it('should convert psi to kpa', () => {
      expect(runConvert('convert:psi->kpa(1)')).toBeCloseTo(6.89476, 3)
    })

    it('should convert mmhg to pa', () => {
      expect(runConvert('convert:mmhg->pa(1)')).toBeCloseTo(133.322, 2)
    })
  })

  describe('energy conversions', () => {
    it('should convert kcal to j', () => {
      expect(runConvert('convert:kcal->j(1)')).toBe(4184)
    })

    it('should convert cal to j', () => {
      expect(runConvert('convert:cal->j(1)')).toBeCloseTo(4.184, 4)
    })

    it('should convert kwh to j', () => {
      expect(runConvert('convert:kwh->j(1)')).toBe(3600000)
    })

    it('should convert btu to kj', () => {
      expect(runConvert('convert:btu->kj(1)')).toBeCloseTo(1.05506, 3)
    })

    it('should convert wh to cal', () => {
      expect(runConvert('convert:wh->cal(1)')).toBeCloseTo(860.421, 2)
    })
  })

  describe('power conversions', () => {
    it('should convert kw to w', () => {
      expect(runConvert('convert:kw->w(1)')).toBe(1000)
    })

    it('should convert hp to w', () => {
      expect(runConvert('convert:hp->w(1)')).toBeCloseTo(745.7, 1)
    })

    it('should convert mw to kw', () => {
      expect(runConvert('convert:mw->kw(1)')).toBe(1000)
    })

    it('should convert hp to kw', () => {
      expect(runConvert('convert:hp->kw(1)')).toBeCloseTo(0.7457, 3)
    })
  })

  describe('frequency conversions', () => {
    it('should convert khz to hz', () => {
      expect(runConvert('convert:khz->hz(1)')).toBe(1000)
    })

    it('should convert mhz to khz', () => {
      expect(runConvert('convert:mhz->khz(1)')).toBe(1000)
    })

    it('should convert ghz to mhz', () => {
      expect(runConvert('convert:ghz->mhz(1)')).toBe(1000)
    })

    it('should convert ghz to hz', () => {
      expect(runConvert('convert:ghz->hz(1)')).toBe(1000000000)
    })
  })

  describe('angle conversions', () => {
    it('should convert degrees to radians', () => {
      expect(runConvert('convert:deg->rad(180)')).toBeCloseTo(Math.PI, 10)
    })

    it('should convert radians to degrees', () => {
      expect(runConvert(`convert:rad->deg(${Math.PI})`)).toBeCloseTo(180, 10)
    })

    it('should convert degrees to gradians', () => {
      expect(runConvert('convert:deg->grad(90)')).toBeCloseTo(100, 10)
    })

    it('should convert turns to degrees', () => {
      expect(runConvert('convert:turn->deg(1)')).toBeCloseTo(360, 10)
    })

    it('should convert turns to radians', () => {
      expect(runConvert('convert:turn->rad(1)')).toBeCloseTo(2 * Math.PI, 10)
    })
  })

  describe('extra units', () => {
    it('should convert stone to kg', () => {
      expect(runConvert('convert:st->kg(1)')).toBeCloseTo(6.35029, 4)
    })

    it('should convert kg to stone', () => {
      expect(runConvert('convert:kg->st(1)')).toBeCloseTo(0.157473, 4)
    })

    it('should convert micrometers to mm', () => {
      expect(runConvert('convert:um->mm(1000)')).toBeCloseTo(1, 10)
    })

    it('should convert angstrom to um', () => {
      expect(runConvert('convert:angstrom->um(10000)')).toBeCloseTo(1, 10)
    })

    it('should convert angstrom to m', () => {
      expect(runConvert('convert:angstrom->m(1e10)')).toBeCloseTo(1, 10)
    })
  })
})
