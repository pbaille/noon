(ns noon.parse.harmony-grammar)

(def grammar
  "
<top> = colon? (secondary-degree | degree | root)? (top/structure | top/mode)

<top/structure> = separator? (structure | structure/shorthand)?

<top/mode> = separator? mode (separator structure/shorthand)?

(* MISC ---------------------------------------------------------------------------------------- *)

<separator> = <'.'>
<colon> = <':'>

(* DEGREE -------------------------------------------------------------------------------------- *)

degree = alteration roman-degree

<alteration> = sharp | bemol | double-bemol | double-sharp | natural

  sharp = <'#'>
  bemol = <'b'>
  double-sharp = < 'x'>
  double-bemol = <'bb' | 'o'>
  natural = <'♮' | 'n' | ''>

<roman-degree> = one | two | three | four | five | six | seven

  one = <'I'>
  two = <'II'>
  three = <'III'>
  four = <'IV'>
  five = <'V'>
  six = <'VI'>
  seven = <'VII'>

<mode-degree> = second | third | fourth | fifth | sixth | seventh

  second = <'2' | '9'>
  third = <'3' | '10'>
  fourth = <'4' | '11'>
  fifth = <'5'>
  sixth = <'6' | '13'>
  seventh = <'7'>

secondary-degree = degree <'/' | 'of'> degree


(* ROOT ---------------------------------------------------------------------------------------- *)

root = pitch-class alteration

  <pitch-class> = C | D | E | F | G | A | B

    C = <'C'>
    D = <'D'>
    E = <'E'>
    F = <'F'>
    G = <'G'>
    A = <'A'>
    B = <'B'>


(* STRUCTURE ------------------------------------------------------------------------------ *)

structure = structure/base structure/modifiers / structure/modifiers

structure/base = tetrad/major-seventh / tetrad/diminished-seventh / tetrad/minor-seventh /
                 tetrad/dominant / tetrad/half-diminished / tetrad/minor-major-seventh /
                 triad/diminished / triad/augmented / triad/minor / triad/major

  tetrad/major-seventh = <'Δ' | 'M7'>
  tetrad/diminished-seventh = <'o7'>
  tetrad/minor-seventh = <'m7'>
  tetrad/dominant = <'7'>
  tetrad/half-diminished = <'ø'>
  tetrad/minor-major-seventh = <'mΔ' | 'mM7'>
  triad/diminished = <'o'>
  triad/augmented = <'+'>
  triad/minor = <'m'>
  triad/major = <'M'>

structure/modifiers = structure.modifier/augmented? structure/modifier* structure.modifier/augmented? (separator? structure.modifier/bass)?

  <structure/modifier> = separator? (structure.modifier/omission | structure.modifier/degree)

structure.modifier/omission = (omit1 | omit3 | omit5)

  omit1 = <'omit1'>
  omit3 = <'sus'> | <'omit3'>
  omit5 = <'omit5'>

structure.modifier/augmented = <'+'>
structure.modifier/degree = alteration mode-degree

structure.modifier/bass = (<'/'> | <'bass'> | <'on'>) structure.modifier.bass/content
  <structure.modifier.bass/content> = structure.modifier.bass/degree | structure.modifier.bass/degree-digit | structure.modifier.bass/pitch-class
  structure.modifier.bass/degree-digit = degree-digit
  structure.modifier.bass/degree = alteration roman-degree
  structure.modifier.bass/pitch-class = pitch-class alteration

structure/shorthand = <'s'> degree-digit+
  <degree-digit> = '1' | '2' | '3' | '4' | '5' | '6' | '7'

(* MODES ------------------------------------------------------------------------------------- *)


mode = <separator?> mode/base mode/alterations?

mode/base = ionian | dorian | phrygian | mixolydian | lydian | aeolian | locrian |
            harmonic-minor | melodic-minor | altered | harmonic-major |
            double-harmonic | ultraphrygian | hungarian-minor | oriental | ultralocrian

  ionian = <'ionian' | 'ion'>
  dorian = <'dorian' | 'dor'>
  phrygian = <'phrygian' | 'phry'>
  lydian = <'lydian' | 'lyd'>
  mixolydian = <'mixolydian' | 'mix'>
  aeolian = <'aeolian' | 'eolian' | 'eol'>
  locrian = <'locrian' | 'loc'>
  harmonic-minor = <'harmonic-minor' | 'harmm'>
  melodic-minor = <'melodic-minor' | 'melm'>
  altered = <'superlocrian' | 'altered' | 'alt'>
  harmonic-major = <'harmonic-major' | 'harmM'>
  double-harmonic = <'double-harmonic'>
  ultraphrygian = <'ultraphrygian'>
  hungarian-minor = <'hungarian-minor' | 'hungarian'>
  oriental = <'oriental'>
  ultralocrian = <'ultralocrian'>

mode/alterations = mode.alteration/augmented-fifth? <separator?> mode.alteration/degree* mode.alteration/augmented-fifth?
  mode.alteration/augmented-fifth = <'+'>
  mode.alteration/degree = alteration mode-degree

")
