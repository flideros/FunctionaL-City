#load "Primitives.fs"
#load "FirstOrderLogic.fs"
#load "CivicSet.fs"

open System
open CivicAlgebraicInfrastructure.Foundations.Primitives
open CivicAlgebraicInfrastructure.Foundations.FOL
open CivicAlgebraicInfrastructure.Foundations.CivicSet
open CivicAlgebraicInfrastructure.Foundations.CivicSet.Operations

// -----------------------------
// Example: ℕ = { x | x ≥ 0 }
// -----------------------------

let geq = { Name = "≥"; Kind = PredicateKind; Arity = Some 2}
let xSym = { Name = "x"; Kind = VariableKind; Arity = None}
let zeroSym = { Name = "0"; Kind = ConstantKind; Arity = None}

let natFormula : Formula<Symbol> =
    Atomic (Predicate (geq, [ Var xSym; Constant zeroSym ]))

let natProvenance : Provenance =
    { SourceName = "ZFC + Peano Axioms"
      Step = 1
      Timestamp = Some(DateTime(1930, 1, 1))
      Note = "Declared ℕ as the set of natural numbers ≥ 0, formalized in ZFC and derived from Peano axioms." }

let naturalNumbers =
    { new ICivicSet<int,Symbol> with
        member _.Symbol       = Some "\u2115"   // ℕ
        member _.Formula      = Some natFormula
        member _.Contains n   = n >= 0
        member _.Elements     = Seq.initInfinite id
        member _.Compare      = Some compare
        member _.Min          = Some 0
        member _.Max          = None
        member _.Metadata     = 
            [ SetTheoretic { Cardinality  = Some Aleph0
                             Countability = Some Countable
                             OrderType    = Some TotalOrder };
              Provenance natProvenance;
              Note """ℕ, the set of natural numbers ≥ 0, originates from Giuseppe Peano's 1889 axioms, 
which defined arithmetic using successor functions and induction. These axioms laid the 
foundation for formal number theory. In the early 20th century, Ernst Zermelo introduced 
axioms for set theory (1908), later extended by Abraham Fraenkel and others to form ZFC. 
Within this framework, ℕ was reconstructed as a set-theoretic object using the von Neumann 
ordinal construction, where each natural number is defined as the set of all smaller 
natural numbers. This civic declaration reflects that lineage: ℕ is countable, totally ordered, 
and formally grounded in ZFC + Peano arithmetic.""" ]
        member _.IsClosedUnder _ = true
        member _.Implies _       = false
        member _.EquivalentTo _  = false }

// -----------------------------
// Example: ℤ = all integers
// -----------------------------

let intPred = { Name = "isInteger"; Kind = PredicateKind; Arity = Some 1 }
let intVar  = { Name = "x"; Kind = VariableKind; Arity = None }

let intFormula : Formula<Symbol> =
    Atomic (Predicate (intPred, [ Var intVar ]))

let intProvenance : Provenance =
    { SourceName = "ZFC + Integer Construction"
      Step = 2
      Timestamp = Some(DateTime(1930, 1, 1))
      Note = "Declared ℤ as the set of integers, constructed from ℕ using equivalence classes of ordered pairs." }

let integers =
    { new ICivicSet<int,Symbol> with
        member _.Symbol       = Some "\u2124"   // ℤ
        member _.Formula      = Some intFormula
        member _.Contains _   = true
        member _.Elements     = Seq.initInfinite (fun n -> if n % 2 = 0 then n/2 else -(n/2 + 1))
        member _.Compare      = Some compare
        member _.Min          = None
        member _.Max          = None
        member _.Metadata     =
            [ SetTheoretic { Cardinality  = Some Aleph0
                             Countability = Some Countable
                             OrderType    = Some TotalOrder };
              Provenance intProvenance; 
              Note """ℤ, the set of integers, extends ℕ by introducing additive inverses. It is constructed using 
equivalence classes of ordered pairs of natural numbers: (a, b) represents the integer a − b. 
This construction, formalized within ZFC, preserves total order and countability. ℤ includes zero, 
positive naturals, and their negatives, forming a foundational ring for arithmetic and algebra.""" ]
        member _.IsClosedUnder _ = true
        member _.Implies _       = false
        member _.EquivalentTo _  = false }

// -----------------------------
// Example: ℚ = rationals
// -----------------------------

open System.Numerics

let gcdInt (p:int) (q:int) : int =
    let bp, bq = BigInteger p, BigInteger q
    int (BigInteger.Abs (BigInteger.GreatestCommonDivisor(bp, bq)))

[<StructuralEquality; StructuralComparison>]
type Rational =
    { Num : int; Den : int }
    static member Create (p:int) (q:int) =
        if q = 0 then invalidArg "q" "Denominator cannot be zero."
        else
            let g = gcdInt p q
            let p', q' = p / g, q / g
            if q' < 0 then { Num = -p'; Den = -q' }
            else { Num = p'; Den = q' }

    override this.ToString() =
        if this.Den = 1 then string this.Num
        else $"{this.Num}/{this.Den}"
let rationalsDiagonal : seq<Rational> =
    seq {
        // sweep diagonals by "sum of absolute values"
        for s in 1 .. System.Int32.MaxValue do
            for n in -s .. s do
                let d = s
                if d <> 0 && gcdInt n d = 1 then
                    yield Rational.Create n d }

let ratPred = { Name = "isRational"; Kind = PredicateKind; Arity = Some 2 }
let pSym    = { Name = "p"; Kind = VariableKind; Arity = None }
let qSym    = { Name = "q"; Kind = VariableKind; Arity = None }

let rationalFormula : Formula<Symbol> =
    Atomic (Predicate (ratPred, [ Var pSym; Var qSym ]))

let ratProvenance : Provenance =
    { SourceName = "ZFC + Rational Construction"
      Step = 3
      Timestamp = Some(DateTime(1930, 1, 1))
      Note = "Declared ℚ as the set of rational numbers, constructed from ℤ using equivalence classes of integer pairs." }

let rationals =
    { new ICivicSet<Rational,Symbol> with
        member _.Symbol       = Some "\u211A"   // ℚ
        member _.Formula      = Some rationalFormula
        member _.Contains r   = r.Den <> 0
        member _.Elements     = rationalsDiagonal
        member _.Compare      = None
        member _.Min          = None
        member _.Max          = None
        member _.Metadata     =
            [ SetTheoretic { Cardinality  = Some Aleph0
                             Countability = Some Countable
                             OrderType    = None };
              Provenance ratProvenance; 
              Note """ℚ, the set of rational numbers, is constructed from ℤ using equivalence classes of 
ordered pairs (a, b) where b ≠ 0. Each pair represents the fraction a/b, with equivalence 
defined by cross-multiplication. This construction, formalized in ZFC, yields a countable, 
totally ordered field. ℚ supports addition, multiplication, and inversion (except for zero), 
forming the backbone of ratio-based arithmetic.""" ]
        member _.IsClosedUnder _ = true
        member _.Implies _       = false
        member _.EquivalentTo _  = false }

// -----------------------------
// Example: ℝ = reals
// -----------------------------

let realSymbol =
    { Name = "Real"; Kind = PredicateKind; Arity = Some 1 }

let xVar =
    Var { Name = "x"; Kind = VariableKind; Arity = None }

let realFormulaFOL : Formula<Symbol> =
    Quantified {
        Bound = ForAll { Name = "x"; Kind = VariableKind; Arity = None }
        Body =
            Atomic (Predicate (realSymbol, [xVar]))
    }

let realProvenance : Provenance =
    { SourceName = "ZFC + Dedekind/Completeness Axioms"
      Step = 4
      Timestamp = Some(DateTime(1930, 1, 1))
      Note = "Declared ℝ as the set of real numbers, constructed via Dedekind cuts or Cauchy sequences over ℚ." }

let reals =
    { new ICivicSet<float, Symbol> with
        member _.Symbol       = Some "\u211D"   // ℝ
        member _.Formula      = Some realFormulaFOL
        member _.Contains _   = true
        member _.Elements     = Seq.initInfinite (fun n -> float n / 10.0) // sample density
        member _.Compare      = Some compare
        member _.Min          = None
        member _.Max          = None
        member _.Metadata     =
            [ SetTheoretic { Cardinality  = Some Continuum
                             Countability = Some Uncountable
                             OrderType    = Some TotalOrder };
              Provenance realProvenance;
              Note """ℝ, the set of real numbers, extends ℚ to include limits of convergent sequences and irrational quantities. 
It can be constructed via Dedekind cuts—partitions of ℚ—or equivalence classes of Cauchy sequences. 
Formalized within ZFC, ℝ is uncountable and totally ordered, satisfying the completeness axiom: 
every bounded set has a least upper bound. ℝ underpins analysis, calculus, and continuous modeling 
in civic infrastructure.""" ]
        member _.IsClosedUnder _ = true
        member _.Implies _       = false
        member _.EquivalentTo _  = false }

// -----------------------------
// Example: ℂ = complex
// -----------------------------

let complexSymbol =
    { Name = "Complex"; Kind = PredicateKind; Arity = Some 1 }

let zVar =
    Var { Name = "z"; Kind = VariableKind; Arity = None }

let complexFormulaFOL : Formula<Symbol> =
    Quantified {
        Bound = ForAll { Name = "z"; Kind = VariableKind; Arity = None }
        Body =
            Atomic (Predicate (complexSymbol, [zVar]))
    }

let complex =
    { new ICivicSet<System.Numerics.Complex, Symbol> with
        member _.Symbol       = Some "\u2102"   // ℂ
        member _.Formula      = Some complexFormulaFOL
        member _.Contains _   = true
        member _.Elements     =
            Seq.initInfinite (fun n ->
                let r = float (n / 100)
                let i = float (n % 100) / 10.0
                System.Numerics.Complex(r, i))
        member _.Compare      = None // no total order
        member _.Min          = None
        member _.Max          = None
        member _.Metadata     =
            [ SetTheoretic { Cardinality  = Some Continuum
                             Countability = Some Uncountable
                             OrderType    = None } ]
        member _.IsClosedUnder _ = true
        member _.Implies _       = false
        member _.EquivalentTo _  = false }

// -----------------------------
// Quick tests
// -----------------------------

printfn "ℕ contains 5? %b" (naturalNumbers.Contains 5)
printfn "ℕ contains -3? %b" (naturalNumbers.Contains -3)
printfn "First 10 ℕ: %A" (naturalNumbers.Elements |> Seq.take 5 |> Seq.toList)
printfn "ℕ formula: %A" naturalNumbers.Formula

printfn "ℤ sample: %A" (integers.Elements |> Seq.take 5 |> Seq.toList)

printfn "ℚ sample first 20 rationals (diagonal): %A" (rationals.Elements |> Seq.take 5 |> Seq.toList)

printfn "ℝ formula: %A" reals.Formula
printfn "ℝ sample: %A" (reals.Elements |> Seq.take 5 |> Seq.toList)

printfn "ℂ formula: %A" complex.Formula
printfn "ℂ sample: %A" (complex.Elements |> Seq.take 5 |> Seq.toList)

/// Try to extract the first Note from metadata
let tryGetNote (metadata: CivicSetMetadataItem list) : string option =
    metadata
    |> List.choose (function Note text -> Some text | _ -> None)
    |> List.tryHead

/// Display the note if available
let displayNote (metadata: CivicSetMetadataItem list) =
    match tryGetNote metadata with
    | Some note -> printfn "🪧 CivicSet Note:\n%s" note
    | None -> printfn "No civic note found in metadata."

displayNote integers.Metadata

/// Generate a civic inspector report for a given CivicSet
let civicSetInspectorReport (set: ICivicSet<'C,'S>) : string =
    let symbol     = set.Symbol |> Option.defaultValue "∅"
    let formulaStr = 
        match box set.Formula with
        | :? Option<Formula<Symbol>> as fOpt ->
            fOpt
            |> Option.map FormulaPrinter.formulaToString
            |> Option.defaultValue "—"
        | _ -> "—"
    let cardinality, countability, orderType =
        set.Metadata
        |> List.choose (function SetTheoretic m -> Some m | _ -> None)
        |> List.tryHead
        |> Option.map (fun m -> m.Cardinality, m.Countability, m.OrderType)
        |> Option.defaultValue (None, None, None)

    let provenance =
        set.Metadata
        |> List.choose (function Provenance p -> Some p | _ -> None)
        |> List.tryHead
        |> Option.map Provenance.describe
        |> Option.defaultValue "No provenance record found."

    let note =
        set.Metadata
        |> List.choose (function Note n -> Some n | _ -> None)
        |> List.tryHead
        |> Option.defaultValue "No civic signage note found."

    let minStr = set.Min |> Option.map string |> Option.defaultValue "—"
    let maxStr = set.Max |> Option.map string |> Option.defaultValue "—"
    let countabilityStr = countability |> Option.map string |> Option.defaultValue "—"
    let cardinalityStr  = cardinality |> Option.map string |> Option.defaultValue "—"
    let orderStr        = orderType |> Option.map string |> Option.defaultValue "—"

    $"""
🧾 CivicSet Inspector Report
────────────────────────────
🆔 Symbol:  {symbol}
📐 Formula: {formulaStr}

📊 Set-Theoretic Metadata:
   • Cardinality: {cardinalityStr}
   • Countability: {countabilityStr}
   • Order Type: {orderStr}

🔍 Min: {minStr}
🔍 Max: {maxStr}

🧭 Provenance:
   {provenance}

📝 Civic Note:
{note}
"""
printfn "%s" (civicSetInspectorReport naturalNumbers)
