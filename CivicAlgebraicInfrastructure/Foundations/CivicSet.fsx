#load "FirstOrderLogic.fs"
#load "CivicSet.fs"

open CivicAlgebraicInfrastructure.Foundations.FOL.DomainModel
open CivicAlgebraicInfrastructure.Foundations.CivicSet.DomainModel
open CivicAlgebraicInfrastructure.Foundations.CivicSet.Operations

// -----------------------------
// Example: ℕ = { x | x ≥ 0 }
// -----------------------------

let geq = { Name = "≥"; Kind = PredicateKind; Arity = Some 2}
let xSym = { Name = "x"; Kind = VariableKind; Arity = None}
let zeroSym = { Name = "0"; Kind = ConstantKind; Arity = None}

let natFormula : Formula<Symbol> =
    Atomic (Predicate (geq, [ Var xSym; Constant zeroSym ]))

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
                             OrderType    = Some TotalOrder } ]
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

let integers =
    { new ICivicSet<int,Symbol> with
        member _.Symbol       = Some "\u2124"   // ℤ
        member _.Formula      = Some intFormula
        member _.Contains _   = true
        member _.Elements     =
            Seq.initInfinite (fun n -> if n % 2 = 0 then n/2 else -(n/2 + 1))
        member _.Compare      = Some compare
        member _.Min          = None
        member _.Max          = None
        member _.Metadata     =
            [ SetTheoretic { Cardinality  = Some Aleph0
                             Countability = Some Countable
                             OrderType    = Some TotalOrder } ]
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
                             OrderType    = None } ]
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
                             OrderType    = Some TotalOrder } ]
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
printfn "First 10 ℕ: %A" (naturalNumbers.Elements |> Seq.take 10 |> Seq.toList)
printfn "ℕ formula: %A" naturalNumbers.Formula

printfn "ℤ sample: %A" (integers.Elements |> Seq.take 10 |> Seq.toList)

printfn "ℚ sample first 20 rationals (diagonal): %A" (rationals.Elements |> Seq.take 20 |> Seq.toList)

printfn "ℝ formula: %A" reals.Formula
printfn "ℝ sample: %A" (reals.Elements |> Seq.take 10 |> Seq.toList)

printfn "ℂ formula: %A" complex.Formula
printfn "ℂ sample: %A" (complex.Elements |> Seq.take 10 |> Seq.toList)

(*
type SimpleSet(elements: seq<int>) =
    interface ICivicSet<int, string> with
        member _.Symbol = Some "SimpleSet"
        member _.Formula = None
        member _.Contains x = elements |> Seq.contains x
        member _.Elements = elements
        member _.Compare = Some compare
        member _.Min = elements |> Seq.min |> Some
        member _.Max = elements |> Seq.max |> Some
        member _.Metadata = [Tag "Test Set"]
        member _.IsClosedUnder _ = true
        member _.Implies _ = false
        member _.EquivalentTo _ = false

let a = SimpleSet([1; 2; 3]) :> ICivicSet<int, string>
let b = SimpleSet([1; 2; 3; 4; 5]) :> ICivicSet<int, string>
let c = SimpleSet([2; 3]) :> ICivicSet<int, string>

let test1 = isSubsetOf a b // true
let test2 = isSubsetOf c a // true
let test3 = isSubsetOf b a // false*)