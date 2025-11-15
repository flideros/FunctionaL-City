#load "Primitives.fs"
#load "FirstOrderLogic.fs"
#load "CivicSet.fs"

open System
open CivicAlgebraicInfrastructure.Foundations.Primitives
open CivicAlgebraicInfrastructure.Foundations.FOL
open CivicAlgebraicInfrastructure.Foundations.CivicSet

let equivalenceDepth = 1000

// -----------------------------
// Example: ℕ = { x | x ≥ 0 }
// -----------------------------
let addRule symbol rule (registry: CivicSetRuleDictionary<'T>) : CivicSetRuleDictionary<'T> =
    registry.Add(symbol, rule)

let geq = { Name = "≥"; Kind = PredicateKind; Arity = Some 2}
let xSym = { Name = "x"; Kind = VariableKind; Arity = None}
let zeroSym = { Name = "0"; Kind = ConstantKind; Arity = None}

let natFormula : Formula<Symbol> =
    Atomic (Predicate (geq, [ Var xSym; Constant zeroSym ]))

let natProvenance : Provenance =
    { SourceName = "ZFC + Peano Axioms"
      Step = 1
      Timestamp = Some(DateTime(1930, 1, 1))
      Note = "Declared ℕ as the set of natural numbers ≥ 0, formalized in ZFC and derived from Peano axioms." 
      Lineage = [] }

let naturalNumbersSpec : CivicSetRule<int> =
    { Filter = fun n -> n >= 0;
      Generator = fun i -> i;
      Formula = Some natFormula;
      Provenance = natProvenance;
      Max = None;
      Min = Some 0;
      Compare = Some compare
      Metadata = {Cardinality = Some Aleph0;
                  Countability = Some Countable;
                  OrderType = Some TotalOrder}
      Note = """ℕ, the set of natural numbers ≥ 0, originates from Giuseppe Peano's 1889 axioms, 
which defined arithmetic using successor functions and induction. These axioms laid the 
foundation for formal number theory. In the early 20th century, Ernst Zermelo introduced 
axioms for set theory (1908), later extended by Abraham Fraenkel and others to form ZFC. 
Within this framework, ℕ was reconstructed as a set-theoretic object using the von Neumann 
ordinal construction, where each natural number is defined as the set of all smaller 
natural numbers. This civic declaration reflects that lineage: ℕ is countable, totally ordered, 
and formally grounded in ZFC + Peano arithmetic.""" 
     }
 
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
      Note = "Declared ℤ as the set of integers, constructed from ℕ using equivalence classes of ordered pairs." 
      Lineage = [] }

let integersSpec : CivicSetRule<int> =            
    { Filter = fun n -> true;
      Generator = fun n -> if n % 2 = 0 then n/2 else -(n/2 + 1);
      Formula = Some intFormula;
      Provenance = intProvenance;
      Max = None;
      Min = None;
      Compare = Some compare
      Metadata = {Cardinality = Some Aleph0; 
                  Countability = Some Countable;
                  OrderType = Some TotalOrder}
      Note = """ℤ, the set of integers, extends ℕ by introducing additive inverses. It is constructed using 
equivalence classes of ordered pairs of natural numbers: (a, b) represents the integer a − b. 
This construction, formalized within ZFC, preserves total order and countability. ℤ includes zero, 
positive naturals, and their negatives, forming a foundational ring for arithmetic and algebra.""" 
     }


// Create a dictionary for infinite integer sets. This will be a state variable in an a state machine at some point.
let intRules : CivicSetRuleDictionary<int> = (Map.ofList [("\u2115", naturalNumbersSpec);("otherNaturals", naturalNumbersSpec);("\u2124", integersSpec)])
// Cast defaultSet to a concrete type
let defaultS = CivicSetConstructors.defaultSet :> ICivicSet<int>
// Create the ICivicSets from the rule set and symbol.
let naturalNumbers  = CivicSetConstructors.infiniteSet intRules "\u2115" equivalenceDepth |> Option.defaultValue defaultS 
let otherNaturalNumbers  = CivicSetConstructors.infiniteSet intRules "otherNaturals" equivalenceDepth |> Option.defaultValue defaultS
let integers = CivicSetConstructors.infiniteSet intRules "\u2124" equivalenceDepth |> Option.defaultValue defaultS


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
      Note = "Declared ℚ as the set of rational numbers, constructed from ℤ using equivalence classes of integer pairs." 
      Lineage = [] }

let rationals =
    { new ICivicSet<Rational> with
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
        member _.IsClosedUnder _ = SetResult.Default()
        member _.Implies _       = SetResult.Default()
        member _.EquivalentTo _  = SetResult.Default() }

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
      Note = "Declared ℝ as the set of real numbers, constructed via Dedekind cuts or Cauchy sequences over ℚ." 
      Lineage = [] }

let reals =
    { new ICivicSet<float> with
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
        member _.IsClosedUnder _ = SetResult.Default()
        member _.Implies _       = SetResult.Default()
        member _.EquivalentTo _  = SetResult.Default() }

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
    { new ICivicSet<System.Numerics.Complex> with
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
        member _.IsClosedUnder _ = SetResult.Default()
        member _.Implies _       = SetResult.Default()
        member _.EquivalentTo _  = SetResult.Default() }

// -----------------------------
// Quick tests
// -----------------------------

printfn "ℕ contains 5? %b" (naturalNumbers.Contains 5)
printfn "ℕ contains -3? %b" (naturalNumbers.Contains -3)
printfn "First 5 ℕ: %A" (naturalNumbers.Elements |> Seq.take 5 |> Seq.toList)
printfn "ℕ formula: %A" naturalNumbers.Formula

printfn "ℤ sample: %A" (integers.Elements |> Seq.take 5 |> Seq.toList)

printfn "ℚ sample first 5 rationals (diagonal): %A" (rationals.Elements |> Seq.take 5 |> Seq.toList)

printfn "ℝ formula: %A" reals.Formula
printfn "ℝ sample: %A" (reals.Elements |> Seq.take 5 |> Seq.toList)

printfn "ℂ formula: %A" complex.Formula
printfn "ℂ sample: %A" (complex.Elements |> Seq.take 5 |> Seq.toList)

// ---------------------------
// Sample reports
// ---------------------------

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
let civicSetInspectorReport (set: ICivicSet<'C> option) : string =
    match set with
    | None -> "Nothing to report"
    | Some set ->
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
printfn "%s" (civicSetInspectorReport (Some naturalNumbers))

// ---------------------------
// Sample ICivicSet implementations for testing 
// ---------------------------
let rec makeSimpleNaturalSet (name: string) (vals: int list) (provOption: Provenance option) : ICivicSet<int> =
    let prov =
                match provOption with 
                | None -> Provenance
                            { SourceName = name; 
                              Step = 2; 
                              Timestamp = Some DateTime.UtcNow; 
                              Note = "original"; 
                              Lineage = [natProvenance] }
                | Some p -> Provenance p
    { new ICivicSet<int> with
        member _.Symbol = Some name
        member _.Formula = None
        member _.Contains x = List.contains x vals
        member _.Elements = vals :> seq<int>
        member _.Compare = Some compare
        member _.Min = if List.isEmpty vals then None else Some (List.min vals)
        member _.Max = if List.isEmpty vals then None else Some (List.max vals)
        member _.Metadata = [ Tag $"SimpleSet:{name}"; 
                              SetTheoretic { Cardinality  = Some (Finite vals.Length)
                                             Countability = Some Countable
                                             OrderType    = Some TotalOrder }
                              prov ]
        member _.IsClosedUnder _ = SetResult.Default()
        member this.Implies (other: ICivicSet<int>) : SetResult<ICivicSet<int>> =            
            let difference = (Operations.setDifferenceResult 100 this other) :> ICivicResult<_> 
            
            let allImply = Seq.isEmpty difference.Value.Value && difference.Success

            let impProv =
                match provOption with  
                | None -> 
                    { Provenance.empty with
                        SourceName = sprintf "Implication (%s ⇒ %s)"
                                        (defaultArg this.Symbol  "A")
                                        (defaultArg other.Symbol "B")
                        Step = difference.Provenance.Step
                        Note = 
                            match allImply with 
                            | true -> "rule = exhaustive-finite; outcome = Proven; method = difference; checked = 100% of source; note = Implication holds"
                            | false -> 
                                match Seq.isEmpty difference.Value.Value with
                                | false -> sprintf "rule = exhaustive-finite; outcome = Refuted; method = difference; counterexamples = %d; witness = %A; ; note = Implication fails" (Seq.toList difference.Value.Value).Length difference.Value.Value
                                | true -> difference.Provenance.Note
                        Lineage = difference.Provenance.Lineage}
                | Some p -> p

            let diffSet = (makeSimpleNaturalSet "Counterexample set" (Seq.toList difference.Value.Value) (Some impProv))

            match allImply with
            | true -> SetResult.Succeed(diffSet,  "Implication holds", impProv)
            | false -> 
                SetResult.FailWithValue(diffSet,  "Implication fails with counterexamples", impProv)
        member _.EquivalentTo _ = SetResult.Default() }

let rec makeSimpleStringSet (name: string) (vals: string list) : ICivicSet<string> =
    
    { new ICivicSet<string> with
        member _.Symbol = Some name
        member _.Formula = None
        member _.Contains x = List.contains x vals
        member _.Elements = vals :> seq<string>
        member _.Compare = Some compare
        member _.Min = if List.isEmpty vals then None else Some (List.min vals)
        member _.Max = if List.isEmpty vals then None else Some (List.max vals)
        member _.Metadata = [ Tag $"SimpleSet:{name}"; 
                              SetTheoretic { Cardinality  = Some (Finite vals.Length)
                                             Countability = Some Countable
                                             OrderType    = Some TotalOrder } // lexicographical order, also known as dictionary order. 
                              Provenance { SourceName = name; 
                                           Step = 1; 
                                           Timestamp = Some DateTime.UtcNow; 
                                           Note = "Strings are finite and ordered sequences of arbitrary symbols drawn from a finite, non-empty set."; 
                                           Lineage = [] } ]
        member _.IsClosedUnder _ = SetResult.Default()
        member this.Implies (other: ICivicSet<String>) : SetResult<ICivicSet<string>> =
            let difference = (Operations.setDifferenceResult 100 this other) :> ICivicResult<_>
            let diffSet = (makeSimpleStringSet "Counterexample set" (Seq.toList difference.Value.Value))
            let allImply = Seq.isEmpty difference.Value.Value && difference.Success

            let prov =
                { Provenance.empty with
                    SourceName = sprintf "Implication (%s ⇒ %s)"
                                    (defaultArg this.Symbol  "A")
                                    (defaultArg other.Symbol "B")
                    Step = difference.Provenance.Step
                    Note = 
                        match allImply with 
                        | true -> "Implication holds"
                        | false -> sprintf "Implication fails: %d counterexample(s)" (Seq.toList difference.Value.Value).Length }

            match allImply with
            | true -> SetResult.Succeed(diffSet,"Implication holds", prov)
            | false -> 
                SetResult.FailWithValue(diffSet,"Implication fails with counterexamples", prov)
        member _.EquivalentTo _ = SetResult.Default() }

let naturalNumberRules = intRules.Item "\u2115"
let setA = CivicSetConstructors.finiteSet  "Nat" [1;2;3;4;5] None (Some naturalNumbersSpec)
let setB = CivicSetConstructors.finiteSet "Odds" [1;3;5;7;9] None (Some naturalNumbersSpec)
let setC = makeSimpleStringSet "String" ["A";"B";"C"]

let unionSet = Union.mkLiftedSets  setA setB 
let civicSet1 = unionSet  
let unionSetHetero = Union.mkLiftedSets setA setC
let civicSet2 = unionSetHetero

// Test Imply and Difference

printfn "Implication Test: naturalNumbers ⇒ integers"
let implicationResult = naturalNumbers.Implies(integers) :> ICivicResult<_>

printfn "Success: %b" implicationResult.Success
printfn "Message: %A" implicationResult.Message
printfn "Provenance Note: %s" implicationResult.Provenance.Note

match implicationResult.Value with
| Some civicSet ->
    printfn "Elements (sample): %A" (civicSet.Elements |> Seq.truncate 10 |> Seq.toList)
    // Uncomment for deeper inspection:
    // printfn "Metadata: %A" civicSet.Value.Metadata
| None ->
    printfn "No CivicSet yielded."
printfn " " 

printfn "Implication Test: integers ⇒ naturalNumbers"
let implicationResult2 = integers.Implies(naturalNumbers) :> ICivicResult<_>

printfn "Success: %b" implicationResult2.Success
printfn "Message: %A" implicationResult2.Message
printfn "Provenance Note: %A" implicationResult2.Provenance.Note

match implicationResult2.Value with
| Some civicSet ->
    printfn "Elements (sample): %A" (civicSet.Elements |> Seq.truncate 10 |> Seq.toList)
    // Uncomment for deeper inspection:
    // printfn "Metadata: %A" civicSet.Value.Metadata
| None ->
    printfn "No CivicSet yielded."
printfn " " 

printfn "Implication Test: setA ⇒ setB"
let implicationResult3 = setA.Implies(setB) :> ICivicResult<_>

printfn "Success: %b" implicationResult3.Success
printfn "Message: %A" implicationResult3.Message
printfn "Provenance Note: %A" implicationResult3.Provenance.Note

match implicationResult3.Value with
| Some civicSet ->
    printfn "Elements (sample): %A" (civicSet.Elements |> Seq.truncate 10 |> Seq.toList)
    // Uncomment for deeper inspection:
    // printfn "Metadata: %A" civicSet.Value.Metadata
| None ->
    printfn "No CivicSet yielded."
printfn " " 

printfn "Set Difference Test: integers \\ naturalNumbers"
let diffResult = Operations.setDifferenceResult equivalenceDepth integers naturalNumbers :> ICivicResult<int seq>

printfn "Success: %b" diffResult.Success
printfn "Message: %A" diffResult.Message
printfn "Provenance Note: %s" diffResult.Provenance.Note
printfn "Provenance Source: %s" diffResult.Provenance.SourceName

match diffResult.Value with
| Some seqVal ->
    printfn "Difference Elements (sample): %A" (seqVal |> Seq.truncate 10 |> Seq.toList)
| None ->
    printfn "No difference yielded."
printfn " " 

printfn "Set Difference Test: setA \\ naturalNumbers"
let diffResult2 = Operations.setDifferenceResult equivalenceDepth setA naturalNumbers :> ICivicResult<int seq>

printfn "Success: %b" diffResult2.Success
printfn "Message: %A" diffResult2.Message
printfn "Provenance Note: %s" diffResult2.Provenance.Note
printfn "Provenance Source: %A" diffResult2.Provenance.SourceName

match diffResult2.Value with
| Some seqVal ->
    printfn "Difference Elements (sample): %A" (seqVal |> Seq.truncate 10 |> Seq.toList)
| None ->
    printfn "No difference yielded."
printfn " " 

printfn "Set Difference Test: naturalNumbers \\ setB"
let diffResult3 = Operations.setDifferenceResult equivalenceDepth naturalNumbers setB :> ICivicResult<int seq>

printfn "Success: %b" diffResult3.Success
printfn "Message: %A" diffResult3.Message
printfn "Provenance Note: %s" diffResult3.Provenance.Note
printfn "Provenance Source: %A" diffResult3.Provenance.SourceName

match diffResult3.Value with
| Some seqVal ->
    printfn "Difference Elements (sample): %A" (seqVal |> Seq.truncate 10 |> Seq.toList)
| None ->
    printfn "No difference yielded." 
printfn " " 

// ---------------------------
// Inspect results
// ---------------------------
let uSet = (Union.tryHomotypicLifted unionSet).Value
printfn "Union symbol: %A" uSet//.Symbol
printfn "Union metadata:"
uSet.Metadata |> Seq.iter (function Tag t -> printfn " Tag: %s" t | Provenance p -> printfn " Prov: %s step=%d" p.SourceName p.Step | _ -> ())
printfn "Union elements (lifted sets):"
for e in uSet.Elements do
    match e with
    | A cell -> printfn " A -> set %A with prov %A" (cell.Value.Symbol) cell.Provenance.Value.SourceName
    | B cell -> printfn " B -> set %A with prov %A" (cell.Value.Symbol) cell.Provenance.Value.SourceName
    | Nested cell -> printfn " Nested -> %A" cell

// Flatten members for demonstration
let flattened =
    Union.flattenLiftedMembersSeq uSet

printfn "Flattened members (Choice): %A" (flattened |> Seq.toList)
let collapsed = Union.tryCollapseCivicUnionToConcrete true false civicSet1

// Collapse to concrete example
let CollapsedToConcrete =    
    match collapsed with
    | Some x -> x.Elements
    | None -> []

printfn "Collapsed to Concrete: %A" (CollapsedToConcrete |> Seq.toList)
// printfn "Collapsed to Concrete Provenance: %A" ((collapsed.Value).Metadata |> List.tryPick (function Provenance p -> Some p | _ -> None))

printfn "Collapsed to Concrete Report:%s" (civicSetInspectorReport collapsed)
printfn "%s" (Provenance.EmitSourceWithLineageTrail ((collapsed.Value).Metadata |> List.tryPick (function Provenance p -> Some p | _ -> None)).Value)
printfn " "    

