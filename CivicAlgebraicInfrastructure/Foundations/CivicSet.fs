namespace CivicAlgebraicInfrastructure.Foundations.CivicSet

open System
open CivicAlgebraicInfrastructure.Foundations.Primitives
open CivicAlgebraicInfrastructure.Foundations.FOL

type Cardinality =
    | Finite of int
    | Aleph0    // ℵ₀
    | Continuum // 2^ℵ₀
    | Other of string

type Countability =
    | Countable
    | Uncountable

type OrderType =
    | TotalOrder
    | PartialOrder
    | Unordered

type SetTheoreticMetadata =
    { Cardinality  : Cardinality option
      Countability : Countability option
      OrderType    : OrderType option }

type CivicSetMetadataItem =
    | SetTheoretic of SetTheoreticMetadata
    | FOL of FOLMetadata
    | Provenance of Provenance  // e.g. "Defined in ZFC", "Derived from Peano axioms"
    | Tag of string             // free-form civic signage
    | Note of string
    | Custom of string * string // extension point

type SetProvenance =
    | FromSet of string
    | FromUnion of SetProvenance list
    | FromIntersection of SetProvenance list
    | FromComplement of SetProvenance
    | FromConstructor of string
    | FromAxiom of string
    | FromImport of string
    | FromSymbolic of Formula<Symbol>

// Interface   
type ICivicSet<'Concrete,'Symbolic> =
    abstract member Symbol : string option
    abstract member Formula : Formula<'Symbolic> option
    abstract member Contains : 'Concrete -> bool
    abstract member Elements : seq<'Concrete>

    // Ordering
    abstract member Compare : option<'Concrete -> 'Concrete -> int>
    abstract member Min : option<'Concrete>
    abstract member Max : option<'Concrete>

    // Meta-signage (derived or declared)
    abstract member Metadata : CivicSetMetadataItem list

    // Logical overlays
    abstract member IsClosedUnder : (ICivicSet<'Concrete,'Symbolic> -> ICivicSet<'Concrete,'Symbolic>) -> bool
    abstract member Implies : ICivicSet<'Concrete,'Symbolic> -> bool
    abstract member EquivalentTo : ICivicSet<'Concrete,'Symbolic> -> bool

module CivicSetConstructors =
    /// Helpers for set metadata / provenance extraction
    let private pickProvenanceFromSetMetadata (meta: CivicSetMetadataItem list) : Provenance option =
        meta |> List.tryPick (function Provenance p -> Some p | _ -> None)    

    /// synthesize canonical quantified union ∀x. x∈A ∨ x∈B from two Symbols
    let private canonicalUnionFromSymbols (aSym: Symbol) (bSym: Symbol) : Formula<Symbol> =
        let x = { Name = "x"; Kind = VariableKind; Arity = None }
        let inA = Formulae.memberOf (Var x) (Constant aSym)
        let inB = Formulae.memberOf (Var x) (Constant bSym)
        Quantified { Bound = ForAll x; Body = Formulae.unionFormula inA inB }

    /// Try to unbox Formula<'S> to Formula<Symbol> when S = Symbol
    let private tryUnboxFormula<'S> (fOpt: Formula<'S> option) : Formula<Symbol> option =
        match fOpt with
        | Some f when typeof<'S> = typeof<Symbol> -> Some (unbox<Formula<Symbol>> (box f))
        | _ -> None

    /// Build synthesized symbolic union (Formula<Symbol> option) using available formulas or symbols
    let private synthesizeSymbolicUnion (a: ICivicSet<_,_>) (b: ICivicSet<_,_>) : Formula<Symbol> option =
        let fA = tryUnboxFormula a.Formula
        let fB = tryUnboxFormula b.Formula
        let sA = a.Symbol |> Option.map (fun n -> { Name = n; Kind = ConstantKind; Arity = None })
        let sB = b.Symbol |> Option.map (fun n -> { Name = n; Kind = ConstantKind; Arity = None })
        match fA, fB, sA, sB with
        | Some fa, Some fb, _, _ -> Some (Formulae.unionFormula fa fb)
        | None, None, Some sa, Some sb -> Some (canonicalUnionFromSymbols sa sb)
        | Some fa, None, _, Some sb ->
            let x = { Name = "x"; Kind = VariableKind; Arity = None }
            Some (Formulae.unionFormula fa (Formulae.memberOf (Var x) (Constant sb)))
        | None, Some fb, Some sa, _ ->
            let x = { Name = "x"; Kind = VariableKind; Arity = None }
            Some (Formulae.unionFormula (Formulae.memberOf (Var x) (Constant sa)) fb)
        | _ -> None

    /// Build a lifted set-of-sets ICivicSet whose Elements are two Lifted cells that hold the operand sets.
    /// Both operands are wrapped in LiftedCell; resulting type is always
    /// ICivicSet<Lifted<ICivicSet<'A,'S>, ICivicSet<'B,'S>>,'S>.
    let unionLiftedSets<'A,'B,'S when 'S :> obj>
        (aName: string) (bName: string)
        (a: ICivicSet<'A,'S>) (b: ICivicSet<'B,'S>)
        : ICivicSet<Lifted<ICivicSet<'A,'S>, ICivicSet<'B,'S>>,'S> =

        // parent provenance
        let pa = pickProvenanceFromSetMetadata a.Metadata
        let pb = pickProvenanceFromSetMetadata b.Metadata
        let sharedSource = $"{aName} ∪ {bName}"

        // LiftedCell wrappers for both sets (derive provenance step from parents)
        let cellA : LiftedCell<ICivicSet<'A,'S>> =
            { Value = a
              Provenance = Some (Provenance.mkDerived "union(A-wrapper)" "union/lifted" [ pa; pb ]) }

        let cellB : LiftedCell<ICivicSet<'B,'S>> =
            { Value = b
              Provenance = Some (Provenance.mkDerived $"union (B-wrapper)" "union/lifted" [ pa; pb ]) }

        let elementsSeq : seq<Lifted<ICivicSet<'A,'S>, ICivicSet<'B,'S>>> = seq { yield A cellA; yield B cellB }

        let containsImpl (z: Lifted<ICivicSet<'A,'S>, ICivicSet<'B,'S>>) : bool =
            match z with
            | A c -> Object.ReferenceEquals(c.Value, a) || (c.Value.Symbol = a.Symbol)
            | B c -> Object.ReferenceEquals(c.Value, b) || (c.Value.Symbol = b.Symbol)
            | Nested _ -> false

        // synthesize top-level formula if possible
        let formulaSymbolicOpt = synthesizeSymbolicUnion a b
        let formulaOpt =
            match formulaSymbolicOpt with
            | Some fs when typeof<'S> = typeof<Symbol> -> Some (box fs :?> Formula<'S>)
            | _ -> None

        let unionSymbol =
            match a.Symbol, b.Symbol with
            | Some sa, Some sb -> Some $"{sa} ∪ {sb}"
            | _ -> None

        let unionProv = Provenance.mkDerived (unionSymbol |> Option.defaultValue sharedSource) "union/lifted" [ pa; pb ]

        { new ICivicSet<Lifted<ICivicSet<'A,'S>, ICivicSet<'B,'S>>,'S> with
            member _.Symbol : string option = unionSymbol
            member _.Formula : Formula<'S> option = formulaOpt
            member _.Contains (z: Lifted<ICivicSet<'A,'S>, ICivicSet<'B,'S>>) : bool = containsImpl z
            member _.Elements : seq<Lifted<ICivicSet<'A,'S>, ICivicSet<'B,'S>>> = elementsSeq
            member _.Compare = None
            member _.Min = None
            member _.Max = None
            member _.Metadata : CivicSetMetadataItem list = [ Tag "LiftedUnion" ] @ [ Provenance unionProv ]
            member _.IsClosedUnder _ = false
            member _.Implies _ = false
            member _.EquivalentTo _ = false }

    /// Recursively collect Choice<'A,'B> elements from a nested Lifted<'A,'B>
    /// Here the Lifted payloads are whole ICivicSet<'A,'S> and ICivicSet<'B,'S>,
    /// so when we hit an A or B we enumerate that set's Elements.
    let rec private recFlattenLifted<'A,'B,'S>
        (lifted: Lifted<ICivicSet<'A,'S>, ICivicSet<'B,'S>>)
        : seq<Choice<'A,'B>> =
        seq {
            match lifted with
            | A cell ->
                for e in cell.Value.Elements do
                    yield Choice1Of2 e
            | B cell ->
                for e in cell.Value.Elements do
                    yield Choice2Of2 e
            | Nested cell ->
                // cell.Value is another Lifted<ICivicSet<'A,'S>, ICivicSet<'B,'S>>
                for inner in recFlattenLifted<'A,'B,'S> cell.Value do
                    yield inner
        }

    /// Flatten the lifted set-of-sets into a sequence of Choice<'A,'B>.
    /// Handles arbitrarily nested Nested branches.
    let flattenLiftedMembers<'A,'B,'S>
        (liftedSet: ICivicSet<Lifted<ICivicSet<'A,'S>, ICivicSet<'B,'S>>,'S>)
        : seq<Choice<'A,'B>> =
        liftedSet.Elements
        |> Seq.collect (fun top ->
            match top with
            | A c -> c.Value.Elements |> Seq.map Choice1Of2
            | B c -> c.Value.Elements |> Seq.map Choice2Of2
            | Nested c -> recFlattenLifted<'A,'B,'S> c.Value)

    /// Collapse a lifted set-of-sets into a concrete ICivicSet<'T,'S> when both branches contain sets of the same element type 'T.
    /// - dedup: whether to distinct elements (default true)
    /// - collapseWhenProvenanceDiffers: when false and provenance differs, returns None to indicate caller must decide
    let collapseLiftedToConcrete<'T,'S when 'S :> obj>
        (dedup: bool)
        (collapseWhenProvenanceDiffers: bool)
        (liftedSet: ICivicSet<Lifted<ICivicSet<'T,'S>, ICivicSet<'T,'S>>,'S>)
        : option<ICivicSet<'T,'S>> =

        // collect underlying sets (expecting A and B branches)
        let parts =
            liftedSet.Elements
            |> Seq.choose (function
                | A c -> Some (c.Value, c.Provenance)
                | B c -> Some (c.Value, c.Provenance)
                | Nested _ -> None)
            |> Seq.toList

        match parts.IsEmpty with
        | true-> None
        | false ->
            let provs =
                parts
                |> List.choose snd

            let distinctSources =
                provs
                |> List.map (fun p -> p.SourceName)
                |> List.distinct

            let canCollapse =
                collapseWhenProvenanceDiffers
                || (List.length distinctSources <= 1)

            let comparer =
                parts
                |> List.map (fun x -> (fst x).Compare)

            match canCollapse with 
            | false -> None
            | true ->
                // 1) collect all elements (one sequence)
                let collected : seq<'T> =
                    parts
                    |> Seq.collect (fun (set, _) -> set.Elements)

                let distinctByCompare (compareFn: 'T -> 'T -> int) (items: seq<'T>) =
                    items
                    |> Seq.sortWith compareFn
                    |> Seq.fold (fun (acc, lastOpt) item ->
                        match lastOpt with
                        | Some last when compareFn last item = 0 -> (acc, lastOpt)
                        | _ -> (item :: acc, Some item)
                    ) ([], None)
                    |> fst
                    |> List.rev

                // 2) apply dedup or identity (both are seq<'T> -> seq<'T>)
                let deduped : seq<'T> =
                    match dedup, comparer.IsEmpty with
                    | true, false -> distinctByCompare comparer.[0].Value collected 
                    | _, _ -> collected
                
                let formulaOpt = liftedSet.Formula

                let parentProvs = parts |> List.map snd
                let sharedSource = match liftedSet.Symbol with Some s -> s | None -> "collapsed-lifted"
                let derived = Provenance.mkDerived sharedSource "collapse lifted to concrete"parentProvs

                let symbol = liftedSet.Symbol

                let concrete : ICivicSet<'T,'S> =
                    { new ICivicSet<'T,'S> with
                        member _.Symbol = symbol
                        member _.Formula = formulaOpt
                        member _.Contains (x:'T) = parts |> Seq.exists (fun (set,_) -> set.Contains x)
                        member _.Elements = deduped
                        member _.Compare = None
                        member _.Min = None
                        member _.Max = None
                        member _.Metadata = [ Tag "LiftedUnion" ] @ [ Provenance derived ]
                        member _.IsClosedUnder _ = false
                        member _.Implies _ = false
                        member _.EquivalentTo _ = false }

                Some concrete        

module Operations =
    
    let isSubsetOf (a: ICivicSet<'T,'S>) (b: ICivicSet<'T,'S>) : bool =
        a.Elements |> Seq.forall b.Contains
