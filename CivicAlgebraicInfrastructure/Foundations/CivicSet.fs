namespace CivicAlgebraicInfrastructure.Foundations.CivicSet

open System
open CivicAlgebraicInfrastructure.Foundations.Primitives
open CivicAlgebraicInfrastructure.Foundations.FOL

/// <summary>
/// Cardinality classification used for set-theoretic signage and provenance.
/// </summary>
/// <signage>
/// - Finite: concrete integer cardinality used for precise signage.
/// - Aleph0: countably infinite, signage-friendly symbol ℵ₀.
/// - Continuum: cardinality of real numbers, signage-friendly symbol 2^ℵ₀.
/// - Other: extension point for domain-specific cardinalities, carry a short identifier.
/// </signage>
type Cardinality =
    | Finite of int
    | Aleph0    // ℵ₀
    | Continuum // 2^ℵ₀
    | Other of string

/// <summary>
/// Countability classification for sets used in metadata merge rules and signage overlays.
/// </summary>
/// <signage>
/// - Countable: supports enumeration signs and enumerator overlays.
/// - Uncountable: signals continuum-like behavior; dominates merges.
/// </signage>
type Countability =
    | Countable
    | Uncountable

/// <summary>
/// Order type of a set used to declare ordering affordances on signage.
/// </summary>
/// <signage>
/// - TotalOrder: every pair comparable; signage can show min/max and ordering rules.
/// - PartialOrder: some pairs incomparable; signage should include comparator policy.
/// - Unordered: no ordering; signage indicates unordered collection.
/// </signage>
type OrderType =
    | TotalOrder
    | PartialOrder
    | Unordered

/// <summary>
/// Aggregated set-theoretic metadata: cardinality, countability, and order type.
/// </summary>
/// <signage>
/// Used as a primary payload for civic signage about sets; can be attached to sets
/// to form overlays that inform UI, proof-checkers, or migrators.
/// </signage>
type SetTheoreticMetadata =
    { Cardinality  : Cardinality option
      Countability : Countability option
      OrderType    : OrderType option }

/// <summary>
/// Union type for metadata items that can be attached to civic sets as signage artifacts.
/// </summary>
/// <signage>
/// - SetTheoretic: structured set-theory metadata for tooling and documentation.
/// - FOL: logical overlay derived from first-order logic metadata.
/// - Provenance: explicit provenance stripe for lineage tracing.
/// - Tag: short free-form signage token for indexers and search.
/// - Note: longer human-readable signage for onboarding overlays.
/// - Custom: extension point (key,value) for downstream consumers.
/// </signage>
type CivicSetMetadataItem =
    | SetTheoretic of SetTheoreticMetadata
    | FOL of FOLMetadata
    | Provenance of Provenance  // e.g. "Defined in ZFC", "Derived from Peano axioms"
    | Tag of string             // free-form civic signage
    | Note of string
    | Custom of string * string // extension point

type SetResult<'T>(value:'T option, success:bool, message:string option, provenance:Provenance) =
    interface ICivicResult<'T> with
        member _.Value = value
        member _.Success = success
        member _.Message = message
        member _.Provenance = provenance

    /// Convenience constructors
    static member Succeed(value:'T, ?msg, ?prov) =
        SetResult(Some value, true, msg, defaultArg prov Provenance.empty)
    static member FailWithValue(value:'T, ?msg, ?prov) =
        SetResult(Some value, false, msg, defaultArg prov Provenance.empty)
    static member Fail(msg:string, ?prov) =
        SetResult(None, false, Some msg, defaultArg prov Provenance.empty)
    static member Default() =
        SetResult<'T>(None, false, Some "Default SetResult", Provenance.empty)

/// <summary>
/// Interface describing a civic set with both concrete and symbolic overlays.
/// </summary>
/// <typeparam name="Concrete">Element type exposed by the concrete set</typeparam>
/// <signage>
/// ICivicSet is the primary contract for all civic set representations. Implementations
/// should prefer to include Metadata entries with SetTheoretic and Provenance items
/// to enable predictable merging and signage syntheses.
/// </signage>  
type ICivicSet<'Concrete> =
    /// Symbolic name for the set used in signage synthesis.
    abstract member Symbol : string option
    /// Optional symbolic formula describing the set (FOL overlay).
    abstract member Formula : Formula<Symbol> option
    /// Predicate testing membership of a concrete element.
    abstract member Contains : 'Concrete -> bool
    /// Elements sequence for enumeration, used by collapse and flatten operations.
    abstract member Elements : seq<'Concrete>
    /// Optional comparer used for ordering, if available.
    abstract member Compare : option<'Concrete -> 'Concrete -> int>
    /// Optional minimal element when ordering is present.
    abstract member Min : option<'Concrete>
    /// Optional maximal element when ordering is present.
    abstract member Max : option<'Concrete>
    /// Attached metadata signage artifacts.
    abstract member Metadata : CivicSetMetadataItem list
    /// Logical overlay: tests closedness under set operators.
    abstract member IsClosedUnder : (ICivicSet<'Concrete> -> ICivicSet<'Concrete>) -> SetResult<bool>
    /// Logical overlay: implication relation to another civic set.
    abstract member Implies : ICivicSet<'Concrete> -> SetResult<ICivicSet<'Concrete>>
    /// Logical overlay: equivalence relation to another civic set.
    abstract member EquivalentTo : ICivicSet<'Concrete> -> SetResult<bool>

/// <summary>
/// Lifted union of homotypic civic sets (same concrete type).
/// </summary>
type HomotypicUnion<'A> = ICivicSet<Lifted<ICivicSet<'A>, ICivicSet<'A>>>

/// <summary>
/// Lifted union of heterotypic civic sets (different concrete types).
/// </summary>
type HeterotypicUnion<'A,'B> = ICivicSet<Lifted<ICivicSet<'A>, ICivicSet<'B>>>

/// <summary>
/// Tagged union wrapper used to represent lifted unions in the civic model.
/// </summary>
type CivicUnion<'A,'B> = 
    | Homotypic of HomotypicUnion<'A>
    | Heterotypic of HeterotypicUnion<'A,'B>

module SetTheoreticMetadata =
    /// Canonical empty instance
    let empty = { Cardinality = None; Countability = None; OrderType = None }

    /// Builders that always return a record (composable)
    let withCardinality c m = { m with Cardinality = Some c }
    let withCountability ct m = { m with Countability = Some ct }
    let withOrderType o m = { m with OrderType = Some o }

    /// Cardinality merge rules:
    /// - If both Finite, return Finite sum
    /// - Aleph0 or Continuum dominates (choose broader)
    /// - If kinds mismatch and neither is dominating, prefer the non-None; otherwise Other with a combined id
    let private mergeCardinality 
        (a: Cardinality option) 
        (b: Cardinality option) 
        (elementCount: int option) 
        : Cardinality option =
        
        match a, b with
        | None, None -> None
        | Some x, None | None, Some x -> Some x
        | Some (Finite n1), Some (Finite n2) -> Some (Finite (match elementCount with | Some c -> c | None -> n1 + n2))
        | Some Aleph0, _ | _, Some Aleph0 -> Some Aleph0
        | Some Continuum, _ | _, Some Continuum -> Some Continuum
        | Some (Other s1), Some (Other s2) -> Some (Other (s1 + ";" + s2))
        | Some (Other s), Some _ -> Some (Other s)
        | Some _, Some (Other s) -> Some (Other s)

    /// Countability: Uncountable dominates
    let private mergeCountability (a: Countability option) (b: Countability option) : Countability option =
        match a, b with
        | None, None -> None
        | Some x, None | None, Some x -> Some x
        | Some Uncountable, _ | _, Some Uncountable -> Some Uncountable
        | Some Countable, Some Countable -> Some Countable

    /// OrderType: Unordered dominates; TotalOrder preserved only when both TotalOrder
    let private mergeOrderType (a: OrderType option) (b: OrderType option) : OrderType option =
        match a, b with
        | None, None -> None
        | Some x, None | None, Some x -> Some x
        | Some Unordered, _ | _, Some Unordered -> Some Unordered
        | Some TotalOrder, Some TotalOrder -> Some TotalOrder
        | Some PartialOrder, Some PartialOrder -> Some PartialOrder
        | Some TotalOrder, Some PartialOrder
        | Some PartialOrder, Some TotalOrder -> Some PartialOrder

    /// Merge two optional SetTheoreticMetadata values into one following dominance rules.
    /// If both inputs are None -> None
    let mergeSetTheoreticMetadata 
        (left: SetTheoreticMetadata option) 
        (right: SetTheoreticMetadata option) 
        (elementCount: int option) 
        : SetTheoreticMetadata =
        match left, right with
        | None, None -> empty
        | _ ->
            let l = defaultArg left empty
            let r = defaultArg right empty
            let merged = {
                Cardinality = mergeCardinality l.Cardinality r.Cardinality (elementCount)
                Countability = mergeCountability l.Countability r.Countability
                OrderType = mergeOrderType l.OrderType r.OrderType
            }
            // If merged has no information, return None to preserve original optionality semantics
            if merged.Cardinality.IsNone && merged.Countability.IsNone && merged.OrderType.IsNone then
                empty
            else
                merged

    /// Extract SetTheoreticMetadata from a metadata token list; returns option
    let tryPickFrom (items: CivicSetMetadataItem list) : SetTheoreticMetadata option =
        items
        |> List.tryPick (function
            | CivicSetMetadataItem.SetTheoretic s -> Some s
            | _ -> None)

module Operations =
    
    /// <summary>
    /// Computes the set difference between two civic sets, returning all elements
    /// in <paramref name="this"/> that are not contained in <paramref name="other"/>.
    /// </summary>
    let setDifference (this: ICivicSet<'T>) (other: ICivicSet<'T>) : SetResult<'T list> =
        
        let pickProvenanceFromSetMetadata (meta: CivicSetMetadataItem list) : Provenance option =
            meta |> List.tryPick (function Provenance p -> Some p | _ -> None)
        
        let sourceName = sprintf "setDifference (%s \ %s%s)"
                                    (defaultArg this.Symbol  "A")                                    
                                    (defaultArg other.Symbol "B")
                                    (defaultArg this.Symbol  "A")

        let prov =                
                let provs  = List.map pickProvenanceFromSetMetadata [this.Metadata;other.Metadata]
                let derivedProv = Provenance.mkDerived sourceName "setDifference" provs
                derivedProv

        let difference = 
            this.Elements
            |> Seq.filter (fun x -> not (other.Contains x))
            |> Seq.toList

        SetResult.Succeed ( difference, sourceName, prov )
 
module CivicSetConstructors =
    /// <summary>
    /// Extracts the first Provenance entry from metadata if present.
    /// </summary>
    /// <returns>Some provenance or None.</returns>
    /// <signage>Used by constructors to inherit and synthesize derived provenance.</signage>
    let private pickProvenanceFromSetMetadata (meta: CivicSetMetadataItem list) : Provenance option =
        meta |> List.tryPick (function Provenance p -> Some p | _ -> None)

    /// <summary>
    /// Extracts the first SetTheoretic metadata entry from metadata if present.
    /// </summary>
    /// <returns>Some set-theoretic metadata or None.</returns>
    /// <signage>Used during merges to preserve cardinality and ordering overlays.</signage>
    let private pickSetTheoreticMetadataFromSetMetadata (meta: CivicSetMetadataItem list) : SetTheoreticMetadata option =
        meta |> List.tryPick (function SetTheoretic st -> Some st | _ -> None)  

    /// <summary>
    /// Try to obtain the minimal element from a sequence using an optional comparer.
    /// </summary>
    /// <returns>Option minimal element or None when no comparer is available.</returns>
    /// <signage>Utility used by collapse operations to populate Min signage.</signage>
    let private tryMinByCompare<'T> (compareOpt: option<'T -> 'T -> int>) (items: seq<'T>) : option<'T> =
        match compareOpt with
        | Some cmp -> items |> Seq.sortWith cmp |> Seq.tryHead
        | None -> None

    /// <summary>
    /// Try to obtain the maximal element from a sequence using an optional comparer.
    /// </summary>
    /// <returns>Option maximal element or None when no comparer is available.</returns>
    /// <signage>Utility used by collapse operations to populate Max signage.</signage>
    let private tryMaxByCompare<'T> (compareOpt: option<'T -> 'T -> int>) (items: seq<'T>) : option<'T> =
        match compareOpt with
        | Some cmp -> items |> Seq.sortWith (fun x y -> cmp y x) |> Seq.tryHead
        | None -> None

    /// <summary>
    /// Synthesize a canonical symbolic union formula from two symbol constants.
    /// </summary>
    /// <remarks>
    /// Produces a quantified formula ∀x. x ∈ A ∨ x ∈ B when symbols are available; used when
    /// two sets carry only symbol names rather than full formulas.
    /// </remarks>
    /// <signage>Emits a compact symbolic union overlay for signage when formulas absent.</signage>
    let private canonicalUnionFromSymbols (aSym: Symbol) (bSym: Symbol) : Formula<Symbol> =
        let x = { Name = "x"; Kind = VariableKind; Arity = None }
        let inA = Formulae.memberOf (Var x) (Constant aSym)
        let inB = Formulae.memberOf (Var x) (Constant bSym)
        Quantified { Bound = ForAll x; Body = Formulae.unionFormula inA inB }

    /// <summary>
    /// Build a synthesized symbolic union using available formulas or symbols from two civic sets.
    /// </summary>
    /// <returns>Optional synthesized Formula<Symbol> representing the union.</returns>
    /// <signage>
    /// Priority:
    /// 1. If both sets have symbol-level formulas, union them.
    /// 2. If both have symbol names, build canonical quantified union from symbols.
    /// 3. If one side has a formula and the other a symbol, synthesize by combining membership and the formula.
    /// </signage>
    let private synthesizeSymbolicUnion (a: ICivicSet<_>) (b: ICivicSet<_>) : Formula<Symbol> option =
        let fA = a.Formula
        let fB = b.Formula
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

    /// <summary>
    /// Constructs a lifted set-of-sets ICivicSet whose Elements are two Lifted cells wrapping operand sets.
    /// </summary>
    /// <param name="aName">Symbolic name for the first operand set, used in provenance and signage synthesis.</param>
    /// <param name="bName">Symbolic name for the second operand set, used in provenance and signage synthesis.</param>
    /// <param name="a">First civic set to be lifted and unioned.</param>
    /// <param name="b">Second civic set to be lifted and unioned.</param>
    /// <returns>
    /// A lifted civic set containing wrappers for both input sets, with derived provenance and optional symbolic formula.
    /// </returns>
    /// <signage>
    /// - Tag: LiftedUnion
    /// - Provenance: derived from parents, step "union/lifted"
    /// - Metadata: merged SetTheoretic metadata computed from children
    /// </signage>
    let unionLiftedSets<'A,'B>
        (aName: string) (bName: string)
        (a: ICivicSet<'A>) (b: ICivicSet<'B>)
        : ICivicSet<Lifted<ICivicSet<'A>, ICivicSet<'B>>> =

        // parent provenance
        let pa = pickProvenanceFromSetMetadata a.Metadata
        let pb = pickProvenanceFromSetMetadata b.Metadata
        let sharedSource = $"{aName} ∪ {bName}"

        // LiftedCell wrappers for both sets (derive provenance step from parents)
        let cellA : LiftedCell<ICivicSet<'A>> =
            { Value = a
              Provenance = Some (Provenance.mkDerived "union(A-wrapper)" "union/lifted" [ pa ]) }

        let cellB : LiftedCell<ICivicSet<'B>> =
            { Value = b
              Provenance = Some (Provenance.mkDerived $"union (B-wrapper)" "union/lifted" [ pb ]) }

        let elementsSeq : seq<Lifted<ICivicSet<'A>, ICivicSet<'B>>> = seq { yield A cellA; yield B cellB }

        // set theoretic metadata
        let sa = pickSetTheoreticMetadataFromSetMetadata a.Metadata
        let sb = pickSetTheoreticMetadataFromSetMetadata b.Metadata
        let mergedSetTheoreticMetadata = SetTheoreticMetadata.mergeSetTheoreticMetadata sa sb (Some (Seq.length elementsSeq))

        let containsImpl (z: Lifted<ICivicSet<'A>, ICivicSet<'B>>) : bool =
            match z with
            | A c -> Object.ReferenceEquals(c.Value, a) || (c.Value.Symbol = a.Symbol)
            | B c -> Object.ReferenceEquals(c.Value, b) || (c.Value.Symbol = b.Symbol)
            | Nested _ -> false

        // synthesize top-level formula if possible
        let formulaSymbolicOpt = synthesizeSymbolicUnion a b
        let formulaOpt =
            match formulaSymbolicOpt with
            | Some fs -> Some (box fs :?> Formula<Symbol>)
            | _ -> None

        let unionSymbol =
            match a.Symbol, b.Symbol with
            | Some sa, Some sb -> Some $"{sa} ∪ {sb}"
            | _ -> None

        let unionProv = Provenance.mkDerived (unionSymbol |> Option.defaultValue sharedSource) "union/lifted" [ pa; pb ]



        { new ICivicSet<Lifted<ICivicSet<'A>, ICivicSet<'B>>> with
            member _.Symbol : string option = unionSymbol
            member _.Formula : Formula<Symbol> option = formulaOpt
            member _.Contains (z: Lifted<ICivicSet<'A>, ICivicSet<'B>>) : bool = containsImpl z
            member _.Elements : seq<Lifted<ICivicSet<'A>, ICivicSet<'B>>> = elementsSeq
            member _.Compare = None
            member _.Min = None
            member _.Max = None
            member _.Metadata : CivicSetMetadataItem list = [ Tag "LiftedUnion" ] @ [SetTheoretic mergedSetTheoreticMetadata] @ [ Provenance unionProv ]
            member _.IsClosedUnder _ = SetResult.Default()
            member _.Implies _ = SetResult.Default()
            member _.EquivalentTo _ = SetResult.Default() }        

    /// <summary>
    /// Wraps a lifted union into the CivicUnion tagged representation.
    /// </summary>
    let wrapCivicUnion<'A,'B>
        (liftedSet: ICivicSet<Lifted<ICivicSet<'A>, ICivicSet<'B>>>)
        : CivicUnion<'A,'B> =

        match liftedSet with
        | :? HomotypicUnion<'A> as a -> Homotypic a
        | _ -> Heterotypic liftedSet

    /// <summary>
    /// Recursively collect Choice<'A,'B> elements from a nested Lifted payload where the Lifted
    /// payloads are whole ICivicSet instances. When encountering A or B, enumerate that set's Elements.
    /// </summary>
    /// <returns>Sequence of Choice representing flattened members.</returns>
    /// <signage>Handles arbitrarily nested Nested branches so flattening signage can be emitted consistently.</signage>
    let rec private recFlattenLifted<'A,'B>
        (lifted: Lifted<ICivicSet<'A>, ICivicSet<'B>>)
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
                for inner in recFlattenLifted<'A,'B> cell.Value do
                    yield inner
        }

    /// <summary>
    /// Flatten the lifted set-of-sets into a sequence of Choice<'A,'B>.
    /// </summary>
    /// <param name="liftedSet">Lifted set instance.</param>
    /// <returns>Flattened sequence of members with branch tags.</returns>
    /// <signage>Used by collapse operations and for readable signage of nested unions.</signage>
    let flattenLiftedMembers<'A,'B>
        (liftedSet: ICivicSet<Lifted<ICivicSet<'A>, ICivicSet<'B>>>)
        : seq<Choice<'A,'B>> =
        liftedSet.Elements
        |> Seq.collect (fun top ->
            match top with
            | A c -> c.Value.Elements |> Seq.map Choice1Of2
            | B c -> c.Value.Elements |> Seq.map Choice2Of2
            | Nested c -> recFlattenLifted<'A,'B> c.Value)

    /// <summary>
    /// Collapse a lifted set-of-sets into a concrete ICivicSet<'T,'S> when both branches contain sets of the same element type 'T.
    /// </summary>
    /// <param name="deDuplicate">Whether to distinct elements (default true).</param>
    /// <param name="collapseWhenProvenanceDiffers">When false and provenance differs, returns None so caller decides.</param>
    /// <param name="liftedSet">Lifted set-of-sets instance.</param>
    /// <returns>Some concrete ICivicSet when collapse permitted, otherwise None.</returns>
    /// <signage>
    /// Collapse policy:
    /// - If provenance differs and collapseWhenProvenanceDiffers is false, refuse collapse to preserve lineage.
    /// - If comparer absent, collapse is refused to prevent accidental reordering semantics.
    /// - De-duplication uses comparer if requested; otherwise elements concatenated in original order.
    /// Emits Metadata: Tag "CollapsedFromLiftedUnion" and derived Provenance step "collapse lifted to concrete".
    /// </signage>
    let private collapseLiftedToConcrete<'T>
        (deDuplicate: bool)
        (collapseWhenProvenanceDiffers: bool)
        (liftedSet: HomotypicUnion<'T>)
        : option<ICivicSet<'T>> =

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
                |> List.map (fun p -> 
                    match Provenance.GetStepOneProvenance p with
                    | Some p1 -> Provenance.EmitSourceWithLineageTrail p1
                    | None -> Provenance.EmitSourceWithLineageTrail p)
                |> List.distinct

            let canCollapse =
                collapseWhenProvenanceDiffers
                || List.length distinctSources <= 1
        
            let comparer =
                parts
                |> List.map (fun x -> (fst x).Compare)
                |> List.tryPick id

            match canCollapse, comparer  with 
            | false, _ | true, None -> None
            | true, Some c ->
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

                // 2) apply deDuplicate or identity (both are seq<'T> -> seq<'T>)
                let deDuplicated : seq<'T> =
                    match deDuplicate, comparer with
                    | true, Some c -> distinctByCompare c collected 
                    | _, _ -> collected
                
                let setTheoreticMetadata = 
                    let stData = pickSetTheoreticMetadataFromSetMetadata liftedSet.Metadata
                    match stData with
                    | Some d -> { d with Cardinality  = Some (Seq.length deDuplicated |> Finite) }
                    | None -> SetTheoreticMetadata.empty

                let min = 
                    match deDuplicate, comparer with
                    | true, Some c -> 
                        distinctByCompare c collected 
                        |> tryMinByCompare comparer
                    | _, _ -> None         
                
                let max = 
                    match deDuplicate, comparer with
                    | true, Some c -> 
                        distinctByCompare c collected 
                        |> tryMaxByCompare comparer
                    | _, _ -> None

                let formulaOpt: Formula<Symbol> option = liftedSet.Formula

                let parentProvs = parts |> List.map snd
                let sharedSource = match liftedSet.Symbol with Some s -> s | None -> "collapsed-lifted"
                let derived = Provenance.mkDerived sharedSource "collapse lifted to concrete"parentProvs

                let symbol = liftedSet.Symbol

                let concrete : ICivicSet<'T> =
                    //let rec mk =
                    
                    { new ICivicSet<'T> with
                        member _.Symbol = symbol
                        member _.Formula = formulaOpt
                        member _.Contains (x:'T) = parts |> Seq.exists (fun (set,_) -> set.Contains x)
                        member _.Elements = deDuplicated
                        member _.Compare = comparer
                        member _.Min = min
                        member _.Max = max
                        member _.Metadata = [ Tag "CollapsedFromLiftedUnion" ] @ [ SetTheoretic setTheoreticMetadata ] @ [ Provenance derived ]
                        member _.IsClosedUnder _ = SetResult.Default()
                        member _.Implies _ = SetResult.Default()
                        member _.EquivalentTo _ = SetResult.Default() }

                Some concrete        
        
    /// <summary>
    /// Attempts to collapse any CivicUnion into a concrete civic set when the concrete types align; returns None when types are incompatible.
    /// </summary>
    let tryCollapseCivicUnionToConcrete<'A,'B>
        (deDuplicate: bool)
        (collapseWhenProvenanceDiffers: bool)
        (liftedSet: CivicUnion<'A,'B>)
        : option<ICivicSet<'A>> =

        match liftedSet with
        | Homotypic a -> collapseLiftedToConcrete deDuplicate collapseWhenProvenanceDiffers a
        | Heterotypic b-> None


