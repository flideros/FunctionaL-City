namespace CivicAlgebraicInfrastructure.Foundations.Primitives

open System
open System.Collections.Concurrent
open Microsoft.FSharp.Reflection

/// Provenance record
type Provenance = { SourceName: string; Step: int }

/// Generic Lifted type used for construction and typed handling
type Lifted<'A,'B> =
  | A of 'A * Provenance option
  | B of 'B * Provenance option

/// Object-view alias used for recursion (no generic runtime tests)
type LiftedObj = Lifted<obj,obj>

module LiftedCore =

  // -----------------------
  // Reflection cache and helpers
  // -----------------------
  module private Reflection =

    /// Thread-safe cache for F# union case arrays per Type
    let private unionCasesCache = ConcurrentDictionary<Type, UnionCaseInfo[]>()

    /// Get union cases with caching
    let getUnionCasesCached (t: Type) : UnionCaseInfo[] =
      unionCasesCache.GetOrAdd(t, fun tt -> FSharpType.GetUnionCases tt)

    let safeIsUnion (t: Type) : bool =
      try FSharpType.IsUnion t with _ -> false

    let fullNameContains (token: string) (t: Type) : bool =
      match t, t.FullName with
      | null, _ -> false
      | _, null -> false
      | _, fn -> fn.IndexOf(token, StringComparison.Ordinal) >= 0

    /// Canonical test for a Lifted`2 runtime union
    let isLiftedType (t: Type) : bool =
      match safeIsUnion t, fullNameContains "Lifted`2" t with
      | true, true -> true
      | _ -> false

    /// Validate and return option case infos using pattern matching only
    let getOptionCases (optTy: Type) : UnionCaseInfo * UnionCaseInfo =
      match optTy with
      | null -> invalidArg "optionType" "optionType is required"
      | ot ->
          match FSharpType.IsUnion ot, ot.IsGenericType, ot.GetGenericTypeDefinition() with
          | true, true, gd when gd = typedefof<Option<_>> ->
              let cases = getUnionCasesCached ot
              let none = cases |> Array.find (fun c -> c.Name = "None")
              let some = cases |> Array.find (fun c -> c.Name = "Some")
              none, some
          | _ -> invalidArg "optionType" "expected an F# option type"

    /// Functional mkOptionOfProvenance: returns a boxed real F# option value using pattern matching
    let mkOptionOfProvenance (optionType: Type) (prov: Provenance option) : obj =
      let noneCase, someCase = getOptionCases optionType
      match prov with
      | None -> FSharpValue.MakeUnion(noneCase, [||])
      | Some p ->
          let elemTy = optionType.GetGenericArguments().[0]
          match elemTy with
          | t when t = typeof<Provenance> -> FSharpValue.MakeUnion(someCase, [| box p |])
          | _ -> invalidArg "optionType" $"unsupported option element type {elemTy.FullName}; expected Provenance"

  // -----------------------
  // Small safe helpers
  // -----------------------
  let inline private tryUnbox<'T> (o: obj) : 'T option =
    match o with
    | null -> None
    | _ ->
      try Some (unbox<'T> o) with _ -> None

  let inline private getTypeOpt (o: obj) : Type option =
    match o with
    | null -> None
    | _ ->
      try Some (o.GetType()) with _ -> None

  // -----------------------
  // Provenance discovery
  // -----------------------
  let rec findNearestProvenance (o: obj) : Provenance option =
    match o with
    | null -> None
    | :? Provenance as p -> Some p
    | :? LiftedObj as lo ->
        match unbox<LiftedObj> lo with
        | A (a, p) -> Option.orElse p (findNearestProvenance a)
        | B (b, p) -> Option.orElse p (findNearestProvenance b)
    | :? string as s -> Some { SourceName = $"source-of-{s}"; Step = 1 }
    | :? int as i -> Some { SourceName = $"source-of-{i}"; Step = 2 }
    | _ ->
        match getTypeOpt o with
        | None -> None
        | Some t ->
            match Reflection.isLiftedType t with
            | true ->
                try
                  let _, fields = FSharpValue.GetUnionFields(o, t)
                  match tryUnbox<Provenance option> fields.[1] with
                  | Some prov -> prov
                  | None ->
                      match fields.[0] with
                      | null -> None
                      | payload -> findNearestProvenance payload
                with _ -> None
            | false -> None

  let provOfField (o: obj) : Provenance option =
    match o with
    | null -> None
    | :? Provenance as p -> Some p
    | _ ->
        match tryUnbox<Provenance option> o with
        | Some provOpt -> provOpt
        | None -> findNearestProvenance o

  // -----------------------
  // Canonicalization: box typed Lifted<'A,'B> into LiftedObj
  // -----------------------
  let private unionMeta = typeof<LiftedObj>
  let private caseMetaA = FSharpType.GetUnionCases(unionMeta) |> Array.find (fun c -> c.Name = "A")
  let private caseMetaB = FSharpType.GetUnionCases(unionMeta) |> Array.find (fun c -> c.Name = "B")

  let private mkCase (caseMeta: UnionCaseInfo) (payload: obj) (provOpt: Provenance option) : LiftedObj =
    let provFieldType = caseMeta.GetFields().[1].PropertyType
    let provBox = Reflection.mkOptionOfProvenance provFieldType provOpt
    FSharpValue.MakeUnion(caseMeta, [| payload; provBox |]) :?> LiftedObj

  /// Try to canonicalize a runtime value that may be a compiled Lifted<'A,'B> into LiftedObj.
  /// Returns Ok LiftedObj on success, or Error message if canonicalization cannot be performed non-lossily.
  let rec tryBoxLiftedTypedObj (v: obj) : Result<LiftedObj,string> =
    match v with
    | null -> Ok (A (null, None))
    | :? LiftedObj as lo -> Ok lo
    | _ ->
        match getTypeOpt v with
        | None -> Ok (A (box v, None))
        | Some t ->
            match Reflection.isLiftedType t with
            | false -> Ok (A (box v, None))
            | true ->
                try
                  let caseInfo, fields = FSharpValue.GetUnionFields(v, t)
                  let prov = provOfField fields.[1]
                  let payload = fields.[0]
                  let rec canonicalizeCase caseName =
                    match caseName with
                    | "A" ->
                        match payload with
                        | null -> Ok (mkCase caseMetaA (box null) prov)
                        | _ ->
                            match getTypeOpt payload with
                            | Some pt ->
                                match Reflection.isLiftedType pt with
                                | true ->
                                    match tryBoxLiftedTypedObj payload with
                                    | Ok nested -> Ok (mkCase caseMetaA (box nested) prov)
                                    | Error e -> Error e
                                | false -> Ok (mkCase caseMetaA (box payload) prov)
                            | None -> Ok (mkCase caseMetaA (box payload) prov)
                    | "B" ->
                        match payload with
                        | null -> Ok (mkCase caseMetaB (box null) prov)
                        | _ ->
                            match getTypeOpt payload with
                            | Some pt ->
                                match Reflection.isLiftedType pt with
                                | true ->
                                    match tryBoxLiftedTypedObj payload with
                                    | Ok nested -> Ok (mkCase caseMetaB (box nested) prov)
                                    | Error e -> Error e
                                | false -> Ok (mkCase caseMetaB (box payload) prov)
                            | None -> Ok (mkCase caseMetaB (box payload) prov)
                    | other -> Error $"unsupported union case '{other}' on lifted type '{t.FullName}'"
                  canonicalizeCase caseInfo.Name
                with ex -> Error $"reflection failure during canonicalization: {ex.Message}"

  /// Lossy wrapper kept for convenience: fall back to previous, permissive behavior.
  /// If tryBoxLiftedTypedObj returns Error, the wrapper preserves boxed input as A payload and preserves provenance if possible.
  let boxLiftedTypedObj (v: obj) : LiftedObj =
    match tryBoxLiftedTypedObj v with
    | Ok lo -> lo
    | Error _ ->
        match getTypeOpt v with
        | Some t ->
            match Reflection.isLiftedType t with
            | true ->
                try
                  let _, fields = FSharpValue.GetUnionFields(v, t)
                  let prov = provOfField fields.[1]
                  mkCase caseMetaA (box v) prov
                with _ -> A (box v, None)
            | false -> A (box v, None)
        | None -> A (box v, None)

  let boxLiftedTyped<'A,'B> (v: Lifted<'A,'B>) : LiftedObj =
    boxLiftedTypedObj (box v)

  // -----------------------
  // Descend to primitive with provenance
  // -----------------------
  let private descendToPrimitiveWithProv (extractor: obj -> Provenance option) (startPayload: obj) (startProv: Provenance option) : obj * Provenance option =
    let rec loop (payload: obj) (nodeProv: Provenance option) =
      match payload with
      | null -> (null, Option.orElse nodeProv (extractor null))
      | :? LiftedObj as lo ->
          match unbox<LiftedObj> lo with
          | A (a, p) -> loop a (Option.orElse p nodeProv)
          | B (b, p) -> loop b (Option.orElse p nodeProv)
      | _ ->
          match getTypeOpt payload with
          | Some pt ->
              match Reflection.isLiftedType pt with
              | true ->
                  let converted = boxLiftedTypedObj payload
                  loop (box converted) nodeProv
              | false -> (payload, Option.orElse nodeProv (extractor payload))
          | None -> (payload, Option.orElse nodeProv (extractor payload))
    loop startPayload startProv

  // -----------------------
  // Traversal: unwrap and map/fold helpers
  // -----------------------
  /// Generic structural walker visiting leaves (non-nested object payloads)
  let rec private walkLiftedObj (onLeaf: obj -> 'r) (v: LiftedObj) : seq<'r> =
    match v with
    | A (a, _) ->
        match a with
        | :? LiftedObj as lo -> walkLiftedObj onLeaf (unbox lo)
        | _ -> Seq.singleton (onLeaf a)
    | B (b, _) ->
        match b with
        | :? LiftedObj as lo -> walkLiftedObj onLeaf (unbox lo)
        | _ -> Seq.singleton (onLeaf b)

  let countLeavesObj (v: LiftedObj) : int =
    walkLiftedObj (fun _ -> 1) v |> Seq.sum

  let rec depthObj (v: LiftedObj) : int =
    match v with
    | A (a, _) ->
        match a with
        | :? LiftedObj as lo -> 1 + depthObj (unbox lo)
        | _ -> 1
    | B (b, _) ->
        match b with
        | :? LiftedObj as lo -> 1 + depthObj (unbox lo)
        | _ -> 1

  let rec mapLiftedRecObj (mapA: obj -> obj) (mapB: obj -> obj) (v: LiftedObj) : LiftedObj =
    match v with
    | A (a, p) ->
        match a with
        | :? LiftedObj as lo -> A (box (mapLiftedRecObj mapA mapB (unbox lo)), p)
        | _ -> A (mapA a, p)
    | B (b, p) ->
        match b with
        | :? LiftedObj as lo -> B (box (mapLiftedRecObj mapA mapB (unbox lo)), p)
        | _ -> B (mapB b, p)

  let rec foldLiftedObj (fA: 's -> obj -> 's) (fB: 's -> obj -> 's) (acc: 's) (v: LiftedObj) : 's =
    match v with
    | A (a, _) ->
        match a with
        | :? LiftedObj as lo -> foldLiftedObj fA fB acc (unbox lo)
        | _ -> fA acc a
    | B (b, _) ->
        match b with
        | :? LiftedObj as lo -> foldLiftedObj fA fB acc (unbox lo)
        | _ -> fB acc b

  // -----------------------
  // Unwrap all (public traversal that preserves provenance)
  // -----------------------
  let rec private unwrapAllObjWithParent (extractor: obj -> Provenance option) (parentProv: Provenance option) (v: LiftedObj) : seq<obj * Provenance option> =
    let nodeProvenance (fieldProv: Provenance option) (payload: obj) =
      Option.orElse fieldProv (Option.orElse parentProv (findNearestProvenance payload))

    let descend (payload: obj) (nodeProv: Provenance option) : seq<obj * Provenance option> =
      match payload with
      | null -> Seq.singleton (null, nodeProv |> Option.orElseWith (fun () -> extractor null))
      | _ ->
          match getTypeOpt payload with
          | Some pt ->
              match Reflection.isLiftedType pt with
              | true -> boxLiftedTypedObj payload |> unwrapAllObjWithParent extractor nodeProv
              | false ->
                  let leaf, prov = descendToPrimitiveWithProv extractor payload nodeProv
                  Seq.singleton (leaf, prov)
          | None ->
              let leaf, prov = descendToPrimitiveWithProv extractor payload nodeProv
              Seq.singleton (leaf, prov)
    match v with
    | A (a, p) -> descend a (nodeProvenance p a)
    | B (b, p) -> descend b (nodeProvenance p b)

  let unwrapAllObj (extractor: obj -> Provenance option) (v: LiftedObj) : seq<obj * Provenance option> =
    unwrapAllObjWithParent extractor None v

  // -----------------------
  // Helpful wrappers (non-exhaustive)
  // -----------------------
  let unwrapAll (extractor: obj -> Provenance option) (v: obj) : seq<obj * Provenance option> =
    match getTypeOpt v with
    | Some t ->
        match Reflection.isLiftedType t with
        | true -> boxLiftedTypedObj v |> unwrapAllObj extractor
        | false ->
            let leaf, prov = descendToPrimitiveWithProv extractor v None
            Seq.singleton (leaf, prov)
    | None ->
        let leaf, prov = descendToPrimitiveWithProv extractor v None
        Seq.singleton (leaf, prov)

  /// Map a typed Lifted<'A,'B> into an object-view LiftedObj, mapping only true payloads of type 'A and 'B.
  /// Preserves nested Lifted nodes and maps leaves only.
  let mapLiftedRec<'A,'B> (mapA: 'A -> obj) (mapB: 'B -> obj) (typed: Lifted<'A,'B>) : LiftedObj =
    let objView = boxLiftedTyped typed
    let mapAobj (o: obj) =
      match o with
      | null -> null
      | :? LiftedObj -> o
      | _ ->
          match tryUnbox<'A> o with
          | Some a -> mapA a
          | None -> o
    let mapBobj (o: obj) =
      match o with
      | null -> null
      | :? LiftedObj -> o
      | _ ->
          match tryUnbox<'B> o with
          | Some b -> mapB b
          | None -> o
    mapLiftedRecObj mapAobj mapBobj objView

  /// Fold over a typed Lifted<'A,'B> by canonicalizing to object view and reusing foldLiftedObj.
  /// Use this when you want a quick fold over leaves; for strictness use tryBoxLiftedTypedObj first.
  let foldLifted<'A,'B,'S> (fA: 'S -> obj -> 'S) (fB: 'S -> obj -> 'S) (acc: 'S) (typed: Lifted<'A,'B>) : 'S =
    boxLiftedTyped typed |> foldLiftedObj fA fB acc

  let countLeaves<'A,'B> (typed: Lifted<'A,'B>) : int = boxLiftedTyped typed |> countLeavesObj
  let depth<'A,'B> (typed: Lifted<'A,'B>) : int = boxLiftedTyped typed |> depthObj

  let tryUnwrapA<'A,'B> (v: Lifted<'A,'B>) : ('A * Provenance option) option =
    match v with A (a,p) -> Some (a,p) | _ -> None

  let tryUnwrapB<'A,'B> (v: Lifted<'A,'B>) : ('B * Provenance option) option =
    match v with B (b,p) -> Some (b,p) | _ -> None

  /// Strict typed mapper: returns a typed Lifted<'A,'B'> with mapped leaves where mapping preserves types where possible.
  /// This is best when you want to remain in the typed world rather than switching to object view.
  let mapLiftedTyped<'A,'B,'A2,'B2>
    (mapA: 'A -> 'A2)
    (mapB: 'B -> 'B2)
    (typed: Lifted<'A,'B>)
    : Lifted<'A2,'B2> =
    let rec goTyped (t: obj) : obj =
      match t with
      | null -> null
      | :? LiftedObj as lo ->
          // already object-view; map recursively on that view then attempt to convert back to typed shape
          let mapped = mapLiftedRec (fun x -> box (mapA x)) (fun y -> box (mapB y)) (unbox lo)
          box mapped
      | _ ->
          match tryUnbox<'A> t with
          | Some a -> box (mapA a)
          | None ->
              match tryUnbox<'B> t with
              | Some b -> box (mapB b)
              | None -> t
    match typed with
    | A (a,p) ->
        match goTyped (box a) with
        | null -> A (Unchecked.defaultof<'A2>, p)
        | o ->
            match tryUnbox<'A2> o with
            | Some a2 -> A (a2, p)
            | None -> A (Unchecked.defaultof<'A2>, p)
    | B (b,p) ->
        match goTyped (box b) with
        | null -> B (Unchecked.defaultof<'B2>, p)
        | o ->
            match tryUnbox<'B2> o with
            | Some b2 -> B (b2, p)
            | None -> B (Unchecked.defaultof<'B2>, p)
