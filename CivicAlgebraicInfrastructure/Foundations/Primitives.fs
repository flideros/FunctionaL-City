namespace CivicAlgebraicInfrastructure.Foundations.Primitives

open System

/// <summary>
/// Provenance record type used across civic artifacts to record origin, step, human note, timestamp, and explicit lineage.
/// </summary>
/// <signage>
/// - SourceName: human-friendly origin label used on signage (e.g., "SensorA", "UserInput", "Euclid").
/// - Step: integer generation step; Step 1 marks original sources; derived steps use mkDerived policy.
/// - Timestamp: optional UTC time for when the provenance was recorded.
/// - Note: short human-readable message used on onboarding signage and audit overlays.
/// - Lineage: explicit list of parent Provenance entries forming the provenance trail.
type Provenance = 
    { /// Clear, human-readable name of the origin used in signage.
      SourceName: string; 
      /// Generation step number; 1 indicates original source.
      Step: int;
      /// UTC timestamp for record creation when available.
      Timestamp: DateTime option;
      /// Short human-visible comment describing the provenance event.
      Note: string 
      /// Explicit lineage trail of parent provenance records.
      Lineage: Provenance list} 

/// <summary>
/// Cell wrapper that always carries a payload and optional provenance.
/// </summary>
/// <typeparam name="T">Payload type.</typeparam>
/// <signage>
/// Use LiftedCell to attach provenance to payload values so downstream merges and signage can trace origin.
/// </signage>
type LiftedCell<'T> = { Value: 'T; Provenance: Provenance option }

/// <summary>
/// Binary-sum-like lifted union that always carries payload cells in each branch and supports nested lifting.
/// </summary>
/// <typeparam name="A">Payload type for branch A.</typeparam>
/// <typeparam name="B">Payload type for branch B.</typeparam>
/// <signage>
/// - A and B branches always hold LiftedCell payloads so provenance is preserved at each node.
/// - Nested allows representing arbitrarily nested lift operations; tooling should flatten for readable signage.
/// </signage>
type Lifted<'A,'B> =
    | A of LiftedCell<'A>
    | B of LiftedCell<'B>
    | Nested of LiftedCell<Lifted<'A,'B>>

module Provenance =
    
    /// <summary>
    /// Empty provenance sentinel used when no provenance is known.
    /// </summary>
    /// <signage>Empty provenance appears on signage as an anonymous source and Step 0.</signage>
    let empty : Provenance =
        { SourceName = ""
          Step = 0
          Timestamp = None
          Note = "" 
          Lineage = [] }

    /// <summary>
    /// Predicate that returns true when a provenance record is the empty sentinel.
    /// </summary>
    /// <returns>True when provenance equals the empty sentinel.</returns>
    /// <signage>Validators use isEmpty to decide whether to synthesize Step 1 provenance or keep absent provenance.</signage>
    let isEmpty (p: Provenance) =
        p.SourceName = "" && p.Note = "" && p.Timestamp.IsNone && p.Step = 0

    /// <summary>
    /// Produce a human readable single-line description of the provenance record.
    /// </summary>
    /// <returns>Short description string used in signage overlays.</returns>
    /// <signage>
    /// Examples:
    /// - "Step 2 from SensorA — Derived provenance (union/lifted)"
    /// - "Step 1 from UserInput on 2025-10-19 — original"
    /// </signage>
    let describe (p: Provenance) =
        match p.Timestamp with
        | None -> $"Step {p.Step} from {p.SourceName} — {p.Note}"
        | Some _ -> $"Step {p.Step} from {p.SourceName} on {p.Timestamp.Value} — {p.Note}"
    
    /// <summary>
    /// Create a derived provenance record from parents using a canonical step policy.
    /// </summary>
    /// <param name="sourceName">Name used for the derived provenance record.</param>
    /// <param name="operation">Short operation label stored in Note for signage (e.g., "union/lifted").</param>
    /// <param name="parents">Optional parent provenance records; None entries are ignored.</param>
    /// <returns>New derived Provenance record where Step = 1 + max(parent.Step) or 1 if no parents.</returns>
    /// <signage>
    /// Derived provenance policy:
    /// - Step is computed as 1 + max(parent.Step) when parents exist; otherwise Step = 1.
    /// - Timestamp set to DateTime.UtcNow.
    /// - Note set to "Derived provenance (operation)" to make the operation visible on signage.
    /// - Lineage contains the concrete parent records (parents filtered to Some).
    /// </signage>
    let mkDerived (sourceName: string) (operation : string) (parents: Provenance option list) : Provenance =
        let lineage = parents |> List.choose id |> List.map (fun p -> p)
        let parentSteps = parents |> List.choose id |> List.map (fun p -> p.Step)
        let maxParent = if List.isEmpty parentSteps then 0 else List.max parentSteps
        { SourceName = sourceName
          Step = maxParent + 1
          Timestamp = Some DateTime.UtcNow
          Note = $"Derived provenance ({operation})" 
          Lineage = lineage }

    /// <summary>
    /// Emit a multi-line string that includes this provenance and recursively lists lineage trail.
    /// </summary>
    /// <returns>Readable trail used on deep audit signage and debug overlays.</returns>
    /// <signage>
    /// Output format:
    /// - [Step N] Source: Name
    /// - → nested parent trail lines prefixed with arrow.
    /// </signage>
    let rec EmitSourceWithLineageTrail (p: Provenance) : string =
        let basis = $"[Step {p.Step}] Source: {p.SourceName}"
        match p.Step, p.Lineage with
        | 1, _ -> $"Extract Source Name With Lineage: {basis}"
        | _, [] -> $"Extract Source Name With Lineage: {basis} → No deeper lineage"
        | _, lineage ->
            let trail =
                lineage
                |> List.map EmitSourceWithLineageTrail
                |> String.concat "\n→ "
            $"Extract Source Name With Lineage: {basis}\n→ {trail}"

    /// <summary>
    /// Find and return the first provenance record in the lineage with Step = 1 (original source).
    /// </summary>
    /// <returns>Some provenance at Step 1 if found otherwise None.</returns>
    /// <signage>Used to extract canonical source identity when merging or emitting root signage.</signage>
    let rec GetStepOneProvenance (p: Provenance) : Provenance option =
        match p.Step = 1 with 
        | true -> Some p
        | false -> 
            p.Lineage
            |> List.choose GetStepOneProvenance
            |> List.tryHead

module Lifted =

    /// <summary>
    /// Map function f over A branch payloads, leaving B and nested branches unchanged except to recurse.
    /// </summary>
    /// <signage>Utility for transforming A payloads while preserving provenance cells.</signage>
    let rec mapA f lifted =
        match lifted with
        | A cell -> A { cell with Value = f cell.Value }
        | B cell -> B cell
        | Nested cell -> Nested { cell with Value = mapA f cell.Value; Provenance = cell.Provenance }

    /// <summary>
    /// Map function f over B branch payloads, leaving A and nested branches unchanged except to recurse.
    /// </summary>
    /// <signage>Utility for transforming B payloads while preserving provenance cells.</signage>
    let rec mapB f lifted =
        match lifted with
        | A cell -> A cell
        | B cell -> B { cell with Value = f cell.Value }
        | Nested cell -> Nested { cell with Value = mapB f cell.Value; Provenance = cell.Provenance }

    /// <summary>
    /// Map both A and B payloads with separate functions.
    /// </summary>
    /// <signage>Transforms both branches; used when converting both sides to a common representation.</signage>
    let rec mapAll fA fB lifted =
        match lifted with
        | A cell -> A { cell with Value = fA cell.Value }
        | B cell -> B { cell with Value = fB cell.Value }
        | Nested cell ->
            Nested {
                cell with
                    Value = mapAll fA fB cell.Value
            }

    /// <summary>
    /// Collect A payload LiftedCell elements as a list (only immediate A nodes are returned; nested A nodes must be extracted via recursion).
    /// </summary>
    /// <returns>List of LiftedCell A payloads found at this node.</returns>
    /// <signage>Simple extractor used by flattening operations and signage that enumerates branch A contributions.</signage>
    let rec collectA lifted =
        match lifted with
        | A cell -> [cell]
        | B _ -> []
        | Nested cell -> collectA cell.Value

    /// <summary>
    /// Collect B payload LiftedCell elements as a list (only immediate B nodes are returned; nested B nodes must be extracted via recursion).
    /// </summary>
    /// <returns>List of LiftedCell B payloads found at this node.</returns>
    /// <signage>Simple extractor used by flattening operations and signage that enumerates branch B contributions.</signage>
    let rec collectB lifted =
        match lifted with
        | A _ -> []
        | B cell -> [cell]
        | Nested cell -> collectB cell.Value

    /// <summary>
    /// Collect all Provenance records present on this lifted node and its immediate child cell.
    /// </summary>
    /// <returns>Sequence of provenance entries found.</returns>
    /// <signage>Used to synthesize combined provenance and to decide collapse safety.</signage>
    let rec collectProvenance lifted =
        let fromCell cell =
            match cell.Provenance with
            | Some p -> [p]
            | None -> []
        match lifted with
        | A cell -> fromCell cell
        | B cell -> fromCell cell
        | Nested cell -> fromCell cell @ collectProvenance cell.Value

    /// <summary>
    /// Try to find the first provenance encountered while traversing the lifted structure depth-first.
    /// </summary>
    /// <returns>Some Provenance if found otherwise None.</returns>
    /// <signage>Convenience for quick existence checks and reporting of the most local provenance.</signage>
    let rec tryFindProvenance lifted =
        let fromCell cell =
            match cell.Provenance with
            | Some p -> Some p
            | None -> None
        match lifted with
        | A cell -> fromCell cell
        | B cell -> fromCell cell
        | Nested cell ->
            match fromCell cell with
            | Some p -> Some p
            | None -> tryFindProvenance cell.Value

    /// <summary>
    /// Fold over the lifted tree with separate handlers for A and B nodes.
    /// </summary>
    /// <param name="fA">Folder for A nodes (acc -> LiftedCell<'A> -> acc).</param>
    /// <param name="fB">Folder for B nodes (acc -> LiftedCell<'B> -> acc).</param>
    /// <param name="acc">Initial accumulator.</param>
    /// <returns>Final accumulator after folding the structure.</returns>
    /// <signage>Generic fold used by analytics, signage aggregation, and transforms that must inspect provenance per-cell.</signage>
    let rec fold fA fB acc lifted =
        match lifted with
        | A cell -> fA acc cell
        | B cell -> fB acc cell
        | Nested cell -> fold fA fB acc cell.Value

    /// <summary>
    /// Count the total number of nodes (A, B, and nested) contained in the lifted structure.
    /// </summary>
    /// <returns>Integer node count.</returns>
    /// <signage>Used to annotate signage with size/complexity metrics for nested lifted unions.</signage>
    let rec count lifted =
        match lifted with
        | A _ | B _ -> 1
        | Nested cell -> 1 + count cell.Value

    /// <summary>
    /// Compute the depth of nesting of the lifted structure (leaf nodes have depth 1).
    /// </summary>
    /// <returns>Integer depth value.</returns>
    /// <signage>Depth is used by UI signage to decide whether to show a nested collapsed view or full expansion.</signage>
    let rec depth lifted =
        match lifted with
        | A _ | B _ -> 1
        | Nested cell -> 1 + depth cell.Value

    /// <summary>
    /// Replace all provenance entries on every cell in the lifted tree with a single provided provenance.
    /// </summary>
    /// <param name="newProv">Provenance to assign to every cell.</param>
    /// <returns>Lifted tree with provenance replaced.</returns>
    /// <signage>
    /// Use reassignProvenance when you need to normalize provenance after a canonicalizing operation
    /// or when stamping a derived step onto a whole lifted payload prior to collapse.
    /// </signage>
    let rec reassignProvenance newProv lifted =
        let tag cell = { cell with Provenance = Some newProv }
        match lifted with
        | A cell -> A (tag cell)
        | B cell -> B (tag cell)
        | Nested cell ->
            Nested {
                Provenance = Some newProv
                Value = reassignProvenance newProv cell.Value
            }
