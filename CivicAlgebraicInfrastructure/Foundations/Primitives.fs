namespace CivicAlgebraicInfrastructure.Foundations.Primitives

open System

/// Provenance record
type Provenance = 
    { SourceName: string; // Clear, human-readable name of the origin (e.g., "SensorA", "UserInput", "Socrotes").
      Step: int;
      Timestamp: DateTime option;
      Note: string 
      Lineage: Provenance list} 

/// Cell that always carries a payload and optional provenance
type LiftedCell<'T> = { Value: 'T; Provenance: Provenance option }

/// Lifted union with guaranteed payloads 
type Lifted<'A,'B> =
    | A of LiftedCell<'A>
    | B of LiftedCell<'B>
    | Nested of LiftedCell<Lifted<'A,'B>>

module Provenance =
    let empty : Provenance =
        { SourceName = ""
          Step = 0
          Timestamp = None
          Note = "" 
          Lineage = [] }

    let isEmpty (p: Provenance) =
        p.SourceName = "" && p.Note = "" && p.Timestamp.IsNone && p.Step = 0

    let describe (p: Provenance) =
        match p.Timestamp with
        | None -> $"Step {p.Step} from {p.SourceName} — {p.Note}"
        | Some _ -> $"Step {p.Step} from {p.SourceName} on {p.Timestamp.Value} — {p.Note}"
    
    /// Derived provenance: Step = 1 + max(parent.Step) or 1 if none.
    let mkDerived (sourceName: string) (operation : string) (parents: Provenance option list) : Provenance =
        let lineage = parents |> List.choose id |> List.map (fun p -> p)
        let parentSteps = parents |> List.choose id |> List.map (fun p -> p.Step)
        let maxParent = if List.isEmpty parentSteps then 0 else List.max parentSteps
        { SourceName = sourceName
          Step = maxParent + 1
          Timestamp = Some DateTime.UtcNow
          Note = $"Derived provenance ({operation})" 
          Lineage = lineage }

module Lifted =

    /// Map over A payloads
    let rec mapA f lifted =
        match lifted with
        | A cell -> A { cell with Value = f cell.Value }
        | B cell -> B cell
        | Nested cell -> Nested { cell with Value = mapA f cell.Value; Provenance = cell.Provenance }

    /// Map over B payloads
    let rec mapB f lifted =
        match lifted with
        | A cell -> A cell
        | B cell -> B { cell with Value = f cell.Value }
        | Nested cell -> Nested { cell with Value = mapB f cell.Value; Provenance = cell.Provenance }

    /// Map over all payloads
    let rec mapAll fA fB lifted =
        match lifted with
        | A cell -> A { cell with Value = fA cell.Value }
        | B cell -> B { cell with Value = fB cell.Value }
        | Nested cell ->
            Nested {
                cell with
                    Value = mapAll fA fB cell.Value
            }

    /// Extract all A payloads
    let rec collectA lifted =
        match lifted with
        | A cell -> [cell]
        | B _ -> []
        | Nested cell -> collectA cell.Value

    /// Extract all B payloads
    let rec collectB lifted =
        match lifted with
        | A _ -> []
        | B cell -> [cell]
        | Nested cell -> collectB cell.Value

    /// Extract all provenance records
    let rec collectProvenance lifted =
        let fromCell cell =
            match cell.Provenance with
            | Some p -> [p]
            | None -> []
        match lifted with
        | A cell -> fromCell cell
        | B cell -> fromCell cell
        | Nested cell -> fromCell cell @ collectProvenance cell.Value

    /// Try to find the first provenance record
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

    /// Fold over the tree with separate handlers for A and B
    let rec fold fA fB acc lifted =
        match lifted with
        | A cell -> fA acc cell
        | B cell -> fB acc cell
        | Nested cell -> fold fA fB acc cell.Value

    /// Count total nodes
    let rec count lifted =
        match lifted with
        | A _ | B _ -> 1
        | Nested cell -> 1 + count cell.Value

    /// Depth of nesting
    let rec depth lifted =
        match lifted with
        | A _ | B _ -> 1
        | Nested cell -> 1 + depth cell.Value

    /// Replace all provenance with a given one
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
