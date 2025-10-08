#r "System.Collections"
#load "Primitives.fs"

open System
open CivicAlgebraicInfrastructure.Foundations.Primitives
open CivicAlgebraicInfrastructure.Foundations.Primitives.Lifted

// === Civic Demonstration ===

// === Sample Provenance ===
let p1 = { SourceName = "SensorA"; Step = 1 }
let p2 = { SourceName = "SensorB"; Step = 2 }
let p3 = { SourceName = "SensorC"; Step = 3 }
let pA = { SourceName = "AlphaSensor"; Step = 11 }
let pB = { SourceName = "BetaSensor"; Step = 12 }
let pN1 = { SourceName = "Nest1"; Step = 100 }
let pN2 = { SourceName = "Nest2"; Step = 200 }

let a1 = { Value = "Alpha"; Provenance = Some p1 }
let b1 = { Value = 42; Provenance = Some p2 }
let a2 = { Value = "Beta"; Provenance = None }
let b2 = { Value = 99; Provenance = Some p3 }

let nested =
    Nested {
        Provenance = Some p3
        Value =
            Nested {
                Provenance = Some pA
                Value =
                    A a1
            }
    }

let mixed =
    Nested {
        Provenance = Some p2
        Value =
            Nested {
                Provenance = Some pB
                Value =
                    B b2
            }
    }

let deep =
    Nested {
        Provenance = Some pN1
        Value =
            Nested {
                Provenance = Some pN2
                Value =
                    Nested {
                        Provenance = Some p1
                        Value = A a2
                    }
            }
    }

let printLifted lifted =
    let rec loop indent lifted =
        let pad = String.replicate indent "  "
        match lifted with
        | A cell -> printfn "%sA: %A" pad cell
        | B cell -> printfn "%sB: %A" pad cell
        | Nested cell ->
            printfn "%sNested Provenance: %A" pad cell.Provenance
            loop (indent + 1) cell.Value
    loop 0 lifted

printfn "\n=== Displaying Nested Structures ==="
printLifted nested
printLifted mixed
printLifted deep

printfn "\n=== Provenance Collection ==="
collectProvenance nested |> List.iter (fun p -> printfn "Provenance: %A" p)

printfn "\n=== Payload Extraction ==="
collectA deep |> List.iter (fun c -> printfn "A Value: %s" c.Value)
collectB mixed |> List.iter (fun c -> printfn "B Value: %d" c.Value)

printfn "\n=== Depth and Count ==="
printfn "Depth (deep): %d" (depth deep)
printfn "Count (mixed): %d" (count mixed)

printfn "\n=== Reassigning Provenance ==="
let reassigned : Lifted<string, int> = reassignProvenance { SourceName = "Override"; Step = 99 } nested
printLifted reassigned
