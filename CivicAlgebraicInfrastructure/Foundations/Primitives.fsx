#r "System.Collections"
#load "Primitives.fs" 

open System
open CivicAlgebraicInfrastructure.Foundations.Primitives
open CivicAlgebraicInfrastructure.Foundations.Primitives.LiftedCore
open Microsoft.FSharp.Reflection

// ------------------------
// Primitive-only extractor (library-traversal expects extractor to only identify true primitives)
// ------------------------
let extractor (o: obj) =
  match o with
  | :? string as s -> Some { SourceName = $"source-of-{s}"; Step = 1 }
  | :? int as i -> Some { SourceName = $"source-of-{i}"; Step = 2 }
  | _ -> None


// ------------------------
// Helpers for printing and inspection
// ------------------------
let printProvenance (p: Provenance option) =
  match p with
  | Some p -> sprintf "%s@%d" p.SourceName p.Step
  | None -> "None"

let printLeaf (o: obj, p: Provenance option) =
  let valueStr =
    match o with
    | null -> "null"
    | :? string as s -> sprintf "\"%s\"" s
    | :? int as i -> sprintf "%d" i
    | _ -> o.ToString()
  sprintf "%s  | provenance=%s" valueStr (printProvenance p)

let printLeaves label (leaves: seq<obj * Provenance option>) =
  printfn "---- %s ----" label
  leaves |> Seq.iter (fun pair -> printfn "%s" (printLeaf pair))
  printfn "--------------------------"

let rec dumpLiftedObj depth (v: LiftedObj) =
  let indent = String.replicate depth "  "
  match v with
  | A (a, p) ->
      printfn "%sA - prov=%A payload=%A" indent p a
      match a with
      | :? LiftedObj as lo -> dumpLiftedObj (depth+1) (unbox lo)
      | _ -> printfn "%s  leaf=%A" indent a
  | B (b, p) ->
      printfn "%sB - prov=%A payload=%A" indent p b
      match b with
      | :? LiftedObj as lo -> dumpLiftedObj (depth+1) (unbox lo)
      | _ -> printfn "%s  leaf=%A" indent b

// ------------------------
// Example data (unchanged)
// ------------------------
let inner : Lifted<string,int> =
  A ("inner", Some { SourceName = "inner-src"; Step = 5 })

let mid : Lifted<Lifted<string,int>,int> =
  A (inner, Some { SourceName = "mid-src"; Step = 6 })

let top : Lifted<Lifted<Lifted<string,int>,int>,string> =
  A (mid, Some { SourceName = "top-src"; Step = 7 })

let mixedTyped : Lifted<Lifted<int,string>, string> =
  let innerA = A (42, Some { SourceName = "leaf-left"; Step = 11 })
  A (innerA, Some { SourceName = "outer-A"; Step = 9 })

let innerAsB : Lifted<string,int> = B (42, Some { SourceName = "leaf-right"; Step = 11 })

let mixedTyped2 : Lifted<Lifted<string,int>, string> =
  A (innerAsB, Some { SourceName = "outer-A"; Step = 9 })

let mixedObj : LiftedObj =
  let innerB : LiftedObj = B (box 42, Some { SourceName = "leaf-right"; Step = 11 })
  A (box innerB, Some { SourceName = "outer-A"; Step = 9 })

// ------------------------
// Tests that use library traversal (unwrapAllObj expects extractor to be primitive-only)
// ------------------------
let testUnwrapAll () =
  let topObj = boxLiftedTyped top
  printfn "Canonical boxLiftedTyped(top):"
  dumpLiftedObj 0 topObj
  let leaves = unwrapAllObj extractor topObj
  printLeaves "unwrapAll - top" leaves

let testMapAndUnwrap () =
  let mappedObj = mapLiftedRec (fun (s:string) -> box (s + "-X")) (fun (i:int) -> box (i + 1)) inner
  let leaves = unwrapAllObj extractor mappedObj
  printLeaves "mapped leaves - inner -> mappedObj" leaves

let testCounts () =
  let cl = countLeaves top
  let d  = depth top
  printfn "countLeaves(top) = %d" cl
  printfn "depth(top) = %d" d
  printfn "--------------------------"

let testFold () =
  let fA (acc: string list) (o: obj) =
    let s =
      match o with
      | :? string as str -> sprintf "A:\"%s\"" str
      | :? int as i -> sprintf "A:%d" i
      | _ -> sprintf "A:%O" o
    s :: acc
  let fB (acc: string list) (o: obj) =
    let s =
      match o with
      | :? string as str -> sprintf "B:\"%s\"" str
      | :? int as i -> sprintf "B:%d" i
      | _ -> sprintf "B:%O" o
    s :: acc
  let acc = foldLifted fA fB [] top
  printfn "fold results (reversed): %A" (List.rev acc)
  printfn "--------------------------"

let testMixedTyped () =
  printfn "---- mixedTyped (A of A) ----"
  printfn "boxLiftedTyped(mixedTyped):"
  dumpLiftedObj 0 (boxLiftedTyped mixedTyped)
  printLeaves "mixedTyped" (unwrapAllObj extractor (boxLiftedTyped mixedTyped))
  printfn ""
  printfn "---- mixedTyped2 (A of B) ----"
  printfn "boxLiftedTyped(mixedTyped2):"
  dumpLiftedObj 0 (boxLiftedTyped mixedTyped2)
  printLeaves "mixedTyped2" (unwrapAllObj extractor (boxLiftedTyped mixedTyped2))
  printfn "--------------------------"

let testMixedObj () =
  printfn "---- mixedObj (obj-view) ----"
  dumpLiftedObj 0 mixedObj
  printLeaves "mixedObj" (unwrapAllObj extractor mixedObj)

// ------------------------
// Runner
// ------------------------
let runAll () =
  printfn "Running CivicCompare FSX tests..."
  printfn ""
  testUnwrapAll()
  printfn ""
  testMapAndUnwrap()
  printfn ""
  testCounts()
  printfn ""
  testFold()
  printfn ""
  testMixedTyped()
  printfn ""
  testMixedObj()
  printfn "\nDone."

// Auto-run when script executed
runAll()
