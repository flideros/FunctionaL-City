# Primitives Ordinance Book

**Subdivision:** CivicAlgebraicInfrastructure.Foundations.Primitives  
**Companion files:**  
- [Primitives.fs](Primitives.fs) — ordinance code  
- [Primitives.fsx](Primitives.fsx) — interactive case law  

## Preamble
This namespace collects domain-model **primitives** that serve as the foundational building blocks for CivicAlgebraicInfrastructure. The first primitive provided is the **LiftedCell / Lifted** type, a minimal, civic-grade union type that encodes payloads with explicit provenance and composable nesting. The module establishes clear, testable contracts for each primitive so remixers, migrators, and auditors can rely on structural, behavioral, and provenance guarantees.

## Purpose
- **Primitive intent** The primitives are small, composable types and helpers that express core domain invariants as legal, narratable artifacts engineers can reason about across migrations and transforms.

- **First primitive** Lifted encodes two tagged payload kinds (A and B) and a Nested wrapper that carries another Lifted instance, with every node stored inside a LiftedCell that optionally records provenance.

- **Design posture** Primitives favor explicitness, total pure functions, and capture-avoiding transformations; they are deliberately small so they remain auditable and easily composed into larger civic ordinances.

- **Evolution model** Additional primitives will be added as the domain model grows; each new primitive must include a short contract, example usage, unit and property tests, and a migration note when semantics change.

- **Namespace responsibility** This namespace documents invariants, exposes only pure helpers that preserve provenance unless documented otherwise, and provides clear onboarding examples so future contributors treat these types as enduring civic infrastructure.

## Primitives

This file defines the lightweight algebraic primitives used across the Civic
Algebraic Infrastructure. It documents the LiftedCell shape, provenance model,
and traversal / mapping semantics expected from implementations.

### Provenance Contract

Provenance records authoritative lineage metadata for LiftedCell payloads and
establishes the following guarantees.

- **Immutability**
  Provenance values attached to LiftedCell instances are authoritative and may
  only be changed by the explicit `reassignProvenance` operation. Helper
  functions that transform payloads must not mutate provenance in-place.

- **Step semantics**
  `Step` is a strictly increasing nonnegative integer representing derivation
  depth. `mkDerived` sets `Step = 1 + max(parent.Step)` where missing parent
  Step values are treated as `0`. Original source claims have `Step = 1`.

- **Timestamp semantics**
  Timestamps are stored in UTC. Implementations must use `DateTime.UtcNow` when
  creating derived provenance to ensure consistent ordering across systems.

- **Lineage shape**
  `Lineage` is a finite, ordered, acyclic list of Provenance entries
  representing direct parent provenance in descending ancestry order.
  Implementations must prevent cycles and keep lineage lists finite.

- **Equality and hashing**
  Provenance equality and hashing are structural over `SourceName`, `Step`,
  `Timestamp`, `Note`, and `Lineage`. Use structural equality when comparing
  Provenance for identity checks.

### Traversal and Fold Semantics

These traversal rules are ordinance-level guarantees so consumer code can
depend on deterministic visitation order and accumulator behavior.

- **Traversal order**
  All `collect*` functions and `collectProvenance` traverse the `LiftedCell`
  structure in root-first, left-to-right recursion order and return sequences
  in the exact order visited.

- **tryFind semantics**
  `tryFindProvenance` and `tryFind` return the first matching value found using
  root-to-leaf, left-to-right search and short-circuit immediately when a match
  is discovered.

- **Fold contract**
  `fold fA fB acc lifted` applies as follows:
  1. Visit the current cell first.
  2. If the cell is `A` call `fA acc aCell` to produce a new accumulator.
  3. If the cell is `B` call `fB acc bCell` to produce a new accumulator.
  4. If the cell is `Nested` recurse into the nested `Value` with the
     accumulator returned from visiting the wrapper cell.
  Both `fA` and `fB` accept the current accumulator and the cell and must
  return the new accumulator.

- **Depth and count**
  `count` and `depth` are pure folds. A leaf node has depth `1`. `depth` of
  `Nested` is `1 + depth` of the inner value.

### Map and Reassign Guarantees

- **Provenance preservation**
  Map functions `mapA`, `mapB`, and `mapAll` must preserve the optional
  `Provenance` on each `LiftedCell` unless the function explicitly returns a
  new `Provenance`. Payload transforms must not change provenance by default.

- **Nested behavior**
  When mapping over `Nested`, the wrapper cell's `Provenance` is preserved and
  mapping recurses into `Value`. Implementations must not drop wrapper
  provenance.

- **Reassign semantics**
  `reassignProvenance` replaces `Provenance` on every cell with `Some newProv`.
  It sets the wrapper `Nested` cell provenance and then recurses into the
  nested `Value`. `reassignProvenance` is the only authorized global-provenance
  replacement operation; callers must create appropriate derived provenance via
  `mkDerived` before calling `reassignProvenance`.

### Robustness, Performance, and Tests

- **Required unit tests**
  Implementations must include unit tests for every public function:
  `mapA`, `mapB`, `mapAll`, `collectA`, `collectB`, `collectProvenance`,
  `tryFindProvenance`, `fold`, `count`, `depth`, `reassignProvenance`. Tests
  must include empty provenance, missing timestamps, deep nesting, and mixed
  A/B trees.

- **Property tests**
  Property tests must cover provenance preservation under `map*`, identity of
  `count` under `reassign`, depth invariants, and traversal determinism
  (`collect*` yields the same sequence as a fold-accumulated visitation).

- **Stack-safety**
  Implementations must document whether traversals are tail-recursive or
  stack-safe. If not stack-safe include a note that extremely deep trees may
  `StackOverflow` and provide a migration strategy or offer a streaming /
  trampolined traversal variant.

- **Migration notes**
  When changing `Step` semantics, `Lineage` shape, or timestamp rules, authors
  must provide a migration plan and tooling to map existing Provenance into the
  new shape. Migration plans must be included with breaking changes to the
  ordinance.

### API Signatures and Examples

Include the following canonical contract snippets in the ordinance so reviewers
can treat the document as an executable contract for implementers.

```fsharp
type Provenance = {
  SourceName: string
  Step: int
  Timestamp: System.DateTime
  Note: string option
  Lineage: Provenance list
}

type LiftedCell<'a,'b> =
  | A of ARecord<'a>
  | B of BRecord<'b>
  | Nested of WrapperRecord<'a,'b>

val mapA : (ARecord<'a> -> ARecord<'a2>) -> LiftedCell<'a,'b> -> LiftedCell<'a2,'b>
val mapB : (BRecord<'b> -> BRecord<'b2>) -> LiftedCell<'a,'b> -> LiftedCell<'a,'b2>
val mapAll : (obj -> obj) -> LiftedCell<'a,'b> -> LiftedCell<'a,'b>
val collectProvenance : LiftedCell<'a,'b> -> Provenance seq
val tryFindProvenance : (Provenance -> bool) -> LiftedCell<'a,'b> -> Provenance option
val reassignProvenance : Provenance -> LiftedCell<'a,'b> -> LiftedCell<'a,'b>
val mkDerived : sourceName:string -> parents:Provenance list -> note:string option -> Provenance
```

## Zoning Map
```
+----------------------------------------------------------------------------+
| Namespace: CivicAlgebraicInfrastructure.Foundations.Primitives             |
|                                                                            |
|  +----------------+    +----------------+    +---------------------------+ |
|  | Provenance     |    | LiftedCell<'T> |    | Lifted<'A,'B>             | |
|  | { SourceName;  |<-->| { Value: 'T;   |    | A of LiftedCell<'A>       | |
|  |   Step }       |    |   Provenance } |    | B of LiftedCell<'B>       | |
|  +----------------+    +----------------+    | Nested of                 | |
|                                              | LiftedCell<Lifted<'A,'B>> | |
|                                              +---------------------------+ |
|                                                                            |
| **Public API (module Lifted):**                                            |
|  mapA, mapB, mapAll, collectA, collectB,                                   |
|  collectProvenance, tryFindProvenance, fold, count, depth,                 |
|  reassignProvenance                                                        |
+----------------------------------------------------------------------------+
```
### Provenance: Step formula and Timestamp rule

Implementers must compute `Step` exactly as:

```text
Step = 1 + (parents |> Seq.map (fun p -> p.Step) |> Seq.tryMax |> Option.defaultValue 0)
```
Timestamps stored in Provenance must use UTC with an explicit DateTimeKind. Example construction:

```fsharp
let ts = System.DateTime.UtcNow.ToUniversalTime()
let p = mkDerived "source" parents (Some "note") // mkDerived must set Timestamp = ts and Step using the rule above
```


## Flow Diagram
```
[Construct Provenance]    [Construct LiftedCell]      [Construct Lifted Tree]
       p1,p2,p3                 cellA,cellB                  A cellA
         |                          |                         |
         +-----------+--------------+-------------------------+
                     |                                       
            (nesting / composition)
                     |
              Nested { Provenance = pN; Value = <Lifted> }
                     |
             +-------v-------+
             |   Lifted Tree |
             +---------------+
                     |
     +---------------+-----------------+----------------------+
     |               |                 |                      |
 [printLifted]   [collectProvenance] [collectA/collectB]   [depth/count]
     |               |                 |                      |
  human view     [list of Provenance] [list of LiftedCell] [metrics ints]
                     |                 |                      |
                     +-----------------+----------------------+
                                       |
                                   [fold/mapAll]
                                       |
                             transformed Lifted tree
                                       |
                                [reassignProvenance]
                                       |
                             Lifted tree with new Provenance
```
### Fold signature and example

Canonical signature to remove ambiguity:

```fsharp
val fold :
  ('acc -> ARecord<'a> -> 'acc) ->
  ('acc -> BRecord<'b> -> 'acc) ->
  'acc ->
  Lifted<'a,'b> ->
  'acc
```
One-line example showing accumulator ordering (counts A nodes by visiting root-first):

```fsharp
// counts A nodes in root-first, left-to-right order
let countA = fold (fun s _ -> s + 1) (fun s _ -> s) 0 nested
```
## Case Law (see `.fsx`)
This script is a runnable demonstration that exercises the Lifted primitives and their provenance semantics. It builds sample provenance records and payload cells, composes nested Lifted values, and exercises traversal, extraction, metrics, printing, and provenance reassignment.

- **Purpose** illustrate real-world usage patterns for Lifted, provide readable examples for reviewers and migrators, and produce console output that documents expected behavior.

- **Contents**: sample provenance definitions; LiftedCell payloads; three nested example trees named nested, mixed, and deep; a recursive printer; calls to collectProvenance, collectA, collectB, depth, count, and reassignProvenance.

- **Demonstrations**: shows provenance aggregation order, payload extraction for both A and B branches, depth and node counting, and a global provenance override that replaces provenance on every node.

- **Observability**: console prints are structured with indentation to show nesting and with labelled sections to separate demonstrations.

- **Usage**: load the script after compiling or loading Primitives.fs, then run it in F# Interactive to observe printed examples and validate invariants.

## Crosslinks
- [🧮 Civic Algebraic Infrastructure](../README.md)