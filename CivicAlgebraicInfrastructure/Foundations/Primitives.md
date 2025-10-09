# Primitives Ordinance Book

**Subdivision:** CivicAlgebraicInfrastructure.Foundations.Primitives  
**Companion files:**  
- [Primitives.fs](Primitives.fs) — ordinance code  
- [Primitives.fsx](Primitives.fsx) — interactive case law  

## Preamble
This namespace collects domain-model **primitives** that serve as the foundational building blocks for CivicAlgebraicInfrastructure. The first primitive provided is the **Lifted** type, a minimal, civic-grade union type that encodes payloads with explicit provenance and composable nesting. The module establishes clear, testable contracts for each primitive so remixers, migrators, and auditors can rely on structural, behavioral, and provenance guarantees.

## Purpose
- **Primitive intent** The primitives are small, composable types and helpers that express core domain invariants as legal, narratable artifacts engineers can reason about across migrations and transforms.

- **First primitive** Lifted encodes two tagged payload kinds (A and B) and a Nested wrapper that carries another Lifted instance, with every node stored inside a LiftedCell that optionally records provenance.

- **Design posture** Primitives favor explicitness, total pure functions, and capture-avoiding transformations; they are deliberately small so they remain auditable and easily composed into larger civic ordinances.

- **Evolution model** Additional primitives will be added as the domain model grows; each new primitive must include a short contract, example usage, unit and property tests, and a migration note when semantics change.

- **Namespace responsibility** This namespace documents invariants, exposes only pure helpers that preserve provenance unless documented otherwise, and provides clear onboarding examples so future contributors treat these types as enduring civic infrastructure.

## Scope
Applies to the module `CivicAlgebraicInfrastructure.Foundations.Primitives.Lifted` and any consumer code that constructs, inspects, folds, or mutates `Lifted<'A,'B>` values.

## Invariants
- **Payload Guarantee** Each `Lifted` node must carry exactly one payload value inside its `LiftedCell<'T>`; there must be no empty payload variant.

- **Provenance Optionality** Provenance is optional on every cell; absence is allowed but explicit; provenance values, when present, are authoritative and immutable except via explicit reassign operations.

- **Nesting Semantics** `Nested` always wraps a `LiftedCell` whose `Value` is a `Lifted<'A,'B>` instance; there are no other hidden wrappers.

 - **Structural Identity** Node identity is structural; equality and hashing should consider payload and provenance as part of the cell.

- **No Side Effects** All module functions must be pure and total for finite trees; no mutation, I/O, or hidden state.

## Naming Conventions
- **Types** Keep Provenance, `LiftedCell<'T>`, and `Lifted<'A,'B>` as declared.

- **Module** Keep module name `Lifted`.

- **Functions** Use clear verbs: `mapA`, `mapB`, `mapAll`, `collectA`, `collectB`, `collectProvenance`, `tryFindProvenance`, `fold`, `count`, `depth`, `reassignProvenance`.

- **Internal helpers** must be private and prefixed with `private` and a verb describing action.

## Function Contracts
*-* **mapA : ('A -> 'A2) -> Lifted<'A,'B> -> Lifted<'A2,'B>**

  * Applies f only to every A payload; preserves cell provenance and B payloads unchanged.

*.* **mapB : ('B -> 'B2) -> Lifted<'A,'B> -> Lifted<'A,'B2>**

  * Applies f only to every B payload; preserves cell provenance and A payloads unchanged.

*-* **mapAll : ('A -> 'A2) -> ('B -> 'B2) -> Lifted<'A,'B> -> Lifted<'A2,'B2>**

  * Applies respective functions to all payloads recursively; preserves provenance on each cell.

*-* **collectA : Lifted<'A,'B> -> LiftedCell<'A> list**

  * Returns every A cell in document order.

*-* **collectB : Lifted<'A,'B> -> LiftedCell<'B> list**

  * Returns every B cell in document order.

*-* **collectProvenance : Lifted<'A,'B> -> Provenance list**

  * Returns every provenance in traversal order starting at the root cell, skipping missing provenance.

*-* **tryFindProvenance : Lifted<'A,'B> -> Provenance option**

  * Returns the first found provenance in root-to-leaf order.

*-* **fold : (acc -> LiftedCell<'A> -> acc) -> (acc -> LiftedCell<'B> -> acc) -> acc -> Lifted<'A,'B> -> acc**

  * Fold must visit the first encountered cell for A and B and recurse through Nested values.

*-* **count : Lifted<'A,'B> -> int**

  * Returns total number of nodes where each A, B, and Nested node counts as 1.

*-* **depth : Lifted<'A,'B> -> int**

  * Returns maximal nesting depth where leaf A or B nodes have depth 1.

*-* **reassignProvenance : Provenance -> Lifted<'A,'B> -> Lifted<'A,'B>**

  * Replaces provenance on every cell with Some newProv.

All functions must be total for finite trees and not raise exceptions for normal inputs.

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
| Public API (module Lifted):                                                |
|  mapA, mapB, mapAll, collectA, collectB,                                   |
|  collectProvenance, tryFindProvenance, fold, count, depth,                 |
|  reassignProvenance                                                        |
+----------------------------------------------------------------------------+
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

## Case Law (see `.fsx`)
This script is a runnable demonstration that exercises the Lifted primitives and their provenance semantics. It builds sample provenance records and payload cells, composes nested Lifted values, and exercises traversal, extraction, metrics, printing, and provenance reassignment.

- **Purpose** illustrate real-world usage patterns for Lifted, provide readable examples for reviewers and migrators, and produce console output that documents expected behavior.

- **Contents**: sample provenance definitions; LiftedCell payloads; three nested example trees named nested, mixed, and deep; a recursive printer; calls to collectProvenance, collectA, collectB, depth, count, and reassignProvenance.

- **Demonstrations**: shows provenance aggregation order, payload extraction for both A and B branches, depth and node counting, and a global provenance override that replaces provenance on every node.

- **Observability**: console prints are structured with indentation to show nesting and with labelled sections to separate demonstrations.

- **Usage**: load the script after compiling or loading Primitives.fs, then run it in F# Interactive to observe printed examples and validate invariants.

## Crosslinks
- [🧮 Civic Algebraic Infrastructure](../README.md)