# First Order Logic (FOL) Ordinance Book

**Subdivision:** CivicAlgebraicInfrastructure.Foundations.FOL  
**Companion files:**  
- [FirstOrderLogic.fs](FirstOrderLogic.fs) — ordinance code  
- [FirstOrderLogic.fsx](FirstOrderLogic.fsx) — interactive case law  

---

## Preamble
This ordinance establishes the foundations of **First Order Logic (FOL)** within the civic algebraic infrastructure. It defines the lawful **symbols, terms, and formulas** that citizens (remixers) may use to construct and reason about propositions. Symbols themselves are typed citizens (`Symbol` records with `Name`, `Kind`, and optional `Arity`), ensuring lawful zoning between variables, constants, functions, and predicates.

---

## Purpose
- Provide a **formal grammar** for terms and formulas.  
- Establish **connectives** (∧, ∨, →, ¬) as civic utilities.  
- Define **quantifiers** (∀, ∃) as lawful operators over variables.  
- Maintain a **metadata registry** for decidability and enumerability. 
- Serve as the **bedrock** for higher‑order extensions (HOL, Modal, etc.).

---

## Ordinance Sections

### 1. Domain Model
- **Symbols**: named citizens classified as variables, constants, functions, or predicates, with optional arity.
- **Terms**: variables, constants, and function applications.  
- **Atomic Formulas**: predicates over terms, or equalities between terms.  
- **Formulas**: built recursively from atomic formulas using connectives and quantifiers.  

### 2. Connectives
- **Primitive**: Negation (¬), Conjunction (∧), Disjunction (∨), Implication (→).  
- **Derived**: Biconditional (↔), Exclusive Or (⊕), NAND (↑), NOR (↓), XNOR (⊙),  
  Nonimplication (⇏), Converse (⇐), Converse Nonimplication (⇍), Nonequivalence (≢).  
- **ConnectiveKind**: a symbolic registry for service APIs (evaluation, rendering, truth tables).  
- Primitive connectives are constructors in the formula grammar; `ConnectiveKind` is the zoning map for assigning lawful services.

### 3. Quantifiers
- **Universal**: ∀x. φ  
- **Existential**: ∃x. φ  
- **Quantified Formulas**: pair a bound variable with a formula body.  
- **Lawfulness**: quantifiers may only bind symbols of kind `VariableKind`.

### 4. Metadata
- **Purpose**: Metadata captures properties *about* formulas and logical systems, not the formulas themselves.  
- **Decidability**:  
  - `Decidable` — there exists a terminating procedure to decide truth.  
  - `SemiDecidable` — some truths can be recognized, but not all falsities.  
  - `Undecidable` — no terminating procedure exists.  
- **Enumerability**:  
  - `Enumerable` — the set of truths can be effectively listed.  
  - `NonEnumerable` — no such listing is possible.  
- **FOLMetadata**:  
  ```fsharp
  type FOLMetadata =
      { Decidability  : Decidability option
        Enumerability : Enumerability option }
   ```
- **Metadata is optional signage**: it does not alter the formula’s structure, but provides civic annotations remixers can use for reasoning about the system itself.

---
## Zoning Map
```
┌─────────────────────┐
│   Symbol            │
│  (Name, Kind, Arity)│
└─────────┬───────────┘
          │
          ▼
┌─────────────────────┐
│   Term<'Symbol>     │
│  (independent)      │
└─────────┬───────────┘
          │
          ▼
┌─────────────────────┐
│ AtomicFormula<'S>   │
│ uses Term<'S>       │
└─────────┬───────────┘
          │
          ▼
┌───────────────────────────────────────────────┐
│                 Formula<'S>                   │
│  ┌────────────────┬─────────────────────────┐ │
│  │ Atomic         │ Connective              │ │
│  │ (AtomicFormula)│ (PrimitiveConnective)   │ │
│  │                │ Quantified              │ │
│  └────────────────┴─────────────────────────┘ │
└─────────┬───────────────────────┬─────────────┘
          │                       │
          ▼                       ▼
┌─────────────────────┐     ┌────────────────────┐
│ PrimitiveConnective │     │   Quantified<'S>   │
│ uses Formula<'S>    │     │ Bound: Quantifier  │
│ (Not, And, Or, →)   │     │ Body: Formula<'S>  │
└─────────────────────┘     └─────────┬──────────┘
                                      │
                                      ▼
                             ┌────────────────────┐
                             │ Quantifier<'S>     │
                             │ ∀x | ∃x            │
                             └────────────────────┘

┌─────────────────────┐
│ ConnectiveKind      │
│ (symbolic enum)     │
│ independent         │
└─────────────────────┘

┌─────────────────────┐
│ FOLMetadata         │
│ (Decidability,      │
│  Enumerability)     │
└─────────────────────┘
```
---

## Flow Diagram
```
Var { Name="x"; Kind=VariableKind; Arity=None }
   │
   ▼
Predicate("P", [x])   Predicate("Q", [x])
   │                         │
   └─────── Atomic ──────────┘
             │
             ▼
      Implies (P(x), Q(x))
             │
             ▼
        Connective
             │
             ▼
Quantifier: ForAll { Name="x"; Kind=VariableKind }
             │
             ▼
Quantified { Bound = ∀x; Body = (P(x) → Q(x)) }
             │
             ▼
        Formula.Quantified
```

---

## Case Law (see `.fsx`)
- Example: Constructing a formula `∀x. P(x) → Q(x)`  
- Example: Evaluating truth under a given interpretation.  
- Example: Expressing equivalence `P(x) ↔ Q(x)` as `(P(x) → Q(x)) ∧ (Q(x) → P(x))`.  
- Example: Expressing exclusive or `P(x) ⊕ Q(x)` as `(P(x) ∨ Q(x)) ∧ ¬(P(x) ∧ Q(x))`.  
- Example: Annotating Presburger arithmetic with `FOLMetadata` = { `Decidability` = `Some Decidable`; `Enumerability` = `Some Enumerable` }.

---

## Commentary
This ordinance is designed to be **remixable**:  
- Citizens may extend the grammar with new connectives.  
- Quantifier style may be adapted via render profiles.  
- Substitution hygiene is enforced to preserve lawful reasoning.
- `ConnectiveKind` may be paired with service overlays (evaluation, rendering, LaTeX) so remixers can assign lawful functions to each connective.
- `FOLMetadata` provides a registry for meta‑properties, enabling remixers to reason not only within the system but also about the system.

---

## Crosslinks
- [🧮 Civic Algebraic Infrastructure](../README.md)
