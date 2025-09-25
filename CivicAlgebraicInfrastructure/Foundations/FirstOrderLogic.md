# First Order Logic (FOL) Ordinance Book

**Subdivision:** CivicAlgebraicInfrastructure.Foundations.FOL  
**Companion files:**  
- [FirstOrderLogic.fs](FirstOrderLogic.fs) — ordinance code  
- [FirstOrderLogic.fsx](FirstOrderLogic.fsx) — interactive case law  

---

## Preamble
This ordinance establishes the foundations of **First Order Logic (FOL)** within the civic algebraic infrastructure. It defines the lawful symbols, terms, and formulas that citizens (remixers) may use to construct and reason about propositions.

---

## Purpose
- Provide a **formal grammar** for terms and formulas.  
- Establish **connectives** (∧, ∨, →, ¬) as civic utilities.  
- Define **quantifiers** (∀, ∃) as lawful operators over variables.  
- Serve as the **bedrock** for higher‑order extensions (HOL, Modal, etc.).

---

## Ordinance Sections

### 1. Domain Model
- **Terms**: variables, constants, and function applications.  
- **Atomic Formulas**: predicates over terms, or equalities between terms.  
- **Formulas**: built recursively from atomic formulas using connectives and quantifiers. connectives.  

### 2. Connectives
- **Primitive**: Negation (¬), Conjunction (∧), Disjunction (∨), Implication (→).  
- **Derived**: Biconditional (↔), Exclusive Or (⊕), NAND (↑), NOR (↓), XNOR (⊙),  
  Nonimplication (⇏), Converse (⇐), Converse Nonimplication (⇍), Nonequivalence (≢).  

### 3. Quantifiers
- **Universal**: ∀x. φ  
- **Existential**: ∃x. φ  
- **Quantified Formulas**: pair a bound variable with a formula body.  

---

## Case Law (see `.fsx`)
- Example: Constructing a formula `∀x. P(x) → Q(x)`  
- Example: Evaluating truth under a given interpretation.  
- Example: Expressing equivalence `P(x) ↔ Q(x)` as `(P(x) → Q(x)) ∧ (Q(x) → P(x))`.  
- Example: Expressing exclusive or `P(x) ⊕ Q(x)` as `(P(x) ∨ Q(x)) ∧ ¬(P(x) ∧ Q(x))`.  

---

## Commentary
This ordinance is designed to be **remixable**:  
- Citizens may extend the grammar with new connectives.  
- Quantifier style may be adapted via render profiles.  
- Substitution hygiene is enforced to preserve lawful reasoning.

---

## Crosslinks
- [Civic Algebraic Infractructure](../README.md)
