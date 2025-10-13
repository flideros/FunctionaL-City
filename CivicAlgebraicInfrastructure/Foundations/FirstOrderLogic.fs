namespace CivicAlgebraicInfrastructure.Foundations.FOL

open System

// Terms
type Term<'Symbol> =
    | Var of 'Symbol
    | Constant of 'Symbol
    | Func of 'Symbol * Term<'Symbol> list

// Quantifiers
type Quantifier<'Symbol> =
    | ForAll of 'Symbol    // Universal Quantifier: ∀x. φ
    | Exists of 'Symbol    // Existential Quantifier: ∃x. φ

// Mutually recursive block
type AtomicFormula<'Symbol> =
    | Predicate of 'Symbol * Term<'Symbol> list   // P(t₁, ..., tₙ)
    | Equality of Term<'Symbol> * Term<'Symbol>   // t₁ = t₂

and Formula<'Symbol> =
    | Atomic of AtomicFormula<'Symbol>
    | Connective of PrimitiveConnective<'Symbol>
    | Quantified of Quantified<'Symbol>

and PrimitiveConnective<'Symbol> =
    | Not_ of Formula<'Symbol>                         // Negation: ¬φ
    | And_ of Formula<'Symbol> * Formula<'Symbol>      // Conjunction: φ ∧ ψ
    | Or_ of Formula<'Symbol> * Formula<'Symbol>       // Disjunction: φ ∨ ψ
    | Implies_ of Formula<'Symbol> * Formula<'Symbol>  // Implication: φ → ψ

and Quantified<'Symbol> =
    { Bound : Quantifier<'Symbol>
      Body  : Formula<'Symbol> }

type ConnectiveKind = //Added 'Kind' to avoid naming collisions down stream.
    | Not
    | Implies
    | Iff
    | Xor
    | Xnor
    | Nor
    | Nand
    | Converse
    | ConverseNonImplication
    | NonImplication
    | Nonequivalence

type Decidability =
    | Decidable
    | SemiDecidable
    | Undecidable

type Enumerability =
    | Enumerable
    | NonEnumerable

type FOLMetadata =
    { Decidability : Decidability option
      Enumerability: Enumerability option }

type SymbolKind =
    | VariableKind
    | ConstantKind
    | FunctionKind
    | PredicateKind

type Symbol =
    { Name  : string
      Kind  : SymbolKind
      Arity : int option }   // only meaningful for functions/predicates

module Formulae =
    /// Constructs a membership predicate: x ∈ A
    /// Represents set membership using a binary predicate symbol.
    let memberOf (term: Term<Symbol>) (set: Term<Symbol>) : Formula<Symbol> =
        Atomic (Predicate({ Name = "∈"; Kind = PredicateKind; Arity = Some 2 }, [term; set]))

    /// Constructs a union formula: φ ∨ ψ
    /// Represents the logical disjunction of two formulas.
    let unionFormula (φ: Formula<Symbol>) (ψ: Formula<Symbol>) : Formula<Symbol> =
        Connective (Or_ (φ, ψ))

    /// Constructs an intersection formula: φ ∧ ψ
    /// Represents the logical conjunction of two formulas.
    let intersectionFormula (φ: Formula<Symbol>) (ψ: Formula<Symbol>) : Formula<Symbol> =
        Connective (And_ (φ, ψ))
    
    /// Constructs an implication formula: φ → ψ
    /// Represents logical implication from φ to ψ.
    let impliesFormula (φ: Formula<Symbol>) (ψ: Formula<Symbol>) : Formula<Symbol> =
        Connective (Implies_ (φ, ψ))

    /// Constructs a complement formula: ¬φ
    /// Represents the logical negation of a formula.
    let complementFormula (φ: Formula<Symbol>) : Formula<Symbol> =
        Connective (Not_ φ)

    /// Constructs a domain restriction formula: ∀x ∈ A. f(x) ∈ B
    /// Ensures that function f maps elements from domain A into codomain B.
    /// Useful for modeling partial functions, dispatch zones, and algebraic mappings.
    let domainRestrictionFormula (f: Symbol) (domain: Symbol) (codomain: Symbol) : Formula<Symbol> =
        let x = { Name = "x"; Kind = VariableKind; Arity = None }
        let fx = Func(f, [Var x])
        let inSet s t = Predicate({ Name = "∈"; Kind = PredicateKind; Arity = Some 2 }, [t; Constant s])
        Quantified {
            Bound = ForAll x
            Body = Connective (Implies_ (Atomic (inSet domain (Var x)), Atomic (inSet codomain fx)))
        }

    /// Constructs a closure formula: ∀x ∈ A. f(x) ∈ A
    /// Ensures that function f preserves membership within set A.
    /// Useful for modeling closure under operations, algebraic structures, and civic invariants
    let closureFormula (f: Symbol) (set: Symbol) : Formula<Symbol> =
        domainRestrictionFormula f set set

    /// Constructs a set equality formula: ∀x. x ∈ A ↔ x ∈ B
    /// Asserts that sets A and B contain exactly the same elements.
    /// Useful for symbolic equivalence, civic audits, and remix-safe comparisons.
    let setEqualityFormula (a: Symbol) (b: Symbol) : Formula<Symbol> =
        let x = { Name = "x"; Kind = VariableKind; Arity = None }
        let inSet s t = Predicate({ Name = "∈"; Kind = PredicateKind; Arity = Some 2 }, [t; Constant s])
        Quantified {
            Bound = ForAll x
            Body = Connective (And_ (
                Connective (Implies_ (Atomic (inSet a (Var x)), Atomic (inSet b (Var x)))),
                Connective (Implies_ (Atomic (inSet b (Var x)), Atomic (inSet a (Var x))))
            ))
        }

    /// Attempts to construct a union formula if 'S is Symbol.
    /// Returns Some formula if successful, otherwise None.
    /// Remixers can pipe None to their own symbolic signage logic.
    let tryUnion<'S> (fa: Formula<'S>) (fb: Formula<'S>) : Formula<'S> option =
        if typeof<'S> = typeof<Symbol> then
            let fa' = unbox<Formula<Symbol>> (box fa)
            let fb' = unbox<Formula<Symbol>> (box fb)
            let result = unionFormula fa' fb'
            Some (box result :?> Formula<'S>)
        else None

    /// Attempts to construct an intersection formula if 'S is Symbol.
    /// Returns Some formula if successful, otherwise None.
    /// Remixers can extend this for other symbolic types.
    let tryIntersection<'S> (fa: Formula<'S>) (fb: Formula<'S>) : Formula<'S> option =
        if typeof<'S> = typeof<Symbol> then
            let fa' = unbox<Formula<Symbol>> (box fa)
            let fb' = unbox<Formula<Symbol>> (box fb)
            let result = intersectionFormula fa' fb'
            Some (box result :?> Formula<'S>)
        else None

    /// Attempts to construct a complement formula if 'S is Symbol.
    /// Returns Some formula if successful, otherwise None.
    /// Remixers can override this for custom signage dialects.
    let tryComplement<'S> (fa: Formula<'S>) : Formula<'S> option =
        if typeof<'S> = typeof<Symbol> then
            let fa' = unbox<Formula<Symbol>> (box fa)
            let result = complementFormula fa'
            Some (box result :?> Formula<'S>)
        else None

    /// Synthesizes a quantified membership predicate ∀x. x ∈ A for a Symbol constant
    let memberPredicateForSymbol (setSym: Symbol) : Formula<Symbol> =
        let x = { Name = "x"; Kind = VariableKind; Arity = None }
        let inSet s t = Predicate({ Name = "∈"; Kind = PredicateKind; Arity = Some 2 }, [t; Constant s])
        Quantified { Bound = ForAll x; Body = Atomic (inSet setSym (Var x)) }

    /// Build a union formula from available formulas or symbols
    let unionFromOptions (faOpt: Formula<Symbol> option) (saOpt: Symbol option)
                        (fbOpt: Formula<Symbol> option) (sbOpt: Symbol option)
                        : Formula<Symbol> option =
        match faOpt, saOpt, fbOpt, sbOpt with
        | Some fa, _, Some fb, _ -> Some (unionFormula fa fb)
        | Some fa, _, None, Some sb -> Some (unionFormula fa (memberPredicateForSymbol sb))
        | None, Some sa, Some fb, _ -> Some (unionFormula (memberPredicateForSymbol sa) fb)
        | None, Some sa, None, Some sb -> Some (unionFormula (memberPredicateForSymbol sa) (memberPredicateForSymbol sb))
        | _ -> None

module FormulaPrinter =

    let rec termToString (term: Term<Symbol>) : string =
        match term with
        | Var s -> s.Name
        | Constant s -> s.Name
        | Func (f, args) ->
            let argsStr = args |> List.map termToString |> String.concat ", "
            $"{f.Name}({argsStr})"

    let atomicToString (atom: AtomicFormula<Symbol>) : string =
        match atom with
        | Predicate (p, args) ->
            match args with
            | [a; b] when p.Name = "≥" || p.Name = "=" || p.Name = "<" || p.Name = "≤" || p.Name = ">" || p.Name = "≠" ->
                $"{termToString a} {p.Name} {termToString b}"
            | _ ->
                let argsStr = args |> List.map termToString |> String.concat ", "
                $"{p.Name}({argsStr})"
        | Equality (t1, t2) -> $"{termToString t1} = {termToString t2}"

    let rec formulaToString (f: Formula<Symbol>) : string =
        match f with
        | Atomic atom -> atomicToString atom
        | Connective conn ->
            match conn with
            | Not_ φ -> $"¬({formulaToString φ})"
            | And_ (φ, ψ) -> $"({formulaToString φ} ∧ {formulaToString ψ})"
            | Or_ (φ, ψ) -> $"({formulaToString φ} ∨ {formulaToString ψ})"
            | Implies_ (φ, ψ) -> $"({formulaToString φ} → {formulaToString ψ})"
        | Quantified q ->
            let quantifierStr =
                match q.Bound with
                | ForAll s -> $"∀{s.Name}"
                | Exists s -> $"∃{s.Name}"
            $"{quantifierStr}.({formulaToString q.Body})"

module Connectives =
    /// All unary connectives have the form:
    /// (φ: Formula<'Symbol>) : Formula<'Symbol>
    /// All binary connectives have the form:
    /// (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol>
    
    /// Negation: ¬φ
    let neg φ = Connective (Not_ φ)    

    /// Biconditional: φ ↔ ψ ≡ (φ → ψ) ∧ (ψ → φ)
    let iff (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> = 
        Connective (And_ (Connective (Implies_ (φ, ψ)), Connective (Implies_ (ψ, φ))))

    /// Exclusive Or: φ ⊕ ψ ≡ (φ ∨ ψ) ∧ ¬(φ ∧ ψ)
    let xor (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> = 
        Connective (And_ (Connective (Or_ (φ, ψ)), Connective (Not_ (Connective (And_ (φ, ψ))))))

    /// NAND: φ ↑ ψ ≡ ¬(φ ∧ ψ)
    let nand (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> =
        Connective (Not_ (Connective (And_ (φ, ψ))))

    /// NOR: φ ↓ ψ ≡ ¬(φ ∨ ψ)
    let nor (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> =
        Connective (Not_ (Connective (Or_ (φ, ψ))))

    /// XNOR: φ ⊙ ψ ≡ ¬(φ ⊕ ψ)
    let xnor (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> =
        Connective (Not_ (xor φ ψ))

    /// Nonimplication: φ ⇏ ψ ≡ φ ∧ ¬ψ
    let nonimplication (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> =
        Connective (And_ (φ, neg ψ))

    /// Converse: ψ → φ
    let converse (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> =
        Connective (Implies_ (ψ, φ))

    /// Converse Nonimplication: ψ ⇍ φ ≡ ψ ∧ ¬φ
    let converseNonimplication (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> =
        Connective (And_ (ψ, Connective (Not_ φ)))

    /// Nonequivalence: φ ≢ ψ ≡ ¬(φ ↔ ψ)
    let nonequivalence (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> =
        Connective (Not_ (iff φ ψ))
