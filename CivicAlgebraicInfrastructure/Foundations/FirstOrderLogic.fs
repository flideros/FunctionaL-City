namespace CivicAlgebraicInfrastructure.Foundations.FOL

module DomainModel =
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
        | And
        | Or
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
    

module Connectives=
    open DomainModel    
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
