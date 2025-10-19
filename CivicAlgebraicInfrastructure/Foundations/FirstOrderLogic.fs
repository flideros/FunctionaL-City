namespace CivicAlgebraicInfrastructure.Foundations.FOL

open System

/// <summary>
/// Term in the first-order logic DSL.
/// </summary>
/// <typeparam name="Symbol">The symbol representation type used for variables, constants and function names.</typeparam>
/// <signage>
/// - Var: variable symbol used for quantified expressions and pattern matching.
/// - Constant: named constant symbol used to represent set names or constant values.
/// - Func: function application with a symbol for the function and an argument list of Terms.
/// Use Term as the canonical representation for symbolic payloads attached to provenance and signage.
/// </signage>
type Term<'Symbol> =
    | Var of 'Symbol
    | Constant of 'Symbol
    | Func of 'Symbol * Term<'Symbol> list

/// <summary>
/// Quantifier used to bind a single variable symbol in a quantified formula.
/// </summary>
/// <typeparam name="Symbol">Symbol type used for variable binding.</typeparam>
/// <signage>
/// - ForAll: universal quantifier (∀x. φ).
/// - Exists: existential quantifier (∃x. φ).
/// Use Quantifier when building Quantified formulas for symbolic signage overlays.
/// </signage>
type Quantifier<'Symbol> =
    | ForAll of 'Symbol    // Universal Quantifier: ∀x. φ
    | Exists of 'Symbol    // Existential Quantifier: ∃x. φ


/// <summary>
/// Atomic formula forms: predicate application or equality of two terms.
/// </summary>
/// <typeparam name="Symbol">Symbol type used in predicate and term symbols.</typeparam>
/// <signage>
/// - Predicate: n-ary predicate applied to a list of Terms.
/// - Equality: standard equality between two Terms.
/// AtomicFormula is the smallest indivisible logical unit used by Formula.
/// </signage>
type AtomicFormula<'Symbol> =
    | Predicate of 'Symbol * Term<'Symbol> list   // P(t₁, ..., tₙ)
    | Equality of Term<'Symbol> * Term<'Symbol>   // t₁ = t₂

/// <summary>
/// Primitive connectives and compound formulas.
/// </summary>
/// <typeparam name="Symbol">Symbol type used in nested Formula and AtomicFormula forms.</typeparam>
/// <signage>
/// - Atomic: wraps an AtomicFormula.
/// - Connective: unary or binary logical connectives (¬, ∧, ∨, →).
/// - Quantified: a bound variable with a body Formula.
/// Formula is the central type for constructing symbolic signage expressions.
/// </signage>
and Formula<'Symbol> =
    | Atomic of AtomicFormula<'Symbol>
    | Connective of PrimitiveConnective<'Symbol>
    | Quantified of Quantified<'Symbol>

/// <summary>
/// Primitive logical connectives used inside Connective nodes.
/// </summary>
/// <typeparam name="Symbol">Symbol type forwarded into nested Formula references.</typeparam>
/// <signage>
/// - Not_: negation (¬φ).
/// - And_, Or_, Implies_: standard binary connectives used to express set operations symbolically.
/// This type intentionally names constructors with underscores to avoid downstream naming collisions.
/// </signage>
and PrimitiveConnective<'Symbol> =
    | Not_ of Formula<'Symbol>                         // Negation: ¬φ
    | And_ of Formula<'Symbol> * Formula<'Symbol>      // Conjunction: φ ∧ ψ
    | Or_ of Formula<'Symbol> * Formula<'Symbol>       // Disjunction: φ ∨ ψ
    | Implies_ of Formula<'Symbol> * Formula<'Symbol>  // Implication: φ → ψ

/// <summary>
/// Quantified formula payload containing bound quantifier and body.
/// </summary>
/// <typeparam name="Symbol">Symbol type for the bound variable.</typeparam>
/// <signage>
/// Use Quantified to represent formulas of the shape ∀x. φ or ∃x. φ in signage overlays and synthesized proofs.
/// </signage>
and Quantified<'Symbol> =
    { Bound : Quantifier<'Symbol>
      Body  : Formula<'Symbol> }

/// <summary>
/// Additional naming for connectives used to avoid collisions in downstream consumers.
/// </summary>
/// <signage>
/// Use ConnectiveKind for tool-facing classifications and analytics where more connective kinds are required.
/// The values here are descriptive tokens for signage mapping rather than concrete Formula constructors.
/// </signage>
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

/// <summary>
/// Decision-theoretic metadata for symbolic formulas.
/// </summary>
/// <signage>
/// - Decidability: signals whether assertions are decidable, semi-decidable, or undecidable.
/// - Enumerability: signals whether witnesses are enumerable for algorithmic tooling.
/// This metadata can be attached to sets or proofs to inform tooling and signage about algorithmic properties.
/// </signage>
type Decidability =
    | Decidable
    | SemiDecidable
    | Undecidable

/// <summary>
/// Enumerability metadata used for signage about enumerability of sets or predicate witnesses.
/// </summary>
/// <signage>
/// - Enumerable: enumerator exists or is expected.
/// - NonEnumerable: no enumerator; signage should indicate care for sampling and merges.
/// </signage>
type Enumerability =
    | Enumerable
    | NonEnumerable

/// <summary>
/// FOL-related metadata record used in set metadata overlays.
/// </summary>
/// <signage>
/// Stores decidability and enumerability hints to guide validators, merges, and UI overlays.
/// Both fields are optional so remixers can add only the hints they compute.
/// </signage>
type FOLMetadata =
    { Decidability : Decidability option
      Enumerability: Enumerability option }

/// <summary>
/// Kinds of symbol roles in the DSL.
/// </summary>
/// <signage>
/// - VariableKind: bound or free variables in formulas.
/// - ConstantKind: named set or value constants.
/// - FunctionKind: function symbols with arity.
/// - PredicateKind: predicate symbols used in AtomicFormula.
/// Use SymbolKind when emitting signage that distinguishes symbol roles for readability.
/// </signage>
type SymbolKind =
    | VariableKind
    | ConstantKind
    | FunctionKind
    | PredicateKind

/// <summary>
/// Concrete symbol representation used across the FOL DSL.
/// </summary>
/// <signage>
/// - Name: human-readable token used on signage.
/// - Kind: role of the symbol (variable/constant/function/predicate).
/// - Arity: meaningful for functions and predicates; None for variables/constants.
/// Symbols are the units displayed on signage and used when synthesizing formulas.
/// </signage>
type Symbol =
    { Name  : string
      Kind  : SymbolKind
      Arity : int option }   // only meaningful for functions/predicates

module Formulae =

    /// <summary>
    /// Construct a membership predicate formula representing "term ∈ set".
    /// </summary>
    /// <param name="term">Term that is tested for membership.</param>
    /// <param name="set">Term representing the set (often a Constant symbol).</param>
    /// <returns>Formula representing membership as a binary predicate.</returns>
    /// <signage>
    /// Uses a reserved predicate symbol "∈" with arity 2. Use this helper when synthesizing symbolic
    /// signage about set membership and union/intersection formation.
    /// </signage>
    let memberOf (term: Term<Symbol>) (set: Term<Symbol>) : Formula<Symbol> =
        Atomic (Predicate({ Name = "∈"; Kind = PredicateKind; Arity = Some 2 }, [term; set]))

    /// <summary>
    /// Construct a disjunction (union) formula φ ∨ ψ.
    /// </summary>
    /// <returns>Connective Or_ node.</returns>
    /// <signage>Use unionFormula to express symbolic union overlays used in signage merges.</signage>
    let unionFormula (φ: Formula<Symbol>) (ψ: Formula<Symbol>) : Formula<Symbol> =
        Connective (Or_ (φ, ψ))

    /// <summary>
    /// Construct a conjunction (intersection) formula φ ∧ ψ.
    /// </summary>
    /// <signage>Use intersectionFormula to express symbolic intersection overlays used in signage merges.</signage>
    let intersectionFormula (φ: Formula<Symbol>) (ψ: Formula<Symbol>) : Formula<Symbol> =
        Connective (And_ (φ, ψ))
    
    /// <summary>
    /// Construct an implication formula φ → ψ.
    /// </summary>
    /// <signage>Use impliesFormula for symbolic containment and implication signage.</signage>
    let impliesFormula (φ: Formula<Symbol>) (ψ: Formula<Symbol>) : Formula<Symbol> =
        Connective (Implies_ (φ, ψ))

    /// <summary>
    /// Construct logical negation ¬φ.
    /// </summary>
    /// <signage>Use complementFormula when synthesizing complement signage or predicates.</signage>
    let complementFormula (φ: Formula<Symbol>) : Formula<Symbol> =
        Connective (Not_ φ)

    /// <summary>
    /// Build a domain restriction formula ∀x ∈ A. f(x) ∈ B.
    /// </summary>
    /// <param name="f">Symbol naming the function.</param>
    /// <param name="domain">Symbol naming the domain set A.</param>
    /// <param name="codomain">Symbol naming the codomain set B.</param>
    /// <returns>Quantified implication formula ensuring f maps domain into codomain.</returns>
    /// <signage>
    /// Useful for modelling partial functions, dispatch zoning, and algebraic mapping signage where function
    /// action must be shown to preserve domains.
    /// </signage>
    let domainRestrictionFormula (f: Symbol) (domain: Symbol) (codomain: Symbol) : Formula<Symbol> =
        let x = { Name = "x"; Kind = VariableKind; Arity = None }
        let fx = Func(f, [Var x])
        let inSet s t = Predicate({ Name = "∈"; Kind = PredicateKind; Arity = Some 2 }, [t; Constant s])
        Quantified {
            Bound = ForAll x
            Body = Connective (Implies_ (Atomic (inSet domain (Var x)), Atomic (inSet codomain fx)))
        }

    /// <summary>
    /// Build a closure formula ∀x ∈ A. f(x) ∈ A ensuring f preserves set A.
    /// </summary>
    /// <signage>Alias for domainRestrictionFormula with identical domain and codomain; used for civic invariants signage.</signage>
    let closureFormula (f: Symbol) (set: Symbol) : Formula<Symbol> =
        domainRestrictionFormula f set set

    /// <summary>
    /// Construct a set equality formula asserting ∀x. x ∈ A ↔ x ∈ B.
    /// </summary>
    /// <param name="a">Symbol naming set A.</param>
    /// <param name="b">Symbol naming set B.</param>
    /// <returns>Quantified conjunction of mutual implications representing set equality.</returns>
    /// <signage>Use setEqualityFormula for symbolic equivalence signage and remix-safe comparisons.</signage>
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

    /// <summary>
    /// Attempt to union two formulas when the generic symbol type equals Symbol.
    /// </summary>
    /// <returns>Some union formula when successful; otherwise None.</returns>
    /// <signage>
    /// This helper supports mixing formulas produced generically with Symbol-level helpers.
    /// If the generic type is not Symbol, None is returned so remixers can provide domain-specific signage logic.
    /// </signage>
    let tryUnion<'S> (fa: Formula<'S>) (fb: Formula<'S>) : Formula<'S> option =
        if typeof<'S> = typeof<Symbol> then
            let fa' = unbox<Formula<Symbol>> (box fa)
            let fb' = unbox<Formula<Symbol>> (box fb)
            let result = unionFormula fa' fb'
            Some (box result :?> Formula<'S>)
        else None

    /// <summary>
    /// Attempt to intersect two formulas when the generic symbol type equals Symbol.
    /// </summary>
    /// <returns>Some intersection formula when successful; otherwise None.</returns>
    /// <signage>See tryUnion for rationale and usage patterns.</signage>
    let tryIntersection<'S> (fa: Formula<'S>) (fb: Formula<'S>) : Formula<'S> option =
        if typeof<'S> = typeof<Symbol> then
            let fa' = unbox<Formula<Symbol>> (box fa)
            let fb' = unbox<Formula<Symbol>> (box fb)
            let result = intersectionFormula fa' fb'
            Some (box result :?> Formula<'S>)
        else None

    /// <summary>
    /// Attempt to complement a formula when the generic symbol type equals Symbol.
    /// </summary>
    /// <returns>Some complement formula when successful; otherwise None.</returns>
    /// <signage>See tryUnion for rationale and usage patterns.</signage>
    let tryComplement<'S> (fa: Formula<'S>) : Formula<'S> option =
        if typeof<'S> = typeof<Symbol> then
            let fa' = unbox<Formula<Symbol>> (box fa)
            let result = complementFormula fa'
            Some (box result :?> Formula<'S>)
        else None

    /// <summary>
    /// Synthesize a universal membership predicate ∀x. x ∈ A for a Symbol constant.
    /// </summary>
    /// <param name="setSym">Symbol naming the set.</param>
    /// <returns>Quantified membership formula for the provided symbol.</returns>
    /// <signage>Useful when a symbol name is available but no explicit formula exists; produces canonical signage.</signage>
    let memberPredicateForSymbol (setSym: Symbol) : Formula<Symbol> =
        let x = { Name = "x"; Kind = VariableKind; Arity = None }
        let inSet s t = Predicate({ Name = "∈"; Kind = PredicateKind; Arity = Some 2 }, [t; Constant s])
        Quantified { Bound = ForAll x; Body = Atomic (inSet setSym (Var x)) }

    /// <summary>
    /// Build a union formula from optional formulas and optional symbol names.
    /// </summary>
    /// <returns>Some union Formula when combinable; otherwise None.</returns>
    /// <signage>
    /// Priority:
    /// - Prefer concrete formulas when available.
    /// - Fall back to mixing formulas and symbol-membership predicates.
    /// - If only symbol names exist, synthesize quantified membership predicates and union them.
    /// This helper centralizes common signage synthesis patterns used by set constructors.
    /// </signage>
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

    /// <summary>
    /// Render a Term symbolically to a single-line string for signage.
    /// </summary>
    /// <signage>Used by UI and logging to display compact formula fragments.</signage>
    let rec termToString (term: Term<Symbol>) : string =
        match term with
        | Var s -> s.Name
        | Constant s -> s.Name
        | Func (f, args) ->
            let argsStr = args |> List.map termToString |> String.concat ", "
            $"{f.Name}({argsStr})"

    /// <summary>
    /// Render an AtomicFormula to a human readable string.
    /// </summary>
    /// <signage>Handles common binary predicate prettification like comparisons and membership tokens.</signage>
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


    /// <summary>
    /// Render a Formula to a human-readable string suitable for signage and logs.
    /// </summary>
    /// <signage>Includes quantifier display and parenthesized connectives for clarity on overlays.</signage>
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
    
    /// <summary>
    /// Logical negation helper: ¬φ.
    /// </summary>
    /// <signage>Convenience alias for building signable connectives.</signage>
    let neg φ = Connective (Not_ φ)    

    /// <summary>
    /// Biconditional (iff) constructed as (φ → ψ) ∧ (ψ → φ).
    /// </summary>
    /// <signage>Common derived connective used in signage when equivalence must be explicit.</signage>
    let iff (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> = 
        Connective (And_ (Connective (Implies_ (φ, ψ)), Connective (Implies_ (ψ, φ))))

    /// <summary>
    /// Exclusive OR: (φ ∨ ψ) ∧ ¬(φ ∧ ψ).
    /// </summary>
    /// <signage>Useful for signage that must show exact one-of semantics.</signage>
    let xor (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> = 
        Connective (And_ (Connective (Or_ (φ, ψ)), Connective (Not_ (Connective (And_ (φ, ψ))))))

    /// <summary>
    /// NAND: ¬(φ ∧ ψ).
    /// </summary>
    let nand (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> =
        Connective (Not_ (Connective (And_ (φ, ψ))))

    /// <summary>
    /// NOR: ¬(φ ∨ ψ).
    /// </summary>
    let nor (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> =
        Connective (Not_ (Connective (Or_ (φ, ψ))))

    /// <summary>
    /// XNOR: ¬(xor φ ψ).
    /// </summary>
    let xnor (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> =
        Connective (Not_ (xor φ ψ))

    /// <summary>
    /// Nonimplication: φ ∧ ¬ψ.
    /// </summary>
    let nonimplication (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> =
        Connective (And_ (φ, neg ψ))

    /// <summary>
    /// Converse implication: ψ → φ.
    /// </summary>
    let converse (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> =
        Connective (Implies_ (ψ, φ))

    /// <summary>
    /// Converse nonimplication: ψ ∧ ¬φ.
    /// </summary>
    let converseNonimplication (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> =
        Connective (And_ (ψ, Connective (Not_ φ)))

    /// <summary>
    /// Nonequivalence: ¬(φ ↔ ψ).
    /// </summary>
    let nonequivalence (φ: Formula<'Symbol>) (ψ: Formula<'Symbol>) : Formula<'Symbol> =
        Connective (Not_ (iff φ ψ))
