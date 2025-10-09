#load "FirstOrderLogic.fs"

open CivicAlgebraicInfrastructure.Foundations.FOL
open Connectives

// --- Sample Terms ---
let x = Var "x"
let y = Var "y"
let c = Constant "c"
let fxy = Func("f", [x; y])

// --- Atomic Formulas ---
let p = Atomic (Predicate ("P", [x; c]))
let q = Atomic (Predicate ("Q", [y]))
let eq = Atomic (Equality (x, c))

// --- Primitive Connectives ---
let conj = Connective (And_ (p, q))          // P(x,c) ∧ Q(y)
let disj = Connective (Or_ (p, q))           // P(x,c) ∨ Q(y)
let impl = Connective (Implies_ (p, q))      // P(x,c) → Q(y)
let neg  = Connective (Not_ p)               // ¬P(x,c)

// --- Quantified Formulas ---
let forallx = Quantified { Bound = ForAll "x"; Body = p }   // ∀x. P(x,c)
let existsy = Quantified { Bound = Exists "y"; Body = q }   // ∃y. Q(y)

// --- Derived Connectives ---
let bicond = iff p q                        // P(x,c) ↔ Q(y)
let exclusive = xor p q                     // P(x,c) ⊕ Q(y)
let nandEx = nand p q                       // ¬(P(x,c) ∧ Q(y))
let norEx = nor p q                         // ¬(P(x,c) ∨ Q(y))
let xnorEx = xnor p q                       // ¬(P(x,c) ⊕ Q(y))
let nonimpl = nonimplication p q            // P(x,c) ∧ ¬Q(y)
let conv = converse p q                     // Q(y) → P(x,c)
let convNonimpl = converseNonimplication p q// Q(y) ∧ ¬P(x,c)
let nonequiv = nonequivalence p q           // ¬(P(x,c) ↔ Q(y))

// --- Print results (raw union cases for now) ---
printfn "Conjunction: %A" conj
printfn "Disjunction: %A" disj
printfn "Implication: %A" impl
printfn "Negation: %A" neg
printfn "ForAll: %A" forallx
printfn "Exists: %A" existsy
printfn "Biconditional: %A" bicond
printfn "XOR: %A" exclusive
printfn "NAND: %A" nandEx
printfn "NOR: %A" norEx
printfn "XNOR: %A" xnorEx
printfn "Nonimplication: %A" nonimpl
printfn "Converse: %A" conv
printfn "Converse Nonimplication: %A" convNonimpl
printfn "Nonequivalence: %A" nonequiv
