#load "FirstOrderLogic.fs"

open CivicAlgebraicInfrastructure.Foundations.FOL
open Connectives

// --- Sample Terms ---
let x = Var { Name  = "x";
                            Kind  = VariableKind;
                            Arity = None } 
let y = Var { Name  = "y";
                            Kind  = VariableKind;
                            Arity = None }
let c = Constant { Name  = "c";
                                  Kind  = ConstantKind;
                                  Arity = None }
let fxy = Func ({ Name  = "f";
                                Kind  = FunctionKind;
                                Arity = None },[x;y])

// --- Atomic Formulas ---
let p = Atomic (Predicate ({ Name  = "P";
                                               Kind  = FunctionKind;
                                               Arity = None }, [x; c]))
let q = Atomic (Predicate ({ Name  = "Q";
                                              Kind  = FunctionKind;
                                              Arity = None }, [y]))
let eq = Atomic (Equality (x, c))

// --- Primitive Connectives ---
let conj = Connective (And_ (p, q))          // P(x,c) ∧ Q(y)
let disj = Connective (Or_ (p, q))           // P(x,c) ∨ Q(y)
let impl = Connective (Implies_ (p, q))      // P(x,c) → Q(y)
let neg  = Connective (Not_ p)               // ¬P(x,c)

// --- Quantified Formulas ---
let forallx = Quantified { Bound = ForAll { Name  = "x";
                                             Kind  = VariableKind;
                                             Arity = None }; Body = p }   // ∀x. P(x,c)
let existsy = Quantified { Bound = Exists { Name  = "y";
                                             Kind  = VariableKind;
                                             Arity = None }; Body = q }   // ∃y. Q(y)

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
printfn "Conjunction: %A" (FormulaPrinter.formulaToString conj)
printfn "Disjunction: %A" (FormulaPrinter.formulaToString disj)
printfn "Implication: %A" (FormulaPrinter.formulaToString impl)
printfn "Negation: %A" (FormulaPrinter.formulaToString neg)
printfn "ForAll: %A" (FormulaPrinter.formulaToString forallx)
printfn "Exists: %A" (FormulaPrinter.formulaToString existsy)
printfn "Biconditional: %A" (FormulaPrinter.formulaToString bicond)
printfn "XOR: %A" (FormulaPrinter.formulaToString exclusive)
printfn "NAND: %A" (FormulaPrinter.formulaToString nandEx)
printfn "NOR: %A" (FormulaPrinter.formulaToString norEx)
printfn "XNOR: %A" (FormulaPrinter.formulaToString xnorEx)
printfn "Nonimplication: %A" (FormulaPrinter.formulaToString nonimpl)
printfn "Converse: %A" (FormulaPrinter.formulaToString conv)
printfn "Converse Nonimplication: %A" (FormulaPrinter.formulaToString convNonimpl)
printfn "Nonequivalence: %A" (FormulaPrinter.formulaToString nonequiv)

