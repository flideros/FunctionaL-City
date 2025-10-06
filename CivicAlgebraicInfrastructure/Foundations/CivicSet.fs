namespace CivicAlgebraicInfrastructure.Foundations.CivicSet

module DomainModel =
    open CivicAlgebraicInfrastructure.Foundations.FOL.DomainModel

    type Cardinality =
        | Finite of int
        | Aleph0    // ℵ₀
        | Continuum // 2^ℵ₀
        | Other of string

    type Countability =
        | Countable
        | Uncountable

    type OrderType =
        | TotalOrder
        | PartialOrder
        | Unordered

    type SetTheoreticMetadata =
        { Cardinality  : Cardinality option
          Countability : Countability option
          OrderType    : OrderType option }

    type CivicSetMetadataItem =
        | SetTheoretic of SetTheoreticMetadata
        | FOL of FOLMetadata
        | Provenance of string      // e.g. "Defined in ZFC", "Derived from Peano axioms"
        | Tag of string             // free-form civic signage
        | Note of string
        | Custom of string * string // extension point

    // Interface   
    type ICivicSet<'Concrete,'Symbolic> =
        abstract member Symbol : string option
        abstract member Formula : Formula<'Symbolic> option
        abstract member Contains : 'Concrete -> bool
        abstract member Elements : seq<'Concrete>

        // Ordering
        abstract member Compare : option<'Concrete -> 'Concrete -> int>
        abstract member Min : option<'Concrete>
        abstract member Max : option<'Concrete>

        // Meta-signage (derived or declared)
        abstract member Metadata : CivicSetMetadataItem list

        // Logical overlays
        abstract member IsClosedUnder : (ICivicSet<'Concrete,'Symbolic> -> ICivicSet<'Concrete,'Symbolic>) -> bool
        abstract member Implies : ICivicSet<'Concrete,'Symbolic> -> bool
        abstract member EquivalentTo : ICivicSet<'Concrete,'Symbolic> -> bool


   
module Operations =
    open DomainModel

    let isSubsetOf (a: ICivicSet<'T,'S>) (b: ICivicSet<'T,'S>) : bool =
        a.Elements |> Seq.forall b.Contains
