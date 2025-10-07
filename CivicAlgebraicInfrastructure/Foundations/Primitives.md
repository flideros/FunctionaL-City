## 🏛️ Reflection in Lifted Core — `Primitives.fs`

This table categorizes each helper by its civic role, reflection risk, and provenance guarantees. 
```
| Helper Function                | Civic Role                            | Reflection Risk  | Provenance Guarantee   | Notes                                         |
|--------------------------------|---------------------------------------|------------------|------------------------|------------------------------------------     |
| `getUnionCasesCached`          | Metadata cache for union inspection   | 🔸 Medium        | ❌ None                | Thread-safe, but relies on runtime shape      |
| `safeIsUnion`                  | Fail-fast union test                  | ✅ Low           | ❌ None                | Wraps `FSharpType.IsUnion` safely             |
| `fullNameContains`             | Heuristic type filter                 | ✅ Low           | ❌ None                | Used in `isLiftedType` for runtime naming     |
| `isLiftedType`                 | Canonical Lifted type test            | 🔸 Medium        | ❌ None                | Combines union check + name probe             |
| `getOptionCases`               | Validates and extracts Option cases   | 🔸 Medium        | ✅ Structural          | Pattern-match only; no unsafe casts           |
| `mkOptionOfProvenance`         | Constructs boxed Option<Provenance>   | 🔴 High          | ✅ Explicit            | Uses `MakeUnion`; fails fast on type mismatch |
| `tryUnbox<'T>`                 | Safe unboxing                         | ✅ Low           | ❌ None                | Civic-native alternative to unsafe cast       |
| `getTypeOpt`                   | Safe type probe                       | ✅ Low           | ❌ None                | Used in fallback extractors                   |
| `findNearestProvenance`        | Recursive provenance search           | 🔸 Medium        | ✅ Best-effort         | Uses structural descent + reflection fallback |
| `provOfField`                  | Layered provenance extractor          | ✅ Low           | ✅ Best-effort         | Combines cast, unbox, and fallback search     |
| `mkCase`                       | Canonical LiftedObj constructor       | 🔴 High          | ✅ Explicit            | Uses `MakeUnion` with provenance boxing       |
| `tryBoxLiftedTypedObj`         | Strict canonicalizer                  | 🔸 Medium        | ✅ Explicit or Error   | Returns `Result`; narrates failure            |
| `boxLiftedTypedObj`            | Lossy fallback wrapper                | 🔴 High          | ✅ Best-effort         | Preserves provenance if possible              |
| `descendToPrimitiveWithProv`   | Structural descent with provenance    | ✅ Low           | ✅ Best-effort         | Uses extractor + fallback search              |
| `unwrapAllObjWithParent`       | Recursive unwrapping with provenance  | ✅ Low           | ✅ Best-effort         | Preserves parent provenance                   |
| `unwrapAll`                    | Public wrapper for unwrapping         | ✅ Low           | ✅ Best-effort         | Entry point for civic-native traversal        |
```
### 🧠 Legend

- **Reflection Risk**:
  - ✅ Low: No runtime construction or unsafe casts.
  - 🔸 Medium: Uses reflection probes or metadata, but no dynamic construction.
  - 🔴 High: Constructs values or relies on runtime shape; fragile without validation.

- **Provenance Guarantee**:
  - ✅ Explicit: Fully validated and boxed with provenance.
  - ✅ Structural: Derived from union shape or pattern match.
  - ✅ Best-effort: Uses fallback search or layered extraction.
  - ❌ None: No provenance