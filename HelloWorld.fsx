(*
# Ordinance: Civic Greeting

## Scope: City-Wide  
## Type: Onboarding Ritual   
---
## Rationale

The Civic Greeting Ordinance serves as the first contact between FunctionaL City and its citizens. It embodies the principle of **inspectable onboarding**, where every welcome is both executable and narratable. By using `printfn`, it affirms the city's commitment to transparency and symbolic clarity.
This ordinance avoids shell-based pause commands, which are prone to failure post-FSI execution. Instead, it uses `System.Console.ReadLine()`—a lawful, native pause mechanism that aligns with FunctionaL City's infrastructure ethos.
---
## Implementation
*)

printfn "Hello World. Welcome to FunctionaL City!"

System.Console.ReadLine() |> ignore
