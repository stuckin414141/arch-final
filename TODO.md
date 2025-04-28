
## Things to do
- add ability to make assignments to records (shudders)

add separate closure-rewrites to the AST
- fyi the lowering from AST -> MIR considers function pointers to simply be addresses

- do a fuck ton of testing of everything
    - records should be fine, as should basic operations
    - you should test scoping issues next
    - along with break statements
write tests to check the typechecker rejects invalid programs


## Nice to haves 
zero-argument functions with unit
re-structure pipeline to better support debugging multiple analyses/re-writes
- optimally you will have a single compilation pipeline
- turning on debug mode dumps debug info to other files

implement polymorphic types/typechecking
implement type-inference
add arrays/pointers???????

## Language features/spec/changes

we are for now forbidding recursive types (will implement after closures)

apparently declarations made in the condition of an if and/or while
statement and/or expression are available in the body. Unify the rest of the system

declarations made inside any expression will not be available outside
of that expression