
## Things to do

- test alpha-renaming
- you currently don't have support for negative integers.
    - add support for two's complement
add separate closure-rewrites to the AST (after type-checking)
- two problems:
    - recursion
        - the ideal is that if the recursive call requires an environment,
            then an environment will be passed to the recursive function
        - solution: this is just a general case of "reference outer variable,
            it's a function variable"
            - insert our variable as a recursive type/ill-defined function type
            - when it is referenced
                - if no environment needed then just address is returned
                - if evnrionment is needed then we will not allocate a new
                    environment. Pass it in the old environment. Of course,
                    we will still allocate the closure object
                - specifically the concept of a not-quite-defined function
                    is required I believe
        - recursion code is still going to be used from MIR, we're just going
            to re-write it now
    - calling functions with different type environments
        - this will 100% violate the type checking, so do it afterwards
        - essentially, we will store information in the upper 16 bits of 
            each function pointer/address
        - MIR will look at the upper 16 bits
            - if it's clear then it's an address in code, just jump to it
            - if there is something in the upper 16 bits then it's a pointer/closure.
                Mask off the upper 16 bits, access the environment from the pointer,
                and pass it into the function call as an arg
            - this also means that will likely need to create a new type/node:
                closure
                - it's basically a 2-word record, only different is that
                    it'll tell MIR to clobber upper 16-bits
            - won't interfere with recursion stuff, since recursion stuff
                will just be treated as a clear address
    - 
- fyi the lowering from AST -> MIR considers function pointers to simply be addresses
- this is the place where you should handle recursion, which is also a fun problem

- do a fuck ton of testing of everything
    - records should be fine, as should basic operations
    - you should test scoping issues next
    - along with break statements
write tests to check the typechecker rejects invalid programs

## Test case ideas
- generators (should be possible with capture)

## Nice to haves 
have recursive types not be opaque/scuffed
- the type checker is currently crying
zero-argument functions with unit

implement polymorphic types/typechecking
implement type-inference
add arrays/pointers???????

## Language features/spec/changes

- nullptr is now added. It is considered a base type for any
    pointer type (records and function pointers)
    - 
we are for now forbidding recursive types (will implement after closures)

apparently declarations made in the condition of an if and/or while
statement and/or expression are available in the body. Unify the rest of the system

declarations made inside any expression will not be available outside
of that expression

assignments to record expressions are completely stupid and functionally no-ops.
As such, even though records are technically l-values, we forbid them

