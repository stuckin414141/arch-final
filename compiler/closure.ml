(*
* We will implement closure as a set of re-writes
* Several changes need to be made:
  - modifications to variables accessed by nested functions
    need to be moved to closure
  - accesses of variables used by nested functions need to be moved to closure
  - we need to add an env arg
  - we need to initialize a static link for every function that includes
    nested functions which access outer variables
  - re-write function application to check if it's calling a closure or
    function address
  - re-write ftmlk/function expressions to either return a closure (with
    requisite masks) or a plain old address
*)

(*
It therefore makes sense to perform closure-convresion in three passes:
- The first is to create the environment argument/static links. 
  The principle piece of information that this requires is exactly
  what variables in a function are accessed by nested functions.
  We perform a first pass to mark this information in let statements.
  After let statements are populated, we will perform two passes on every
  function body:
  - the first pass refrains from entering any function body. It is soley
    to construct the type of the current 
    static link, absent the type of the prev pointer
  - we use the previous information to perform another pass with deference to function
    bodies, passing the type of the previous static link/environment into the function.
    That type becomes of the type of the function's 0env argument.
  - for all types other than bool, we will treat them as integers and initialize
    them with 0
- Finally, when the static links are all created + their types are determined,
  we will go back through and re-write all variable accesses accordingly

*)