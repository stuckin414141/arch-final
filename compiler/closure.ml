(*
* We will implement closure as a set of re-writes
* 4 changes need to be made:
  - modifications to variables accessed by nested functions
    need to be moved to closure
  - accesses of variables used by nested functions need to be moved to closure
  - we need to add an env arg
  - we need to initialize a static link for every function
    - this will be 0-initialized in the case of integers, false-initialized in the case
      of chars
        - there may be function/records but it doesn't matter since
          they're basically pointers
*)