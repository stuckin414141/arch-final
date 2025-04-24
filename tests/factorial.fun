let fac : int -> int = ftmlk (n : int) {
    if n == 0 then
        1
    else 
        n * fac(n-1)
}