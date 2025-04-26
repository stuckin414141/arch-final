let rec fac (n : int) : int = 
  if n = 0 then
    0
  else
    let next : int = n - 1 in
    n * (fac next)

