Succ: 
Pred: 
Label main
Var(fac0) := Address(0func_fac0) (size: 8)
Temp(t5) := Call Var(fac0) (6)
Call Address(print) (Temp(t5))
Return 0


Succ: 0Else, 0Then
Pred: 
Label 0func_fac0
Var(0func_fac0) := Address(0func_fac0) (size: 8)
n0 = Arg
Temp(t1) := Var(n0) == 0 (size: 1)
 if Temp(t1) goto 0Then
 goto 0Else


Succ: 0IfEnd
Pred: 
Label 0Then
Temp(t0) := 1 (size: 8)
 goto 0IfEnd


Succ: 0IfEnd
Pred: 
Label 0Else
Temp(t2) := Var(n0) - 1 (size: 8)
Temp(t3) := Call Var(fac0) (Temp(t2))
Temp(t4) := Var(n0) * Temp(t3) (size: 8)
Temp(t0) := Temp(t4) (size: 8)
 goto 0IfEnd


Succ: 
Pred: 
Label 0IfEnd
Return Temp(t0)
