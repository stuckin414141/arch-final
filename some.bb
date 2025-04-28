Succ: 
Pred: 
Label main
Var(outer10) := 10 (size: 8)
Var(outer20) := 20 (size: 8)
Var(outer30) := 30 (size: 8)
Var(selective_capture0) := Address(0func_selective_capture0) (size: 8)
Temp(t2) := Call Var(selective_capture0) (0)
Call Address(print) (Temp(t2))
Return 0


Succ: 
Pred: 
Label 0func_selective_capture0
b0 = Arg
Var(inner10) := Address(1func_inner10) (size: 8)
Var(unused0) := Var(outer20) (size: 8)
Temp(t1) := Call Var(inner10) (1)
Return Temp(t1)


Succ: 
Pred: 
Label 1func_inner10
b1 = Arg
Temp(t0) := Var(outer10) + Var(outer30) (size: 8)
Return Temp(t0)
