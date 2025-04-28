Succ: 
Pred: 
Label main
Var(x0) := 10 (size: 8)
Var(nested_functions0) := Address(0func_nested_functions0) (size: 8)
Temp(t3) := Call Var(nested_functions0) (20)
Call Address(print) (Temp(t3))
Var(make_adder0) := Address(2func_make_adder0) (size: 8)
Temp(t8) := Call Address(gib_mem) (16)
*(Temp(t8) + 0) := 5 (size: 8)
*(Temp(t8) + 8) := 10 (size: 8)
Var(point0) := Temp(t8) (size: 8)
Temp(t9) := Call Var(make_adder0) (Var(point0))
Var(adder0) := Temp(t9) (size: 8)
Temp(t10) := Call Var(adder0) (15)
Call Address(print) (Temp(t10))
Return 0


Succ: 
Pred: 
Label 2func_make_adder0
p0 = Arg
Return Address(3F)


Succ: 
Pred: 
Label 3F
z0 = Arg
Temp(t4) := *(Var(p0) + 0) (size: 8)
Temp(t5) := *(Var(p0) + 8) (size: 8)
Temp(t6) := Temp(t4) + Temp(t5) (size: 8)
Temp(t7) := Temp(t6) + Var(z0) (size: 8)
Return Temp(t7)


Succ: 
Pred: 
Label 0func_nested_functions0
y0 = Arg
Var(f0) := Address(1func_f0) (size: 8)
Temp(t2) := Call Var(f0) (Var(y0))
Return Temp(t2)


Succ: 
Pred: 
Label 1func_f0
z0 = Arg
Temp(t0) := Var(x0) + Var(y0) (size: 8)
Temp(t1) := Temp(t0) + Var(z0) (size: 8)
Return Temp(t1)
