Succ: 
Pred: 
Label main
Var(add_elem0) := Address(0func_add_elem) (size: 8)
Var(traverse_list0) := Address(1func_traverse_list) (size: 8)
Temp(t5) := Call Var(add_elem0) (333, 0)
Temp(t6) := Call Var(add_elem0) (11, Temp(t5))
Temp(t7) := Call Var(add_elem0) (10, Temp(t6))
Var(z0) := Temp(t7) (size: 8)
Temp(t8) := Call Var(traverse_list0) (Var(z0))
Var(n0) := Temp(t8) (size: 8)
Return 0


Succ: 0IfUnitEnd, 0IfUnitTrue
Pred: 
Label 1func_traverse_list
Var(traverse_list0) := Address(1func_traverse_list) (size: 8)
head0 = Arg
Temp(t1) := Var(head0) != 0 (size: 1)
 if Temp(t1) goto 0IfUnitTrue
 goto 0IfUnitEnd


Succ: 0IfUnitEnd
Pred: 
Label 0IfUnitTrue
Temp(t2) := *(Var(head0) + 0) (size: 8)
Call Address(print) (Temp(t2))
Temp(t3) := *(Var(head0) + 8) (size: 8)
Temp(t4) := Call Var(traverse_list0) (Temp(t3))
Var(z0) := Temp(t4) (size: 8)
 goto 0IfUnitEnd


Succ: 
Pred: 
Label 0IfUnitEnd
Return 0


Succ: 
Pred: 
Label 0func_add_elem
elem0 = Arg
list0 = Arg
Temp(t0) := Call Address(gib_mem) (16)
*(Temp(t0) + 0) := Var(elem0) (size: 8)
*(Temp(t0) + 8) := Var(list0) (size: 8)
Return Temp(t0)
