Succ: 
Pred: 
Label main
Var(traverse_list0) := Address(0func_traverse_list) (size: 8)
Return 0


Succ: 0IfUnitEnd, 0IfUnitTrue
Pred: 
Label 0func_traverse_list
Var(traverse_list0) := Address(0func_traverse_list) (size: 8)
head0 = Arg
Temp(t0) := Var(head0) != 0 (size: 1)
 if Temp(t0) goto 0IfUnitTrue
 goto 0IfUnitEnd


Succ: 0IfUnitEnd
Pred: 
Label 0IfUnitTrue
Temp(t1) := *(Var(head0) + 0) (size: 8)
Call Address(print) (Temp(t1))
Temp(t2) := *(Var(head0) + 8) (size: 8)
Temp(t3) := Call Var(traverse_list0) (Temp(t2))
Var(z0) := Temp(t3) (size: 8)
 goto 0IfUnitEnd


Succ: 
Pred: 
Label 0IfUnitEnd
Return 0
