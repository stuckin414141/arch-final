Succ: 0IfUnitEnd, 0IfUnitTrue
Pred: 
Label main
Temp(t0) := Call Address(gib_mem) (16)
*(Temp(t0) + 0) := 5 (size: 8)
*(Temp(t0) + 8) := 10 (size: 8)
Var(p10) := Temp(t0) (size: 8)
Temp(t1) := Call Address(gib_mem) (16)
*(Temp(t1) + 0) := 3 (size: 8)
*(Temp(t1) + 8) := 7 (size: 8)
Var(p20) := Temp(t1) (size: 8)
Temp(t2) := *(Var(p10) + 0) (size: 8)
Temp(t3) := *(Var(p20) + 8) (size: 8)
Temp(t4) := Temp(t2) + Temp(t3) (size: 8)
Call Address(print) (Temp(t4))
Temp(t5) := Call Address(gib_mem) (40)
*(Temp(t5) + 0) := 30 (size: 8)
*(Temp(t5) + 8) := 1 (size: 1)
*(Temp(t5) + 9) := 0 (size: 1)
*(Temp(t5) + 16) := 4 (size: 8)
*(Temp(t5) + 24) := 40000 (size: 8)
*(Temp(t5) + 32) := 9 (size: 8)
Var(employee0) := Temp(t5) (size: 8)
Temp(t6) := *(Var(employee0) + 24) (size: 8)
Call Address(print) (Temp(t6))
Temp(t7) := *(Var(employee0) + 8) (size: 1)
 if Temp(t7) goto 0IfUnitTrue
 goto 0IfUnitEnd


Succ: 0IfUnitEnd
Pred: 
Label 0IfUnitTrue
Call Address(print) (1)
 goto 0IfUnitEnd


Succ: 
Pred: 
Label 0IfUnitEnd
Var(yourefired0) := Address(0func_yourefired) (size: 8)
Temp(t15) := Call Address(gib_mem) (16)
*(Temp(t15) + 0) := Var(yourefired0) (size: 8)
*(Temp(t15) + 8) := 10 (size: 8)
Var(tyrell0) := Temp(t15) (size: 8)
Temp(t17) := *(Var(tyrell0) + 0) (size: 8)
Temp(t16) := Call Temp(t17) (Var(employee0))
Var(sad0) := Temp(t16) (size: 8)
Temp(t18) := *(Var(sad0) + 32) (size: 8)
Call Address(print) (Temp(t18))
*(Var(sad0) + 32) := 33333 (size: 8)
Temp(t19) := *(Var(sad0) + 32) (size: 8)
Call Address(print) (Temp(t19))
Return 0


Succ: 1Else, 1Then
Pred: 
Label 0func_yourefired
e0 = Arg
Temp(t9) := *(Var(e0) + 16) (size: 8)
Temp(t10) := Temp(t9) < 5 (size: 1)
 if Temp(t10) goto 1Then
 goto 1Else


Succ: 1IfEnd
Pred: 
Label 1Then
Temp(t11) := Call Address(gib_mem) (40)
Temp(t12) := *(Var(e0) + 0) (size: 8)
*(Temp(t11) + 0) := Temp(t12) (size: 8)
Temp(t13) := *(Var(e0) + 8) (size: 1)
*(Temp(t11) + 8) := Temp(t13) (size: 1)
*(Temp(t11) + 9) := 0 (size: 1)
*(Temp(t11) + 16) := 0 (size: 8)
Temp(t14) := *(Var(e0) + 24) (size: 8)
*(Temp(t11) + 24) := Temp(t14) (size: 8)
*(Temp(t11) + 32) := 0 (size: 8)
Temp(t8) := Temp(t11) (size: 8)
 goto 1IfEnd


Succ: 1IfEnd
Pred: 
Label 1Else
Temp(t8) := Var(e0) (size: 8)
 goto 1IfEnd


Succ: 
Pred: 
Label 1IfEnd
Return Temp(t8)
