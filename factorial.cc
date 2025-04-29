Seq
  LetStmt 0sl
    type: {fac0: (int)->(int)}
    Expr
      RecordExp
        Field fac0
          Expr
            nullptr
          Type: (int)->(int)
    Is closure: false
    Is recursive: false
  Seq
    LetStmt fac0
      type: (int)->(int)
      Expr
        Ftmlk
        Requires env: true
          Args
            0env : {fac0: (int)->(int)}
              Ref: false
            n0 : int
              Ref: false
          Body
            Let 0sl
              Expr
                RecordExp
                  Field 0prev
                    Expr
                      Var 0env
                    Type: {fac0: (int)->(int)}
              Body
                If
                  Cond
                    BinOp ==
                      Left
                        Var n0
                      Right
                        Num 0
                      Type: bool
                  Then
                    Num 1
                  Else
                    BinOp *
                      Left
                        Var n0
                      Right
                        FtmlkApp
                          Func
                            Var fac0
                          Args
                            BinOp -
                              Left
                                Var n0
                              Right
                                Num 1
                              Type: int
                      Type: int
                  Type: int
              Is closure: false
              Recursive: false
              Type: {0prev: {fac0: (int)->(int)}}
      Is closure: true
      Is recursive: true
    Print
      FtmlkApp
        Func
          Var fac0
        Args
          Num 6
