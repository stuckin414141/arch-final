Seq
  Seq
    Seq
      Seq
        Seq
          IfUnit
            Cond
              BinOp >
                Left
                  Num 5
                Right
                  Num 3
            Body
              Print
                Num 1
          LetStmt x
            Type
              Typ int
            Expr
              If
                Cond
                  BinOp >
                    Left
                      Num 5
                    Right
                      Num 3
                Then
                  If
                    Cond
                      BinOp <
                        Left
                          Num 1
                        Right
                          Num 2
                    Then
                      Let x
                        Type
                          Typ int
                        Expr
                          Num 4
                        Body
                          ESeq
                            Stmt
                              While
                                BinOp <
                                  Left
                                    Var x
                                  Right
                                    Num 15
                                Assign x
                                  Expr
                                    BinOp *
                                      Left
                                        Var x
                                      Right
                                        Var x
                            Expr
                              Var x
                        Ref: false
                        Recursive: false
                    Else
                      Num 200
                Else
                  Num 300
            Ref: false
            Is recursive: false
        Print
          Var x
      LetStmt i
        Type
          Typ int
        Expr
          Num 0
        Ref: false
        Is recursive: false
    LetStmt is_the_counter_up_to_date
      Type
        ArrowType
          From
            Typ int
          To
            Typ bool
      Expr
        Ftmlk
          Args
            counter
              Type
                Typ int
              Ref: false
          Body
            BinOp ==
              Left
                Var counter
              Right
                Num 0
      Ref: false
      Is recursive: false
  IfUnit
    Cond
      ESeq
        Stmt
          LetStmt h
            Type
              Typ bool
            Expr
              FtmlkApp
                Func
                  Var is_the_counter_up_to_date
                Args
                  Num 0
            Ref: false
            Is recursive: false
        Expr
          Var h
    Body
      Print
        Num 1
