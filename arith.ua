Seq
  Seq
    Seq
      Seq
        Seq
          Seq
            Seq
              Seq
                Seq
                  Seq
                    Seq
                      Seq
                        Seq
                          LetStmt a1
                            Type
                              Typ int
                            Expr
                              BinOp +
                                Left
                                  Num 3
                                Right
                                  BinOp *
                                    Left
                                      Num 4
                                    Right
                                      Num 5
                            Ref: false
                            Is recursive: false
                          LetStmt a2
                            Type
                              Typ int
                            Expr
                              BinOp -
                                Left
                                  Num 10
                                Right
                                  BinOp /
                                    Left
                                      Num 6
                                    Right
                                      Num 2
                            Ref: false
                            Is recursive: false
                        LetStmt a3
                          Type
                            Typ int
                          Expr
                            BinOp *
                              Left
                                BinOp +
                                  Left
                                    Num 3
                                  Right
                                    Num 4
                              Right
                                Num 5
                          Ref: false
                          Is recursive: false
                      LetStmt a4
                        Type
                          Typ int
                        Expr
                          BinOp -
                            Left
                              Num 10
                            Right
                              BinOp /
                                Left
                                  Num 6
                                Right
                                  Num 2
                        Ref: false
                        Is recursive: false
                    LetStmt a5
                      Type
                        Typ int
                      Expr
                        BinOp +
                          Left
                            BinOp *
                              Left
                                Num 2
                              Right
                                Num 3
                          Right
                            BinOp *
                              Left
                                Num 4
                              Right
                                Num 5
                      Ref: false
                      Is recursive: false
                  LetStmt a6
                    Type
                      Typ int
                    Expr
                      BinOp /
                        Left
                          BinOp /
                            Left
                              Num 20
                            Right
                              Num 4
                        Right
                          Num 5
                    Ref: false
                    Is recursive: false
                LetStmt a7
                  Type
                    Typ int
                  Expr
                    BinOp %
                      Left
                        BinOp *
                          Left
                            Num 3
                          Right
                            Num 4
                      Right
                        Num 5
                  Ref: false
                  Is recursive: false
              Print
                Var a1
            Print
              Var a2
          Print
            Var a3
        Print
          Var a4
      Print
        Var a5
    Print
      Var a6
  Print
    Var a7
