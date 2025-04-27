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
                          LetStmt b1
                            Type
                              Typ int
                            Expr
                              BinOp |
                                Left
                                  Num 5
                                Right
                                  BinOp &
                                    Left
                                      Num 3
                                    Right
                                      Num 2
                            Ref: false
                            Is recursive: false
                          LetStmt b2
                            Type
                              Typ int
                            Expr
                              BinOp |
                                Left
                                  BinOp &
                                    Left
                                      Num 5
                                    Right
                                      Num 3
                                Right
                                  Num 2
                            Ref: false
                            Is recursive: false
                        LetStmt b3
                          Type
                            Typ int
                          Expr
                            BinOp |
                              Left
                                Num 5
                              Right
                                BinOp ^
                                  Left
                                    Num 3
                                  Right
                                    Num 2
                          Ref: false
                          Is recursive: false
                      LetStmt b4
                        Type
                          Typ int
                        Expr
                          BinOp |
                            Left
                              BinOp <<
                                Left
                                  Num 1
                                Right
                                  Num 2
                            Right
                              Num 3
                        Ref: false
                        Is recursive: false
                    LetStmt b5
                      Type
                        Typ int
                      Expr
                        BinOp &
                          Left
                            BinOp >>
                              Left
                                Num 12
                              Right
                                Num 1
                          Right
                            Num 3
                      Ref: false
                      Is recursive: false
                  LetStmt b6
                    Type
                      Typ int
                    Expr
                      BinOp |
                        Left
                          Num 1
                        Right
                          BinOp <<
                            Left
                              Num 2
                            Right
                              Num 1
                    Ref: false
                    Is recursive: false
                LetStmt b7
                  Type
                    Typ int
                  Expr
                    BinOp <<
                      Left
                        BinOp |
                          Left
                            Num 1
                          Right
                            Num 2
                      Right
                        Num 1
                  Ref: false
                  Is recursive: false
              Print
                Var b1
            Print
              Var b2
          Print
            Var b3
        Print
          Var b4
      Print
        Var b5
    Print
      Var b6
  Print
    Var b7
