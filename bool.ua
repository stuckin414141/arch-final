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
                          Seq
                            Seq
                              Seq
                                LetStmt int_to_bool
                                  Type
                                    ArrowType
                                      From
                                        Typ bool
                                      To
                                        Typ int
                                  Expr
                                    Ftmlk
                                      Args
                                        b
                                          Type
                                            Typ bool
                                          Ref: false
                                      Body
                                        If
                                          Cond
                                            Var b
                                          Then
                                            Num 1
                                          Else
                                            Num 0
                                  Ref: false
                                  Is recursive: false
                                LetStmt c1
                                  Type
                                    Typ bool
                                  Expr
                                    BinOp ||
                                      Left
                                        BinOp &&
                                          Left
                                            Bool true
                                          Right
                                            Bool false
                                      Right
                                        Bool true
                                  Ref: false
                                  Is recursive: false
                              LetStmt c2
                                Type
                                  Typ bool
                                Expr
                                  BinOp ||
                                    Left
                                      Bool true
                                    Right
                                      BinOp &&
                                        Left
                                          Bool false
                                        Right
                                          Bool true
                                Ref: false
                                Is recursive: false
                            LetStmt c3
                              Type
                                Typ bool
                              Expr
                                BinOp ||
                                  Left
                                    BinOp &&
                                      Left
                                        Bool false
                                      Right
                                        Bool true
                                  Right
                                    Bool true
                              Ref: false
                              Is recursive: false
                          LetStmt c4
                            Type
                              Typ bool
                            Expr
                              BinOp ||
                                Left
                                  Bool false
                                Right
                                  BinOp &&
                                    Left
                                      Bool true
                                    Right
                                      Bool false
                            Ref: false
                            Is recursive: false
                        LetStmt c5
                          Type
                            Typ bool
                          Expr
                            BinOp &&
                              Left
                                BinOp <
                                  Left
                                    Num 3
                                  Right
                                    Num 5
                              Right
                                BinOp >
                                  Left
                                    Num 10
                                  Right
                                    Num 5
                          Ref: false
                          Is recursive: false
                      LetStmt c6
                        Type
                          Typ bool
                        Expr
                          BinOp ||
                            Left
                              BinOp <
                                Left
                                  Num 3
                                Right
                                  Num 5
                            Right
                              BinOp >
                                Left
                                  Num 2
                                Right
                                  Num 5
                        Ref: false
                        Is recursive: false
                    LetStmt c7
                      Type
                        Typ bool
                      Expr
                        BinOp &&
                          Left
                            BinOp ==
                              Left
                                Num 3
                              Right
                                Num 3
                          Right
                            BinOp !=
                              Left
                                Num 4
                              Right
                                Num 5
                      Ref: false
                      Is recursive: false
                  LetStmt c8
                    Type
                      Typ bool
                    Expr
                      BinOp ||
                        Left
                          BinOp &&
                            Left
                              Bool true
                            Right
                              BinOp <
                                Left
                                  Num 5
                                Right
                                  Num 3
                        Right
                          BinOp <
                            Left
                              Num 2
                            Right
                              Num 5
                    Ref: false
                    Is recursive: false
                Print
                  Var c1
              Print
                Var c2
            Print
              Var c3
          Print
            Var c4
        Print
          Var c5
      Print
        Var c6
    Print
      Var c7
  Print
    Var c8
