Seq
  Seq
    Seq
      Seq
        Seq
          Seq
            Seq
              TypeDecl point
                RecordType
                  Field x
                    Typ int
                  Field y
                    Typ int
              TypeDecl triangle
                RecordType
                  Field first
                    Typ point
                  Field second
                    Typ point
                  Field third
                    Typ point
            LetStmt origin
              Type
                Typ point
              Expr
                RecordExp
                  Field x
                    Expr
                      Num 0
                  Field y
                    Expr
                      Num 0
              Ref: false
              Is recursive: false
          LetStmt A
            Type
              Typ point
            Expr
              RecordExp
                Field x
                  Expr
                    Num 0
                Field y
                  Expr
                    Num 1
            Ref: false
            Is recursive: false
        LetStmt B
          Type
            Typ point
          Expr
            RecordExp
              Field x
                Expr
                  Num 1
              Field y
                Expr
                  Num 0
          Ref: false
          Is recursive: false
      LetStmt unit_triangle
        Type
          Typ triangle
        Expr
          RecordExp
            Field first
              Expr
                Var origin
            Field second
              Expr
                Var A
            Field third
              Expr
                Var B
        Ref: false
        Is recursive: false
    LetStmt area
      Type
        ArrowType
          From
            Typ triangle
          To
            Typ int
      Expr
        Ftmlk
          Args
            t
              Type
                Typ triangle
              Ref: false
          Body
            BinOp +
              Left
                MemberOf 
                  Expr
                    MemberOf 
                      Expr
                        Var t
                     field: first
                 field: x
              Right
                MemberOf 
                  Expr
                    MemberOf 
                      Expr
                        Var t
                     field: second
                 field: y
      Ref: false
      Is recursive: false
  Print
    FtmlkApp
      Func
        Var area
      Args
        Var unit_triangle
