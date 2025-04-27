Seq
  Seq
    Seq
      Seq
        Seq
          Seq
            Seq
              TypeDecl Point
                RecordType
                  Field x
                    Typ int
                  Field y
                    Typ int
              LetStmt p1
                Type
                  Typ Point
                Expr
                  RecordExp
                    Field x
                      Expr
                        Num 5
                    Field y
                      Expr
                        Num 10
                Ref: false
                Is recursive: false
            LetStmt p2
              Type
                Typ Point
              Expr
                RecordExp
                  Field x
                    Expr
                      Num 3
                  Field y
                    Expr
                      Num 7
              Ref: false
              Is recursive: false
          Print
            BinOp +
              Left
                MemberOf 
                  Expr
                    Var p1
                 field: x
              Right
                MemberOf 
                  Expr
                    Var p2
                 field: y
        TypeDecl Person
          RecordType
            Field age
              Typ int
            Field active
              Typ bool
            Field employed
              Typ bool
            Field gender
              Typ int
            Field income
              Typ int
      LetStmt employee
        Type
          Typ Person
        Expr
          RecordExp
            Field age
              Expr
                Num 30
            Field active
              Expr
                Bool true
            Field employed
              Expr
                Bool false
            Field gender
              Expr
                Num 40000
            Field income
              Expr
                Num 9
        Ref: false
        Is recursive: false
    Print
      MemberOf 
        Expr
          Var employee
       field: gender
  IfUnit
    Cond
      MemberOf 
        Expr
          Var employee
       field: active
    Body
      Print
        Num 1
