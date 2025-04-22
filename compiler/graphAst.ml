
module AstNode = struct
  type t = Var of string 
  | AstStmt of Ast.ast_stmt
  | AstExpr of Ast.ast_expr
  | AstType of Types.t
  | AstBinOp of Ast.ast_binop
  let compare _ _ = 0

  let hash _ = 0
end

module AstNodeS = struct
  type t = AstNode.t
  include Regular.Std.Opaque.Make (AstNode)
end

module T = Graphlib.Std.Graphlib.Make (AstNodeS) (Core_kernel.Bool)

