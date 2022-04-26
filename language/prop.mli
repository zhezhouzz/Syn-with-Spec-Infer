include Ast.Prop with type t = Ast.P.t

val layout : t -> string

val pretty_layout : t -> string

val subst : (string -> string) -> t -> t

val is_op : string -> bool
