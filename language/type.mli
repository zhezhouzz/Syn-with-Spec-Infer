include Ast.Type with type t = Ast.T.t

val compare : t -> t -> int

val eq : t -> t -> bool

val is_dt : t -> bool

val is_basic : t -> bool

val compare_tvar : tvar -> tvar -> int

val eq_tvar : tvar -> tvar -> bool

val fresh_from_type : t -> tvar

val layout : t -> string

val of_string : string -> t

val layout_l : t list -> string

val subst : (string, t) Hashtbl.t -> t -> t option
