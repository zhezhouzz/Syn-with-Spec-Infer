type uf = Typeast.tvar list * Propast.t

type spec = {
  name : string;
  args : Typeast.tvar list;
  qv : Typeast.tvar list;
  body : Propast.t;
}
