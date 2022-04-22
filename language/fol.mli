type uf = Type.tvar list * Prop.t

type spec = {
  name : string;
  args : Type.tvar list;
  qv : Type.tvar list;
  body : Prop.t;
}
