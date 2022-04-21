type uf = Typedvar.t list * Prop.t

type spec = {
  name : string;
  args : Typedvar.t list;
  qv : Typedvar.t list;
  body : Prop.t;
}
