type size = SmallNat | SizeBound of int

type element = SmallUnsign | LowUpper of int * int

type tree_fq = { fq_leaf : int; fq_node : int }

type t = {
  int_conf : element;
  list_conf : element * size;
  tree_conf : element * size * tree_fq;
  treei_conf : element * size * tree_fq;
  treeb_conf : element * size * tree_fq;
  binomialhp_conf : element * size;
  pairinghp_conf : element * size;
  physicistsq_conf : element * size;
  realtimeq_conf : element * size;
  skewhp_conf : element * size;
}

val qcconf : t option ref

val load_qc_config : string -> unit
