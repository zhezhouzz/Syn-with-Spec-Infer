type debug_flag = Debug of { logc : Core.Out_channel.t } | Opt

type size = SmallNat | SizeBound of int

type element = SmallUnsign | LowUpper of int * int

type tree_fq = { fq_leaf : int; fq_node : int }

type quick_check_config = {
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

type config = {
  debug_flag : debug_flag;
  z3_ctx : Z3.context option;
  qc: quick_check_config option;
}
