open Value
open Datatype
open Imp
open Imp_helper
open Sugar

let table =
  [
    {
      imp_name = "list_destruct";
      imp_itps = [ List Int ];
      imp_otps = [ Int; List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l ] ->
            Sugar.(
              let* h, t = List.destruct_opt l in
              Some [ I h; IL t ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "list_last_destruct";
      imp_itps = [ List Int ];
      imp_otps = [ List Int; Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l ] ->
            Sugar.(
              let* t, h = List.last_destruct_opt l in
              Some [ IL t; I h ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "list_mid_partition";
      imp_itps = [ List Int ];
      imp_otps = [ List Int; List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l ] ->
            let l, r = List.mid_partition l in
            Some [ IL l; IL r ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "list_alter_partition";
      imp_itps = [ List Int ];
      imp_otps = [ List Int; List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l ] ->
            let l, r = List.alter_partition l in
            Some [ IL l; IL r ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "insert";
      imp_itps = [ List Int; Int; Int ];
      imp_otps = [ List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l; I idx; I elem ] ->
            let rec aux l n =
              if n == 0 then elem :: l
              else match l with [] -> [ elem ] | h :: t -> h :: aux t (n - 1)
            in
            Some [ IL (aux l idx) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "replace";
      imp_itps = [ List Int; Int; Int ];
      imp_otps = [ List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l; I idx; I elem ] ->
            let* x = List.replace_opt l idx elem in
            Some [ IL x ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "swap";
      imp_itps = [ List Int; Int; Int ];
      imp_otps = [ List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l; I idx; I idx' ] ->
            Sugar.(
              let* x = List.swap_opt l idx idx' in
              Some [ IL x ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "cons";
      imp_itps = [ Int; List Int ];
      imp_otps = [ List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ I elem; IL l ] -> Some [ IL (elem :: l) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "liblazy";
      imp_itps = [ List Int ];
      imp_otps = [ List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l ] -> Some [ IL l ] | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "libforce";
      imp_itps = [ List Int ];
      imp_otps = [ List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l ] -> Some [ IL l ] | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "concat";
      imp_itps = [ List Int; List Int ];
      imp_otps = [ List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL a; IL b ] -> Some [ IL (a @ b) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "cons_rev";
      imp_itps = [ List Int ];
      imp_otps = [ Int; List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL [] ] -> None
        | [ IL (h :: t) ] -> Some [ I h; IL t ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "nil";
      imp_itps = [];
      imp_otps = [ List Int ];
      nondet = false;
      imp_exec =
        (function [] -> Some [ IL [] ] | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "nil_rev";
      imp_itps = [ List Int ];
      imp_otps = [];
      nondet = false;
      imp_exec =
        (function
        | [ IL [] ] -> Some []
        | [ IL (_ :: _) ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "append";
      imp_itps = [ List Int; Int ];
      imp_otps = [ List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l; I elem ] -> Some [ IL (l @ [ elem ]) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "rev";
      imp_itps = [ List Int ];
      imp_otps = [ List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l ] -> Some [ IL (List.rev l) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "top";
      imp_itps = [ List Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL [] ] -> None
        | [ IL (h :: _) ] -> Some [ I h ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "list_single";
      imp_itps = [ Int ];
      imp_otps = [ List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ I x ] -> Some [ IL [ x ] ] | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "list_head";
      imp_itps = [ List Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL [] ] -> None
        | [ IL (h :: _) ] -> Some [ I h ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "bottom";
      imp_itps = [ List Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL [] ] -> None
        | [ IL l ] -> Some [ I (List.last l) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "list_last";
      imp_itps = [ List Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL [] ] -> None
        | [ IL l ] -> Some [ I (List.last l) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "list_len";
      imp_itps = [ List Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l ] -> Some [ I (List.length l) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "max";
      imp_itps = [ List Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l ] ->
            Sugar.(
              let* x = IntList.max_opt l in
              Some [ I x ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "min";
      imp_itps = [ List Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l ] ->
            let* x = IntList.min_opt l in
            Some [ I x ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "list_upper_bound";
      imp_itps = [ List Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l ] ->
            let* x = IntList.max_opt l in
            Some [ I (x + 1) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "list_lower_bound";
      imp_itps = [ List Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l ] ->
            let* x = IntList.min_opt l in
            Some [ I (x - 1) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "is_empty";
      imp_itps = [ List Int ];
      imp_otps = [ Bool ];
      nondet = false;
      imp_exec =
        (function
        | [ IL [] ] -> Some [ B true ]
        | [ IL _ ] -> Some [ B false ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tail";
      imp_itps = [ List Int ];
      imp_otps = [ List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL [] ] -> Some [ IL [] ]
        | [ IL (_ :: t) ] -> Some [ IL t ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "list_tail";
      imp_itps = [ List Int ];
      imp_otps = [ List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL [] ] -> None
        | [ IL (_ :: t) ] -> Some [ IL t ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "list_id";
      imp_itps = [ List Int ];
      imp_otps = [ List Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IL l ] -> Some [ IL l ] | _ -> raise @@ exn __FILE__ __LINE__);
    };
  ]
