open Value
open Datatype
open Imp
open Imp_helper

let table =
  [
    (* tree lib *)
    {
      imp_name = "tree_leaf";
      imp_itps = [];
      imp_otps = [ Tree Int ];
      nondet = false;
      imp_exec =
        (function
        | [] -> Some [ IT Tree.Leaf ] | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_node";
      imp_itps = [ Int; Tree Int; Tree Int ];
      imp_otps = [ Tree Int ];
      nondet = false;
      imp_exec =
        (function
        | [ I a; IT b; IT c ] -> Some [ IT (Tree.Node (a, b, c)) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_node_elrond";
      imp_itps = [ Tree Int; Int; Tree Int ];
      imp_otps = [ Tree Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IT b; I a; IT c ] -> Some [ IT (Tree.Node (a, b, c)) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_leaf_rev";
      imp_itps = [ Tree Int ];
      imp_otps = [];
      nondet = false;
      imp_exec =
        (function
        | [ IT Tree.Leaf ] -> Some []
        | [ IT _ ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_node_rev";
      imp_itps = [ Tree Int ];
      imp_otps = [ Int; Tree Int; Tree Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IT (Tree.Node (a, b, c)) ] -> Some [ I a; IT b; IT c ]
        | [ IT _ ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    (* tree manipulation *)
    {
      imp_name = "tree_left_right_subtree";
      imp_itps = [ Tree Int ];
      imp_otps = [ Tree Int; Tree Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IT (Tree.Node (_, b, c)) ] -> Some [ IT b; IT c ]
        | [ IT Tree.Leaf ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_root";
      imp_itps = [ Tree Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IT (Tree.Node (a, _, _)) ] -> Some [ I a ]
        | [ IT Tree.Leaf ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_flip";
      imp_itps = [ Tree Int ];
      imp_otps = [ Tree Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IT tr ] -> Some [ IT (Tree.flip tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_rec_flip";
      imp_itps = [ Tree Int ];
      imp_otps = [ Tree Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IT tr ] -> Some [ IT (Tree.rec_flip tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_rotation_left";
      imp_itps = [ Tree Int ];
      imp_otps = [ Tree Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IT tr ] ->
            Sugar.(
              let* tr' = Tree.rotation_left_opt tr in
              Some [ IT tr' ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_rotation_right";
      imp_itps = [ Tree Int ];
      imp_otps = [ Tree Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IT tr ] ->
            Sugar.(
              let* tr' = Tree.rotation_right_opt tr in
              Some [ IT tr' ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_append_to_left_most";
      imp_itps = [ Int; Tree Int ];
      imp_otps = [ Tree Int ];
      nondet = false;
      imp_exec =
        (function
        | [ I x; IT tr ] -> Some [ IT (Tree.append_to_left_most x tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_append_to_right_most";
      imp_itps = [ Int; Tree Int ];
      imp_otps = [ Tree Int ];
      nondet = false;
      imp_exec =
        (function
        | [ I x; IT tr ] -> Some [ IT (Tree.append_to_right_most x tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_max";
      imp_itps = [ Tree Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IT tr ] ->
            Sugar.(
              let* e = Tree.max_opt Stdlib.compare tr in
              Some [ I e ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_min";
      imp_itps = [ Tree Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IT tr ] ->
            Sugar.(
              let* e = Tree.min_opt Stdlib.compare tr in
              Some [ I e ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_upper_bound";
      imp_itps = [ Tree Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IT tr ] -> (
            match Tree.max_opt Stdlib.compare tr with
            | None -> Some [ I 0 ]
            | Some m -> Some [ I (m + 1) ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_lower_bound";
      imp_itps = [ Tree Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IT tr ] -> (
            match Tree.min_opt Stdlib.compare tr with
            | None -> Some [ I 0 ]
            | Some m -> Some [ I (m - 1) ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_add_to_bottom_left";
      imp_itps = [ Int; Tree Int ];
      imp_otps = [ Tree Int ];
      nondet = false;
      imp_exec =
        (function
        | [ I x; IT tr ] -> Some [ IT (Tree.add_to_bottom_left x tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_add_to_bottom_right";
      imp_itps = [ Int; Tree Int ];
      imp_otps = [ Tree Int ];
      nondet = false;
      imp_exec =
        (function
        | [ I x; IT tr ] -> Some [ IT (Tree.add_to_bottom_right x tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_drop_bottom";
      imp_itps = [ Tree Int ];
      imp_otps = [ Tree Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IT tr ] -> Some [ IT (Tree.drop_bottom tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_destruct";
      imp_itps = [ Tree Int ];
      imp_otps = [ Int; Tree Int; Tree Int ];
      nondet = false;
      imp_exec =
        (function
        | [ IT tr ] ->
            Sugar.(
              let* x, a, b = Tree.destruct_opt tr in
              Some [ I x; IT a; IT b ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_node_single";
      imp_itps = [ Int ];
      imp_otps = [ Tree Int ];
      nondet = false;
      imp_exec =
        (function
        | [ I i ] -> Some [ IT (Node (i, Leaf, Leaf)) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
  ]
