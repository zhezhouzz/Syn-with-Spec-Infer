open Value
open Datatype
open Imp
open Imp_helper
open Sugar

let table =
  [
    {
      imp_name = "plus";
      imp_itps = [ Int; Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ I elem; I elem' ] -> Some [ I (elem + elem') ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "plus1";
      imp_itps = [ Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ I elem ] -> Some [ I (elem + 1) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "minus";
      imp_itps = [ Int; Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ I elem; I elem' ] -> Some [ I (elem - elem') ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "minus1";
      imp_itps = [ Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ I elem ] -> Some [ I (elem - 1) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "neg";
      imp_itps = [ Bool ];
      imp_otps = [ Bool ];
      nondet = false;
      imp_exec =
        (function
        | [ B v ] -> Some [ B (not v) ] | _ -> raise @@ exn __FILE__ __LINE__);
    };
  ]
  @ List.map
      (fun b ->
        {
          imp_name = spf "const%b" b;
          imp_itps = [];
          imp_otps = [ Bool ];
          nondet = false;
          imp_exec =
            (function
            | [] -> Some [ B b ] | _ -> raise @@ exn __FILE__ __LINE__);
        })
      [ true; false ]
  @ List.init 4 (fun i ->
        {
          imp_name = spf "const%i" i;
          imp_itps = [];
          imp_otps = [ Int ];
          nondet = false;
          imp_exec =
            (function
            | [] -> Some [ I i ] | _ -> raise @@ exn __FILE__ __LINE__);
        })
