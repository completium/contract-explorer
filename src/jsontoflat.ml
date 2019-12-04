open Yojson
open Safe
open Util

let storage = "{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"int\":\"808\"},{\"prim\":\"False\"}]},{\"prim\":\"Pair\",\"args\":[{\"bytes\":\"ab\"},{\"int\":\"1\"}]}]},{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[[{\"int\":\"20\"},{\"int\":\"21\"}],[{\"prim\":\"Elt\",\"args\":[{\"int\":\"30\"},{\"int\":\"30\"}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"31\"},{\"int\":\"31\"}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"32\"},{\"int\":\"32\"}]}]]},{\"prim\":\"Pair\",\"args\":[[{\"prim\":\"Elt\",\"args\":[{\"int\":\"40\"},[{\"int\":\"400\"},{\"int\":\"401\"},{\"int\":\"402\"}]]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"50\"},[{\"int\":\"500\"},{\"int\":\"501\"},{\"int\":\"502\"}]]}],[{\"prim\":\"Elt\",\"args\":[{\"int\":\"60\"},[{\"prim\":\"Elt\",\"args\":[{\"int\":\"70\"},{\"int\":\"80\"}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"71\"},{\"int\":\"81\"}]}]]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"61\"},[{\"prim\":\"Elt\",\"args\":[{\"int\":\"77\"},{\"int\":\"88\"}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"99\"},{\"int\":\"101\"}]}]]}]]}]}]},{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[[{\"prim\":\"Elt\",\"args\":[{\"int\":\"1001\"},{\"prim\":\"Pair\",\"args\":[{\"int\":\"1002\"},{\"string\":\"myval0\"}]}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"1003\"},{\"prim\":\"Pair\",\"args\":[{\"int\":\"1004\"},{\"string\":\"myval1\"}]}]}],{\"int\":\"2\"}]},{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Some\",\"args\":[{\"int\":\"3\"}]},[{\"int\":\"777\"},{\"int\":\"888\"},{\"int\":\"999\"}]]}]},{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"string\":\"mystr\"},{\"prim\":\"Unit\"}]},{\"prim\":\"Pair\",\"args\":[{\"int\":\"4\"},{\"string\":\"myval\"}]}]}]}]}"
let storage_type = "{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"big_map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"int\"}],\"annots\":[\"%mbmap\"]},{\"prim\":\"bool\",\"annots\":[\"%mbool\"]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"bytes\",\"annots\":[\"%mbytes\"]},{\"prim\":\"int\",\"annots\":[\"%mint\"]}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"list\",\"args\":[{\"prim\":\"int\"}],\"annots\":[\"%mlist\"]},{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"int\"}],\"annots\":[\"%mmap\"]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"list\",\"args\":[{\"prim\":\"int\"}]}],\"annots\":[\"%mmaplist\"]},{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"int\"}]}],\"annots\":[\"%mmapmap\"]}]}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"pair\",\"args\":[{\"prim\":\"int\",\"annots\":[\"%id\"]},{\"prim\":\"string\",\"annots\":[\"%value\"]}]}],\"annots\":[\"%mmapmyrecord\"]},{\"prim\":\"nat\",\"annots\":[\"%mnat\"]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"option\",\"args\":[{\"prim\":\"int\"}],\"annots\":[\"%moption\"]},{\"prim\":\"set\",\"args\":[{\"prim\":\"int\"}],\"annots\":[\"%mset\"]}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"string\",\"annots\":[\"%mstring\"]},{\"prim\":\"unit\",\"annots\":[\"%munit\"]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"int\",\"annots\":[\"%id\"]},{\"prim\":\"string\",\"annots\":[\"%value\"]}],\"annots\":[\"%myr\"]}]}]}]}"
let storage_type2 = "{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"big_map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"int\"}]},{\"prim\":\"bool\"}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"bytes\"},{\"prim\":\"int\"}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"list\",\"args\":[{\"prim\":\"int\"}]},{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"int\"}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"list\",\"args\":[{\"prim\":\"int\"}]}]},{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"int\"}]}]}]}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"pair\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"string\"}]}]},{\"prim\":\"nat\"}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"option\",\"args\":[{\"prim\":\"int\"}]},{\"prim\":\"set\",\"args\":[{\"prim\":\"int\"}]}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"string\"},{\"prim\":\"unit\"}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"string\"}]}]}]}]}"
(* Michelson value ----------------------------------------------------------*)

type ordered =
| Mint of int
| Mnat of int
| Mstr of string
| Mbytes of bytes
[@@deriving yojson, show {with_path = false}]

type mvalue =
| Mordered of ordered
| Mbool of bool
| Munit
| Moption of mvalue option
| Mpair of (mvalue * mvalue)
| Melt of (mvalue * mvalue)
| Munion of (mvalue * mvalue)
| Mlist of mvalue list
[@@deriving yojson, show {with_path = false}]

(* Michelson type -----------------------------------------------------------*)

type ordered_mtype =
| Tint
| Tnat
| Tstr
| Tbytes
[@@deriving yojson, show {with_path = false}]

type mtype =
| Tpair of amtype * amtype
| Tunion of amtype * amtype
| Tordered of ordered_mtype
| Toption of mtype
| Tbool
| Tunit
| Tlist of mtype
| Tmap of ordered_mtype * mtype
| Tbigmap of ordered_mtype * mtype
| Tset of ordered_mtype
[@@deriving yojson, show {with_path = false}]
and amtype = string option * mtype
[@@deriving yojson, show {with_path = false}]

let rec fold_amtype acc (amt : amtype) =
match amt with
| None,Tpair (t1,t2) -> fold_amtype (fold_amtype acc t1) t2
| _ as t -> acc @ [t]

(* for debug purpose *)
let rec mtype_to_string = function
| Tpair (a1,a2) -> "PAIR of ("^(amtype_to_string a1)^") and ("^(amtype_to_string a2)^")"
| Tordered Tint -> "INT"
| Tordered Tnat -> "NAT"
| Tordered Tstr -> "STR"
| Tordered Tbytes -> "BYTES"
| Toption t -> "OPTION of ("^(mtype_to_string t)^")"
| Tbool -> "BOOL"
| Tunit -> "UNIT"
| Tlist t -> "LIST of ("^(mtype_to_string t)^")"
| Tmap (t1,t2) -> "MAP of ("^(mtype_to_string (Tordered t1))^") to ("^(mtype_to_string t2)^")"
| Tbigmap (t1,t2) -> "BIG_MAP of ("^(mtype_to_string (Tordered t1))^") to ("^(mtype_to_string t2)^")"
| Tset t -> "SET of ("^(mtype_to_string (Tordered t))^")"
| _ as t -> Format.printf "%a" pp_mtype t;raise Not_found
and amtype_to_string amt =
let types = fold_amtype [] amt in
(String.concat "\n" (List.mapi (fun i t ->
    match t with
    | Some l,t -> l^" : "^(mtype_to_string t)
    | None, t -> "field"^(string_of_int i)^" : "^(mtype_to_string t)
) types))

(* flat storage type --------------------------------------------------------*)

type sftype =
| Fint
| Fnat
| Fstr
| Fbytes
| Fbool
| Funit
| Foption of sftype
| Flist of sftype
| Fset of sftype
| Frecord of (string * sftype) list
| Fmap of sftype * sftype
[@@deriving yojson, show {with_path = false}]

type sfval =
| Velt of string
| Vpair of sfval * sfval
| Vlist of sfval list
[@@deriving yojson, show {with_path = false}]
and sfield = string * sftype * sfval
[@@deriving yojson, show {with_path = false}]

type storage = sfield list
[@@deriving yojson, show {with_path = false}]

let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_list sep pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
    pp

let with_paren wp s = if wp then "("^s^")" else s

let rec sftype_to_string wp = function
| Fint -> "int"
| Fnat -> "nat"
| Fbytes -> "bytes"
| Fstr -> "string"
| Fbool -> "bool"
| Foption t -> with_paren wp ("option of "^(sftype_to_string true t))
| Flist t -> with_paren wp ("list of "^(sftype_to_string true t))
| Fmap (t1,t2) -> with_paren wp ("map of "^(sftype_to_string true t1)^" to "^(sftype_to_string true t2))
| Frecord l -> with_paren wp ("record { "^(String.concat "; "(List.map (fun (s,t) -> s^" : "^(sftype_to_string false t)) l))^" }")
| Fset t -> with_paren wp ("set of "^(sftype_to_string true t))
| _ -> "unit"

let pp_sftype fmt t = pp_str fmt (sftype_to_string false t)

exception ExpectedPair

let rec pp_sfval t fmt = function
| Velt e -> pp_str fmt e
| Vlist l -> begin
    match t with
    | Foption t ->
        if List.length l > 0 then
            Format.fprintf fmt "Some %a"
            (pp_sfval t) (List.nth l 0)
        else pp_str fmt "None"
    | Flist t ->
        Format.fprintf fmt "[@\n  @[%a@]@\n]"
        (pp_list ";@\n" (pp_sfval t)) l
    | Fmap (t1,t2) ->
        Format.fprintf fmt "[@\n  @[%a@]@\n]"
        (pp_list ";@\n" (pp_pair t1 t2)) l
    | Frecord _ ->
        Format.fprintf fmt "{ @[%a@] }"
        (pp_list "; " (pp_sfval Fint)) l
    | Fset t ->
        Format.fprintf fmt "{ @[%a@] }"
        (pp_list "; " (pp_sfval t)) l
    | _ ->
        Format.fprintf fmt "%a"
        (pp_list ";" (pp_sfval t)) l
    end
| _ -> pp_str fmt ""
and pp_pair t1 t2 fmt = function
| Vpair (v1,v2) ->
    Format.fprintf fmt "%a \x1B[92m->\x1B[39m @[%a@]"
    (pp_sfval t1) v1
    (pp_sfval t2) v2
| _ -> raise ExpectedPair

let pp_sfield fmt (s,t,v) =
    Format.fprintf fmt "\x1B[94m%a\x1B[39m : \x1B[93m%a\x1B[39m := %a"
    pp_str s
    pp_sftype t
    (pp_sfval t) v

let pp_st fmt st =
    Format.fprintf fmt "Storage = {@\n  @[%a@]@\n}@\n"
    (pp_list ";@\n" pp_sfield) st

(* Conversions --------------------------------------------------------------*)

exception ExpectedNbargs of int
exception ExpectedOrdered
exception InvalidPrim

let amtype_to_ordered : amtype -> ordered_mtype = function
| _, Tordered t -> t
| _ -> raise ExpectedOrdered

let get_annot keys json =
if List.mem "annots" keys then
    Some (json  |> member "annots" |> to_list
                |> fun l -> List.nth l 0
                |> to_string
                |> fun s -> String.sub s 1 (String.length s - 1))
else None

let rec json_to_mtype (json : Safe.t) : amtype =
    let keys= json |> keys in
    if List.mem "prim" keys then
        let prim = json |> member "prim" |> to_string in
        match prim with
        | "pair" -> begin
            match json |> member "args" |> to_list with
            | arg1 :: arg2 :: [] -> (get_annot keys json, Tpair (json_to_mtype arg1, json_to_mtype arg2))
            | _ -> raise (ExpectedNbargs 2) end
        | "big_map" -> begin
            match json |> member "args" |> to_list with
            | arg1 :: arg2 :: [] ->
                (get_annot keys json, Tbigmap (
                    amtype_to_ordered (json_to_mtype arg1),
                    snd (json_to_mtype arg2)))
            | _ -> raise (ExpectedNbargs 2) end
        | "map" -> begin
            match json |> member "args" |> to_list with
            | arg1 :: arg2 :: [] ->
                (get_annot keys json, Tmap (
                    amtype_to_ordered (json_to_mtype arg1),
                    snd (json_to_mtype arg2)))
            | _ -> raise (ExpectedNbargs 2) end
        | "list" -> begin
            match json |> member "args" |> to_list with
            | arg :: [] -> (get_annot keys json, Tlist (snd (json_to_mtype arg)))
            | _ -> raise (ExpectedNbargs 1) end
        | "set" -> begin
            match json |> member "args" |> to_list with
            | arg :: [] -> (get_annot keys json, Tset (amtype_to_ordered (json_to_mtype arg)))
            | _ -> raise (ExpectedNbargs 1) end
        | "bool" -> (get_annot keys json, Tbool)
        | "int" -> (get_annot keys json, Tordered Tint)
        | "nat" -> (get_annot keys json, Tordered Tnat)
        | "string" -> (get_annot keys json, Tordered Tstr)
        | "bytes" -> (get_annot keys json, Tordered Tbytes)
        | "option" -> begin
            match json |> member "args" |> to_list with
            | arg :: [] -> (get_annot keys json, Toption (snd (json_to_mtype arg)))
            | _ -> raise (ExpectedNbargs 1) end
        | "unit" -> (get_annot keys json, Tunit)
        | _ -> raise InvalidPrim
    else raise Not_found

let rec json_to_mvalue json : mvalue =
    try
        let l = to_list json in
        Mlist (List.map json_to_mvalue l)
    with _ ->
    let keys= json |> keys in
    if List.mem "prim" keys then
        let prim = json |> member "prim" |> to_string in
        match prim with
        | "Pair" -> begin
            match json |> member "args" |> to_list with
            | arg1 :: arg2 :: [] -> Mpair (json_to_mvalue arg1, json_to_mvalue arg2)
            | _ -> raise (ExpectedNbargs 2) end
        | "Elt" -> begin
            match json |> member "args" |> to_list with
            | arg1 :: arg2 :: [] -> Melt (json_to_mvalue arg1, json_to_mvalue arg2)
            | _ -> raise (ExpectedNbargs 2) end
        | "Unit" -> Munit
        | "False" -> Mbool false
        | "True" -> Mbool true
        | "Some" -> begin
            match json |> member "args" |> to_list with
            | arg :: [] -> Moption (Some (json_to_mvalue arg))
            | _ -> raise (ExpectedNbargs 1) end
        | "None" -> Moption None
        | _ -> Munit
    else if List.mem "int" keys then
        let i = json |> member "int" |> to_string |> int_of_string in
        Mordered (Mint i)
    else if List.mem "string" keys then
        let s = json |> member "string" |> to_string in
        Mordered (Mstr s)
    else if List.mem "bytes" keys then
        let s = json |> member "bytes" |> to_string |> Bytes.unsafe_of_string in
        Mordered (Mbytes s)
    else raise Not_found

(* Mk flat storage-----------------------------------------------------------*)

let rec  fold_value acc value : mvalue list =
match value with
| Mpair (v1,v2) -> fold_value (fold_value acc v1) v2
| _ as v -> acc @ [v]

let rec  fold_type acc typ : amtype list =
match typ with
| (_,Tpair (t1,t2)) -> fold_type (fold_type acc t1) t2
| _ as t -> acc @ [t]

let rec fold_type_value acc stype svalue : (amtype*mvalue) list =
match stype, svalue with
| (None,Tpair (t1,t2)), Mpair (v1,v2) -> fold_type_value (fold_type_value acc t1 v1) t2 v2
| t,v -> acc @ [t,v]

let lbl_to_str i = function None -> "f"^(string_of_int i) | Some l -> l

let rec mval_to_sval = function
| Mordered (Mint i) -> Velt (string_of_int i)
| Mordered (Mnat n) -> Velt (string_of_int n)
| Mordered (Mbytes b) -> Velt (Bytes.to_string b)
| Mordered (Mstr s) -> Velt s
| Moption (Some v) -> Vlist [mval_to_sval v]
| Moption None -> Velt ""
| Mbool b -> Velt (string_of_bool b)
| Mlist l -> Vlist (List.map (fun v ->
    match v with
    | Melt (v1,v2) -> Vpair (mval_to_sval v1, mval_to_sval v2)
    | _ -> mval_to_sval v) l)
| Mpair _ as v ->
    let lv = List.map mval_to_sval (fold_value [] v) in
    Vlist lv
| _ -> Velt ""

let rec mtyp_to_styp = function
| Tordered Tint -> Fint
| Tordered Tnat -> Fnat
| Tordered Tbytes -> Fbytes
| Tordered Tstr -> Fstr
| Tbool -> Fbool
| Toption t -> Foption (mtyp_to_styp t)
| Tlist t -> Flist (mtyp_to_styp t)
| Tmap (t1,t2) -> Fmap (mtyp_to_styp (Tordered t1), mtyp_to_styp t2)
| Tpair _ as t ->
    let lt = fold_type [] (None,t) in
    let ln = List.mapi (fun i (s,t) ->
        match s with
        | None -> ("r"^(string_of_int (i+1)),t)
        | Some s -> (s,t)) lt in
    Frecord (List.map (fun (s,t) -> s, mtyp_to_styp t) ln)
| Tset t -> Fset (mtyp_to_styp (Tordered t))
| _ -> Funit

let mk_storage stype svalue : storage =
    let leafs = fold_type_value [] stype svalue in
    List.mapi (fun i ((lbl,t),v) ->
        let st = mtyp_to_styp t in
        let sv = mval_to_sval v in
        (lbl_to_str i lbl, st, sv)
    ) leafs

(*---------------------------------------------------------------------------*)

let main () =
  let storage = Safe.from_string storage in
  let storage_type = Safe.from_string storage_type in
  let storage_type2 = Safe.from_string storage_type2 in
 (*  print_endline (Safe.to_string storage_type);
  print_endline "";
  Format.printf "%a" pp_amtype (json_to_mtype storage_type);
  print_endline ""; *)
  let svalue = json_to_mvalue storage in
  let stype = json_to_mtype storage_type in
  let storage = mk_storage stype svalue in
  (* print_endline (amtype_to_string stype);
  print_endline "";
  print_endline (amtype_to_string (json_to_mtype storage_type2));
  print_endline "";
  Format.printf "%a" pp_mvalue svalue;
  print_endline "";
  Format.printf "%a" pp_storage storage; *)
  print_endline "";
  pp_st Format.std_formatter storage

let _ = main ()
