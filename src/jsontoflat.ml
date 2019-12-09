open Yojson
open Safe
open Util

let storage = "{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"int\":\"808\"},{\"prim\":\"False\"}]},{\"prim\":\"Pair\",\"args\":[{\"bytes\":\"ab\"},{\"int\":\"1\"}]}]},{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[[{\"int\":\"20\"},{\"int\":\"21\"}],[{\"prim\":\"Elt\",\"args\":[{\"int\":\"30\"},{\"int\":\"30\"}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"31\"},{\"int\":\"31\"}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"32\"},{\"int\":\"32\"}]}]]},{\"prim\":\"Pair\",\"args\":[[{\"prim\":\"Elt\",\"args\":[{\"int\":\"40\"},[{\"int\":\"400\"},{\"int\":\"401\"},{\"int\":\"402\"}]]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"50\"},[{\"int\":\"500\"},{\"int\":\"501\"},{\"int\":\"502\"}]]}],[{\"prim\":\"Elt\",\"args\":[{\"int\":\"60\"},[{\"prim\":\"Elt\",\"args\":[{\"int\":\"70\"},{\"int\":\"80\"}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"71\"},{\"int\":\"81\"}]}]]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"61\"},[{\"prim\":\"Elt\",\"args\":[{\"int\":\"77\"},{\"int\":\"88\"}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"99\"},{\"int\":\"101\"}]}]]}]]}]}]},{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[[{\"prim\":\"Elt\",\"args\":[{\"int\":\"1001\"},{\"prim\":\"Pair\",\"args\":[{\"int\":\"1002\"},{\"string\":\"myval0\"}]}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"1003\"},{\"prim\":\"Pair\",\"args\":[{\"int\":\"1004\"},{\"string\":\"myval1\"}]}]}],{\"int\":\"2\"}]},{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Some\",\"args\":[{\"int\":\"3\"}]},[{\"int\":\"777\"},{\"int\":\"888\"},{\"int\":\"999\"}]]}]},{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"string\":\"mystr\"},{\"prim\":\"Unit\"}]},{\"prim\":\"Pair\",\"args\":[{\"int\":\"4\"},{\"string\":\"myval\"}]}]}]}]}"
let storage_type = "{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"big_map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"int\"}],\"annots\":[\"%mbmap\"]},{\"prim\":\"bool\",\"annots\":[\"%mbool\"]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"bytes\",\"annots\":[\"%mbytes\"]},{\"prim\":\"int\",\"annots\":[\"%mint\"]}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"list\",\"args\":[{\"prim\":\"int\"}],\"annots\":[\"%mlist\"]},{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"int\"}],\"annots\":[\"%mmap\"]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"list\",\"args\":[{\"prim\":\"int\"}]}],\"annots\":[\"%mmaplist\"]},{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"int\"}]}],\"annots\":[\"%mmapmap\"]}]}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"pair\",\"args\":[{\"prim\":\"int\",\"annots\":[\"%id\"]},{\"prim\":\"string\",\"annots\":[\"%value\"]}]}],\"annots\":[\"%mmapmyrecord\"]},{\"prim\":\"nat\",\"annots\":[\"%mnat\"]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"option\",\"args\":[{\"prim\":\"int\"}],\"annots\":[\"%moption\"]},{\"prim\":\"set\",\"args\":[{\"prim\":\"int\"}],\"annots\":[\"%mset\"]}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"string\",\"annots\":[\"%mstring\"]},{\"prim\":\"unit\",\"annots\":[\"%munit\"]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"int\",\"annots\":[\"%id\"]},{\"prim\":\"string\",\"annots\":[\"%value\"]}],\"annots\":[\"%myr\"]}]}]}]}"
let storage_type2 = "{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"big_map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"int\"}]},{\"prim\":\"bool\"}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"bytes\"},{\"prim\":\"int\"}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"list\",\"args\":[{\"prim\":\"int\"}]},{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"int\"}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"list\",\"args\":[{\"prim\":\"int\"}]}]},{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"int\"}]}]}]}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"pair\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"string\"}]}]},{\"prim\":\"nat\"}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"option\",\"args\":[{\"prim\":\"int\"}]},{\"prim\":\"set\",\"args\":[{\"prim\":\"int\"}]}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"string\"},{\"prim\":\"unit\"}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"string\"}]}]}]}]}"

let param_type = "{\"prim\": \"or\",\"args\": [{\"prim\": \"int\",\"annots\": [\"%aaa\"]},{\"prim\": \"string\",\"annots\": [\"%bbb\"]}]}"
let param_val  = "{\"prim\": \"Left\",\"args\": [{\"int\": \"1\"}]}"
let param_val2  = "{\"prim\": \"Right\",\"args\": [{\"string\": \"toto\"}]}"
(* Michelson value ----------------------------------------------------------*)

type ordered =
| Mint of int
| Mnat of int
| Mstr of string
| Mbytes of bytes
| Maddress of string
| Mkey of string
| Mkeyhash of string
| Mmutez of int
| Mtimestamp of string
[@@deriving yojson, show {with_path = false}]

type mvalue =
| Mordered of ordered
| Mbool of bool
| Munit
| Mcontract of mvalue
| Moption of mvalue option
| Mpair of (mvalue * mvalue)
| Mleft of mvalue
| Mright of mvalue
| Melt of (mvalue * mvalue)
| Munion of (mvalue * mvalue)
| Mlist of mvalue list
| MLambda of mvalue list
[@@deriving yojson, show {with_path = false}]

(* Michelson type -----------------------------------------------------------*)

type ordered_mtype =
| Tint
| Tnat
| Tstr
| Tbytes
| Taddress
| Tkey
| Tkeyhash
| Tmutez
| Ttimestamp
[@@deriving yojson, show {with_path = false}]

type mtype =
| Tpair of amtype * amtype
| Tunion of amtype * amtype
| Tordered of ordered_mtype
| Tcontract of mtype
| Toption of mtype
| Tbool
| Tunit
| Tlist of mtype
| Tmap of ordered_mtype * mtype
| Tbigmap of ordered_mtype * mtype
| Tset of ordered_mtype
| Tlambda of mtype * mtype
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
| Tunion (t1,t2) -> "UNION of ("^(amtype_to_string t1)^") to ("^(amtype_to_string t2)^")"
| Tordered Tint -> "INT"
| Tordered Tnat -> "NAT"
| Tordered Tstr -> "STR"
| Tordered Taddress -> "ADDRESS"
| Tordered Tkey -> "KEY"
| Tordered Tkeyhash -> "KEYHASH"
| Tordered Tmutez -> "MUTEZ"
| Tordered Ttimestamp -> "TIMESTAMP"
| Tordered Tbytes -> "BYTES"
| Toption t -> "OPTION of ("^(mtype_to_string t)^")"
| Tbool -> "BOOL"
| Tunit -> "UNIT"
| Tcontract t -> "CONTRACT of ("^(mtype_to_string t)^")"
| Tlist t -> "LIST of ("^(mtype_to_string t)^")"
| Tmap (t1,t2) -> "MAP of ("^(mtype_to_string (Tordered t1))^") to ("^(mtype_to_string t2)^")"
| Tbigmap (t1,t2) -> "BIG_MAP of ("^(mtype_to_string (Tordered t1))^") to ("^(mtype_to_string t2)^")"
| Tset t -> "SET of ("^(mtype_to_string (Tordered t))^")"
| Tlambda (t1,t2) -> "LAMBDA of ("^(mtype_to_string t1)^" -> "^(mtype_to_string t2)^")"
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
| Faddress
| Fkey
| Fkeyhash
| Fmutez
| Ftimestamp
| Fcontract of sftype
| Foption of sftype
| Flist of sftype
| Fset of sftype
| Fmap of sftype * sftype
| Fbigmap of sftype * sftype
| Frecord of (string * sftype) list
| For of (string * sftype) list
| Flambda of (string * sftype) list
[@@deriving yojson, show {with_path = false}]

type sfval =
| Vunit
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
| Fkey -> "key"
| Fkeyhash -> "keyhash"
| Fmutez -> "mutez"
| Ftimestamp -> "timestamp"
| Fcontract t -> with_paren wp ("contract of "^(sftype_to_string true t))
| Faddress -> "address"
| Funit -> "unit"
| Foption t -> with_paren wp ("option of "^(sftype_to_string true t))
| Flist t -> with_paren wp ("list of "^(sftype_to_string true t))
| Fmap (t1,t2) -> with_paren wp ("map of "^(sftype_to_string true t1)^" to "^(sftype_to_string true t2))
| Fbigmap (t1,t2) -> with_paren wp ("big map of "^(sftype_to_string true t1)^" to "^(sftype_to_string true t2))
| Fset t -> with_paren wp ("set of "^(sftype_to_string true t))
| Frecord l -> with_paren wp ("record { "^(String.concat "; "(List.map (fun (s,t) -> s^" : "^(sftype_to_string false t)) l))^" }")
| For l -> with_paren wp (String.concat " or "(List.map (fun (s,t) -> with_paren true (s^" : "^(sftype_to_string false t))) l))
| Flambda l -> with_paren wp (String.concat " -> "(List.map (fun (s,t) -> with_paren true (s^" : "^(sftype_to_string false t))) l))

let pp_sftype fmt t = pp_str fmt (sftype_to_string false t)

exception ExpectedPair

let is_big_map = function Fbigmap _ -> true | _ -> false

let rec pp_sfval t fmt = function
| Vunit -> pp_str fmt "unit"
| Velt e when (not (is_big_map t)) -> pp_str fmt e
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

let rec pp_sfval_json fmt = function
| Vunit -> Format.fprintf fmt "{\"val\":\"unit\"}"
| Velt s -> Format.fprintf fmt "{\"val\":\"%a\"}" pp_str s
| Vpair (v1, v2) ->
    Format.fprintf fmt "{\"val\":\"pair\",\"args\":[%a,%a]}"
    pp_sfval_json v1
    pp_sfval_json v2
| Vlist l ->
    Format.fprintf fmt "{\"val\":\"list\",\"args\":[%a]}"
    (pp_list "," pp_sfval_json) l

let rec pp_sftype_json fmt = function
| Fint -> Format.fprintf fmt "{ \"val\":\"int\"}"
| Fnat -> Format.fprintf fmt "{ \"val\":\"nat\"}"
| Fbytes -> Format.fprintf fmt "{\"val\":\"bytes\"}"
| Fstr -> Format.fprintf fmt "{\"val\":\"str\"}"
| Faddress -> Format.fprintf fmt "{\"val\":\"addr\"}"
| Fkey -> Format.fprintf fmt "{\"val\":\"key\"}"
| Fkeyhash -> Format.fprintf fmt "{\"val\":\"keyh\"}"
| Fmutez -> Format.fprintf fmt "{\"val\":\"mutez\"}"
| Funit -> Format.fprintf fmt "{\"val\":\"unit\"}"
| Ftimestamp -> Format.fprintf fmt "{\"val\":\"time\"}"
| Fcontract t ->
    Format.fprintf fmt "{\"val\":\"contract\",\"args\":[%a]}"
    pp_sftype_json t
| Fbool -> Format.fprintf fmt "{\"val\":\"bool\"}"
| Foption t ->
    Format.fprintf fmt "{\"val\":\"option\",\"args\":[%a]}"
    pp_sftype_json t
| Flist t ->
    Format.fprintf fmt "{\"val\":\"list\",\"args\":[%a]}"
    pp_sftype_json t
| Fset t ->
    Format.fprintf fmt "{\"val\":\"set\",\"args\":[%a]}"
    pp_sftype_json t
| Fmap (t1,t2) ->
    Format.fprintf fmt "{\"val\":\"map\",\"args\":[%a,%a]}"
    pp_sftype_json t1
    pp_sftype_json t2
| Fbigmap (t1,t2) ->
    Format.fprintf fmt "{\"val\":\"bigmap\",\"args\":[%a,%a]}"
    pp_sftype_json t1
    pp_sftype_json t2
| Frecord l ->
    Format.fprintf fmt "{\"val\":\"record\",\"args\":[%a]}"
    (pp_list "," pp_named_sftype_json) l
| For l ->
    Format.fprintf fmt "{\"val\":\"or\",\"args\":[%a]}"
    (pp_list "," pp_named_sftype_json) l
| Flambda l ->
    Format.fprintf fmt "{\"val\":\"lambda\",\"args\":[%a]}"
    (pp_list "," pp_named_sftype_json) l
and pp_named_sftype_json fmt (s,t) =
    Format.fprintf fmt "{\"val\":\"%a\",\"args\":[%a]}"
    pp_str s
    pp_sftype_json t

let pp_val_json fmt (_s,_t,v) =
    (* Format.fprintf fmt "\"name\":\"%a\",\"type\":%a,\"value\":%a" *)
    Format.fprintf fmt "%a"
    pp_sfval_json v

let pp_typ_json fmt (s,t,_v) =
    (* Format.fprintf fmt "\"name\":\"%a\",\"type\":%a,\"value\":%a" *)
    Format.fprintf fmt "\"name\":\"%a\",\"type\":%a"
    pp_str s
    pp_sftype_json t

let pp_sfield_json fmt (s,t,v) =
    Format.fprintf fmt "\"name\":\"%a\",\"type\":%a,\"value\":%a"
    pp_str s
    pp_sftype_json t
    pp_sfval_json v

let pp_vals_json fmt st =
    Format.fprintf fmt "[%a@\n]"
    (pp_list "},{" pp_val_json) st

let pp_typs_json fmt st =
    Format.fprintf fmt "[{%a}@\n]"
    (pp_list "},{" pp_typ_json) st

let pp_storage_json fmt st =
    Format.fprintf fmt "[{%a}@\n]"
    (pp_list "},{" pp_sfield_json) st

(* Conversions --------------------------------------------------------------*)

exception ExpectedNbargs of string * int
exception ExpectedOrdered of string
exception InvalidPrim of string

let amtype_to_ordered : amtype -> ordered_mtype = function
| _, Tordered t -> t
| _ as t -> raise (ExpectedOrdered (amtype_to_string t))

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
            | _ -> raise (ExpectedNbargs ("pair",2)) end
        | "or" -> begin
            match json |> member "args" |> to_list with
            | arg1 :: arg2 :: [] -> (get_annot keys json, Tunion (json_to_mtype arg1, json_to_mtype arg2))
            | _ -> raise (ExpectedNbargs ("or",2)) end
        | "big_map" -> begin
            match json |> member "args" |> to_list with
            | arg1 :: arg2 :: [] ->
                (get_annot keys json, Tbigmap (
                    amtype_to_ordered (json_to_mtype arg1),
                    snd (json_to_mtype arg2)))
            | _ -> raise (ExpectedNbargs ("big_map",2)) end
        | "map" -> begin
            match json |> member "args" |> to_list with
            | arg1 :: arg2 :: [] ->
                (get_annot keys json, Tmap (
                    amtype_to_ordered (json_to_mtype arg1),
                    snd (json_to_mtype arg2)))
            | _ -> raise (ExpectedNbargs ("map",2)) end
        | "list" -> begin
            match json |> member "args" |> to_list with
            | arg :: [] -> (get_annot keys json, Tlist (snd (json_to_mtype arg)))
            | _ -> raise (ExpectedNbargs ("list",1)) end
        | "set" -> begin
            match json |> member "args" |> to_list with
            | arg :: [] -> (get_annot keys json, Tset (amtype_to_ordered (json_to_mtype arg)))
            | _ -> raise (ExpectedNbargs ("set",1)) end
        | "bool" -> (get_annot keys json, Tbool)
        | "int" -> (get_annot keys json, Tordered Tint)
        | "nat" -> (get_annot keys json, Tordered Tnat)
        | "string" -> (get_annot keys json, Tordered Tstr)
        | "bytes" -> (get_annot keys json, Tordered Tbytes)
        | "option" -> begin
            match json |> member "args" |> to_list with
            | arg :: [] -> (get_annot keys json, Toption (snd (json_to_mtype arg)))
            | _ -> raise (ExpectedNbargs ("option",1)) end
        | "unit" -> (get_annot keys json, Tunit)
        | "key" -> (get_annot keys json, Tordered Tkey)
        | "key_hash" -> (get_annot keys json, Tordered Tkeyhash)
        | "mutez" -> (get_annot keys json, Tordered Tkeyhash)
        | "timestamp" -> (get_annot keys json, Tordered Ttimestamp)
        | "address" -> (get_annot keys json, Tordered Taddress)
        | "lambda" ->  begin
            match json |> member "args" |> to_list with
            | arg1 :: arg2 :: [] -> (get_annot keys json, Tlambda (snd (json_to_mtype arg1), snd (json_to_mtype arg2)))
            | _ -> raise (ExpectedNbargs ("lambda",2)) end
        | "contract" -> begin match json |> member "args" |> to_list with
            | arg :: [] -> (get_annot keys json, Tcontract (snd (json_to_mtype arg)))
            | _ -> raise (ExpectedNbargs ("list",1)) end
        | _ as p -> raise (InvalidPrim p)
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
            | _ -> raise (ExpectedNbargs ("Pair",2)) end
        | "Left" -> begin
            match json |> member "args" |> to_list with
            | arg :: [] -> Mleft (json_to_mvalue arg)
            | _ -> raise (ExpectedNbargs ("Left",1)) end
        | "Right" -> begin
            match json |> member "args" |> to_list with
            | arg :: [] -> Mright (json_to_mvalue arg)
            | _ -> raise (ExpectedNbargs ("Right",1)) end
        | "Elt" -> begin
            match json |> member "args" |> to_list with
            | arg1 :: arg2 :: [] -> Melt (json_to_mvalue arg1, json_to_mvalue arg2)
            | _ -> raise (ExpectedNbargs ("Elt",2)) end
        | "Unit" -> Munit
        | "False" -> Mbool false
        | "True" -> Mbool true
        | "Some" -> begin
            match json |> member "args" |> to_list with
            | arg :: [] -> Moption (Some (json_to_mvalue arg))
            | _ -> raise (ExpectedNbargs ("Some",1)) end
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

let rec fold_left_right lacc racc value : mvalue list =
match value with
| Mleft v -> fold_left_right lacc (racc@[Munit]) v
| Mright v -> fold_left_right (lacc@[Munit]) racc v
| _ as v -> lacc @ [v] @ racc

let rec  fold_pair acc typ : amtype list =
match typ with
| (_,Tpair (t1,t2)) -> fold_pair (fold_pair acc t1) t2
| _ as t -> acc @ [t]

let rec  fold_union acc typ : amtype list =
match typ with
| (_,Tunion (t1,t2)) -> fold_union (fold_union acc t1) t2
| _ as t -> acc @ [t]

let rec  fold_type_none acc typ : amtype list =
match typ with
| (None,Tpair (t1,t2)) -> fold_type_none (fold_type_none acc t1) t2
| _ as t -> acc @ [t]

let rec fold_type_value acc stype svalue : (amtype*mvalue) list =
match stype, svalue with
| (None,Tpair (t1,t2)), Mpair (v1,v2) -> fold_type_value (fold_type_value acc t1 v1) t2 v2
| t,v -> acc @ [t,v]

let lbl_to_str i = function None -> "f"^(string_of_int i) | Some l -> l

let rec mval_to_sval = function
| Munit -> Vunit
| Mordered (Mint i) -> Velt (string_of_int i)
| Mordered (Mnat n) -> Velt (string_of_int n)
| Mordered (Mbytes b) -> Velt (Bytes.to_string b)
| Mordered (Mstr s) -> Velt s
| Moption (Some v) -> Vlist [mval_to_sval v]
| Mordered (Maddress s) -> Velt s
| Mordered (Mkey s) -> Velt s
| Mordered (Mkeyhash s) -> Velt s
| Mordered (Mmutez i) -> Velt (string_of_int i)
| Mordered (Mtimestamp s) -> Velt s
| MLambda l -> Vlist (List.map mval_to_sval l)
| Moption None -> Velt ""
| Mbool b -> Velt (string_of_bool b)
| Mlist l -> Vlist (List.map (fun v ->
    match v with
    | Melt (v1,v2) -> Vpair (mval_to_sval v1, mval_to_sval v2)
    | _ -> mval_to_sval v) l)
| Mpair _ as v ->
    let lv = List.map mval_to_sval (fold_value [] v) in
    Vlist lv
| Mleft _ as v ->
    let lv = List.map mval_to_sval (fold_left_right [] [] v) in
    Vlist lv
| Mright _ as v ->
    let lv = List.map mval_to_sval (fold_left_right [] [] v) in
    Vlist lv
| _ -> Velt ""

let rec mtyp_to_styp = function
| Tordered Tint -> Fint
| Tordered Tnat -> Fnat
| Tordered Tbytes -> Fbytes
| Tordered Tstr -> Fstr
| Tbool -> Fbool
| Tordered Taddress -> Faddress
| Tordered Tkey -> Fkey
| Tordered Tkeyhash -> Fkeyhash
| Tordered Tmutez -> Fmutez
| Tordered Ttimestamp -> Ftimestamp
| Tunit -> Funit
| Tcontract t -> Fcontract (mtyp_to_styp t)
| Tlambda (t1,t2) -> Flambda (["a", mtyp_to_styp t1;"r",mtyp_to_styp t2])
| Toption t -> Foption (mtyp_to_styp t)
| Tlist t -> Flist (mtyp_to_styp t)
| Tmap (t1,t2) -> Fmap (mtyp_to_styp (Tordered t1), mtyp_to_styp t2)
| Tbigmap (t1,t2) -> Fbigmap (mtyp_to_styp (Tordered t1), mtyp_to_styp t2)
| Tpair _ as t ->
  let lt = fold_pair [] (None,t) in
  let ln = List.mapi (fun i (s,t) ->
      match s with
      | None -> ("r"^(string_of_int (i+1)),t)
      | Some s -> (s,t)) lt in
  Frecord (List.map (fun (s,t) -> s, mtyp_to_styp t) ln)
| Tunion _ as t ->
  let lt = fold_union [] (None,t) in
  let ln = List.mapi (fun i (s,t) ->
      match s with
      | None -> ("r"^(string_of_int (i+1)),t)
      | Some s -> (s,t)) lt in
  For (List.map (fun (s,t) -> s, mtyp_to_styp t) ln)
| Tset t -> Fset (mtyp_to_styp (Tordered t))


let mk_storage stype svalue : storage =
    let leafs = fold_type_value [] stype svalue in
    List.mapi (fun i ((lbl,t),v) ->
        let st = mtyp_to_styp t in
        let sv = mval_to_sval v in
        (lbl_to_str i lbl, st, sv)
    ) leafs

let mk_storage_typ stype : storage =
    let leafs = fold_type_none [] stype in
    List.mapi (fun i (lbl,t) ->
        let st = mtyp_to_styp t in
        (lbl_to_str i lbl, st, Velt "")
    ) leafs


(*---------------------------------------------------------------------------*)

let flatten_storage typ storage =
  let storage = Safe.from_string storage in
  let storage_type = Safe.from_string typ in
  let svalue = json_to_mvalue storage in
  let stype = json_to_mtype storage_type in
  let storage = mk_storage stype svalue in
  Format.asprintf "%a" pp_vals_json storage

let flatten_typ typ =
  let storage_type = Safe.from_string typ in
  let stype = json_to_mtype storage_type in
  let storage = mk_storage_typ stype in
  Format.asprintf "%a" pp_typs_json storage

(* let _ =
  let param = Safe.from_string param_val2 in
  let param_type = Safe.from_string param_type in
  print_endline (Safe.to_string param);
  print_endline (Safe.to_string param_type);
  let param = json_to_mvalue param in
  let param_type = json_to_mtype param_type in
  Format.fprintf Format.std_formatter "%a@\n" pp_mvalue param;
  Format.fprintf Format.std_formatter "%a@\n" pp_amtype param_type;
  let storage = mk_storage param_type param in
  Format.fprintf Format.std_formatter "%a@\n" pp_storage storage;
  Format.fprintf Format.std_formatter "%a" pp_st storage
 *)