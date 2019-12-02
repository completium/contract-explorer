open Yojson
open Safe

let storage = "{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"int\":\"808\"},{\"prim\":\"False\"}]},{\"prim\":\"Pair\",\"args\":[{\"bytes\":\"ab\"},{\"int\":\"1\"}]}]},{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[[{\"int\":\"20\"},{\"int\":\"21\"}],[{\"prim\":\"Elt\",\"args\":[{\"int\":\"30\"},{\"int\":\"30\"}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"31\"},{\"int\":\"31\"}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"32\"},{\"int\":\"32\"}]}]]},{\"prim\":\"Pair\",\"args\":[[{\"prim\":\"Elt\",\"args\":[{\"int\":\"40\"},[{\"int\":\"400\"},{\"int\":\"401\"},{\"int\":\"402\"}]]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"50\"},[{\"int\":\"500\"},{\"int\":\"501\"},{\"int\":\"502\"}]]}],[{\"prim\":\"Elt\",\"args\":[{\"int\":\"60\"},[{\"prim\":\"Elt\",\"args\":[{\"int\":\"70\"},{\"int\":\"80\"}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"71\"},{\"int\":\"81\"}]}]]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"61\"},[{\"prim\":\"Elt\",\"args\":[{\"int\":\"77\"},{\"int\":\"88\"}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"99\"},{\"int\":\"101\"}]}]]}]]}]}]},{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[[{\"prim\":\"Elt\",\"args\":[{\"int\":\"1001\"},{\"prim\":\"Pair\",\"args\":[{\"int\":\"1002\"},{\"string\":\"myval0\"}]}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"1003\"},{\"prim\":\"Pair\",\"args\":[{\"int\":\"1004\"},{\"string\":\"myval1\"}]}]}],{\"int\":\"2\"}]},{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Some\",\"args\":[{\"int\":\"3\"}]},[{\"int\":\"777\"},{\"int\":\"888\"},{\"int\":\"999\"}]]}]},{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"string\":\"mystr\"},{\"prim\":\"Unit\"}]},{\"prim\":\"Pair\",\"args\":[{\"int\":\"4\"},{\"string\":\"myval\"}]}]}]}]}"
let storage_type = "{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"big_map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"int\"}],\"annots\":[\"%mbmap\"]},{\"prim\":\"bool\",\"annots\":[\"%mbool\"]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"bytes\",\"annots\":[\"%mbytes\"]},{\"prim\":\"int\",\"annots\":[\"%mint\"]}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"list\",\"args\":[{\"prim\":\"int\"}],\"annots\":[\"%mlist\"]},{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"int\"}],\"annots\":[\"%mmap\"]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"list\",\"args\":[{\"prim\":\"int\"}]}],\"annots\":[\"%mmaplist\"]},{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"int\"}]}],\"annots\":[\"%mmapmap\"]}]}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"map\",\"args\":[{\"prim\":\"int\"},{\"prim\":\"pair\",\"args\":[{\"prim\":\"int\",\"annots\":[\"%id\"]},{\"prim\":\"string\",\"annots\":[\"%value\"]}]}],\"annots\":[\"%mmapmyrecord\"]},{\"prim\":\"nat\",\"annots\":[\"%mnat\"]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"option\",\"args\":[{\"prim\":\"int\"}],\"annots\":[\"%moption\"]},{\"prim\":\"set\",\"args\":[{\"prim\":\"int\"}],\"annots\":[\"%mset\"]}]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"pair\",\"args\":[{\"prim\":\"string\",\"annots\":[\"%mstring\"]},{\"prim\":\"unit\",\"annots\":[\"%munit\"]}]},{\"prim\":\"pair\",\"args\":[{\"prim\":\"int\",\"annots\":[\"%id\"]},{\"prim\":\"string\",\"annots\":[\"%value\"]}],\"annots\":[\"%myr\"]}]}]}]}"


type ordered =
| Oint of int
| Onat of int
| Ostr of string
| Obytes of bytes
[@@deriving yojson, show {with_path = false}]

type storage_field =
| Sordered of ordered
| Sbool of bool
| Sunit
| Soption of storage_field option
| Sunion of (storage_field * storage_field)
| Scontainer of (ordered * storage_field) list
[@@deriving yojson, show {with_path = false}]

type storage = storage_field list
[@@deriving yojson, show {with_path = false}]

let main () =
  let storage = Safe.from_string storage in
  let _storage_type = Safe.from_string storage_type in
  print_endline (Safe.to_string storage)

let _ = main ()
