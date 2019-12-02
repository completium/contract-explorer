open Yojson
open Safe

let storage = "{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"int\":\"808\"},{\"prim\":\"False\"}]},{\"prim\":\"Pair\",\"args\":[{\"bytes\":\"ab\"},{\"int\":\"1\"}]}]},{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[[{\"int\":\"20\"},{\"int\":\"21\"}],[{\"prim\":\"Elt\",\"args\":[{\"int\":\"30\"},{\"int\":\"30\"}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"31\"},{\"int\":\"31\"}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"32\"},{\"int\":\"32\"}]}]]},{\"prim\":\"Pair\",\"args\":[[{\"prim\":\"Elt\",\"args\":[{\"int\":\"40\"},[{\"int\":\"400\"},{\"int\":\"401\"},{\"int\":\"402\"}]]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"50\"},[{\"int\":\"500\"},{\"int\":\"501\"},{\"int\":\"502\"}]]}],[{\"prim\":\"Elt\",\"args\":[{\"int\":\"60\"},[{\"prim\":\"Elt\",\"args\":[{\"int\":\"70\"},{\"int\":\"80\"}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"71\"},{\"int\":\"81\"}]}]]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"61\"},[{\"prim\":\"Elt\",\"args\":[{\"int\":\"77\"},{\"int\":\"88\"}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"99\"},{\"int\":\"101\"}]}]]}]]}]}]},{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[[{\"prim\":\"Elt\",\"args\":[{\"int\":\"1001\"},{\"prim\":\"Pair\",\"args\":[{\"int\":\"1002\"},{\"string\":\"myval0\"}]}]},{\"prim\":\"Elt\",\"args\":[{\"int\":\"1003\"},{\"prim\":\"Pair\",\"args\":[{\"int\":\"1004\"},{\"string\":\"myval1\"}]}]}],{\"int\":\"2\"}]},{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Some\",\"args\":[{\"int\":\"3\"}]},[{\"int\":\"777\"},{\"int\":\"888\"},{\"int\":\"999\"}]]}]},{\"prim\":\"Pair\",\"args\":[{\"prim\":\"Pair\",\"args\":[{\"string\":\"mystr\"},{\"prim\":\"Unit\"}]},{\"prim\":\"Pair\",\"args\":[{\"int\":\"4\"},{\"string\":\"myval\"}]}]}]}]}"

let main () =
  let json = Safe.from_string storage in
  print_endline (Safe.to_string json)

let _ = main ()
