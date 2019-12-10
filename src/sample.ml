open Sqlite3

let mydb = db_open "test.db"

let create_table_sql = "CREATE TABLE contacts ( \
                          contact_id INTEGER PRIMARY KEY, \
                          first_name TEXT NOT NULL, \
                          last_name TEXT NOT NULL, \
                          email text NOT NULL UNIQUE, \
                          phone text NOT NULL UNIQUE \
                        );"

let db = db_open "test.db"

let () =
  match exec db create_table_sql with
  | Rc.OK -> print_endline "Ok"
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)

  let protect str : string =
    let pattern = "'" in
    let re = Str.regexp pattern in
    Str.global_replace re "''" str

let main () =
  let input = "toto'ta'ta" in
  print_endline (protect input)
  (* print_endline Jsontoflat.storage;
  print_endline Jsontoflat.storage_type;
  Jsontoflat.flatten_storage Jsontoflat.storage_type Jsontoflat.storage *)

let _ = main ()
