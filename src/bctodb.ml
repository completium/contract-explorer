open Yojson
open Safe
open Util
open Sqlite3

let version = "0.1"

(* Options ------------------------------------------------------------------*)

type command =
  | Sync
  | AddContract of string
  | FillStorageFlat
  | None
[@@deriving show {with_path = false}]

module MOptions = struct
  let address = ref "localhost"
  let getAddress _ = !address
  let setAddress s = address := s

  let port = ref "8732"
  let getPort _ = !port
  let setPort s = port := s

  let branch = ref "main"
  let getBranch _ = !branch
  let setBranch s = branch := s

  let path = ref "."
  let getPath _ = !path
  let setPath s = path := s

  let cmd : command ref = ref None

end


(* Tools --------------------------------------------------------------------*)

module type RPC = sig
  exception RPCNotFound of string * (Curly.Error.t)
  val url_to_string : string -> string
  val url_to_json : string -> Safe.t
end
module Rpc : RPC = struct

  exception RPCNotFound of string * (Curly.Error.t)

  let url_to_string url =
    match Curly.(run (Request.make ~url:url ~meth:`GET ())) with
    | Ok x -> x.Curly.Response.body
    | Error e -> raise (RPCNotFound(url,e))

  let url_to_json url = Safe.from_string (url_to_string url)

end

module type BCinfo = sig
  val getIp     : unit -> string
  val getPort   : unit -> string
  val getBranch : unit -> string
end

(* BC types -----------------------------------------------------------------*)

type block_id = {
  hash         : string;
  previous     : string;
}
[@@deriving yojson, show {with_path = false}]

type op = {
  hash : string;
  timestamp : string;
  source : string;
  destination : string;
  parameters : string;
}
[@@deriving yojson, show {with_path = false}]

type block_data = {
  timestamp    : string;
  operations   : op list;
}
[@@deriving yojson, show {with_path = false}]

type contract_info = {
  id : string;
  storage_type : string;
  storage_type_flat : string;
  entries : string list;
}
[@@deriving yojson, show {with_path = false}]

type storage = {
  timestamp : string;
  hash : string;
  storage : string;
  balance : string;
}
[@@deriving yojson, show {with_path = false}]

let cmp_contracts c1 c2 =
  (c1.storage = c2.storage) &&
  (c1.balance = c2.balance)

(* factories ----------------------------------------------------------------*)

module type Url = sig
  val getBlock    : string -> string
  val getHead     : unit   -> string
  val getContract : string -> string -> string
end

module type Block = sig
  val mk_id   : string -> block_id
  val mk_data : string -> block_data
end

module type Contract = sig
  val mk : string -> string -> string -> storage option
  val mk_data : string -> contract_info
end

(* output module ------------------------------------------------------------*)

module type Db = sig
  val clear : unit -> unit
  val open_logs : unit -> unit
  val close_logs : unit -> unit
  val write_contract_info : contract_info -> unit
  val write_storage : string -> storage -> unit
  val write_op : string -> op -> unit
  val get_storage_type : string -> string
  val get_storage_id_values_from_contract_id : string -> (string * string) list
  val write_storage_flat : (string * string) list -> unit
  val write_head : string -> string -> unit
  val get_contract_ids : unit -> string list
  val get_contract_ids_head : unit -> (string * string option) list
end

module Make_Db : Db = struct

  let db = db_open "bc.db"
  let table_info     = "contracts_info"
  let table_storages = "storages"
  let table_ops      = "operations"

  let exec_cmd cmd =
    match exec db cmd with
    | Rc.OK -> ()
    | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)

  let drop_tables table =
    let drop_table_sql = "DROP TABLE IF EXISTS " ^ table ^ ";" in
    exec_cmd drop_table_sql

  let clear () =
    drop_tables table_storages;
    drop_tables table_ops

  let create_table_info () =
    let create_table_sql =
      Printf.sprintf "CREATE TABLE IF NOT EXISTS %s ( \
                      id VARCHAR(37) PRIMARY KEY, \
                      storage_type text NOT NULL, \
                      storage_type_flat text, \
                      entries text NOT NULL, \
                      head text \
                      );"

        table_info
    in
    exec_cmd create_table_sql

  let create_table_storages () =
    let create_table_sql =
      Printf.sprintf "CREATE TABLE IF NOT EXISTS %s ( \
                      hash VARCHAR(52) PRIMARY KEY, \
                      contract_id VARCHAR(37) NOT NULL, \
                      timestamp date NOT NULL, \
                      storage text NOT NULL, \
                      storage_flat text, \
                      balance text NOT NULL \
                      );"

        table_storages
    in
    exec_cmd create_table_sql

  let create_table_ops () =
    let create_table_sql =
      Printf.sprintf "CREATE TABLE IF NOT EXISTS %s ( \
                      hash VARCHAR(52) PRIMARY KEY, \
                      contract_id VARCHAR(37) NOT NULL, \
                      timestamp date NOT NULL, \
                      source VARCHAR(37) NOT NULL, \
                      destination VARCHAR(37) NOT NULL, \
                      parameters text \
                      );"

        table_ops
    in
    exec_cmd create_table_sql

  let create_tables () =
    create_table_info ();
    create_table_storages ();
    create_table_ops ()

  let open_logs () =
    create_tables ()

  let close_logs () =
    ()

  let write_contract_info (c : contract_info) =
    let insert : string =
      Printf.sprintf "INSERT OR REPLACE INTO %s VALUES('%s', '%s', '%s', '%s', NULL);"
        table_info
        c.id
        c.storage_type
        c.storage_type_flat
        (List.fold_left (fun (accu : string) (x : string) -> accu ^ " " ^ x) "" c.entries)
    in
    exec_cmd insert

  let write_storage contract_id (s : storage) =
    let insert : string =
      Printf.sprintf "INSERT INTO %s VALUES('%s', '%s', '%s', '%s', NULL, '%s');"
        table_storages
        s.hash
        contract_id
        s.timestamp
        s.storage
        s.balance
    in
    exec_cmd insert

  let write_op contract_id (op : op) =
    let insert : string =
      Printf.sprintf "INSERT INTO %s VALUES('%s', '%s', '%s', '%s', '%s', '%s');"
        table_ops
        op.hash
        contract_id
        op.timestamp
        op.source
        op.destination
        op.parameters
    in
    exec_cmd insert

  let get_contract_ids () : string list =
    let select_sql = Printf.sprintf "SELECT id FROM %s;" table_info in
    let l = ref [] in
    match exec db select_sql ~cb:(fun row _ ->
        match row.(0) with
        | Some a -> l := a::!l
        | _ -> ()
      ) with
    | Rc.OK -> !l
    | _ -> assert false

  let get_contract_ids_head () : (string * string option) list =
    let select_sql = Printf.sprintf "SELECT id, head FROM %s;" table_info in
    let l : (string * string option) list ref = ref [] in
    match exec db select_sql ~cb:(fun row _ ->
        match row.(0), row.(1) with
        | Some a, b -> l := (a, b)::!l
        | _ -> ()
      ) with
    | Rc.OK -> !l
    | _ -> assert false

  let get_storage_type contract_id =
    let select_sql = Printf.sprintf "SELECT storage_type FROM %s WHERE id = '%s';" table_info contract_id in
    let str = ref "" in
    match exec db select_sql ~cb:(fun row _ ->
        match row.(0) with
        | Some a -> str := a
        | _ -> ()
      ) with
    | Rc.OK -> !str
    | _ -> assert false

  let get_storage_id_values_from_contract_id contract_id : (string * string) list =
    let select_sql = Printf.sprintf "SELECT hash, storage FROM %s WHERE contract_id = '%s';" table_storages contract_id in
    let l = ref [] in
    match exec db select_sql ~cb:(fun row _ ->
        match row.(0), row.(1) with
        | Some a, Some b -> l := (a, b)::!l
        | _ -> ()
      ) with
    | Rc.OK -> !l
    | _ -> assert false

  let write_head head contract_id =
    let insert : string =
      Printf.sprintf "UPDATE %s SET head = '%s' WHERE id = '%s';"
        table_info
        head
        contract_id
    in
    exec_cmd insert

  let write_storage_flat l =
    List.iter (fun (id, value) ->
        begin
          let insert : string =
            Printf.sprintf "UPDATE %s SET storage_flat = '%s' WHERE hash = '%s';"
              table_storages
              value
              id
          in
          exec_cmd insert
        end
      ) l

end

(* Tezos --------------------------------------------------------------------*)

module Make_TzURL (Info : BCinfo) : Url = struct
  let getBlock str = (Info.getIp()^":"^Info.getPort()^"/chains/"^Info.getBranch()^"/blocks/"^str)
  let getHead () = getBlock "head"
  let getContract b c = (getBlock b)^"/context/contracts/"^c
end

module Make_TzBlock (Url : Url) (Rpc : RPC) : Block = struct

  let mk_id hash =
    let json = Rpc.url_to_json (Url.getBlock hash) in {
      hash         = json |> member "hash" |> to_string;
      previous     = json |> member "header" |> member "predecessor" |> to_string;
    }

  let json_to_ops timestamp json : op list =
    json |> member "operations" |> to_list |> List.fold_left (fun acc ops ->
        ops |> to_list |> List.fold_left (fun acc op ->
            let hash = op |> member "hash" |> to_string in
            op |> member "contents" |> to_list |> List.fold_left (fun acc c ->
                let kind = c |> member "kind" |> to_string in
                match kind with
                | "transaction" -> acc @ [{
                    hash = hash;
                    timestamp = timestamp;
                    source = c |> member "source" |> to_string;
                    destination = c |> member "destination" |> to_string;
                    parameters = c |> member "parameters" |> Safe.to_string;
                  }]
                | _ -> acc
              ) acc
          ) acc
      ) []

  let mk_data hash =
    let json = Rpc.url_to_json (Url.getBlock hash) in
    let timestamp = json |> member "header" |> member "timestamp" |> to_string in {
      timestamp = timestamp;
      operations = json_to_ops timestamp json;
    }

end

module Make_TzContract (Url : Url) (Block : Block) (Rpc : RPC) : Contract = struct

  let mk timestamp bhash chash =
    try
      let json = Rpc.url_to_json (Url.getContract bhash chash) in (Some {
          timestamp = timestamp;
          hash = bhash;
          storage = json |> member "script" |> member "storage" |> Safe.to_string;
          balance = json |> member "balance" |> to_string;
        })
    with _ -> None

  let json_to_storage_type json =
    json |> member "script" |> member "code" |> to_list |> List.fold_left (fun acc code ->
        let prim = code |> member "prim" |> to_string in
        if compare prim "storage" = 0 then
          code |> member "args" |> to_list |> fun l -> List.nth l 0 |> Safe.to_string
        else acc
      ) ""

  let mk_data chash =
    let json = Rpc.url_to_json (Url.getContract "head" chash) in
    let storage_type = json_to_storage_type json in
    let storage_type_flat = Jsontoflat.flatten_typ storage_type in
    {
      id = chash;
      storage_type = storage_type;
      storage_type_flat = storage_type_flat;
      entries = [];
    }

end



module type Pool = sig
  val make : (string * string option) list -> unit
  val init : string -> string -> unit
  val remove_contract : string -> unit
  val get_contract_ids : unit -> string list
  val get_contract_ids_head : unit -> (string * string option) list
  val get_contract : string -> storage
  val set_contract : string -> storage -> unit
  val is_not_empty : unit -> bool
end
module Make_Pool (Contract : Contract) : Pool = struct
  let heads : (string * string option) list ref = ref []
  let storages : (string * storage) list ref = ref []

  let make l = heads := l
  let init timestamp hash =
    storages := List.map (fun (contract_id, _) ->
        match  Contract.mk timestamp hash contract_id with
        | Some v -> contract_id, v
        | _ -> assert false) !heads

  let remove_contract contract_id =
    let f (a, _) = not (String.equal a contract_id) in
    heads := List.filter f !heads;
    storages := List.filter f !storages
  let get_contract_ids _ = List.map fst !heads
  let get_contract_ids_head _ = !heads
  let get_contract contract_id = List.assoc contract_id !storages
  let set_contract contract_id s = storages := List.map (fun (id, st) ->
      if String.equal contract_id id
      then (id, s)
      else (id, st)) !storages
  let is_not_empty _ = List.length !heads > 0
end

(* Contract Explorer --------------------------------------------------------*)

module Make_ContractExplorer (Block : Block) (Contract : Contract) (Db : Db) (Pool : Pool) = struct

  let rec explore block_hash =
    let block = Block.mk_id block_hash in
    (* dump operations *)
    let data = Block.mk_data block.hash in
    print_string ("."); flush stdout;
    let l : (string * string option) list = Pool.get_contract_ids_head () in
    List.iter (fun (contract_id, contract_head) ->
        match contract_head, Contract.mk data.timestamp block.hash contract_id with
        | Some f, _ when String.equal f block.hash ->
          Pool.remove_contract contract_id
        | _, Some c ->
          begin
            let previous_contract = Pool.get_contract contract_id in
            if not (cmp_contracts previous_contract c)
            then
              begin
                Db.write_storage contract_id previous_contract;
                Pool.set_contract contract_id c
              end
          end
        | _, None ->
          begin
            let previous_contract = Pool.get_contract contract_id in
            Db.write_storage contract_id previous_contract;
            Pool.remove_contract contract_id
          end
      ) l;
    let contract_ids = Pool.get_contract_ids () in
    List.iter (fun op ->
        let { hash=_; source=src; destination=dst } = op in
        if (List.mem src contract_ids)
        then Db.write_op src op;
        if (List.mem dst contract_ids)
        then Db.write_op dst op;
      ) data.operations;
    if Pool.is_not_empty()
    then explore block.previous

end

let process _ =
  let module Db = Make_Db in

  let add_contract (contract_id : string) =
    let module TzInfo : BCinfo = struct
      let getIp () = MOptions.getAddress()
      let getPort () = MOptions.getPort()
      let getBranch () = MOptions.getBranch()
    end in
    let module Url = Make_TzURL (TzInfo) in
    let module Block = Make_TzBlock (Url) (Rpc) in
    let module Contract = Make_TzContract (Url) (Block) (Rpc) in
    let cinfo = Contract.mk_data contract_id in
    Db.write_contract_info cinfo
  in

  let sync _ =
    let module TzInfo : BCinfo = struct
      let getIp () = MOptions.getAddress()
      let getPort () = MOptions.getPort()
      let getBranch () = MOptions.getBranch()
    end in
    let module Url = Make_TzURL (TzInfo) in
    let module Block = Make_TzBlock (Url) (Rpc) in
    let module Contract = Make_TzContract (Url) (Block) (Rpc) in
    let module Pool = Make_Pool (Contract) in
    let module Explorer = Make_ContractExplorer (Block) (Contract) (Db) (Pool) in
    let init_block = "head" in
    let block_id   = Block.mk_id init_block in
    let block_data = Block.mk_data init_block in
    let contract_ids = Db.get_contract_ids_head () in
    Pool.make contract_ids;
    Pool.init block_data.timestamp block_id.hash;
    Explorer.explore block_id.hash;
    List.iter (fun (x,_) -> Db.write_head block_id.hash x) contract_ids;
    print_endline "";
  in

  let fill_storage_flat _ =
    let process_storage_flat contract_id =
      let storage_type = Db.get_storage_type contract_id in
      let storage_id_values = Db.get_storage_id_values_from_contract_id contract_id in
      let s = List.map (fun (x, y) -> (x, Jsontoflat.flatten_storage storage_type y)) storage_id_values in
      Db.write_storage_flat s
    in
    let l : string list = Db.get_contract_ids() in
    List.iter process_storage_flat l
  in

  Db.open_logs ();
  begin
    match !MOptions.cmd with
    | AddContract arg -> add_contract arg
    | Sync            -> sync ()
    | FillStorageFlat -> fill_storage_flat ()
    | _ -> ()
  end;
  Db.close_logs()

let main () =
  let print_version () = Format.printf "%s@." version; exit 0 in
  let arg_list = Arg.align [
      "--add-contract", Arg.String (fun s -> MOptions.cmd := AddContract s), "<contract_id> Add contract <contract_id>";
      "--sync", Arg.Unit (fun _ -> MOptions.cmd := Sync), " Synchronise database";
      "--fill-storage-flat", Arg.Unit (fun _ -> MOptions.cmd := FillStorageFlat), " Fill storage flat";
      "--address", Arg.String (MOptions.setAddress), "<address> Set address";
      "--branch", Arg.String (MOptions.setBranch), "<branch> Set branch";
      "--port", Arg.String (MOptions.setPort), "<port> Set port";
      "-v", Arg.Unit (fun () -> print_version ()), " Show version number and exit";
      "--version", Arg.Unit (fun () -> print_version ()), " Same as -v";
    ] in
  let arg_usage = String.concat "\n" [
      "usage : bctojson [OPTIONS]";
      "";
      "Available options:";
    ] in
  Arg.parse arg_list (fun (s : string) -> print_endline s) arg_usage;
  process ()

let _ = main ()