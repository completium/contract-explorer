open Yojson
open Safe
open Util

let version = "0.1"

(* Options ------------------------------------------------------------------*)

type command =
  | Sync
  | AddContract of string
  | FillStorageFlat
  | GetBigMap of string * string * string
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

(* pp functions -------------------------------------------------------------*)

let pp_list sep pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
    pp

(* BC types -----------------------------------------------------------------*)

type block_id = {
  hash         : string;
  previous     : string;
}
[@@deriving yojson, show {with_path = false}]

type big_map_diff = {
  action : string;
  mapid : string;
  key : string;
  value : string;
}
[@@deriving yojson, show {with_path = false}]

type op = {
  kind : string;
  hash : string;
  timestamp : string;
  source : string;
  destination : string;
  parameters : string;
  amount: string;
  bigmapdiffs: big_map_diff list;
}
[@@deriving yojson, show {with_path = false}]

let is_origination op = String.equal "org" op.kind
let is_transaction op = String.equal "tr" op.kind

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
  val write_op : string -> op -> storage -> unit
  val get_storage_type : string -> string
  val get_storage_id_values_from_contract_id : string -> (string * string) list
  val write_storage_flat : (string * string) list -> unit
  val write_head : string -> string -> unit
  val get_contract_ids : unit -> string list
  val get_contract_ids_head : unit -> (string * string option) list
  val get_operations_for : string -> op list
end

module Make_Db : Db = struct

  let db = Sqlite3.db_open "bc.db"
  let table_info     = "contracts_info"
  let table_ops      = "operations"

  let exec_cmd cmd =
    match Sqlite3.exec db cmd with
    | Sqlite3.Rc.OK -> ()
    | r -> prerr_endline (Sqlite3.Rc.to_string r); prerr_endline (Sqlite3.errmsg db)

  let drop_tables table =
    let drop_table_sql = "DROP TABLE IF EXISTS " ^ table ^ ";" in
    exec_cmd drop_table_sql

  let clear () =
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

  let create_table_ops () =
    let create_table_sql =
      Printf.sprintf "CREATE TABLE IF NOT EXISTS %s ( \
                      id INTEGER PRIMARY KEY AUTOINCREMENT, \
                      hash VARCHAR(52) NOT NULL, \
                      block_hash VARCHAR(52) NOT NULL, \
                      bc VARCHAR(3) NOT NULL, \
                      contract_id VARCHAR(37) NOT NULL, \
                      kind VARCHAR(3) NOT NULL, \
                      timestamp date NOT NULL, \
                      source VARCHAR(37) NOT NULL, \
                      destination VARCHAR(37) NOT NULL, \
                      parameters text, \
                      bigmapdiffs text, \
                      amount text, \
                      storage text NOT NULL, \
                      storage_flat text, \
                      balance text NOT NULL
                      );"

        table_ops
    in
    exec_cmd create_table_sql

  let create_tables () =
    create_table_info ();
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

  let diffs_to_string diffs = "["^(String.concat "," (List.map (fun d ->
      Safe.to_string (big_map_diff_to_yojson d)) diffs))^"]"

  let string_to_diffs str =
    let to_diff input =
      {
        action = input |> member "action" |> to_string;
        mapid  = input |> member "mapid"  |> to_string;
        key    = input |> member "key"    |> to_string;
        value  = input |> member "value"  |> to_string;
      } in
    match str with
    | Some str ->
      begin
        let json =  Safe.from_string str in
        match json with
        | `List l -> List.map to_diff l;
        | _ -> assert false;
      end
    | _ -> []

  let write_op contract_id (op : op) storage =
    let insert : string =
      Printf.sprintf "INSERT INTO %s(hash, block_hash, bc, contract_id, kind, timestamp, source, destination, parameters, bigmapdiffs, amount, storage, balance) VALUES('%s', '%s', 'tz', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');"
        table_ops
        op.hash
        storage.hash
        contract_id
        op.kind
        op.timestamp
        op.source
        op.destination
        op.parameters
        (diffs_to_string op.bigmapdiffs)
        op.amount
        storage.storage
        storage.balance
    in
    exec_cmd insert

  let get_contract_ids () : string list =
    let select_sql = Printf.sprintf "SELECT id FROM %s;" table_info in
    let l = ref [] in
    match Sqlite3.exec db select_sql ~cb:(fun row _ ->
        match row.(0) with
        | Some a -> l := a::!l
        | _ -> ()
      ) with
    | Sqlite3.Rc.OK -> !l
    | _ -> assert false

  let get_contract_ids_head () : (string * string option) list =
    let select_sql = Printf.sprintf "SELECT id, head FROM %s;" table_info in
    let l : (string * string option) list ref = ref [] in
    match Sqlite3.exec db select_sql ~cb:(fun row _ ->
        match row.(0), row.(1) with
        | Some a, b -> l := (a, b)::!l
        | _ -> ()
      ) with
    | Sqlite3.Rc.OK -> !l
    | _ -> assert false

  let get_storage_type contract_id =
    let select_sql = Printf.sprintf "SELECT storage_type FROM %s WHERE id = '%s';" table_info contract_id in
    let str = ref "" in
    match Sqlite3.exec db select_sql ~cb:(fun row _ ->
        match row.(0) with
        | Some a -> str := a
        | _ -> ()
      ) with
    | Sqlite3.Rc.OK -> !str
    | _ -> assert false

  let get_storage_id_values_from_contract_id contract_id : (string * string) list =
    let select_sql = Printf.sprintf "SELECT hash, storage FROM %s WHERE contract_id = '%s';" table_ops contract_id in
    let l = ref [] in
    match Sqlite3.exec db select_sql ~cb:(fun row _ ->
        match row.(0), row.(1) with
        | Some a, Some b -> l := (a, b)::!l
        | _ -> ()
      ) with
    | Sqlite3.Rc.OK -> !l
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
              table_ops
              value
              id
          in
          exec_cmd insert
        end
      ) l

  let get_operations_for contract_id =
    let select_sql = Printf.sprintf "SELECT kind, hash, timestamp, source, destination, parameters, amount, bigmapdiffs FROM %s WHERE contract_id = '%s' order by timestamp;" table_ops contract_id in
    let l = ref [] in
    match Sqlite3.exec db select_sql ~cb:(fun row _ ->
        match row.(0), row.(1), row.(2), row.(3), row.(4), row.(5), row.(6), row.(7) with
        | Some k, Some h, Some t, Some s, Some d, Some p, Some a, b ->
          begin
            let op : op =
              {
                kind = k;
                hash = h;
                timestamp = t;
                source = s;
                destination = d;
                parameters = p;
                amount = a;
                bigmapdiffs = string_to_diffs b;
              } in
            l := op::!l
          end
        | _ -> ()

      ) with
    | Sqlite3.Rc.OK -> List.rev !l
    | _ -> assert false
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

  let get_big_map_diffs json =
    let json = json |> member "metadata" in
    let entries = json |> keys in
    if List.mem "operation_result" entries then
      let json = json |> member "operation_result" in
      let entries = json |> keys in
      if List.mem "big_map_diff" entries then
        List.map (fun bmd -> {
              action = bmd |> member "action" |> to_string;
              mapid = bmd |> member "big_map" |> to_string;
              key = bmd |> member "key" |> Safe.to_string;
              value = bmd |> member "value" |> Safe.to_string;
            }) (json |> member "big_map_diff" |> to_list)
      else []
    else []

  let get_origination_dest json =
    let json = json |> member "metadata" in
    let entries = json |> keys in
    if List.mem "operation_result" entries then
      let json = json |> member "operation_result" in
      let entries = json |> keys in
      (* print_endline (String.concat "\n" entries); *)
      if List.mem "originated_contracts" entries then
        let json = json |> member "originated_contracts" |> to_list in
        List.nth json 0 |> to_string (* shall we treat all originated contracts ? *)
      else ""
    else ""

  let json_to_ops timestamp json : op list =
    json |> member "operations" |> to_list |> List.fold_left (fun acc ops ->
        ops |> to_list |> List.fold_left (fun acc op ->
            let hash = op |> member "hash" |> to_string in
            op |> member "contents" |> to_list |> List.fold_left (fun acc c ->
                let kind = c |> member "kind" |> to_string in
                match kind with
                | "transaction" -> acc @ [{
                    kind = "tr";
                    hash = hash;
                    timestamp = timestamp;
                    source = c |> member "source" |> to_string;
                    destination = c |> member "destination" |> to_string;
                    parameters = c |> member "parameters" |> Safe.to_string;
                    amount = c |> member "amount" |> Safe.to_string;
                    bigmapdiffs = get_big_map_diffs c;
                  }]
                | "origination" -> acc @ [{
                    kind = "org";
                    hash = hash;
                    timestamp = timestamp;
                    source = c |> member "source" |> to_string;
                    destination = get_origination_dest c;
                    parameters = "";
                    amount = "0";
                    bigmapdiffs = get_big_map_diffs c;
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

  let mk_data cid =
    let json = Rpc.url_to_json (Url.getContract "head" cid) in
    let storage_type = json_to_storage_type json in
    let storage_type_flat =
      try
        Jsontoflat.flatten_typ storage_type
      with
      | e ->
        Printf.eprintf "contract_id: %s " cid;
        raise e
    in
    {
      id = cid;
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
  val get_contract : string -> storage
  val set_contract : string -> storage -> unit
  val is_not_empty : unit -> bool
  val contains : string -> bool
  val get_contract_head : string -> string option
  val get_head_contracts : string -> string list
end
module Make_Pool (Contract : Contract) : Pool = struct
  let heads = ref (Hashtbl.create 0)
  let storages = ref (Hashtbl.create 0)
  let contract_heads = ref (Hashtbl.create 0)

  let make l =
    heads := Hashtbl.create (List.length l);
    List.iter (fun (cid,hd) ->
        begin
          Hashtbl.add !heads cid hd;
          match hd with
          | Some hd ->
            begin
              if not (Hashtbl.mem !contract_heads hd)
              then Hashtbl.add !contract_heads hd [cid]
              else
                begin
                  let ll = Hashtbl.find !contract_heads hd in
                  Hashtbl.replace !contract_heads hd (cid::ll)
                end
            end
          | _ -> ()
        end) l

  let init timestamp hash =
    storages := Hashtbl.create (Hashtbl.length !heads);
    Hashtbl.iter (fun cid _ ->
        match  Contract.mk timestamp hash cid with
        | Some v -> Hashtbl.add !storages cid v
        | _ -> assert false) !heads

  let remove_contract contract_id =
    Hashtbl.remove !heads contract_id;
    Hashtbl.remove !storages contract_id

  let get_contract_ids _ = Hashtbl.fold (fun cid _ acc -> acc@[cid]) !heads []
  let get_contract contract_id = Hashtbl.find !storages contract_id
  let set_contract contract_id s =
    if Hashtbl.mem !storages contract_id then
      Hashtbl.remove !storages contract_id;
    Hashtbl.add !storages contract_id s

  let is_not_empty _ = Hashtbl.length !heads > 0

  let contains contract_id = Hashtbl.mem !heads contract_id
  let get_contract_head contract_id = Hashtbl.find !heads contract_id
  let get_head_contracts head =
    if Hashtbl.mem !contract_heads head
    then Hashtbl.find !contract_heads head
    else []
end

(* Contract Explorer --------------------------------------------------------*)

module Make_ContractExplorer (Block : Block) (Contract : Contract) (Db : Db) (Pool : Pool) = struct

  exception ContractNotFound of string

  let write timestamp block_hash op contract_id =
    begin
      match Contract.mk timestamp block_hash contract_id with
      | Some storage ->
        Db.write_op contract_id op storage;
      | None -> raise (ContractNotFound contract_id)
    end;
    if is_origination op && Pool.contains op.destination then
      Pool.remove_contract op.destination

  let rec explore block_hash =
    let block = Block.mk_id block_hash in
    (* dump operations *)
    let data = Block.mk_data block.hash in
    print_string ("."); flush stdout;
    (* remove contract when head is equal to current block *)
    Pool.get_head_contracts block.hash
    |> List.iter Pool.remove_contract;
    (* scan operations *)
    List.iter (fun op ->
        if Pool.contains op.destination then
          write data.timestamp block.hash op op.destination;
        if Pool.contains op.source then
          write data.timestamp block.hash op op.source
      ) data.operations;
    if Pool.is_not_empty()
    then explore block.previous

end

let process rargs =
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

  let fold_map_diff map action key value =
  match action with
  | "alloc" when (String.equal key "null") -> ()
  | "alloc" -> Hashtbl.add map key value
  | "update" | "insert" ->
      if Hashtbl.mem map key then
        Hashtbl.replace map key value
      else
        Hashtbl.add map key value
  | "remove" ->
      if Hashtbl.mem map key then
        Hashtbl.remove map key
  | _ -> raise Not_found
  in

  let get_big_map (cid, hash, mid) =
    (* Format.printf "get_big_map for %s %s %s@\n" cid hash mid; *)
    let ops = Db.get_operations_for cid in
    let map = Hashtbl.create 0 in
    let rec iter_until_hash = function
    | (op : op)::tl ->
      List.iter (fun bmd ->
        if String.equal bmd.mapid mid then
          fold_map_diff map bmd.action bmd.key bmd.value
      ) op.bigmapdiffs;
      if not (String.equal op.hash hash) then
        iter_until_hash tl
    | _ -> () in
    iter_until_hash ops;
    print_endline (Jsontoflat.to_sfval map)
  in

  let cmd =
    match List.rev !rargs with
    | ["get-big-map"; cid; hash; mid ] -> GetBigMap (cid, hash, mid)
    | _ -> !MOptions.cmd
  in

  Db.open_logs ();
  begin
    match cmd with
    | AddContract arg            -> add_contract arg
    | Sync                       -> sync ()
    | FillStorageFlat            -> fill_storage_flat ()
    | GetBigMap (cid, hash, mid) -> get_big_map (cid, hash, mid)
    | _ -> print_endline "unknown command"
  end;
  Db.close_logs()

let main () =
  let print_version () = Format.printf "%s@." version; exit 0 in
  let rargs = ref [] in
  let arg_list = Arg.align [
      "--add-contract", Arg.String (fun s -> MOptions.cmd := AddContract s), "<contract_id> Add contract <contract_id>";
      "--sync", Arg.Unit (fun _ -> MOptions.cmd := Sync), " Synchronise database";
      "--fill-storage-flat", Arg.Unit (fun _ -> MOptions.cmd := FillStorageFlat), " Fill storage flat";
      "--address", Arg.String (MOptions.setAddress), "<address> Set address";
      "--branch", Arg.String (MOptions.setBranch), "<branch> Set branch";
      "--port", Arg.String (MOptions.setPort), "<port> Set port";
      "-v", Arg.Unit (fun () -> print_version ()), " Show version number and exit";
      "--version", Arg.Unit (fun () -> print_version ()), " Same as -v";
      "--", Arg.Rest (fun arg -> rargs := arg::!rargs), ""
    ] in
  let arg_usage = String.concat "\n" [
      "usage : bctojson [OPTIONS] (-- CMD args)";
      "";
      "Available commands:";
      "  get-big-map <contract_id> <hash> <big_map_id>";
      "";
      "Available options:";
    ] in
  Arg.parse arg_list (fun (s : string) -> print_endline s) arg_usage;
  process rargs

let _ = main ()