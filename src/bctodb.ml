open Yojson
open Safe
open Util
open Sqlite3

let version = "0.1"

(* Options ------------------------------------------------------------------*)

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

  let force = ref false
  let fill_storage_flat = ref false
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

module type Writer = sig
  val clear : unit -> unit
  val open_logs : unit -> unit
  val close_logs : unit -> unit
  val write_contract_info : contract_info -> string -> unit
  val write_storage : string -> storage -> unit
  val write_op : string -> op -> unit
  val get_head_content : string -> string option
  val get_storage_type : unit -> string
  val get_storage_id_values : unit -> (string * string) list
  val write_storage_flat : (string * string) list -> unit
end

module Make_Writer (Dirs : sig
    val path : string
    val contract : string
  end) : Writer = struct

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
                      entries text NOT NULL, \
                      head text NOT NULL \
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

  let write_contract_info (c : contract_info) head =
    let insert : string =
      Printf.sprintf "INSERT OR REPLACE INTO %s VALUES('%s', '%s', '%s', '%s');"
        table_info
        c.id
        c.storage_type
        (List.fold_left (fun (accu : string) (x : string) -> accu ^ " " ^ x) "" c.entries)
        head
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

  (* print_json true !out_contract_info (contract_info_to_yojson st) *)

  let get_head_content c : string option =
    let select_sql = Printf.sprintf "SELECT head FROM %s WHERE id = '%s';" table_info c in
    (* let select_stmt = prepare db select_sql in *)
    let str = ref "" in
    match exec db select_sql ~cb:(fun row _ ->
        match row.(0) with
        | Some a -> str := a
        | _ -> ()
      ) with
    | Rc.OK -> Some !str
    | _ -> None


  let get_contract_ids () : string list =
    []

  let get_storage_type () =
    let select_sql = Printf.sprintf "SELECT storage_type FROM %s WHERE id = '%s';" table_info Dirs.contract in
    (* let select_stmt = prepare db select_sql in *)
    let str = ref "" in
    match exec db select_sql ~cb:(fun row _ ->
        match row.(0) with
        | Some a -> str := a
        | _ -> ()
      ) with
    | Rc.OK -> !str
    | _ -> assert false

  let get_storage_id_values () : (string * string) list =
    let select_sql = Printf.sprintf "SELECT hash, storage FROM %s;" table_storages in
    (* let select_stmt = prepare db select_sql in *)
    let l = ref [] in
    match exec db select_sql ~cb:(fun row _ ->
        match row.(0), row.(1) with
        | Some a, Some b -> l := (a, b)::!l
        | _ -> ()
      ) with
    | Rc.OK -> !l
    | _ -> assert false

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
    let json = Rpc.url_to_json (Url.getContract "head" chash) in {
      id = chash;
      storage_type = json_to_storage_type json;
      entries = [];
    }

end

(* Contract Explorer --------------------------------------------------------*)

module Make_ContractExplorer (Block : Block) (Contract : Contract) (Writer : Writer) = struct

  let rec explore contract_id previous_contract previous_block fblock =
    let block = Block.mk_id previous_block.previous in
    (* dump operations *)
    let data = Block.mk_data block.hash in
    let ops = List.filter(fun op ->
        let { hash=_; source=src; destination=dst } = op in
        compare src contract_id = 0 || compare dst contract_id = 0
      ) data.operations in
    List.iter (fun op -> Writer.write_op contract_id op) ops;
    print_string ("."); flush stdout;
    match fblock, Contract.mk data.timestamp block.hash contract_id with
    | Some f, _ when String.equal f block.hash ->
      Format.printf "\nHEAD BLOCK FOUND@."
    | _, Some c ->
      if not (cmp_contracts previous_contract c) then (
        Writer.write_storage contract_id previous_contract;
        explore contract_id c block fblock)
      else
        explore contract_id c block fblock
    | _, None -> (
        Format.printf "\nNO MORE CONTRACT@.";
        Writer.write_storage contract_id previous_contract
      )

end

let process contract_id =
  let contract_id = match contract_id with
    | "" -> Format.eprintf "no contract"; exit 1
    | _  -> contract_id
  in

  let module TzInfo : BCinfo = struct
    let getIp () = MOptions.getAddress()
    let getPort () = MOptions.getPort()
    let getBranch () = MOptions.getBranch()
  end in
  let module Writer = Make_Writer (struct
      let path = MOptions.getPath()
      let contract = contract_id
    end) in

  if !MOptions.force
  then Writer.clear();

  let fblock = Writer.get_head_content contract_id in

  match !MOptions.fill_storage_flat with
  | true ->
    begin
      let process_storage_flat () =
        let storage_type = Writer.get_storage_type () in
        let storage_id_values = Writer.get_storage_id_values () in
        let s = List.map (fun (x, y) -> (x, Jsontoflat.flatten_storage storage_type y)) storage_id_values in
        Writer.write_storage_flat s
      in
      (* get_contract_ids()
         |> List.iter (fun x -> process_storage_flat x) *)
      process_storage_flat ()
    end
  | _ ->
    begin
      Writer.open_logs();
      let module Url = Make_TzURL (TzInfo) in
      let module Block = Make_TzBlock (Url) (Rpc) in
      let module Contract = Make_TzContract (Url) (Block) (Rpc) in
      let module Explorer = Make_ContractExplorer (Block) (Contract) (Writer) in
      let init_block = "head" in
      begin
        match Contract.mk "" init_block contract_id with
        | Some c ->
          begin
            let cinfo = Contract.mk_data contract_id in
            let block = Block.mk_id "head" in
            Writer.write_contract_info cinfo block.hash;
            Explorer.explore contract_id c { hash=""; previous=init_block } fblock
          end
        | _ -> Format.printf "Contract not found in %s@." init_block
      end;
      Writer.close_logs()
    end

let main () =
  let print_version () = Format.printf "%s@." version; exit 0 in
  let arg_list = Arg.align [
      "--force", Arg.Set (MOptions.force), " Force";
      "--fill-storage-flat", Arg.Set (MOptions.fill_storage_flat), " Fill storage flat";
      "--address", Arg.String (MOptions.setAddress), "<address> Set address";
      "--branch", Arg.String (MOptions.setBranch), "<branch> Set branch";
      "--port", Arg.String (MOptions.setPort), "<port> Set port";
      "-v", Arg.Unit (fun () -> print_version ()), " Show version number and exit";
      "--version", Arg.Unit (fun () -> print_version ()), " Same as -v";
    ] in
  let arg_usage = String.concat "\n" [
      "usage : bctojson [OPTIONS] <contract_id>";
      "";
      "Available options:";
    ] in
  Arg.parse arg_list process arg_usage

let _ = main ()