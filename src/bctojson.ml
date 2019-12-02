open Yojson
open Safe
open Util

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
  storage_type : string;
  entries : string list;
}

type contract = {
  timestamp : string;
  storage : string;
  balance : string;
}
[@@deriving yojson, show {with_path = false}]

let cmp_contracts c1 c2 = 
(c1.storage = c2.storage) && 
(c1.balance = c2.balance) 

(* fabrics ------------------------------------------------------------------*)

module type Url = sig
  val getBlock    : string -> string
  val getHead     : unit -> string
  val getContract : string -> string -> string
end

module type Block = sig
  val mk_id   : string -> block_id
  val mk_data : string -> block_data
end 

module type Contract = sig
  val mk : string -> string -> string -> contract option
  val mk_data : string -> contract_info
end

(* output module ------------------------------------------------------------*)

module type Writer = sig
  val open_logs : unit -> unit
  val close_logs : unit -> unit
  val write_contract : contract -> unit
  val write_op : op -> unit
end

module Make_Writer (Dirs : sig 
  val path : string
  val contract : string
end) : Writer = struct
  let out_contracts = ref stdout
  let out_ops = ref stdout

  let open_logs () = 
    out_contracts := open_out_gen [Open_append] 0 (Dirs.path^"/"^Dirs.contract^"/contracts.json");
    out_ops := open_out_gen [Open_append] 0 (Dirs.path^"/"^Dirs.contract^"/ops.json")

  let close_logs () = 
    close_out !out_contracts;
    close_out !out_ops

  let print_json channel json = 
    let str = Yojson.Safe.to_string json in
    Printf.fprintf channel "%s" (str^"\n");
    flush channel

  let write_contract contract = 
    print_json !out_contracts (contract_to_yojson contract)

  let write_op op = 
    print_json !out_ops (op_to_yojson op)
  
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
      hash         = hash;
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
        storage_type = json_to_storage_type json;
        entries = [];
      }

end

(* Contract Explorer --------------------------------------------------------*)

module Make_ContractExplorer (Block : Block) (Contract : Contract) (Writer : Writer) = struct

  let rec explore first contract_key previous_contract previous_block =
    let block = Block.mk_id previous_block.previous in
    (* dump operations *)
    let data = Block.mk_data block.hash in
    let ops = List.filter(fun op ->
      let { hash=_; source=src; destination=dst } = op in 
        compare src contract_key = 0 || compare dst contract_key = 0
    ) data.operations in
    if List.length ops > 0 then
      List.iter (fun op -> Writer.write_op op) ops;
    print_string ("."); flush stdout;
    match Contract.mk data.timestamp block.hash contract_key with
    | Some c ->
      if not (cmp_contracts previous_contract c) then (
        if first then
          Writer.write_contract previous_contract;
        Writer.write_contract c;
        explore false contract_key c block)
      else 
        explore first contract_key c block
    | None -> (
      print_endline "\nNO MORE CONTRACT";
      if first then
        Writer.write_contract previous_contract
    )
    
end

let main () =
  let contract_key = "KT1BoLiscRVVgBkvFSRrbDTJkDfpzCDkBKa2" in
  let module TzInfo : BCinfo = struct 
    let getIp () = "localhost"
    let getPort () = "8732"
    let getBranch () = "main"
  end in
  let module Writer = Make_Writer (struct 
    let path = "/Users/benoitrognier/Documents/contract-explorer"
    let contract = contract_key
  end) in 
  Writer.open_logs();
  let module Url = Make_TzURL (TzInfo) in
  let module Block = Make_TzBlock (Url) (Rpc) in
  let module Contract = Make_TzContract (Url) (Block) (Rpc) in
  let module Explorer = Make_ContractExplorer (Block) (Contract) (Writer) in
  let init_block = "head" in
  match Contract.mk "" init_block contract_key with
  | Some c -> 
    let json = Rpc.url_to_json (Url.getContract "head" contract_key) in
      print_endline (Safe.to_string json);
    Explorer.explore true contract_key c { hash=""; previous=init_block }
  | _ -> print_endline ("Contract not found in "^init_block);
  Writer.close_logs()

let _ = main ()
