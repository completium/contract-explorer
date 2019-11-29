open Yojson
open Basic
open Util

(* Tools --------------------------------------------------------------------*)

module type RPC = sig
  exception RPCNotFound of string * (Curly.Error.t)
  val url_to_string : string -> string
  val url_to_json : string -> Basic.t
end
module Rpc : RPC = struct

  exception RPCNotFound of string * (Curly.Error.t)
  
  let url_to_string url =
    match Curly.(run (Request.make ~url:url ~meth:`GET ())) with
      | Ok x -> x.Curly.Response.body
      | Error e -> raise (RPCNotFound(url,e))

  let url_to_json url = from_string (url_to_string url)

end

(* Interfaces ---------------------------------------------------------------*)

module type BCinfo = sig
  val getIp     : unit -> string
  val getPort   : unit -> string
  val getBranch : unit -> string
end

module type Url = sig
  val getBlock    : string -> string
  val getHead     : unit -> string
  val getContract : string -> string -> string
end
type block_id = {
  hash         : string;
  previous     : string;
}
[@@deriving show {with_path = false}]

type block_data = {
  timestamp    : string;
  transactions : string;
}
[@@deriving show {with_path = false}]

module type Block = sig
  val mk_id   : string -> block_id
  val mk_data : string -> block_data
end 

type contract = {
  storage : string;
  balance : string;
}
[@@deriving show {with_path = false}]

module type Contract = sig
  val mk : string -> string -> contract option
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
    
  let mk_data hash = 
    let json = Rpc.url_to_json (Url.getBlock hash) in {
      timestamp = json |> member "header" |> member "timestamp" |> to_string;
      transactions = "";
    }

end

module Make_TzContract (Url : Url) (Block : Block) (Rpc : RPC) : Contract = struct

  let mk bhash chash = 
    try 
      let json = Rpc.url_to_json (Url.getContract bhash chash) in (Some {
        storage = json |> member "script" |> member "storage" |> show;
        balance = json |> member "balance" |> to_string;
      })
    with _ -> None

end

(* Contract Explorer *)

module Make_ContractExplorer (Block : Block) (Contract : Contract) = struct

  let rec explore contract_key previous_contract previous_block =
    let block = Block.mk_id previous_block.previous in
    print_string ("."); flush_all();
    match Contract.mk block.hash contract_key with
    | Some c ->
      if compare previous_contract c = 0 then ()
      else (
        let data = Block.mk_data block.hash in
        print_endline ("\nCHANGE AT "^data.timestamp)
      );
      explore contract_key c block
    | None -> print_endline "\nNO MORE CONTRACT"
    
end

let main () =
  let contract_key = "KT1CJuwyRn4C8nAn9E4eGbKHaPAeT4GfVkAW" in
  let module TzInfo : BCinfo = struct 
    let getIp () = "localhost"
    let getPort () = "8732"
    let getBranch () = "main"
  end in
  let module Url = Make_TzURL (TzInfo) in
  let module Block = Make_TzBlock (Url) (Rpc) in
  let module Contract = Make_TzContract (Url) (Block) (Rpc) in
  let module Explorer = Make_ContractExplorer (Block) (Contract) in
  match Contract.mk "head" contract_key with
  | Some c -> Explorer.explore contract_key c { hash=""; previous="head" }
  | _ -> ()

let _ = main ()
