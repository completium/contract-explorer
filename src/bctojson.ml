let main () =
  let url = "localhost:8732/chains/main/blocks/head/context/contracts/KT1CJuwyRn4C8nAn9E4eGbKHaPAeT4GfVkAW/storage" in
  begin
    match Curly.(run (Request.make ~url:url ~meth:`GET ())) with
    | Ok x ->
      Format.printf "status: %d\n" x.Curly.Response.code;
      Format.printf "headers: %a\n" Curly.Header.pp x.Curly.Response.headers;
      Format.printf "body: %s\n" x.Curly.Response.body
    | Error e ->
      Format.printf "Failed: %a" Curly.Error.pp e
  end;
  print_endline "bctojson"

let _ = main ()
