open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type url_object = { url : string } [@@deriving yojson]

let () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/:url" (fun request ->
             Dream.html (Dream.param request "url"));
         Dream.post "/api/shorten" (fun request ->
             let%lwt body = Dream.body request in

             let url_object =
               body |> Yojson.Safe.from_string |> url_object_of_yojson
             in

             `String url_object.url |> Yojson.Safe.to_string |> Dream.json);
       ]
