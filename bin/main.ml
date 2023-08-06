open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module type DB = Caqti_lwt.CONNECTION

module T = Caqti_type

type url_object = { url : string } [@@deriving yojson]

let get_url_from_id =
  let query =
    let open Caqti_request.Infix in
    (T.string ->* T.string) "SELECT url FROM links WHERE id = $1"
  in
  fun uuid (module Db : DB) ->
    let%lwt link_or_error = Db.collect_list query uuid in
    Caqti_lwt.or_fail link_or_error

let add_url =
  let query =
    let open Caqti_request.Infix in
    (T.(tup2 string string) ->. T.unit) "INSERT INTO links VALUES($1, $2)"
  in
  fun uuid url (module Db : DB) ->
    let%lwt unit_or_error = Db.exec query (uuid, url) in
    Caqti_lwt.or_fail unit_or_error

let () =
  Dream.run @@ Dream.logger
  @@ Dream.sql_pool "sqlite3:db.sqlite"
  @@ Dream.router
       [
         Dream.get "/:url" (fun request ->
             let url = Dream.param request "url" in
             let%lwt full_url = Dream.sql request (get_url_from_id url) in
             match full_url with
             | [] -> Dream.empty `Not_Found
             | url :: [] -> Dream.html url
             | _ :: _ -> Dream.empty `Internal_Server_Error);
         Dream.post "/api/shorten" (fun request ->
             let%lwt body = Dream.body request in
             try
               let url_object =
                 body |> Yojson.Safe.from_string |> url_object_of_yojson
               in
               let uuid =
                 Uuidm.v3 Uuidm.ns_url url_object.url
                 |> Uuidm.to_string
                 |> Str.global_replace (Str.regexp "-") ""
               in
               Dream.log "Attempting to insert (%s, %s)" uuid url_object.url;
               let%lwt () = Dream.sql request (add_url uuid url_object.url) in
               Dream.json uuid
             with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
               Dream.log "Bad request: %s" (Exn.to_string exn);
               Dream.empty `Bad_Request);
       ]
