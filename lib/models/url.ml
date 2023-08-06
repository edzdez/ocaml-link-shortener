open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type url_object = { url : string } [@@deriving yojson]

let get_url_from_id =
  let query =
    let open Caqti_request.Infix in
    (Caqti_type.string ->* Caqti_type.string)
      "SELECT url FROM links WHERE id = $1"
  in
  fun uuid (module Db : Db.DB) ->
    let%lwt link_or_error = Db.collect_list query uuid in
    Caqti_lwt.or_fail link_or_error

let add_url =
  let query =
    let open Caqti_request.Infix in
    (Caqti_type.(tup2 string string) ->. Caqti_type.unit)
      "INSERT INTO links VALUES($1, $2)"
  in
  fun uuid url (module Db : Db.DB) ->
    let%lwt unit_or_error = Db.exec query (uuid, url) in
    Caqti_lwt.or_fail unit_or_error

let from_json s = s |> Yojson.Safe.from_string |> url_object_of_yojson
