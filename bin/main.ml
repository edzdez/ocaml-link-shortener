open Base
open Ppx_yojson_conv_lib
open Ocaml_link_shortener

let error_handler (error : Dream.error) (_debug_info : string)
    (suggested_response : Dream.response) =
  match error.condition with
  | `Exn exn -> (
      match exn with
      | Utils.Bad_content_type s ->
          Dream.log "Bad content type: %s" s;
          Dream.empty `Bad_Request
      | Yojson_conv.Of_yojson_error (exn, _) ->
          Dream.log "Json error: %s" (Exn.to_string exn);
          Dream.empty `Bad_Request
      | _ -> Lwt.return suggested_response)
  | _ -> Lwt.return suggested_response

let () =
  Dream.run ~error_handler:(Dream.error_template error_handler)
  @@ Dream.logger
  @@ Dream.sql_pool "sqlite3:db.sqlite"
  @@ Dream.router
       [
         Dream.get "/:url" (fun request ->
             let url = Dream.param request "url" in
             let%lwt full_url =
               Dream.sql request (Models.Url.get_url_from_id url)
             in
             match full_url with
             | [] -> Dream.empty `Not_Found
             | url :: [] -> Dream.html url
             | _ :: _ -> Dream.empty `Internal_Server_Error);
         Dream.post "/api/shorten" (fun request ->
             let content_type = Dream.header request "Content-Type" in
             Utils.validate_content_type content_type;
             let%lwt body = Dream.body request in
             let url_object = Models.Url.from_json body in
             let uuid = Models.ShortenedId.create url_object.url in
             Dream.log "Attempting to insert (%s, %s)" uuid url_object.url;
             let%lwt () =
               Dream.sql request (Models.Url.add_url uuid url_object.url)
             in
             Dream.json uuid);
       ]
