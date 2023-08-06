open Base

exception Bad_content_type of string

let validate_content_type content_type =
  match content_type with
  | Some content_type ->
      if String.equal content_type Dream.application_json |> not then
        raise
          (Bad_content_type
             (String.concat ~sep:" "
                [
                  "Content-Type:";
                  content_type;
                  "when expected";
                  Dream.application_json;
                ]))
  | None -> raise (Bad_content_type "No Content")
