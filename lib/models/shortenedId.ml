let create url =
  Uuidm.v3 Uuidm.ns_url url |> Uuidm.to_string
  |> Str.global_replace (Str.regexp "-") ""
