let replace k v l =
  let prefix = k ^ "=" in
  (prefix ^ v) :: List.filter (fun x -> not (String.starts_with ~prefix x)) l
