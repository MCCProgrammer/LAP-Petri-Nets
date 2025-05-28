let (|>>) x f = 
  match x with
  | None -> None
  | Some y -> f y