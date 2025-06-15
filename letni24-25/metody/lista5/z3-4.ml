let correct_par par_string = 
  let rec it curr_str n = 
    match curr_str with
    | [] -> n = 0
    | '(' :: xs' -> it xs' (n+1) 
    | ')' :: xs' -> it xs' (n-1)
    | _ -> false
  in it (par_string |> String.to_seq |> List.of_seq) 0 

  let correct_par par_string = 
    let rec it curr_str stack =
      match curr_str with
      | [] -> stack = []
      | '(' :: xs' -> it xs' ('('::stack)
      | '{' :: xs' -> it xs' ('{'::stack)
      | '[' :: xs' -> it xs' ('['::stack)
      | ')' :: xs' -> (match stack with
                      | '(' :: rest -> it xs' rest
                      | _ -> false)
      | '}' :: xs' -> (match stack with
                      | '{' :: rest -> it xs' rest
                      | _ -> false)
      | ']' :: xs' -> (match stack with
                      | '[' :: rest -> it xs' rest
                      | _ -> false) 
      | _ -> false
    in it (par_string |> String.to_seq |> List.of_seq)  []