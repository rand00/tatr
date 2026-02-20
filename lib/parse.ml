
module Indentation_tree = struct

  let count_indent_until_visual ~tab_is_spaces line =
    let n_indent, _, idx_visual = 
      let exception Shortcircuit of (int * int * int option) in
      try
        line |> CCString.fold_left (fun (n_indent, idx, _idx_visual) ->
          function
          | '\t' -> n_indent + tab_is_spaces, succ idx, None
          | ' '  -> n_indent + 1            , succ idx, None
          | '\n' ->
            failwith "Parse.Indentation_tree.count_indent_until_visual: \
                      we only support a single line as input"
          | _ ->
            let res = n_indent, idx, Some idx in
            raise @@ Shortcircuit res
        ) (0, 0, None)
      with Shortcircuit v -> v
    in
    n_indent, idx_visual

  (*> Helpful for choosing the right visual char's:
      for i = 0 to 255 do 
        CCFormat.printf "%d: '%c'\n%!" i (Char.chr i)
      done
  *)
  let is_wordchar = function
    | 'a'..'z'
    | '_'
    | '-'
    | 'A'..'Z'
    | '0'..'9' -> true
    | _ -> false

  (*> Note: I made this interface to avoid too much extra allocation for lines*)
  let extract_words ~include_char ~exclude_char ~from_idx line =
    let words = ref [] in
    let word_range = ref None in
    let maybe_append_word () =
      match !word_range with
      | None -> ()
      | Some (start, stop) ->
        let word_str = CCString.sub line start (stop - start) in
        words := word_str :: !words;
        word_range := None;
    in
    for i = from_idx to CCString.length line -1 do
      let c = CCString.get line i in
      if not (exclude_char c) && (include_char c || is_wordchar c) then (
        let word_range' = match !word_range with
          | None -> Some (i, i+1)
          | Some (start, stop) -> Some (start, stop+1)
        in
        word_range := word_range';
      ) else (
        maybe_append_word ()
      )
    done;
    maybe_append_word ();
    CCList.rev !words

  let add_line ~config line_data tree =
    (*> Note that n-indent is not the same as index
      * tab is counted a specified number of spaces
      * when supporting unicode in the future, whitespace can be of even more sizes
    *)
    let line = line_data.Line_data.line in
    let indent, idx_visual =
      count_indent_until_visual
        ~tab_is_spaces:config.Config.tab_is_spaces
        line
    in
    match idx_visual with
    | None -> Some tree
    | Some idx_visual ->
      let words =
        extract_words
          ~include_char:config.Config.include_char
          ~exclude_char:config.Config.exclude_char
          ~from_idx:idx_visual
          line
      in
      let under parent = parent.Line_data.indent < indent in
      let v = Line_data.{ line_data with indent; words } in
      Tree.insert_in_first_path ~under v tree

end
