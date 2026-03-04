
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
            Log.failwith "Parse.Indentation_tree.count_indent_until_visual" 
              "Only a single line is supported as input"
          | _ ->
            let res = n_indent, idx, Some idx in
            raise @@ Shortcircuit res
        ) (0, 0, None)
      with Shortcircuit v -> v
    in
    n_indent, idx_visual

  let is_wordgrapheme = function
    | "-" | "_" -> true
    | gc ->
      let dec = CCString.get_utf_8_uchar gc 0 in
      let uchar = Uchar.utf_decode_uchar dec in
      let word_break = Uucp.Break.word uchar in
      (* CCFormat.eprintf "DEBUG: gc = '%s' -- word_break = %a\n%!" *)
      (*   gc Uucp.Break.pp_word word_break; *)
      match word_break with
      | `LE (*letter*) | `NU (*number*) -> true
      | _ -> false 

  let extract_words ~include_grapheme ~exclude_grapheme ~from_idx line =
    let line = CCString.sub line from_idx (CCString.length line - from_idx) in
    let acc_word = Buffer.create 128 in
    let maybe_add_word words =
      let new_word = Buffer.contents acc_word in
      Buffer.clear acc_word;
      if CCInt.Infix.(CCString.length new_word = 0) then
        words
      else 
        new_word :: words 
    in
    let accumulate acc_words grapheme_cluster =
      let gc = grapheme_cluster in
      if
        not (exclude_grapheme gc) && (
          include_grapheme gc ||
          is_wordgrapheme gc
        )
      then (
        Buffer.add_string acc_word gc;
        acc_words
      ) else (
        maybe_add_word acc_words
      )
    in
    let words = 
      line
      |> Uunf_string.normalize_utf_8 Grapheme.normalization_method
      |> Uuseg_string.fold_utf_8 `Grapheme_cluster accumulate []
    in
    maybe_add_word words 
    |> CCList.rev

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
          ~include_grapheme:config.Config.include_grapheme
          ~exclude_grapheme:config.Config.exclude_grapheme
          ~from_idx:idx_visual
          line
      in
      (* CCFormat.eprintf "DEBUG: words = %a\n%!" (CCList.pp CCString.pp) words; *)
      let under parent = parent.Line_data.indent < indent in
      let v = Line_data.{ line_data with indent; words } in
      Tree.insert_in_first_path ~under v tree

end
