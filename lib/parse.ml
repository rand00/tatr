
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

  module Word_alphabet = struct

    let is_utf8 = function
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

    let is_latin1 uchar =
      let ui = Uchar.to_int uchar in
      if
        (ui >= 0x30 && ui <= 0x39) ||
        (ui >= 0x41 && ui <= 0x5a) ||
        (ui >= 0x61 && ui <= 0x7a) ||
        (ui = 0xc280) ||
        (ui = 0xc283) ||
        (ui = 0xc28a) ||
        (ui = 0xc28c) ||
        (ui = 0xc28e) ||
        (ui = 0xc29a) ||
        (ui = 0xc29c) ||
        (ui = 0xc29e) ||
        (ui = 0xc29f) ||
        (ui >= 0xc380 && ui <= 0xc396) ||
        (ui >= 0xc398 && ui <= 0xc3b6) ||
        (ui >= 0xc3b8 && ui <= 0xc6bf) ||
        (ui >= 0xc784 && ui <= 0xcaaf) || 
        (ui >= 0xcdb0 && ui <= 0xcdb3) || 
        (ui >= 0xcdb6 && ui <= 0xcdb7) || 
        (ui >= 0xcdbb && ui <= 0xcdbd) || 
        (ui >= 0xcdbf) || 
        (ui >= 0xce86) || 
        (ui >= 0xce88 && ui <= 0xcfbf) 
          (*< goto check if done; https://www.fileformat.info/info/charset/UTF-8/list.htm *)
      then
        true
      else
        false

    let min_latin1 = 0x30
    let max_latin1 = 0xcaaf

    let all_latin1_str =
      let uchars = 
        CCSeq.init ((max_latin1 - min_latin1) + 1) (fun i ->
          Uchar.of_int @@ min_latin1 + i
        )
        |> CCSeq.filter is_latin1
      in
      let buf = Buffer.create (CCSeq.length uchars * 4) in
      uchars |> CCSeq.iter (Buffer.add_utf_8_uchar buf);
      Buffer.contents buf
      |> Uunf_string.normalize_utf_8 Grapheme.normalization_method

    let match_latin1_words =
      let word_regex =
        let regex = CCFormat.sprintf "[%s]+" all_latin1_str in
        Re.Posix.compile_pat regex
      in
      fun line -> 
        Re.matches word_regex line

  end

  (*> @goto optimize; this is an expensive function based on linux 'perf' report
      * @idea; construct a `re` regex that splits line up into words
        * where all word-graphemes are normalized + inserted in regex expr '[..]+'
          * and 'exclude_grapheme' is used to remove graphemes from this list 
  *)
  (* let extract_words ~include_grapheme ~exclude_grapheme ~from_idx line = *)
  (*   let line = CCString.sub line from_idx (CCString.length line - from_idx) in *)
  (*   let acc_word = Buffer.create 128 in *)
  (*   let maybe_add_word words = *)
  (*     let new_word = Buffer.contents acc_word in *)
  (*     Buffer.clear acc_word; *)
  (*     if CCInt.Infix.(CCString.length new_word = 0) then *)
  (*       words *)
  (*     else  *)
  (*       new_word :: words  *)
  (*   in *)
  (*   let accumulate acc_words grapheme_cluster = *)
  (*     let gc = grapheme_cluster in *)
  (*     if *)
  (*       not (exclude_grapheme gc) && ( *)
  (*         include_grapheme gc || *)
  (*         Word_alphabet.is_utf8 gc *)
  (*       ) *)
  (*     then ( *)
  (*       Buffer.add_string acc_word gc; *)
  (*       acc_words *)
  (*     ) else ( *)
  (*       maybe_add_word acc_words *)
  (*     ) *)
  (*   in *)
  (*   let words =  *)
  (*     line *)
  (*     |> Uunf_string.normalize_utf_8 Grapheme.normalization_method *)
  (*     |> Uuseg_string.fold_utf_8 `Grapheme_cluster accumulate [] *)
  (*   in *)
  (*   maybe_add_word words  *)
  (*   |> CCList.rev *)

  (*> @goto @goo
    * this is much faster than prev version
      [ ] but @debug / @fix that some utf8 chars are not matched correctly (e.g. λ)
        * @idea; don't construct utf8 regex from POSIX str - but use internal Re regex API
  *)
  let extract_words ~include_grapheme ~exclude_grapheme ~from_idx line =
    Word_alphabet.match_latin1_words line

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
