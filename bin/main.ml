
open Tatr
open Tree.T

(*goto howto;
  * POC-version
    [*] parse user tag-query given via argv:
      * split on commas
      * map with Re.Glob + Re.compile + Re.execp into a proposition (word -> bool)
      * if there are no tag-regexes / empty-string - fail
    [*] read lines incrementally (using In_channel) from argv file given
    [*] incrementally append lines to tree using Parse.Indentation_tree.add_line
      * when returns None - the tree is done
        * (remember to reuse the line that lead to None)
        * query the prev tree with user-supplied tag-query:
          [ ] have 3 different tree-extractors based on user-config (that can query tree):
            * [ fulltree; subtree; matchtree ]
              [*] (in POC just hardcode which one we choose)
            * where user query is tried against each Line_data within a branch
              * each tag-regex is tested agains the nodes from the root towards each branch leaf
                * on node:
                  * when a tag-regex is matched,
                    * the regex it's removed from testing
                      * if there are no more tag-regexes left,
                        * return Some tree (depending on version of extractor - include subtree)
                    * recurse
                * on leaf :
                  * if there are more unmatched tag-regexes, then return None
      [*] if the queried tree is not Nil - then
        [*] pretty-print
          * the lines extracted
          * a line to fast-open the file in editor/less/zim
          * a line-separator
        * else don't print anything
*)

let pretty_print_tree ~get_line_num ~get_line tree =
  tree |> Tree.iter_df (fun v ->
    (*> Note: important to print tab here - otherwise some tab-based formats
        become visually weird *)
    CCFormat.printf "%05d:\t%s\n%!" (get_line_num v) (get_line v);
  )

let next_tree ~config in_chan unused_line_data =
  let rec aux line_num tree =
    let line_opt = In_channel.input_line in_chan in
    match line_opt with
    | None ->
      let read_more = false in
      Tree.rev tree, None, read_more
    | Some line -> 
      let line_data = Line_data.init ~line ~line_num in
      let new_tree = 
        Parse.Indentation_tree.add_line ~config
          line_data
          tree
      in
      match new_tree with
      | None ->
        let read_more = true in
        Tree.rev tree, Some line_data, read_more
      | Some tree -> aux (succ line_num) tree
  in
  let line_num, init_tree = match unused_line_data with
    | None -> 1, Tree.empty
    | Some unused_line_data -> 
      let tree =
        Parse.Indentation_tree.add_line ~config
          unused_line_data
          Tree.empty
        |> CCOption.get_exn_or "Error: first line couldn't be added"
      in
      (*> Note: this +1 is for the _next_ line-number via aux recursive call
          .. as is done in the inner aux recursion *)
      unused_line_data.line_num +1, tree
  in
  aux line_num init_tree

let () =
  let query = Sys.argv.(1) |> Query.of_string in
  let file = Sys.argv.(2) in
  let tab_is_spaces = 4 in
  (* let include_char = function *)
  (*   | '@' -> true *)
  (*   | _ -> false in *)
  let include_char _ = false in
  let exclude_char _ = false in
  let config = Config.{
    tab_is_spaces;
    exclude_char;
    include_char;
  } in
  let get_line_num (v, _) = v.Line_data.line_num
  and get_line     (v, _) = v.Line_data.line
  in
  In_channel.with_open_text file (fun in_chan ->
    let rec loop unused_line_data =
      let tree, unused_line_data, read_more =
        next_tree ~config in_chan unused_line_data
      in
      (* CCFormat.eprintf "DEBUG: loop full tree =\n%a\n%!" (Tree.pp Line_data.pp) tree; *)
      (*> goto switch matching-function out based on CLI-param*)
      (* let filtered_tree = Query.Tree.match_matchtree query tree in *)
      let filtered_tree = Query.Tree.match_subtree query tree in
      begin match filtered_tree with
        | Nil -> ()
        | tree -> 
          tree |> pretty_print_tree ~get_line_num ~get_line;
          CCFormat.printf "%s\n%!"
            "-----------------------------------------------------------------\
             ---------------";
      end;
      if read_more then loop @@ unused_line_data
    in
    loop None
  )
