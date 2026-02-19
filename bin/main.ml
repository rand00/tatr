

module Config = struct

  type t = {
    tab_is_spaces : int;
    include_char : char -> bool;
    exclude_char : char -> bool;
  }

end

module Tree = struct

  module T = struct
  
    type 'a t =
      | Nil
      | Tree of ('a * 'a t list)
    [@@deriving show]

  end
  include T

  open CCOption.Infix

  let empty = Nil

  let singleton v = Tree (v, [])

  (** Insertion within the first path towards leaf*)
  let insert_in_first_path ~under v tree =
    let rec aux = function
      | Nil -> Some (singleton v)
      | Tree (parent, []) ->
        if under parent then
          Some (Tree (parent, [ singleton v ]))
        else
          (*> Note: returning None can possibly be interpreted as tree
              being 'done' and a new one should be started instead
              .. this depends on how 'under' is defined
          *)
          None
      | Tree (parent, (headsub :: tailsub as subtree)) ->
        if under parent then (
          let try_insert_further_down = aux headsub in
          match try_insert_further_down with
          | None ->
            Some (Tree (parent, singleton v :: subtree))
          | Some headsub' ->
            Some (Tree (parent, headsub' :: tailsub))
        ) else
          let+ headsub = aux headsub in
          Tree (parent, headsub :: tailsub)
    in
    aux tree
        
  let rec rev = function
    | Nil -> Nil
    | Tree (v, subtree) ->
      let subtree =
        subtree
        |> CCList.rev
        |> CCList.map rev
      in
      Tree (v, subtree)

  let map f tree =
    let rec aux = function
      | Nil -> Nil
      | Tree (v, subtree) -> Tree (f v, CCList.map aux subtree)
    in
    aux tree

  let iter_df f tree =
    let rec aux = function
      | Nil -> ()
      | Tree (v, subtree) ->
        f v;
        aux_depth subtree
    and aux_depth = function
      | [] -> ()
      | head :: tail ->
        aux head;
        aux_depth tail
    in
    aux tree

end

open Tree.T

module Line_data = struct

  type t = {
    indent : int;
    words : string list;
    line : string;
    line_num : int;
  } [@@deriving show]

  let init ~line ~line_num = {
    line;
    line_num;
    indent = 0;
    words = [];
  }

end

module Parse = struct

  module Indentation_tree = struct

    let count_indent_until_visual ~tab_is_spaces line =
      let n_indent, _, idx_visual = 
        let exception Shortcircuit of (int * int * int option) in
        try
          line |> CCString.fold_left (fun (n_indent, idx, idx_visual as acc) ->
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

end

module Tag_regex = struct

  module T = struct

    type t = {
      id : int;
      orig_regex : string;
      prop : string -> bool;
    }

    let compare x y = CCInt.compare x.id y.id

  end
  include T

  (*> Warning; don't like it's sematics - the only advantage is that you can
      avoid writing as many chars in regex to match on any seq of chars*)
  let make_glob regex =
    Re.Glob.glob regex
      (*> match on whole string*)
      ~anchored:true
      ~pathname:false
      ~period:false
      ~expand_braces:true
      ~double_asterisk:false
    |> Re.compile
    |> Re.execp

  let make_posix regex =
    (*> Note: wrapping in start/end-line tag so this is not a partial matcher
        by default *)
    Re.Posix.compile_pat ("^"^regex^"$")
    |> Re.execp

  let make =
    let id = ref 0 in
    fun tag_regex ->
      let id' = !id in
      incr id;
      let prop = make_posix tag_regex in
      { id = id'; orig_regex = tag_regex; prop }

  module Set = CCSet.Make(T)

end

module Query = struct

  type t = Tag_regex.Set.t 

  let of_string str : t =
    CCString.split_on_char ',' str
    |> CCList.map Tag_regex.make
    |> Tag_regex.Set.of_list

  module Tree = struct

    module RSet = Tag_regex.Set

    (*> goto goo; *)
    let match_subtree tree : _ Tree.t =
      failwith "todo"

    let match_fulltree tree : _ Tree.t =
      failwith "todo"

    let match_matchtree (query:t) tree : _ Tree.t =
      let rec aux ancestor_matched = function
        | Nil -> Nil
        | Tree (v, subtree) ->
          (* CCFormat.eprintf "DEBUG: words = %a\n%!" *)
          (*   (CCList.pp CCString.pp) v.Line_data.words; *)
          let found_match =
            query |> RSet.exists (fun regex ->
              v.Line_data.words |> CCList.exists regex.Tag_regex.prop 
            )
          in
          (* CCFormat.eprintf "DEBUG: found-match = %b\n%!" found_match; *)
          if found_match then
            Tree (v, aux_depth true subtree)
          else if ancestor_matched then
            let matching_children = aux_depth true subtree in
            match matching_children with
            | [] -> Nil
            | children -> Tree (v, children)
          else
            let matching_children = aux_depth false subtree in
            match matching_children with
            | [] -> Nil
            | [ child ] -> child
            | children -> Tree (v, children)
      and aux_depth ancestor_matched children = 
        children |> CCList.filter_map (fun child ->
          match aux ancestor_matched child with
          | Nil -> None
          | tree -> Some tree
        )
      in
      aux false tree

  end

end


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

open CCOption.Infix 

let pretty_print_tree tree =
  tree |> Tree.iter_df (fun line_data ->
    (*> Note: important to print tab here - otherwise some tab-based formats
        become visually weird *)
    CCFormat.printf "%05d:\t%s\n%!"
      line_data.Line_data.line_num
      line_data.Line_data.line;
  )

(*? gomaybe fix that some lines are dropped from output - line numbers are
    correct now; so is probably just empty lines that are filtered away via parser?*)
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
  (*> goto change back (ctrl via CLI)*)
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
  In_channel.with_open_text file (fun in_chan ->
    let rec loop unused_line_data =
      let tree, unused_line_data, read_more =
        next_tree ~config in_chan unused_line_data
      in
      (* CCFormat.eprintf "DEBUG: loop full tree =\n%a\n%!" (Tree.pp Line_data.pp) tree; *)
      (*> goto switch matching-function out based on CLI-param*)
      let filtered_tree = Query.Tree.match_matchtree query tree in
      begin match filtered_tree with
        | Nil -> ()
        | tree -> 
          pretty_print_tree tree;
          CCFormat.printf "%s\n%!"
            "-----------------------------------------------------------------\
             ---------------";
      end;
      if read_more then loop @@ unused_line_data
    in
    loop None
  )
