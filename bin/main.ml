
module A = Angstrom

module Tree = struct

  open CCOption.Infix

  type 'a t =
    | Nil
    | Tree of ('a * 'a t list)

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
          None
      | Tree (parent, (headsub :: tailsub as subtree)) ->
        if under parent then
          Some (Tree (parent, singleton v :: subtree))
        else
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
            | '\n' | '\r' ->
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
    
    let add_line_to_tree
        ~tab_is_spaces
        ~include_char
        ~exclude_char
        line
        tree =
      (*> Note that n-indent is not the same as index
        * tab is counted a specified number of spaces
        * when supporting unicode in the future, whitespace can be of even more sizes
      *)
      let indent, idx_visual =
        count_indent_until_visual ~tab_is_spaces line
      in
      match idx_visual with
      | None -> tree
      | Some idx_visual ->
        let words =
          extract_words
            ~include_char
            ~exclude_char
            ~from_idx:idx_visual
            line
        in
        failwith "todo"

  end

end

let () = print_endline "Hello, World!"
