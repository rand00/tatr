
open Tatr
open Tree.T

let pretty_print_tree ~get_line_num ~get_line tree =
  let prev_line_num = ref None in
  tree |> Tree.iter_df (fun v ->
    let line_num = get_line_num v in
    begin match !prev_line_num with
      | None -> ()
      | Some prev_line_num ->
        if prev_line_num <> pred line_num then
          CCFormat.printf "......\n%!"
    end;
    (*> Note: important to print tab here - otherwise some tab-based formats
        become visually weird *)
    CCFormat.printf "%05d:\t%s\n%!" line_num (get_line v);
    prev_line_num := Some line_num;
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

module CSet = CCSet.Make(CCChar)

let main
    query
    files
    tab_is_spaces
    include_chars
    exclude_chars
    match_filter
    case_is_significant
    match_filename
  =
  let query = match query with
    | None -> failwith "You need to pass a query - see --help";
    | Some query ->
      let ignore_case = not case_is_significant in
      let query = Query.of_string ~ignore_case query in
      if Tag_regex.Set.is_empty query then
        failwith "You need to pass a query - see --help";
      query |> Tag_regex.Set.iter (fun regex ->
        if regex.Tag_regex.orig_regex = "" then
          failwith "Your queries shouldn't be empty - see --help"
      );
      query
  in
  let include_chars = CSet.of_list include_chars in
  let exclude_chars = CSet.of_list exclude_chars in
  let include_char c = CSet.mem c include_chars in
  let exclude_char c = CSet.mem c exclude_chars in
  let config = Config.{
    tab_is_spaces;
    exclude_char;
    include_char;
  } in
  let get_line_num (v, _) = v.Line_data.line_num
  and get_line     (v, _) = v.Line_data.line
  in
  (*> goto recursively find files if file is dir*)
  files
  |> Files.find_recursively ~match_filename
  |> CCSeq.iter (fun file ->
    In_channel.with_open_text file (fun in_chan ->
      let rec loop unused_line_data =
        let tree, unused_line_data, read_more =
          next_tree ~config in_chan unused_line_data
        in
        (* CCFormat.eprintf "DEBUG: loop full tree =\n%a\n%!" (Tree.pp Line_data.pp) tree; *)
        let filtered_tree = match match_filter with
          | `Matchtree ->
            Query.Tree.match_matchtree query tree
          | `Subtree ->
            Query.Tree.match_subtree query tree
          | `Fulltree ->
            Query.Tree.match_fulltree query tree
          | `Completetree ->
            Query.Tree.match_fulltree' query tree
        in
        begin match filtered_tree with
          | Nil -> ()
          | tree ->
            let sep_width = 80 in
            let dashes = CCString.make sep_width '-' in
            let file_title =
              let title = "-- " ^ file ^ " " in
              let title = title |> CCString.pad ~side:`Right ~c:'-' sep_width in
              title
            in
            CCFormat.printf "%s\n%!" file_title;
            tree |> pretty_print_tree ~get_line_num ~get_line;
            (*> goto choose based on term-width?*)
            CCFormat.printf "%s\n%!" dashes;
        end;
        if read_more then loop @@ unused_line_data
      in
      loop None
    )
  )

let () = Cli.apply main


