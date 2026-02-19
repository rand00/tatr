
open Tree.T

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

  (*> goto goo; fix that each tag-matcher should match at least _once_ per
      matched branch in returned tree *)
  let match_matchtree (query:t) tree : _ Tree.t =
    let rec is_complete = function
      | Nil -> false
      | Tree ((v, path_is_complete'), children) ->
        path_is_complete'
        || CCList.exists is_complete children
    in
    let rec aux ancestor_matched acc_path_matches = function
      | Nil -> Nil
      | Tree (v, children) ->
        (* CCFormat.eprintf "DEBUG: words = %a\n%!" *)
        (*   (CCList.pp CCString.pp) v.Line_data.words; *)
        let matches, non_matches =
          query |> RSet.partition (fun regex ->
            v.Line_data.words |> CCList.exists regex.Tag_regex.prop 
          )
        in
        let acc_path_matches = RSet.union matches acc_path_matches in
        (*> Note: we need to continue to look even when complete already*)
        let path_is_complete = RSet.(cardinal acc_path_matches = cardinal query) in
        let found_match = not @@ RSet.is_empty matches in
        (* CCFormat.eprintf "DEBUG: found-match = %b\n%!" found_match; *)
        if found_match then (
          let matching_children =
            aux_children true acc_path_matches children
            |> CCList.filter is_complete
          in
          let any_child_is_complete = 
            not @@ CCList.is_empty matching_children
          in
          (*> Note: filtering incomplete paths in tree away*)
          if path_is_complete || any_child_is_complete then
            Tree ((v, path_is_complete), matching_children)
          else
            Nil
        ) else if ancestor_matched then (
          let matching_children =
            aux_children true acc_path_matches children
            |> CCList.filter is_complete
          in
          let any_child_is_complete = 
            not @@ CCList.is_empty matching_children
          in
          match matching_children with
          | [] -> Nil
          | children -> Tree ((v, path_is_complete), children)
        ) else
          let matching_children =
            aux_children false acc_path_matches children
            |> CCList.filter is_complete
          in
          let any_child_is_complete = 
            not @@ CCList.is_empty matching_children
          in
          match matching_children with
          | [] -> Nil
          | [ child ] -> child
          | children -> Tree ((v, path_is_complete), children)
    and aux_children ancestor_matched acc_path_matches children = 
      children |> CCList.filter_map (fun child ->
        match aux ancestor_matched acc_path_matches child with
        | Nil -> None
        | tree -> Some tree
      )
    in
    aux false RSet.empty tree

end
