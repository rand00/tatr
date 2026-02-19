
open Tree.T

type t = Tag_regex.Set.t 

let of_string str : t =
  CCString.split_on_char ',' str
  |> CCList.map Tag_regex.make
  |> Tag_regex.Set.of_list

module Tree = struct

  module RSet = Tag_regex.Set

  let rec is_complete = function
    | Nil -> false
    | Tree ((v, path_is_complete'), children) ->
      path_is_complete'
      || CCList.exists is_complete children
        
  let match_fulltree tree : _ Tree.t =
    failwith "todo"

  let match_matchtree ?(include_subtree=false) (query:t) tree : _ Tree.t =
    let rec aux ancestor_matched acc_path_matches = function
      | Nil -> Nil
      | Tree (v, children) ->
        let matches, non_matches =
          query |> RSet.partition (fun regex ->
            v.Line_data.words |> CCList.exists regex.Tag_regex.prop 
          )
        in
        let acc_path_matches = RSet.union matches acc_path_matches in
        (*> Note: we need to continue to look even when complete already*)
        let path_is_complete = RSet.(cardinal acc_path_matches = cardinal query) in
        let found_match = not @@ RSet.is_empty matches in
        if found_match then (
          if include_subtree then (
            if path_is_complete then (
              let children =
                children
                |> CCList.map (Tree.map (fun v -> (v, false)))
              in
              Tree ((v, path_is_complete), children)
            ) else (
              let matching_children =
                aux_children true acc_path_matches children
                |> CCList.filter is_complete
              in
              let any_child_is_complete = 
                not @@ CCList.is_empty matching_children
              in
              (*> Note: filtering incomplete paths in tree away*)
              if any_child_is_complete then
                Tree ((v, path_is_complete), matching_children)
              else
                Nil
            )
          ) else ( (*Prune branches away that don't match*)
            let matching_children =
              aux_children true acc_path_matches children
              |> CCList.filter is_complete
            in
            let any_child_is_complete = 
              not @@ CCList.is_empty matching_children
            in
            (*> Note: filtering incomplete paths in tree away*)
            if path_is_complete || any_child_is_complete then
              (*> goto @brian; would it be an optimization if we propagated that
                  some child is complete to our node?
                  * then the 'is_complete' might shortcircuit faster?
                    * though it's already good that 'is_complete' is depth-first
              *)
              Tree ((v, path_is_complete), matching_children)
            else
              Nil
          )
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

  let match_subtree (query:t) tree : _ Tree.t =
    match_matchtree ~include_subtree:true query tree
  
end
