
open Tree.T

type t = Tag_regex.Set.t
[@@deriving show]

let of_string ?ignore_case str : t =
  CCString.split_on_char ',' str
  |> CCList.map (Tag_regex.make ?ignore_case)
  |> Tag_regex.Set.of_list

let parse ?ignore_case str : (t, string) CCResult.t =
  CCResult.guard_str (fun () -> of_string ?ignore_case str)

module Tree = struct

  module RSet = Tag_regex.Set

  let rec is_complete = function
    | Nil -> false
    | Tree ((_, path_is_complete'), children) ->
      path_is_complete'
      || CCList.exists is_complete children

  let match_completetree (query:t) tree : _ Tree.t =
    let rec aux acc_path_matches = function
      | Nil -> Nil
      | Tree (v, children) ->
        let matches, _ =
          query |> RSet.partition (fun regex ->
            v.Line_data.words |> CCList.exists regex.Tag_regex.prop 
          )
        in
        let acc_path_matches = RSet.union matches acc_path_matches in
        let path_is_complete = RSet.(cardinal acc_path_matches = cardinal query) in
        let matching_children = aux_children acc_path_matches children in
        Tree ((v, path_is_complete), matching_children)
    and aux_children acc_path_matches children = 
      children |> CCList.filter_map (fun child ->
        match aux acc_path_matches child with
        | Nil -> None
        | tree -> Some tree
      )
    in
    let checked_tree = aux RSet.empty tree in
    if is_complete checked_tree then
      checked_tree
    else
      Nil

  (*> Note for the following algorithms - the 'is_complete' field of children
      is now propagated to parents, which optimizes for how far down in tree we
      need to look to find if subtree is complete. We can do this because for
      now we have no other use for 'is_complete' than checking if whole subtree
      contains complete children *)
  
  (**
     @param include_ancestors: if children match - include their ancestors
     @param include_subtree: for every branch that matches all tags in query,
     include the whole subtree beneath that branch
  *)
  let match_matchtree
      ?(include_subtree=false)
      ?(include_ancestors=false)
      (query:t)
      tree
    : _ Tree.t
    =
    let rec aux ancestor_matched acc_path_matches = function
      | Nil -> Nil
      | Tree (v, children) ->
        let matches, _ =
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
                Tree ((v, path_is_complete || any_child_is_complete), matching_children)
              else
                Nil
            )
          ) else ( (*don't include subtree of matching branches*)
            (*> @goto brian; note that this CCList.filter is running for each level of the tree
                .. is this optimal enough? - could be more?
            *)
            let matching_children =
              aux_children true acc_path_matches children
              |> CCList.filter is_complete
            in
            let any_child_is_complete = 
              not @@ CCList.is_empty matching_children
            in
            (*> Note: filtering incomplete paths in tree away*)
            if path_is_complete || any_child_is_complete then
              Tree ((v, path_is_complete || any_child_is_complete), matching_children)
            else
              Nil
          )
        ) else if ancestor_matched then (
          let matching_children =
            aux_children true acc_path_matches children
            |> CCList.filter is_complete
          in
          match matching_children with
          | [] -> Nil
          | children ->
            Tree ((v, true (*.. as some child is complete*)), children)
        ) else ((*havn't found any match yet*)
          let matching_children =
            aux_children false acc_path_matches children
            |> CCList.filter is_complete
          in
          match matching_children with
          | [] -> Nil
          | [ child ] when not include_ancestors -> child
          | children ->
            Tree ((v, true (*.. as some child is complete*)), children)
        )
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
  
  let match_fulltree (query:t) tree : _ Tree.t =
    match_matchtree ~include_ancestors:true ~include_subtree:true query tree
  
end
