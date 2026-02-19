
(**A Rosetree - but with special combinators that fit the problem*)

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
