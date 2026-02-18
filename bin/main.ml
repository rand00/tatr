
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

  end

end

let () = print_endline "Hello, World!"
