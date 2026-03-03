
let normalization_method = `NFKC

let normalize grapheme =
  let maybe_grapheme = 
    grapheme
    |> Uunf_string.normalize_utf_8 normalization_method
    |> Uuseg_string.fold_utf_8 `Grapheme_cluster CCList.cons' []
  in
  match maybe_grapheme with
  | [ grapheme ] -> grapheme
  | _ :: _ :: _ ->
    Log.failwith "Grapheme.normalize"
      "You passed too many grapheme clusters"
  | [] -> 
    Log.failwith "Grapheme.normalize"
      "You passed an empty grapheme-cluster"

(*> Warning: this depends on all graphemes having been normalized
    .. both the ones in the set + the given graphemes to be checked
*)
module Set = struct
  include CCSet.Make(CCString)

  let of_list graphemes =
    graphemes
    |> CCList.map normalize
    |> of_list
         
end
