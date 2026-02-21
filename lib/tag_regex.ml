
module T = struct

  type t = {
    id : int;
    orig_regex : string;
    prop : string -> bool;
  }
  [@@deriving show]

  let compare x y = CCInt.compare x.id y.id

end
include T

let pp_t = pp

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

let make_posix ?(ignore_case=true) regex =
  let opts = if ignore_case then [ `ICase ] else [] in
  (*> Note: wrapping in start/end-line tag so this is not a partial matcher
      by default *)
  Re.Posix.compile_pat ~opts ("^"^regex^"$")
  |> Re.execp

let make =
  let id = ref 0 in
  fun ?ignore_case tag_regex ->
    let id' = !id in
    incr id;
    let prop = make_posix ?ignore_case tag_regex in
    { id = id'; orig_regex = tag_regex; prop }

module Set = struct
  include CCSet.Make(T)
  let pp = pp pp_t
end
