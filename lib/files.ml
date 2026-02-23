
let make_glob regex =
  Re.Glob.glob regex
    (*> match on whole string*)
    ~anchored:true
    ~pathname:true (*slashes not matched by '*'*)
    ~period:true (*dotfiles*)
    ~expand_braces:true
    ~double_asterisk:false
  |> Re.compile
  |> Re.execp

let find_recursively ~match_filename files =
  let regex = make_glob match_filename in
  let rec aux file =
    if not @@ Sys.is_directory file then
      CCSeq.pure file
    else 
      Sys.readdir file
      |> CCArray.sorted CCString.compare
      |> CCArray.to_seq
      |> CCSeq.map (Filename.concat file)
      |> CCSeq.filter (fun file ->
        if Sys.is_directory file then true else
          let basename = Filename.basename file in
          regex basename
      )
      |> CCSeq.flat_map aux
  in
  files
  |> CCSeq.of_list
  |> CCSeq.flat_map aux











