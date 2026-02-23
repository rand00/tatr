
let make_glob regex =
  Re.Glob.glob regex
    ~anchored:true (*match on whole string*)
    ~pathname:true (*slashes not matched by '*'*)
    ~period:true   (*dotfiles need to be matched explicitly*)
    ~expand_braces:false
    ~double_asterisk:false
  |> Re.compile
  |> Re.execp

let find_recursively ~match_filename files =
  let matching_filename = make_glob match_filename in
  let rec aux file =
    if not @@ Sys.is_directory file then
      CCSeq.pure file
    else 
      Sys.readdir file
      |> CCArray.sorted CCString.compare
      |> CCArray.to_seq
      |> CCSeq.map (Filename.concat file)
      |> CCSeq.filter (fun file ->
        Sys.is_directory file || (
          matching_filename (Filename.basename file)
        )
      )
      |> CCSeq.flat_map aux
  in
  files
  |> CCSeq.of_list
  |> CCSeq.flat_map aux











