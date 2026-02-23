
open Cmdliner
open Tatr

module A = struct

  let query =
    let doc = "A commaseparated list of POSIX regular expressions that \
               are queried for in the given white-space indented trees. \
               The comma means AND." in
    let docv = "REGEX,…" in
    (* let format_conv = Arg.conv' Query.(parse, pp) in *)
    Arg.(value & pos 0 (some string) None & info [] ~docv ~doc)

  let files =
    let doc = "The files and directories to look for trees within." in
    let docv = "FILE" in
    Arg.(value & pos_right 0 file [] & info [] ~docv ~doc)

  let tab_is_spaces = 
    let doc = "Baba is you, and tab is `n` spaces." in
    let docv = "INT" in
    Arg.(value & opt int 4 & info ["tab-is-spaces"] ~docv ~doc)
      
  let include_chars = 
    let doc = "Include this commaseparated list of chars when tokenizing \
               words." in
    let docv = "CHAR,…" in
    Arg.(value & opt (list char) [] & info ["include-chars"] ~docv ~doc)
      
  let exclude_chars = 
    let doc = "Exclude this commaseparated list of chars when tokenizing \
               words." in
    let docv = "CHAR,…" in
    Arg.(value & opt (list char) [] & info ["exclude-chars"] ~docv ~doc)

  let match_filter = 
    Arg.(value & vflag `Subtree [
      `Matchtree, info ["extract-matchtree"]
        ~doc:"Extract only the paths of tree that match query completely.";
      `Subtree, info ["extract-subtree"]
        ~doc:"Extract the paths of tree that match query completely + \
              their subtrees. This is the default.";
      `Fulltree, info ["extract-fulltree"]
        ~doc:"Extract the paths of tree that match query completely + \
              their subtrees and their ancestors.";
      `Completetree, info ["extract-completetree"]
        ~doc:"Extract the complete tree where the query match somewhere within.";
    ])
  
  let case_is_significant = 
    let doc = "Character-case becomes significant for matching queries." in
    Arg.(value & flag & info ["case-is-significant"] ~doc)
      
  let match_filename =
    let doc = "Regular expression (shell globbing style) for matching on \
               filenames to query on within the given directories." in
    let docv = "REGEX" in
    (* let format_conv = Arg.conv' Query.(parse, pp) in *)
    Arg.(value & opt string "*" & info ["match-file"] ~docv ~doc)

end

let apply main_f =
  let info =
    let doc = "matching on indented trees in any kind of text-format by \
               querying tokens" in
    Cmd.(info "tatr" ~doc)
  in
  let cmd = Cmd.v info
      Term.(
        const main_f
        $ A.query
        $ A.files
        $ A.tab_is_spaces
        $ A.include_chars
        $ A.exclude_chars
        $ A.match_filter
        $ A.case_is_significant
        $ A.match_filename
      )
  in
  Cmd.(eval cmd |> exit)

