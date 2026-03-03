
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
               words. See the section on INCLUDED CHARACTERS." in 
    let docv = "CHAR,…" in
    Arg.(value & opt (list string) [] & info ["include-chars"; "c"] ~docv ~doc)
      
  let exclude_chars = 
    let doc = "Exclude this commaseparated list of chars when tokenizing \
               words." in
    let docv = "CHAR,…" in
    Arg.(value & opt (list string) [] & info ["exclude-chars"] ~docv ~doc)

  let match_filter = 
    Arg.(value & vflag `Subtree [
      `Matchtree,    info ["extract-matchtree"; "emt"]
        ~doc:"Extract only the paths of tree that match query exactly, excluding \
              the rest of the tree.";
      `Subtree,      info ["extract-subtree"; "est" ]
        ~doc:"Extract the paths of tree that match query exactly + their \
              subtrees. This is the default.";
      `Fulltree,     info ["extract-fulltree"; "eft"]
        ~doc:"Extract the paths of tree that match query exactly + their \
              subtrees and ancestors.";
      `Completetree, info ["extract-completetree"; "ect"]
        ~doc:"Extract the complete tree where the query match somewhere within. \
              This includes all branches - even those that don't match the \
              query.";
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
    let man = [
      `S Manpage.s_examples;
      `S "INCLUDED CHARACTERS IN WORDS";
      `P "`tatr` finds words in 2 separate passes. One where --include-chars \
          and --exclude-chars are used to find word-boundaries, and one where \
          the given \
          regular expressions are applied. The default word-characters are \
          all Unicode word-characters defined in \
          https://www.unicode.org/reports/tr29/#C2-1 \
          and these additional characters:";
      `P "{ -, _, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }";
    ] in
    let doc = "matching on indented subtrees in structured text-files, by \
               querying tokens contained in their branches" in
    Cmd.(info "tatr" ~doc ~man)
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

