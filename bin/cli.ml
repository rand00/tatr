
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
    Arg.(value & opt (list char) [] & info ["include-chars"; "c"] ~docv ~doc)
      
  let exclude_chars = 
    let doc = "Exclude this commaseparated list of chars when tokenizing \
               words." in
    let docv = "CHAR,…" in
    Arg.(value & opt (list char) [] & info ["exclude-chars"] ~docv ~doc)

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
    (*> Note: Can't splice in commas between chars simply via this method
      .. and can't even make the resulting string be printed correctly 
         in manual...
    *)
    (* let word_chars_str = *)
    (*   CCSeq.(0 --^ 255) *)
    (*   |> CCSeq.map CCChar.chr *)
    (*   |> CCSeq.filter Parse.Indentation_tree.is_wordchar *)
    (*   |> CCSeq.map CCString.of_char *)
    (*   |> CCString.concat_seq ~sep:"" *)
    (* in *)
    let man = [
      `S Manpage.s_examples;
      `S "INCLUDED CHARACTERS IN WORDS";
      `P "`tatr` finds words in 2 separate passes. One where --include-chars \
          and --exclude-chars are used to find word-boundaries, and one where \
          the given \
          regular expressions are applied. The default word-characters are the \
          following subset of the 'latin1' characterset:";
      (* `P (Manpage.escape @@ (\* CCFormat.sprintf "{ %s }" *\) word_chars_str); *)
      `P "{ -, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F, G, H, I, J, K, L, \
          M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, _, a, b, c, d, e, f, g, h, \
          i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, À, Á, Â, Ã, Ä, \
          Å, Æ, Ç, È, É, Ê, Ë, Ì, Í, Î, Ï, Ð, Ñ, Ò, Ó, Ô, Õ, Ö, Ø, Ù, Ú, Û, Ü, \
          Ý, Þ, ß, à, á, â, ã, ä, å, æ, ç, è, é, ê, ë, ì, í, î, ï, ð, ñ, ò, ó, \
          ô, õ, ö, ø, ù, ú, û, ü, ý, þ }";
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

