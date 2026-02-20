
open Cmdliner
open Tatr

module A = struct

  let query =
    let doc = "A commaseparated list of POSIX regular expressions that \
               are queried for in the given white-space indented trees. \
               The comma means AND." in
    let docv = "REGEX,…" in
    let format_conv = Arg.conv' Query.(parse, pp) in
    Arg.(value & pos 0 (some format_conv) None & info [] ~docv ~doc)

  let files =
    let doc = "The files and directories to look for trees within." in
    let docv = "FILE" in
    Arg.(value & pos_right 0 file [] & info [] ~docv ~doc)

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
      )
  in
  Cmd.(eval cmd |> exit)

