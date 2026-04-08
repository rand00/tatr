
let failwith ?(exitcode=1) scope msg =
  CCFormat.eprintf "@{<red>Error: %s:@} %s\n%!" scope msg;
  exit exitcode

let warn scope msg =
  CCFormat.eprintf "@{<red>Warning: %s:@} %s\n%!" scope msg



