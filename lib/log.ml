
let failwith ?(exitcode=1) scope msg =
  CCFormat.eprintf "@{<red>Error: %s:@} %s\n%!" scope msg;
  exit exitcode


