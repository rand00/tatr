
type t = {
  indent : int;
  words : string list;
  line : string;
  line_num : int;
} [@@deriving show]

let init ~line ~line_num = {
  line;
  line_num;
  indent = 0;
  words = [];
}
