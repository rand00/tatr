
type t = {
  tab_is_spaces : int;
  include_grapheme : string (*normalized grapheme*) -> bool;
  exclude_grapheme : string (*normalized grapheme*) -> bool;
}
