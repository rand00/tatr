# `tatr` - tagged tree matching

`tatr` lets you recursively query into files on your filesystem for tags/words that match the given POSIX regular expressions, and 
extract the exact paths within the indented trees that match your query. E.g.
```bash
> tatr --match-file='*.txt' fix,code ~/my_notes
```

.. where `fix` and `code` are words that need to be present in the matched tree, and `~/my_notes` is a directory to look in.

-----------------
There are several options that allow you to configure `tatr` for your specific format - e.g. `--include-chars` lets you
specify that e.g. `#`/`@` should be included in matched words, so you can limit yourself to match *tags* as specified in 
the text-format:
```bash
> tatr --match-file='*.txt' --include-chars=@ @fix,@code ~/my_notes
```

-----------------
`tatr` can also be used to query your configs. Here we query all `dune` files of your ocaml repository for references to a 
specific library, `containers`:
```bash
> tatr --extract-fulltree --match-file=dune libraries,containers .
-- ./bin/dune ------------------------------------------------------------------
00001:  (executable
......
00013:   (libraries
......
00017:    ;; containers
--------------------------------------------------------------------------------
-- ./lib/dune ------------------------------------------------------------------
00001:  (library
......
00011:   (libraries
00012:    containers
--------------------------------------------------------------------------------
```

.. there are different *extraction* algorithms to let you choose what part of the matched tree is returned.
In this example `--extract-fulltree` both includes the subtree beneath the match (none here) and the ancestors 
towards the root (here `(executable|(library`). 

As the tags are POSIX regular expressions separated by comma, you can express more complex patterns like `tag0,(tag1|tag2)`,
where `tatr` will match on all paths in trees that both include `tag0` and -- `tag1` or `tag2`.

## Related tools

For textual search - often one will use something like a mix of `grep` and `find` - where you can
search for words within a single line - but these tools
don't operate at the tree-level. 

A notetaking system like `zim` let you query for matching pages that contains tags - but not at the indented tree-level.

## History

I've for many years been organizing my notes in [zim](https://github.com/zim-desktop-wiki/zim-desktop-wiki/tree/develop) - 
which lets you organize your notes in trees, that are represented as wiki-files placed in folders on your filesystem, and link your pages in a graph. 
These features are extremely powerful in themselves, for creating your own custom organization for remembering what you are doing
and have done before. 

Another feature of `zim` is *tags* which allow you to select pages that include several custom tags you've made. 

I found that I often create long lists of indented notes within single pages. Where a lot of relatively unrelated things are placed.
What I really want is to be able to query for what specific sections of my pages include a set of tags - and to 
extract these sections.

I realized that we use a lot of other structured formats based on textual indentation to represent trees of related elements - which 
is why `tatr` by default doesn't know about any specific textual format - but can operate solely based on indentation.
This method is e.g. compatible with
* note-taking formats: markdown, wiki, ...
* pretty-printed config formats: json, sexp, xml, ...

## Limitations

There are some limitations of the default method of `tatr` when working with formats that hide their tree-nature within special syntax.
This e.g. includes *headings* from markdown and wiki-formats, certain config-formats like `yaml`, and a lot of syntax in programming languages. 

Future support for some of these formats can be easily added to `tatr` via *pre-indenters* that map the syntax at the beginning of lines to 
a synthetic indentation-level of the following text.






