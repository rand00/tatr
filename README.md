# `tatr` - tagged tree matching

`tatr` lets you recursively query into files on your filesystem for tags/words that match the given POSIX regular expressions, and 
extract the exact paths within the indented trees of the found text-files that match your query. 

## Examples 

### Matching on indentation-trees containing specific words
```bash
> tatr --match-file='*.txt' fix,code ~/my_notes
```
.. where `fix` and `code` are words that need to be present in the matched tree, and `~/my_notes` is a directory to look in.
Indented trees of text in notes usually come in the form of deeply nested indented points. 

I will write a blogpost in the future about how it's useful to take notes this way.

### Configuring `tatr` to include special characters to search for *tags*
There are several options that allow you to configure `tatr` for your specific format - e.g. `--include-chars` lets you
specify that e.g. `#`/`@` should be included in matched words, so you can limit yourself to match *tags* as specified in 
the text-format:
```bash
> tatr --match-file='*.txt' --include-chars=@ @fix,@code ~/my_notes
```

### Querying your configuration-files for specific library dependencies
`tatr` can also be used to query your configs. Here we query all `dune` files of this ocaml repository for references to a 
specific library, `containers`:
```bash
> tatr --extract-fulltree --match-file=dune libraries,containers .
-- ./lib/dune ------------------------------------------------------------------
00001:  (library
......
00011:   (libraries
00012:    containers
--------------------------------------------------------------------------------
```

.. there are different *extraction* algorithms to let you choose what part of the matched tree is returned.
In this example `--extract-fulltree` both includes the subtree beneath the match (none here) and the ancestors 
towards the root (here `library`). 

### Querying your configuration-files for all library dependencies

As the tags are POSIX regular expressions separated by comma, you can express more complex patterns like `tag0,(tag1|tag2)`,
where `tatr` will match on all paths in trees that both include `tag0` and -- `tag1` or `tag2`.

To extract all library dependencies, preprocessors and the name of each executable/library you could do:
```bash
> tatr --eft --match-file=dune '(name|libraries|preprocess)' . 
-- ./bin/dune ------------------------------------------------------------------
00001:  (executable
......
00004:   (name main)
......
00013:   (libraries
00014:    tatr
00015:    cmdliner
00016:    )
00017:   (preprocess
00018:    (pps
00019:     ppx_deriving.std
00020:     )
00021:    )
--------------------------------------------------------------------------------
-- ./lib/dune ------------------------------------------------------------------
00001:  (library
00002:   (name tatr)
......
00011:   (libraries
00012:    containers
00013:    re
00014:    )
00015:   (preprocess
00016:    (pps
00017:     ppx_deriving.std
00018:     )
00019:    )
--------------------------------------------------------------------------------
```

## Related tools

For textual search - often one will use something like a mix of `grep` and `find` - where you can
search for words within a single line - but these tools
don't operate at the tree-level. 

A notetaking system like `zim` let you query for matching pages that contains tags - but not at the indented tree-level.

## History

I've for many years been organizing my notes in [zim](https://github.com/zim-desktop-wiki/zim-desktop-wiki/tree/develop) - 
which lets you organize your notes in trees, that are represented as wiki-files placed in folders on your filesystem, and link your pages in a graph. 
These features are extremely powerful by themselves, for creating your own custom organization for remembering what you are doing
and have done before. 

Another feature of `zim` is *tags* which allow you to select pages that include several custom tags you've made. 

I found that I often create long lists of indented notes within single pages. Where a lot of relatively unrelated things are placed.
What I really want is to be able to query for what specific sections of my pages include a set of tags - and to 
extract these sections.

I realized that we use a lot of other structured formats based on textual indentation to represent trees of related elements - which 
is why `tatr` by default doesn't know about any specific textual format - but operates solely based on indentation.
This method is e.g. compatible with
* note-taking formats: markdown, wiki, ...
* pretty-printed config formats: json, sexp, xml, ...

## Limitations

There are some limitations of the default method of `tatr` when working with formats that hide their tree-nature within special syntax.
This e.g. includes *headings* from markdown and wiki-formats, certain config-formats like `yaml`, and a lot of syntax in programming languages. 

Future support for some of these formats can be easily added to `tatr` via *pre-indenters* that map the syntax at the beginning of lines to 
a synthetic indentation-level of the following text.

Another problem is if your structured format is not *pretty-printed* within each file - so the structure is not laid out via indentation.
To solve this you can pass your structured format to some pretty-printer like: `cat my.json | jq '.' > my_pretty.json`. 
`tatr` could possible get a feature to apply a user-specified script to text-files on recursive traversal. 

A current ideal of `tatr` is to be as independent as possible from specific formats - and let the user rely on existing tools to make the given 
text compatible with the `tatr` indentation-based interpretation.




