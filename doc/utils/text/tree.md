[§2] (text tree) - Lightweight text generation {#text.tree}
-------------

###### [!Library] `(text tree)` 

Defines simple but commonly used functions for a text construction.

This library is ported from Gauche.


###### [!Generic] `write-tree`  _tree_ _:optional_ _out_

Write out an _tree_ as a tree of text, to the output port _out_.
If the _out_ is omitted, then current output port is used.


###### [!Function] `tree->string`  _tree_

Just calls the `write-tree` method for _tree] using an output
string port, and returns the result string.
_

