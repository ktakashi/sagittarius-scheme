[§2] (text yaml) -- YAML parser {#text.yaml}
-------------

###### [!Library] `(text yaml)` 

This library provides YAML parser and writer.

###### [!Function] `yaml-read`  _in_

Reads YAML documents from the given textual input port _in_ and
returns a list of S-expression represented YAML documents.

The returning YAML documents are the compatible with vector JSON
format described in [(text json)](#text.json).

``````````scheme
(yaml-read (open-string-input-port "
%YAML 1.2
---
foo: bar
boo:
- 1
- 2
- 3
- 4"))
``````````
=> ``'(#((foo . bar) (boo 1 2 3 4)))``



###### [!Function] `yaml-write`  _yaml_
###### [!Function] `yaml-write`  _yaml_ _out_

Writes the given list of S-expression represented YAML documents
_yaml_ to the _out_.

If the first form is used, then it writes to `(current-output-port)`.

``````````scheme
(yaml-write '(#(("foo" . "bar") ("boo" 1 2 3 4))))
;; Prints the following
#|
%YAML 1.2
---
foo: bar
boo:
- 1
- 2
- 3
- 4
...
|#
``````````



