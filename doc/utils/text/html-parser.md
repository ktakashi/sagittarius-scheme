[§2] (text sxml html-parser) - Flxible HTML parser {#text.html-parser}
-------------

###### [!Library] `(text sxml html-parser)` 

This library provides HTML parser and printer procedures.

The provided procedures are compatible with HTMLPrag.


### [§3] High level APIs

###### [!Function] `html->sxml-0nf`  _in_
###### [!Function] `html->sxml-1nf`  _in_
###### [!Function] `html->sxml-2nf`  _in_
###### [!Function] `html->sxml`  _in_
###### [!Function] `html->shtml`  _in_

Parse given input port to SHTML. The _in_ must be a textual input
port.

The `html->sxml-2nf` procedure normalizes attributes if it doesn't have
value. Others don't. Normalization in this context means adding attribute name
as its value.


###### [!Function] `write-shtml-as-html`  _shtml_ _:optional_ _out_ _error-filter_

Writes given _shtml_ to _out_.

If optional argument _out_ is given, it must be a textual output port,
then the procedure writes to given output port. Otherwise 
`current-output-port` is used.

Optional argument _error-filter_ is used when unexpected SHTML entity is
met. It must be a procedure takes 2 arguments, unexpected entity and boolean
flag indicates if the entity is attribute or not. By default, it raises an
error.


###### [!Function] `shtml->html`  _shtml_

Converts _shtml_ to string represented HTML.

### [§3] Low level APIs

###### [!Variable] `shtml-comment-symbol` 
###### [!Variable] `shtml-decl-symbol` 
###### [!Variable] `shtml-empty-symbol` 
###### [!Variable] `shtml-end-symbol` 
###### [!Variable] `shtml-entity-symbol` 
###### [!Variable] `shtml-pi-symbol` 
###### [!Variable] `shtml-start-symbol` 
###### [!Variable] `shtml-text-symbol` 
###### [!Variable] `shtml-top-symbol` 

SXML entity symbols: `*COMMENT*`, `*DECL*`, `*EMPTY*`,
`*END*`, `*ENTITY*`, `*PI*`, `*START*`, `*TEXT*` and
`*TOP*` respectively.


###### [!Variable] `shtml-named-char-id` 
###### [!Variable] `shtml-numeric-char-id` 

These variables are used in `*ENTITY*` as public identifier.


###### [!Function] `make-shtml-entity`  _value_

Makes SHTML character entity. The _value_ must be a symbol, string
or integer.

``(make-shtml-entity 151)`` => ``(& 151)``



###### [!Function] `shtml-entity-value` 

Retrieve SHTML entity value.


