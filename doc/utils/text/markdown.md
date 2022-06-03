[§2] (text markdown) -- Markdown parser and converter {#text.markdown}
--------------------------------

###### [!Library] `(text markdown)` 

This library provides markdown parser.

The library consists with 3 parts, first one is the parser, second one is the
converter which converts markdown node to HTML. And the last one is the
extensions and its APIs.


A simple example of how to generate HTML snippet from Markdown document
```scheme
(import (rnrs)
        (text markdown)
        (text sxml serializer))

(define markdown-doc "
Hello markdown
==============

- list
- list2
")

(srl:sxml->html-noindent
  (convert-markdown
    (parse-markdown markdown-parser (open-string-input-port markdown-doc))
	default-markdown-converter 'html))
```
=> ``"<h1>Hello markdown</h1>\n<ul>\n<li>list</li>\n<li>list2</li>\n</ul>\n"``

### [§3] Markdown parser APIs

###### [!Function] `markdown-parser?`  _obj_

Returns `#t` if the given _obj_ is a Markdown parser, otherwise `#f`.

###### [!Function] `markdown-node?`  _obj_

Returns `#t` if the given _obj_ is a Markdown node, otherwise `#f`.

###### [!Function] `parse-markdown`  _parser_
###### [!Function] `parse-markdown`  _parser_ _input-port_

_parser_ must be a Markdown parser.  
If the second form is used, then _input-port_ must be a textual input port.
If the first form is used, then `(current-input-port)` will be used.  
Parse Markdown document retrieved from the given textual port and returns
Markdown node.

###### [!Function] `markdown-parser:parse`  _parser_
###### [!Function] `markdown-parser:parse`  _parser_ _input-port_

Alias of the `parse-markdown` for better naming matching.

###### [!Variable] `markdown-parser`

Sagittarius default Markdown parser, this parser supports the following
Markdown syntax:

- [Commonmark 0.30](https://spec.commonmark.org/0.30/)
- [GFM table syntax](https://github.github.com/gfm/#tables-extension-)
- [Strikethrough](https://github.github.com/gfm/#strikethrough-extension-)
- [Task list item](https://github.github.com/gfm/#task-list-items-extension-)
- Definition lists: see [Supported syntax](#markdown.supported.syntax)
- Footnote: see [Supported syntax](#markdown.supported.syntax)

###### [!Variable] `commonmark-parser`

Strictly complying commonmark specification parser. This parser only
supports Commonmark syntax.

#### [§4] Supported syntax

For those well-known syntax, please refer the appropriate links listed 
on the `markdown-parser`.

##### Definition lists

Definition lists can be written like this:

```markdown
definition
: description
  can also be multiple lines
  
definition2
: description of definition2
```

##### Footnotes

Footnote can be written like this:

```markdown
Lorem ipsum[^lorem]

^[lorem]: dolor sit amet
```

### [§3] Markdown converter APIs

###### [!Function] `markdown-converter?` _obj_

Returns `#t` if the given _obj_ is a Markdown converter, otherwise `#f`.

###### [!Variable] `default-markdown-converter`

A default converter. This converter supports the below converters and
most of the extensions.

###### [!Variable] `markdown->html-converter`

A markdown converter which converts Markdown node to HTML (SXML).  
The result SXML can be converted to string by using `srl:sxml->html-noindent`
defined in `(text sxml serializer)` library.

NOTE: this converter only supports commonmark nodes. So if you want to use
GFM or other extensions, use `default-markdown-converter` or create a custom
converter.

###### [!Variable] `markdown->sxml-converter`

A markdown converter which converts Markdown node to XML (SXML).  
The result XML is mostly complies the commonmark DTD.

###### [!Function] `convert-markdown` _node_ _converter_ _type_
###### [!Function] `convert-markdown` _node_ _converter_ _type_ _options_

Converts given _node_ to _type_ by using given _converter_.  
If the second form is used then _options_ must be a Markdown conversion
options object.

###### [!Function] `markdown-converter:convert` _converter_ _type_ _node_
###### [!Function] `markdown-converter:convert` _converter_ _type_ _node_ _options_

Alias of the `convert-markdown` with different argument order for better
name match.

###### [!Function] `markdown-converter:merge` _converter0_ _converter1_ _..._

Merge given converters to one newly allocated converter.

###### [!Function] `markdown-conversion-options?` _obj_

Returns `#t` if the given _obj_ is a Markdown conversion options,
otherwise `#f`.

###### [!Macro] `markdown-conversion-options-builder` 

A record builder macro. The field can be specified on this macro is below

`unknown-node-handler`
: A procedure must accept one argument, which is a markdown node.  
  This is called when the converter doesn't know how to handle the
  given node.

`context-date`
: A context data for the converter. The value depends on the type of converter.


### [§3] Deprecated APIs

This section will be removed in the future release.

Below APIs are supported for backward compatibility. New application shouldn't
use these APIs.

###### [!Function] `markdown-read`  _in_ _:key_ _(as_ _'sxml)_ _:allow-other-keys_ _opt_

Reads markdown from given input port _in_.

The keyword argument _as_ specifies the return value. Following 3 are
supported:

`sxml`
: Returns SHTML. The procedure uses
     `markdown-sexp->sxml` to convert raw markdown S-expression.

`html`
: Returns HTML string.  The procedure uses
     `markdown-sexp->string` to convert raw markdown S-expression.

`sexp`
: Returns S-expression representation of markdown.

Parsing markdown is done by `parse-markdown`. The rest argument _opt_is passed to both `parse-markdown` and underlying convertion procedure.
So if you want to read a markdown document as an HTML string without
indentation, then the procedure should be called like the following:

``````````scheme
(markdown-read in :as 'html :no-indent #t)
``````````

###### [!Function] `string->markdown` _string_ _:rest_ _opt_

Reads markdown from given _string_.

This procedure is thin wrapper of `markdown-read`. It opens string
input port of _string_ and call the `markdown-read`.

The rest argument _opt_ is passed to `markdown-read`.


###### [!Function] `markdown-sexp->sxml` _sexp_ _:key_ _(no-reference #t)_ _(no-notes #f)_ _:allow-other-keys_

Converts given markdown S-expression _sexp_ to SXML.

The keyword arguments _no-reference_ and _no-notes_ control the
result SXML to have reference section and note section. If the values are
true values, then returning SXML doesn't have reference or note section.


###### [!Function] `markdown-sexp->string`  _sexp_ _:key_ _(no-indent #f)_ _:allow-other-keys_ _opts_

Converts given markdown S-expression _sexp_ to HTML string.

The procedure first calls `markdown-sexp->sxml` passing _sexp_ and
_opts_ then converts the returned value to HTML string.

The keyword argument _no-indent_ controls if the returning string
has indentation or not. If the value is true value, then returning string
doesn't have indentation.


###### [!Function] `markdown-parser-error?`  _obj_

Returns #t if the given object is a markdown parser error condition,
otherwise #f.


###### [!Function] `markdown-parser-position`  _condition_
###### [!Function] `markdown-parser-expected`  _condition_

Returns `position` and `expexted` slot of the given
markdown parser error condition, respectively.


###### [!Function] `parse-markdown`  _in_ _:allow-other-keys_

Parses given input port _in_ and returns markdown S-expression.

The returning value is S-expression represented AST of markdown. The
structure is **not** specified in this document yet, thus it might be
changed in future.


