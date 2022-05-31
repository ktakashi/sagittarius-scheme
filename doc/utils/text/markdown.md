[§2] (text markdown) -- Markdown parser {#text.markdown}
-------------

###### [!Library] `(text markdown)` 

This library provides markdown parser.

The library consists 3 parts, first one is the parser, second one is the
converter which converts markdown sexp to SHTML. And the last one is the
user APIs.


### [§3] High level APIs

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



###### [!Function] `string->markdown`  _string_ _:rest_ _opt_

Reads markdown from given _string_.

This procedure is thin wrapper of `markdown-read`. It opens string
input port of _string_ and call the `markdown-read`.

The rest argument _opt_ is passed to `markdown-read`.


### [§3] Supporting syntaxes

This section describes which syntax is supported on this library. Markdown
has variety of dialect and it is rather impossible to provide all of them.
So this library only provides most common features.

Atx heading
: Starting `#` up to 6 of them is supported.
  ``````````scheme
  # header\n
  ``````````
  => ``\<h1>header\</h1>``
  The line can contain inline syntaxes and must end with line break. If it
  doesn't have line break, then the line is considered plain text.

Setext heading
: A line which have `=` or `-` in its
  next line is supported. The `=` or `-` can be more than one.
  ``````````scheme
  header
  ======
  ``````````
  => ``\<h1>header\</h1>``
  The fist line can contain inline syntaxes. The second line must end with
  line break.

Blockquote
: A section which starts with `>` is supported.
  ``````````scheme
  > sentence1
  > sentence2
  ``````````
  => ``\<blockquote>sentence1\\nsentence2\</blockquote>``
  Nested blockquote is not supported.

Verbatim
: A section which starts with `"    "` 
  (4 spaces or one tab) is supported.
  ``````````scheme
      this
        is
          the sentence
  ``````````
  => ``\<pre>thie\\n  is\\n    the sentence\</pre>``

Code block
: A section which starts with `"```\n"` and end with
  `"```\n"` is supported.
  ``````````scheme
  ```
  (define (print . args)
    (for-each display args) (newline))
  ```
  ``````````
  => ``\<pre>(define (print . args)\\n  (for-each display args) (newline))\</pre>``

Note
: A line which start with `"[^_$name_]: "` is 
  supported. To refer this note, use `"[^_$name_]"`. _$name_can be alphanumeric.
  ``````````scheme
  [^1]: note1
  ``````````
  => ``\<div id="notes">\<ol>\<li>note1\</li>\</div>``
  To refer:
  ``````````scheme
  Note to refer [^1].
  ``````````
  => ``\<p>Note to refer \<sup>\<a href="#id">1\</a>\</sup>.``
  The generated HTML also contains `title` attribute with the value of
  note.

Reference
: A line which start with `"[_$name_]: "` is 
  supported. To refer this note, use `"[^_$name_]"`. _$name_can be alphanumeric. To refer the reference, use `"[label][ref]"`.
  ``````````scheme
  [Ref]: http://foo (title)
  or
  [Ref]: http://foo 'title'
  or
  [Ref]: http://foo "title"
  ``````````
  => ``\<div id="references">\<div>[Ref]: http://foo 'title'\</div>\</div>``
  To refer:
  ``````````scheme
  [label][Ref]
  ``````````
  => ``\<a href="http://foo">label\</a>``

### [§3] Conversion APIs

###### [!Function] `markdown-sexp->sxml`  _sexp_ _:key_ _(no-reference_ _#t)_ _(no-notes_ _#f)_ _:allow-other-keys_

Converts given markdown S-expression _sexp_ to SXML.

The keyword arguments _no-reference_ and _no-notes_ control the
result SXML to have reference section and note section. If the values are
true values, then returning SXML doesn't have reference or note section.


###### [!Function] `markdown-sexp->string`  _sexp_ _:key_ _(no-indent_ _#f)_ _:allow-other-keys_ _opts_

Converts given markdown S-expression _sexp_ to HTML string.

The procedure first calls `markdown-sexp->sxml` passing _sexp_ and
_opts_ then converts the returned value to HTML string.

The keyword argument _no-indent_ controls if the returning string
has indentation or not. If the value is true value, then returning string
doesn't have indentation.


### [§3] Parser APIs

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


