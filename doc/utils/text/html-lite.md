[§2] (text html-lite) - Simple HTML document builder library {#text.html-lite}
-------------

###### [!Library] `(text html-lite)` 

This library provides simple HTML builder procedures based on HTML5.

###### [!Function] `html-escape`  _:optional_ _(in_ _(current-input-port))_
###### [!Function] `html-escape-string`  _string_

Escapes the unsafe characters in HTML and returns escaped string.

The `html-escape` reads the string from optional argument _in_ which
must be an textual input port and returns an escaped string.

The `html-escape-string` takes a string and returns an escaped string.


###### [!Function] `html-doctype`  _:key_ _(type_ _:html-4.01-strict)_

Returns a doctype declaration for an HTML document. _type_ can be one
of the followings;

`:html-5`
: HTML 5

`:html-4.01-strict :html-4.01, :strict, :html-strict`
: HTML 4.01 Strict DTD

`:html-4.01-transitional :html-transitional, :transitional`
: HTML 4.01 Transitional DTD

`:xhtml-1.0-strict :xhtml-1.0`
: XHTML 1.0 Strict DTD

`:xhtml-1.0-transitional`
: XHTML 1.0 Transitional DTD

`:xhtml-1.0-frameset`
: XHTML 1.0 Frameset DTD

`:xhtml-1.1`
: XHTML 1.1 DTD



###### [!Function] `html:`  _element_ _args_ _..._

Construct an HTML element _element_. Currently following elements are
provided. 

``````````scheme
   a       abbr    address  area       article  aside
   audio   b       base     bdi        bdo      blockquote
   body    br      button   canvas     caption  cite
   code    col     colgroup command    datalist dd
   del     details dfn      div        dl       dt
   em      embed   fieldset figcaption figure   footer
   form
   h1      h2      h3       h4         h5       h6
   head    header  hgroup   hr         html
   i       iframe  img      input      ins      kbd
   keygen  label   legend   li         link     map
   mark    menu    meta     meter      nav      noscript
   object  ol      optgroup option     output   p
   param   pre     progress q          rp       rt
   ruby    s       samp     script     section  select
   small   source  span     strong     style    sub
   summary sup     table    tbody      td       textarea
   tfoot   th      thead    time       title    tr
   track   u       ul       var        video    wbr
``````````

The result of these functions is a tree of text segments, which can be written
out to a port by `write-tree` or can be converted to a string by
`tree->string` [(text tree) - Lightweight text generation](#text.tree).

You can specify attributes of the element by using a keyword-value notation
before the actual content.

``(tree->string (html:a :href "http://example.com" "example"))
`` => ``\<a href="http://example.com">example\</a>``

The boolean value given to the attribute has special meaning. If #t is given,
the attribute is rendered without a value. If #f is given, the attribute is not
rendered.

``(tree->string (html:table :border #t))`` => ``\<table border>\</table>``

``(tree->string (html:table :border #f))`` => ``\<table>\</table>``

Special characters in attribute values are escaped by the function, but the
ones in the content are not.

``(tree->string (html:div :foo "<>&\"" "<not escaped>"))
`` => ``\<div foo="&lt;&gt;&amp;&quot;">\<not escaped>\</div>``



