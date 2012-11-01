@; -*- coding: utf-8 -*-
@subsection[:tag "text.html-lite"]{(text html-lite) - Simple HTML document builder library}


@define[Library]{@name{(text html-lite)}}
@desc{This library provides simple HTML builder procedures based on HTML5.}

@define[Function]{@name{html-escape} @args{:optional (in (current-input-port))}}
@define[Function]{@name{html-escape-string} @args{string}}
@desc{Escapes the unsafe characters in HTML and returns escaped string.

The @code{html-escape} reads the string from optional argument @var{in} which
must be an textual input port and returns an escaped string.

The @code{html-escape-string} takes a string and returns an escaped string.
}

@define[Function]{@name{html-doctype} @args{:key (type :html-4.01-strict)}}
@desc{Returns a doctype declaration for an HTML document. @var{type} can be one
of the followings;

@dl-list{
  @dl-item[@code{:html-5}]{
    HTML 5
  }
  @dl-item[@code{:html-4.01-strict :html-4.01, :strict, :html-strict}]{
    HTML 4.01 Strict DTD
  }
  @dl-item[@code{:html-4.01-transitional :html-transitional, :transitional}]{
    HTML 4.01 Transitional DTD
  }
  @dl-item[@code{:xhtml-1.0-strict :xhtml-1.0}]{
    XHTML 1.0 Strict DTD
  }
  @dl-item[@code{:xhtml-1.0-transitional}]{
    XHTML 1.0 Transitional DTD
  }
  @dl-item[@code{:xhtml-1.0-frameset}]{
    XHTML 1.0 Frameset DTD
  }
  @dl-item[@code{:xhtml-1.1}]{
    XHTML 1.1 DTD
  }
}
}

@define[Function]{@name{html:@var{element}} @args{args @dots{}}}
@desc{Construct an HTML element @var{element}. Currently following elements are
provided. 

@codeblock{
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
}

The result of these functions is a tree of text segments, which can be written
out to a port by @code{write-tree} or can be converted to a string by
@code{tree->string} @secref["text.tree"]{(text tree) - Lightweight text generation}.

You can specify attributes of the element by using a keyword-value notation
before the actual content.

@snipet[=> "<a href=\"http://example.com\">example</a>"]{
(tree->string (html:a :href "http://example.com" "example"))
}

The boolean value given to the attribute has special meaning. If #t is given,
the attribute is rendered without a value. If #f is given, the attribute is not
rendered.

@snipet[=> "<table border></table>"]{(tree->string (html:table :border #t))}
@snipet[=> "<table></table>"]{(tree->string (html:table :border #f))}

Special characters in attribute values are escaped by the function, but the
ones in the content are not.

@snipet[=> "<div foo=\"&lt;&gt;&amp;&quot;\"><not escaped></div>"]{
(tree->string (html:div :foo "<>&\"" "<not escaped>"))
}


}