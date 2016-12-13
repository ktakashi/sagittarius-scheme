@; -*- coding: utf-8 -*-
@subsection[:tag "text.object-builder"]{(text sxml object-builder) - SXML to Scheme object builder}

@define[Library]{@name{(text sxml object-builder)}}
@desc{This library provides APIs to build Scheme object from SXML.}

@subsubsection{High level APIs}

@define[Function]{@name{sxml->object}
 @args{sxml builder :optional unknown-tag-handler}}
@desc{Builds a Scheme object from given SXML @var{sxml}. The @var{builder}
must be a object-builder described below.

If optional argument @var{unknown-tag-handler} is given, then it must be a
procedure accepts 2 arguments, @var{builder} and @var{sxml}. The procedure
is called when the process met a tag which can't be handled by given
@var{builder}. Users can return an object if needed. The default behaviour
of the handler is raising an error.
}

@define[Macro]{@name{sxml-object-builder} @args{spec @dots{}}}
@define["Auxiliary syntax"]{@name{*namespace*}}
@define["Auxiliary syntax"]{@name{<!>}}
@define["Auxiliary syntax"]{@name{?}}
@define["Auxiliary syntax"]{@name{??}}
@desc{A DSL which constructs object-builder.

The @var{spec} must be one of the followings:
@itemlist[
@item{@code{(*namespace* ((ns uri) ...) spec @dots{})}}
  @item{@code{(* spec @dots{})}}
  @item{@code{(+ spec @dots{})}}
  @item{@code{(/ spec @dots{})}}
  @item{@code{(? spec @dots{})}}
  @item{@code{(<!> @var{tag} builder)}}
  @item{@code{spec spec* @dots{}}}
  @item{@code{(@var{tag} @var{ctr})}}
  @item{@code{(@var{tag} @var{ctr} @var{next})}}
]
@var{tag} can be either a symbol or the following clause:
@itemlist[
  @item{@code{(?? pred)}}
]
@var{pred} must be a predicate of SXML tag.

@var{ctr} must be a procedure which takes 3 arguments, @var{name},
@var{attributes} and @var{contents}. These are SXML's tagname, list of
attributes and SXML contents, respectively.

The first form of the @var{spec} specifies aliases of namespaces. Users can write
qualified name with prefixed instead of unprefixed qualified name.

The second to forth form of @var{spec} specify the amount of nested
@var{spec @dots{}} existence. The @code{*} means 0 or more.
The @code{+} means 1 or more. And the @code{?} means 0 or 1.

The fifth form of @var{spec} means cyclic structure.

The sixth form of @var{spec} means set of @var{spec spec @dots{}}.

The following shows how to use this DSL macro
@codeblock{
(define builder
  (sxml-object-builder
    (*namespace* ((ns "urn:foo"))
    (ns:bar
      (ns:buz list)
      (foo list)))))
}
The above definition can build an object from the following SXML
@codeblock{
(*TOP*
  (urn:foo:bar
    (urn:buz "buz")
    (foo "foo")))
}
}
