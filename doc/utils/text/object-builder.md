[§2] (text sxml object-builder) - SXML to Scheme object builder {#text.object-builder}
-------------

###### [!Library] `(text sxml object-builder)` 

This library provides APIs to build Scheme object from SXML.

### [§3] High level APIs

###### [!Function] `sxml->object`  _sxml_ _builder_ _:optional_ _unknown-tag-handler_

Builds a Scheme object from given SXML _sxml_. The _builder_must be a object-builder described below.

If optional argument _unknown-tag-handler_ is given, then it must be a
procedure accepts 2 arguments, _builder_ and _sxml_. The procedure
is called when the process met a tag which can't be handled by given
_builder_. Users can return an object if needed. The default behaviour
of the handler is raising an error.


###### [!Macro] `sxml-object-builder`  _spec_ _..._
###### [!Auxiliary syntax] `*namespace*` 
###### [!Auxiliary syntax] `<!>` 
###### [!Auxiliary syntax] `?` 
###### [!Auxiliary syntax] `??` 

A DSL which constructs object-builder.

The _spec_ must be one of the followings:

- `(*namespace* ((ns uri) ...) spec ...)`
- `(* spec ...)`
- `(+ spec ...)`
- `(/ spec ...)`
- `(? spec ...)`
- `(<!> _tag_ builder)`
- `spec spec* ...`
- `(_tag_ _ctr_)`
- `(_tag_ _ctr_ _next_)`

_tag_ can be either a symbol or the following clause:

- `(?? pred)`

_pred_ must be a predicate of SXML tag.

_ctr_ must be a procedure which takes 3 arguments, _name_,
_attributes_ and _contents_. These are SXML's tagname, list of
attributes and SXML contents, respectively.

The first form of the _spec_ specifies aliases of namespaces. Users can
write qualified name with prefixed instead of unprefixed qualified name.

The second to forth form of _spec_ specify the amount of nested
_spec ..._ existence. The `*` means 0 or more.
The `+` means 1 or more. And the `?` means 0 or 1.

The fifth form of _spec_ means cyclic structure.

The sixth form of _spec_ means set of _spec spec ..._.

The following shows how to use this DSL macro

``````````scheme
(define builder
  (sxml-object-builder
    (*namespace* ((ns "urn:foo")))
    (ns:bar list
      (ns:buz list)
      (foo list))))
``````````

The above definition can build an object from the following SXML

``````````scheme
(*TOP*
  (urn:foo:bar
    (urn:foo:buz "buz")
    (foo "foo")))
``````````

A generic SXML builder can be written like this:

``````````scheme
(define-record-type xml-object
  (fields name attributes contents))

(define xml-object-builder
  (sxml-object-builder
   (<!> (?? values) make-xml-object)))
``````````



#### [§4] XML object

This section describes convenience record type and procedures.

###### [!Record Type] `xml-object` 
###### [!Function] `xml-object?`  _obj_
###### [!Function] `make-xml-object`  _name_ _attributes_ _contents_
###### [!Function] `xml-object-name`  _xml-object_
###### [!Function] `xml-object-attributes`  _xml-object_
###### [!Function] `xml-object-contents`  _xml-object_

A very simple XML object type. An instance of this record type
holds tag name (_name_), attribute as alist (_attributes_) and
contents which must be a valid SXML or other XML objects (_contents_).


###### [!Function] `sxml->xml-object`  _sxml_ _:optional_ _handler_

Builds XML object described above from given _sxml_.

