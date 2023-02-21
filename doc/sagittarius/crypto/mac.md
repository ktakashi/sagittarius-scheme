[§3] MAC library - (sagittarius crypto mac) {#sagittarius.crypto.mac}
------------------------------------------------------

The Message Authentication Code, MAC, library provides MAC 
generation / verification operations.

###### [!Library] `(sagittarius crypto mac)`

The MAC library, this library exports the procedures listed
below sections.

### [§4] Algorithms

Below are the MAC algorithms supported by the library.

###### [!MAC algorithm] `*mac:hmac*`
###### [!MAC algorithm] `*mac:cmac*`
###### [!MAC algorithm] `*mac:gmac*`
###### [!MAC algorithm] `*mac:kmac*`
###### [!MAC algorithm] `*mac:kmac-128*`
###### [!MAC algorithm] `*mac:kmac-256*`

###### [!Function] `mac?` _obj_

Returns `#t` if the given _obj_ is MAC object, otherwise `#f`.

###### [!Function] `make-mac` _type_ _key_ _opts_ _..._

Creates a MAC object of algorithm _type_.  
_key_ must be an appropriate shared secret per _type_. At this moment,
all the algorithms require bytevector.

###### [!Function] `mac-type` (_mac_ `mac?`)

Returns the type of given _mac_.

###### [!Function] `mac-mac-size` (_mac_ `mac?`)

Returns MAC size of the given _mac_, if the MAC size is fixed, 
otherwise `#f`.  
Only KMAC is variable length.

###### [!Function] `mac-oid` (_mac_ `mac?`)

Returns OID of the _mac_, if it has, otherwise `#f`.

###### [!Function] `generate-mac` (_mac_ `mac?`) (_message_ `bytevector?`) :optional _length_

Generates MAC of given _message_ by _mac_.  
Optional argument _length_ specifies the length of returning bytevector.

NOTE: _length_ doesn't guarantee the bytevector is filled with MAC. If the
MAC size is shorter than _length_, then excess values are filled with 0.

###### [!Function] `generate-mac!` (_mac_ `mac?`) (_message_ `bytevector?`) (_out_ `bytevector?`) :optional _start_ _length_

Generates MAC of given _message_ by _mac_ and fills it in the _out_ from
position _start_ to _length_ count.

###### [!Function] `verify-mac` (_mac_ `mac?`) (_message_ `bytevector?`) (_auth-mac_ `bytevector?`)

Verifies if the given _auth-mac_ is MAC of _message_ by _mac_ and returns `#t`
if the MAC is verified, otherwise signals an error.

###### [!Function] `mac-init!` (_mac_ `mac?`)

Initialises the given _mac_. If this procedure is called during the
message processing, then the previous state will be reset.

###### [!Function] `mac-process!` (_mac_ `mac?`) (_message_ `bytevector?`) :optional _start_ _length_

Processes the given _message_ with _mac_.  
Optional arguments specifies the range of _message_ to be processed.

###### [!Function] `mac-done!` (_mac_ `mac?`) (_out_ `bytevector?`) :optional _start_ _length_

Generates MAC by _mac_ and fills it into the _out_ from position
_start to _length_ count.
