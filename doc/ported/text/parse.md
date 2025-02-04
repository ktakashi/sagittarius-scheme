[§2] (text parse) - Parsing input stream {#ported.text.parse}
-------------

###### [!Library] `(text parse)` 

The `(text parse)` library is inspired and compatible with Oleg
Kiselyov's input parsing library. You can use this library in place of his
`'input-parse.scm'` and `'look-for-str.scm'`.

###### [!Function] `find-string-from-port?`  _str_ _in-port_ _:optional_ _max-no-char_

Looks for a string _str_ from the input port _in-port_. The
optional argument _max-no-char_ limits the maxmum number of characters to be
read from the port; the search span is until EOF.

If _str_ is found, the function returns the number of characters it has read
from the port, and the port is set to read the first char after that (that is,
after the _str_). If _str_ is not found, the function returns #f.

Note: Although this procedure has ``?'` in its name, it may return
non-boolean value, contrary to the Scheme convention.


In the following functions, _char-list_ refers to one of the following.

- A character set which defined in SRFI-14.
- A list of characters, character sets and/or symbol `*eof*`.

That denotes a set of characters. If a symbol `*eof*` is included, the EOF
condition is also included.  Without `*eof*`, the EOF condition is regarded
as an error.

###### [!Function] `assert-curr-char`  _char-list_ _string_ _:optional_ _port_

Reads a character from the _port_ and looks it up in the
_char-list_ of expected characters. If the read character was found among
the expected, it is returned. Otherwise, the procedure writes a nasty message
using _string_ as a comment, and quits.


###### [!Function] `skip-until`  _char-list/number_ _:optional_ _port_

_Char-list/number_ must be either char-list or number.

If it is a number; skips the specified number of characters from the port and
returns #f.

If it is a char-list; reads and skips characters from the port until one of the
break characters is encountered. This break character is returned. The break
characters are specified as the char-list. This list may include EOF, which is
to be coded as a symbol `*eof*`.


###### [!Function] `skip-while`  _char-list_ _:optional_ _port_

Advances the _port_ to the first character that is not a member of the
_char-list_ -- or till the EOF, whichever occurs sooner. This character or
the EOF object is returned. This character is left on the stream.


###### [!Function] `peek-next-char`  _:optional_ _port_

Advances to the next character in the port and peeks at it. This function
is useful when parsing LR(1)-type languages.


###### [!Function] `next-token`  _prefix-char-list_ _break-char-list_ _:optional_ _comment_ _port_

Skips any number of characters in _prefix-char-list_, then collects the
characters until it sees _break-char-list_. The collected characters are
returned as a string. The break character remains in the _port_.

If the function encounters EOF and `*eof*` is not included in
_break-char-list_, an error is signalled with _comment_ is included in the
message.


###### [!Function] `next-token-of`  _char-list/pred_ _:optional_ _port_

Reads and collects the characters as far as it belongs to _char-list/pred_,
then returns them as a string. The first character that doesn't belong to
_char-list/pred_ remains on the _port_.

_Char-list/pred_ may be a char-list or a predicate that takes a character.
If it is a predicate, each character is passed to it, and the character is
regarded to \`\`belong to'' _char-list/pred_ when it returns a true value.


###### [!Function] `read-string`  _n_ _:optional_ _port_

Reads up to _n_ characters, collects them into a string, and returns
it. If the input stream contains less characters, the returns string contains
as many characters available.

This function is similar with `get-string-n`.


